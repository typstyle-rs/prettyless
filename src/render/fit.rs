use crate::{visitor::visit_sequence_rev, Doc, DocPtr, Render};

use super::write::{write_newline, BufferWrite};

pub fn print_doc<'a, W, T>(doc: &Doc<'a, T>, width: usize, out: &mut W) -> Result<(), W::Error>
where
    T: DocPtr<'a> + 'a,
    W: ?Sized + Render,
{
    Printer {
        pos: 0,
        cmds: vec![Cmd {
            indent: 0,
            mode: Mode::Break,
            kind: CmdKind::Doc(doc),
        }],
        fit_docs: vec![],
        line_suffixes: vec![],
        width,
        #[cfg(feature = "contextual")]
        temp_arena: &typed_arena::Arena::new(),
    }
    .print_to(out, PrintState::default())?;

    Ok(())
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Mode {
    Break,
    Flat,
}

struct Cmd<'d, 'a, T>
where
    T: DocPtr<'a> + 'a,
{
    indent: usize,
    mode: Mode,
    kind: CmdKind<'d, 'a, T>,
}

#[derive(Clone, Copy)]
enum CmdKind<'d, 'a, T>
where
    T: DocPtr<'a> + 'a,
{
    Doc(&'d Doc<'a, T>),
    TagExit(u32),
}

struct FitCmd<'d, 'a, T>
where
    T: DocPtr<'a> + 'a,
{
    mode: Mode,
    doc: &'d Doc<'a, T>,
}

struct Printer<'d, 'a, T>
where
    T: DocPtr<'a> + 'a,
{
    pos: usize,
    cmds: Vec<Cmd<'d, 'a, T>>,
    fit_docs: Vec<FitCmd<'d, 'a, T>>,
    line_suffixes: Vec<&'d Doc<'a, T>>,
    width: usize,
    #[cfg(feature = "contextual")]
    temp_arena: &'d typed_arena::Arena<T>,
}

#[derive(Default, Clone, Copy)]
struct PrintState {
    cmd_top: usize,
    line_suffix_top: usize,
}

impl<'d, 'a, T> Printer<'d, 'a, T>
where
    T: DocPtr<'a> + 'a,
{
    fn print_to<W>(&mut self, out: &mut W, state: PrintState) -> Result<bool, W::Error>
    where
        W: ?Sized + Render,
    {
        let PrintState {
            cmd_top: top,
            line_suffix_top: ls_top,
        } = state;

        let mut fits = true;
        while self.cmds.len() > top {
            // Pop the next command
            let mut cmd = self.cmds.pop().unwrap();

            // Drill down until we hit a leaf or emit something
            loop {
                let indent = cmd.indent;
                let mode = cmd.mode;
                let CmdKind::Doc(doc) = cmd.kind else {
                    let CmdKind::TagExit(id) = cmd.kind else {
                        unreachable!();
                    };
                    out.on_tag_exit(id)?;
                    break;
                };
                match *doc {
                    Doc::Nil => break,
                    Doc::Fail => return Err(out.fail_doc()),

                    Doc::Text(ref s) => {
                        out.write_str_all(s)?;
                        self.pos += s.len();
                        fits &= self.pos <= self.width;
                        break;
                    }

                    Doc::TextWithLen(len, ref inner) => {
                        // inner must be a text node
                        let str = match **inner {
                            Doc::Text(ref s) => s,
                            _ => unreachable!(),
                        };
                        out.write_str_all(str)?;
                        self.pos += len;
                        fits &= self.pos <= self.width;
                        break;
                    }

                    Doc::HardLine => {
                        // flush line suffixes
                        if self.line_suffixes.len() > ls_top {
                            self.cmds.push(cmd);
                            self.push_line_suffixes(ls_top, mode, indent);
                            break;
                        }

                        // The next document may have different indentation so we should use it if
                        // we can
                        if let Some(next) = self.cmds.pop() {
                            write_newline(next.indent, out)?;
                            self.pos = next.indent;
                            cmd = next;
                        } else {
                            write_newline(indent, out)?;
                            self.pos = indent;
                            break;
                        }
                    }

                    Doc::Append(ref left, ref right) => {
                        // Push children in reverse so we process ldoc before rdoc
                        cmd.kind = CmdKind::Doc(visit_sequence2(left, right, |doc| {
                            self.cmds.push(Cmd {
                                indent,
                                mode,
                                kind: CmdKind::Doc(doc),
                            })
                        }));
                    }
                    Doc::Tagged(id, ref inner) => {
                        out.on_tag_enter(id)?;
                        self.cmds.push(Cmd {
                            indent,
                            mode,
                            kind: CmdKind::TagExit(id),
                        });
                        cmd.kind = CmdKind::Doc(inner);
                    }
                    Doc::LineSuffix(ref inner) => {
                        self.line_suffixes.push(inner);
                        break;
                    }

                    Doc::Nest(offset, ref inner) => {
                        cmd.indent = indent.saturating_add_signed(offset);
                        cmd.kind = CmdKind::Doc(inner);
                    }
                    Doc::DedentToRoot(ref inner) => {
                        // Dedent to the root level, which is always 0.
                        cmd.indent = 0;
                        cmd.kind = CmdKind::Doc(inner);
                    }
                    Doc::Align(ref inner) => {
                        // Align to the current position.
                        cmd.indent = self.pos;
                        cmd.kind = CmdKind::Doc(inner);
                    }

                    Doc::ExpandParent => break,
                    Doc::Flatten(ref inner) => {
                        cmd.mode = Mode::Flat;
                        cmd.kind = CmdKind::Doc(inner);
                    }
                    Doc::BreakOrFlat(ref break_doc, ref flat_doc) => {
                        cmd.kind = CmdKind::Doc(match mode {
                            Mode::Break => break_doc,
                            Mode::Flat => flat_doc,
                        });
                    }
                    Doc::Group(ref inner) => {
                        if mode == Mode::Break && self.fitting(inner, self.pos, indent, Mode::Flat)
                        {
                            cmd.mode = Mode::Flat;
                        }
                        cmd.kind = CmdKind::Doc(inner);
                    }
                    Doc::Union(ref left, ref right) => {
                        if mode == Mode::Flat {
                            cmd.kind = CmdKind::Doc(left);
                            continue;
                        }

                        // Try the left branch in a buffer
                        let save_pos = self.pos;
                        let save_state = PrintState {
                            cmd_top: self.cmds.len(),
                            line_suffix_top: self.line_suffixes.len(),
                        };

                        self.cmds.push(Cmd {
                            indent,
                            mode,
                            kind: CmdKind::Doc(left),
                        });
                        let mut buffer = BufferWrite::new();

                        if let Ok(true) = self.print_to(&mut buffer, save_state) {
                            buffer.render(out)?;
                            break;
                        } else {
                            // Revert and try right
                            self.pos = save_pos;
                            self.cmds.truncate(save_state.cmd_top);
                            self.line_suffixes.truncate(save_state.line_suffix_top);
                            cmd.kind = CmdKind::Doc(right);
                        }
                    }
                    Doc::PartialUnion(ref left, ref right) => {
                        if mode == Mode::Flat || self.fitting(left, self.pos, indent, Mode::Break) {
                            cmd.kind = CmdKind::Doc(left);
                        } else {
                            cmd.kind = CmdKind::Doc(right);
                        }
                    }

                    #[cfg(feature = "contextual")]
                    Doc::OnColumn(ref f) => {
                        cmd.kind = CmdKind::Doc(self.temp_arena.alloc(f(self.pos)));
                    }
                    #[cfg(feature = "contextual")]
                    Doc::OnNesting(ref f) => {
                        cmd.kind = CmdKind::Doc(self.temp_arena.alloc(f(indent)));
                    }
                }
            }

            // handle line suffixes when all cleared
            if self.cmds.len() == top && self.line_suffixes.len() > ls_top {
                self.push_line_suffixes(ls_top, Mode::Break, 0);
            }
        }

        Ok(fits)
    }

    fn push_line_suffixes(&mut self, ls_top: usize, mode: Mode, indent: usize) {
        self.line_suffixes
            .drain(ls_top..)
            .rev()
            .for_each(|doc| {
                self.cmds.push(Cmd {
                    indent,
                    mode,
                    kind: CmdKind::Doc(doc),
                })
            });
    }

    #[cfg_attr(not(feature = "contextual"), allow(unused_variables))]
    fn fitting(&mut self, next: &'d Doc<'a, T>, mut pos: usize, indent: usize, mode: Mode) -> bool {
        // We start in "flat" mode and may fall back to "break" mode when backtracking.
        let mut cmd_bottom = self.cmds.len();

        // fit_docs is our work‐stack for documents to check in flat mode.
        self.fit_docs.clear();
        self.fit_docs.push(FitCmd { mode, doc: next });

        // As long as we have either flat‐stack items or break commands to try...
        while cmd_bottom > 0 || !self.fit_docs.is_empty() {
            // Pop the next doc to inspect, or backtrack to bcmds in break mode.
            let mut next = self.fit_docs.pop();
            if next.is_none() {
                while cmd_bottom > 0 {
                    cmd_bottom -= 1;
                    if let CmdKind::Doc(doc) = self.cmds[cmd_bottom].kind {
                        next = Some(FitCmd {
                            mode: Mode::Break,
                            doc,
                        });
                        break;
                    }
                }
                if next.is_none() {
                    continue;
                }
            }
            let FitCmd { mut mode, mut doc } = next.unwrap();

            // Drill into this doc until we either bail or consume a leaf.
            loop {
                match *doc {
                    Doc::Nil => break,
                    Doc::Fail => return false,

                    Doc::Text(ref s) => {
                        pos += s.len();
                        if pos > self.width {
                            return false;
                        }
                        break;
                    }
                    Doc::TextWithLen(len, _) => {
                        pos += len;
                        if pos > self.width {
                            return false;
                        }
                        break;
                    }

                    Doc::HardLine => {
                        // A hard_line only “fits” in break mode.
                        return mode == Mode::Break;
                    }

                    Doc::Append(ref left, ref right) => {
                        // Push r then l so we process l first.
                        doc = visit_sequence2(left, right, |doc| {
                            self.fit_docs.push(FitCmd { mode, doc })
                        });
                    }
                    Doc::LineSuffix(_) => break, // Line suffixes don't affect fitting, skip them entirely

                    Doc::ExpandParent => {
                        if mode == Mode::Flat {
                            return false;
                        }
                        break;
                    }
                    Doc::Tagged(_, ref inner) => {
                        doc = inner;
                    }
                    Doc::Flatten(ref inner) => {
                        mode = Mode::Flat;
                        doc = inner;
                    }
                    Doc::BreakOrFlat(ref break_doc, ref flat_doc) => {
                        // Select branch based on current mode.
                        doc = if mode == Mode::Break {
                            break_doc
                        } else {
                            flat_doc
                        };
                    }

                    Doc::Union(ref inner, _) | Doc::PartialUnion(ref inner, _)
                        if mode == Mode::Flat =>
                    {
                        // In flat mode we only consider the first branch.
                        doc = inner;
                    }

                    Doc::Nest(_, ref inner)
                    | Doc::DedentToRoot(ref inner)
                    | Doc::Align(ref inner)
                    | Doc::Group(ref inner)
                    | Doc::Union(_, ref inner)
                    | Doc::PartialUnion(_, ref inner) => {
                        doc = inner;
                    }

                    #[cfg(feature = "contextual")]
                    Doc::OnColumn(ref f) => {
                        doc = self.temp_arena.alloc(f(pos));
                    }
                    #[cfg(feature = "contextual")]
                    Doc::OnNesting(ref f) => {
                        doc = self.temp_arena.alloc(f(indent));
                    }
                }
            }
        }

        // If we've exhausted both fcmds and break_idx, everything fit.
        true
    }
}

fn visit_sequence2<'a, 'd, T>(
    ldoc: &'d Doc<'a, T>,
    rdoc: &'d Doc<'a, T>,
    mut consumer: impl FnMut(&'d Doc<'a, T>),
) -> &'d Doc<'a, T>
where
    T: DocPtr<'a>,
{
    let d = visit_sequence_rev(rdoc, &mut consumer);
    consumer(d);
    visit_sequence_rev(ldoc, &mut consumer)
}
