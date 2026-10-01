use crate::{Doc, DocPtr, Render, visitor::visit_sequence_rev};

use super::{
    LineEnding, RenderOptions,
    write::{BufferWrite, write_newline},
};

pub fn print_doc<'a, W, T>(
    doc: &Doc<'a, T>,
    options: RenderOptions,
    out: &mut W,
) -> Result<(), W::Error>
where
    T: DocPtr<'a> + 'a,
    W: ?Sized + Render,
{
    Printer {
        column: ColumnState::new(0),
        cmds: vec![Cmd {
            indent: 0,
            mode: Mode::Break,
            doc,
        }],
        fit_docs: vec![],
        line_suffixes: vec![],
        suffix_start: 0,
        union_depth: 0,
        width: options.width(),
        line_ending: options.line_ending(),
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
    doc: &'d Doc<'a, T>,
}

// Commands copy document references without requiring the pointer type to be Clone.
impl<'a, T: DocPtr<'a> + 'a> Copy for Cmd<'_, 'a, T> {}

impl<'a, T: DocPtr<'a> + 'a> Clone for Cmd<'_, 'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

struct Printer<'d, 'a, T>
where
    T: DocPtr<'a> + 'a,
{
    column: ColumnState,
    cmds: Vec<Cmd<'d, 'a, T>>,
    fit_docs: Vec<Cmd<'d, 'a, T>>,
    line_suffixes: Vec<&'d Doc<'a, T>>,
    // Consumed entries survive speculation so rollback only restores indices.
    suffix_start: usize,
    union_depth: usize,
    width: usize,
    line_ending: LineEnding,
    #[cfg(feature = "contextual")]
    temp_arena: &'d typed_arena::Arena<T>,
}

#[derive(Default, Clone, Copy)]
struct PrintState {
    cmd_top: usize,
    // A union's pending suffixes belong to the caller's line, not its buffer.
    defer_suffixes: bool,
}

/// The printer's current column, shared by rendering and fitting.
#[derive(Clone, Copy)]
struct ColumnState {
    pos: usize,
}

impl ColumnState {
    fn new(indent: usize) -> Self {
        Self { pos: indent }
    }
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
            defer_suffixes,
        } = state;

        let mut fits = true;
        while self.cmds.len() > top {
            // Pop the next command
            let mut cmd = self.cmds.pop().unwrap();

            // Drill down until we hit a leaf or emit something
            loop {
                let Cmd { indent, mode, doc } = cmd;
                match doc {
                    Doc::Nil => break,
                    Doc::Fail => return Err(out.fail_doc()),

                    Doc::Text(s) => {
                        out.write_str_all(s)?;
                        self.column.pos += s.len();
                        fits &= self.column.pos <= self.width;
                        break;
                    }

                    Doc::TextWithLen(len, inner) => {
                        // inner must be a text node
                        let str = match &**inner {
                            Doc::Text(s) => s,
                            _ => unreachable!(),
                        };
                        out.write_str_all(str)?;
                        self.column.pos += len;
                        fits &= self.column.pos <= self.width;
                        break;
                    }

                    Doc::HardLine => {
                        // A break ends the shared line, including suffixes queued
                        // by the caller before entering this speculative branch.
                        if self.suffix_start < self.line_suffixes.len() {
                            self.cmds.push(cmd);
                            self.push_line_suffixes(mode, indent);
                            break;
                        }

                        // Borrow the continuation's indentation without consuming
                        // it: a union buffer must stop at its saved command boundary.
                        let next_indent = self.cmds.last().map_or(indent, |next| next.indent);
                        write_newline(next_indent, self.line_ending, out)?;
                        self.column = ColumnState::new(next_indent);
                        break;
                    }

                    Doc::Append(left, right) => {
                        // Push children in reverse so we process ldoc before rdoc
                        cmd.doc = visit_sequence2(left, right, |doc| {
                            self.cmds.push(Cmd { indent, mode, doc })
                        });
                    }
                    Doc::LineSuffix(inner) => {
                        self.line_suffixes.push(inner);
                        break;
                    }

                    Doc::Nest(offset, inner) => {
                        cmd.indent = indent.saturating_add_signed(*offset);
                        cmd.doc = inner;
                    }
                    Doc::DedentToRoot(inner) => {
                        // Dedent to the root level, which is always 0.
                        cmd.indent = 0;
                        cmd.doc = inner;
                    }
                    Doc::Align(inner) => {
                        // Align to the current position.
                        cmd.indent = self.column.pos;
                        cmd.doc = inner;
                    }

                    Doc::ExpandParent => break,
                    Doc::Flatten(inner) => {
                        cmd.mode = Mode::Flat;
                        cmd.doc = inner;
                    }
                    Doc::BreakOrFlat(break_doc, flat_doc) => {
                        cmd.doc = match mode {
                            Mode::Break => break_doc,
                            Mode::Flat => flat_doc,
                        };
                    }
                    Doc::Group(inner) => {
                        if mode == Mode::Break && self.fitting(inner, indent, Mode::Flat) {
                            cmd.mode = Mode::Flat;
                        }
                        cmd.doc = inner;
                    }
                    Doc::Union(left, right) => {
                        if mode == Mode::Flat {
                            cmd.doc = left;
                            continue;
                        }

                        // Buffer output and save both queue boundaries. Flushing
                        // advances suffix_start; new suffixes only append, so an
                        // unsuccessful branch can restore the original queue.
                        let save_column = self.column;
                        let save_suffix_start = self.suffix_start;
                        let save_suffix_len = self.line_suffixes.len();
                        let save_state = PrintState {
                            cmd_top: self.cmds.len(),
                            // Reaching the branch boundary is not the caller's EOF.
                            defer_suffixes: true,
                        };

                        self.cmds.push(Cmd {
                            indent,
                            mode,
                            doc: left,
                        });
                        let mut buffer = BufferWrite::new();
                        self.union_depth += 1;
                        let fits = matches!(self.print_to(&mut buffer, save_state), Ok(true));
                        self.union_depth -= 1;

                        if fits {
                            self.compact_suffixes();
                            buffer.render(out)?;
                            break;
                        } else {
                            // Discard branch commands and newly queued suffixes,
                            // then revive any caller suffixes the branch consumed.
                            self.column = save_column;
                            self.cmds.truncate(save_state.cmd_top);
                            self.line_suffixes.truncate(save_suffix_len);
                            self.suffix_start = save_suffix_start;
                            cmd.doc = right;
                        }
                    }
                    Doc::PartialUnion(left, right) => {
                        if mode == Mode::Flat || self.fitting(left, indent, Mode::Break) {
                            cmd.doc = left;
                        } else {
                            cmd.doc = right;
                        }
                    }

                    #[cfg(feature = "contextual")]
                    Doc::OnColumn(f) => {
                        cmd.doc = self.temp_arena.alloc(f(self.column.pos));
                    }
                    #[cfg(feature = "contextual")]
                    Doc::OnNesting(f) => {
                        cmd.doc = self.temp_arena.alloc(f(indent));
                    }
                }
            }

            // Only actual rendering EOF flushes the remaining suffixes. A union
            // branch leaves them queued so later caller text precedes them.
            if !defer_suffixes
                && self.cmds.len() == top
                && self.suffix_start < self.line_suffixes.len()
            {
                self.push_line_suffixes(Mode::Break, 0);
            }
        }

        Ok(fits)
    }

    fn push_line_suffixes(&mut self, mode: Mode, indent: usize) {
        // Commands are popped from the end; reverse insertion preserves the
        // order in which suffixes were queued on this line.
        for index in (self.suffix_start..self.line_suffixes.len()).rev() {
            let doc = self.line_suffixes[index];
            self.cmds.push(Cmd { indent, mode, doc });
        }
        self.suffix_start = self.line_suffixes.len();
        self.compact_suffixes();
    }

    fn compact_suffixes(&mut self) {
        // Even a successful inner union may be rolled back by its outer union.
        // Keep consumed entries until no speculative frame can need them again.
        if self.union_depth == 0 && self.suffix_start > 0 {
            self.line_suffixes.drain(..self.suffix_start);
            self.suffix_start = 0;
        }
    }

    fn fitting(&mut self, next: &'d Doc<'a, T>, indent: usize, mode: Mode) -> bool {
        let mut column = self.column;
        // We start in "flat" mode and may fall back to "break" mode when backtracking.
        let mut cmd_bottom = self.cmds.len();

        // fit_docs is our work‐stack for documents to check in flat mode.
        self.fit_docs.clear();
        self.fit_docs.push(Cmd {
            indent,
            mode,
            doc: next,
        });

        // As long as we have either flat‐stack items or break commands to try...
        while cmd_bottom > 0 || !self.fit_docs.is_empty() {
            // Pop the next doc to inspect, or backtrack to bcmds in break mode.
            let mut cmd = self.fit_docs.pop().unwrap_or_else(|| {
                cmd_bottom -= 1;
                Cmd {
                    mode: Mode::Break,
                    ..self.cmds[cmd_bottom]
                }
            });

            // Drill into this doc until we either bail or consume a leaf.
            loop {
                // Contextual callbacks must observe this command's nesting,
                // including when the command comes from the caller's continuation.
                let Cmd { indent, mode, doc } = cmd;
                match doc {
                    Doc::Nil => break,
                    Doc::Fail => return false,

                    Doc::Text(s) => {
                        column.pos += s.len();
                        if column.pos > self.width {
                            return false;
                        }
                        break;
                    }
                    Doc::TextWithLen(len, _) => {
                        column.pos += len;
                        if column.pos > self.width {
                            return false;
                        }
                        break;
                    }

                    Doc::HardLine => {
                        // A hard_line only “fits” in break mode.
                        return mode == Mode::Break;
                    }

                    Doc::Append(left, right) => {
                        // Push r then l so we process l first.
                        cmd.doc = visit_sequence2(left, right, |doc| {
                            self.fit_docs.push(Cmd { indent, mode, doc })
                        });
                    }
                    Doc::LineSuffix(_) => break, // Line suffixes don't affect fitting, skip them entirely

                    Doc::ExpandParent => {
                        if mode == Mode::Flat {
                            return false;
                        }
                        break;
                    }
                    Doc::Flatten(inner) => {
                        cmd.mode = Mode::Flat;
                        cmd.doc = inner;
                    }
                    Doc::BreakOrFlat(break_doc, flat_doc) => {
                        // Select branch based on current mode.
                        cmd.doc = if mode == Mode::Break {
                            break_doc
                        } else {
                            flat_doc
                        };
                    }

                    Doc::Union(inner, _) | Doc::PartialUnion(inner, _) if mode == Mode::Flat => {
                        // In flat mode we only consider the first branch.
                        cmd.doc = inner;
                    }

                    // These wrappers change callback inputs even before a line
                    // break is emitted, so fitting must traverse them like rendering.
                    Doc::Nest(offset, inner) => {
                        cmd.indent = indent.saturating_add_signed(*offset);
                        cmd.doc = inner;
                    }
                    Doc::DedentToRoot(inner) => {
                        cmd.indent = 0;
                        cmd.doc = inner;
                    }
                    Doc::Align(inner) => {
                        cmd.indent = column.pos;
                        cmd.doc = inner;
                    }
                    Doc::Group(inner) | Doc::Union(_, inner) | Doc::PartialUnion(_, inner) => {
                        cmd.doc = inner;
                    }

                    #[cfg(feature = "contextual")]
                    Doc::OnColumn(f) => {
                        cmd.doc = self.temp_arena.alloc(f(column.pos));
                    }
                    #[cfg(feature = "contextual")]
                    Doc::OnNesting(f) => {
                        cmd.doc = self.temp_arena.alloc(f(indent));
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
