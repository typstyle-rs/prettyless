use crate::{Doc, DocPtr, Render};

use super::write::{write_newline, BufferWrite};

pub fn print_doc<'a, W, T>(doc: &Doc<'a, T>, width: usize, out: &mut W) -> Result<(), W::Error>
where
    T: DocPtr<'a> + 'a,
    W: ?Sized + Render,
{
    let temp_arena = &typed_arena::Arena::new();
    Printer {
        pos: 0,
        cmds: vec![Cmd {
            indent: 0,
            mode: Mode::Break,
            doc,
        }],
        fit_docs: vec![],
        width,
        temp_arena,
    }
    .print_to(0, out)?;

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

struct Printer<'d, 'a, T>
where
    T: DocPtr<'a> + 'a,
{
    pos: usize,
    cmds: Vec<Cmd<'d, 'a, T>>,
    fit_docs: Vec<&'d Doc<'a, T>>,
    width: usize,
    temp_arena: &'d typed_arena::Arena<T>,
}

impl<'d, 'a, T> Printer<'d, 'a, T>
where
    T: DocPtr<'a> + 'a,
{
    fn print_to<W>(&mut self, top: usize, out: &mut W) -> Result<bool, W::Error>
    where
        W: ?Sized + Render,
    {
        let mut fits = true;
        while self.cmds.len() > top {
            // Pop the next command
            let mut cmd = self.cmds.pop().unwrap();

            // Drill down until we hit a leaf or emit something
            loop {
                let Cmd { indent, mode, doc } = cmd;
                match *doc {
                    Doc::Nil => break,
                    Doc::Fail => return Err(out.fail_doc()),

                    Doc::Text(ref s) => {
                        out.write_str_all(s)?;
                        self.pos += s.len();
                        fits &= self.pos <= self.width;
                        break;
                    }

                    Doc::RenderLen(len, ref inner) => {
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

                    Doc::Hardline => {
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
                        cmd.doc = append_docs2(left, right, |doc| {
                            self.cmds.push(Cmd { indent, mode, doc })
                        });
                    }

                    Doc::Nest(offset, ref inner) => {
                        cmd.indent = indent.saturating_add_signed(offset);
                        cmd.doc = inner;
                    }

                    Doc::Group(ref inner) => {
                        if mode == Mode::Break && self.fitting(inner, self.pos, indent) {
                            cmd.mode = Mode::Flat;
                        }
                        cmd.doc = inner;
                    }
                    Doc::FlatAlt(ref break_doc, ref flat_doc) => {
                        cmd.doc = match mode {
                            Mode::Break => break_doc,
                            Mode::Flat => flat_doc,
                        };
                    }
                    Doc::Union(ref left, ref right) => {
                        // Try the left branch in a buffer
                        let save_pos = self.pos;
                        let save_cmds = self.cmds.len();

                        self.cmds.push(Cmd {
                            indent,
                            mode,
                            doc: left,
                        });
                        let mut buffer = BufferWrite::new();

                        if let Ok(true) = self.print_to(save_cmds, &mut buffer) {
                            buffer.render(out)?;
                            break;
                        } else {
                            // Revert and try right
                            self.pos = save_pos;
                            self.cmds.truncate(save_cmds);
                            cmd.doc = right;
                        }
                    }

                    Doc::Column(ref f) => {
                        cmd.doc = self.temp_arena.alloc(f(self.pos));
                    }
                    Doc::Nesting(ref f) => {
                        cmd.doc = self.temp_arena.alloc(f(indent));
                    }
                }
            }
        }

        Ok(fits)
    }

    fn fitting(&mut self, next: &'d Doc<'a, T>, mut pos: usize, indent: usize) -> bool {
        // We start in "flat" mode and may fall back to "break" mode when backtracking.
        let mut cmd_bottom = self.cmds.len();
        let mut mode = Mode::Flat;

        // fit_docs is our work‐stack for documents to check in flat mode.
        self.fit_docs.clear();
        self.fit_docs.push(next);

        // As long as we have either flat‐stack items or break commands to try...
        while cmd_bottom > 0 || !self.fit_docs.is_empty() {
            // Pop the next doc to inspect, or backtrack to bcmds in break mode.
            let mut doc = if let Some(d) = self.fit_docs.pop() {
                d
            } else {
                mode = Mode::Break;
                cmd_bottom -= 1;
                self.cmds[cmd_bottom].doc
            };

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
                    Doc::RenderLen(len, _) => {
                        pos += len;
                        if pos > self.width {
                            return false;
                        }
                        break;
                    }

                    Doc::Hardline => {
                        // A hardline only “fits” in break mode.
                        return mode == Mode::Break;
                    }

                    Doc::Append(ref left, ref right) => {
                        // Push r then l so we process l first.
                        doc = append_docs2(left, right, |d| self.fit_docs.push(d));
                    }

                    Doc::FlatAlt(ref break_doc, ref flat_doc) => {
                        // Select branch based on current mode.
                        doc = if mode == Mode::Break {
                            break_doc
                        } else {
                            flat_doc
                        };
                    }

                    Doc::Nest(_, ref inner) | Doc::Group(ref inner) | Doc::Union(_, ref inner) => {
                        doc = inner;
                    }

                    Doc::Column(ref f) => {
                        doc = self.temp_arena.alloc(f(pos));
                    }

                    Doc::Nesting(ref f) => {
                        doc = self.temp_arena.alloc(f(indent));
                    }
                }
            }
        }

        // If we've exhausted both fcmds and break_idx, everything fit.
        true
    }
}

fn append_docs2<'a, 'd, T>(
    ldoc: &'d Doc<'a, T>,
    rdoc: &'d Doc<'a, T>,
    mut consumer: impl FnMut(&'d Doc<'a, T>),
) -> &'d Doc<'a, T>
where
    T: DocPtr<'a>,
{
    let d = append_docs(rdoc, &mut consumer);
    consumer(d);
    append_docs(ldoc, &mut consumer)
}

fn append_docs<'a, 'd, T>(
    mut doc: &'d Doc<'a, T>,
    consumer: &mut impl FnMut(&'d Doc<'a, T>),
) -> &'d Doc<'a, T>
where
    T: DocPtr<'a>,
{
    loop {
        // Since appended documents often appear in sequence on the left side we
        // gain a slight performance increase by batching these pushes (avoiding
        // to push and directly pop `Append` documents)
        match doc {
            Doc::Append(l, r) => {
                consumer(r);
                doc = l;
            }
            _ => return doc,
        }
    }
}
