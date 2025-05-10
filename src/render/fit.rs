use crate::{Doc, DocPtr};

use super::{
    write::{write_newline, BufferWrite},
    RenderAnnotated,
};

pub fn best<'a, W, T, A>(doc: &Doc<'a, T, A>, width: usize, out: &mut W) -> Result<(), W::Error>
where
    T: DocPtr<'a, A> + 'a,
    for<'b> W: RenderAnnotated<'b, A>,
    W: ?Sized,
{
    let temp_arena = &typed_arena::Arena::new();
    Best {
        pos: 0,
        bcmds: vec![(0, Mode::Break, doc)],
        fcmds: vec![],
        annotation_levels: vec![],
        width,
        temp_arena,
    }
    .best(0, out)?;

    Ok(())
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum Mode {
    Break,
    Flat,
}

type Cmd<'d, 'a, T, A> = (usize, Mode, &'d Doc<'a, T, A>);

struct Best<'d, 'a, T, A>
where
    T: DocPtr<'a, A> + 'a,
{
    pos: usize,
    bcmds: Vec<Cmd<'d, 'a, T, A>>,
    fcmds: Vec<&'d Doc<'a, T, A>>,
    annotation_levels: Vec<usize>,
    width: usize,
    temp_arena: &'d typed_arena::Arena<T>,
}

impl<'d, 'a, T, A> Best<'d, 'a, T, A>
where
    T: DocPtr<'a, A> + 'a,
{
    fn best<W>(&mut self, top: usize, out: &mut W) -> Result<bool, W::Error>
    where
        W: RenderAnnotated<'d, A>,
        W: ?Sized,
    {
        let mut fits = true;

        while top < self.bcmds.len() {
            let mut cmd = self.bcmds.pop().unwrap();
            loop {
                let (ind, mode, doc) = cmd;
                match *doc {
                    Doc::Nil => {}
                    Doc::Append(ref ldoc, ref rdoc) => {
                        cmd.2 = append_docs2(ldoc, rdoc, |doc| self.bcmds.push((ind, mode, doc)));
                        continue;
                    }
                    Doc::FlatAlt(ref b, ref f) => {
                        cmd.2 = match mode {
                            Mode::Break => b,
                            Mode::Flat => f,
                        };
                        continue;
                    }
                    Doc::Group(ref doc) => {
                        if let Mode::Break = mode {
                            if self.fitting(doc, self.pos, ind) {
                                cmd.1 = Mode::Flat;
                            }
                        }
                        cmd.2 = doc;
                        continue;
                    }
                    Doc::Nest(off, ref doc) => {
                        // Once https://doc.rust-lang.org/std/primitive.usize.html#method.saturating_add_signed is stable
                        // this can be replaced
                        let new_ind = if off >= 0 {
                            ind.saturating_add(off as usize)
                        } else {
                            ind.saturating_sub(off.unsigned_abs())
                        };
                        cmd = (new_ind, mode, doc);
                        continue;
                    }
                    Doc::Hardline => {
                        // The next document may have different indentation so we should use it if
                        // we can
                        if let Some(next) = self.bcmds.pop() {
                            write_newline(next.0, out)?;
                            self.pos = next.0;
                            cmd = next;
                            continue;
                        } else {
                            write_newline(ind, out)?;
                            self.pos = ind;
                        }
                    }
                    Doc::RenderLen(len, ref doc) => match **doc {
                        Doc::OwnedText(ref s) => {
                            out.write_str_all(s)?;
                            self.pos += len;
                            fits &= self.pos <= self.width;
                        }
                        Doc::BorrowedText(s) => {
                            out.write_str_all(s)?;
                            self.pos += len;
                            fits &= self.pos <= self.width;
                        }
                        Doc::SmallText(ref s) => {
                            out.write_str_all(s)?;
                            self.pos += len;
                            fits &= self.pos <= self.width;
                        }
                        _ => unreachable!(),
                    },
                    Doc::OwnedText(ref s) => {
                        out.write_str_all(s)?;
                        self.pos += s.len();
                        fits &= self.pos <= self.width;
                    }
                    Doc::BorrowedText(s) => {
                        out.write_str_all(s)?;
                        self.pos += s.len();
                        fits &= self.pos <= self.width;
                    }
                    Doc::SmallText(ref s) => {
                        out.write_str_all(s)?;
                        self.pos += s.len();
                        fits &= self.pos <= self.width;
                    }
                    Doc::Annotated(ref ann, ref doc) => {
                        out.push_annotation(ann)?;
                        self.annotation_levels.push(self.bcmds.len());
                        cmd.2 = doc;
                        continue;
                    }
                    Doc::Union(ref l, ref r) => {
                        let pos = self.pos;
                        let annotation_levels = self.annotation_levels.len();
                        let bcmds = self.bcmds.len();

                        self.bcmds.push((ind, mode, l));

                        let mut buffer = BufferWrite::new();

                        match self.best(bcmds, &mut buffer) {
                            Ok(true) => buffer.render(out)?,
                            Ok(false) | Err(()) => {
                                self.pos = pos;
                                self.bcmds.truncate(bcmds);
                                self.annotation_levels.truncate(annotation_levels);
                                cmd.2 = r;
                                continue;
                            }
                        }
                    }
                    Doc::Column(ref f) => {
                        cmd.2 = self.temp_arena.alloc(f(self.pos));
                        continue;
                    }
                    Doc::Nesting(ref f) => {
                        cmd.2 = self.temp_arena.alloc(f(ind));
                        continue;
                    }
                    Doc::Fail => return Err(out.fail_doc()),
                }

                break;
            }
            while self.annotation_levels.last() == Some(&self.bcmds.len()) {
                self.annotation_levels.pop();
                out.pop_annotation()?;
            }
        }

        Ok(fits)
    }

    fn fitting(&mut self, next: &'d Doc<'a, T, A>, mut pos: usize, ind: usize) -> bool {
        let mut bidx = self.bcmds.len();
        self.fcmds.clear(); // clear from previous calls from best
        self.fcmds.push(next);

        let mut mode = Mode::Flat;
        loop {
            let mut doc = match self.fcmds.pop() {
                None => {
                    if bidx == 0 {
                        // All commands have been processed
                        return true;
                    } else {
                        bidx -= 1;
                        mode = Mode::Break;
                        self.bcmds[bidx].2
                    }
                }
                Some(cmd) => cmd,
            };

            loop {
                match *doc {
                    Doc::Nil => {}
                    Doc::Append(ref ldoc, ref rdoc) => {
                        doc = append_docs2(ldoc, rdoc, |doc| self.fcmds.push(doc));
                        continue;
                    }
                    // Newlines inside the group makes it not fit, but those outside lets it
                    // fit on the current line
                    Doc::Hardline => return mode == Mode::Break,
                    Doc::RenderLen(len, _) => {
                        pos += len;
                        if pos > self.width {
                            return false;
                        }
                    }
                    Doc::BorrowedText(str) => {
                        pos += str.len();
                        if pos > self.width {
                            return false;
                        }
                    }
                    Doc::OwnedText(ref str) => {
                        pos += str.len();
                        if pos > self.width {
                            return false;
                        }
                    }
                    Doc::SmallText(ref str) => {
                        pos += str.len();
                        if pos > self.width {
                            return false;
                        }
                    }
                    Doc::FlatAlt(ref b, ref f) => {
                        doc = match mode {
                            Mode::Break => b,
                            Mode::Flat => f,
                        };
                        continue;
                    }

                    Doc::Column(ref f) => {
                        doc = self.temp_arena.alloc(f(pos));
                        continue;
                    }
                    Doc::Nesting(ref f) => {
                        doc = self.temp_arena.alloc(f(ind));
                        continue;
                    }
                    Doc::Nest(_, ref next)
                    | Doc::Group(ref next)
                    | Doc::Annotated(_, ref next)
                    | Doc::Union(_, ref next) => {
                        doc = next;
                        continue;
                    }
                    Doc::Fail => return false,
                }
                break;
            }
        }
    }
}

fn append_docs2<'a, 'd, T, A>(
    ldoc: &'d Doc<'a, T, A>,
    rdoc: &'d Doc<'a, T, A>,
    mut consumer: impl FnMut(&'d Doc<'a, T, A>),
) -> &'d Doc<'a, T, A>
where
    T: DocPtr<'a, A>,
{
    let d = append_docs(rdoc, &mut consumer);
    consumer(d);
    append_docs(ldoc, &mut consumer)
}

fn append_docs<'a, 'd, T, A>(
    mut doc: &'d Doc<'a, T, A>,
    consumer: &mut impl FnMut(&'d Doc<'a, T, A>),
) -> &'d Doc<'a, T, A>
where
    T: DocPtr<'a, A>,
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
