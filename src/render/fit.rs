use crate::{Doc, DocPtr, Render, visitor::visit_sequence_rev};

use super::{
    IndentationPolicy, RenderOptions,
    write::{BufferWrite, write_spaces},
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
        options,
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
    options: RenderOptions,
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
    // Generated indentation is deferred until content commits it under
    // `IndentationPolicy::Deferred`; under `Eager` it is written by the break and
    // this stays zero.
    pending_indent: usize,
    pending_padding: usize,
    // Pending indentation alone is not content, so weak whitespace can disappear
    // on an otherwise empty line.
    fresh: bool,
}

impl ColumnState {
    fn new(indent: usize) -> Self {
        Self {
            pos: 0,
            pending_indent: indent,
            pending_padding: 0,
            fresh: true,
        }
    }

    fn is_fresh(self) -> bool {
        self.fresh
    }

    fn within(self, width: usize) -> bool {
        self.pos <= width
    }

    fn fits_suffix_padding(self, width: usize, has_suffix: bool) -> bool {
        // Trailing weak spaces disappear unless a suffix commits them. Count
        // that padding consistently in rendering and fitting, excluding suffix text.
        !has_suffix || self.pending_padding == 0 || self.prospective_column() <= width
    }

    fn prospective_column(self) -> usize {
        // Callbacks and alignment see where the next text would start. Observing
        // this column must not emit padding that might be discarded at a break.
        self.pos
            .saturating_add(self.pending_indent)
            .saturating_add(self.pending_padding)
    }

    fn weak_space(&mut self) {
        if !self.is_fresh() {
            self.pending_padding = self.pending_padding.saturating_add(1);
        }
    }

    fn commit_indent(&mut self) -> usize {
        let indent = std::mem::take(&mut self.pending_indent);
        self.pos = self.pos.saturating_add(indent);
        indent
    }

    // Empty text leaves indentation, padding, and line freshness unchanged. Zero
    // display width does not imply empty text (a combining mark commits padding).
    fn commit_text(&mut self, text: &str, width: usize) -> Option<usize> {
        if text.is_empty() {
            return None;
        }
        Some(self.commit_content(width))
    }

    // A suffix is treated as content without inspecting the document it contains.
    fn commit_content(&mut self, width: usize) -> usize {
        let indent = self.commit_indent();
        let padding = std::mem::take(&mut self.pending_padding);
        self.pos = self.pos.saturating_add(padding).saturating_add(width);
        self.fresh = false;
        indent.saturating_add(padding)
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

                    Doc::WeakSpace => {
                        self.column.weak_space();
                        break;
                    }
                    Doc::Text(s) => {
                        fits &= self.write_str(out, s, s.len())?;
                        if self.rejects_branch(fits) {
                            return Ok(false);
                        }
                        break;
                    }

                    Doc::TextWithLen(len, inner) => {
                        // inner must be a text node
                        let str = match &**inner {
                            Doc::Text(s) => s,
                            _ => unreachable!(),
                        };
                        fits &= self.write_str(out, str, *len)?;
                        if self.rejects_branch(fits) {
                            return Ok(false);
                        }
                        break;
                    }

                    Doc::HardLine | Doc::WeakLine => {
                        // A break ends the shared line, including suffixes queued
                        // by the caller before entering this speculative branch.
                        if self.suffix_start < self.line_suffixes.len() {
                            fits &= self.write_suffix_padding(out)?;
                            if self.rejects_branch(fits) {
                                return Ok(false);
                            }
                            self.cmds.push(cmd);
                            self.push_line_suffixes(mode, indent);
                            break;
                        }

                        if matches!(doc, Doc::WeakLine) && self.column.is_fresh() {
                            break;
                        }
                        // Borrow the continuation's indentation without consuming
                        // it: a union buffer must stop at its saved command boundary.
                        let next_indent = self.cmds.last().map_or(indent, |next| next.indent);
                        self.write_newline(out, next_indent)?;
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
                        cmd.indent = self.column.prospective_column();
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
                        cmd.doc = self.temp_arena.alloc(f(self.column.prospective_column()));
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
                fits &= self.write_suffix_padding(out)?;
                self.push_line_suffixes(Mode::Break, 0);
            }
        }

        // Deferred suffix text is excluded from fitting, but the weak spaces it
        // commits still belong to the branch's ordinary layout.
        Ok(fits
            && self.column.fits_suffix_padding(
                self.options.width,
                self.suffix_start < self.line_suffixes.len(),
            ))
    }

    /// Whether a speculative branch can be abandoned once it has failed.
    ///
    /// Inside a branch (`union_depth > 0`) a text that landed past the width has
    /// already decided it: the union that opened the branch discards the buffered
    /// output and restores the column, the command stack and the suffix queue, so
    /// rendering the rest of the branch cannot change any result.
    fn rejects_branch(&self, fits: bool) -> bool {
        !fits && self.union_depth > 0
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

    fn write_str<W>(&mut self, out: &mut W, s: &str, len: usize) -> Result<bool, W::Error>
    where
        W: ?Sized + Render,
    {
        let Some(spaces) = self.column.commit_text(s, len) else {
            return Ok(true);
        };
        write_spaces(spaces, out)?;
        out.write_str_all(s)?;
        Ok(self.column.within(self.options.width))
    }

    fn write_suffix_padding<W>(&mut self, out: &mut W) -> Result<bool, W::Error>
    where
        W: ?Sized + Render,
    {
        // Treat the queued suffix as content before evaluating its document.
        // Empty suffixes deliberately commit spaces too; no suffix inspection
        // or speculative callback evaluation is needed to predict weak behavior.
        let fits = self.column.fits_suffix_padding(self.options.width, true);
        write_spaces(self.column.commit_content(0), out)?;
        Ok(fits)
    }

    fn write_newline<W>(&mut self, out: &mut W, indent: usize) -> Result<(), W::Error>
    where
        W: ?Sized + Render,
    {
        out.write_str_all(self.options.line_ending.as_str())?;
        self.column = ColumnState::new(indent);
        if self.options.indentation_policy == IndentationPolicy::Eager {
            // Eager indentation follows the terminator immediately, so blank and
            // terminal lines carry trailing spaces. Indentation is not content, so
            // the new line stays fresh for weak whitespace.
            write_spaces(self.column.commit_indent(), out)?;
        }
        Ok(())
    }

    fn fitting(&mut self, next: &'d Doc<'a, T>, indent: usize, mode: Mode) -> bool {
        let mut column = self.column;
        // Only suffix presence matters: its body and width are excluded from
        // fitting. Presence commits weak padding and prevents a fresh weak line
        // from being suppressed, matching the eventual flush.
        let mut has_suffix = self.suffix_start < self.line_suffixes.len();
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

                    Doc::WeakSpace => {
                        column.weak_space();
                        break;
                    }
                    Doc::Text(s) => {
                        if column.commit_text(s, s.len()).is_some()
                            && !column.within(self.options.width)
                        {
                            return false;
                        }
                        break;
                    }
                    Doc::TextWithLen(len, inner) => {
                        let text = match &**inner {
                            Doc::Text(s) => s,
                            _ => unreachable!(),
                        };
                        if column.commit_text(text, *len).is_some()
                            && !column.within(self.options.width)
                        {
                            return false;
                        }
                        break;
                    }

                    // A suppressed break cannot end the fit check: following
                    // text remains on this line and may still exceed the width.
                    Doc::WeakLine if column.is_fresh() && !has_suffix => break,
                    Doc::HardLine | Doc::WeakLine => {
                        return mode == Mode::Break
                            && column.fits_suffix_padding(self.options.width, has_suffix);
                    }

                    Doc::Append(left, right) => {
                        // Push r then l so we process l first.
                        cmd.doc = visit_sequence2(left, right, |doc| {
                            self.fit_docs.push(Cmd { indent, mode, doc })
                        });
                    }
                    Doc::LineSuffix(_) => {
                        has_suffix = true;
                        break;
                    }

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
                        cmd.indent = column.prospective_column();
                        cmd.doc = inner;
                    }
                    Doc::Group(inner) | Doc::Union(_, inner) | Doc::PartialUnion(_, inner) => {
                        cmd.doc = inner;
                    }

                    #[cfg(feature = "contextual")]
                    Doc::OnColumn(f) => {
                        cmd.doc = self.temp_arena.alloc(f(column.prospective_column()));
                    }
                    #[cfg(feature = "contextual")]
                    Doc::OnNesting(f) => {
                        cmd.doc = self.temp_arena.alloc(f(indent));
                    }
                }
            }
        }

        column.fits_suffix_padding(self.options.width, has_suffix)
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
