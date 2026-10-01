mod fit;
mod options;
mod write;

use std::{fmt, io};

use crate::{Doc, DocPtr};

use fit::print_doc;
pub use options::{IndentationPolicy, LineEnding, RenderOptions};
pub use write::{FmtWrite, IoWrite};

pub struct PrettyFmt<'a, 'd, T>
where
    T: DocPtr<'a> + 'a,
{
    doc: &'d Doc<'a, T>,
    options: RenderOptions,
}

impl<'a, T> fmt::Display for PrettyFmt<'a, '_, T>
where
    T: DocPtr<'a>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.doc.render_fmt_with(self.options, f)
    }
}

impl<'a, T> Doc<'a, T>
where
    T: DocPtr<'a> + 'a,
{
    /// Writes a rendered document to a `std::io::Write` object.
    ///
    /// Equivalent to [`render_with`](Doc::render_with) with
    /// [`RenderOptions::new(width)`](RenderOptions::new).
    #[inline]
    pub fn render<W>(&self, width: usize, out: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.render_with(RenderOptions::new(width), out)
    }

    /// Writes a rendered document to a `std::io::Write` object, honoring `options`.
    #[inline]
    pub fn render_with<W>(&self, options: RenderOptions, out: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.render_raw_with(options, &mut IoWrite::new(out))
    }

    /// Writes a rendered document to a `std::fmt::Write` object.
    ///
    /// Equivalent to [`render_fmt_with`](Doc::render_fmt_with) with
    /// [`RenderOptions::new(width)`](RenderOptions::new).
    #[inline]
    pub fn render_fmt<W>(&self, width: usize, out: &mut W) -> fmt::Result
    where
        W: ?Sized + fmt::Write,
    {
        self.render_fmt_with(RenderOptions::new(width), out)
    }

    /// Writes a rendered document to a `std::fmt::Write` object, honoring `options`.
    #[inline]
    pub fn render_fmt_with<W>(&self, options: RenderOptions, out: &mut W) -> fmt::Result
    where
        W: ?Sized + fmt::Write,
    {
        self.render_raw_with(options, &mut FmtWrite::new(out))
    }

    /// Writes a rendered document to a `Render` object.
    ///
    /// Equivalent to [`render_raw_with`](Doc::render_raw_with) with
    /// [`RenderOptions::new(width)`](RenderOptions::new).
    #[inline]
    pub fn render_raw<W>(&self, width: usize, out: &mut W) -> Result<(), W::Error>
    where
        W: ?Sized + Render,
    {
        self.render_raw_with(RenderOptions::new(width), out)
    }

    /// Writes a rendered document to a `Render` object, honoring `options`.
    #[inline]
    pub fn render_raw_with<W>(&self, options: RenderOptions, out: &mut W) -> Result<(), W::Error>
    where
        W: ?Sized + Render,
    {
        print_doc(self, options, out)
    }

    /// Returns a value which implements `std::fmt::Display`
    ///
    /// Equivalent to [`print_with`](Doc::print_with) with
    /// [`RenderOptions::new(width)`](RenderOptions::new).
    ///
    /// ```
    /// use prettyless::{Doc, BoxDoc};
    /// let doc = BoxDoc::group(
    ///     BoxDoc::text("hello").append(Doc::line()).append(Doc::text("world"))
    /// );
    /// assert_eq!(format!("{}", doc.print(80)), "hello world");
    /// ```
    #[inline]
    pub fn print<'d>(&'d self, width: usize) -> PrettyFmt<'a, 'd, T> {
        self.print_with(RenderOptions::new(width))
    }

    /// Returns a value which implements `std::fmt::Display`, honoring `options`.
    ///
    /// ```
    /// use prettyless::{Doc, BoxDoc, LineEnding, RenderOptions};
    /// let doc = BoxDoc::group(
    ///     BoxDoc::text("hello").append(Doc::line()).append(Doc::text("world"))
    /// );
    /// let options = RenderOptions::new(1).with_line_ending(LineEnding::Crlf);
    /// assert_eq!(format!("{}", doc.print_with(options)), "hello\r\nworld");
    /// ```
    #[inline]
    pub fn print_with<'d>(&'d self, options: RenderOptions) -> PrettyFmt<'a, 'd, T> {
        PrettyFmt { doc: self, options }
    }
}

/// Trait representing the operations necessary to render a document
pub trait Render {
    type Error;

    /// Write part of `s`, returning the number of bytes accepted.
    ///
    /// A returned count must be at most `s.len()`, must fall on a UTF-8
    /// character boundary and must make progress. [`Render::write_str_all`]
    /// rejects any other count through [`Render::write_fault`], so a sink that
    /// can write half a character must implement it directly, as
    /// [`crate::IoWrite`] does.
    fn write_str(&mut self, s: &str) -> Result<usize, Self::Error>;

    /// Write all of `s`, retrying through [`Render::write_str`].
    fn write_str_all(&mut self, mut s: &str) -> Result<(), Self::Error> {
        while !s.is_empty() {
            let count = self.write_str(s)?;
            if count == 0 || count > s.len() || !s.is_char_boundary(count) {
                return Err(self.write_fault());
            }
            s = &s[count..];
        }
        Ok(())
    }

    /// Error reported when a document contains [`crate::Doc::Fail`].
    fn fail_doc(&self) -> Self::Error;

    /// Error reported when the sink violates the [`Render::write_str`] contract.
    ///
    /// Defaults to [`Render::fail_doc`].
    fn write_fault(&self) -> Self::Error {
        self.fail_doc()
    }
}

#[cfg(test)]
mod tests {
    use crate::{Arena, DocAllocator};

    use super::*;

    #[derive(Debug, PartialEq)]
    struct WroteWrong;

    /// Writes one byte at a time, ignoring UTF-8 character boundaries.
    struct ByteAtATime;

    impl Render for ByteAtATime {
        type Error = WroteWrong;

        fn write_str(&mut self, s: &str) -> Result<usize, Self::Error> {
            Ok(s.len().min(1))
        }

        fn fail_doc(&self) -> Self::Error {
            WroteWrong
        }
    }

    #[test]
    fn partial_write_off_a_char_boundary_is_an_error() {
        let arena = Arena::new();
        let doc = arena.text("é");
        let mut out = ByteAtATime;
        assert_eq!(doc.render_raw(80, &mut out), Err(WroteWrong));
    }

    /// Writes whole characters, one per call: a legal partial writer.
    #[derive(Default)]
    struct CharAtATime {
        written: String,
    }

    impl Render for CharAtATime {
        type Error = WroteWrong;

        fn write_str(&mut self, s: &str) -> Result<usize, Self::Error> {
            let len = s.chars().next().map_or(0, char::len_utf8);
            self.written.push_str(&s[..len]);
            Ok(len)
        }

        fn fail_doc(&self) -> Self::Error {
            WroteWrong
        }
    }

    #[test]
    fn boundary_respecting_partial_writer_assembles_the_output() {
        let arena = Arena::new();
        let doc = (arena.text("你好") + arena.line() + arena.text("z")).group();

        let mut out = CharAtATime::default();
        assert_eq!(doc.render_raw(6, &mut out), Ok(()));
        assert_eq!(out.written, "你好 z");
    }

    /// Accepts nothing, which a full or non-blocking sink may report. Before the
    /// contract check this spun forever instead of failing.
    struct NoProgress;

    impl Render for NoProgress {
        type Error = WroteWrong;

        fn write_str(&mut self, _s: &str) -> Result<usize, Self::Error> {
            Ok(0)
        }

        fn fail_doc(&self) -> Self::Error {
            WroteWrong
        }
    }

    #[test]
    fn zero_progress_write_is_an_error() {
        let arena = Arena::new();
        let doc = (arena.text("x") + arena.line() + arena.text("y")).group();
        let mut out = NoProgress;
        assert_eq!(doc.render_raw(80, &mut out), Err(WroteWrong));
    }

    #[derive(Debug, PartialEq)]
    enum SpaceWriteError {
        Document,
        WriteFault,
    }

    struct InvalidSpaceWriter {
        count: usize,
        space_calls: usize,
    }

    impl Render for InvalidSpaceWriter {
        type Error = SpaceWriteError;

        fn write_str(&mut self, s: &str) -> Result<usize, Self::Error> {
            if s.bytes().all(|b| b == b' ') {
                self.space_calls += 1;
                // Fail promptly if a regression retries a zero-progress write.
                assert_eq!(self.space_calls, 1);
                Ok(self.count)
            } else {
                Ok(s.len())
            }
        }

        fn fail_doc(&self) -> Self::Error {
            SpaceWriteError::Document
        }

        fn write_fault(&self) -> Self::Error {
            SpaceWriteError::WriteFault
        }
    }

    #[test]
    fn invalid_generated_space_writes_report_write_fault() {
        let a = Arena::new();
        let padding = a.text("a") + a.weak_space() + a.text("b");
        let indentation = a.hard_line().nest(1);
        for doc in [padding, indentation] {
            for count in [0, 2, usize::MAX] {
                let mut out = InvalidSpaceWriter {
                    count,
                    space_calls: 0,
                };
                assert_eq!(
                    doc.render_raw(80, &mut out),
                    Err(SpaceWriteError::WriteFault)
                );
                assert_eq!(out.space_calls, 1);
            }
        }
    }

    #[test]
    fn partial_writer_assembles_generated_spaces_across_chunks() {
        let a = Arena::new();
        let doc =
            (a.text("a") + a.hard_line() + a.text("b") + a.weak_space() + a.text("c")).nest(205);
        let mut out = CharAtATime::default();
        assert_eq!(doc.render_raw(300, &mut out), Ok(()));
        assert_eq!(out.written, format!("a\n{}b c", " ".repeat(205)));
    }
}
