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

    fn write_str(&mut self, s: &str) -> Result<usize, Self::Error>;

    fn write_str_all(&mut self, mut s: &str) -> Result<(), Self::Error> {
        while !s.is_empty() {
            let count = self.write_str(s)?;
            s = &s[count..];
        }
        Ok(())
    }

    fn fail_doc(&self) -> Self::Error;
}
