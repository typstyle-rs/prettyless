mod fit;
mod write;

use std::{fmt, io};

use crate::{Doc, DocPtr};

use fit::print_doc;
pub use write::{FmtWrite, IoWrite};

pub struct PrettyFmt<'a, 'd, T>
where
    T: DocPtr<'a> + 'a,
{
    doc: &'d Doc<'a, T>,
    width: usize,
}

impl<'a, T> fmt::Display for PrettyFmt<'a, '_, T>
where
    T: DocPtr<'a>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.doc.render_fmt(self.width, f)
    }
}

impl<'a, T> Doc<'a, T>
where
    T: DocPtr<'a> + 'a,
{
    /// Writes a rendered document to a `std::io::Write` object.
    #[inline]
    pub fn render<W>(&self, width: usize, out: &mut W) -> io::Result<()>
    where
        W: ?Sized + io::Write,
    {
        self.render_raw(width, &mut IoWrite::new(out))
    }

    /// Writes a rendered document to a `std::fmt::Write` object.
    #[inline]
    pub fn render_fmt<W>(&self, width: usize, out: &mut W) -> fmt::Result
    where
        W: ?Sized + fmt::Write,
    {
        self.render_raw(width, &mut FmtWrite::new(out))
    }

    /// Writes a rendered document to a `RenderAnnotated<A>` object.
    #[inline]
    pub fn render_raw<W>(&self, width: usize, out: &mut W) -> Result<(), W::Error>
    where
        W: ?Sized + Render,
    {
        print_doc(self, width, out)
    }

    /// Returns a value which implements `std::fmt::Display`
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
        PrettyFmt { doc: self, width }
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
