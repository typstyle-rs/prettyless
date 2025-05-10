mod fit;
mod write;

use std::{fmt, io};

#[cfg(feature = "termcolor")]
use termcolor::{ColorSpec, WriteColor};

use crate::{Doc, DocPtr};

use fit::best;
pub use write::{FmtWrite, IoWrite, RenderAnnotated};

pub struct PrettyFmt<'a, 'd, T, A>
where
    A: 'a,
    T: DocPtr<'a, A> + 'a,
{
    doc: &'d Doc<'a, T, A>,
    width: usize,
}

impl<'a, T, A> fmt::Display for PrettyFmt<'a, '_, T, A>
where
    T: DocPtr<'a, A>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.doc.render_fmt(self.width, f)
    }
}

impl<'a, T, A> Doc<'a, T, A>
where
    T: DocPtr<'a, A> + 'a,
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
        for<'b> W: RenderAnnotated<'b, A>,
        W: ?Sized,
    {
        best(self, width, out)
    }

    /// Returns a value which implements `std::fmt::Display`
    ///
    /// ```
    /// use prettyless::{Doc, BoxDoc};
    /// let doc = BoxDoc::<()>::group(
    ///     BoxDoc::text("hello").append(Doc::line()).append(Doc::text("world"))
    /// );
    /// assert_eq!(format!("{}", doc.pretty(80)), "hello world");
    /// ```
    #[inline]
    pub fn pretty<'d>(&'d self, width: usize) -> PrettyFmt<'a, 'd, T, A> {
        PrettyFmt { doc: self, width }
    }
}

#[cfg(feature = "termcolor")]
impl<'a, T> Doc<'a, T, ColorSpec>
where
    T: DocPtr<'a, ColorSpec> + 'a,
{
    #[inline]
    pub fn render_colored<W>(&self, width: usize, out: W) -> io::Result<()>
    where
        W: WriteColor,
    {
        best(self, width, &mut write::TermColored::new(out))
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

macro_rules! make_spaces {
    () => { "" };
    ($s: tt $($t: tt)*) => { concat!("          ", make_spaces!($($t)*)) };
}

pub(crate) const SPACES: &str = make_spaces!(,,,,,,,,,,);
