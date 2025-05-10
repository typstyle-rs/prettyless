use std::{fmt, io};

#[cfg(feature = "termcolor")]
use termcolor::{ColorSpec, WriteColor};

use super::{Render, SPACES};

/// Writes to something implementing `std::io::Write`
pub struct IoWrite<W> {
    upstream: W,
}

impl<W> IoWrite<W> {
    pub fn new(upstream: W) -> IoWrite<W> {
        IoWrite { upstream }
    }
}

impl<W> Render for IoWrite<W>
where
    W: io::Write,
{
    type Error = io::Error;

    fn write_str(&mut self, s: &str) -> io::Result<usize> {
        self.upstream.write(s.as_bytes())
    }

    fn write_str_all(&mut self, s: &str) -> io::Result<()> {
        self.upstream.write_all(s.as_bytes())
    }

    fn fail_doc(&self) -> Self::Error {
        io::Error::other("Document failed to render")
    }
}

/// Writes to something implementing `std::fmt::Write`
pub struct FmtWrite<W> {
    upstream: W,
}

impl<W> FmtWrite<W> {
    pub fn new(upstream: W) -> FmtWrite<W> {
        FmtWrite { upstream }
    }
}

impl<W> Render for FmtWrite<W>
where
    W: fmt::Write,
{
    type Error = fmt::Error;

    fn write_str(&mut self, s: &str) -> Result<usize, fmt::Error> {
        self.write_str_all(s).map(|_| s.len())
    }

    fn write_str_all(&mut self, s: &str) -> fmt::Result {
        self.upstream.write_str(s)
    }

    fn fail_doc(&self) -> Self::Error {
        fmt::Error
    }
}

/// Trait representing the operations necessary to write an annotated document.
pub trait RenderAnnotated<'a, A>: Render {
    fn push_annotation(&mut self, annotation: &'a A) -> Result<(), Self::Error>;
    fn pop_annotation(&mut self) -> Result<(), Self::Error>;
}

impl<A, W> RenderAnnotated<'_, A> for IoWrite<W>
where
    W: io::Write,
{
    fn push_annotation(&mut self, _: &A) -> Result<(), Self::Error> {
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

impl<A, W> RenderAnnotated<'_, A> for FmtWrite<W>
where
    W: fmt::Write,
{
    fn push_annotation(&mut self, _: &A) -> Result<(), Self::Error> {
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

#[cfg(feature = "termcolor")]
pub struct TermColored<W> {
    color_stack: Vec<ColorSpec>,
    upstream: W,
}

#[cfg(feature = "termcolor")]
impl<W> TermColored<W> {
    pub fn new(upstream: W) -> TermColored<W> {
        TermColored {
            color_stack: Vec::new(),
            upstream,
        }
    }
}

#[cfg(feature = "termcolor")]
impl<W> Render for TermColored<W>
where
    W: io::Write,
{
    type Error = io::Error;

    fn write_str(&mut self, s: &str) -> io::Result<usize> {
        self.upstream.write(s.as_bytes())
    }

    fn write_str_all(&mut self, s: &str) -> io::Result<()> {
        self.upstream.write_all(s.as_bytes())
    }

    fn fail_doc(&self) -> Self::Error {
        io::Error::new(io::ErrorKind::Other, "Document failed to render")
    }
}

#[cfg(feature = "termcolor")]
impl<W> RenderAnnotated<'_, ColorSpec> for TermColored<W>
where
    W: WriteColor,
{
    fn push_annotation(&mut self, color: &ColorSpec) -> Result<(), Self::Error> {
        self.color_stack.push(color.clone());
        self.upstream.set_color(color)
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        self.color_stack.pop();
        match self.color_stack.last() {
            Some(previous) => self.upstream.set_color(previous),
            None => self.upstream.reset(),
        }
    }
}

enum Annotation<'a, A> {
    Push(&'a A),
    Pop,
}

pub(super) struct BufferWrite<'a, A> {
    buffer: String,
    annotations: Vec<(usize, Annotation<'a, A>)>,
}

impl<'a, A> BufferWrite<'a, A> {
    pub(super) fn new() -> Self {
        BufferWrite {
            buffer: String::new(),
            annotations: Vec::new(),
        }
    }

    pub(super) fn render<W>(&mut self, render: &mut W) -> Result<(), W::Error>
    where
        W: RenderAnnotated<'a, A>,
        W: ?Sized,
    {
        let mut start = 0;
        for (end, annotation) in &self.annotations {
            let s = &self.buffer[start..*end];
            if !s.is_empty() {
                render.write_str_all(s)?;
            }
            start = *end;
            match annotation {
                Annotation::Push(a) => render.push_annotation(a)?,
                Annotation::Pop => render.pop_annotation()?,
            }
        }
        let s = &self.buffer[start..];
        if !s.is_empty() {
            render.write_str_all(s)?;
        }
        Ok(())
    }
}

impl<A> Render for BufferWrite<'_, A> {
    type Error = ();

    fn write_str(&mut self, s: &str) -> Result<usize, Self::Error> {
        self.buffer.push_str(s);
        Ok(s.len())
    }

    fn write_str_all(&mut self, s: &str) -> Result<(), Self::Error> {
        self.buffer.push_str(s);
        Ok(())
    }

    fn fail_doc(&self) -> Self::Error {}
}

impl<'a, A> RenderAnnotated<'a, A> for BufferWrite<'a, A> {
    fn push_annotation(&mut self, a: &'a A) -> Result<(), Self::Error> {
        self.annotations
            .push((self.buffer.len(), Annotation::Push(a)));
        Ok(())
    }

    fn pop_annotation(&mut self) -> Result<(), Self::Error> {
        self.annotations.push((self.buffer.len(), Annotation::Pop));
        Ok(())
    }
}

pub(super) fn write_newline<W>(ind: usize, out: &mut W) -> Result<(), W::Error>
where
    W: ?Sized + Render,
{
    out.write_str_all("\n")?;
    write_spaces(ind, out)
}

pub(super) fn write_spaces<W>(spaces: usize, out: &mut W) -> Result<(), W::Error>
where
    W: ?Sized + Render,
{
    let mut inserted = 0;
    while inserted < spaces {
        let insert = SPACES.len().min(spaces - inserted);
        inserted += out.write_str(&SPACES[..insert])?;
    }

    Ok(())
}
