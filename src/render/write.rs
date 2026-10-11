use std::{fmt, io};

use crate::text::SPACES;

use super::Render;

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

    /// Byte oriented: a partial `io::Write` may stop inside a character, so this
    /// is only usable together with the `write_str_all` override below.
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

/// Internal extension of [`Render`] for committing an accepted union branch.
///
/// Branch output is committed once its layout succeeds. Consuming the buffer
/// lets a speculative parent reuse its storage instead of copying the same
/// bytes at each nesting level; direct outputs forward the bytes to their sink.
pub(super) trait PrintWrite: Render {
    /// Appends an accepted branch's bytes in order, consuming its output buffer.
    ///
    /// This must preserve existing output and the bytes in the buffer. It must
    /// not render the branch again: its callbacks and layout decisions have
    /// already run during speculation.
    ///
    /// # Errors
    ///
    /// Propagates errors from the destination sink. The branch has already been
    /// selected, so a sink error must not cause its union to try the fallback.
    fn write_buffer(&mut self, buffer: BufferWrite) -> Result<(), Self::Error>;
}

/// Adapts a [`Render`] sink to the printer's [`PrintWrite`] contract.
///
/// Ordinary writes and errors are forwarded, including any override of
/// [`Render::write_str_all`] needed for byte-oriented partial writes.
pub(super) struct DirectWrite<'w, W: ?Sized>(&'w mut W);

impl<'w, W: ?Sized> DirectWrite<'w, W> {
    /// Wraps a sink without buffering its ordinary writes.
    pub(super) fn new(out: &'w mut W) -> Self {
        Self(out)
    }
}

impl<W: ?Sized + Render> Render for DirectWrite<'_, W> {
    type Error = W::Error;

    fn write_str(&mut self, s: &str) -> Result<usize, Self::Error> {
        self.0.write_str(s)
    }

    fn write_str_all(&mut self, s: &str) -> Result<(), Self::Error> {
        self.0.write_str_all(s)
    }

    fn fail_doc(&self) -> Self::Error {
        self.0.fail_doc()
    }

    fn write_fault(&self) -> Self::Error {
        self.0.write_fault()
    }
}

impl<W: ?Sized + Render> PrintWrite for DirectWrite<'_, W> {
    fn write_buffer(&mut self, buffer: BufferWrite) -> Result<(), Self::Error> {
        self.0.write_str_all(&buffer.buffer)
    }
}

/// Owns the output of a speculative union branch.
///
/// Rejected output is discarded with this buffer. Accepted output is consumed
/// through [`PrintWrite::write_buffer`], preserving the already evaluated
/// layout and allowing an empty parent buffer to take ownership of its storage.
pub(super) struct BufferWrite {
    /// UTF-8 output collected while the branch is still speculative.
    buffer: String,
}

impl BufferWrite {
    /// Starts an empty branch without allocating storage until it writes bytes.
    pub(super) fn new() -> Self {
        BufferWrite {
            buffer: String::new(),
        }
    }
}

impl Render for BufferWrite {
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

impl PrintWrite for BufferWrite {
    fn write_buffer(&mut self, buffer: BufferWrite) -> Result<(), Self::Error> {
        if self.buffer.is_empty() {
            self.buffer = buffer.buffer;
        } else {
            self.buffer.push_str(&buffer.buffer);
        }
        Ok(())
    }
}

pub(super) fn write_spaces<W>(spaces: usize, out: &mut W) -> Result<(), W::Error>
where
    W: ?Sized + Render,
{
    let mut remaining = spaces;
    while remaining > 0 {
        let insert = SPACES.len().min(remaining);
        out.write_str_all(&SPACES[..insert])?;
        remaining -= insert;
    }

    Ok(())
}
