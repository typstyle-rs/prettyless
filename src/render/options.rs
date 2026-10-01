/// Configuration for a render pass.
///
/// Carries everything a render call needs beyond the document itself, so new controls
/// can be added without introducing another family of methods. Pass one to any render
/// method of [`crate::Doc`]:
///
/// ```
/// use prettyless::{Doc, BoxDoc, LineEnding, RenderOptions};
/// let doc = BoxDoc::text("a").append(Doc::line()).append(Doc::text("b"));
/// let options = RenderOptions::new(1).with_line_ending(LineEnding::Crlf);
///
/// let mut out = String::new();
/// doc.render_fmt_with(options, &mut out).unwrap();
/// assert_eq!(out, "a\r\nb");
/// ```
///
/// All fields are private, so new options can be added without breaking callers.
#[derive(Clone, Copy, Debug)]
#[must_use]
pub struct RenderOptions {
    width: usize,
    line_ending: LineEnding,
}

impl RenderOptions {
    /// Creates options with a target line width of `width` and [`LineEnding::Lf`] breaks.
    #[inline]
    pub const fn new(width: usize) -> Self {
        Self {
            width,
            line_ending: LineEnding::Lf,
        }
    }

    /// Replaces the target line width, keeping the other options.
    #[inline]
    pub const fn with_width(mut self, width: usize) -> Self {
        self.width = width;
        self
    }

    /// Sets the terminator emitted for structural breaks.
    #[inline]
    pub const fn with_line_ending(mut self, ending: LineEnding) -> Self {
        self.line_ending = ending;
        self
    }

    /// The target line width.
    ///
    /// Layout aims to keep lines within this width, but unbreakable text and line
    /// suffixes can exceed it.
    #[inline]
    pub const fn width(self) -> usize {
        self.width
    }

    /// The terminator emitted for structural breaks.
    #[inline]
    pub const fn line_ending(self) -> LineEnding {
        self.line_ending
    }
}

/// The line terminator emitted for document line breaks when rendering.
///
/// Selected through [`RenderOptions`]. It applies to every structural document break,
/// including breaks emitted inside line suffixes and union branches. Text nodes are
/// written verbatim, so newlines embedded in text (`Doc::text("a\nb")`) are never
/// normalized, and the terminator length never affects fitting, indentation, or
/// contextual column values. [`LineEnding::Lf`] is the default.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum LineEnding {
    /// A single line feed, `\n`.
    #[default]
    Lf,
    /// Carriage return + line feed, `\r\n`.
    Crlf,
}

impl LineEnding {
    /// The terminator string.
    #[inline]
    pub fn as_str(self) -> &'static str {
        match self {
            LineEnding::Lf => "\n",
            LineEnding::Crlf => "\r\n",
        }
    }
}
