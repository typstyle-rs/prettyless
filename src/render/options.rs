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
#[derive(Clone, Copy, Debug)]
#[must_use]
#[non_exhaustive]
pub struct RenderOptions {
    /// The target line width.
    ///
    /// Layout aims to keep lines within this width, but unbreakable text and line
    /// suffixes can exceed it.
    pub width: usize,
    /// The terminator emitted for structural breaks.
    pub line_ending: LineEnding,
    /// When a structural break writes its indentation.
    pub indentation_policy: IndentationPolicy,
}

impl RenderOptions {
    /// Creates options with a target line width of `width`, [`LineEnding::Lf`] breaks,
    /// and [`IndentationPolicy::Eager`] indentation.
    #[inline]
    pub const fn new(width: usize) -> Self {
        Self {
            width,
            line_ending: LineEnding::Lf,
            indentation_policy: IndentationPolicy::Eager,
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

    /// Sets when a structural break writes its indentation.
    #[inline]
    pub const fn with_indentation_policy(mut self, policy: IndentationPolicy) -> Self {
        self.indentation_policy = policy;
        self
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

/// When a structural break writes its indentation.
///
/// Selected through [`RenderOptions`]. The policy changes only *when* the
/// indentation of a break is written; the break itself, the indentation it
/// selects, blank-line counts, fitting, and contextual columns are unaffected.
/// [`IndentationPolicy::Eager`] is the default.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub enum IndentationPolicy {
    /// Writes a break's indentation immediately after its terminator.
    ///
    /// Blank lines and a trailing break therefore carry trailing spaces. This is
    /// the behavior of upstream `pretty` and the default.
    #[default]
    Eager,
    /// Postpones a break's indentation until content on the new line commits it.
    ///
    /// Blank lines and a trailing break emit no indentation. Nonempty text and a
    /// line suffix commit pending indentation; empty text does not. A later break
    /// replaces pending indentation and the end of output discards it.
    Deferred,
}
