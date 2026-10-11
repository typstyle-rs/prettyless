use std::{borrow::Cow, fmt, ops::Deref};

use unicode_width::UnicodeWidthStr;

#[cfg(feature = "small-text")]
type SmallText = arrayvec::ArrayString<[u8; 22]>;

#[derive(Clone)]
pub enum Text<'a> {
    Owned(Box<str>),
    Borrowed(&'a str),
    #[cfg(feature = "small-text")]
    Small(SmallText),
}

impl Text<'_> {
    /// Create a text from [`std::fmt::Display`] string.
    /// It may use `SmallText` with `small-text` feature enabled.
    pub fn from_display(value: impl fmt::Display) -> Result<Self, fmt::Error> {
        use std::fmt::Write;

        #[cfg(feature = "small-text")]
        {
            let mut buf = FmtText::Small(SmallText::new());
            write!(buf, "{value}")?;
            Ok(match buf {
                FmtText::Small(b) => Self::Small(b),
                FmtText::Large(b) => Self::Owned(b.into()),
            })
        }

        #[cfg(not(feature = "small-text"))]
        {
            let mut buf = String::new();
            write!(buf, "{value}")?;
            Ok(Self::Owned(buf.into()))
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::Owned(s) => s,
            Self::Borrowed(s) => s,
            #[cfg(feature = "small-text")]
            Self::Small(s) => s,
        }
    }
}

impl fmt::Debug for Text<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl Deref for Text<'_> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Owned(s) => s,
            Self::Borrowed(s) => s,
            #[cfg(feature = "small-text")]
            Self::Small(s) => s,
        }
    }
}

impl<'a> From<Cow<'a, str>> for Text<'a> {
    fn from(value: Cow<'a, str>) -> Self {
        match value {
            Cow::Owned(t) => Self::Owned(t.into()),
            Cow::Borrowed(t) => Self::Borrowed(t),
        }
    }
}

#[cfg(feature = "small-text")]
enum FmtText {
    Small(SmallText),
    Large(String),
}

#[cfg(feature = "small-text")]
impl fmt::Write for FmtText {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        match self {
            FmtText::Small(buf) => {
                if buf.try_push_str(s).is_err() {
                    let mut new_str = String::with_capacity(buf.len() + s.len());
                    new_str.push_str(buf);
                    new_str.push_str(s);
                    *self = FmtText::Large(new_str);
                }
            }
            FmtText::Large(buf) => buf.push_str(s),
        }
        Ok(())
    }
}

macro_rules! make_spaces {
    () => { "" };
    ($s: tt $($t: tt)*) => { concat!("          ", make_spaces!($($t)*)) };
}

pub(crate) const SPACES: &str = make_spaces!(,,,,,,,,,,);

/// Layout metrics for a payload with at least one embedded break.
///
/// Nonemptiness is separate from display width: a combining mark has zero
/// width but still commits pending whitespace and makes a line non-fresh.
#[derive(Clone, Copy)]
pub(crate) struct MultilineMetrics {
    /// Display width of the first physical line, excluding generated indentation and padding.
    pub first_width: usize,
    /// Maximum display width after the first break; these lines start at column zero.
    pub max_rest_width: usize,
    /// Display width of the final physical line, zero after a terminal break.
    pub last_width: usize,
    /// Whether the first line commits pending indentation and weak spaces.
    pub first_nonempty: bool,
    /// Whether the final line contains text and therefore leaves the column non-fresh.
    pub last_nonempty: bool,
}

/// The payload of [`Doc::MultilineText`](crate::Doc::MultilineText): one borrowed or owned source
/// string plus a fixed-size summary of its line widths and boundary content.
///
/// The type is opaque; documents are built through [`crate::DocAllocator::multiline_text`]. The
/// complete source, including raw input breaks (`"\n"`, `"\r\n"` or a lone `"\r"`), is retained
/// and emitted verbatim, leaving [`RenderOptions::line_ending`](crate::RenderOptions::line_ending)
/// to structural breaks. Line indexes can be derived from the source when needed.
#[derive(Clone)]
pub struct MultilineTextData<'a> {
    /// The original source, including its whitespace and terminator bytes.
    text: Text<'a>,
    /// The layout summary, independent of the number of input lines.
    metrics: MultilineMetrics,
}

impl fmt::Debug for MultilineTextData<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_str().fmt(f)
    }
}

impl<'a> MultilineTextData<'a> {
    /// Retains the source and summarizes its physical line widths for layout.
    ///
    /// The source must contain at least one LF or CR byte. A terminal break
    /// leaves an empty final line for column tracking. CRLF contributes the
    /// same metrics as one break, while its bytes remain in the source.
    /// [`crate::DocAllocator::multiline_text`] handles empty and break-free
    /// inputs before constructing this payload.
    ///
    /// # Panics
    ///
    /// Panics in debug builds if the source contains no LF or CR byte.
    pub(crate) fn new(text: Text<'a>) -> Self {
        let mut metrics = MultilineMetrics {
            first_width: 0,
            max_rest_width: 0,
            last_width: 0,
            first_nonempty: false,
            last_nonempty: false,
        };
        let mut start = 0;
        // ASCII delimiter positions are UTF-8 boundaries. The empty segment
        // inside CRLF cannot affect the widths or boundary content.
        for (index, byte) in text.bytes().enumerate() {
            if byte != b'\r' && byte != b'\n' {
                continue;
            }
            let line = &text[start..index];
            let width = display_width(line);
            if start == 0 {
                metrics.first_width = width;
                metrics.first_nonempty = !line.is_empty();
            } else {
                metrics.max_rest_width = metrics.max_rest_width.max(width);
            }
            start = index + 1;
        }
        debug_assert!(start > 0, "multiline payload must contain a break");
        let last = &text[start..];
        metrics.last_width = display_width(last);
        metrics.max_rest_width = metrics.max_rest_width.max(metrics.last_width);
        metrics.last_nonempty = !last.is_empty();
        Self { text, metrics }
    }

    /// The full source string, including terminators.
    pub fn as_str(&self) -> &str {
        &self.text
    }

    /// Returns the cached summary without scanning or copying the source.
    pub(crate) fn metrics(&self) -> &MultilineMetrics {
        &self.metrics
    }
}

/// Width of a text under the rules used for text documents: byte length for ASCII, no tab
/// expansion otherwise.
fn display_width(text: &str) -> usize {
    if text.is_ascii() {
        text.len()
    } else {
        UnicodeWidthStr::width(text)
    }
}
