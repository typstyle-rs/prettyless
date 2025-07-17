use std::{borrow::Cow, fmt, ops::Deref};

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
