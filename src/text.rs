use std::{fmt, ops::Deref};

type SmallText = arrayvec::ArrayString<[u8; 22]>;

#[derive(Clone)]
pub enum Text<'a> {
    Owned(Box<str>),
    Borrowed(&'a str),
    Small(SmallText),
}

impl Text<'_> {
    pub fn as_str(&self) -> &str {
        match self {
            Text::Owned(s) => s,
            Text::Borrowed(s) => s,
            Text::Small(s) => s,
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
            Text::Owned(s) => s,
            Text::Borrowed(s) => s,
            Text::Small(s) => s,
        }
    }
}

impl<T> From<T> for Text<'_>
where
    T: fmt::Display,
{
    fn from(value: T) -> Self {
        use std::fmt::Write;
        let mut buf = FmtText::Small(SmallText::new());
        write!(buf, "{value}").unwrap();
        match buf {
            FmtText::Small(b) => Text::Small(b),
            FmtText::Large(b) => Text::Owned(b.into()),
        }
    }
}

enum FmtText {
    Small(SmallText),
    Large(String),
}

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
