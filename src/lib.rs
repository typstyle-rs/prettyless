//! # prettyless (a fork of pretty.rs)
//!
//! Original work Copyright (c) 2014 Jonathan Sterling and Darin Morrison
//! Modifications Copyright (c) 2025 QuadnucYard
//!
//! This crate defines a
//! [Wadler-style](http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf)
//! pretty-printing API.
//!
//! Start with the static functions of [Doc](enum.Doc.html).
//!
//! ## Quick start
//!
//! Let's pretty-print simple sexps!  We want to pretty print sexps like
//!
//! ```lisp
//! (1 2 3)
//! ```
//! or, if the line would be too long, like
//!
//! ```lisp
//! ((1)
//!  (2 3)
//!  (4 5 6))
//! ```
//!
//! A _simple symbolic expression_ consists of a numeric _atom_ or a nested ordered _list_ of
//! symbolic expression children.
//!
//! ```rust
//! # use prettyless::*;
//! enum SExp {
//!     Atom(u32),
//!     List(Vec<SExp>),
//! }
//! use SExp::*;
//! # fn main() { }
//! ```
//!
//! We define a simple conversion to a [Doc](enum.Doc.html).  Atoms are rendered as strings; lists
//! are recursively rendered, with spaces between children where appropriate.  Children are
//! [nested]() and [grouped](), allowing them to be laid out in a single line as appropriate.
//!
//! ```rust
//! # use prettyless::*;
//! # enum SExp {
//! #     Atom(u32),
//! #     List(Vec<SExp>),
//! # }
//! # use SExp::*;
//! impl SExp {
//!     /// Return a pretty printed format of self.
//!     pub fn to_doc(&self) -> RcDoc {
//!         match *self {
//!             Atom(ref x) => RcDoc::as_string(x),
//!             List(ref xs) =>
//!                 RcDoc::text("(")
//!                     .append(RcDoc::intersperse(xs.into_iter().map(|x| x.to_doc()), Doc::line()).nest(1).group())
//!                     .append(RcDoc::text(")"))
//!         }
//!     }
//! }
//! # fn main() { }
//! ```
//!
//! Next, we convert the [Doc](enum.Doc.html) to a plain old string.
//!
//! ```rust
//! # use prettyless::*;
//! # enum SExp {
//! #     Atom(u32),
//! #     List(Vec<SExp>),
//! # }
//! # use SExp::*;
//! # impl SExp {
//! #     /// Return a pretty printed format of self.
//! #     pub fn to_doc(&self) -> BoxDoc {
//! #         match *self {
//! #             Atom(ref x) => BoxDoc::as_string(x),
//! #             List(ref xs) =>
//! #                 BoxDoc::text("(")
//! #                     .append(BoxDoc::intersperse(xs.into_iter().map(|x| x.to_doc()), Doc::line()).nest(1).group())
//! #                     .append(BoxDoc::text(")"))
//! #         }
//! #     }
//! # }
//! impl SExp {
//!     pub fn to_pretty(&self, width: usize) -> String {
//!         let mut w = Vec::new();
//!         self.to_doc().render(width, &mut w).unwrap();
//!         String::from_utf8(w).unwrap()
//!     }
//! }
//! # fn main() { }
//! ```
//!
//! And finally we can test that the nesting and grouping behaves as we expected.
//!
//! ```rust
//! # use prettyless::*;
//! # enum SExp {
//! #     Atom(u32),
//! #     List(Vec<SExp>),
//! # }
//! # use SExp::*;
//! # impl SExp {
//! #     /// Return a pretty printed format of self.
//! #     pub fn to_doc(&self) -> BoxDoc {
//! #         match *self {
//! #             Atom(ref x) => BoxDoc::as_string(x),
//! #             List(ref xs) =>
//! #                 BoxDoc::text("(")
//! #                     .append(BoxDoc::intersperse(xs.into_iter().map(|x| x.to_doc()), Doc::line()).nest(1).group())
//! #                     .append(BoxDoc::text(")"))
//! #         }
//! #     }
//! # }
//! # impl SExp {
//! #     pub fn to_pretty(&self, width: usize) -> String {
//! #         let mut w = Vec::new();
//! #         self.to_doc().render(width, &mut w).unwrap();
//! #         String::from_utf8(w).unwrap()
//! #     }
//! # }
//! # fn main() {
//! let atom = SExp::Atom(5);
//! assert_eq!("5", atom.to_pretty(10));
//! let list = SExp::List(vec![SExp::Atom(1), SExp::Atom(2), SExp::Atom(3)]);
//! assert_eq!("(1 2 3)", list.to_pretty(10));
//! assert_eq!("\
//! (1
//!  2
//!  3)", list.to_pretty(5));
//! # }
//! ```
//!
//! ## Advanced usage
//!
//! There's a more efficient pattern that uses the [DocAllocator](trait.DocAllocator.html) trait, as
//! implemented by [BoxAllocator](struct.BoxAllocator.html), to allocate
//! [DocBuilder](struct.DocBuilder.html) instances.  See
//! [examples/trees.rs](https://github.com/freebroccolo/pretty.rs/blob/master/examples/trees.rs#L39)
//! for this approach.

mod alloc;
mod builder;
mod doc;
mod render;
pub mod text;
pub(crate) mod visitor;

pub use alloc::{Arena, BoxAllocator, DocAllocator, RcAllocator};
pub use builder::DocBuilder;
pub use doc::{BoxDoc, BuildDoc, Doc, DocPtr, RcDoc, RefDoc};
pub use render::{FmtWrite, IoWrite, Render};

/// Trait for types which can be converted to a `Document`
pub trait Pretty<'a, D>
where
    D: ?Sized + DocAllocator<'a>,
{
    /// Converts `self` into a document
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D>;
}

impl<'a> Pretty<'a, BoxAllocator> for BoxDoc<'a> {
    fn pretty(self, allocator: &'a BoxAllocator) -> DocBuilder<'a, BoxAllocator> {
        DocBuilder(allocator, self.into())
    }
}

impl<'a> Pretty<'a, RcAllocator> for RcDoc<'a> {
    fn pretty(self, allocator: &'a RcAllocator) -> DocBuilder<'a, RcAllocator> {
        DocBuilder(allocator, self.into())
    }
}

impl<'a> Pretty<'a, Arena<'a>> for RefDoc<'a> {
    fn pretty(self, allocator: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        DocBuilder(allocator, self.into())
    }
}

impl<'a, D> Pretty<'a, D> for BuildDoc<'a, D::Doc>
where
    D: ?Sized + DocAllocator<'a>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        DocBuilder(allocator, self)
    }
}

impl<'a, D> Pretty<'a, D> for Doc<'a, D::Doc>
where
    D: ?Sized + DocAllocator<'a>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        DocBuilder(allocator, self.into())
    }
}

impl<'a, D> Pretty<'a, D> for DocBuilder<'a, D>
where
    D: ?Sized + DocAllocator<'a>,
{
    fn pretty(self, _: &'a D) -> DocBuilder<'a, D> {
        self
    }
}

impl<'a, D, T> Pretty<'a, D> for Option<T>
where
    D: ?Sized + DocAllocator<'a>,
    T: Pretty<'a, D>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        match self {
            Some(x) => x.pretty(allocator),
            None => allocator.nil(),
        }
    }
}

impl<'a, D> Pretty<'a, D> for &'a str
where
    D: ?Sized + DocAllocator<'a>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        allocator.text(self)
    }
}

impl<'a, D> Pretty<'a, D> for &'a String
where
    D: ?Sized + DocAllocator<'a>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        self[..].pretty(allocator)
    }
}

impl<'a, D> Pretty<'a, D> for String
where
    D: ?Sized + DocAllocator<'a>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        allocator.text(self)
    }
}

impl<'a, D, S> Pretty<'a, D> for std::borrow::Cow<'a, S>
where
    D: ?Sized + DocAllocator<'a>,
    S: ?Sized + ToOwned,
    &'a S: Pretty<'a, D>,
    S::Owned: Pretty<'a, D>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        match self {
            std::borrow::Cow::Borrowed(s) => s.pretty(allocator),
            std::borrow::Cow::Owned(s) => s.pretty(allocator),
        }
    }
}

/// Concatenates a number of documents (or values that can be converted into a document via the
/// `Pretty` trait, like `&str`)
///
/// ```
/// use prettyless::{docs, Arena, DocAllocator};
/// let arena = &Arena::new();
/// let doc = docs![
///     arena,
///     "let",
///     arena.softline(),
///     "x",
///     arena.softline(),
///     "=",
///     arena.softline(),
///     Some("123"),
/// ];
/// assert_eq!(doc.1.print(80).to_string(), "let x = 123");
/// ```
#[macro_export]
macro_rules! docs {
    ($alloc: expr, $first: expr $(,)?) => {
        $crate::Pretty::pretty($first, $alloc)
    };
    ($alloc: expr, $first: expr $(, $rest: expr)+ $(,)?) => {{
        let mut doc = $crate::Pretty::pretty($first, $alloc);
        $(
            doc = doc.append($rest);
        )*
        doc
    }}
}
