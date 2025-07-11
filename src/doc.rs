use std::{borrow::Cow, fmt, ops::Deref, rc::Rc};

use crate::{
    text::Text, visitor::visit_sequence_deep, BoxAllocator, DocAllocator, DocBuilder, Pretty,
    RcAllocator,
};

pub trait DocPtr<'a>: Deref<Target = Doc<'a, Self>> + Sized {
    type ColumnFn: Deref<Target = dyn Fn(usize) -> Self + 'a> + Clone + 'a;
    type WidthFn: Deref<Target = dyn Fn(isize) -> Self + 'a> + Clone + 'a;
}

pub trait StaticDoc<'a>: DocPtr<'a> {
    type Allocator: DocAllocator<'a, Doc = Self> + 'static;
    const ALLOCATOR: &'static Self::Allocator;
}

/// The concrete document type. This type is not meant to be used directly. Instead use the static
/// functions on `Doc` or the methods on an `DocAllocator`.
///
/// The `T` parameter is used to abstract over pointers to `Doc`. See `RefDoc` and `BoxDoc` for how
/// it is used
#[derive(Clone)]
pub enum Doc<'a, T>
where
    T: DocPtr<'a>,
{
    // Primitives
    Nil,
    Fail,

    // Texts
    Text(Text<'a>),
    TextWithLen(usize, T), // Stores the length of a string document that is not just ascii
    HardLine,

    // Structural
    Append(T, T), // sequencing

    // Indentation and Alignment
    Nest(isize, T),  // Changes the indentation level
    DedentToRoot(T), // Dedent to the root level, which is always 0
    Align(T),        // Align to the current position

    // Choices
    ExpandParent,       // make the parent group break
    Flatten(T),         // always flat inside
    BreakOrFlat(T, T),  // break vs flat
    Group(T),           // try flat vs broken
    Union(T, T),        // alternative layouts
    PartialUnion(T, T), // like union, but only fit on the first line

    // Contextual
    OnColumn(T::ColumnFn),
    OnNesting(T::ColumnFn),
}

impl<'a, T> Doc<'a, T>
where
    T: StaticDoc<'a>,
{
    /// The text `t.to_string()`.
    ///
    /// The given text must not contain line breaks.
    #[inline]
    pub fn as_string<U: fmt::Display>(data: U) -> Self {
        T::ALLOCATOR.as_string(data).into_plain_doc()
    }

    /// The given text, which must not contain line breaks.
    #[inline]
    pub fn text<U: Into<Cow<'a, str>>>(data: U) -> Self {
        T::ALLOCATOR.text(data).into_plain_doc()
    }

    fn flat_alt<D>(self, doc: D) -> Self
    where
        D: Pretty<'a, T::Allocator>,
    {
        DocBuilder(T::ALLOCATOR, self.into())
            .flat_alt(doc)
            .into_plain_doc()
    }
}

impl<'a, T> Default for Doc<'a, T>
where
    T: DocPtr<'a>,
{
    fn default() -> Self {
        Self::Nil
    }
}

impl<'a, T, S> From<S> for Doc<'a, T>
where
    T: StaticDoc<'a>,
    S: Into<Cow<'a, str>>,
{
    fn from(s: S) -> Doc<'a, T> {
        Doc::text(s)
    }
}

impl<'a, T> fmt::Debug for Doc<'a, T>
where
    T: DocPtr<'a> + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let write_compact = |f: &mut fmt::Formatter<'_>, doc: &T, name: &str| {
            if matches!(**doc, Doc::Append(_, _)) {
                f.write_str(name)?;
                f.write_str("(")?;
                doc.fmt(f)?;
                f.write_str(")")
            } else {
                f.debug_tuple(name).field(doc).finish()
            }
        };

        match self {
            Doc::Nil => f.write_str("Nil"),
            Doc::Fail => f.write_str("Fail"),

            Doc::HardLine => f.write_str("HardLine"),
            Doc::TextWithLen(_, d) => d.fmt(f),
            Doc::Text(s) => s.fmt(f),

            Doc::Append(..) => {
                let mut f = f.debug_list();
                visit_sequence_deep(self, &mut |doc| {
                    f.entry(doc);
                });
                f.finish()
            }
            Doc::Nest(off, ref doc) => {
                write!(f, "Nest({off}, ",)?;
                doc.fmt(f)?;
                write!(f, ")")
            }
            Doc::DedentToRoot(ref doc) => write_compact(f, doc, "DedentToRoot"),
            Doc::Align(ref doc) => write_compact(f, doc, "Align"),

            Doc::ExpandParent => f.write_str("ExpandParent"),
            Doc::Flatten(ref doc) => write_compact(f, doc, "Flatten"),
            Doc::BreakOrFlat(ref x, ref y) => match (&**x, &**y) {
                (Doc::HardLine, Doc::Text(Text::Borrowed(" "))) => f.write_str("LineOrSpace"),
                (Doc::HardLine, Doc::Nil) => f.write_str("LineOrNil"),
                (_, Doc::Nil) => f.debug_tuple("WhenBreak").field(x).finish(),
                (Doc::Nil, _) => f.debug_tuple("WhenFlat").field(y).finish(),
                _ => f.debug_tuple("FlatOrBreak").field(y).field(x).finish(),
            },
            Doc::Group(ref doc) => match &**doc {
                Doc::BreakOrFlat(x, y)
                    if matches!(
                        (&**x, &**y),
                        (Doc::HardLine, Doc::Text(Text::Borrowed(" ")))
                    ) =>
                {
                    f.write_str("SoftLineOrSpace")
                }
                Doc::BreakOrFlat(x, y) if matches!((&**x, &**y), (Doc::HardLine, Doc::Nil)) => {
                    f.write_str("SoftLineOrNil")
                }
                _ => write_compact(f, doc, "Group"),
            },
            Doc::Union(ref l, ref r) => f.debug_tuple("Union").field(l).field(r).finish(),
            Doc::PartialUnion(ref l, ref r) => {
                f.debug_tuple("PartialUnion").field(l).field(r).finish()
            }

            Doc::OnColumn(_) => f.write_str("OnColumn(..)"),
            Doc::OnNesting(_) => f.write_str("OnNesting(..)"),
        }
    }
}

macro_rules! impl_doc {
    ($name: ident, $ptr: ident, $allocator: ident) => {
        #[derive(Clone)]
        pub struct $name<'a>($ptr<Doc<'a, $name<'a>>>);

        impl<'a> fmt::Debug for $name<'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }

        impl<'a> $name<'a> {
            pub fn new(doc: Doc<'a, $name<'a>>) -> $name<'a> {
                $name($ptr::new(doc))
            }
        }

        impl<'a> From<Doc<'a, Self>> for $name<'a> {
            fn from(doc: Doc<'a, $name<'a>>) -> $name<'a> {
                $name::new(doc)
            }
        }

        impl<'a> Deref for $name<'a> {
            type Target = Doc<'a, $name<'a>>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl<'a> DocAllocator<'a> for $allocator
        where

        {
            type Doc = $name<'a>;

            #[inline]
            fn alloc(&'a self, doc: Doc<'a, Self::Doc>) -> Self::Doc {
                $name::new(doc)
            }
            fn alloc_column_fn(
                &'a self,
                f: impl Fn(usize) -> Self::Doc + 'a,
            ) -> <Self::Doc as DocPtr<'a>>::ColumnFn {
                Rc::new(f)
            }
            fn alloc_width_fn(
                &'a self,
                f: impl Fn(isize) -> Self::Doc + 'a,
            ) -> <Self::Doc as DocPtr<'a>>::WidthFn {
                Rc::new(f)
            }
        }

        impl<'a> DocPtr<'a> for $name<'a> {
            type ColumnFn = std::rc::Rc<dyn Fn(usize) -> Self + 'a>;
            type WidthFn = std::rc::Rc<dyn Fn(isize) -> Self + 'a>;
        }

        impl<'a> StaticDoc<'a> for $name<'a> {
            type Allocator = $allocator;
            const ALLOCATOR: &'static Self::Allocator = &$allocator;
        }

        impl_doc_methods!($name ('a) where () where ());

        impl<'a> $name<'a> {
            /// The text `t.to_string()`.
            ///
            /// The given text must not contain line breaks.
            #[inline]
            pub fn as_string<U: fmt::Display>(data: U) -> Self {
                $allocator.as_string(data).into_doc()
            }

            /// The given text, which must not contain line breaks.
            #[inline]
            pub fn text<U: Into<Cow<'a, str>>>(data: U) -> Self {
                $allocator.text(data).into_doc()
            }

            #[inline]
            pub fn softline() -> Self {
                Self::line().group()
            }

            /// A `softline_` acts like `nil` if the document fits the page, otherwise like `line_`
            #[inline]
            pub fn softline_() -> Self {
                Self::line_().group()
            }

            /// Append the given document after this document.
            #[inline]
            pub fn append<D>(self, that: D) -> Self
            where
                D: Pretty<'a, $allocator>,
            {
                DocBuilder(&$allocator, self.into()).append(that).into_doc()
            }

            /// A single document concatenating all the given documents.
            #[inline]
            pub fn concat<I>(docs: I) -> Self
            where
                I: IntoIterator,
                I::Item: Pretty<'a, $allocator>,
            {
                $allocator.concat(docs).into_doc()
            }

            /// A single document interspersing the given separator `S` between the given documents.  For
            /// example, if the documents are `[A, B, C, ..., Z]`, this yields `[A, S, B, S, C, S, ..., S, Z]`.
            ///
            /// Compare [the `intersperse` method from the `itertools` crate](https://docs.rs/itertools/0.5.9/itertools/trait.Itertools.html#method.intersperse).
            ///
            /// NOTE: The separator type, `S` may need to be cloned. Consider using cheaply cloneable ptr
            /// like `RefDoc` or `RcDoc`
            #[inline]
            pub fn intersperse<I, S>(docs: I, separator: S) -> Self
            where
                I: IntoIterator,
                I::Item: Pretty<'a, $allocator>,
                S: Pretty<'a, $allocator> + Clone,
            {
                $allocator.intersperse(docs, separator).into_doc()
            }

            /// Acts as `self` when laid out on multiple lines and acts as `that` when laid out on a single line.
            #[inline]
            pub fn flat_alt<D>(self, doc: D) -> Self
            where
                D: Pretty<'a, $allocator>,
            {
                DocBuilder(&$allocator, self.into())
                    .flat_alt(doc)
                    .into_doc()
            }

            /// Mark this document as a group.
            ///
            /// Groups are layed out on a single line if possible.  Within a group, all basic documents with
            /// several possible layouts are assigned the same layout, that is, they are all layed out
            /// horizontally and combined into a one single line, or they are each layed out on their own
            /// line.
            #[inline]
            pub fn group(self) -> Self {
                DocBuilder(&$allocator, self.into()).group().into_doc()
            }

            /// Increase the indentation level of this document.
            #[inline]
            pub fn nest(self, offset: isize) -> Self {
                DocBuilder(&$allocator, self.into()).nest(offset).into_doc()
            }

            #[inline]
            pub fn union<D>(self, other: D) -> Self
            where
                D: Into<BuildDoc<'a, Self>>,
            {
                DocBuilder(&$allocator, self.into()).union(other).into_doc()
            }

            #[inline]
            pub fn column(f: impl Fn(usize) -> Self + 'static) -> Self {
                DocBuilder(&$allocator, Doc::OnColumn($allocator.alloc_column_fn(f)).into()).into_doc()
            }

            #[inline]
            pub fn nesting(f: impl Fn(usize) -> Self + 'static) -> Self {
                DocBuilder(&$allocator, Doc::OnNesting($allocator.alloc_column_fn(f)).into()).into_doc()
            }
        }
    };
}

macro_rules! impl_doc_methods {
    ($name: ident ( $($params: tt)* ) where ( $($where_: tt)* ) where ( $($where_2: tt)* )) => {
        impl< $($params)* > $name< $($params)* >
            where $($where_)*
        {
            /// An empty document.
            #[inline]
            pub fn nil() -> Self {
                Doc::Nil.into()
            }

            #[inline]
            pub fn fail() -> Self {
                Doc::Fail.into()
            }

            /// A single hardline.
            #[inline]
            pub fn hardline() -> Self {
                Doc::HardLine.into()
            }

            #[inline]
            pub fn space() -> Self {
                Doc::Text(Text::Borrowed(" ")).into()
            }

            /// Make the parent group break
            #[inline]
            pub fn expand_parent() -> Self {
                Doc::ExpandParent.into()
            }
        }

        impl< $($params)* > $name< $($params)* >
            where $($where_2)*
        {
            /// A line acts like a `\n` but behaves like `space` if it is grouped on a single line.
            #[inline]
            pub fn line() -> Self {
                Self::hardline().flat_alt(Self::space()).into()
            }

            /// Acts like `line` but behaves like `nil` if grouped on a single line
            #[inline]
            pub fn line_() -> Self {
                Self::hardline().flat_alt(Self::nil()).into()
            }
        }
    };
}

impl_doc!(BoxDoc, Box, BoxAllocator);
impl_doc!(RcDoc, Rc, RcAllocator);

impl_doc_methods!(Doc ('a, D) where (D: DocPtr<'a>) where (D: StaticDoc<'a>));
impl_doc_methods!(BuildDoc ('a, D) where (D: DocPtr<'a>) where (D: StaticDoc<'a>));

/// Newtype wrapper for `&Doc`
pub struct RefDoc<'a>(pub &'a Doc<'a, RefDoc<'a>>);

impl<'a> DocPtr<'a> for RefDoc<'a> {
    type ColumnFn = &'a (dyn Fn(usize) -> Self + 'a);
    type WidthFn = &'a (dyn Fn(isize) -> Self + 'a);
}

impl Copy for RefDoc<'_> {}
impl Clone for RefDoc<'_> {
    fn clone(&self) -> Self {
        *self
    }
}

impl fmt::Debug for RefDoc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a> Deref for RefDoc<'a> {
    type Target = Doc<'a, RefDoc<'a>>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

/// Either a `Doc` or a pointer to a `Doc` (`D`)
#[derive(Clone)]
pub enum BuildDoc<'a, D>
where
    D: DocPtr<'a>,
{
    DocPtr(D),
    Doc(Doc<'a, D>),
}

impl<'a, T> BuildDoc<'a, T>
where
    T: StaticDoc<'a>,
{
    /// The text `t.to_string()`.
    ///
    /// The given text must not contain line breaks.
    #[inline]
    pub fn as_string<U: fmt::Display>(data: U) -> Self {
        T::ALLOCATOR.as_string(data).1
    }

    /// The given text, which must not contain line breaks.
    #[inline]
    pub fn text<U: Into<Cow<'a, str>>>(data: U) -> Self {
        T::ALLOCATOR.text(data).1
    }

    fn flat_alt<D>(self, doc: D) -> Self
    where
        D: Pretty<'a, T::Allocator>,
    {
        DocBuilder(T::ALLOCATOR, self).flat_alt(doc).1
    }
}

impl<'a, D> Default for BuildDoc<'a, D>
where
    D: DocPtr<'a>,
{
    fn default() -> Self {
        Self::Doc(Doc::default())
    }
}

impl<'a, D> fmt::Debug for BuildDoc<'a, D>
where
    D: DocPtr<'a> + fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<'a, D> Deref for BuildDoc<'a, D>
where
    D: DocPtr<'a>,
{
    type Target = Doc<'a, D>;
    fn deref(&self) -> &Self::Target {
        match self {
            BuildDoc::DocPtr(d) => d,
            BuildDoc::Doc(d) => d,
        }
    }
}

impl<'a> From<RefDoc<'a>> for BuildDoc<'a, RefDoc<'a>> {
    fn from(s: RefDoc<'a>) -> Self {
        BuildDoc::DocPtr(s)
    }
}

impl<'a> From<BoxDoc<'a>> for BuildDoc<'a, BoxDoc<'a>> {
    fn from(s: BoxDoc<'a>) -> Self {
        BuildDoc::DocPtr(s)
    }
}

impl<'a> From<RcDoc<'a>> for BuildDoc<'a, RcDoc<'a>> {
    fn from(s: RcDoc<'a>) -> Self {
        BuildDoc::DocPtr(s)
    }
}

impl<'a, T> From<Doc<'a, T>> for BuildDoc<'a, T>
where
    T: DocPtr<'a>,
{
    fn from(s: Doc<'a, T>) -> Self {
        BuildDoc::Doc(s)
    }
}

impl<'a, T> From<String> for BuildDoc<'a, T>
where
    T: StaticDoc<'a>,
{
    fn from(s: String) -> Self {
        BuildDoc::Doc(Doc::text(s))
    }
}

impl<'a, T> From<&'a str> for BuildDoc<'a, T>
where
    T: StaticDoc<'a>,
{
    fn from(s: &'a str) -> Self {
        BuildDoc::Doc(Doc::text(s))
    }
}

impl<'a, T> From<&'a String> for BuildDoc<'a, T>
where
    T: StaticDoc<'a>,
{
    fn from(s: &'a String) -> Self {
        BuildDoc::Doc(Doc::text(s))
    }
}

impl<'a, T, S> From<Option<S>> for BuildDoc<'a, T>
where
    T: DocPtr<'a>,
    S: Into<BuildDoc<'a, T>>,
{
    fn from(s: Option<S>) -> Self {
        match s {
            Some(s) => s.into(),
            None => BuildDoc::Doc(Doc::Nil),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[cfg(target_pointer_width = "64")]
    #[test]
    fn doc_size() {
        // Safeguard against accidentally growing Doc
        assert_eq!(8 * 3, std::mem::size_of::<Doc<RefDoc>>());
    }

    #[test]
    fn debug_concat() {
        let a = Arena::new();
        let doc = (a.text("1") + a.text("2")) + a.text("3") + a.text("4");
        assert_eq!(
            format!("{doc:#?}"),
            r#"[
    "1",
    "2",
    "3",
    "4",
]"#
        )
    }
}
