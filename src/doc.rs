use std::{borrow::Cow, fmt, ops::Deref, rc::Rc};

use crate::{BoxAllocator, DocAllocator, DocBuilder, Pretty, RcAllocator, SmallText};

pub trait DocPtr<'a, A>: Deref<Target = Doc<'a, Self, A>> + Sized
where
    A: 'a,
{
    type ColumnFn: Deref<Target = dyn Fn(usize) -> Self + 'a> + Clone + 'a;
    type WidthFn: Deref<Target = dyn Fn(isize) -> Self + 'a> + Clone + 'a;
}

pub trait StaticDoc<'a, A>: DocPtr<'a, A>
where
    A: 'a,
{
    type Allocator: DocAllocator<'a, A, Doc = Self> + 'static;
    const ALLOCATOR: &'static Self::Allocator;
}

/// The concrete document type. This type is not meant to be used directly. Instead use the static
/// functions on `Doc` or the methods on an `DocAllocator`.
///
/// The `T` parameter is used to abstract over pointers to `Doc`. See `RefDoc` and `BoxDoc` for how
/// it is used
#[derive(Clone)]
pub enum Doc<'a, T, A = ()>
where
    T: DocPtr<'a, A>,
{
    Nil,
    Append(T, T),
    Group(T),
    FlatAlt(T, T),
    Nest(isize, T),
    Hardline,
    // Stores the length of a string document that is not just ascii
    RenderLen(usize, T),
    OwnedText(Box<str>),
    BorrowedText(&'a str),
    SmallText(SmallText),
    Annotated(A, T),
    Union(T, T),
    Column(T::ColumnFn),
    Nesting(T::ColumnFn),
    Fail,
}

impl<'a, T, A> Doc<'a, T, A>
where
    T: StaticDoc<'a, A>,
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
        D: Pretty<'a, T::Allocator, A>,
    {
        DocBuilder(T::ALLOCATOR, self.into())
            .flat_alt(doc)
            .into_plain_doc()
    }
}

impl<'a, T, A> Default for Doc<'a, T, A>
where
    T: DocPtr<'a, A>,
{
    fn default() -> Self {
        Self::Nil
    }
}

impl<'a, T, A, S> From<S> for Doc<'a, T, A>
where
    T: StaticDoc<'a, A>,
    S: Into<Cow<'a, str>>,
{
    fn from(s: S) -> Doc<'a, T, A> {
        Doc::text(s)
    }
}

impl<'a, T, A> fmt::Debug for Doc<'a, T, A>
where
    T: DocPtr<'a, A> + fmt::Debug,
    A: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let is_line = |doc: &Doc<'a, T, A>| match doc {
            Doc::FlatAlt(x, y) => {
                matches!((&**x, &**y), (Doc::Hardline, Doc::BorrowedText(" ")))
            }
            _ => false,
        };
        let is_line_ = |doc: &Doc<'a, T, A>| match doc {
            Doc::FlatAlt(x, y) => {
                matches!((&**x, &**y), (Doc::Hardline, Doc::Nil))
            }
            _ => false,
        };
        match self {
            Doc::Nil => f.debug_tuple("Nil").finish(),
            Doc::Append(..) => {
                let mut f = f.debug_list();
                append_docs(self, &mut |doc| {
                    f.entry(doc);
                });
                f.finish()
            }
            _ if is_line(self) => f.debug_tuple("Line").finish(),
            _ if is_line_(self) => f.debug_tuple("Line_").finish(),
            Doc::FlatAlt(ref x, ref y) => f.debug_tuple("FlatAlt").field(x).field(y).finish(),
            Doc::Group(ref doc) => {
                if is_line(self) {
                    return f.debug_tuple("SoftLine").finish();
                }
                if is_line_(self) {
                    return f.debug_tuple("SoftLine_").finish();
                }
                f.debug_tuple("Group").field(doc).finish()
            }
            Doc::Nest(off, ref doc) => f.debug_tuple("Nest").field(&off).field(doc).finish(),
            Doc::Hardline => f.debug_tuple("Hardline").finish(),
            Doc::RenderLen(_, d) => d.fmt(f),
            Doc::OwnedText(ref s) => s.fmt(f),
            Doc::BorrowedText(ref s) => s.fmt(f),
            Doc::SmallText(ref s) => s.fmt(f),
            Doc::Annotated(ref ann, ref doc) => {
                f.debug_tuple("Annotated").field(ann).field(doc).finish()
            }
            Doc::Union(ref l, ref r) => f.debug_tuple("Union").field(l).field(r).finish(),
            Doc::Column(_) => f.debug_tuple("Column(..)").finish(),
            Doc::Nesting(_) => f.debug_tuple("Nesting(..)").finish(),
            Doc::Fail => f.debug_tuple("Fail").finish(),
        }
    }
}

fn append_docs<'a, 'd, T, A>(
    mut doc: &'d Doc<'a, T, A>,
    consumer: &mut impl FnMut(&'d Doc<'a, T, A>),
) where
    T: DocPtr<'a, A>,
{
    loop {
        match doc {
            Doc::Append(l, r) => {
                append_docs(l, consumer);
                doc = r;
            }
            _ => break consumer(doc),
        }
    }
}

macro_rules! impl_doc {
    ($name: ident, $ptr: ident, $allocator: ident) => {
        #[derive(Clone)]
        pub struct $name<'a, A = ()>($ptr<Doc<'a, $name<'a, A>, A>>);

        impl<'a, A> fmt::Debug for $name<'a, A>
        where
            A: fmt::Debug,
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.0.fmt(f)
            }
        }

        impl<'a, A> $name<'a, A> {
            pub fn new(doc: Doc<'a, $name<'a, A>, A>) -> $name<'a, A> {
                $name($ptr::new(doc))
            }
        }

        impl<'a, A> From<Doc<'a, Self, A>> for $name<'a, A> {
            fn from(doc: Doc<'a, $name<'a, A>, A>) -> $name<'a, A> {
                $name::new(doc)
            }
        }

        impl<'a, A> Deref for $name<'a, A> {
            type Target = Doc<'a, $name<'a, A>, A>;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl<'a, A> DocAllocator<'a, A> for $allocator
        where
            A: 'a,
        {
            type Doc = $name<'a, A>;

            #[inline]
            fn alloc(&'a self, doc: Doc<'a, Self::Doc, A>) -> Self::Doc {
                $name::new(doc)
            }
            fn alloc_column_fn(
                &'a self,
                f: impl Fn(usize) -> Self::Doc + 'a,
            ) -> <Self::Doc as DocPtr<'a, A>>::ColumnFn {
                Rc::new(f)
            }
            fn alloc_width_fn(
                &'a self,
                f: impl Fn(isize) -> Self::Doc + 'a,
            ) -> <Self::Doc as DocPtr<'a, A>>::WidthFn {
                Rc::new(f)
            }
        }

        impl<'a, A> DocPtr<'a, A> for $name<'a, A> {
            type ColumnFn = std::rc::Rc<dyn Fn(usize) -> Self + 'a>;
            type WidthFn = std::rc::Rc<dyn Fn(isize) -> Self + 'a>;
        }

        impl<'a, A> StaticDoc<'a, A> for $name<'a, A> {
            type Allocator = $allocator;
            const ALLOCATOR: &'static Self::Allocator = &$allocator;
        }

        impl_doc_methods!($name ('a, A) where () where ());

        impl<'a, A> $name<'a, A> {
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

            /// Append the given document after this document.
            #[inline]
            pub fn append<D>(self, that: D) -> Self
            where
                D: Pretty<'a, $allocator, A>,
            {
                DocBuilder(&$allocator, self.into()).append(that).into_doc()
            }

            /// A single document concatenating all the given documents.
            #[inline]
            pub fn concat<I>(docs: I) -> Self
            where
                I: IntoIterator,
                I::Item: Pretty<'a, $allocator, A>,
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
                I::Item: Pretty<'a, $allocator, A>,
                S: Pretty<'a, $allocator, A> + Clone,
                A: Clone,
            {
                $allocator.intersperse(docs, separator).into_doc()
            }

            /// Acts as `self` when laid out on multiple lines and acts as `that` when laid out on a single line.
            #[inline]
            pub fn flat_alt<D>(self, doc: D) -> Self
            where
                D: Pretty<'a, $allocator, A>,
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
            pub fn annotate(self, ann: A) -> Self {
                DocBuilder(&$allocator, self.into())
                    .annotate(ann)
                    .into_doc()
            }

            #[inline]
            pub fn union<D>(self, other: D) -> Self
            where
                D: Into<BuildDoc<'a, Self, A>>,
            {
                DocBuilder(&$allocator, self.into()).union(other).into_doc()
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

            #[inline]
            pub fn column(f: impl Fn(usize) -> Self + 'static) -> Self {
                DocBuilder(&$allocator, Doc::Column($allocator.alloc_column_fn(f)).into()).into_doc()
            }

            #[inline]
            pub fn nesting(f: impl Fn(usize) -> Self + 'static) -> Self {
                DocBuilder(&$allocator, Doc::Nesting($allocator.alloc_column_fn(f)).into()).into_doc()
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

            /// A single hardline.
            #[inline]
            pub fn hardline() -> Self {
                Doc::Hardline.into()
            }

            #[inline]
            pub fn space() -> Self {
                Doc::BorrowedText(" ").into()
            }

            #[inline]
            pub fn fail() -> Self {
                Doc::Fail.into()
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

impl_doc_methods!(Doc ('a, D, A) where (D: DocPtr<'a, A>) where (D: StaticDoc<'a, A>));
impl_doc_methods!(BuildDoc ('a, D, A) where (D: DocPtr<'a, A>) where (D: StaticDoc<'a, A>));

/// Newtype wrapper for `&Doc`
pub struct RefDoc<'a, A = ()>(pub &'a Doc<'a, RefDoc<'a, A>, A>);

impl<'a, A> DocPtr<'a, A> for RefDoc<'a, A> {
    type ColumnFn = &'a (dyn Fn(usize) -> Self + 'a);
    type WidthFn = &'a (dyn Fn(isize) -> Self + 'a);
}

impl<A> Copy for RefDoc<'_, A> {}
impl<A> Clone for RefDoc<'_, A> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<A> fmt::Debug for RefDoc<'_, A>
where
    A: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a, A> Deref for RefDoc<'a, A> {
    type Target = Doc<'a, RefDoc<'a, A>, A>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

/// Either a `Doc` or a pointer to a `Doc` (`D`)
#[derive(Clone)]
pub enum BuildDoc<'a, D, A>
where
    D: DocPtr<'a, A>,
{
    DocPtr(D),
    Doc(Doc<'a, D, A>),
}

impl<'a, T, A> BuildDoc<'a, T, A>
where
    T: StaticDoc<'a, A>,
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
        D: Pretty<'a, T::Allocator, A>,
    {
        DocBuilder(T::ALLOCATOR, self).flat_alt(doc).1
    }
}

impl<'a, D, A> Default for BuildDoc<'a, D, A>
where
    D: DocPtr<'a, A>,
{
    fn default() -> Self {
        Self::Doc(Doc::default())
    }
}

impl<'a, D, A> fmt::Debug for BuildDoc<'a, D, A>
where
    D: DocPtr<'a, A> + fmt::Debug,
    A: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<'a, D, A> Deref for BuildDoc<'a, D, A>
where
    D: DocPtr<'a, A>,
{
    type Target = Doc<'a, D, A>;
    fn deref(&self) -> &Self::Target {
        match self {
            BuildDoc::DocPtr(d) => d,
            BuildDoc::Doc(d) => d,
        }
    }
}

impl<'a, A> From<RefDoc<'a, A>> for BuildDoc<'a, RefDoc<'a, A>, A> {
    fn from(s: RefDoc<'a, A>) -> Self {
        BuildDoc::DocPtr(s)
    }
}

impl<'a, A> From<BoxDoc<'a, A>> for BuildDoc<'a, BoxDoc<'a, A>, A> {
    fn from(s: BoxDoc<'a, A>) -> Self {
        BuildDoc::DocPtr(s)
    }
}

impl<'a, A> From<RcDoc<'a, A>> for BuildDoc<'a, RcDoc<'a, A>, A> {
    fn from(s: RcDoc<'a, A>) -> Self {
        BuildDoc::DocPtr(s)
    }
}

impl<'a, T, A> From<Doc<'a, T, A>> for BuildDoc<'a, T, A>
where
    T: DocPtr<'a, A>,
{
    fn from(s: Doc<'a, T, A>) -> Self {
        BuildDoc::Doc(s)
    }
}

impl<'a, T, A> From<String> for BuildDoc<'a, T, A>
where
    T: StaticDoc<'a, A>,
{
    fn from(s: String) -> Self {
        BuildDoc::Doc(Doc::text(s))
    }
}

impl<'a, T, A> From<&'a str> for BuildDoc<'a, T, A>
where
    T: StaticDoc<'a, A>,
{
    fn from(s: &'a str) -> Self {
        BuildDoc::Doc(Doc::text(s))
    }
}

impl<'a, T, A> From<&'a String> for BuildDoc<'a, T, A>
where
    T: StaticDoc<'a, A>,
{
    fn from(s: &'a String) -> Self {
        BuildDoc::Doc(Doc::text(s))
    }
}

impl<'a, T, A, S> From<Option<S>> for BuildDoc<'a, T, A>
where
    T: DocPtr<'a, A>,
    S: Into<BuildDoc<'a, T, A>>,
{
    fn from(s: Option<S>) -> Self {
        match s {
            Some(s) => s.into(),
            None => BuildDoc::Doc(Doc::Nil),
        }
    }
}
