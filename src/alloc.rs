use std::{borrow::Cow, fmt};

use crate::{BuildDoc, Doc, DocBuilder, DocPtr, Pretty, RefDoc, SmallText};

/// The `DocAllocator` trait abstracts over a type which can allocate (pointers to) `Doc`.
pub trait DocAllocator<'a, A = ()>
where
    A: 'a,
{
    type Doc: DocPtr<'a, A>;

    fn alloc(&'a self, doc: Doc<'a, Self::Doc, A>) -> Self::Doc;

    fn alloc_column_fn(
        &'a self,
        f: impl Fn(usize) -> Self::Doc + 'a,
    ) -> <Self::Doc as DocPtr<'a, A>>::ColumnFn;

    fn alloc_width_fn(
        &'a self,
        f: impl Fn(isize) -> Self::Doc + 'a,
    ) -> <Self::Doc as DocPtr<'a, A>>::WidthFn;

    fn alloc_cow(&'a self, doc: BuildDoc<'a, Self::Doc, A>) -> Self::Doc {
        match doc {
            BuildDoc::DocPtr(d) => d,
            BuildDoc::Doc(d) => self.alloc(d),
        }
    }

    /// Allocate an empty document.
    #[inline]
    fn nil(&'a self) -> DocBuilder<'a, Self, A> {
        DocBuilder(self, Doc::Nil.into())
    }

    /// Fails document rendering immediately.
    ///
    /// Primarily used to abort rendering inside the left side of `Union`
    #[inline]
    fn fail(&'a self) -> DocBuilder<'a, Self, A> {
        DocBuilder(self, Doc::Fail.into())
    }

    /// Allocate a single hardline.
    #[inline]
    fn hardline(&'a self) -> DocBuilder<'a, Self, A> {
        DocBuilder(self, Doc::Hardline.into())
    }

    #[inline]
    fn space(&'a self) -> DocBuilder<'a, Self, A> {
        self.text(" ")
    }

    /// A line acts like a `\n` but behaves like `space` if it is grouped on a single line.
    #[inline]
    fn line(&'a self) -> DocBuilder<'a, Self, A> {
        self.hardline().flat_alt(self.space())
    }

    /// Acts like `line` but behaves like `nil` if grouped on a single line
    ///
    /// ```
    /// use prettyless::{Doc, RcDoc};
    ///
    /// let doc = RcDoc::<()>::group(
    ///     RcDoc::text("(")
    ///         .append(
    ///             RcDoc::line_()
    ///                 .append(Doc::text("test"))
    ///                 .append(Doc::line())
    ///                 .append(Doc::text("test"))
    ///                 .nest(2),
    ///         )
    ///         .append(Doc::line_())
    ///         .append(Doc::text(")")),
    /// );
    /// assert_eq!(doc.pretty(5).to_string(), "(\n  test\n  test\n)");
    /// assert_eq!(doc.pretty(100).to_string(), "(test test)");
    /// ```
    #[inline]
    fn line_(&'a self) -> DocBuilder<'a, Self, A> {
        self.hardline().flat_alt(self.nil())
    }

    /// A `softline` acts like `space` if the document fits the page, otherwise like `line`
    #[inline]
    fn softline(&'a self) -> DocBuilder<'a, Self, A> {
        self.line().group()
    }

    /// A `softline_` acts like `nil` if the document fits the page, otherwise like `line_`
    #[inline]
    fn softline_(&'a self) -> DocBuilder<'a, Self, A> {
        self.line_().group()
    }

    /// Allocate a document containing the text `t.to_string()`.
    ///
    /// The given text must not contain line breaks.
    #[inline]
    fn as_string<U: fmt::Display>(&'a self, data: U) -> DocBuilder<'a, Self, A> {
        use std::fmt::Write;
        let mut buf = FmtText::Small(SmallText::new());
        write!(buf, "{data}").unwrap();
        let doc = match buf {
            FmtText::Small(b) => Doc::SmallText(b),
            FmtText::Large(b) => Doc::OwnedText(b.into()),
        };
        DocBuilder(self, doc.into()).with_utf8_len()
    }

    /// Allocate a document containing the given text.
    ///
    /// The given text must not contain line breaks.
    #[inline]
    fn text<U: Into<Cow<'a, str>>>(&'a self, data: U) -> DocBuilder<'a, Self, A> {
        let data: Cow<_> = data.into();
        let doc = if data.is_empty() {
            Doc::Nil.into()
        } else {
            match data {
                Cow::Owned(t) => Doc::OwnedText(t.into()).into(),
                Cow::Borrowed(t) => Doc::BorrowedText(t).into(),
            }
        };
        DocBuilder(self, doc).with_utf8_len()
    }

    /// Allocate a document concatenating the given documents.
    #[inline]
    fn concat<I>(&'a self, docs: I) -> DocBuilder<'a, Self, A>
    where
        I: IntoIterator,
        I::Item: Pretty<'a, Self, A>,
    {
        docs.into_iter().fold(self.nil(), |a, b| a.append(b))
    }

    /// Allocate a document that intersperses the given separator `S` between the given documents
    /// `[A, B, C, ..., Z]`, yielding `[A, S, B, S, C, S, ..., S, Z]`.
    ///
    /// Compare [the `intersperse` method from the `itertools` crate](https://docs.rs/itertools/0.5.9/itertools/trait.Itertools.html#method.intersperse).
    ///
    /// NOTE: The separator type, `S` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    #[inline]
    fn intersperse<I, S>(&'a self, docs: I, separator: S) -> DocBuilder<'a, Self, A>
    where
        I: IntoIterator,
        I::Item: Pretty<'a, Self, A>,
        S: Pretty<'a, Self, A> + Clone,
    {
        let mut result = self.nil();
        let mut iter = docs.into_iter();

        if let Some(first) = iter.next() {
            result = result.append(first);

            for doc in iter {
                result = result.append(separator.clone());
                result = result.append(doc);
            }
        }

        result
    }

    /// Allocate a document that acts differently based on the position and page layout
    ///
    /// ```rust
    /// use prettyless::DocAllocator;
    ///
    /// let arena = prettyless::Arena::<()>::new();
    /// let doc = arena.text("prefix ")
    ///     .append(arena.column(|l| {
    ///         arena.text("| <- column ").append(arena.as_string(l)).into_doc()
    ///     }));
    /// assert_eq!(doc.1.pretty(80).to_string(), "prefix | <- column 7");
    /// ```
    #[inline]
    fn column(&'a self, f: impl Fn(usize) -> Self::Doc + 'a) -> DocBuilder<'a, Self, A> {
        DocBuilder(self, Doc::Column(self.alloc_column_fn(f)).into())
    }

    /// Allocate a document that acts differently based on the current nesting level
    ///
    /// ```rust
    /// use prettyless::DocAllocator;
    ///
    /// let arena = prettyless::Arena::<()>::new();
    /// let doc = arena.text("prefix ")
    ///     .append(arena.nesting(|l| {
    ///         arena.text("[Nested: ").append(arena.as_string(l)).append("]").into_doc()
    ///     }).nest(4));
    /// assert_eq!(doc.1.pretty(80).to_string(), "prefix [Nested: 4]");
    /// ```
    #[inline]
    fn nesting(&'a self, f: impl Fn(usize) -> Self::Doc + 'a) -> DocBuilder<'a, Self, A> {
        DocBuilder(self, Doc::Nesting(self.alloc_column_fn(f)).into())
    }

    /// Reflows `text` inserting `softline` in place of any whitespace
    #[inline]
    fn reflow(&'a self, text: &'a str) -> DocBuilder<'a, Self, A>
    where
        Self: Sized,
        Self::Doc: Clone,
        A: Clone,
    {
        self.intersperse(text.split(char::is_whitespace), self.softline())
    }
}

pub struct BoxAllocator;

pub struct RcAllocator;

trait DropT {}
impl<T> DropT for T {}

/// An arena which can be used to allocate `Doc` values.
pub struct Arena<'a, A = ()> {
    docs: typed_arena::Arena<Doc<'a, RefDoc<'a, A>, A>>,
    column_fns: typed_arena::Arena<Box<dyn DropT>>,
}

impl<A> Default for Arena<'_, A> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, A> Arena<'a, A> {
    pub fn new() -> Self {
        Arena {
            docs: typed_arena::Arena::new(),
            column_fns: Default::default(),
        }
    }

    fn alloc_any<T>(&'a self, f: T) -> &'a T
    where
        T: 'a,
    {
        let f = Box::new(f);
        let f_ptr = &*f as *const T;
        // Until #[may_dangle] https://github.com/rust-lang/rust/issues/34761 is stabilized (or
        // equivalent) we need to use unsafe to cast away the lifetime of the function as we do not
        // have any other way of asserting that the `typed_arena::Arena` destructor does not touch
        // `'a`
        //
        // Since `'a` is used elsewhere in our `Arena` type we still have all the other lifetime
        // checks in place (the other arena stores no `Drop` value which touches `'a` which lets it
        // compile)
        unsafe {
            self.column_fns
                .alloc(std::mem::transmute::<Box<dyn DropT>, Box<dyn DropT>>(f));
            &*f_ptr
        }
    }
}

impl<'a, D, A> DocAllocator<'a, A> for &'a D
where
    D: ?Sized + DocAllocator<'a, A>,
    A: 'a,
{
    type Doc = D::Doc;

    #[inline]
    fn alloc(&'a self, doc: Doc<'a, Self::Doc, A>) -> Self::Doc {
        (**self).alloc(doc)
    }

    fn alloc_column_fn(
        &'a self,
        f: impl Fn(usize) -> Self::Doc + 'a,
    ) -> <Self::Doc as DocPtr<'a, A>>::ColumnFn {
        (**self).alloc_column_fn(f)
    }

    fn alloc_width_fn(
        &'a self,
        f: impl Fn(isize) -> Self::Doc + 'a,
    ) -> <Self::Doc as DocPtr<'a, A>>::WidthFn {
        (**self).alloc_width_fn(f)
    }
}

impl<'a, A> DocAllocator<'a, A> for Arena<'a, A> {
    type Doc = RefDoc<'a, A>;

    #[inline]
    fn alloc(&'a self, doc: Doc<'a, Self::Doc, A>) -> Self::Doc {
        RefDoc(match doc {
            // Return 'static references for common variants to avoid some allocations
            Doc::Nil => &Doc::Nil,
            Doc::Hardline => &Doc::Hardline,
            Doc::Fail => &Doc::Fail,
            // line()
            Doc::FlatAlt(RefDoc(Doc::Hardline), RefDoc(Doc::BorrowedText(" "))) => {
                &Doc::FlatAlt(RefDoc(&Doc::Hardline), RefDoc(&Doc::BorrowedText(" ")))
            }
            // line_()
            Doc::FlatAlt(RefDoc(Doc::Hardline), RefDoc(Doc::Nil)) => {
                &Doc::FlatAlt(RefDoc(&Doc::Hardline), RefDoc(&Doc::Nil))
            }
            // softline()
            Doc::Group(RefDoc(Doc::FlatAlt(
                RefDoc(Doc::Hardline),
                RefDoc(Doc::BorrowedText(" ")),
            ))) => &Doc::Group(RefDoc(&Doc::FlatAlt(
                RefDoc(&Doc::Hardline),
                RefDoc(&Doc::BorrowedText(" ")),
            ))),
            // softline_()
            Doc::Group(RefDoc(Doc::FlatAlt(RefDoc(Doc::Hardline), RefDoc(Doc::Nil)))) => {
                &Doc::Group(RefDoc(&Doc::FlatAlt(
                    RefDoc(&Doc::Hardline),
                    RefDoc(&Doc::Nil),
                )))
            }
            _ => self.docs.alloc(doc),
        })
    }

    fn alloc_column_fn(
        &'a self,
        f: impl Fn(usize) -> Self::Doc + 'a,
    ) -> <Self::Doc as DocPtr<'a, A>>::ColumnFn {
        self.alloc_any(f)
    }

    fn alloc_width_fn(
        &'a self,
        f: impl Fn(isize) -> Self::Doc + 'a,
    ) -> <Self::Doc as DocPtr<'a, A>>::WidthFn {
        self.alloc_any(f)
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
