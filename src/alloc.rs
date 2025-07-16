use std::{borrow::Cow, fmt};

use crate::{text::Text, BuildDoc, Doc, DocBuilder, DocPtr, Pretty, RefDoc};

/// The `DocAllocator` trait abstracts over a type which can allocate (pointers to) `Doc`.
pub trait DocAllocator<'a> {
    type Doc: DocPtr<'a>;

    fn alloc(&'a self, doc: Doc<'a, Self::Doc>) -> Self::Doc;

    #[cfg(feature = "contextual")]
    fn alloc_column_fn(
        &'a self,
        f: impl Fn(usize) -> Self::Doc + 'a,
    ) -> <Self::Doc as DocPtr<'a>>::ColumnFn;

    #[cfg(feature = "contextual")]
    fn alloc_width_fn(
        &'a self,
        f: impl Fn(isize) -> Self::Doc + 'a,
    ) -> <Self::Doc as DocPtr<'a>>::WidthFn;

    fn alloc_cow(&'a self, doc: BuildDoc<'a, Self::Doc>) -> Self::Doc {
        match doc {
            BuildDoc::DocPtr(d) => d,
            BuildDoc::Doc(d) => self.alloc(d),
        }
    }

    /// Allocate an empty document.
    #[inline]
    fn nil(&'a self) -> DocBuilder<'a, Self> {
        DocBuilder(self, Doc::Nil.into())
    }

    /// Fails document rendering immediately.
    ///
    /// Primarily used to abort rendering inside the left side of `Union`
    #[inline]
    fn fail(&'a self) -> DocBuilder<'a, Self> {
        DocBuilder(self, Doc::Fail.into())
    }

    /// Allocate a single hardline.
    #[inline]
    fn hardline(&'a self) -> DocBuilder<'a, Self> {
        DocBuilder(self, Doc::HardLine.into())
    }

    #[inline]
    fn space(&'a self) -> DocBuilder<'a, Self> {
        self.text(" ")
    }

    /// A line acts like a `\n` but behaves like `space` if it is grouped on a single line.
    #[inline]
    fn line(&'a self) -> DocBuilder<'a, Self> {
        self.hardline().flat_alt(self.space())
    }

    /// Acts like `line` but behaves like `nil` if grouped on a single line
    ///
    /// ```
    /// use prettyless::{Doc, RcDoc};
    ///
    /// let doc = RcDoc::group(
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
    /// assert_eq!(doc.print(5).to_string(), "(\n  test\n  test\n)");
    /// assert_eq!(doc.print(100).to_string(), "(test test)");
    /// ```
    #[inline]
    fn line_(&'a self) -> DocBuilder<'a, Self> {
        self.hardline().flat_alt(self.nil())
    }

    /// A `softline` acts like `space` if the document fits the page, otherwise like `line`
    #[inline]
    fn softline(&'a self) -> DocBuilder<'a, Self> {
        self.line().group()
    }

    /// A `softline_` acts like `nil` if the document fits the page, otherwise like `line_`
    #[inline]
    fn softline_(&'a self) -> DocBuilder<'a, Self> {
        self.line_().group()
    }

    /// Equivalent to `self.nil().flat_alt(doc.pretty(self))`
    #[inline]
    fn if_group_flat(&'a self, doc: impl Pretty<'a, Self>) -> DocBuilder<'a, Self> {
        self.nil().flat_alt(doc.pretty(self))
    }

    /// Equivalent to `doc.pretty(self).flat_alt(self.nil())`
    #[inline]
    fn if_group_break(&'a self, doc: impl Pretty<'a, Self>) -> DocBuilder<'a, Self> {
        doc.pretty(self).flat_alt(self.nil())
    }

    /// Make the parent group break
    ///
    /// ```
    /// use prettyless::DocAllocator;
    ///
    /// let arena = prettyless::Arena::new();
    /// let doc = (arena.line() + arena.expand_parent()).group();
    /// assert_eq!(doc.print(80).to_string(), "\n");
    /// ```
    #[inline]
    fn expand_parent(&'a self) -> DocBuilder<'a, Self> {
        DocBuilder(self, Doc::ExpandParent.into())
    }

    /// Allocate a document containing the text `t.to_string()`.
    ///
    /// The given text must not contain line breaks.
    #[inline]
    fn as_string<U: fmt::Display>(&'a self, data: U) -> DocBuilder<'a, Self> {
        DocBuilder::from_utf8_text(self, data.into())
    }

    /// Allocate a document containing the given text.
    ///
    /// The given text must not contain line breaks.
    #[inline]
    fn text<U: Into<Cow<'a, str>>>(&'a self, data: U) -> DocBuilder<'a, Self> {
        let data: Cow<_> = data.into();
        if data.is_empty() {
            return DocBuilder(self, Doc::Nil.into());
        }
        let doc = match data {
            Cow::Owned(t) => Text::Owned(t.into()),
            Cow::Borrowed(t) => Text::Borrowed(t),
        };
        DocBuilder::from_utf8_text(self, doc)
    }

    /// Allocate a document containing n spaces.
    fn spaces(&'a self, n: usize) -> DocBuilder<'a, Self> {
        use crate::text::SPACES;

        if n == 0 {
            self.nil()
        } else if n <= SPACES.len() {
            self.text(&SPACES[..n])
        } else {
            let mut doc = self.nil();
            let mut remaining = n;
            while remaining != 0 {
                let i = SPACES.len().min(remaining);
                remaining -= i;
                doc = doc.append(self.text(&SPACES[..i]))
            }
            doc
        }
    }

    /// Pushes some content to the end of the current line.
    ///
    /// Multiple `line_suffix` calls accumulate in order and are all flushed together when a line
    /// break occurs or rendering ends.
    ///
    /// ```
    /// use prettyless::{Arena, DocAllocator};
    ///
    /// let arena = Arena::new();
    ///
    /// let doc = (arena.text("a")
    ///     + arena.line_suffix(" // 1")
    ///     + arena.line()
    ///     + arena.line_suffix(" // 2")
    ///     + arena.text("b"))
    /// .group();
    /// assert_eq!(doc.print(5).to_string(), "a b // 1 // 2");
    ///
    /// let doc = arena.text("a")
    ///     + arena.line_suffix(" // 1")
    ///     + arena.hardline()
    ///     + arena.line_suffix(" // 2")
    ///     + arena.text("b");
    /// assert_eq!(doc.print(5).to_string(), "a // 1\nb // 2");
    /// ```
    #[inline]
    fn line_suffix(&'a self, doc: impl Pretty<'a, Self>) -> DocBuilder<'a, Self> {
        let doc = doc.pretty(self).into_doc();
        DocBuilder(self, Doc::LineSuffix(doc).into())
    }

    /// Allocate a document concatenating the given documents.
    #[inline]
    fn concat<I>(&'a self, docs: I) -> DocBuilder<'a, Self>
    where
        I: IntoIterator,
        I::Item: Pretty<'a, Self>,
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
    fn intersperse<I, S>(&'a self, docs: I, separator: S) -> DocBuilder<'a, Self>
    where
        I: IntoIterator,
        I::Item: Pretty<'a, Self>,
        S: Pretty<'a, Self> + Clone,
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

    /// Reflows `text` inserting `softline` in place of any whitespace
    #[inline]
    fn reflow(&'a self, text: &'a str) -> DocBuilder<'a, Self>
    where
        Self: Sized,
        Self::Doc: Clone,
    {
        self.intersperse(text.split(char::is_whitespace), self.softline())
    }

    /// Allocate a document that acts differently based on the position and page layout
    ///
    /// ```rust
    /// use prettyless::DocAllocator;
    ///
    /// let arena = prettyless::Arena::new();
    /// let doc = arena.text("prefix ")
    ///     .append(arena.on_column(|l| {
    ///         arena.text("| <- column ").append(arena.as_string(l)).into_doc()
    ///     }));
    /// assert_eq!(doc.print(80).to_string(), "prefix | <- column 7");
    /// ```
    #[cfg(feature = "contextual")]
    #[inline]
    fn on_column(&'a self, f: impl Fn(usize) -> Self::Doc + 'a) -> DocBuilder<'a, Self> {
        DocBuilder(self, Doc::OnColumn(self.alloc_column_fn(f)).into())
    }

    /// Allocate a document that acts differently based on the current nesting level
    ///
    /// ```rust
    /// use prettyless::DocAllocator;
    ///
    /// let arena = prettyless::Arena::new();
    /// let doc = arena.text("prefix ")
    ///     .append(arena.on_nesting(|l| {
    ///         arena.text("[Nested: ").append(arena.as_string(l)).append("]").into_doc()
    ///     }).nest(4));
    /// assert_eq!(doc.print(80).to_string(), "prefix [Nested: 4]");
    /// ```
    #[cfg(feature = "contextual")]
    #[inline]
    fn on_nesting(&'a self, f: impl Fn(usize) -> Self::Doc + 'a) -> DocBuilder<'a, Self> {
        DocBuilder(self, Doc::OnNesting(self.alloc_column_fn(f)).into())
    }
}

pub struct BoxAllocator;

pub struct RcAllocator;

trait DropT {}
impl<T> DropT for T {}

/// An arena which can be used to allocate `Doc` values.
pub struct Arena<'a> {
    docs: typed_arena::Arena<Doc<'a, RefDoc<'a>>>,
    column_fns: typed_arena::Arena<Box<dyn DropT>>,
}

impl Default for Arena<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Arena<'a> {
    pub fn new() -> Self {
        Self {
            docs: typed_arena::Arena::new(),
            column_fns: Default::default(),
        }
    }

    pub fn len(&self) -> usize {
        self.docs.len() + self.column_fns.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    #[cfg(feature = "contextual")]
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

impl<'a, D> DocAllocator<'a> for &'a D
where
    D: ?Sized + DocAllocator<'a>,
{
    type Doc = D::Doc;

    #[inline]
    fn alloc(&'a self, doc: Doc<'a, Self::Doc>) -> Self::Doc {
        (**self).alloc(doc)
    }

    #[cfg(feature = "contextual")]
    fn alloc_column_fn(
        &'a self,
        f: impl Fn(usize) -> Self::Doc + 'a,
    ) -> <Self::Doc as DocPtr<'a>>::ColumnFn {
        (**self).alloc_column_fn(f)
    }

    #[cfg(feature = "contextual")]
    fn alloc_width_fn(
        &'a self,
        f: impl Fn(isize) -> Self::Doc + 'a,
    ) -> <Self::Doc as DocPtr<'a>>::WidthFn {
        (**self).alloc_width_fn(f)
    }
}

impl<'a> DocAllocator<'a> for Arena<'a> {
    type Doc = RefDoc<'a>;

    #[inline]
    fn alloc(&'a self, doc: Doc<'a, Self::Doc>) -> Self::Doc {
        RefDoc(match doc {
            // Return 'static references for common variants to avoid some allocations
            Doc::Nil => &Doc::Nil,
            Doc::HardLine => &Doc::HardLine,
            Doc::Fail => &Doc::Fail,
            // line()
            Doc::BreakOrFlat(RefDoc(Doc::HardLine), RefDoc(Doc::Text(Text::Borrowed(" ")))) => {
                &Doc::BreakOrFlat(
                    RefDoc(&Doc::HardLine),
                    RefDoc(&Doc::Text(Text::Borrowed(" "))),
                )
            }
            // line_()
            Doc::BreakOrFlat(RefDoc(Doc::HardLine), RefDoc(Doc::Nil)) => {
                &Doc::BreakOrFlat(RefDoc(&Doc::HardLine), RefDoc(&Doc::Nil))
            }
            // softline()
            Doc::Group(RefDoc(Doc::BreakOrFlat(
                RefDoc(Doc::HardLine),
                RefDoc(Doc::Text(Text::Borrowed(" "))),
            ))) => &Doc::Group(RefDoc(&Doc::BreakOrFlat(
                RefDoc(&Doc::HardLine),
                RefDoc(&Doc::Text(Text::Borrowed(" "))),
            ))),
            // softline_()
            Doc::Group(RefDoc(Doc::BreakOrFlat(RefDoc(Doc::HardLine), RefDoc(Doc::Nil)))) => {
                &Doc::Group(RefDoc(&Doc::BreakOrFlat(
                    RefDoc(&Doc::HardLine),
                    RefDoc(&Doc::Nil),
                )))
            }
            _ => self.docs.alloc(doc),
        })
    }

    #[cfg(feature = "contextual")]
    fn alloc_column_fn(
        &'a self,
        f: impl Fn(usize) -> Self::Doc + 'a,
    ) -> <Self::Doc as DocPtr<'a>>::ColumnFn {
        self.alloc_any(f)
    }

    #[cfg(feature = "contextual")]
    fn alloc_width_fn(
        &'a self,
        f: impl Fn(isize) -> Self::Doc + 'a,
    ) -> <Self::Doc as DocPtr<'a>>::WidthFn {
        self.alloc_any(f)
    }
}
