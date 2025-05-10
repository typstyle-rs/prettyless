use std::{
    convert::TryInto,
    fmt,
    ops::{Add, AddAssign, Deref},
};

use crate::{BuildDoc, Doc, DocAllocator, Pretty};

/// The `DocBuilder` type allows for convenient appending of documents even for arena allocated
/// documents by storing the arena inline.
pub struct DocBuilder<'a, D, A = ()>(pub &'a D, pub BuildDoc<'a, D::Doc, A>)
where
    D: ?Sized + DocAllocator<'a, A>;

impl<'a, D, A> DocBuilder<'a, D, A>
where
    A: 'a,
    D: ?Sized + DocAllocator<'a, A>,
{
    pub(crate) fn with_utf8_len(self) -> Self {
        let s = match &*self {
            Doc::OwnedText(s) => &s[..],
            Doc::BorrowedText(s) => s,
            Doc::SmallText(s) => s,
            _ => return self,
        };

        if s.is_ascii() {
            self
        } else {
            let display_width = unicode_width::UnicodeWidthStr::width(s);

            let DocBuilder(allocator, _) = self;
            DocBuilder(
                allocator,
                Doc::RenderLen(display_width, self.into_doc()).into(),
            )
        }
    }

    /// Append the given document after this document.
    #[inline]
    pub fn append<E>(self, that: E) -> DocBuilder<'a, D, A>
    where
        E: Pretty<'a, D, A>,
    {
        let DocBuilder(allocator, _) = self;
        let that = that.pretty(allocator);
        match (&*self, &*that) {
            (Doc::Nil, _) => that,
            (_, Doc::Nil) => self,
            _ => DocBuilder(
                allocator,
                Doc::Append(
                    allocator.alloc_cow(self.into()),
                    allocator.alloc_cow(that.into()),
                )
                .into(),
            ),
        }
    }

    /// Acts as `self` when laid out on multiple lines and acts as `that` when laid out on a single line.
    ///
    /// ```
    /// use prettyless::{Arena, DocAllocator};
    ///
    /// let arena = Arena::<()>::new();
    /// let body = arena.line().append("x");
    /// let doc = arena.text("let")
    ///     .append(arena.line())
    ///     .append("x")
    ///     .group()
    ///     .append(
    ///         body.clone()
    ///             .flat_alt(
    ///                 arena.line()
    ///                     .append("in")
    ///                     .append(body)
    ///             )
    ///     )
    ///     .group();
    ///
    /// assert_eq!(doc.1.pretty(100).to_string(), "let x in x");
    /// assert_eq!(doc.1.pretty(8).to_string(), "let x\nx");
    /// ```
    #[inline]
    pub fn flat_alt<E>(self, that: E) -> DocBuilder<'a, D, A>
    where
        E: Pretty<'a, D, A>,
    {
        let DocBuilder(allocator, this) = self;
        let that = that.pretty(allocator);
        DocBuilder(
            allocator,
            Doc::FlatAlt(allocator.alloc_cow(this), allocator.alloc_cow(that.into())).into(),
        )
    }

    /// Mark this document as a group.
    ///
    /// Groups are layed out on a single line if possible.  Within a group, all basic documents with
    /// several possible layouts are assigned the same layout, that is, they are all layed out
    /// horizontally and combined into a one single line, or they are each layed out on their own
    /// line.
    #[inline]
    pub fn group(self) -> DocBuilder<'a, D, A> {
        match *self.1 {
            Doc::Group(_)
            | Doc::OwnedText(_)
            | Doc::BorrowedText(_)
            | Doc::SmallText(_)
            | Doc::Nil => self,
            _ => {
                let DocBuilder(allocator, this) = self;
                DocBuilder(allocator, Doc::Group(allocator.alloc_cow(this)).into())
            }
        }
    }

    /// Increase the indentation level of this document.
    #[inline]
    pub fn nest(self, offset: isize) -> DocBuilder<'a, D, A> {
        if let Doc::Nil = &*self.1 {
            return self;
        }
        if offset == 0 {
            return self;
        }
        let DocBuilder(allocator, this) = self;
        DocBuilder(
            allocator,
            Doc::Nest(offset, allocator.alloc_cow(this)).into(),
        )
    }

    #[inline]
    pub fn annotate(self, ann: A) -> DocBuilder<'a, D, A> {
        let DocBuilder(allocator, this) = self;
        DocBuilder(
            allocator,
            Doc::Annotated(ann, allocator.alloc_cow(this)).into(),
        )
    }

    #[inline]
    pub fn union<E>(self, other: E) -> DocBuilder<'a, D, A>
    where
        E: Into<BuildDoc<'a, D::Doc, A>>,
    {
        let DocBuilder(allocator, this) = self;
        let other = other.into();
        let doc = Doc::Union(allocator.alloc_cow(this), allocator.alloc_cow(other));
        DocBuilder(allocator, doc.into())
    }

    /// Lays out `self` so with the nesting level set to the current column
    ///
    /// NOTE: The doc pointer type, `D` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    ///
    /// ```rust
    /// use prettyless::{docs, DocAllocator};
    ///
    /// let arena = &prettyless::Arena::<()>::new();
    /// let doc = docs![
    ///     arena,
    ///     "lorem",
    ///     " ",
    ///     arena.intersperse(["ipsum", "dolor"].iter().cloned(), arena.line_()).align(),
    ///     arena.hardline(),
    ///     "next",
    /// ];
    /// assert_eq!(doc.1.pretty(80).to_string(), "lorem ipsum\n      dolor\nnext");
    /// ```
    #[inline]
    pub fn align(self) -> DocBuilder<'a, D, A>
    where
        DocBuilder<'a, D, A>: Clone,
    {
        let allocator = self.0;
        allocator.column(move |col| {
            let self_ = self.clone();
            allocator
                .nesting(move |nest| self_.clone().nest(col as isize - nest as isize).into_doc())
                .into_doc()
        })
    }

    /// Lays out `self` with a nesting level set to the current level plus `adjust`.
    ///
    /// NOTE: The doc pointer type, `D` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    ///
    /// ```rust
    /// use prettyless::DocAllocator;
    ///
    /// let arena = prettyless::Arena::<()>::new();
    /// let doc = arena.text("prefix").append(arena.text(" "))
    ///     .append(arena.reflow("Indenting these words with nest").hang(4));
    /// assert_eq!(
    ///     doc.1.pretty(24).to_string(),
    ///     "prefix Indenting these\n           words with\n           nest",
    /// );
    /// ```
    #[inline]
    pub fn hang(self, adjust: isize) -> DocBuilder<'a, D, A>
    where
        DocBuilder<'a, D, A>: Clone,
    {
        self.nest(adjust).align()
    }

    /// Indents `self` by `adjust` spaces from the current cursor position
    ///
    /// NOTE: The doc pointer type, `D` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    ///
    /// ```rust
    /// use prettyless::DocAllocator;
    ///
    /// let arena = prettyless::Arena::<()>::new();
    /// let doc = arena.text("prefix").append(arena.text(" "))
    ///     .append(arena.reflow("The indent function indents these words!").indent(4));
    /// assert_eq!(
    ///     doc.1.pretty(24).to_string(),
    /// "
    /// prefix     The indent
    ///            function
    ///            indents these
    ///            words!".trim_start(),
    /// );
    /// ```
    #[inline]
    pub fn indent(self, adjust: usize) -> DocBuilder<'a, D, A>
    where
        DocBuilder<'a, D, A>: Clone,
    {
        let spaces = {
            use crate::render::SPACES;
            let DocBuilder(allocator, _) = self;
            let mut doc = allocator.nil();
            let mut remaining = adjust;
            while remaining != 0 {
                let i = SPACES.len().min(remaining);
                remaining -= i;
                doc = doc.append(allocator.text(&SPACES[..i]))
            }
            doc
        };
        spaces.append(self).hang(adjust.try_into().unwrap())
    }

    /// Lays out `self` and provides the column width of it available to `f`
    ///
    /// NOTE: The doc pointer type, `D` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
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
    pub fn width(self, f: impl Fn(isize) -> D::Doc + 'a) -> DocBuilder<'a, D, A>
    where
        BuildDoc<'a, D::Doc, A>: Clone,
    {
        let DocBuilder(allocator, this) = self;
        let f = allocator.alloc_width_fn(f);
        allocator.column(move |start| {
            let f = f.clone();

            DocBuilder(allocator, this.clone())
                .append(allocator.column(move |end| f(end as isize - start as isize)))
                .into_doc()
        })
    }

    /// Puts `self` between `before` and `after`
    #[inline]
    pub fn enclose<E, F>(self, before: E, after: F) -> DocBuilder<'a, D, A>
    where
        E: Pretty<'a, D, A>,
        F: Pretty<'a, D, A>,
    {
        let DocBuilder(allocator, _) = self;
        DocBuilder(allocator, before.pretty(allocator).1)
            .append(self)
            .append(after)
    }

    pub fn single_quotes(self) -> DocBuilder<'a, D, A> {
        self.enclose("'", "'")
    }

    pub fn double_quotes(self) -> DocBuilder<'a, D, A> {
        self.enclose("\"", "\"")
    }
    pub fn parens(self) -> DocBuilder<'a, D, A> {
        self.enclose("(", ")")
    }

    pub fn angles(self) -> DocBuilder<'a, D, A> {
        self.enclose("<", ">")
    }
    pub fn braces(self) -> DocBuilder<'a, D, A> {
        self.enclose("{", "}")
    }

    pub fn brackets(self) -> DocBuilder<'a, D, A> {
        self.enclose("[", "]")
    }

    pub fn into_doc(self) -> D::Doc {
        match self.1 {
            BuildDoc::DocPtr(d) => d,
            BuildDoc::Doc(d) => self.0.alloc(d),
        }
    }

    pub(crate) fn into_plain_doc(self) -> Doc<'a, D::Doc, A> {
        match self.1 {
            BuildDoc::DocPtr(_) => unreachable!(),
            BuildDoc::Doc(d) => d,
        }
    }
}

impl<'a, D, A, P> Add<P> for DocBuilder<'a, D, A>
where
    D: ?Sized + DocAllocator<'a, A>,
    P: Pretty<'a, D, A>,
{
    type Output = DocBuilder<'a, D, A>;
    fn add(self, other: P) -> Self::Output {
        self.append(other)
    }
}

impl<'a, D, A, P> AddAssign<P> for DocBuilder<'a, D, A>
where
    D: ?Sized + DocAllocator<'a, A>,
    P: Pretty<'a, D, A>,
{
    fn add_assign(&mut self, other: P) {
        *self = DocBuilder(self.0, std::mem::take(&mut self.1)).append(other)
    }
}

impl<'a, D, A> Deref for DocBuilder<'a, D, A>
where
    D: ?Sized + DocAllocator<'a, A>,
{
    type Target = Doc<'a, D::Doc, A>;
    fn deref(&self) -> &Self::Target {
        match &self.1 {
            BuildDoc::DocPtr(d) => d,
            BuildDoc::Doc(d) => d,
        }
    }
}

impl<'a, D, A> fmt::Debug for DocBuilder<'a, D, A>
where
    D: ?Sized + DocAllocator<'a, A>,
    D::Doc: fmt::Debug,
    A: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.1.fmt(f)
    }
}

impl<'a, A, D> Clone for DocBuilder<'a, D, A>
where
    A: Clone,
    D: DocAllocator<'a, A> + 'a,
    D::Doc: Clone,
{
    fn clone(&self) -> Self {
        DocBuilder(self.0, self.1.clone())
    }
}

impl<'a, D, A> From<DocBuilder<'a, D, A>> for BuildDoc<'a, D::Doc, A>
where
    D: ?Sized + DocAllocator<'a, A>,
{
    fn from(val: DocBuilder<'a, D, A>) -> Self {
        val.1
    }
}
