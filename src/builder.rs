use std::{
    convert::TryInto,
    fmt,
    ops::{Add, AddAssign, Deref},
};

use crate::{text::Text, BuildDoc, Doc, DocAllocator, Pretty};

/// The `DocBuilder` type allows for convenient appending of documents even for arena allocated
/// documents by storing the arena inline.
pub struct DocBuilder<'a, D>(pub &'a D, pub BuildDoc<'a, D::Doc>)
where
    D: ?Sized + DocAllocator<'a>;

impl<'a, D> DocBuilder<'a, D>
where
    D: ?Sized + DocAllocator<'a>,
{
    pub(crate) fn from_utf8_text(allocator: &'a D, text: Text<'a>) -> Self {
        let s = &*text;

        let doc = if s.is_ascii() {
            Doc::Text(text)
        } else {
            let display_width = unicode_width::UnicodeWidthStr::width(s);
            Doc::TextWithLen(display_width, allocator.alloc(Doc::Text(text)))
        };

        Self(allocator, doc.into())
    }

    pub fn into_doc(self) -> D::Doc {
        match self.1 {
            BuildDoc::DocPtr(d) => d,
            BuildDoc::Doc(d) => self.0.alloc(d),
        }
    }

    pub(crate) fn into_plain_doc(self) -> Doc<'a, D::Doc> {
        match self.1 {
            BuildDoc::DocPtr(_) => unreachable!(),
            BuildDoc::Doc(d) => d,
        }
    }

    /// Append the given document after this document.
    #[inline]
    pub fn append<E>(self, that: E) -> Self
    where
        E: Pretty<'a, D>,
    {
        let Self(allocator, _) = self;
        let that = that.pretty(allocator);
        match (&*self, &*that) {
            (Doc::Nil, _) => that,
            (_, Doc::Nil) => self,
            _ => Self(
                allocator,
                Doc::Append(
                    allocator.alloc_cow(self.into()),
                    allocator.alloc_cow(that.into()),
                )
                .into(),
            ),
        }
    }

    /// Repeats `self` `n` times, appending each repetition.
    ///
    /// ```
    /// use prettyless::{Arena, DocAllocator};
    ///
    /// let arena = Arena::new();
    /// let doc = arena.text("[]");
    ///
    /// assert_eq!(doc.clone().repeat(0).1.print(100).to_string(), "");
    /// assert_eq!(arena.len(), 0); // +0
    ///
    /// assert_eq!(doc.clone().repeat(1).1.print(100).to_string(), "[]");
    /// assert_eq!(arena.len(), 0); // +0
    ///
    /// assert_eq!(doc.clone().repeat(2).1.print(100).to_string(), "[][]");
    /// assert_eq!(arena.len(), 1); // +1
    ///
    /// assert_eq!(doc.clone().repeat(5).1.print(100).to_string(), "[][][][][]");
    /// assert_eq!(arena.len(), 5); // +4
    /// ```
    pub fn repeat(self, n: usize) -> Self
    where
        D::Doc: Clone,
    {
        if n == 1 {
            return self;
        }

        let Self(allocator, this) = self;
        if n == 0 {
            return Self(allocator, Doc::Nil.into());
        }

        let this = allocator.alloc_cow(this);
        let mut doc = Doc::Append(this.clone(), this.clone());
        for _ in 2..n {
            doc = Doc::Append(allocator.alloc_cow(doc.into()), this.clone());
        }

        Self(allocator, doc.into())
    }

    /// Acts as `self` when laid out on multiple lines and acts as `that` when laid out on a single line.
    ///
    /// ```
    /// use prettyless::{Arena, DocAllocator};
    ///
    /// let arena = Arena::new();
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
    /// assert_eq!(doc.1.print(100).to_string(), "let x in x");
    /// assert_eq!(doc.1.print(8).to_string(), "let x\nx");
    /// ```
    #[inline]
    pub fn flat_alt<E>(self, that: E) -> Self
    where
        E: Pretty<'a, D>,
    {
        let Self(allocator, this) = self;
        let that = that.pretty(allocator);
        Self(
            allocator,
            Doc::BreakOrFlat(allocator.alloc_cow(this), allocator.alloc_cow(that.into())).into(),
        )
    }

    /// Equivalent to `nil.flat_alt(self)`
    /// ```
    /// use prettyless::{Arena, DocAllocator};
    ///
    /// let arena = Arena::new();
    /// let doc = arena.text("flat-only").when_group_flat().group();
    ///
    /// assert_eq!(doc.1.print(0).to_string(), "");
    /// assert_eq!(doc.1.print(10).to_string(), "flat-only");
    /// ```
    #[inline]
    pub fn when_group_flat(self) -> Self {
        let Self(allocator, this) = self;
        allocator.if_group_flat(this)
    }

    /// Equivalent to `self.flat_alt(nil)`
    /// ```
    /// use prettyless::{Arena, DocAllocator};
    ///
    /// let arena = Arena::new();
    /// let doc = arena.text("a")
    ///     .append(arena.text(",").when_group_break())
    ///     .enclose(arena.line_(), arena.line_())
    ///     .parens()
    ///     .group();
    ///
    /// assert_eq!(doc.1.print(1).to_string(), "(\na,\n)");
    /// assert_eq!(doc.1.print(10).to_string(), "(a)");
    /// ```
    #[inline]
    pub fn when_group_break(self) -> Self {
        let Self(allocator, this) = self;
        allocator.if_group_break(this)
    }

    /// Mark this document as a group.
    ///
    /// Groups are layed out on a single line if possible.  Within a group, all basic documents with
    /// several possible layouts are assigned the same layout, that is, they are all layed out
    /// horizontally and combined into a one single line, or they are each layed out on their own
    /// line.
    #[inline]
    pub fn group(self) -> Self {
        match *self.1 {
            Doc::Group(_) | Doc::Text(_) | Doc::Nil => self,
            _ => {
                let Self(allocator, this) = self;
                Self(allocator, Doc::Group(allocator.alloc_cow(this)).into())
            }
        }
    }

    /// Increase the indentation level of this document.
    #[inline]
    pub fn nest(self, offset: isize) -> Self {
        if let Doc::Nil = &*self.1 {
            return self;
        }
        if offset == 0 {
            return self;
        }
        let Self(allocator, this) = self;
        Self(
            allocator,
            Doc::Nest(offset, allocator.alloc_cow(this)).into(),
        )
    }

    #[inline]
    pub fn union<E>(self, other: E) -> Self
    where
        E: Into<BuildDoc<'a, D::Doc>>,
    {
        let Self(allocator, this) = self;
        let other = other.into();
        let doc = Doc::Union(allocator.alloc_cow(this), allocator.alloc_cow(other));
        Self(allocator, doc.into())
    }

    /// Like `union`, but it only ensures fitting on the first line.
    #[inline]
    pub fn quick_union<E>(self, other: E) -> Self
    where
        E: Into<BuildDoc<'a, D::Doc>>,
    {
        let Self(allocator, this) = self;
        let other = other.into();
        let doc = Doc::QuickUnion(allocator.alloc_cow(this), allocator.alloc_cow(other));
        Self(allocator, doc.into())
    }

    /// Lays out `self` so with the nesting level set to the current column
    ///
    /// NOTE: The doc pointer type, `D` may need to be cloned. Consider using cheaply cloneable ptr
    /// like `RefDoc` or `RcDoc`
    ///
    /// ```rust
    /// use prettyless::{docs, DocAllocator};
    ///
    /// let arena = &prettyless::Arena::new();
    /// let doc = docs![
    ///     arena,
    ///     "lorem",
    ///     " ",
    ///     arena.intersperse(["ipsum", "dolor"].iter().cloned(), arena.line_()).align(),
    ///     arena.hardline(),
    ///     "next",
    /// ];
    /// assert_eq!(doc.1.print(80).to_string(), "lorem ipsum\n      dolor\nnext");
    /// ```
    #[inline]
    pub fn align(self) -> Self
    where
        Self: Clone,
    {
        let allocator = self.0;
        allocator.on_column(move |col| {
            let self_ = self.clone();
            allocator
                .on_nesting(move |nest| self_.clone().nest(col as isize - nest as isize).into_doc())
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
    /// let arena = prettyless::Arena::new();
    /// let doc = arena.text("prefix").append(arena.text(" "))
    ///     .append(arena.reflow("Indenting these words with nest").hang(4));
    /// assert_eq!(
    ///     doc.1.print(24).to_string(),
    ///     "prefix Indenting these\n           words with\n           nest",
    /// );
    /// ```
    #[inline]
    pub fn hang(self, adjust: isize) -> Self
    where
        Self: Clone,
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
    /// let arena = prettyless::Arena::new();
    /// let doc = arena.text("prefix").append(arena.text(" "))
    ///     .append(arena.reflow("The indent function indents these words!").indent(4));
    /// assert_eq!(
    ///     doc.1.print(24).to_string(),
    /// "
    /// prefix     The indent
    ///            function
    ///            indents these
    ///            words!".trim_start(),
    /// );
    /// ```
    #[inline]
    pub fn indent(self, adjust: usize) -> Self
    where
        Self: Clone,
    {
        let spaces = {
            use crate::text::SPACES;
            let Self(allocator, _) = self;
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
    #[inline]
    pub fn measure_width(self, f: impl Fn(isize) -> D::Doc + 'a) -> Self
    where
        BuildDoc<'a, D::Doc>: Clone,
    {
        let Self(allocator, this) = self;
        let f = allocator.alloc_width_fn(f);
        allocator.on_column(move |start| {
            let f = f.clone();

            Self(allocator, this.clone())
                .append(allocator.on_column(move |end| f(end as isize - start as isize)))
                .into_doc()
        })
    }

    /// Puts `self` between `before` and `after`
    #[inline]
    pub fn enclose<E, F>(self, before: E, after: F) -> Self
    where
        E: Pretty<'a, D>,
        F: Pretty<'a, D>,
    {
        let Self(allocator, _) = self;
        Self(allocator, before.pretty(allocator).1)
            .append(self)
            .append(after)
    }

    pub fn single_quotes(self) -> Self {
        self.enclose("'", "'")
    }

    pub fn double_quotes(self) -> Self {
        self.enclose("\"", "\"")
    }
    pub fn parens(self) -> Self {
        self.enclose("(", ")")
    }

    pub fn angles(self) -> Self {
        self.enclose("<", ">")
    }
    pub fn braces(self) -> Self {
        self.enclose("{", "}")
    }

    pub fn brackets(self) -> Self {
        self.enclose("[", "]")
    }
}

impl<'a, D> Deref for DocBuilder<'a, D>
where
    D: ?Sized + DocAllocator<'a>,
{
    type Target = Doc<'a, D::Doc>;
    fn deref(&self) -> &Self::Target {
        match &self.1 {
            BuildDoc::DocPtr(d) => d,
            BuildDoc::Doc(d) => d,
        }
    }
}

impl<'a, D> fmt::Debug for DocBuilder<'a, D>
where
    D: ?Sized + DocAllocator<'a>,
    D::Doc: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.1.fmt(f)
    }
}

impl<'a, D> Clone for DocBuilder<'a, D>
where
    D: DocAllocator<'a> + 'a,
    D::Doc: Clone,
{
    fn clone(&self) -> Self {
        Self(self.0, self.1.clone())
    }
}

impl<'a, D> From<DocBuilder<'a, D>> for BuildDoc<'a, D::Doc>
where
    D: ?Sized + DocAllocator<'a>,
{
    fn from(val: DocBuilder<'a, D>) -> Self {
        val.1
    }
}

impl<'a, D, P> Add<P> for DocBuilder<'a, D>
where
    D: ?Sized + DocAllocator<'a>,
    P: Pretty<'a, D>,
{
    type Output = Self;
    fn add(self, other: P) -> Self::Output {
        self.append(other)
    }
}

impl<'a, D, P> AddAssign<P> for DocBuilder<'a, D>
where
    D: ?Sized + DocAllocator<'a>,
    P: Pretty<'a, D>,
{
    fn add_assign(&mut self, other: P) {
        *self = Self(self.0, std::mem::take(&mut self.1)).append(other)
    }
}
