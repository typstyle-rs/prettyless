//! Allocator-free document combinators.
//!
//! Every function in this module returns a small value implementing [`Pretty`](crate::Pretty), so a
//! document can be described before an allocator exists and materialized later with
//! [`DocAllocator::pretty`](crate::DocAllocator::pretty):
//!
//! ```
//! use prettyless::combinators::*;
//! use prettyless::{Arena, DocAllocator};
//!
//! let arena = Arena::new();
//! let list = group((
//!     "(",
//!     nest(1)((
//!         line_or_space(),
//!         intersperse(line_or_space())(["a", "b", "c"]),
//!     )),
//!     line_or_space(),
//!     ")",
//! ));
//!
//! assert_eq!(arena.pretty(list.clone()).print(80).to_string(), "( a b c )");
//! assert_eq!(arena.pretty(list).print(5).to_string(), "(\n a\n b\n c\n)");
//! ```
//!
//! String literals, [`String`] and [`Cow<str>`](std::borrow::Cow) already implement
//! [`Pretty`](crate::Pretty) and can be used directly, so there is no `text` combinator; use
//! [`as_string`] for other [`Display`](std::fmt::Display) values.
//!
//! The nullary tokens are `Copy`, and the wrappers are `Clone` whenever their contents are, so a
//! separator can be moved into [`intersperse`] or reused across documents:
//!
//! ```
//! use prettyless::combinators::*;
//! use prettyless::{Arena, DocAllocator};
//!
//! let arena = Arena::new();
//! let doc = arena
//!     .pretty(intersperse(line_or_space())(["a", "b", "c"]))
//!     .group();
//! assert_eq!(doc.print(80).to_string(), "a b c");
//! ```

use crate::{DocAllocator, DocBuilder, Pretty, builder::nesting_offset};

/// Declares a nullary token whose `Pretty` impl delegates to `DocAllocator::$method`.
macro_rules! token_combinator {
    ($(#[$attr:meta])* $fn_name:ident, $ty_name:ident, $method:ident) => {
        $(#[$attr])*
        #[inline]
        pub const fn $fn_name() -> $ty_name {
            $ty_name
        }

        #[doc = concat!("Token returned by [`", stringify!($fn_name), "`].")]
        #[derive(Clone, Copy, Debug)]
        pub struct $ty_name;

        impl<'a, D> Pretty<'a, D> for $ty_name
        where
            D: ?Sized + DocAllocator<'a>,
        {
            fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
                allocator.$method()
            }
        }
    };
}

/// Declares a unary wrapper whose `Pretty` impl delegates to `DocBuilder::$fn_name`.
macro_rules! unary_combinator {
    ($(#[$attr:meta])* $fn_name:ident, $ty_name:ident) => {
        $(#[$attr])*
        #[inline]
        pub const fn $fn_name<T>(doc: T) -> $ty_name<T> {
            $ty_name(doc)
        }

        #[doc = concat!("Wrapper returned by [`", stringify!($fn_name), "`].")]
        #[derive(Clone, Debug)]
        pub struct $ty_name<T>(T);

        impl<'a, D, T> Pretty<'a, D> for $ty_name<T>
        where
            D: ?Sized + DocAllocator<'a>,
            T: Pretty<'a, D> + Sized,
        {
            fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
                allocator.pretty(self.0).$fn_name()
            }
        }
    };
}

/// Declares a binary wrapper whose `Pretty` impl delegates to `DocBuilder::$fn_name`, converting
/// both arguments through `Pretty`.
macro_rules! binary_combinator {
    ($(#[$attr:meta])* $fn_name:ident, $ty_name:ident, $left:ident, $right:ident) => {
        $(#[$attr])*
        #[inline]
        pub const fn $fn_name<T, U>($left: T, $right: U) -> $ty_name<T, U> {
            $ty_name($left, $right)
        }

        #[doc = concat!("Wrapper returned by [`", stringify!($fn_name), "`].")]
        #[derive(Clone, Debug)]
        pub struct $ty_name<T, U>(T, U);

        impl<'a, D, T, U> Pretty<'a, D> for $ty_name<T, U>
        where
            D: ?Sized + DocAllocator<'a>,
            T: Pretty<'a, D> + Sized,
            U: Pretty<'a, D> + Sized,
        {
            fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
                self.0.pretty(allocator).$fn_name(self.1.pretty(allocator))
            }
        }
    };
}

// === Leaf combinators ===

token_combinator!(
    /// An empty document.
    nil, Nil, nil
);
token_combinator!(
    /// A document that fails to render, aborting the left branch of a [`union`].
    fail, Fail, fail
);
token_combinator!(
    /// A line break that always breaks.
    hard_line, HardLine, hard_line
);
token_combinator!(
    /// A line break, or nothing when the enclosing group fits on one line.
    line_or_nil, LineOrNil, line_
);
token_combinator!(
    /// A line break, or a space when the enclosing group fits on one line.
    line_or_space, LineOrSpace, line
);
token_combinator!(
    /// A grouped [`line_or_nil`]. It forms its own group, so it stays nil whenever it fits, even
    /// inside an enclosing group that breaks.
    soft_line_or_nil, SoftLineOrNil, softline_
);
token_combinator!(
    /// A grouped [`line_or_space`]. It forms its own group, so it stays a space whenever it fits,
    /// even inside an enclosing group that breaks.
    soft_line_or_space, SoftLineOrSpace, softline
);
token_combinator!(
    /// A single space.
    space, Space, space
);

/// A run of `count` spaces.
#[inline]
pub const fn spaces(count: usize) -> Spaces {
    Spaces(count)
}

/// Token returned by [`spaces`].
#[derive(Clone, Copy, Debug)]
pub struct Spaces(usize);

impl<'a, D> Pretty<'a, D> for Spaces
where
    D: ?Sized + DocAllocator<'a>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        allocator.spaces(self.0)
    }
}

/// Renders `value` with its [`Display`](std::fmt::Display) impl, like
/// [`DocAllocator::as_string`](crate::DocAllocator::as_string).
///
/// Strings implement [`Pretty`](crate::Pretty) directly and do not need this; use it for values
/// such as numbers.
///
/// ```
/// use prettyless::combinators::*;
/// use prettyless::{Arena, DocAllocator};
///
/// let arena = Arena::new();
/// let doc = arena.pretty(("x = ", as_string(42)));
/// assert_eq!(doc.print(80).to_string(), "x = 42");
/// ```
#[inline]
pub const fn as_string<T>(value: T) -> AsString<T> {
    AsString(value)
}

/// Wrapper returned by [`as_string`].
#[derive(Clone, Debug)]
pub struct AsString<T>(T);

impl<'a, D, T> Pretty<'a, D> for AsString<T>
where
    D: ?Sized + DocAllocator<'a>,
    T: std::fmt::Display,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        allocator.as_string(self.0)
    }
}

token_combinator!(
    /// Forces the parent group to break.
    expand_parent, ExpandParent, expand_parent
);

// === Nesting ===

/// Returns a function that increases the indentation of its argument by `offset`.
///
/// Apply it where the document goes: `nest(2)(doc)`.
#[inline]
pub const fn nest<T>(offset: isize) -> impl Fn(T) -> Nest<T> {
    move |doc: T| Nest(offset, doc)
}

/// Returns a function that increases the indentation of its argument by `offset`.
///
/// Equivalent to `nest(nesting_offset(offset))`.
///
/// # Panics
///
/// Panics if `offset` does not fit in `isize`.
#[inline]
pub const fn indent<T>(offset: usize) -> impl Fn(T) -> Nest<T> {
    nest(nesting_offset(offset))
}

/// Returns a function that decreases the indentation of its argument by `offset`.
///
/// Equivalent to `nest(-nesting_offset(offset))`.
///
/// # Panics
///
/// Panics if `offset` does not fit in `isize`.
#[inline]
pub const fn dedent<T>(offset: usize) -> impl Fn(T) -> Nest<T> {
    nest(-nesting_offset(offset))
}

/// Wrapper returned by [`nest`], [`indent`] and [`dedent`].
#[derive(Clone, Debug)]
pub struct Nest<T>(isize, T);

impl<'a, D, T> Pretty<'a, D> for Nest<T>
where
    D: ?Sized + DocAllocator<'a>,
    T: Pretty<'a, D> + Sized,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        self.1.pretty(allocator).nest(self.0)
    }
}

// === Layout ===

unary_combinator!(
    /// Lays `doc` out on a single line if it fits, otherwise on multiple lines.
    group, Group
);

unary_combinator!(
    /// Sets the indentation of `doc` to the current column.
    align, Align
);

unary_combinator!(
    /// Dedents `doc` to the root (column 0).
    dedent_to_root, DedentToRoot
);

unary_combinator!(
    /// Forces `doc` to render flat; a hard line inside makes it fail.
    flatten, Flatten
);

// === Trailing content ===

/// Pushes `doc` to the end of the current line.
#[inline]
pub const fn line_suffix<T>(doc: T) -> LineSuffix<T> {
    LineSuffix(doc)
}

/// Wrapper returned by [`line_suffix`].
#[derive(Clone, Debug)]
pub struct LineSuffix<T>(T);

impl<'a, D, T> Pretty<'a, D> for LineSuffix<T>
where
    D: ?Sized + DocAllocator<'a>,
    T: Pretty<'a, D> + Sized,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        allocator.line_suffix(self.0)
    }
}

// === Alternatives ===

binary_combinator!(
    /// Renders `left` if it fits, otherwise `right`.
    union, Union, left, right
);

binary_combinator!(
    /// Like [`union`], but only requires `left` to fit on its first line.
    ///
    /// ```
    /// use prettyless::combinators::*;
    /// use prettyless::{Arena, DocAllocator};
    ///
    /// let arena = Arena::new();
    /// let doc = arena.pretty(partial_union(
    ///     ("short", hard_line(), "long long long"),
    ///     ("short", hard_line(), "short"),
    /// ));
    /// assert_eq!(doc.print(10).to_string(), "short\nlong long long");
    /// ```
    partial_union, PartialUnion, left, right
);

binary_combinator!(
    /// Renders `broken` when the enclosing group breaks, and `flat` when it fits on one line.
    ///
    /// ```
    /// use prettyless::combinators::*;
    /// use prettyless::{Arena, DocAllocator};
    ///
    /// let arena = Arena::new();
    /// let doc = arena.pretty(group(("a", flat_alt(line_or_space(), ","), "b")));
    ///
    /// assert_eq!(doc.print(80).to_string(), "a,b");
    /// assert_eq!(doc.print(2).to_string(), "a\nb");
    /// ```
    flat_alt, FlatAlt, broken, flat
);

// === Iteration ===

/// Concatenates the documents yielded by `docs`.
///
/// ```
/// use prettyless::combinators::*;
/// use prettyless::{Arena, DocAllocator};
///
/// let arena = Arena::new();
/// let doc = arena.pretty(concat(["a", "b", "c"]));
/// assert_eq!(doc.print(80).to_string(), "abc");
/// ```
#[inline]
pub const fn concat<I>(docs: I) -> Concat<I> {
    Concat(docs)
}

/// Wrapper returned by [`concat`].
#[derive(Clone, Debug)]
pub struct Concat<I>(I);

impl<'a, D, I> Pretty<'a, D> for Concat<I>
where
    D: ?Sized + DocAllocator<'a>,
    I: IntoIterator,
    I::Item: Pretty<'a, D>,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        allocator.concat(self.0)
    }
}

/// Returns a function that intersperses `separator` between the documents it is applied to.
///
/// The separator is cloned into each gap, so it must be [`Clone`]; the nullary tokens are `Copy`
/// and the returned function can be called any number of times.
///
/// The separator goes strictly *between* documents and may itself be any document, including one
/// containing line breaks. This defers [`DocAllocator::intersperse`](crate::DocAllocator::intersperse)
/// and is more general than a string `join`, which materializes text and accepts only a string
/// separator.
///
/// ```
/// use prettyless::combinators::*;
/// use prettyless::{Arena, DocAllocator};
///
/// let arena = Arena::new();
/// let doc = arena
///     .pretty(intersperse(line_or_space())(["a", "b", "c"]))
///     .group();
/// assert_eq!(doc.print(80).to_string(), "a b c");
/// ```
#[inline]
pub fn intersperse<I, S>(separator: S) -> impl Fn(I) -> Intersperse<I, S>
where
    S: Clone,
{
    move |docs| Intersperse(docs, separator.clone())
}

/// Wrapper returned by [`intersperse`].
#[derive(Clone, Debug)]
pub struct Intersperse<I, S>(I, S);

impl<'a, D, I, S> Pretty<'a, D> for Intersperse<I, S>
where
    D: ?Sized + DocAllocator<'a>,
    I: IntoIterator,
    I::Item: Pretty<'a, D>,
    S: Pretty<'a, D> + Clone,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        allocator.intersperse(self.0, self.1)
    }
}

/// Returns a function that repeats the document it is applied to `times` times.
///
/// ```
/// use prettyless::combinators::*;
/// use prettyless::{Arena, DocAllocator};
///
/// let arena = Arena::new();
/// let doc = arena.pretty(repeat(3)("ab"));
/// assert_eq!(doc.print(80).to_string(), "ababab");
/// ```
#[inline]
pub const fn repeat<T>(times: usize) -> impl Fn(T) -> Repeat<T> {
    move |doc| Repeat(doc, times)
}

/// Wrapper returned by [`repeat`].
#[derive(Clone, Debug)]
pub struct Repeat<T>(T, usize);

impl<'a, D, T> Pretty<'a, D> for Repeat<T>
where
    D: ?Sized + DocAllocator<'a>,
    T: Pretty<'a, D> + Sized,
    D::Doc: Clone,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        allocator.pretty(self.0).repeat(self.1)
    }
}

// === Contextual ===

/// Defers a function of the current column to rendering time.
///
/// The function must return an already allocated document, exactly like
/// [`DocAllocator::on_column`](crate::DocAllocator::on_column); capture the allocator in the
/// closure if it needs to allocate.
///
/// ```
/// use prettyless::combinators::*;
/// use prettyless::{Arena, DocAllocator};
///
/// let arena = Arena::new();
/// let doc = arena.pretty((
///     "prefix ",
///     on_column(|column| arena.pretty(("col ", as_string(column))).into_doc()),
/// ));
/// assert_eq!(doc.print(80).to_string(), "prefix col 7");
/// ```
#[cfg(feature = "contextual")]
#[inline]
pub const fn on_column<F>(f: F) -> OnColumn<F> {
    OnColumn(f)
}

/// Wrapper returned by [`on_column`].
#[cfg(feature = "contextual")]
#[derive(Clone)]
pub struct OnColumn<F>(F);

#[cfg(feature = "contextual")]
impl<'a, D, F> Pretty<'a, D> for OnColumn<F>
where
    D: ?Sized + DocAllocator<'a>,
    F: Fn(usize) -> D::Doc + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        allocator.on_column(self.0)
    }
}

/// Defers a function of the current nesting level to rendering time.
///
/// The function must return an already allocated document, exactly like
/// [`DocAllocator::on_nesting`](crate::DocAllocator::on_nesting).
#[cfg(feature = "contextual")]
#[inline]
pub const fn on_nesting<F>(f: F) -> OnNesting<F> {
    OnNesting(f)
}

/// Wrapper returned by [`on_nesting`].
#[cfg(feature = "contextual")]
#[derive(Clone)]
pub struct OnNesting<F>(F);

#[cfg(feature = "contextual")]
impl<'a, D, F> Pretty<'a, D> for OnNesting<F>
where
    D: ?Sized + DocAllocator<'a>,
    F: Fn(usize) -> D::Doc + 'a,
{
    fn pretty(self, allocator: &'a D) -> DocBuilder<'a, D> {
        allocator.on_nesting(self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokens_are_copy() {
        fn assert_copy<T: Copy>(_: T) {}

        assert_copy(nil());
        assert_copy(fail());
        assert_copy(hard_line());
        assert_copy(line_or_nil());
        assert_copy(line_or_space());
        assert_copy(soft_line_or_nil());
        assert_copy(soft_line_or_space());
        assert_copy(space());
        assert_copy(spaces(3));
        assert_copy(expand_parent());
    }

    #[test]
    fn wrappers_are_clone() {
        fn assert_clone<T: Clone>(_: &T) {}

        assert_clone(&line_suffix("// comment"));
        assert_clone(&as_string(42));
        assert_clone(&nest(1)("x"));
        assert_clone(&indent(1)("x"));
        assert_clone(&dedent(1)("x"));
        assert_clone(&group("x"));
        assert_clone(&align("x"));
        assert_clone(&dedent_to_root("x"));
        assert_clone(&flatten("x"));
        assert_clone(&union("a", "b"));
        assert_clone(&partial_union("a", "b"));
        assert_clone(&flat_alt("a", "b"));
        assert_clone(&concat(["a", "b"]));
        assert_clone(&intersperse(line_or_space())(["a", "b"]));
        assert_clone(&repeat(3)("x"));
    }

    #[test]
    #[should_panic(expected = "nesting offset must not exceed isize::MAX")]
    fn indent_rejects_offset_above_isize_max() {
        let _ = indent::<&str>(std::hint::black_box(usize::MAX));
    }

    #[test]
    #[should_panic(expected = "nesting offset must not exceed isize::MAX")]
    fn dedent_rejects_offset_above_isize_max() {
        let _ = dedent::<&str>(std::hint::black_box(usize::MAX));
    }
}
