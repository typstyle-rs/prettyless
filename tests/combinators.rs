mod macros;

use prettyless::combinators::*;
use prettyless::{BoxAllocator, DocAllocator};

// === Leaves ===

#[test]
fn nil_is_empty() {
    assert_print!(("a", nil(), "b"), "ab");
}

#[test]
fn line_breaks_choose_nil_or_space() {
    assert_print!(group(("a", line_or_nil(), "b")), "ab");
    assert_print!(group(("a", line_or_space(), "b")), "a b");
    assert_print!(1, group(("a", line_or_nil(), "b")), "a\nb");
}

#[test]
fn soft_line_tokens_group_themselves() {
    assert_print!(("a", soft_line_or_space(), "b"), "a b");
    assert_print!(1, ("a", soft_line_or_space(), "b"), "a\nb");
    assert_print!(("a", soft_line_or_nil(), "b"), "ab");
}

#[test]
fn spaces_token_emits_spaces() {
    assert_print!(spaces(3), "   ");
}

#[test]
fn weak_tokens_defer_whitespace() {
    // A weak line breaks even inside a group that would fit, and has no flat alternative.
    assert_print!(group(("a", weak_line(), "b")), "a\nb");
    assert_print!(1, group((weak_line(), "long")), "long");

    // An explicit flat alternative makes it choose a space when the group fits, or nothing.
    assert_print!(3, group(("a", flat_alt(weak_line(), space()), "b")), "a b");
    assert_print!(2, group(("a", flat_alt(weak_line(), space()), "b")), "a\nb");
    assert_print!(2, group(("a", flat_alt(weak_line(), nil()), "b")), "ab");
    assert_print!(1, group(("a", flat_alt(weak_line(), nil()), "b")), "a\nb");

    // Weak spaces vanish at line boundaries and accumulate between text.
    assert_print!((weak_space(), "a"), "a");
    assert_print!(("a", weak_space(), weak_space(), "b"), "a  b");
    assert_print!(("a", weak_space()), "a");
}

#[test]
fn as_string_renders_display_values() {
    assert_print!(("x = ", as_string(42)), "x = 42");
}

#[test]
fn expand_parent_forces_group_to_break() {
    assert_print!(group(("a", expand_parent(), line_or_space(), "b")), "a\nb");
}

// === Nesting ===

#[test]
fn nest_indent_and_dedent_offset_nesting() {
    let doc = (
        nest(4)((hard_line(), "a")),
        nest(4)((hard_line(), "b", dedent(2)((hard_line(), "c")))),
    );

    assert_print!(doc, "\n    a\n    b\n  c");
}

// === Layout ===

#[test]
fn align_sets_indentation_to_current_column() {
    let doc = (
        "lorem ",
        align(intersperse(line_or_nil())(["ipsum", "dolor"])),
        hard_line(),
        "next",
    );

    assert_print!(doc, "lorem ipsum\n      dolor\nnext");
}

#[test]
fn dedent_to_root_ignores_enclosing_indent() {
    let doc = indent(4)((
        "a",
        dedent_to_root(("b", hard_line(), "c")),
        hard_line(),
        "e",
    ));

    assert_print!(10, doc, "ab\nc\n    e");
}

#[test]
fn flatten_forces_flat_rendering() {
    assert_print!(1, flatten(("a", line_or_space(), "b")), "a b");
}

// === Trailing content ===

#[test]
fn line_suffix_flushes_at_line_end() {
    let doc = ("a", line_suffix(" // comment"), hard_line(), "b");

    assert_print!(doc, "a // comment\nb");
}

// === Alternatives ===

#[test]
fn union_defers_branch_selection() {
    assert_print!(5, union("short", "much longer text"), "short");
    assert_print!(1, union("short", "much longer text"), "much longer text");
}

#[test]
fn fail_aborts_union_branch() {
    assert_print!(union(fail(), "fallback"), "fallback");
}

#[test]
fn partial_union_only_fits_the_first_line() {
    let doc = partial_union(
        ("short", hard_line(), "long long long"),
        ("short", hard_line(), "short"),
    );

    assert_print!(10, doc, "short\nlong long long");
}

#[test]
fn flat_alt_picks_branch_by_layout() {
    assert_print!(group(("a", flat_alt(line_or_space(), ","), "b")), "a,b");
    assert_print!(2, group(("a", flat_alt(line_or_space(), ","), "b")), "a\nb");
}

// === Iteration ===

#[test]
fn concat_materializes_in_order() {
    assert_print!(concat(["a", "b", "c"]), "abc");
}

#[test]
fn intersperse_accepts_shared_token_separator() {
    let doc = BoxAllocator
        .pretty(intersperse(line_or_space())(["a", "b", "c"]))
        .group();

    assert_print!(doc.clone(), "a b c");
    assert_print!(3, doc, "a\nb\nc");
}

#[test]
fn intersperse_separator_function_is_reusable() {
    let spaced = intersperse(line_or_space());

    let first = BoxAllocator.pretty(spaced(["a", "b"])).group();
    let second = BoxAllocator.pretty(spaced(["c", "d"])).group();

    assert_print!(first, "a b");
    assert_print!(second, "c d");
}

#[test]
fn repeat_repeats_deferred_doc() {
    assert_print!(repeat(3)("ab"), "ababab");
}

// === Interop ===

#[test]
fn combinators_compose_with_docs_macro() {
    let doc = prettyless::docs![&BoxAllocator, "a", line_or_space(), "b"].group();

    assert_print!(doc, "a b");
}

// === Contextual ===

#[cfg(feature = "contextual")]
#[test]
fn on_column_defers_column_lookup() {
    let doc = (
        "prefix ",
        on_column(|column| BoxAllocator.pretty(("col ", as_string(column))).into_doc()),
    );

    assert_print!(doc, "prefix col 7");
}

#[cfg(feature = "contextual")]
#[test]
fn on_nesting_defers_nesting_lookup() {
    let doc = nest(4)(on_nesting(|level| {
        BoxAllocator.pretty(as_string(level)).into_doc()
    }));

    assert_print!(doc, "4");
}
