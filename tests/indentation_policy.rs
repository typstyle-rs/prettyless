mod macros;

use prettyless::combinators::*;
use prettyless::*;

/// Rendering options that defer indentation until a line is committed.
fn deferred(width: usize) -> RenderOptions {
    RenderOptions::new(width).with_indentation_policy(IndentationPolicy::Deferred)
}

/// Materializes a document for the few places that must store one: arrays, loops and snapshots.
fn boxed<'a>(doc: impl Pretty<'a, BoxAllocator>) -> DocBuilder<'a, BoxAllocator> {
    BoxAllocator.pretty(doc)
}

/// An empty text document; an empty string would produce `nil` instead.
fn empty_text() -> DocBuilder<'static, BoxAllocator> {
    BoxAllocator.ascii_text("")
}

/// An empty text with an explicit length of zero, as [`Doc::TextWithLen`].
fn empty_text_with_len() -> DocBuilder<'static, BoxAllocator> {
    boxed(Doc::TextWithLen(0, empty_text().into_doc()))
}

/// Asserts that `doc` renders as `expected` under either indentation policy.
#[track_caller]
fn assert_policies(width: usize, doc: impl Pretty<'static, BoxAllocator> + Clone, expected: &str) {
    assert_print!(width, doc.clone(), expected);
    assert_print_with!(deferred(width), doc, expected);
}

/// Asserts that both indentation policies render `doc` identically at every width below 10.
#[track_caller]
fn assert_policy_agnostic(doc: impl Pretty<'static, BoxAllocator> + Clone) {
    let doc = boxed(doc);
    for width in 0..10 {
        assert_eq!(
            doc.print(width).to_string(),
            doc.print_with(deferred(width)).to_string(),
            "policy changed output at width {width}"
        );
    }
}

#[test]
fn test_hard_line() {
    let doc = nest(2)(("aaa", hard_line(), hard_line(), "bbb"));

    // Eager indentation (the default) follows each break immediately.
    test_snapshot!(80, boxed(doc.clone()), @"aaa\n  \n  bbb");

    assert_print_with!(deferred(80), doc, "aaa\n\n  bbb");
}

#[test]
fn hard_lines_emit_terminal_indentation_eagerly() {
    assert_print!(0, nest(2)(hard_line()), "\n  ");
    assert_print!(1, nest(2)(("a", weak_space(), hard_line())), "a\n  ");

    let doc = nest(2)(("a", weak_line()));
    assert_print!(1, doc.clone(), "a\n  ");

    let eager = RenderOptions::new(0).with_indentation_policy(IndentationPolicy::Eager);
    assert_eq!(eager.indentation_policy, IndentationPolicy::Eager);
    assert_print_with!(eager, doc, "a\n  ");

    // Eager is the default and survives unrelated option changes.
    assert_eq!(
        RenderOptions::new(80).indentation_policy,
        IndentationPolicy::Eager
    );
    let changed = RenderOptions::new(80)
        .with_indentation_policy(IndentationPolicy::Deferred)
        .with_width(40);
    assert_eq!(changed.width, 40);
    assert_eq!(changed.indentation_policy, IndentationPolicy::Deferred);
}

#[test]
fn deferred_indentation_omits_terminal_indentation() {
    assert_print_with!(deferred(0), nest(2)(hard_line()), "\n");
    assert_print_with!(
        deferred(1),
        nest(2)(("a", weak_space(), hard_line())),
        "a\n"
    );
    assert_print_with!(deferred(1), nest(2)(("a", weak_line())), "a\n");
}

#[test]
fn indentation_policy_changes_only_emission_timing() {
    // Every line is committed by content, so layout, fitting, and union choice are identical and
    // only blank/terminal lines can differ.
    assert_policy_agnostic(nest(2)(("aaa", line_or_space(), "bbb")));
    assert_policy_agnostic(nest(2)(("aaa", hard_line(), "bbb")));
    assert_policy_agnostic(nest(2)(("aaa", flat_alt(weak_line(), space()), "bbb")));
    assert_policy_agnostic(nest(2)(group((
        line_or_nil(),
        "aaa",
        line_or_space(),
        "bbb",
    ))));
    assert_policy_agnostic(union(nest(2)(("aaa", line_or_space(), "bbb")), "fallback"));
    assert_policy_agnostic(("a", repeat(2)(weak_space()), "b", line_suffix("//")));
    assert_policy_agnostic(nest(2)(group(("aaa", line_or_space(), "bbb"))));

    // Weak spaces are still deferred under eager indentation.
    assert_policies(
        80,
        nest(2)(("a", repeat(2)(weak_space()), hard_line(), "b")),
        "a\n  b",
    );
}

#[test]
fn hard_line_indentation_does_not_commit_weak_whitespace() {
    let doc = nest(2)((
        "a",
        hard_line(),
        weak_line(),
        weak_space(),
        empty_text(),
        weak_line(),
        "b",
    ));
    assert_print!(3, doc, "a\n  b");

    let doc = nest(2)((
        hard_line(),
        partial_union((empty_text(), weak_space(), weak_line(), "long"), "x"),
    ));
    assert_print!(3, doc, "\n  x");
}

#[test]
fn test_weak_space() {
    let doc = nest(2)((
        weak_space(),
        "aaa",
        weak_space(),
        weak_line(),
        weak_space(),
        "bbb",
    ));

    test_snapshot!(80, boxed(doc), @"aaa\n  bbb");
}

#[test]
fn test_weak_line() {
    let doc = (
        "(",
        indent(2)((
            weak_line(),
            ("aaa", ",", weak_space(), "// comment", weak_line()),
            weak_line(),
            ("bbb", ","),
        )),
        weak_line(),
        ")",
    );

    test_snapshot!(80, boxed(doc), @"(\n  aaa, // comment\n  bbb,\n)");
}

#[test]
fn weak_line_constructors_are_primitives() {
    // Each constructor yields the bare `Doc::WeakLine`: no enclosing group, no flat alternative.
    // `Doc`/`BuildDoc` share the generated body and differ only in when allocation happens, as do
    // `BoxDoc`/`RcDoc` in their pointer type, so one check per path is enough.
    assert_print!(group(("a", weak_line(), "b")), "a\nb");
    assert_print!(group(("a", Doc::weak_line(), "b")), "a\nb");
    assert_print!(group(("a", BoxDoc::weak_line(), "b")), "a\nb");
}

#[test]
fn weak_line_constructors_suppress_empty_broken_lines() {
    assert_print!(1, group((weak_line(), "long")), "long");
    assert_print!(1, group((flat_alt(weak_line(), space()), "long")), "long");
    assert_print!(1, group((flat_alt(weak_line(), nil()), "long")), "long");

    let doc = nest(2)((weak_line(), "a", weak_line()));
    assert_print!(80, doc.clone(), "a\n  ");
    assert_print_with!(deferred(80), doc, "a\n");
}

#[test]
fn trailing_weak_spaces_do_not_break_groups() {
    let doc = group(("a", line_or_space(), "b", repeat(2)(weak_space())));

    assert_print!(3, doc.clone(), "a b");
    assert_print!(2, doc.clone(), "a\nb");
    assert_print!(3, (doc.clone(), hard_line()), "a b\n");
    assert_print!(3, (doc, weak_line()), "a b\n");
}

#[test]
fn weak_space_contract() {
    // Leading weak spaces disappear; consecutive ones accumulate.
    assert_print!((repeat(3)(weak_space()), "a"), "a");
    assert_print!(("a", repeat(3)(weak_space()), "b"), "a   b");

    // Pending weak spaces disappear at a break or at the end of output.
    assert_print!(("a", repeat(3)(weak_space()), hard_line(), "b"), "a\nb");
    assert_print!(("a", repeat(3)(weak_space())), "a");

    // Group boundaries keep them: text on either side still commits them.
    assert_print!((group(("a", weak_space())), "b"), "a b");
    assert_print!(("a", weak_space(), group("b")), "a b");

    // Explicit spaces and source text stay literal.
    assert_print!(("a", space(), hard_line()), "a \n");
    assert_print!("a  ", "a  ");
}

#[test]
fn weak_spaces_count_when_text_commits_them() {
    let doc = group((
        weak_space(),
        "a",
        repeat(2)(weak_space()),
        empty_text(),
        "b",
        line_or_space(),
        "c",
    ));

    assert_print!(6, doc.clone(), "a  b c");
    assert_print!(5, doc, "a  b\nc");
}

#[test]
fn weak_spaces_are_committed_by_suffix_content() {
    let doc = ("a", weak_space(), line_suffix("//"));

    assert_print!(1, doc.clone(), "a //");
    assert_print!(1, (doc.clone(), hard_line()), "a //\n");
    assert_print!(1, (doc, weak_line()), "a //\n");

    // Suffix width still does not force groups or partial unions to break.
    let doc = group(("a", line_or_space(), "b", weak_space(), line_suffix("//")));
    assert_print!(4, doc.clone(), "a b //");
    assert_print!(3, doc, "a\nb //");
    let doc = partial_union(("a", weak_space(), line_suffix("//"), hard_line()), "x");
    assert_print!(2, doc.clone(), "a //\n");
    assert_print!(1, doc, "x");
}

#[test]
fn empty_suffixes_commit_weak_whitespace_without_inspecting_content() {
    let doc = ("a", weak_space(), line_suffix(nil()));
    assert_print!(2, doc.clone(), "a ");
    assert_print!(2, (doc, hard_line()), "a \n");

    assert_print!(
        group((line_suffix(nil()), weak_line(), "a", line_or_space(), "b")),
        "\na\nb"
    );
}

#[test]
fn full_unions_count_weak_padding_committed_by_deferred_suffixes() {
    for suffix in ["//", ""] {
        let left = ("a", weak_space(), line_suffix(suffix));
        assert_print!(1, union(left.clone(), "x"), "x");
        assert_print!(1, partial_union(left, "x"), "x");
    }
}

#[test]
fn suppressed_weak_lines_do_not_end_fitting() {
    assert_print!(1, partial_union((weak_line(), "long"), "x"), "x");

    let doc = group((weak_line(), "a", line_or_space(), "b"));
    assert_print!(3, doc.clone(), "a b");
    assert_print!(2, doc, "a\nb");

    assert_print!(
        3,
        nest(2)((hard_line(), partial_union((weak_line(), "long"), "x"))),
        "\n  x"
    );

    // Once the weak line is emitted, only the first line of a partial union matters.
    assert_print!(
        1,
        partial_union(("a", weak_space(), weak_line(), "long"), "x"),
        "a\nlong"
    );
}

#[test]
fn nonempty_suffixes_make_weak_lines_break() {
    assert_print!(
        group((line_suffix("//"), weak_line(), "a", line_or_space(), "b")),
        "//\na\nb"
    );
}

#[test]
fn empty_text_does_not_commit_indentation() {
    assert_print!(
        1,
        group((empty_text(), empty_text_with_len(), weak_line(), "x")),
        "x"
    );

    let doc = nest(2)((hard_line(), empty_text(), hard_line(), "x"));
    assert_print!(3, doc.clone(), "\n  \n  x");
    assert_print_with!(deferred(3), doc, "\n\n  x");

    let doc = nest(2)(("a", weak_line(), empty_text(), weak_line()));
    assert_print!(3, doc.clone(), "a\n  ");
    assert_print_with!(deferred(3), doc, "a\n");

    // Nonempty Unicode text of zero display width still makes a line nonempty.
    assert_print!(1, ("\u{301}", weak_line(), "x"), "\u{301}\nx");
}

#[test]
fn union_fallback_restores_indentation_and_padding() {
    // Rollback restores pending indentation and padding under either policy.
    assert_policies(3, nest(2)((hard_line(), union("long", "x"))), "\n  x");
    assert_policies(
        3,
        nest(2)((hard_line(), union("long", (weak_space(), weak_line(), "x")))),
        "\n  x",
    );
    assert_policies(
        3,
        ("p", weak_space(), union(("long", hard_line(), fail()), "x")),
        "p x",
    );
    assert_policies(
        3,
        ("p", union((hard_line(), fail()), (weak_space(), "x"))),
        "p x",
    );
    assert_policies(2, (union(("a", weak_space(), fail()), "x"), "y"), "xy");
}

#[test]
fn union_fallback_preserves_outer_suffixes() {
    assert_print!(
        3,
        (
            "p",
            weak_space(),
            line_suffix("outer"),
            union((line_suffix("inner"), "long"), "x"),
        ),
        "p xouter"
    );

    // A deferred suffix does not affect the union's branch choice.
    assert_print!(1, union(("x", line_suffix("long")), "y"), "xlong");
}

#[test]
fn union_rollback_restores_suffixes_consumed_by_nested_successful_branches() {
    for end in [boxed(fail()), boxed("toolong")] {
        let inner = union((line_suffix("I"), weak_line(), "ok"), "r");
        let left = (inner, line_suffix("J"), weak_line(), end);
        let doc = ("p", weak_space(), line_suffix("O"), union(left, "x"));
        assert_print!(4, doc.clone(), "p xO");
        assert_print!(4, (doc, "y"), "p xyO");
    }
}

#[test]
fn nested_union_commits_flush_each_suffix_once() {
    let inner = union((line_suffix("I"), weak_line(), "b"), "r");
    let doc = (
        line_suffix("O"),
        union((inner, line_suffix("J"), hard_line()), "x"),
        "c",
    );
    assert_print!(80, doc, "OI\nbJ\nc");
}

#[test]
fn mandatory_breaks_preserve_blank_lines_without_indentation() {
    let breaking = [
        boxed(hard_line()),
        boxed(line_or_space()),
        boxed(line_or_nil()),
    ];
    for line in breaking {
        assert_print_with!(
            deferred(0),
            nest(4)(("a", repeat(3)(line.clone()), "b")),
            "a\n\n\n    b"
        );
        assert_print!(
            0,
            nest(4)(("a", repeat(3)(line), "b")),
            "a\n    \n    \n    b"
        );
    }
    for line in [boxed(soft_line_or_space()), boxed(soft_line_or_nil())] {
        assert_policies(0, nest(4)(("a", line, "b")), "a\n    b");
    }
    let doc = nest(4)((repeat(3)(hard_line()), empty_text(), weak_space()));
    assert_print_with!(deferred(0), doc.clone(), "\n\n\n");
    assert_print!(0, doc, "\n    \n    \n    ");
}

#[test]
fn union_terminal_breaks_do_not_commit_over_width_indentation() {
    // Deferred indentation is never committed at a branch boundary, so it cannot inflate the
    // checked width nor leak into a fallback.
    let doc = union(nest(20)(("a", repeat(2)(hard_line()))), "fallback");
    assert_print_with!(deferred(1), doc.clone(), "a\n\n");
    assert_print!(1, doc, format!("a\n{}\n{}", " ".repeat(20), " ".repeat(20)));

    assert_print!(1, (union(nest(2)(("a", hard_line())), "x"), "b"), "a\nb");
}
