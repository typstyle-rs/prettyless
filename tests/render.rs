mod macros;

use std::borrow::Cow;

use prettyless::*;

#[test]
fn box_doc_inference() {
    let doc = BoxDoc::group(
        BoxDoc::text("test")
            .append(BoxDoc::line())
            .append(BoxDoc::text("test")),
    );

    test_snapshot!(doc, @"test test");
}

#[test]
fn newline_in_text() {
    let doc = BoxDoc::group(
        BoxDoc::text("test").append(
            BoxDoc::line()
                .append(BoxDoc::text("\"test\n     test\""))
                .nest(4),
        ),
    );

    test_snapshot!(5, doc, @r#"
    test
        "test
         test"
    "#);
}

#[test]
fn forced_newline() {
    let doc = BoxDoc::group(
        BoxDoc::text("test")
            .append(BoxDoc::hard_line())
            .append(BoxDoc::text("test")),
    );

    test_snapshot!(doc, @r"
    test
    test
    ");
}

#[test]
fn space_do_not_reset_pos() {
    let doc = BoxDoc::group(BoxDoc::text("test").append(BoxDoc::line()))
        .append(BoxDoc::text("test"))
        .append(BoxDoc::group(BoxDoc::line()).append(BoxDoc::text("test")));

    test_snapshot!(9, doc, @r"
    test test
    test
    ");
}

// Tests that the `BoxDoc::hard_line()` does not cause the rest of document to think that it fits on
// a single line but instead breaks on the `BoxDoc::line()` to fit with 6 columns
#[test]
fn newline_does_not_cause_next_line_to_be_to_long() {
    let doc = RcDoc::group(
        RcDoc::text("test").append(RcDoc::hard_line()).append(
            RcDoc::text("test")
                .append(RcDoc::line())
                .append(RcDoc::text("test")),
        ),
    );

    test_snapshot!(6, doc, @r"
    test
    test
    test
    ");
}

#[test]
fn newline_after_group_does_not_affect_it() {
    let arena = Arena::new();
    let doc = arena.text("x").append(arena.line()).append("y").group();

    test_snapshot!(100, doc.append(arena.hard_line()).1, @"x y");
}

#[test]
fn block() {
    let doc = RcDoc::group(
        RcDoc::text("{")
            .append(
                RcDoc::line()
                    .append(RcDoc::text("test"))
                    .append(RcDoc::line())
                    .append(RcDoc::text("test"))
                    .nest(2),
            )
            .append(RcDoc::line())
            .append(RcDoc::text("}")),
    );

    test_snapshot!(5, doc, @r"
    {
      test
      test
    }
    ");
}

#[test]
fn block_with_hardline() {
    let doc = RcDoc::group(
        RcDoc::text("{")
            .append(
                RcDoc::line()
                    .append(RcDoc::text("test"))
                    .append(RcDoc::hard_line())
                    .append(RcDoc::text("test"))
                    .nest(2),
            )
            .append(RcDoc::line())
            .append(RcDoc::text("}")),
    );

    test_snapshot!(10, doc, @r"
    {
      test
      test
    }
    ");
}

#[test]
fn block_with_hardline_negative_nest() {
    let doc = RcDoc::group(
        RcDoc::text("{")
            .append(
                RcDoc::line()
                    .append(RcDoc::text("test"))
                    .append(RcDoc::hard_line())
                    .append(RcDoc::text("test"))
                    .nest(-2),
            )
            .append(RcDoc::line())
            .append(RcDoc::text("}")),
    );

    test_snapshot!(10, doc, @r"
    {
    test
    test
    }
    ");
}

#[test]
fn line_comment() {
    let doc = BoxDoc::group(
        BoxDoc::text("{")
            .append(
                BoxDoc::line()
                    .append(BoxDoc::text("test"))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("// a").append(BoxDoc::hard_line()))
                    .append(BoxDoc::text("test"))
                    .nest(2),
            )
            .append(BoxDoc::line())
            .append(BoxDoc::text("}")),
    );

    test_snapshot!(14, doc, @r"
    {
      test
      // a
      test
    }
    ");
}

fn hang2(
    from: BoxDoc<'static>,
    body_whitespace: BoxDoc<'static>,
    body: BoxDoc<'static>,
    trailer: BoxDoc<'static>,
) -> BoxDoc<'static> {
    let body1 = body_whitespace
        .append(body.clone())
        .nest(2)
        .group()
        .append(trailer.clone());
    let body2 = BoxDoc::hard_line()
        .append(body.clone())
        .nest(2)
        .group()
        .append(trailer.clone());

    let single = from.clone().append(body1.clone()).group();

    let hang = from.clone().append(body2).group();

    let break_all = from.append(body1).group().nest(2);

    BoxDoc::group(single.union(hang.union(break_all)))
}

#[test]
fn hang_lambda2() {
    let from = BoxAllocator
        .pretty((
            BoxAllocator
                .pretty(("let", BoxDoc::line(), "x", BoxDoc::line(), "="))
                .group(),
            BoxDoc::line(),
            "\\y ->",
        ))
        .group()
        .into_doc();

    let body = BoxAllocator.pretty("y").group().into_doc();

    let trailer = BoxDoc::nil();

    let doc = hang2(from, BoxDoc::line(), body, trailer);

    test_snapshot!(doc, @r"let x = \y -> y");
    test_snapshot!(14, doc, @r"
    let x = \y ->
      y
    ");
}

#[test]
fn union2() {
    let from = BoxAllocator
        .pretty((
            BoxAllocator
                .pretty(("let", BoxDoc::line(), "x", BoxDoc::line(), "="))
                .group(),
            BoxDoc::line(),
            "(",
        ))
        .group()
        .into_doc();

    let body = BoxAllocator
        .pretty((
            BoxAllocator.pretty(("x", ",")).group(),
            BoxDoc::line(),
            BoxAllocator.pretty(("1234567890", ",")).group(),
        ))
        .group()
        .into_doc();

    let trailer = BoxDoc::line_().append(")");

    let doc = hang2(from, BoxDoc::line_(), body, trailer);

    test_snapshot!(doc, @"let x = (x, 1234567890,)");
    test_snapshot!(14, doc, @r"
    let x = (
      x,
      1234567890,
    )
    ");
}

#[test]
fn line_suffix_with_union() {
    let arena = Arena::new();

    let doc = arena.pretty((
        arena.text("a"),
        arena.line_suffix(" // 1"),
        arena.text("a"),
        (arena.line_suffix(" // 3") + arena.text("6666666"))
            .union(arena.line_suffix(" // 4") + arena.text("77")),
    ));

    test_snapshot!(5, doc, @"aa77 // 1 // 4");
}

#[test]
fn line_suffix_with_union2() {
    let arena = Arena::new();

    let doc = arena.pretty((
        arena.line_suffix(" // 1"),
        arena.text("a"),
        (arena.line_suffix(" // 3") + arena.hard_line() + arena.text("xxxxxxx"))
            .union(arena.line_suffix(" // 4") + arena.text("yyy")),
    ));

    test_snapshot!(5, doc, @"ayyy // 1 // 4");
}

#[test]
fn usize_max_value() {
    let doc = BoxDoc::group(
        BoxDoc::text("test")
            .append(BoxDoc::line())
            .append(BoxDoc::text("test")),
    );

    test_snapshot!(usize::MAX, doc, @"test test");
}

#[test]
fn fail() {
    let fail_break = BoxDoc::fail().flat_alt(Doc::nil());

    let doc = fail_break.append(Doc::text("12345")).group().union("abc");

    test_snapshot!(5, doc, @"12345");
    test_snapshot!(4, doc, @"abc");
}

#[test]
fn non_ascii_is_not_byte_length() {
    let doc = BoxDoc::group(
        BoxDoc::text("ÅÄÖ")
            .append(BoxDoc::line())
            .append(BoxDoc::text("test")),
    );

    test_snapshot!(8, doc, @"ÅÄÖ test");
}

#[test]
fn cjk_display_width() {
    let arena = Arena::new();
    let doc = arena
        .text("你好")
        .append(arena.line().append(arena.text("abc")).align());

    test_snapshot!(doc, @r"
    你好
        abc
    ");
}

#[test]
fn pretty_cow() {
    let doc = BoxAllocator
        .pretty((
            Cow::<str>::Borrowed("abc"),
            BoxDoc::line(),
            Cow::<str>::Owned("123".to_string()),
        ))
        .group();

    test_snapshot!(8, doc, @"abc 123");
}

#[test]
fn successful_union_defers_suffixes_in_order_until_the_callers_line_ends() {
    let a = Arena::new();
    let doc = a.line_suffix("outer") + (a.text("a") + a.line_suffix("inner")).union(a.text("x"));
    assert_eq!(doc.print(80).to_string(), "aouterinner");
    assert_eq!((doc + a.text("b")).print(80).to_string(), "abouterinner");

    // The caller's text precedes the suffix even when the line exceeds width.
    let doc = (a.text("a") + a.line_suffix("long")).union(a.text("x")) + a.text("b");
    assert_eq!(doc.print(1).to_string(), "ablong");
}

#[test]
fn deferred_suffix_failure_does_not_fall_back_to_another_union_branch() {
    let a = Arena::new();
    let doc = (a.text("a") + a.line_suffix(a.fail())).union(a.text("x")) + a.text("b");
    let mut out = String::new();
    assert!(doc.render_fmt(2, &mut out).is_err());
    assert_eq!(out, "ab");
}

#[test]
fn union_branches_leave_the_continuation_to_the_caller() {
    let a = Arena::new();
    let doc = ((a.text("a") + a.hard_line()).union(a.text("x")) + a.text("b")).nest(2);
    // The caller's over-width text must not reject a branch which already fits.
    assert_eq!(doc.print(1).to_string(), "a\n  b");

    let doc = ((a.text("long") + a.hard_line())
        .union((a.text("a") + a.hard_line()).union(a.text("x")))
        + a.text("b"))
    .nest(2);
    assert_eq!(doc.print(1).to_string(), "a\n  b");
    let doc = (a.text("a") + a.weak_space()).union(a.text("x")) + a.hard_line();
    assert_eq!(doc.print(1).to_string(), "a\n");
}

#[test]
fn successful_union_breaks_flush_outer_suffixes() {
    let a = Arena::new();
    let doc = a.text("a")
        + a.line_suffix("//outer")
        + (a.text("b") + a.hard_line()).union(a.text("x"))
        + a.text("c");
    assert_eq!(doc.print(80).to_string(), "ab//outer\nc");
}

#[test]
fn union_rollback_restores_suffixes_after_nested_breaks() {
    let a = Arena::new();
    for end in [a.fail(), a.text("toolong")] {
        let inner = (a.line_suffix("I") + a.hard_line() + a.text("ok")).union(a.text("r"));
        let left = inner + a.line_suffix("J") + a.hard_line() + end;
        let doc = a.text("p") + a.line_suffix("O") + left.union(a.text("x"));
        assert_eq!(doc.print(4).to_string(), "pxO");
        assert_eq!((doc + a.text("y")).print(4).to_string(), "pxyO");
    }
}
