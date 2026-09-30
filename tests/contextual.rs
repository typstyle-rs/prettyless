#![cfg(feature = "contextual")]

mod macros;

use prettyless::*;

fn nest_on_line(doc: BoxDoc<'static>) -> BoxDoc<'static> {
    BoxDoc::softline().append(BoxDoc::nesting(move |n| {
        let doc = doc.clone();
        BoxDoc::column(move |c| {
            if n == c {
                BoxDoc::text("  ").append(doc.clone()).nest(2)
            } else {
                doc.clone()
            }
        })
    }))
}

#[test]
fn hang_lambda1() {
    let doc = chain![
        chain!["let", BoxDoc::line(), "x", BoxDoc::line(), "="].group(),
        nest_on_line(chain![
            "\\y ->",
            chain![BoxDoc::line(), "y"].nest(2).group()
        ]),
    ]
    .group();

    test_snapshot!(doc, @r"let x = \y -> y");
    test_snapshot!(8, doc, @r"
    let x =
      \y ->
        y
    ");
    test_snapshot!(14, doc, @r"
    let x = \y ->
      y
    ");
}

#[test]
fn hang_comment() {
    let body = chain!["y"].nest(2).group();
    let doc = chain![
        chain!["let", BoxDoc::line(), "x", BoxDoc::line(), "="].group(),
        nest_on_line(chain![
            "\\y ->",
            nest_on_line(chain!["// abc", BoxDoc::hard_line(), body])
        ]),
    ]
    .group();

    test_snapshot!(8, doc, @r"
    let x =
      \y ->
        // abc
        y
    ");
    test_snapshot!(14, doc, @r"
    let x = \y ->
      // abc
      y
    ");
}

#[test]
fn union() {
    let doc = chain![
        chain!["let", BoxDoc::line(), "x", BoxDoc::line(), "="].group(),
        nest_on_line(chain![
            "(",
            chain![
                BoxDoc::line_(),
                chain!["x", ","].group(),
                BoxDoc::line(),
                chain!["1234567890", ","].group()
            ]
            .nest(2)
            .group(),
            BoxDoc::line_().append(")"),
        ])
    ]
    .group();

    test_snapshot!(doc, @"let x = (x, 1234567890,)");
    test_snapshot!(8, doc, @r"
    let x =
      (
        x,
        1234567890,
      )
    ");
    test_snapshot!(14, doc, @r"
    let x = (
      x,
      1234567890,
    )
    ");
}

#[test]
fn union_suffixes_use_outer_column_at_flush() {
    let a = Arena::new();
    let suffix = a.on_column(|column| {
        if column == 2 {
            a.text("long").into_doc()
        } else {
            a.nil().into_doc()
        }
    });
    let doc = a.line_suffix("O") + (a.text("a") + a.line_suffix(suffix)).union(a.text("x"));
    assert_eq!(doc.print(2).to_string(), "aOlong");
    assert_eq!(doc.print(6).to_string(), "aOlong");
}

#[test]
fn union_suffixes_use_the_actual_flush_column() {
    let a = Arena::new();
    let suffix = a.on_column(|column| {
        if column == 2 {
            a.text("S").into_doc()
        } else {
            a.fail().into_doc()
        }
    });
    let doc = (a.text("a") + a.line_suffix(suffix)).union(a.text("x")) + a.text("b");
    assert_eq!(doc.print(80).to_string(), "abS");
}

#[test]
fn union_suffixes_do_not_limit_caller_text_width() {
    let a = Arena::new();
    let suffix = a.on_column(|column| {
        if column == 5 {
            a.hard_line().into_doc()
        } else {
            a.fail().into_doc()
        }
    });
    let doc = (a.text("a") + a.line_suffix(suffix)).union(a.text("x")) + a.text("long");
    assert_eq!(doc.print(1).to_string(), "along\n");
}

#[test]
fn fitting_tracks_each_commands_nesting() {
    let a = Arena::new();
    let nested = a
        .on_nesting(|n| {
            if n == 2 {
                a.text("a").into_doc()
            } else {
                a.fail().into_doc()
            }
        })
        .nest(2);
    let doc = (nested + a.line() + a.text("b")).group();
    assert_eq!(doc.print(3).to_string(), "a b");

    let rooted = a
        .on_nesting(|n| {
            if n == 0 {
                a.text("a").into_doc()
            } else {
                a.fail().into_doc()
            }
        })
        .dedent_to_root();
    let doc = (rooted + a.line() + a.text("b")).nest(2).group();
    assert_eq!(doc.print(3).to_string(), "a b");

    let continuation = a
        .on_nesting(|n| {
            if n == 3 {
                a.nil().into_doc()
            } else {
                a.fail().into_doc()
            }
        })
        .nest(3);
    let doc = (a.text("a") + a.line() + a.text("b")).group() + continuation;
    assert_eq!(doc.print(3).to_string(), "a b");
}

#[test]
fn fitting_tracks_alignment_for_nesting_callbacks() {
    let a = Arena::new();
    let doc = (a.text("ab")
        + (a.on_nesting(|n| {
            if n == 2 {
                a.text("c").into_doc()
            } else {
                a.fail().into_doc()
            }
        }) + a.line()
            + a.text("d"))
        .align())
    .group();
    assert_eq!(doc.print(5).to_string(), "abc d");
    assert_eq!(doc.print(4).to_string(), "abc\n  d");
}

#[test]
fn column_callbacks_include_pending_indentation_in_fitting_and_rendering() {
    let a = Arena::new();
    let doc = (a.hard_line()
        + (a.on_column(|c| {
            if c == 2 {
                a.text("ab").into_doc()
            } else {
                a.fail().into_doc()
            }
        }) + a.line()
            + a.text("x"))
        .group())
    .nest(2);

    assert_eq!(doc.print(6).to_string(), "\n  ab x");
    assert_eq!(doc.print(5).to_string(), "\n  ab\n  x");

    let doc = (a.hard_line()
        + a.text("ab")
            .measure_width(|width| a.as_string(width).into_doc()))
    .nest(2);
    assert_eq!(doc.print(5).to_string(), "\n  ab2");
}

#[test]
fn column_callbacks_observe_padding_without_committing_it() {
    let a = Arena::new();
    let doc = (a.text("a")
        + a.line()
        + a.text("b")
        + a.weak_space()
        + a.on_column(|c| {
            if c == 4 || c == 2 {
                a.ascii_text("").into_doc()
            } else {
                a.fail().into_doc()
            }
        }))
    .group();
    assert_eq!(doc.print(3).to_string(), "a b");
    assert_eq!(doc.print(2).to_string(), "a\nb");

    let doc = a.text("a") + a.weak_space() + a.on_column(|c| a.as_string(c).into_doc());
    assert_eq!(doc.print(3).to_string(), "a 2");
}

#[test]
fn alignment_and_dedentation_preserve_prospective_columns() {
    let a = Arena::new();
    let doc = (a.text("a")
        + a.weak_space()
        + (a.on_nesting(|n| {
            if n == 2 {
                a.text("b").into_doc()
            } else {
                a.fail().into_doc()
            }
        }) + a.line()
            + a.text("c"))
        .align())
    .group();
    assert_eq!(doc.print(5).to_string(), "a b c");
    assert_eq!(doc.print(4).to_string(), "a b\n  c");

    let doc = (a.hard_line()
        + (a.on_column(|c| a.as_string(c).into_doc()) + a.hard_line() + a.text("x")).align())
    .nest(2);
    assert_eq!(doc.print(3).to_string(), "\n  2\n  x");

    for to_root in [false, true] {
        let body = a.on_column(|c| a.as_string(c).into_doc()) + a.hard_line() + a.text("x");
        let body = if to_root {
            body.dedent_to_root()
        } else {
            body.dedent(2)
        };
        let doc = (a.hard_line() + body).nest(2);
        assert_eq!(doc.print(3).to_string(), "\n  2\nx");
    }
}

#[test]
fn indentation_policy_preserves_prospective_columns() {
    let a = Arena::new();
    let deferred = RenderOptions::new(4).with_indentation_policy(IndentationPolicy::Deferred);
    let doc = (a.hard_line()
        + (a.on_column(|c| a.as_string(c).into_doc()) + a.hard_line() + a.text("x")).align())
    .nest(2);

    assert_eq!(doc.print(4).to_string(), "\n  2\n  x");
    assert_eq!(doc.print_with(deferred).to_string(), "\n  2\n  x");
}

#[test]
fn same_line_suffix_callbacks_commit_padding_without_speculative_evaluation() {
    for empty in [false, true] {
        for width in [3, 4] {
            let calls = std::cell::Cell::new(0);
            let a = Arena::new();
            let suffix = a.on_column(|column| {
                calls.set(calls.get() + 1);
                assert_eq!(column, if width == 4 { 4 } else { 2 });
                if empty {
                    a.nil().into_doc()
                } else {
                    a.text("//").into_doc()
                }
            });
            let doc =
                (a.text("a") + a.line() + a.text("b") + a.weak_space() + a.line_suffix(suffix))
                    .group();
            let expected = match (width, empty) {
                (4, false) => "a b //",
                (4, true) => "a b ",
                (_, false) => "a\nb //",
                (_, true) => "a\nb ",
            };
            assert_eq!(doc.print(width).to_string(), expected);
            assert_eq!(calls.get(), 1);
        }
    }
}
