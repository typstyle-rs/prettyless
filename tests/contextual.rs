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
