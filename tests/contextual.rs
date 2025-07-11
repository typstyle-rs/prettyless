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

    test!(doc, "let x = \\y -> y");
    test!(
        8,
        doc,
        r"let x =
  \y ->
    y"
    );
    test!(
        14,
        doc,
        r"let x = \y ->
  y"
    );
}

#[test]
fn hang_comment() {
    let body = chain!["y"].nest(2).group();
    let doc = chain![
        chain!["let", BoxDoc::line(), "x", BoxDoc::line(), "="].group(),
        nest_on_line(chain![
            "\\y ->",
            nest_on_line(chain!["// abc", BoxDoc::hardline(), body])
        ]),
    ]
    .group();

    test!(8, doc, "let x =\n  \\y ->\n    // abc\n    y");
    test!(14, doc, "let x = \\y ->\n  // abc\n  y");
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

    test!(doc, "let x = (x, 1234567890,)");
    test!(8, doc, "let x =\n  (\n    x,\n    1234567890,\n  )");
    test!(14, doc, "let x = (\n  x,\n  1234567890,\n)");
}
