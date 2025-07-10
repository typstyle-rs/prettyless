use std::borrow::Cow;

use prettyless::*;

macro_rules! chain {
    ($first: expr $(, $rest: expr)* $(,)?) => {{
        #[allow(unused_mut)]
        let mut doc = DocBuilder(&BoxAllocator, $first.into());
        $(
            doc = doc.append($rest);
        )*
        doc.into_doc()
    }}
}

macro_rules! test {
    ($size:expr, $actual:expr, $expected:expr) => {
        let mut s = String::new();
        $actual.render_fmt($size, &mut s).unwrap();
        difference::assert_diff!(&s, $expected, "\n", 0);
    };
    ($actual:expr, $expected:expr) => {
        test!(70, $actual, $expected)
    };
}

#[test]
fn box_doc_inference() {
    let doc = BoxDoc::group(
        BoxDoc::text("test")
            .append(BoxDoc::line())
            .append(BoxDoc::text("test")),
    );

    test!(doc, "test test");
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

    test!(5, doc, "test\n    \"test\n     test\"");
}

#[test]
fn forced_newline() {
    let doc = BoxDoc::group(
        BoxDoc::text("test")
            .append(BoxDoc::hardline())
            .append(BoxDoc::text("test")),
    );

    test!(doc, "test\ntest");
}

#[test]
fn space_do_not_reset_pos() {
    let doc = BoxDoc::group(BoxDoc::text("test").append(BoxDoc::line()))
        .append(BoxDoc::text("test"))
        .append(BoxDoc::group(BoxDoc::line()).append(BoxDoc::text("test")));

    test!(9, doc, "test test\ntest");
}

// Tests that the `BoxDoc::hardline()` does not cause the rest of document to think that it fits on
// a single line but instead breaks on the `BoxDoc::line()` to fit with 6 columns
#[test]
fn newline_does_not_cause_next_line_to_be_to_long() {
    let doc = RcDoc::group(
        RcDoc::text("test").append(RcDoc::hardline()).append(
            RcDoc::text("test")
                .append(RcDoc::line())
                .append(RcDoc::text("test")),
        ),
    );

    test!(6, doc, "test\ntest\ntest");
}

#[test]
fn newline_after_group_does_not_affect_it() {
    let arena = Arena::new();
    let doc = arena.text("x").append(arena.line()).append("y").group();

    test!(100, doc.append(arena.hardline()).1, "x y\n");
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

    test!(5, doc, "{\n  test\n  test\n}");
}

#[test]
fn block_with_hardline() {
    let doc = RcDoc::group(
        RcDoc::text("{")
            .append(
                RcDoc::line()
                    .append(RcDoc::text("test"))
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("test"))
                    .nest(2),
            )
            .append(RcDoc::line())
            .append(RcDoc::text("}")),
    );

    test!(10, doc, "{\n  test\n  test\n}");
}

#[test]
fn block_with_hardline_negative_nest() {
    let doc = RcDoc::group(
        RcDoc::text("{")
            .append(
                RcDoc::line()
                    .append(RcDoc::text("test"))
                    .append(RcDoc::hardline())
                    .append(RcDoc::text("test"))
                    .nest(-2),
            )
            .append(RcDoc::line())
            .append(RcDoc::text("}")),
    );

    test!(10, doc, "{\ntest\ntest\n}");
}

#[test]
fn line_comment() {
    let doc = BoxDoc::group(
        BoxDoc::text("{")
            .append(
                BoxDoc::line()
                    .append(BoxDoc::text("test"))
                    .append(BoxDoc::line())
                    .append(BoxDoc::text("// a").append(BoxDoc::hardline()))
                    .append(BoxDoc::text("test"))
                    .nest(2),
            )
            .append(BoxDoc::line())
            .append(BoxDoc::text("}")),
    );

    test!(14, doc, "{\n  test\n  // a\n  test\n}");
}

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
    let body2 = BoxDoc::hardline()
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
    let from = chain![
        chain!["let", BoxDoc::line(), "x", BoxDoc::line(), "="].group(),
        BoxDoc::line(),
        "\\y ->",
    ]
    .group();

    let body = chain!["y"].group();

    let trailer = BoxDoc::nil();

    let doc = hang2(from, BoxDoc::line(), body, trailer);
    eprintln!("{doc:#?}");

    test!(doc, "let x = \\y -> y");
    test!(14, doc, "let x = \\y ->\n  y");
}

#[test]
fn union2() {
    let from = chain![
        chain!["let", BoxDoc::line(), "x", BoxDoc::line(), "="].group(),
        BoxDoc::line(),
        "(",
    ]
    .group();

    let body = chain![
        chain!["x", ","].group(),
        BoxDoc::line(),
        chain!["1234567890", ","].group()
    ]
    .group();

    let trailer = BoxDoc::line_().append(")");

    let doc = hang2(from, BoxDoc::line_(), body, trailer);

    test!(doc, "let x = (x, 1234567890,)");
    test!(14, doc, "let x = (\n  x,\n  1234567890,\n)");
}

#[test]
fn usize_max_value() {
    let doc = BoxDoc::group(
        BoxDoc::text("test")
            .append(BoxDoc::line())
            .append(BoxDoc::text("test")),
    );

    test!(usize::MAX, doc, "test test");
}

#[test]
fn fail() {
    let fail_break = BoxDoc::fail().flat_alt(Doc::nil());

    let doc = fail_break.append(Doc::text("12345")).group().union("abc");

    test!(5, doc, "12345");
    test!(4, doc, "abc");
}

#[test]
fn non_ascii_is_not_byte_length() {
    let doc = BoxDoc::group(
        BoxDoc::text("ÅÄÖ")
            .append(BoxDoc::line())
            .append(BoxDoc::text("test")),
    );

    test!(8, doc, "ÅÄÖ test");
}

#[test]
fn cjk_display_width() {
    let arena = Arena::new();
    let doc = arena
        .text("你好")
        .append(arena.line().append(arena.text("abc")).align())
        .into_doc();

    test!(doc, "你好\n    abc");
}

#[test]
fn pretty_cow() {
    let doc = docs![
        &BoxAllocator,
        Cow::<str>::Borrowed("abc"),
        BoxDoc::line(),
        Cow::<str>::Owned("123".to_string()),
    ]
    .group()
    .into_doc();

    test!(8, doc, "abc 123");
}
