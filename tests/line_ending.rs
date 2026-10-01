mod macros;

use prettyless::*;

#[test]
fn crlf_line_endings() {
    let a = Arena::new();
    let doc = (a.text("x") + a.hard_line() + a.text("y")).nest(2) + a.hard_line() + a.text("z");
    let crlf = RenderOptions::new(80).with_line_ending(LineEnding::Crlf);

    assert_eq!(crlf.width(), 80);
    assert_eq!(crlf.line_ending(), LineEnding::Crlf);
    assert_eq!(RenderOptions::new(80).line_ending(), LineEnding::Lf);
    // `with_width` reuses configured options without dropping the other settings.
    assert_eq!(crlf.with_width(40).width(), 40);
    assert_eq!(crlf.with_width(40).line_ending(), LineEnding::Crlf);

    assert_eq!(doc.print_with(crlf).to_string(), "x\r\n  y\r\nz");
    assert_eq!(
        doc.print_with(RenderOptions::new(80)).to_string(),
        doc.print(80).to_string()
    );
    assert_eq!(doc.print(80).to_string(), "x\n  y\nz");

    let mut bytes = Vec::new();
    doc.render_with(crlf, &mut bytes).unwrap();
    assert_eq!(bytes, b"x\r\n  y\r\nz");

    let mut s = String::new();
    doc.render_fmt_with(crlf, &mut s).unwrap();
    assert_eq!(s, "x\r\n  y\r\nz");

    assert_eq!(LineEnding::Crlf.as_str(), "\r\n");
    assert_eq!(LineEnding::Lf.as_str(), "\n");
}

#[test]
fn crlf_preserves_layout() {
    let a = Arena::new();
    let doc = (a.text("test") + a.line() + a.text("test")).group();

    let lf = doc.print(5).to_string();
    let crlf = doc
        .print_with(RenderOptions::new(5).with_line_ending(LineEnding::Crlf))
        .to_string();

    assert_eq!(lf, "test\ntest");
    assert_eq!(crlf, "test\r\ntest");
    assert_eq!(crlf, lf.replace('\n', "\r\n"));
}

#[test]
fn crlf_keeps_embedded_newlines_verbatim() {
    let doc = BoxDoc::text("a\nb")
        .append(BoxDoc::hard_line())
        .append(BoxDoc::text("c"));

    let crlf = RenderOptions::new(80).with_line_ending(LineEnding::Crlf);
    assert_eq!(doc.print_with(crlf).to_string(), "a\nb\r\nc");
}

#[test]
fn crlf_union_speculation() {
    let a = Arena::new();
    let doc = (a.text("a") + a.hard_line() + a.text("b")).union(a.text("long long"));

    let crlf = RenderOptions::new(1).with_line_ending(LineEnding::Crlf);
    assert_eq!(doc.print_with(crlf).to_string(), "a\r\nb");
}

#[test]
fn crlf_union_rollback() {
    let a = Arena::new();
    // The left branch buffers "ab\r\ncd" before failing on width, so rollback must
    // discard those bytes whole and leave the fallback's own break intact.
    let doc = (a.text("ab") + a.hard_line() + a.text("cd")).union(a.text("z"))
        + a.hard_line()
        + a.text("w");

    let crlf = RenderOptions::new(1).with_line_ending(LineEnding::Crlf);
    assert_eq!(doc.print(1).to_string(), "z\nw");
    assert_eq!(doc.print_with(crlf).to_string(), "z\r\nw");
}

#[test]
fn crlf_line_suffix() {
    let a = Arena::new();
    let doc = a.text("a") + a.line_suffix("// c") + a.hard_line() + a.text("b");

    let crlf = RenderOptions::new(80).with_line_ending(LineEnding::Crlf);
    assert_eq!(doc.print_with(crlf).to_string(), "a// c\r\nb");
}

#[cfg(feature = "contextual")]
#[test]
fn crlf_column_callbacks_observe_the_same_columns() {
    let a = Arena::new();
    // The suffix fires only when the flush column is exactly 5, so a terminator length
    // leaking into column accounting would make it fail under CRLF.
    let suffix = a.on_column(|column| {
        if column == 5 {
            a.hard_line().into_doc()
        } else {
            a.fail().into_doc()
        }
    });
    let doc = (a.text("a") + a.line_suffix(suffix)).union(a.text("x")) + a.text("long");

    assert_eq!(doc.print(1).to_string(), "along\n");
    let crlf = RenderOptions::new(1).with_line_ending(LineEnding::Crlf);
    assert_eq!(doc.print_with(crlf).to_string(), "along\r\n");
}
