use prettyless::*;

#[cfg(target_pointer_width = "64")]
#[test]
fn doc_size() {
    // Safeguard against accidentally growing Doc
    assert_eq!(8 * 3, std::mem::size_of::<Doc<RefDoc>>());
}

#[test]
fn debug_concat() {
    let a = Arena::new();
    let doc = (a.text("1") + a.text("2")) + a.text("3") + a.text("4");
    assert_eq!(
        format!("{doc:#?}"),
        r#"[
    "1",
    "2",
    "3",
    "4",
]"#
    )
}
