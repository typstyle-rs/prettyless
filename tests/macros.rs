#[macro_export]
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

#[macro_export]
macro_rules! test_snapshot {
    ($size:expr, $doc:expr, @$expected:literal) => {
        let mut s = String::new();
        $doc.render_fmt($size, &mut s).unwrap();
        insta::assert_snapshot!(s, @$expected)
    };
    ($doc:expr, @$expected:expr) => {
        test_snapshot!(70, $doc, @$expected)
    };
}

/// Asserts that `doc`, rendered at `size`, is exactly `expected`.
///
/// `size` defaults to 80; pass it explicitly only when the test depends on a specific width.
/// `doc` is anything implementing `Pretty`, such as a tuple of documents or a `DocBuilder`.
/// Like `chain!`, the expansion uses the caller's `BoxAllocator` and `DocAllocator` imports.
#[macro_export]
macro_rules! assert_print {
    ($size:expr, $doc:expr, $expected:expr) => {
        assert_eq!(
            BoxAllocator.pretty($doc).print($size).to_string(),
            $expected
        )
    };
    ($doc:expr, $expected:expr) => {
        assert_print!(80, $doc, $expected)
    };
}
