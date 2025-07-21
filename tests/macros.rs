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
