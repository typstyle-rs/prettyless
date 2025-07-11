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
