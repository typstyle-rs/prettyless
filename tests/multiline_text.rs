mod macros;

use std::borrow::Cow;

use prettyless::combinators::*;
use prettyless::text::MultilineTextData;
use prettyless::*;

/// Rendering options that defer indentation until a line is committed.
fn deferred(width: usize) -> RenderOptions {
    RenderOptions::new(width).with_indentation_policy(IndentationPolicy::Deferred)
}

/// Materializes a document for the places that must store one: arrays, loops and the print
/// macros, which go through `BoxAllocator`.
fn boxed<'a>(doc: impl Pretty<'a, BoxAllocator>) -> DocBuilder<'a, BoxAllocator> {
    BoxAllocator.pretty(doc)
}

#[test]
fn empty_input_is_nil_and_break_free_input_is_text() {
    let a = Arena::new();
    assert!(a.multiline_text("").is_nil());
    assert_eq!(a.multiline_text("").print(80).to_string(), "");

    // Without breaks the document *is* a text node, not a multiline payload.
    assert_eq!(
        format!("{:?}", a.multiline_text("abc")),
        format!("{:?}", a.text("abc"))
    );
    assert_eq!(a.multiline_text("abc").print(80).to_string(), "abc");
}

#[test]
fn borrowed_and_owned_sources_are_equivalent() {
    let a = Arena::new();
    for text in ["a\nb", "a\r\nb", "  x\n\ty\n"] {
        let borrowed = a.multiline_text(text);
        let owned = a.multiline_text(text.to_string());
        let cow = a.multiline_text(Cow::Borrowed(text));
        let owned_cow = a.multiline_text(Cow::Owned(text.to_string()));

        assert_eq!(borrowed.print(80).to_string(), owned.print(80).to_string());
        assert_eq!(borrowed.print(80).to_string(), cow.print(80).to_string());
        assert_eq!(
            borrowed.print(80).to_string(),
            owned_cow.print(80).to_string()
        );
        assert_eq!(format!("{borrowed:?}"), format!("{owned:?}"));
    }
}

#[test]
fn input_terminators_are_preserved() {
    assert_print!(80, boxed(multiline_text("a\nb")), "a\nb");
    // CRLF counts as one break and is emitted verbatim, not rewritten to LF.
    assert_print!(80, boxed(multiline_text("a\r\nb")), "a\r\nb");
    assert_print!(80, boxed(multiline_text("a\rb")), "a\rb");
    assert_print!(80, boxed(multiline_text("a\r\nb\rc\nd")), "a\r\nb\rc\nd");
    // A break may be the whole input.
    assert_print!(80, boxed(multiline_text("\n")), "\n");
}

#[test]
fn blank_lines_are_preserved() {
    assert_print!(80, boxed(multiline_text("a\n\nb")), "a\n\nb");
    assert_print!(80, boxed(multiline_text("a\n\n\nb")), "a\n\n\nb");
    // Only whitespace on a line is preserved verbatim.
    assert_print!(80, boxed(multiline_text("a\n \t\nb")), "a\n \t\nb");
}

#[test]
fn terminal_breaks_count() {
    // "a\n\nb\n" holds three hard breaks, the last one terminal: the output
    // ends with a break, and no phantom line follows it.
    let a = Arena::new();
    let out = a.multiline_text("a\n\nb\n").print(80).to_string();
    assert_eq!(out, "a\n\nb\n");
    assert_eq!(out.matches('\n').count(), 3);

    // A document after the terminal break starts on the new line, not on a
    // phantom empty line after it.
    assert_print!(80, boxed((multiline_text("a\n"), "b")), "a\nb");
    assert_print!(80, boxed((multiline_text("a\n\nb\n"), "c")), "a\n\nb\nc");
}

#[test]
fn first_line_starts_at_the_current_position() {
    let a = Arena::new();
    let doc = a.text("key: ") + a.multiline_text("one\n  two");
    assert_eq!(doc.print(80).to_string(), "key: one\n  two");

    // Nesting around the node affects only its first line: lines after an
    // embedded break act as if dedented to the root and keep only the source
    // indentation. `.align()` therefore positions nothing here.
    let doc = a.text("key: ") + a.multiline_text("one\n  two").align();
    assert_eq!(doc.print(80).to_string(), "key: one\n  two");

    // The first line does follow the current line's continuation indentation.
    assert_print!(
        80,
        boxed(nest(2)((hard_line(), multiline_text("a\nb")))),
        "\n  a\nb"
    );
}

#[test]
fn embedded_lines_ignore_surrounding_nesting() {
    // The node and separated texts render identically without nesting...
    assert_print!(80, boxed(multiline_text("a\nb")), "a\nb");
    assert_print!(80, boxed(("a", hard_line(), "b")), "a\nb");
    // ...but under `nest` the node's embedded lines stay at the root while a
    // separated hard line inherits the nesting.
    assert_print!(80, boxed(nest(2)(multiline_text("a\nb"))), "a\nb");
    assert_print!(80, boxed(nest(2)(("a", hard_line(), "b"))), "a\n  b");

    // The same for `align` and `dedent`-style offsets.
    assert_print!(80, boxed(align(multiline_text("a\n  b"))), "a\n  b");
}

#[test]
fn terminal_break_leaves_column_zero() {
    // An embedded terminal break writes no generated indentation: the
    // following document continues at column zero, whatever the nesting says.
    assert_print!(80, boxed(nest(2)((multiline_text("a\n"), "b"))), "a\nb");

    // Inside `align` too, and across a quoting pair.
    let a = Arena::new();
    let doc = a.text("key: ") + (a.multiline_text("a\n") + a.text("b")).align();
    assert_eq!(doc.print(80).to_string(), "key: a\nb");

    let doc = a.multiline_text("a\n").double_quotes().nest(2);
    assert_eq!(doc.print(80).to_string(), "\"a\n\"");

    // At the end of output no trailing indentation appears under Eager either.
    let doc = a.multiline_text("a\n").nest(2);
    assert_eq!(doc.print(80).to_string(), "a\n");
    assert_eq!(doc.print_with(deferred(80)).to_string(), "a\n");
}

#[test]
fn all_breaks_are_policy_agnostic() {
    // Embedded breaks emit no generated indentation under either policy, so
    // blank embedded lines stay empty and nesting never leaks into them.
    let doc = boxed(nest(2)(multiline_text("a\n\nb")));
    assert_print!(80, doc.clone(), "a\n\nb");
    assert_print_with!(deferred(80), doc, "a\n\nb");

    // The document after a terminal break starts at column zero under both.
    let doc = boxed(nest(2)((multiline_text("a\n\nb\n"), "c")));
    assert_print!(80, doc.clone(), "a\n\nb\nc");
    assert_print_with!(deferred(80), doc, "a\n\nb\nc");

    // A terminal break at end of output gains no trailing indentation.
    let doc = boxed(nest(2)(multiline_text("a\n")));
    assert_print!(0, doc.clone(), "a\n");
    assert_print_with!(deferred(80), doc, "a\n");
}

#[test]
fn weak_whitespace_resets_at_embedded_breaks() {
    // A weak space before the node is committed by the first line's text.
    assert_print!(
        80,
        boxed(("a", weak_space(), multiline_text("b\nc"))),
        "a b\nc"
    );
    // An empty first line commits nothing, and the break drops the space.
    assert_print!(
        80,
        boxed(("a", weak_space(), multiline_text("\nc"))),
        "a\nc"
    );
    // Weak spaces between embedded lines cannot occur; none are invented.
    assert_print!(80, boxed(multiline_text("a\n\nb")), "a\n\nb");
}

#[test]
fn zero_width_content_still_commits_weak_whitespace() {
    let a = Arena::new();
    let doc = a.text("a") + a.weak_space() + a.multiline_text("\u{301}\nb");
    assert_eq!(doc.print(80).to_string(), "a \u{301}\nb");

    let doc = a.multiline_text("a\n\u{301}") + a.weak_space() + a.text("b");
    assert_eq!(doc.print(80).to_string(), "a\n\u{301} b");

    // A terminal break, unlike a zero-width character, leaves the line fresh.
    let doc = a.multiline_text("a\r\n") + a.weak_space() + a.weak_line() + a.text("b");
    assert_eq!(doc.print(80).to_string(), "a\r\nb");
}

#[test]
fn suffixes_stay_queued_across_embedded_breaks() {
    // A queued suffix never lands inside the payload: it stays queued until
    // the next structural break or the end of rendering, so a string literal
    // keeps its contents intact.
    assert_print!(
        80,
        boxed(("a", line_suffix(" // 1"), multiline_text("\"b\nc\""))),
        "a\"b\nc\" // 1"
    );
    // At end of output the suffix lands on the node's last line...
    assert_print!(
        80,
        boxed(("a", line_suffix(" // 1"), multiline_text("b\nc"))),
        "ab\nc // 1"
    );
    // ...or on the empty line a terminal break left behind.
    assert_print!(
        80,
        boxed(("a", line_suffix(" // 1"), multiline_text("b\n"))),
        "ab\n // 1"
    );
    // A suffix queued after the node flushes at the end of output too.
    assert_print!(
        80,
        boxed((multiline_text("a\n"), line_suffix(" // 1"), "b")),
        "a\nb // 1"
    );
    // The next structural break after the node flushes it.
    assert_print!(
        80,
        boxed((
            multiline_text("a\nb"),
            line_suffix(" // 1"),
            hard_line(),
            "c"
        )),
        "a\nb // 1\nc"
    );
    // Pending weak whitespace is dropped at an embedded break; the later flush
    // of an empty suffix commits only what is still pending.
    assert_print!(
        80,
        boxed(("a", weak_space(), line_suffix(nil()), multiline_text("\nc"))),
        "a\nc"
    );
}

#[test]
fn groups_cannot_fit_multiline_text_flat() {
    assert_print!(
        80,
        boxed(group(("[", multiline_text("a\nb"), "]"))),
        "[a\nb]"
    );
    assert_print!(
        2,
        boxed(group(("[", multiline_text("a\nb"), "]"))),
        "[a\nb]"
    );

    // An explicit flat alternative supplies the flat representation.
    let doc = boxed(group(("[", flat_alt(multiline_text("a\nb"), "a; b"), "]")));
    assert_print!(80, doc.clone(), "[a; b]");
    assert_print!(4, doc, "[a\nb]");
}

#[test]
fn flat_layout_fails() {
    // A forced-flat multiline text fails to render...
    let mut s = String::new();
    assert!(
        boxed(flatten(multiline_text("a\nb")))
            .render_fmt(80, &mut s)
            .is_err()
    );

    // ...exactly like the same content written as separated nodes, so the
    // result never depends on the representation.
    let mut s = String::new();
    assert!(
        boxed(flatten(("a", hard_line(), "b")))
            .render_fmt(80, &mut s)
            .is_err()
    );

    // A hard line nested in a flattened document fails too.
    let mut s = String::new();
    assert!(
        boxed(flatten(("a", hard_line(), "b", hard_line(), "c")))
            .render_fmt(80, &mut s)
            .is_err()
    );

    // Inside a union the failed flat branch falls back to the alternative.
    assert_print!(
        80,
        boxed(boxed(multiline_text("a\nb")).flatten().union("a; b")),
        "a; b"
    );
}

#[test]
fn union_measures_every_line_of_the_preferred_branch() {
    let doc = boxed(union(
        multiline_text("aaaa\nbbbbbbbbbb"),
        multiline_text("aaaa\ntiny"),
    ));
    // Every line fits at width 10.
    assert_print!(10, doc.clone(), "aaaa\nbbbbbbbbbb");
    // The second line overflows at width 4, so the branch is rolled back.
    assert_print!(4, doc, "aaaa\ntiny");

    // The first line is measured from the column it starts at.
    let doc = boxed((
        "key: ",
        union(multiline_text("three\nx"), multiline_text("hi\nx")),
    ));
    assert_print!(10, doc.clone(), "key: three\nx");
    assert_print!(7, doc, "key: hi\nx");

    // A middle line can be the widest, even when the payload ends in a break.
    let doc = boxed(union(multiline_text("a\r\nlongest\rb\n"), "fallback"));
    assert_print!(7, doc.clone(), "a\r\nlongest\rb\n");
    assert_print!(6, doc, "fallback");
}

#[test]
fn union_rollback_restores_suffixes_queued_in_the_branch() {
    // The suffix queued inside the preferred branch is discarded with it when
    // a later line of the node overflows.
    let doc = boxed(union(
        (
            "a",
            line_suffix(" // s"),
            multiline_text("bbbb\ncccccccccccc"),
        ),
        multiline_text("abbbb\ndd"),
    ));
    assert_print!(8, doc, "abbbb\ndd");

    // When the branch fits, the suffix survives and flushes at end of output.
    let doc = boxed(("a", line_suffix(" // s"), multiline_text("bbbb\ncc")));
    assert_print!(80, doc, "abbbb\ncc // s");
}

#[test]
fn literal_union_selection_matches_buffered_selection() {
    let a = Arena::new();
    let prefixes = [
        a.nil(),
        a.text("x") + a.weak_space(),
        a.text("long"),
        a.hard_line().nest(2),
        a.text("x") + a.weak_space() + a.line_suffix(" // s"),
    ];
    for source in [
        "\r\n",
        "a\n",
        "\nlongest\nx",
        "\u{301}\nx",
        "\u{301}\r\n你好\r\r\né\n",
    ] {
        let literal = a.multiline_text(source);
        // Nesting keeps a wrapper around the leaf, exercising full speculation.
        // Embedded lines have identical layout under either representation.
        let direct = literal.clone().union(a.text("fallback"));
        let buffered = literal.nest(1).union(a.text("fallback"));
        for prefix in &prefixes {
            let direct = prefix.clone() + direct.clone() + a.text("!");
            let buffered = prefix.clone() + buffered.clone() + a.text("!");
            for width in [0, 1, 2, 4, 8] {
                for policy in [IndentationPolicy::Eager, IndentationPolicy::Deferred] {
                    let options = RenderOptions::new(width).with_indentation_policy(policy);
                    assert_eq!(
                        direct.print_with(options).to_string(),
                        buffered.print_with(options).to_string(),
                        "source={source:?}, width={width}, policy={policy:?}"
                    );
                }
            }
        }
    }
}

#[test]
fn nested_unions_preserve_transferred_output_and_rollback() {
    let a = Arena::new();
    let inner = (a.multiline_text("a\r\nbb") + a.text("!") + a.line_suffix(" // inner"))
        .union(a.text("inner fallback"));
    let nested = inner.clone().union(a.text("outer fallback"));
    assert_eq!(nested.print(3).to_string(), "a\r\nbb! // inner");

    // A nonempty parent buffer appends the accepted inner output in order.
    let nested = (a.text("p") + inner.clone()).union(a.text("outer fallback"));
    assert_eq!(nested.print(3).to_string(), "pa\r\nbb! // inner");

    // Failure after an accepted inner branch discards its output and suffix.
    let rejected = (inner + a.text("overflow")).union(a.text("outer fallback"));
    let doc = a.line_suffix(" // caller") + rejected;
    assert_eq!(doc.print(3).to_string(), "outer fallback // caller");
}

#[test]
fn partial_union_measures_through_the_first_break() {
    let doc = boxed(partial_union(
        multiline_text("short\nlong long long"),
        multiline_text("short\nshort"),
    ));
    // Only the first line has to fit; the rest of the node is not measured.
    assert_print!(10, doc, "short\nlong long long");

    // An empty first line commits nothing, even at an overflowing start column.
    let a = Arena::new();
    let doc = a.text("long")
        + a.multiline_text("\nchosen")
            .partial_union(a.text("fallback"));
    assert_eq!(doc.print(2).to_string(), "long\nchosen");

    // A zero-width first line does commit pending spaces and can overflow.
    let doc = a.text("x")
        + a.weak_space()
        + a.multiline_text("\u{301}\nchosen")
            .partial_union(a.multiline_text("\nother"));
    assert_eq!(doc.print(1).to_string(), "x\nother");
}

#[test]
fn unicode_line_widths_are_measured_per_line() {
    let doc = boxed(union(multiline_text("你好\nx"), multiline_text("ab\ncd")));
    // Each CJK character is two columns wide.
    assert_print!(4, doc.clone(), "你好\nx");
    assert_print!(3, doc, "ab\ncd");

    // Tabs measure as zero width, like in text documents; no expansion.
    assert_print!(80, boxed(multiline_text("a\tb\nc")), "a\tb\nc");
}

#[test]
fn unbreakable_lines_may_overflow() {
    assert_print!(3, boxed(multiline_text("a\nunbreakable")), "a\nunbreakable");
    assert_print!(3, boxed(multiline_text("unbreakable\na")), "unbreakable\na");
}

#[test]
fn render_options_do_not_rewrite_input_terminators() {
    let a = Arena::new();
    let doc = a.multiline_text("a\r\nb\rc");
    // Payload terminators are emitted verbatim under default options...
    assert_eq!(doc.print(80).to_string(), "a\r\nb\rc");
    // ...and an LF payload stays LF under CRLF options.
    let crlf = RenderOptions::new(80).with_line_ending(LineEnding::Crlf);
    assert_eq!(
        a.multiline_text("a\nb").print_with(crlf).to_string(),
        "a\nb"
    );
    // Structural breaks outside the payload do honor the option.
    let doc = a.multiline_text("a\nb") + a.hard_line() + a.text("c");
    assert_eq!(doc.print_with(crlf).to_string(), "a\nb\r\nc");
}

#[test]
fn column_tracks_the_first_and_last_line() {
    #[cfg(feature = "contextual")]
    {
        let a = Arena::new();
        // A callback before the node sees where its first line starts.
        let doc =
            a.text("ab") + a.on_column(|c| a.as_string(c).into_doc()) + a.multiline_text("x\ny");
        assert_eq!(doc.print(80).to_string(), "ab2x\ny");

        // After the node the column tracks its last line, which is at the root.
        let doc =
            a.text("ab") + a.multiline_text("x\ny") + a.on_column(|c| a.as_string(c).into_doc());
        assert_eq!(doc.print(80).to_string(), "abx\ny1");

        for (source, column) in [("x\r\n你好", 4), ("x\r\n", 0), ("x\r\u{301}", 0)] {
            let doc = a.multiline_text(source) + a.on_column(|c| a.as_string(c).into_doc());
            assert_eq!(doc.print(80).to_string(), format!("{source}{column}"));
        }
    }
}

#[test]
fn partial_writes_preserve_payload_and_propagate_errors() {
    struct CharWriter {
        output: String,
        remaining: usize,
    }

    impl Render for CharWriter {
        type Error = std::fmt::Error;

        fn write_str(&mut self, source: &str) -> Result<usize, Self::Error> {
            if self.remaining == 0 {
                return Err(std::fmt::Error);
            }
            let len = source.chars().next().unwrap().len_utf8();
            self.output.push_str(&source[..len]);
            self.remaining -= 1;
            Ok(len)
        }

        fn fail_doc(&self) -> Self::Error {
            std::fmt::Error
        }
    }

    let a = Arena::new();
    let source = "a\r\n你好\rb\n\u{301}";
    let doc = (a.hard_line() + a.multiline_text(source)).nest(2);
    let mut out = CharWriter {
        output: String::new(),
        remaining: usize::MAX,
    };
    assert!(doc.render_raw(80, &mut out).is_ok());
    assert_eq!(out.output, format!("\n  {source}"));

    let mut out = CharWriter {
        output: String::new(),
        remaining: 7,
    };
    assert!(doc.render_raw(80, &mut out).is_err());
    assert_eq!(out.output, "\n  a\r\n你");

    // A sink error after branch selection must propagate without a fallback.
    let doc = a.multiline_text(source).union(a.text("fallback"));
    let mut out = CharWriter {
        output: String::new(),
        remaining: 7,
    };
    assert!(doc.render_raw(80, &mut out).is_err());
    assert_eq!(out.output, "a\r\n你好\rb");
}

#[test]
fn static_constructors_match_the_allocator() {
    assert_eq!(BoxDoc::multiline_text("a\nb").print(80).to_string(), "a\nb");
    assert_eq!(RcDoc::multiline_text("a\nb").print(80).to_string(), "a\nb");
    assert_eq!(
        BoxDoc::multiline_text(String::from("a\nb"))
            .print(80)
            .to_string(),
        "a\nb"
    );

    let doc = Doc::<'static, BoxDoc<'static>>::multiline_text("a\nb");
    assert_eq!(doc.print(80).to_string(), "a\nb");

    let doc: BuildDoc<'static, BoxDoc<'static>> = BuildDoc::multiline_text("a\nb");
    assert_eq!(BoxAllocator.pretty(doc).print(80).to_string(), "a\nb");
}

#[test]
fn prelude_exports_the_combinator() {
    use prettyless::prelude::*;

    let arena = Arena::new();
    let doc = arena.pretty(multiline_text("one\n  two"));
    assert_eq!(doc.print(80).to_string(), "one\n  two");
}

#[test]
fn debug_shows_the_source_string() {
    let a = Arena::new();
    assert_eq!(
        format!("{:?}", a.multiline_text("a\nb")),
        r#"MultilineText("a\nb")"#
    );
}

#[test]
#[cfg(target_pointer_width = "64")]
fn boxed_payload_does_not_enlarge_documents() {
    // The payload lives behind a box, so ordinary variants keep their size.
    assert_eq!(std::mem::size_of::<Doc<RefDoc>>(), 8 * 3);
    assert_eq!(std::mem::size_of::<Box<MultilineTextData>>(), 8);
}
