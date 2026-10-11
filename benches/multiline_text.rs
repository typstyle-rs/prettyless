use std::{hint::black_box, io};

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use prettyless::{Arena, DocAllocator};

/// Measures source scanning, literal branch selection, and nested union buffering.
fn multiline_text(c: &mut Criterion) {
    let mut construction = c.benchmark_group("multiline_text_construction");
    for (name, source) in [
        ("short", "hello\nworld".to_owned()),
        ("ascii_lf", ("a".repeat(63) + "\n").repeat(16_384)),
        ("ascii_crlf", ("a".repeat(63) + "\r\n").repeat(16_384)),
        ("unicode", "你好世界\n".repeat(80_660)),
        ("late_break", "a".repeat(1_048_576) + "\n"),
    ] {
        let arena = Arena::new();
        construction.throughput(Throughput::Bytes(source.len() as u64));
        construction.bench_function(BenchmarkId::new("borrowed", name), |b| {
            b.iter(|| arena.multiline_text(black_box(source.as_str())));
        });
        // Includes copying the source when constructing an owned payload.
        construction.bench_function(BenchmarkId::new("owned", name), |b| {
            b.iter(|| arena.multiline_text(black_box(source.clone())));
        });
    }
    construction.finish();

    // Discard output to isolate layout and speculative buffering costs.
    let mut rendering = c.benchmark_group("multiline_text_render_sink");
    let source = ("a".repeat(63) + "\n").repeat(16_384);
    rendering.throughput(Throughput::Bytes(source.len() as u64));
    let arena = Arena::new();
    let literal = arena.multiline_text(&source);
    let accepted = literal.clone().union(arena.text("fallback"));
    rendering.bench_function("literal", |b| {
        b.iter(|| black_box(&literal).render(80, &mut io::sink()).unwrap());
    });
    rendering.bench_function("literal_union", |b| {
        b.iter(|| black_box(&accepted).render(80, &mut io::sink()).unwrap());
    });

    // Compound branches still need speculation. Empty parent buffers can take
    // ownership of accepted inner output as nesting increases.
    for depth in [1, 8, 32] {
        let mut doc = arena.multiline_text(&source) + arena.text("!");
        for _ in 0..depth {
            doc = doc.union(arena.text("fallback"));
        }
        rendering.bench_with_input(BenchmarkId::new("compound_union", depth), &doc, |b, doc| {
            b.iter(|| black_box(doc).render(80, &mut io::sink()).unwrap());
        });
    }
    rendering.finish();

    let mut rejection = c.benchmark_group("multiline_text_union_rejection");
    for lines in [16, 16_384] {
        let source = ("a".repeat(63) + "\n").repeat(lines) + &"z".repeat(128);
        let arena = Arena::new();
        let doc = arena.multiline_text(&source).union(arena.text("fallback"));
        rejection.bench_with_input(BenchmarkId::from_parameter(lines), &doc, |b, doc| {
            b.iter(|| black_box(doc).render(80, &mut io::sink()).unwrap());
        });
    }
    rejection.finish();
}

criterion_group!(benches, multiline_text);
criterion_main!(benches);
