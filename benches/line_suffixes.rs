use std::io;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use prettyless::{Arena, DocAllocator};

fn line_suffixes(c: &mut Criterion) {
    let mut group = c.benchmark_group("line_suffixes");
    for size in [1_000, 2_000, 4_000, 8_000, 16_000] {
        group.throughput(Throughput::Elements(size as u64));

        // Fit checks reject immediately, while pending suffixes accumulate on
        // the same line. No weak whitespace is needed to exercise this path.
        let arena = Arena::new();
        let entry = arena.line_suffix("//") + arena.text("long").partial_union(arena.text("x"));
        let doc = arena.concat((0..size).map(|_| entry.clone()));
        group.bench_with_input(
            BenchmarkId::new("pending_suffixes", size),
            &doc,
            |b, doc| {
                b.iter(|| doc.render(0, &mut io::sink()).unwrap());
            },
        );

        // Full unions can accumulate deferred suffixes without inspecting
        // their content at each branch.
        let arena = Arena::new();
        let entry = arena.line_suffix("//").union(arena.nil());
        let doc = arena.concat((0..size).map(|_| entry.clone()));
        group.bench_with_input(BenchmarkId::new("union_suffixes", size), &doc, |b, doc| {
            b.iter(|| doc.render(size * 2, &mut io::sink()).unwrap());
        });
    }
    group.finish();
}

criterion_group!(benches, line_suffixes);
criterion_main!(benches);
