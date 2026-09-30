use std::io;

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use prettyless::{Arena, combinators, prelude::*};

fn weak_whitespace(c: &mut Criterion) {
    let mut group = c.benchmark_group("weak_whitespace");
    for size in [1_000, 2_000, 4_000, 8_000, 16_000] {
        group.throughput(Throughput::Elements(size as u64));

        // A nonempty suffix makes an otherwise fresh weak line break.
        let arena = Arena::new();

        let doc = arena.pretty(repeat(size)((
            combinators::group((line_suffix("//"), weak_line(), "x")),
            hard_line(),
        )));
        group.bench_with_input(
            BenchmarkId::new("weak_line_suffixes", size),
            &doc,
            |b, doc| {
                b.iter(|| doc.render(80, &mut io::sink()).unwrap());
            },
        );

        // Accumulated weak spaces become output only when text commits them.
        let arena = Arena::new();
        let doc = arena.pretty(combinators::group(("a", repeat(size)(weak_space()), "b")));
        group.bench_with_input(
            BenchmarkId::new("weak_spaces_committed", size),
            &doc,
            |b, doc| b.iter(|| doc.render(size + 2, &mut io::sink()).unwrap()),
        );

        // Fitting and rendering both discard trailing padding at EOF or a break.
        for at_break in [false, true] {
            let arena = Arena::new();
            let doc = (arena.text("a") + arena.weak_space().repeat(size)).group();
            let doc = if at_break {
                doc + arena.hard_line()
            } else {
                doc
            };
            let name = if at_break {
                "weak_spaces_discarded_at_break"
            } else {
                "weak_spaces_discarded_at_eof"
            };
            group.bench_with_input(BenchmarkId::new(name, size), &doc, |b, doc| {
                b.iter(|| doc.render(1, &mut io::sink()).unwrap());
            });
        }
    }
    group.finish();
}

criterion_group!(benches, weak_whitespace);
criterion_main!(benches);
