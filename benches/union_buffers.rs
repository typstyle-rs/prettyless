use std::{hint::black_box, io};

use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use prettyless::{Arena, DocAllocator};

/// Measures nested [`prettyless::DocBuilder::union`] buffering with ordinary text.
///
/// Rendering to [`io::sink`] isolates layout and buffer costs. Prefixing each
/// parent exercises the copy path when it already contains output; empty
/// parents can take ownership of an accepted child's storage. Construction
/// and output validation happen outside the timed iterations.
fn union_buffers(c: &mut Criterion) {
    let source = "a".repeat(1_048_576);
    let width = source.len() + 33;
    let arena = Arena::new();
    let leaf = arena.text(source.as_str()) + arena.text("!");
    let mut group = c.benchmark_group("union_buffers");
    group.throughput(Throughput::Bytes((source.len() + 1) as u64));
    group.bench_function("no_unions", |b| {
        b.iter(|| black_box(&leaf).render(width, &mut io::sink()).unwrap());
    });

    for prefixed in [false, true] {
        let name = if prefixed {
            "prefixed_parent"
        } else {
            "empty_parent"
        };
        for depth in [1, 8, 32] {
            let mut doc = leaf.clone();
            for _ in 0..depth {
                doc = if prefixed {
                    (arena.text("p") + doc).union(arena.text("fallback"))
                } else {
                    doc.union(arena.text("fallback"))
                };
            }
            let expected = "p".repeat(if prefixed { depth } else { 0 }) + &source + "!";
            assert_eq!(doc.print(width).to_string(), expected);
            group.throughput(Throughput::Bytes(expected.len() as u64));
            group.bench_with_input(BenchmarkId::new(name, depth), &doc, |b, doc| {
                b.iter(|| black_box(doc).render(width, &mut io::sink()).unwrap());
            });
        }
    }
    group.finish();
}

criterion_group!(benches, union_buffers);
criterion_main!(benches);
