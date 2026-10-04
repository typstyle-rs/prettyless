//! Regression probes for construction and rendering hot paths, written
//! against APIs shared with prettyless 0.3.0 so the file can be copied
//! unchanged into an old checkout for identical baseline tests.
//!
//! Workload families: text width measurement, group fit boundaries, nested
//! group depth, union speculation (accept, early and late rejection), a
//! formatter-like composition, and `contextual` callbacks.

#![allow(deprecated)] // Keep hardline for the published baseline.

use criterion::{
    BenchmarkGroup, BenchmarkId, Criterion, Throughput, criterion_group, criterion_main,
    measurement::WallTime,
};
use prettyless::{Arena, DocAllocator, DocBuilder};
use std::hint::black_box;

const SIZES: [usize; 3] = [16, 256, 4_096];

#[derive(Clone, Copy)]
enum Workload {
    Ascii,
    Unicode,
    WideUnicode,
    Group,
    Nested {
        depth: usize,
    },
    Union {
        outcome: UnionOutcome,
        tail: usize,
    },
    Mixed,
    #[cfg(feature = "contextual")]
    Column,
    #[cfg(feature = "contextual")]
    Nesting,
}

#[derive(Clone, Copy)]
enum UnionOutcome {
    Accept,
    RejectEarly,
    RejectLate,
}

fn nested<'a>(arena: &'a Arena<'a>, depth: usize) -> DocBuilder<'a, Arena<'a>> {
    let mut doc = arena.text("x");
    for _ in 0..depth {
        doc = (arena.text("(") + (arena.line_() + doc).nest(2) + arena.line_() + arena.text(")"))
            .group();
    }
    doc
}

impl Workload {
    fn build<'a>(self, arena: &'a Arena<'a>, items: usize) -> DocBuilder<'a, Arena<'a>> {
        let mut doc = arena.nil();
        for _ in 0..items {
            let entry = match self {
                Self::Ascii => arena.text("a") + arena.space() + arena.text("b"),
                Self::Unicode => arena.text("α") + arena.space() + arena.text("β"),
                Self::WideUnicode => arena.text("界") + arena.space() + arena.text("語"),
                Self::Group => {
                    (arena.text("abcdefgh") + arena.line() + arena.text("ijklmnop")).group()
                }
                Self::Nested { depth } => nested(arena, depth),
                Self::Union { outcome, tail } => match outcome {
                    UnionOutcome::RejectEarly => {
                        let left =
                            arena.text("abcdefgh ijklmnop") + arena.text(" tail").repeat(tail);
                        // Synthetic branch contents differ deliberately: fixed
                        // fallback output isolates discarded speculation cost.
                        left.union(arena.text("x"))
                    }
                    UnionOutcome::Accept | UnionOutcome::RejectLate => {
                        let mut left = arena.nil();
                        for _ in 0..tail {
                            let line = arena.text("a") + arena.hardline();
                            left += line;
                        }
                        if matches!(outcome, UnionOutcome::Accept) {
                            let right = (arena.text("a") + arena.hardline()).repeat(tail)
                                + arena.text("end")
                                + arena.hardline();
                            // Same content, distinct layout even with no tail:
                            // bypassing successful speculation must fail the oracle.
                            (left + arena.text("end")).union(right)
                        } else {
                            (left + arena.text("too long!!")).union(arena.text("x"))
                        }
                    }
                },
                Self::Mixed => {
                    let values = arena.intersperse(["α", "β", "γ"], arena.text(",") + arena.line());
                    let array = (arena.text("[")
                        + (arena.line_() + values).nest(2)
                        + arena.line_()
                        + arena.text("]"))
                    .group();
                    (arena.text("let value =") + (arena.line() + array).nest(2)).group()
                        + arena.hardline()
                        + arena.text("done")
                }
                #[cfg(feature = "contextual")]
                Self::Column => (arena.text("a")
                    + arena.line()
                    + arena.on_column(move |column| {
                        arena.text(if column == 2 { "b" } else { "c" }).into_doc()
                    }))
                .group(),
                #[cfg(feature = "contextual")]
                Self::Nesting => (arena.hardline()
                    + arena.on_nesting(move |nesting| arena.as_string(nesting).into_doc())
                    + arena.on_column(move |column| arena.as_string(column).into_doc()))
                .nest(4),
            };
            doc += entry + arena.hardline();
        }
        doc
    }
}

fn bench_case(
    group: &mut BenchmarkGroup<'_, WallTime>,
    workload: Workload,
    items: usize,
    width: usize,
    parameter: usize,
    expected: String,
) {
    let arena = Arena::new();
    let doc = workload.build(&arena, items);
    let mut actual = String::new();
    doc.render_fmt(width, &mut actual).unwrap();
    assert_eq!(
        actual, expected,
        "items={items} width={width} parameter={parameter}"
    );
    group.throughput(Throughput::Elements(items as u64));
    group.bench_function(BenchmarkId::new("construct", parameter), |b| {
        // Allocation and teardown are timed, with a fresh arena each iteration.
        b.iter(|| {
            let arena = Arena::new();
            black_box(workload.build(&arena, black_box(items)));
        });
    });
    group.bench_function(BenchmarkId::new("render", parameter), |b| {
        // Only caller output capacity is reused. Printer scratch allocation,
        // traversal, speculative buffers, and copying remain part of rendering.
        let mut out = String::with_capacity(expected.len());
        b.iter(|| {
            out.clear();
            black_box(&doc)
                .render_fmt(black_box(width), &mut out)
                .unwrap();
            black_box(&out);
        });
    });
}

fn nested_output(depth: usize, flat: bool) -> String {
    if flat {
        return format!("{}x{}\n", "(".repeat(depth), ")".repeat(depth));
    }
    let mut out = String::new();
    for level in 0..depth {
        out.push_str(&format!("{}(\n", " ".repeat(level * 2)));
    }
    out.push_str(&format!("{}x\n", " ".repeat(depth * 2)));
    for level in (0..depth).rev() {
        out.push_str(&format!("{})\n", " ".repeat(level * 2)));
    }
    out
}

fn hot_paths(c: &mut Criterion) {
    for (name, workload, width, line) in [
        ("ascii", Workload::Ascii, 80, "a b\n"),
        ("unicode", Workload::Unicode, 80, "α β\n"),
        ("wide_unicode", Workload::WideUnicode, 80, "界 語\n"),
        // Below, at, and above the 17-column flat layout boundary.
        ("group_breaks", Workload::Group, 16, "abcdefgh\nijklmnop\n"),
        ("group_boundary", Workload::Group, 17, "abcdefgh ijklmnop\n"),
        ("group_fits", Workload::Group, 18, "abcdefgh ijklmnop\n"),
        (
            "mixed_fits",
            Workload::Mixed,
            80,
            "let value = [α, β, γ]\ndone\n",
        ),
        (
            "mixed_breaks",
            Workload::Mixed,
            12,
            "let value =\n  [α, β, γ]\ndone\n",
        ),
    ] {
        let mut group = c.benchmark_group(format!("hot_paths/{name}"));
        for items in SIZES {
            bench_case(
                &mut group,
                workload,
                items,
                width,
                items,
                line.repeat(items),
            );
        }
        group.finish();
    }
    #[cfg(feature = "contextual")]
    for (name, workload, width, line) in [
        ("column_fits", Workload::Column, 80, "a b\n"),
        ("column_breaks", Workload::Column, 1, "a\nc\n"),
        ("nesting_callbacks", Workload::Nesting, 80, "\n    45\n"),
    ] {
        let mut group = c.benchmark_group(format!("hot_paths/{name}"));
        for items in SIZES {
            bench_case(
                &mut group,
                workload,
                items,
                width,
                items,
                line.repeat(items),
            );
        }
        group.finish();
    }
    // Vary depth independently of count, retaining all points in one Criterion
    // group so its summary contains the complete depth curve.
    for mode in ["fits", "breaks", "near_fits"] {
        let mut group = c.benchmark_group(format!("hot_paths/nested_{mode}"));
        for depth in [1, 8, 32] {
            let width = match mode {
                "fits" => depth * 2 + 1,
                "breaks" => 1,
                // Long failed probes at every depth, rather than immediate
                // rejection after indentation: exposes repeated fitting cost.
                _ => depth * 2,
            };
            bench_case(
                &mut group,
                Workload::Nested { depth },
                64,
                width,
                depth,
                nested_output(depth, mode == "fits").repeat(64),
            );
        }
        group.finish();
    }
    // Fixed count/width and constant fallback output isolate discarded work.
    // Nested rollback uses current semantics in whitespace_policy.
    // Append traversal may still visit and stack a tail before the first
    // text rejects, so early-rejection time need not be constant in tail
    // length.
    for (name, outcome) in [
        ("union_accepts", UnionOutcome::Accept),
        ("union_rejects_early", UnionOutcome::RejectEarly),
        ("union_rejects_late", UnionOutcome::RejectLate),
    ] {
        let mut group = c.benchmark_group(format!("hot_paths/{name}"));
        for tail in [0, 16, 256] {
            let line = if matches!(outcome, UnionOutcome::Accept) {
                format!("{}end\n", "a\n".repeat(tail))
            } else {
                "x\n".to_owned()
            };
            bench_case(
                &mut group,
                Workload::Union { outcome, tail },
                64,
                8,
                tail,
                line.repeat(64),
            );
        }
        group.finish();
    }
}

criterion_group!(benches, hot_paths);
criterion_main!(benches);
