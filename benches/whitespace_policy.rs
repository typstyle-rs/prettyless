//! Compares literal and weak spaces under eager and deferred indentation,
//! with and without blank lines. Padding sweeps isolate commitment, discard
//! at a break and at EOF, suppression at line start, suffix commitment, and
//! group fitting that must ignore trailing weak spaces.

use criterion::{
    BenchmarkGroup, BenchmarkId, Criterion, Throughput, criterion_group, criterion_main,
    measurement::WallTime,
};
use prettyless::{Arena, DocAllocator, DocBuilder, IndentationPolicy, RenderOptions};
use std::hint::black_box;

#[derive(Clone, Copy)]
enum Whitespace {
    Literal,
    Committed,
    DiscardedAtBreak,
    DiscardedAtEof,
    Leading,
    SuffixCommitted,
    GroupFits,
}

#[derive(Clone, Copy)]
struct Case {
    whitespace: Whitespace,
    blank: bool,
    padding: usize,
    items: usize,
}

impl Case {
    fn build<'a>(self, arena: &'a Arena<'a>) -> DocBuilder<'a, Arena<'a>> {
        let mut doc = arena.nil();
        for item in 0..self.items {
            let weak = if matches!(self.whitespace, Whitespace::Literal) {
                arena.nil()
            } else {
                arena.weak_space().repeat(self.padding)
            };
            let entry = match self.whitespace {
                Whitespace::Literal => {
                    arena.text("a") + arena.space().repeat(self.padding) + arena.text("b")
                }
                Whitespace::Committed => arena.text("a") + weak + arena.text("b"),
                Whitespace::DiscardedAtBreak | Whitespace::DiscardedAtEof => arena.text("a") + weak,
                Whitespace::Leading => weak + arena.text("a"),
                Whitespace::SuffixCommitted => arena.text("a") + weak + arena.line_suffix("//"),
                Whitespace::GroupFits => {
                    (arena.text("a") + arena.line() + arena.text("b") + weak).group()
                }
            };
            doc += entry;
            if !matches!(self.whitespace, Whitespace::DiscardedAtEof) || item + 1 < self.items {
                doc += arena.hard_line();
                if self.blank {
                    doc += arena.hard_line();
                }
            }
        }
        doc.nest(4)
    }

    // Independent exact-output oracle, including blank and terminal indentation.
    fn expected(self, policy: IndentationPolicy) -> String {
        let content = match self.whitespace {
            Whitespace::Literal | Whitespace::Committed => {
                format!("a{}b", " ".repeat(self.padding))
            }
            Whitespace::DiscardedAtBreak | Whitespace::DiscardedAtEof | Whitespace::Leading => {
                "a".to_owned()
            }
            Whitespace::SuffixCommitted => format!("a{}//", " ".repeat(self.padding)),
            Whitespace::GroupFits => "a b".to_owned(),
        };
        let mut out = String::new();
        for item in 0..self.items {
            if item > 0 {
                out.push_str("    ");
            }
            out.push_str(&content);
            if !matches!(self.whitespace, Whitespace::DiscardedAtEof) || item + 1 < self.items {
                out.push('\n');
                if self.blank {
                    if policy == IndentationPolicy::Eager {
                        out.push_str("    ");
                    }
                    out.push('\n');
                }
            }
        }
        if policy == IndentationPolicy::Eager
            && !matches!(self.whitespace, Whitespace::DiscardedAtEof)
        {
            out.push_str("    ");
        }
        out
    }
}

fn bench_case(group: &mut BenchmarkGroup<'_, WallTime>, case: Case, parameter: usize) {
    let arena = Arena::new();
    let doc = case.build(&arena);
    group.throughput(Throughput::Elements(case.items as u64));
    for (policy_name, policy) in [
        ("eager", IndentationPolicy::Eager),
        ("deferred", IndentationPolicy::Deferred),
    ] {
        let width = if matches!(case.whitespace, Whitespace::GroupFits) {
            7
        } else {
            80
        };
        let options = RenderOptions::new(width).with_indentation_policy(policy);
        let expected = case.expected(policy);
        let mut actual = String::new();
        doc.render_fmt_with(options, &mut actual).unwrap();
        assert_eq!(
            actual, expected,
            "{policy_name} items={} padding={}",
            case.items, case.padding
        );
        group.bench_function(BenchmarkId::new(policy_name, parameter), |b| {
            let mut out = String::with_capacity(expected.len());
            b.iter(|| {
                out.clear();
                black_box(&doc)
                    .render_fmt_with(black_box(options), &mut out)
                    .unwrap();
                black_box(&out);
            });
        });
    }
    // Construction is independent of rendering policy, so time it only once.
    group.bench_function(BenchmarkId::new("construct", parameter), |b| {
        b.iter(|| {
            let arena = Arena::new();
            black_box(black_box(case).build(&arena));
        });
    });
}

fn whitespace_policy(c: &mut Criterion) {
    for (name, whitespace, blank) in [
        ("literal", Whitespace::Literal, false),
        ("weak", Whitespace::Committed, false),
        ("blank_literal", Whitespace::Literal, true),
        ("blank_weak", Whitespace::Committed, true),
    ] {
        let mut group = c.benchmark_group(format!("whitespace_policy/{name}"));
        for items in [16, 256, 4_096] {
            bench_case(
                &mut group,
                Case {
                    whitespace,
                    blank,
                    padding: 1,
                    items,
                },
                items,
            );
        }
        group.finish();
    }
    // Fixed count, varying accumulated padding. Separate discard from commitment
    // so extra output copying does not obscure discard cost.
    for (name, whitespace, items) in [
        ("committed", Whitespace::Committed, 64),
        ("discarded_break", Whitespace::DiscardedAtBreak, 64),
        // No hard lines: isolate EOF discard from break handling.
        ("discarded_eof", Whitespace::DiscardedAtEof, 1),
        ("discarded_eof_multiline", Whitespace::DiscardedAtEof, 64),
        ("leading", Whitespace::Leading, 64),
        ("suffix_committed", Whitespace::SuffixCommitted, 64),
        ("group_fits_trailing", Whitespace::GroupFits, 64),
    ] {
        let mut group = c.benchmark_group(format!("whitespace_policy/{name}_padding"));
        for padding in [1, 16, 256] {
            bench_case(
                &mut group,
                Case {
                    whitespace,
                    blank: false,
                    padding,
                    items,
                },
                padding,
            );
        }
        group.finish();
    }
    nested_unions(c);
}
fn build_nested_union<'a>(
    arena: &'a Arena<'a>,
    tail: usize,
    accepts: bool,
) -> DocBuilder<'a, Arena<'a>> {
    let mut doc = arena.nil();
    for _ in 0..64 {
        let mut left = arena.nil();
        for _ in 0..tail {
            // Each successful inner union flushes suffixes into the outer
            // speculative buffer. Acceptance makes these effects observable;
            // rejection must restore the caller's suffix and weak padding.
            left += (arena.line_suffix("I") + arena.text("a") + arena.hard_line())
                .union(arena.text("a") + arena.hard_line());
        }
        doc += arena.text("p")
            + arena.weak_space()
            + arena.line_suffix("O")
            + (left + arena.text(if accepts { "z" } else { "far too long!!" }))
                .union(arena.text("x"))
            + arena.hard_line();
    }
    doc.nest(4)
}

fn nested_union_output(tail: usize, accepts: bool, policy: IndentationPolicy) -> String {
    let mut out = String::new();
    for item in 0..64 {
        if item > 0 {
            out.push_str("    ");
        }
        if !accepts {
            out.push_str("p xO\n");
        } else if tail == 0 {
            out.push_str("p zO\n");
        } else {
            out.push_str("p aOI\n");
            out.push_str(&"    aI\n".repeat(tail - 1));
            out.push_str("    z\n");
        }
    }
    if policy == IndentationPolicy::Eager {
        out.push_str("    ");
    }
    out
}

fn nested_unions(c: &mut Criterion) {
    for (group_name, accepts) in [
        ("nested_accepts_tail", true),
        ("nested_rollback_tail", false),
    ] {
        let mut group = c.benchmark_group(format!("whitespace_policy/{group_name}"));
        group.throughput(Throughput::Elements(64));
        for tail in [0, 16, 256] {
            let arena = Arena::new();
            let doc = build_nested_union(&arena, tail, accepts);
            for (name, policy) in [
                ("eager", IndentationPolicy::Eager),
                ("deferred", IndentationPolicy::Deferred),
            ] {
                let options = RenderOptions::new(12).with_indentation_policy(policy);
                let expected = nested_union_output(tail, accepts, policy);
                let mut actual = String::new();
                doc.render_fmt_with(options, &mut actual).unwrap();
                assert_eq!(actual, expected, "{group_name}/{tail}/{name}");
                group.bench_function(BenchmarkId::new(name, tail), |b| {
                    let mut out = String::with_capacity(expected.len());
                    b.iter(|| {
                        out.clear();
                        black_box(&doc)
                            .render_fmt_with(black_box(options), &mut out)
                            .unwrap();
                        black_box(&out);
                    });
                });
            }
            group.bench_function(BenchmarkId::new("construct", tail), |b| {
                b.iter(|| {
                    let arena = Arena::new();
                    black_box(build_nested_union(
                        &arena,
                        black_box(tail),
                        black_box(accepts),
                    ));
                });
            });
        }
        group.finish();
    }
}

criterion_group!(benches, whitespace_policy);
criterion_main!(benches);
