use criterion::{black_box, criterion_group, criterion_main, Criterion};

use rask::parse::{lex, split, PROG};

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("split", |b| b.iter(|| split(black_box(PROG))));
    c.bench_function("split_and_lex", |b| {
        b.iter(|| {
            let sp = split(black_box(PROG)).ok().unwrap();
            sp.into_iter().map(|s| lex(black_box(s))).collect::<Vec<_>>()
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
