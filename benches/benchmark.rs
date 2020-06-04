use criterion::{black_box, criterion_group, criterion_main, Criterion};

use rask::parse::split;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("split", |b| b.iter(|| split(black_box("(abc de (f #\\\\) #\\\") (gh #\\) (#\\i ())"))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
