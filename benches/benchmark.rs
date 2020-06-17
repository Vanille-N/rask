use core::time::Duration;
use criterion::{criterion_group, criterion_main, Criterion, Throughput};

use rask::parse::{build, lex, split};
use rask::source;

const ASSETS: [&str; 9] = [
    "sort",
    "set-construct",
    "word-count",
    "printer",
    "interprete",
    "unification",
    "timer",
    "sprintf",
    "matrix",
];

fn criterion_split(c: &mut Criterion) {
    let crit = std::mem::replace(&mut *c, Criterion::default())
        .measurement_time(Duration::new(3, 0))
        .warm_up_time(Duration::new(1, 0));
    std::mem::replace(&mut *c, crit);

    let mut group = c.benchmark_group("Split");
    for file in ASSETS.iter() {
        let prog = source(&("assets/".to_owned() + *file)).unwrap();
        let size = prog.len() as u64;
        group.throughput(Throughput::Bytes(size));
        group.bench_with_input(*file, &size, |b, _| {
            b.iter(|| split(&prog[..]))
        });
    }
    group.finish();
}

fn criterion_lex(c: &mut Criterion) {
    let crit = std::mem::replace(&mut *c, Criterion::default())
        .measurement_time(Duration::new(3, 0))
        .warm_up_time(Duration::new(1, 0));
    std::mem::replace(&mut *c, crit);

    let mut group = c.benchmark_group("Lex");
    for file in ASSETS.iter() {
        let prog = source(&("assets/".to_owned() + *file)).unwrap();
        let symbols = split(&prog[..]).ok().unwrap();
        let size = symbols.len() as u64;
        group.throughput(Throughput::Bytes(size));
        group.bench_with_input(*file, &size, |b, _| {
            b.iter(|| lex(&symbols))
        });
    }
    group.finish();
}

fn criterion_build(c: &mut Criterion) {
    let crit = std::mem::replace(&mut *c, Criterion::default())
        .measurement_time(Duration::new(3, 0))
        .warm_up_time(Duration::new(1, 0));
    std::mem::replace(&mut *c, crit);

    let mut group = c.benchmark_group("Build");
    for file in ASSETS.iter() {
        let prog = source(&("assets/".to_owned() + *file)).unwrap();
        let symbols = split(&prog[..]).ok().unwrap();
        let tokens = lex(&symbols).ok().unwrap();
        let size = tokens.len() as u64;
        group.throughput(Throughput::Bytes(size));
        group.bench_with_input(*file, &size, |b, _| {
            b.iter(|| build(&tokens))
        });
    }
    group.finish();
}

criterion_group!(benches, criterion_split, criterion_lex, criterion_build);
criterion_main!(benches);
