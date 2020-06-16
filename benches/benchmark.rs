use core::time::Duration;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

use rask::parse::{split, lex, build};
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

    let mut group_split = c.benchmark_group("split");
    for file in ASSETS.iter() {
        let prog = source(&("assets/".to_owned() + *file)).unwrap();
        group_split.bench_with_input(*file, *file, |b, _| b.iter(|| split(black_box(&prog[..]))));
    }
    group_split.finish();
}

fn criterion_lex(c: &mut Criterion) {
    let crit = std::mem::replace(&mut *c, Criterion::default())
        .measurement_time(Duration::new(3, 0))
        .warm_up_time(Duration::new(1, 0));
    std::mem::replace(&mut *c, crit);

    let mut group_lex = c.benchmark_group("lex");
    for file in ASSETS.iter() {
        let prog = source(&("assets/".to_owned() + *file)).unwrap();
        let symbols = split(&prog[..]).ok().unwrap();
        group_lex.bench_with_input(*file, *file, |b, _| {
            b.iter(|| lex(black_box(symbols.clone())))
        });
    }
    group_lex.finish();
}

fn criterion_build(c: &mut Criterion) {
    let crit = std::mem::replace(&mut *c, Criterion::default())
        .measurement_time(Duration::new(3, 0))
        .warm_up_time(Duration::new(1, 0));
    std::mem::replace(&mut *c, crit);

    let mut group_build = c.benchmark_group("build");
    for file in ASSETS.iter() {
        let prog = source(&("assets/".to_owned() + *file)).unwrap();
        let symbols = split(&prog[..]).ok().unwrap();
        let tokens = lex(symbols).ok().unwrap();
        group_build.bench_with_input(*file, *file, |b, _| {
            b.iter(|| build(black_box(tokens.clone())))
        });
    }
    group_build.finish();
}

criterion_group!(benches, criterion_build);
criterion_main!(benches);
