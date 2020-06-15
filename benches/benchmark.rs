use core::time::Duration;
use criterion::{black_box, criterion_group, criterion_main, Criterion};

use rask::parse::{lex, source, split};

fn criterion_split(c: &mut Criterion) {
    let assets = [
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
    let crit = std::mem::replace(&mut *c, Criterion::default())
        .measurement_time(Duration::new(3, 0))
        .warm_up_time(Duration::new(1, 0));
    std::mem::replace(&mut *c, crit);

    let mut group_split = c.benchmark_group("split");
    for file in assets.iter() {
        let prog = source(&("assets/".to_owned() + *file)).unwrap();
        group_split.bench_with_input(*file, *file, |b, _| b.iter(|| split(black_box(&prog[..]))));
    }
    group_split.finish();
}

fn criterion_lex(c: &mut Criterion) {
    let assets = [
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

    let crit = std::mem::replace(&mut *c, Criterion::default())
        .measurement_time(Duration::new(3, 0))
        .warm_up_time(Duration::new(1, 0));
    std::mem::replace(&mut *c, crit);

    let mut group_lex = c.benchmark_group("lex");
    for file in assets.iter() {
        let prog = source(&("assets/".to_owned() + *file)).unwrap();
        let sp = split(&prog[..]);
        group_lex.bench_with_input(*file, *file, |b, _| {
            b.iter(|| sp.iter().map(|s| black_box(s)).collect::<Vec<_>>())
        });
    }
    group_lex.finish();
}

// criterion_group!(benches, criterion_split, criterion_lex);
criterion_main!(benches);
