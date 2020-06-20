# Rask

#### A Scheme interpreter

[![](https://img.shields.io/badge/github-Vanille--N/rask-8da0cb?logo=github)](https://github.com/Vanille-N/rask)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![codecov](https://codecov.io/gh/Vanille-N/rask/branch/master/graph/badge.svg)](https://codecov.io/gh/Vanille-N/rask)

`rask` [![crates.io](http://meritbadge.herokuapp.com/rask)](https://crates.io/crates/rask)
[![API](https://docs.rs/rask/badge.svg)](https://docs.rs/rask)

`chainmap` [![](http://meritbadge.herokuapp.com/chainmap)](https://crates.io/crates/chainmap)
[![API](https://docs.rs/chainmap/badge.svg)](https://docs.rs/chainmap) (dependency)


---
> **rask** (Norwegian) *{adj.}*: **fast**, **swift**, **nimble**<br>Rust + Racket → Rask

**I could've gone for 'rasket', but as that one means 'trash'/'heavy' depending on the language, I decided against it.*

---

Rask is under development.

#### 0.1 roadmap:
- [X] **Split** (text → symbols)
- [X] **Lex** (symbols → tokens)
- [X] **Parse** (tokens → expression tree)
- [X] **Eval** (expression tree → expression)
- [X] **Cov** (decent test coverage)

Test coverage is now considered sufficient, 0.1 has had its last commit. Development will resume with 0.2, shortly after the 11th of July.

#### 0.2 roadmap:
- [ ] **Def** (defines and variable bindings)
- [ ] **Boot** (basic constructs: if, let, letrec, let*, ...)
- [ ] **REPL** (interactive [text → expression] loop)
- [ ] **Disp** (pretty-print expressions)
- [ ] **Err** (better error messages)

From 0.3 onwards, a standard library will be built.
