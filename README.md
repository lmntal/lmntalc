# LMNtalc

[LMNtal](https://www.ueda.info.waseda.ac.jp/lmntal/index.php) compiler written in Rust (WIP).

## Installation

### Install from crates.io

```sh
cargo install lmntalc
```

### Install from source

```sh
git clone https://github.com/lmntal/lmntalc.git
cd lmntalc
cargo install --path .
```

## Features

- Full support of parsing HyperLMNtal.
- Simple static analysis on parse tree level.
- Support of compiling Flat HyperLMNtal to target languages.
  - Currently, C++, Java, and Python are supported.

## Behavioral Difference from [lmntal-compiler](https://github.com/lmntal/lmntal-compiler)

### Expression

`a + b` is the same with `'+'(a, b)` in lmntal-compiler,
but they are different in LMNtalc.

## Known Issues

- Rules in membranes in top level rule is not supported. **i.e.** Cannot generate membranes with rules.

## License

This software is released under the MIT License, see [LICENSE](LICENSE).