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

## Usage

```sh
lmntalc <input-file>
```

or

```sh
lmntalc -t <target-language> <input-file>
```

Then, the compiled code will be written to the file with the same name as the input file, but with the extension of the target language.
To specify the output file, use `-o` option.

```sh
lmntalc -t <target-language> -o <output-file> <input-file>
```

For more information, use `lmntalc --help`.

## Features

- Full support of parsing HyperLMNtal.
- Simple static analysis on parse tree level.
- Support of compiling Flat HyperLMNtal to target languages.

### Target Languages

- C++ 20
- Java 17
- Python 3.11

## Behavioral Difference from [lmntal-compiler](https://github.com/lmntal/lmntal-compiler)

### Expression

`a + b` is the same with `'+'(a, b)` in lmntal-compiler,
but they are different in LMNtalc.

## Known Issues

- Rules in membranes in top level rule is not supported. **i.e.** Cannot generate membranes with rules.

## License

This software is released under the MIT License, see [LICENSE](LICENSE).