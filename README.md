# LMNtalc

[LMNtal](https://www.ueda.info.waseda.ac.jp/lmntal/index.php) compiler written in Rust (WIP).

## Workspace

This repository is a Rust workspace with the following crates:

- `lmntalc`
  The supported compiler library and CLI surface.
- `lmntalc-core`
  Internal backend-neutral compiler stages and shared diagnostics.
- `lmntalc-ide`
  Internal editor-analysis layer used by the embedded language server.
- `lmntal-language-server`
  The supported LMNtal language-server surface, maintained in this repository under `crates/lmntal-language-server`.

## Installation

### Install from crates.io

```sh
cargo install lmntalc
```

To install the language server from this workspace:

```sh
cargo install --path crates/lmntal-language-server
```

### Install from source

```sh
git clone https://github.com/lmntal/lmntalc.git
cd lmntalc
cargo install --path .
```

To build or run the language server from source:

```sh
cargo run -p lmntal-language-server
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
