# LMNtal Language Server

A language server implementation for LMNtal.

This crate is maintained in the [`lmntalc`](https://github.com/lmntal/lmntalc) workspace under `crates/lmntal-language-server` and uses the internal `lmntalc-ide` analysis layer.

## Features

- Semantic highlighting
- Diagnostics from the shared `lmntalc` analysis pipeline
- Document symbols
- References and document highlights

## Installation

Install from this workspace:

```sh
cargo install --path crates/lmntal-language-server
```

## Usage

Run over standard I/O:

```sh
lmntal-language-server
```

Run over TCP for debugging:

```sh
lmntal-language-server --port 3000
```

## License

This software is released under the MIT License, see [LICENSE](LICENSE).
