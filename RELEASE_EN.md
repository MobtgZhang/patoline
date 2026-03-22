# Patoline — Release notes

This release targets modern OCaml, Earley 3.0, and Dune. Syntax-extension code is integrated so that **the build does not invoke the `pa_ocaml` executable**, and several user-visible issues when compiling `.txp` files are addressed.

---

## Build and toolchain

- New **`patoline_ocaml/`** directory: OCaml syntax extension code that shipped with Earley 3.0’s `pa_ocaml`, checked in as expanded plain OCaml. Dependencies include `earley.core`, `earley.str`, and `compiler-libs.common`. Main sources include `pa_ast.ml`, `pa_lexing.ml`, `pa_main.ml`, `pa_ocaml.ml`, `pa_ocaml_prelude.ml`, `pa_parser.ml`, etc.
- **`dune-project` / `patoline.opam`**: Aligned with Dune-generated opam metadata and declared project dependencies.
- **`cesure/`**, **`patobuild/`**, **`unicodelib/`**: Sources are Earley “parser” syntax expanded to OCaml; respective `dune` files no longer use `(run pa_ocaml …)`, so the build does not depend on `pa_ocaml` preprocessing.

---

## pa_patoline

- **`pa_patoline/dune`**: Depends on `patoline_ocaml`; `(wrapped false)` on `pa_patoline_support` so `open Pa_patoline_ast_helpers` works; executable modules include `pa_patoline`, `prefixTree`, `Subsup`, etc., so PrefixTree / Subsup resolve at link time.
- **`pa_patoline.ml`**, **`Subsup.ml`**: Expanded syntax; remaining `<:struct<…>>` / `<:expr<…>>` spans are replaced with explicit `Ast_helper` / `Pa_ast` construction, with large pieces delegated to helpers.
- **`pa_patoline_ast_helpers.ml`**: Central place for Parsetree fragments (include-like modules, macro blocks, display math via `newPar` + `displayedFormula`, headings `newStruct` / `go_up`, `write_main` and `wrap_document`, etc.). **maths_invisible** uses `Maths.node (Maths.glyphs "invisible")` inside `Maths.Normal` to satisfy `Typography.Maths` `binary_type` expectations.
- **Title rule**: Returns `[ str_let_any … ]` (structure) so types match `wrap` / `full_text`.
- **print_math_deco**: `print_math_deco_finish` is not wrapped in an extra `Pa_ast.exp_list`, avoiding `[[Maths.Ordinary …]]` and broken builds for real documents (e.g. `examples/hello_world.txp`).

---

## Build system and optional dependencies

- **`typography/dune`**: `test_offset.ml` is excluded from the default build because it needs the `Graphics` module, which is not always installed.
- **`grammars/dune`**: The `pa_patoline` invocation that generates `DefaultGrammar.ml` no longer passes `--ascii`, which the standalone driver does not support (that flag belongs to the higher-level patobuild path).

---

## Giac, drivers, and other fixes

- **`packages/Giac.ml`**: `Pow`, `Inv`, `Opp`, `Ind` implemented with plain OCaml and `Maths` constructors to avoid malformed structures from the `(<$ … $>)` expander on superscripts.
- **`patoraw/DynDriver.ml`**, **`drivers/DriverGL/DriverGL.ml`**: Maintenance fixes; DriverGL uses `Stdlib.stderr` instead of `Pervasives.stderr` for OCaml 4.08+ (avoids build failures when deprecated alerts are errors).

---

## Documentation

- **`README.md`**: Describes `patoline_ocaml` and how to maintain this tree without a build-time dependency on `pa_ocaml`.

---

## CJK fonts and examples (catalog for this batch)

- **`patfonts/Opentype/Opentype.ml`**: Recognize `.ttc` extensions; for `ttcf` (TrueType collection) fonts, load the **first face** in the collection (same idea as many system renderers).
- **`typography/ConfigFindFont.legacy.ml`**: Resolve **WenQuanYi Zen Hei** via common Linux paths to `wqy-zenhei.ttc`; if `pat_to_name` returns an **absolute path** and the file exists, use it directly for font lookup.
- **`formats/DefaultFormat.ml`**: Default body font family prefers **WenQuanYi Zen Hei** when available, otherwise falls back to Alegreya with a stderr hint (install `fonts-wqy-zenhei` or use `--extra-fonts-dir`); documents why some Noto CJK fonts are not the default (CFF vs `CFF.index`).
- **`examples/bilingual_zh_en.txp`**: Chinese / English / Korean UTF-8 sample (no math) to validate CJK and Hangul rendering.
- **`examples/README.md`**: How to build the sample, default font behavior, and **git development** notes (`dune install` / `opam install .`, `OCAMLPATH`) so local `DefaultFormat` changes apply.
- **`examples/dune`**: Install rule maps `bilingual_zh_en.txp` under `share`.
- **`RELEASE.md` / `RELEASE_EN.md`**: Release notes and this catalog (Chinese / English).

---

## Upgrade notes

- Rebuild and install `pa_patoline` (or the full patoline / patobuild toolchain) so the updated preprocessor is on `PATH`.
- Remove stale build directories (e.g. `examples/.patobuild`) and re-run patoline on `.txp` files to regenerate `.ml` sources.

For the GitHub Release page, add **version**, **date**, **tested OCaml / opam sets**, and any **divergence from upstream Patoline** as needed.

---

## Main paths touched (reference)

`patoline_ocaml/` · `dune-project` · `patoline.opam` · `cesure/` · `patobuild/` · `unicodelib/` · `pa_patoline/` · `grammars/` · `typography/` · `packages/Giac.ml` · `patoraw/DynDriver.ml` · `drivers/DriverGL/DriverGL.ml` · `README.md` · `patfonts/Opentype/Opentype.ml` · `typography/ConfigFindFont.legacy.ml` · `formats/DefaultFormat.ml` · `examples/bilingual_zh_en.txp` · `examples/README.md` · `examples/dune` · `RELEASE.md` · `RELEASE_EN.md`
