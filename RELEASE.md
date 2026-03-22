# Patoline 发布说明

本版本面向现代 OCaml、Earley 3.0 与 Dune，在**不于构建期调用 `pa_ocaml` 可执行文件**的前提下完成语法扩展相关代码的整合，并修复编译 `.txp` 时的若干用户可见问题。

---

## 构建与工具链

- 新增 **`patoline_ocaml/`**：收纳原 Earley 3.0 随 `pa_ocaml` 分发的 OCaml 语法扩展实现，已预先展开为普通 OCaml；依赖包括 `earley.core`、`earley.str`、`compiler-libs.common`。主要文件如 `pa_ast.ml`、`pa_lexing.ml`、`pa_main.ml`、`pa_ocaml.ml`、`pa_ocaml_prelude.ml`、`pa_parser.ml` 等。
- **`dune-project` / `patoline.opam`**：与 Dune 生成的 opam 元数据及项目依赖声明保持一致。
- **`cesure/`、`patobuild/`、`unicodelib/`**：对应源码为 Earley「parser」语法展开后的结果；各模块 `dune` 中已去掉 `(run pa_ocaml …)`，构建不再依赖 `pa_ocaml` 预处理。

---

## pa_patoline

- **`pa_patoline/dune`**：依赖 `patoline_ocaml`；对 `pa_patoline_support` 使用 `(wrapped false)`，便于 `open Pa_patoline_ast_helpers`；可执行模块包含 `pa_patoline`、`prefixTree`、`Subsup` 等，保证 PrefixTree / Subsup 在链接期可解析。
- **`pa_patoline.ml`、`Subsup.ml`**：展开后的语法；大块由显式 `Ast_helper` / `Pa_ast` 构造替代遗留的 `<:struct<…>>` / `<:expr<…>>`，并委托给辅助模块。
- **`pa_patoline_ast_helpers.ml`**：集中存放 Parsetree 片段（include 类模块、宏环境、行间公式 `newPar` + `displayedFormula`、章节 `newStruct` / `go_up`、`write_main` 与 `wrap_document` 等）。**maths_invisible** 使用 `Maths.node (Maths.glyphs "invisible")` 置于 `Maths.Normal` 内，以符合 `Typography.Maths` 对 `binary_type` 的期望。
- **标题规则**：相关规则返回 `[ str_let_any … ]`（结构体），以匹配 `wrap` / `full_text` 的类型。
- **print_math_deco**：不再将 `print_math_deco_finish` 包在多余的 `Pa_ast.exp_list` 中，避免生成 `[[Maths.Ordinary …]]` 并破坏真实文档（如 `examples/hello_world.txp`）的编译。

---

## 构建系统与可选依赖

- **`typography/dune`**：将 `test_offset.ml` 从默认构建中排除，因该文件依赖并非总是安装的 `Graphics` 模块，避免拖垮整库构建。
- **`grammars/dune`**：生成 `DefaultGrammar.ml` 时不再向 `pa_patoline` 传入独立驱动不支持的 `--ascii` 标志（该标志属于更高层的 patobuild 路径）。

---

## Giac、驱动与其它修复

- **`packages/Giac.ml`**：`Pow`、`Inv`、`Opp`、`Ind` 等分支改为普通 OCaml 与 `Maths` 构造子实现，避免 `(<$ … $>)` 展开在上标等处产生畸形嵌套结构。
- **`patoraw/DynDriver.ml`**、**`drivers/DriverGL/DriverGL.ml`**：相关维护性修复；其中 DriverGL 将 `Pervasives.stderr` 改为 `Stdlib.stderr`，以兼容 OCaml 4.08+ 对 `Pervasives` 的弃用（在将弃用视为错误时可避免构建失败）。

---

## 文档

- **`README.md`**：说明 `patoline_ocaml` 的角色，以及如何在「构建不依赖 `pa_ocaml`」的前提下维护本仓库。

---

## CJK 字体与示例（本批次变更目录）

- **`patfonts/Opentype/Opentype.ml`**：将 `.ttc` 纳入可识别扩展名；对 `ttcf`（TrueType collection）从集合内 **第一个子字体** 偏移加载，与常见系统渲染行为一致。
- **`typography/ConfigFindFont.legacy.ml`**：为 **WenQuanYi Zen Hei** 增加常见 Linux 路径下的 `wqy-zenhei.ttc` 解析；若 `pat_to_name` 返回 **绝对路径** 且文件存在，则直接使用该路径查找字体。
- **`formats/DefaultFormat.ml`**：正文默认字体族在可用时选 **文泉驿正黑**（`WenQuanYi Zen Hei`），否则回退 Alegreya 并在 stderr 提示安装或 `--extra-fonts-dir`；说明不默认使用部分 Noto CJK（CFF 与 `CFF.index` 限制）。
- **`examples/bilingual_zh_en.txp`**：中英韩 UTF-8 混排示例（无数学公式），用于验证 CJK/韩文显示。
- **`examples/README.md`**：补充该示例的编译方式、默认字体与 **git 开发时** 需 `dune install` / `opam install .` 与 `OCAMLPATH`，使本地 `DefaultFormat` 生效。
- **`examples/dune`**：`install` 段增加 `bilingual_zh_en.txp` 的安装映射。
- **`RELEASE.md` / `RELEASE_EN.md`**：发布说明与本目录（中英文）。

---

## 升级建议

- 重新构建并安装 `pa_patoline`（或整套 patoline / patobuild 工具链），确保修复后的预处理器在 `PATH` 中。
- 清理旧构建目录（如 `examples/.patobuild`），对 `.txp` 重新运行 patoline 以重新生成 `.ml`。

发布页面可另行补充：**版本号**、**发布日期**、**测试通过的 OCaml / opam 组合**，以及与上游 Patoline 的差异说明。

---

## 主要涉及路径（参考）

`patoline_ocaml/` · `dune-project` · `patoline.opam` · `cesure/` · `patobuild/` · `unicodelib/` · `pa_patoline/` · `grammars/` · `typography/` · `packages/Giac.ml` · `patoraw/DynDriver.ml` · `drivers/DriverGL/DriverGL.ml` · `README.md` · `patfonts/Opentype/Opentype.ml` · `typography/ConfigFindFont.legacy.ml` · `formats/DefaultFormat.ml` · `examples/bilingual_zh_en.txp` · `examples/README.md` · `examples/dune` · `RELEASE.md` · `RELEASE_EN.md`
