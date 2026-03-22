Patoline - A modern digital typesetting system
==============================================

This repository contains the source code of Patoline, which homepage is hosted
at https://patoline.github.io.

### Dependencies

The minimal dependencies are
 - OCaml (4.14.2 or newer)
 - Opam (2.5.0 or higher recommended)
 - Dune (2.7.0 or higher)
 - Ocamlfind
 - Earley (3.0.0)
 - stdlib-shims
 - Camlzip
 - Sqlite3
 - Imagelib
 - GNU make

To setup a suitable OCaml environment, the simplest possible solution is to
use the Opam package manager. It can be installed by following instructions
at http://opam.ocaml.org/doc/Install.html. You can then install an OCaml
compiler and the required libraries as follows.

```bash
opam switch create 4.14.2
eval $(opam env)
opam install dune earley.3.0.0 stdlib-shims camlzip sqlite3 imagelib
```

**Note (earley 3.0):** Upstream earley 3.0.0 removed the `earley.ocaml` library from Opam; this tree ships the equivalent in `patoline_ocaml/` (using `compiler-libs.common`). Earley `parser` syntax in `patoline_ocaml/`, `pa_patoline/`, `unicodelib/`, `cesure/`, and `patobuild/` is checked in **already expanded** to plain OCaml, so **`dune build` does not invoke the `pa_ocaml` preprocessor**. To edit the original BNF-style sources, install earley and run `pa_ocaml` on the file, then replace the corresponding `.ml`.

You can optionally install more Opam packages (OCaml libraries) depending
on the Patoline drivers that you want to use. For instance, you will need
to install ``lablgl`` for our OpenGL driver, ``cairo2`` for our Cairo
driver, ``cryptokit`` for our ``Patonet`` driver.

### Compilation from source

Patoline can be compiled from source as follows:

```bash
wget https://github.com/mobtgzhang/patoline/archive/master.zip
unzip master.zip
cd patoline-master
make
make install
```

### First Patoline document

Here is a hello world patoline document (other examples can be found in the
`examples` folder).

```
======================
Hello, World!
----------------------
Patoline newbies
======================

-> Section with text

This is my //first// document! That's so **cool**!

-<

-> Section with maths

Did you known that $(a + b)² = a² + 2ab + b²$?

-<
```
If you copy-paste it in a file ``hello_world.txp``, then you can compile it
with the command ``patoline hello_world.txp``. This will produce a PDF file
called ``hello_world.pdf``. Note that you can compile to other format drivers
such as a webpage using SVG format or an OpenGL window. To do that, select
a driver with the command ``patoline --driver DRIVER hello_world.txp``. You
can obtain the list of the existing drivers with ``patoline drivers`` (they
may not all be installed).

### List of developpers and contributors

Main developpers and contributors:
 - Pierre-Étienne Meunier
 - Christophe Raffalli
 - Rodolphe Lepigre
 - Tom Hirschowitz
 - Florian Hatat
 - Pierre Hyvernat
 - Guillaume Theyssier
 - Vincent Laporte
 - Alan Schmitt
