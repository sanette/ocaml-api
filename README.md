# OCaml-API

Proposition for modernizing the OCaml API from the official manual

Try it out [here](https://sanette.github.io/ocaml-api/)


_The documentation shown on the web page that is linked above is
copyright Institut National de Recherche en Informatique et en
Automatique (INRIA). A complete version can be obtained from
[this page](http://caml.inria.fr/pub/docs/manual-ocaml/)._

# Running the script

1. Install lambdasoup

```opam install lambdasoup```

2. Download or clone the repository

3. make

In the `ocaml-api` directory, run `make`.  This will run the
`process.ml` script, which downloads the manuals, populates the `docs`
directory, and copy the css, js and svg files.

4. Browse!

`firefox docs/index.html`

# More options

Cleaning the `docs` directory:

```make clean```

The `src/index-??.js` files can be re-created from scratch:

```make index```

The behaviour of the script can be controlled by a few keywords:
`makeindex`, `overwrite`, `silent`, and `html`. (`html` is true by
default unless `makeindex` is present.)

```
dune exec src/process.exe                # only process html files
dune exec src/process.exe makeindex      # only create the index file
dune exec src/process.exe overwrite      # force overwriting existing html files
dune exec src/process.exe makeindex html # create index and process html files
dune exec src/process.exe [...] silent   # suppress console output
etc... (all keywords can be combined)
```
