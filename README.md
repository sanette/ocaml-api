# ocaml-api

Proposition for modernizing the OCaml API from the official manual

Try it out [here](https://sanette.github.io/ocaml-api/)


_The documentation shown on the web page that is linked above is
copyright Institut National de Recherche en Informatique et en
Automatique (INRIA). A complete version can be obtained from
[this page](http://caml.inria.fr/pub/docs/manual-ocaml/)._

# running the script

1. Install lambdasoup

```opam install lambdasoup```

2. Download or clone the repository

3. Download the OCaml manual:

```bash
cd /tmp
wget http://caml.inria.fr/distrib/ocaml-4.09/ocaml-4.09-refman-html.tar.gz
```

and move the `libref` directory (found in the `htmlman` dir) into the
`ocaml-api` directory of Step 2.

4. make

In the `ocaml-api` directory, run `make`.  This will run the
`process.ml` script, which populates the `docs` directory, and copy
the css, js and svg files.

Note: the `index.js` file can be re-created from scratch (see the
`process.ml` file), but it's a bit long (15 sec on my laptop).

5. Browse!

`firefox docs/index.html`
