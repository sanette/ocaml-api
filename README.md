# ocaml-api

Proposition for modernizing the OCaml API from the official manual

Try it out [here](https://sanette.github.io/ocaml-api/)


The present documentation is copyright Institut National de Recherche
en Informatique et en Automatique (INRIA). A complete version can be
obtained from
[this page](http://caml.inria.fr/pub/docs/manual-ocaml/).

# running the script

1. Download or clone the repository

And `cd` in the `ocaml-api` directory.

2. Download the OCaml manual:

```bash
cd /tmp
wget http://caml.inria.fr/distrib/ocaml-4.09/ocaml-4.09-refman-html.tar.gz
```

and move the `libref` directory into the `ocaml-api` directory of Step 1.

3. make

In the `ocaml-api` directory, run `make`.  This will run the
`process.ml` script, which populates the `docs` directory, and copy
the css and js files.

Note: the `index.js` file can be re-created from scratch (see the
`process.ml` file), but it's a bit long (15sec on my laptop).

4. Browse!

`firefox docs/index.html`
