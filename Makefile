local:
	mkdir -p docs/compilerlibref
	dune exec src/process.exe
	cp src/index.html docs/
	cp src/compilerlibref/index.html docs/compilerlibref/
	cp src/search.js docs/
	ln -f -s ../search.js docs/compilerlibref/
	cp src/scroll.js docs/
	ln -f -s ../scroll.js docs/compilerlibref/
	cp src/colour-logo-gray.svg docs/
	ln -f -s ../colour-logo-gray.svg docs/compilerlibref/
	cp src/search_icon.svg docs/
	ln -f -s ../search_icon.svg docs/compilerlibref/


index:
	dune exec src/process.exe makeindex

clean:
	rm -rf docs/*

css:
	sass src/style.scss > docs/style.css
	ln -f -s ../style.css docs/compilerlibref/

ocamlorg: css 
	./ocamlorg.sh

all: local ocamlorg
