local:
	mkdir -p docs
	dune exec src/process.exe
	cp src/index.html docs/
	cp src/search.js docs/
	cp src/scroll.js docs/
	cp src/colour-logo-gray.svg docs/
	cp src/search_icon.svg docs/

index:
	dune exec src/process.exe makeindex

clean:
	rm -rf docs/*

css:
	sass src/style.scss > docs/style.css

ocamlorg: css 
	./ocamlorg.sh

all: local ocamlorg
