all:
	dune exec src/process.exe
	mkdir -p docs
	cp src/search.js docs/
	cp src/index.js docs/
	cp src/style.css docs/
	cp src/colour-logo-gray.svg docs/
	cp src/search_icon.svg docs/

clean:
	rm -f docs/*
