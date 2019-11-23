all:
	mkdir -p docs
	dune exec src/process.exe
	cp src/search.js docs/
	cp src/index.js docs/
	cp src/style.css docs/
	cp src/colour-logo-gray.svg docs/
	cp src/search_icon.svg docs/

index:
	dune exec src/process.exe makeindex

clean:
	rm -f docs/*
