build:
	dune build

code:
	-dune build
	code .
	! dune build --watch

wordle:
	OCAMLRUNPARAM=b dune utop lib -- start

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/final.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

clean: bisect-clean
	dune clean
	rm -f search.zip

zip:
	rm -f wordle.zip
	zip -r wordle.zip . -x@exclude.lst
	
test:
	OCAMLRUNPARAM=b dune exec test/final.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh	