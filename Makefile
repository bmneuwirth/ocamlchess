.PHONY: test

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec _build/default/bin/main.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh	
