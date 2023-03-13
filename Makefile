build:
	dune build

play:
	OCAMLRUNPARAM=b dune exec _build/default/bin/main.exe