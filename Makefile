.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/Boggle.exe

check:
	@bash check.sh

clean:
	dune clean
	