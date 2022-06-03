all: _opam build

_opam:
	opam update
	opam switch create . ocaml.4.13.2 -y
	eval $$(opam env) && \
	opam install ocaml-lsp-server -y && \
	opam install merlin -y && \

clean:
	eval $$(opam env) && \
	dune clean
	opam switch remove . -y

build: _opam
	eval $$(opam env) && \
	dune build

.PHONY: all clean