all: src-coq coq-project

_opam:
	opam update
	opam switch create . ocaml-base-compiler.4.09.1
	eval $(opam env) && \
	opam pin add coq 8.10.2 -y

hs-to-coq: _opam
	eval $(opam env) && \
	cd hs-to-coq && \
	stack build && \
	make -C base && \
	make -C base-thy

src-coq: hs-to-coq src/Reduction.hs
	eval $$(opam env) && \
	cd hs-to-coq && \
	stack exec hs-to-coq -- -e base/edits -p ../preamble.v --iface-dir base -e ../edits -o ../src-coq ../src/Reduction.hs

Makefile.coq: _CoqProject
	coq_makefile -f _CoqProject -o $@

coq: Makefile.coq src-coq
	make -f Makefile.coq OPT=$(COQFLAGS)

coq-project: src-coq
	eval $$(opam env) && \
	coqc -R hs-to-coq/base "" -Q src-coq/ Src src-coq/Reduction.v && \
	coqc -Q hs-to-coq/base-thy Proofs -Q src-coq Src -Q theories Proving -R hs-to-coq/base '' theories/ReductionSpec.v

clean:
	opam switch remove . -y

.PHONY: all clean hs-to-coq src-coq coq-project