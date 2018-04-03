.PHONY: all clean byte test repl utop

OCB_FLAGS = -tag bin_annot -use-menhir -use-ocamlfind -pkgs oUnit
OCB = ocamlbuild $(OCB_FLAGS)

all: infer tests

clean:
	$(OCB) -clean

tests:
	$(OCB) tests.byte

infer:
	$(OCB) infer.byte

utop: byte
	utop

test:
	$(OCB) test.byte && ./test.byte
