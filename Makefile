OCB_FLAGS = -use-ocamlfind -use-menhir
OCB = ocamlbuild $(OCB_FLAGS)

.PHONY: all native byte clean

all: native btye

clean:
	$(OCB) -clean

native:
	$(OCB) main.native

btye:
	$(OCB) main.byte
