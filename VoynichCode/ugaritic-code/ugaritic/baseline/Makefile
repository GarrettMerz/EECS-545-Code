
OCAMLMAKEFILE = OCamlMakefile
BINDIR = ~/bin
SOURCES = ugaritic_em.ml 
RESULT = ugaritic_em
OCAMLOPT = batteries/ocamlopt
OCAMLDEP = batteries/ocamldep
OCAMLC = batteries/ocamlc
OCAML = batteries/ocaml
PACKS = ben gsl hhl
DOC_FILES = $(SOURCES)
#export OCAML_DEP_PACKAGES = -package sexplib.syntax
OCAMLFLAGS = -g
OCAMLNCFLAGS = -g
OCAMLNLDFLAGS = -g 
OCAMLMKLIB_FLAGS = -g 
OCAMLLDFLAGS = -g

default:	nc
all:	nc top
prof-install:	pnc
	install -c -m 755 $(RESULT) $(BINDIR)
install:	nc
	install -c -m 755 $(RESULT) $(BINDIR)
#	install -c -m 755 ugaritic.top $(BINDIR)

include $(OCAMLMAKEFILE)

