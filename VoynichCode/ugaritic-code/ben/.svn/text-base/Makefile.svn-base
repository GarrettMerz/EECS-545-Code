
OCAMLMAKEFILE = OCamlMakefile

SOURCES = util.ml lattice.ml
RESULT = ben
PACKS = str extlib hashSet unix num camomile myDynArray batteries batteries.pa_openin.syntax batteries.pa_where.syntax batteries.pa_strings.syntax
PP = camlp4find $(PACKS)
export PP
LIBINSTALL_FILES = ben.cma ben.cmxa ben.a \
  $(SOURCES) $(SOURCES:.ml=.cmi)
DOC_FILES = $(SOURCES)
OCAMLFLAGS = -g
OCAMLLDFLAGS = -g
OCAMLMKLIB_FLAGS = -g
OCAMLDOCFLAGS = -hide Pervasives -keep-code -colorize-code

all:	dcl ncl
doc:	htdoc
install:	libuninstall libinstall
docinstall:
	cp -r /afs/csail.mit.edu/u/b/bsnyder/src/ben/doc/ben/html/ \
              /afs/csail.mit.edu/u/b/bsnyder/public_html/docs/ocaml/ben

include $(OCAMLMAKEFILE)

cleandoc:
	rm -rf /afs/csail.mit.edu/u/b/bsnyder/src/ben/doc/html

