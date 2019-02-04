
OCAMLMAKEFILE = OCamlMakefile

SOURCES = myDynArray.ml
RESULT = myDynArray
PACKS = extlib
LIBINSTALL_FILES = myDynArray.cma myDynArray.cmxa myDynArray.a myDynArray.cmo \
  $(SOURCES) $(SOURCES:.ml=.cmi)
DOC_FILES = $(SOURCES)
OCAMLFLAGS = -g
OCAMLLDFLAGS = -g
OCAMLMKLIB_FLAGS = -g
OCAMLDOCFLAGs = -hide Pervasives -keep-code -colorize-code

all:	dcl ncl
doc:	htdoc
install:	libuninstall libinstall

include $(OCAMLMAKEFILE)


