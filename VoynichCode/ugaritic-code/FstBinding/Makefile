OPT = ""

library: openfst.cma openfst.cmxa libmlopenfst.a

libmlopenfst.a: wrapperCFromCpp.o wrapperOcamlFromC.o
	ocamlmklib -oc mlopenfst wrapperCFromCpp.o wrapperOcamlFromC.o -lfst -lpthread 

openfst.cma: OpenFst.cmo
	ocamlmklib -oc mlopenfst -o openfst OpenFst.cmo -lfst -lpthread

openfst.cmxa: OpenFst.cmx
	ocamlmklib -oc mlopenfst -o openfst OpenFst.cmx -lfst -lpthread

install: library
	ocamlfind remove openfst
	ocamlfind install openfst META openfst.cma openfst.cmxa openfst.a libmlopenfst.a dllmlopenfst.so OpenFst.cmi

test: wrappertest.c.out wrappertest.ocaml.out
	if diff wrappertest.c.out wrappertest.ocaml.out; then echo test passed; else echo C and OCaml got different answers; fi

clean:
	-rm -f *.out *.exe *.o *.cmo *.cmi *.cma *.cmxa *.cmx *.a *.so

OpenFst.cmo OpenFst.cmi: OpenFst.ml
	ocamlfind batteries/ocamlc -syntax camlpfo -g -c OpenFst.ml

OpenFst.cmx: OpenFst.ml
	ocamlfind batteries/ocamlopt -syntax camlpfo -g -c OpenFst.ml

wrapperCFromCpp.o: wrapperCFromCpp.cpp
	g++ -fPIC -c wrapperCFromCpp.cpp

wrapperOcamlFromC.o: wrapperOcamlFromC.c wrapperOcamlFromC.h wrapperCFromCpp.h
	ocamlc -g -c wrapperOcamlFromC.c

wrappertest.c.out: wrappertest.c.exe
	./wrappertest.c.exe

wrappertest.c.exe: wrapperCFromCpp.o wrappertest_c.o
	g++ wrapperCFromCpp.o wrappertest_c.o -lfst -lpthread -o wrappertest.c.exe

wrappertest_c.o: wrappertest_c.c wrapperCFromCpp.h
	gcc -c wrappertest_c.c

wrappertest.ocaml.out: wrappertest.ocaml.exe
	./wrappertest.ocaml.exe

wrappertest.ocaml.exe: OpenFst.ml wrappertest.cmx wrapperOcamlFromC.o wrapperCFromCpp.o
	ocamlopt -g wrapperCFromCpp.o wrapperOcamlFromC.o OpenFst.ml wrappertest.cmx  -ccopt -lfst -ccopt -lpthread -o wrappertest.ocaml.exe

wrappertest.cmo: wrappertest.ml OpenFst.cmo
	ocamlc -g -c wrappertest.ml OpenFst.cmo

wrappertest.cmx: wrappertest.ml OpenFst.cmx
	ocamlopt -g -c wrappertest.ml OpenFst.cmx

OpenFst.top: OpenFst.cma
	ocamlmktop OpenFst.cma -o OpenFst.top

