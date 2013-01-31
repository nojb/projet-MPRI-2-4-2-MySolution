EXECUTABLE := joujou

all:
	ocamlbuild -use-menhir front.native
	mv front.native $(EXECUTABLE)

clean:
	ocamlbuild -clean
	rm -f $(EXECUTABLE)

.PHONY: all clean
