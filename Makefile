SRCFILES = lib/*.ml lib/*.mli debug/*.ml

OCPINDENT = ocp-indent \
	--inplace \
	$(SRCFILES)

.PHONY: all
all :
	dune build @all

.PHONY: debug
debug :
	dune exec --no-buffer debug/main.exe

.PHONY: format
format :
	$(OCPINDENT)

.PHONY : clean
clean:
	dune clean
