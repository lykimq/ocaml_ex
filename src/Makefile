.DEFAULT_GOAL := all

.PHONY: all
all: 
	dune build @all

.PHONY: check
check:
	dune build @check
  
.PHONY: clean 
clean: # Clean build artifacts and other generated files
	dune clean

.PHONY: fmt 
fmt: # Format the codebase with ocamlformat 
	dune build @fmt --auto-promote