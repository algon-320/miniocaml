miniocaml: src/*.ml
	dune build
	cp ./_build/default/src/miniocaml.exe miniocaml

.PHONY: test
test: miniocaml
	dune runtest
	./compiler_test.sh

.PHONY: bench
bench: miniocaml
	make -C ./benches run

.PHONY: clean
clean:
	make -C ./benches clean
	dune clean
	-rm miniocaml
