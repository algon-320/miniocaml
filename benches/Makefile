.PHONY: run
run: input tarai_c tarai_miniocaml tarai_ocamlc tarai_ocamlopt tarai.py
	time -p ./tarai_miniocaml < input
	time -p ./tarai_miniocaml < input
	time -p ./tarai_c < input
	time -p ./tarai_c < input
	time -p ./tarai_ocamlc < input
	time -p ./tarai_ocamlc < input
	time -p ./tarai_ocamlopt < input
	time -p ./tarai_ocamlopt < input
	time -p python tarai.py < input
	time -p python tarai.py < input
	
tarai_miniocaml: tarai.mml ../miniocaml
	../miniocaml tarai.mml -o tarai_miniocaml

tarai_c: tarai.c
	gcc tarai.c -O3 -o tarai_c

tarai_ocamlc: tarai.ml
	ocamlc tarai.ml -o tarai_ocamlc

tarai_ocamlopt: tarai.ml
	ocamlopt tarai.ml -o tarai_ocamlopt

.PHONY: clean
clean:
	-rm tarai.cm* tarai.o
	-rm tarai_*
