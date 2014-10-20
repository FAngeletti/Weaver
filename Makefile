all: main.js

main.js:main.byte
	js_of_ocaml --pretty --noinline --debug-info $<

main.byte:*.ml 
	ocamlbuild -use-ocamlfind -pkg js_of_ocaml -cflag -g $@

