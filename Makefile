.PHONY: clean test install uninstall

all: src/redis_protocol.ml src/redis_protocol.mli docs
	ocamlbuild -r src/redis_protocol.cmx src/redis_protocol.cmo src/redis_protocol.cmxa src/redis_protocol.cma
docs: src/redis_protocol.mli
	ocamlbuild -r redis_protocol.docdir/index.html
clean:
	ocamlbuild -clean
test:
	corebuild -r test/property_tests.native && time ./property_tests.native
install: all
	ocamlfind install redis-protocol META _build/src/redis_protocol.cmi _build/src/redis_protocol.cma _build/src/redis_protocol.cmo _build/src/redis_protocol.o _build/src/redis_protocol.cmx _build/src/redis_protocol.cmxa _build/src/redis_protocol.mli
uninstall:
	ocamlfind remove redis-protocol
