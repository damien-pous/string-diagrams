RUN=edit
WWW=~/work/public_html/string-diagrams/

all:: run

build::
	dune build

test::
	dune build
	OCAMLRUNPARAM=b dune runtest

run::
	dune build
	OCAMLRUNPARAM=b dune runtest
	OCAMLRUNPARAM=b dune exec ./bin/$(RUN).exe

www::
	dune runtest
	dune build ./www/applet.bc.js
	cp ./www/hip.css ./www/index.html _build/default/www/applet.bc.js $(WWW)

clean::
	dune clean
