WWW=~/work/public_html/string-diagrams/

all:: test

build::
	dune build

test::
	dune build
	OCAMLRUNPARAM=b dune runtest
	OCAMLRUNPARAM=b dune exec ./bin/check.exe -- *.sd

run::
	dune build
	OCAMLRUNPARAM=b dune runtest
	OCAMLRUNPARAM=b dune exec ./bin/sd.exe -- mumu

www::
	dune runtest
	dune build ./bin/applet.bc.js
	cp ./www/hip.css ./www/index.html _build/default/bin/applet.bc.js $(WWW)

clean::
	dune clean
