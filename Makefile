WWW=~/work/public_html/string-diagrams/

all:: test

build::
	dune build

test::
	dune build
	OCAMLRUNPARAM=b dune runtest
	dune exec ./bin/text.exe -- -check unicode
	dune exec ./bin/text.exe -- -check default
	dune exec ./bin/text.exe -- -check mumu

prove::
	dune build
	OCAMLRUNPARAM=b dune runtest
	OCAMLRUNPARAM=b dune exec ./bin/prove.exe -- mumu

edit::
	dune build
	OCAMLRUNPARAM=b dune runtest
	OCAMLRUNPARAM=b dune exec ./bin/edit.exe

www::
	dune runtest
	dune build ./www/applet.bc.js
	cp ./www/hip.css ./www/index.html _build/default/www/applet.bc.js $(WWW)

clean::
	dune clean
