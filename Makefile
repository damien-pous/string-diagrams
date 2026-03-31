WWW=~/work/public_html/string-diagrams/

all:: test

build::
	dune build

lock::
	nix develop --extra-experimental-features nix-command -f default.nix lock

nix: lock
	direnv allow

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

archive:
	git archive --prefix string-diagrams/ main | bzip2 > string-diagrams.tar.bz2
