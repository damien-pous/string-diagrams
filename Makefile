RUN=hgui
WWW=~/work/public_html/string-diagrams/

all:: run

build::
	dune build

test::
	dune build
	dune runtest

run::
	dune build
	dune runtest
	dune exec ./bin/$(RUN).exe

www::
	dune runtest
	dune build ./www/hg.bc.js
	cp ./www/hip.css ./www/index.html _build/default/www/hg.bc.js $(WWW)

clean::
	dune clean
