LISP=ecl
PARAM=""

.if "${LISP}" == "sbcl"
PARAM=--dynamic-space-size 60 --script
.elif "${LISP}" == "clisp"
PARAM=
.elif "${LISP}" == "ecl"
PARAM=-shell
.endif

all:
	mkdir -p output/static
	cp -fr static/* output/static/
	$(LISP) $(PARAM) generator.lisp

clean:
	rm -fr output/*

css:
	mkdir -p output/static
	cp -fr static/* output/static/	
