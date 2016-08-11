LISP=          sbcl
MD=            multimarkdown -o

HTMLDIR=       temp/data
ARTICLES!=     ls data/*.md
HTML=          $(ARTICLES:.md=.html)

.if "${LISP}" == "sbcl"
PARAM=--dynamic-space-size 60 --script
.elif "${LISP}" == "clisp"
PARAM=
.elif "${LISP}" == "ecl"
PARAM=-shell
.endif

all: clean dirs html

html: $(HTML) css
	LANG=en_US.UTF-8 $(LISP) $(PARAM) generator.lisp
	rm -fr "temp"

dirs:
	mkdir -p "$(HTMLDIR)"
	mkdir -p "output/html/static"
	mkdir -p "output/gopher"

.SUFFIXES: .md .html
.md.html:
	$(MD) "$(HTMLDIR)/$(@F)" "$<"

clean:
	rm -fr output/html/* output/gopher/* "temp"

css:
	mkdir -p "output/html/static"
	cp -fr static/* "output/html/static/"
