LISP=          sbcl
MD=            multimarkdown -o

HTMLDIR=       temp/data
ARTICLES!=     ls data/*.md
HTML=          $(ARTICLES:.md=.html)

all: clean dirs html

html: $(HTML) css
	$(LISP) --load generator.lisp
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
