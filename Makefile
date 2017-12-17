LISP=          sbcl

all: dirs html

html: $(HTML) css
	$(LISP) --load generator.lisp

dirs:
	mkdir -p "output/html/static"
	mkdir -p "output/gopher"


clean:
	rm -fr output/html/* output/gopher/* "temp"

css:
	mkdir -p "output/html/static"
	cp -fr static/* "output/html/static/"
