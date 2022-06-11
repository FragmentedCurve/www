LISP=          ecl

all: dirs html

html: $(HTML) css
	$(LISP) --load generator.lisp

dirs:
	mkdir -p "output/html/static"
	mkdir -p "output/gopher"
	mkdir -p "output/gemini/articles/"


clean:
	rm -fr output/html/* output/gopher/* "temp"

css:
	mkdir -p "output/html/static"
	cp -fr static/* "output/html/static/"

live:
	rsync -avh --delete output/html/ -e ssh paco@pacopascal.com:/home/paco/www/
