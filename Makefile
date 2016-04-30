all:
	mkdir -p output/static
	cp -fr static/* output/static/
	sbcl --dynamic-space-size 60 --script generator.lisp

clean:
	rm -fr output/*

css:
	mkdir -p output/static
	cp -fr static/* output/static/	
