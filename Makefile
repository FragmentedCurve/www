ifeq ($(wildcard config.mk),)
$(shell cp config.mk.def config.mk)
endif

include config.mk

LISP ?= ecl

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
ifeq ($(RSYNC_DEST),)
	@false
else
	rsync -rlvh --delete output/html/ $(RSYNC_DEST)
endif
