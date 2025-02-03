.if !exists("./config.mk")
!= cp config.mk.def config.mk
.endif

.include "./config.mk"

all: dirs html

html: ${HTML} css
	${LISP} --load generator.lisp

dirs:
	mkdir -p "output/html/static"
	mkdir -p "output/gopher"
	mkdir -p "output/gemini/articles/"

css:
	mkdir -p "output/html/static"
	cp -fr static/* "output/html/static/"

clean:
	rm -fr output/html/* output/gopher/* "temp"

.ifmake post
.for v in ${TITLE:tl}
POST_TITLE := ${POST_TITLE}_${v}
.endfor

POST_DATE != date "+%Y%m%d"
POST_FILE = ${POST_DATE}${POST_TITLE}.org

post:
	@git checkout -b ${POST_FILE}

	@echo mkdir static/img/${POST_FILE}
	@echo "#+TITLE:   ${TITLE}"        >> data/${POST_FILE}
	@echo "#+AUTHOR:  ${AUTHOR}"       >> data/${POST_FILE}
	@echo "#+DATE:    ${POST_DATE}"    >> data/${POST_FILE}
	@echo "#+OPTIONS: toc:nil num:nil" >> data/${POST_FILE}

	@git add data/${POST_FILE}
	@git commit -m 'new post: ${TITLE}'
.endif

live:
.if empty(RSYNC_DEST)
	@false
.else
	rsync -rlvh --delete output/html/ $(RSYNC_DEST)
.endif
