(defparameter *articles* '())
(defparameter *converters* '())

;; structure to store links
(defstruct article title tag date id tiny author short)
(defstruct converter name command extension)

(defun post(&optional &key title tag date id (tiny nil) (author nil) (short nil))
  (push (make-article :title title
                      :tag tag
                      :date date
                      :tiny tiny
                      :author author
                      :short short
                      :id id)
        *articles*))

;; we add a converter to the list of the one availables
(defun converter(&optional &key name command extension)
  (push (make-converter :name name
                        :command command
                        :extension extension)
        *converters*))

(load "data/articles.lisp")
(setf *articles* (reverse *articles*))


;; common-lisp don't have a replace string function natively
(defun replace-all (string part replacement &key (test #'char=))
  (with-output-to-string (out)
			 (loop with part-length = (length part)
			       for old-pos = 0 then (+ pos part-length)
			       for pos = (search part string
						 :start2 old-pos
						 :test test)
			       do (write-string string out
						:start old-pos
						:end (or pos (length string)))
			       when pos do (write-string replacement out)
			       while pos)))

;; common-lisp don't have a split string function natively
(defun split-str(text &optional (separator #\Space))
  "this function split a string with separator and return a list"
  (let ((text (concatenate 'string text (string separator))))
    (loop for char across text
	  counting char into count
	  when (char= char separator)
	  collect
	  ;; we look at the position of the left separator from right to left
	  (let ((left-separator-position (position separator text :from-end t :end (- count 1))))
	    (subseq text
		    ;; if we can't find a separator at the left of the current, then it's the start of
		    ;; the string
		    (if left-separator-position (+ 1 left-separator-position) 0)
		    (- count 1))))))

;; load a file as a string
;; we escape ~ to avoid failures with format
(defun load-file(path)
  (if (probe-file path)
      (replace-all
       (apply #'concatenate 'string
              (with-open-file (stream path)
                (loop for line = (read-line stream nil)
                   while line
                   collect
                   (format nil "~a~%" line))))
       "~" "~~")
    (progn
      (format t "ERROR : file ~a not found. Aborting~%" path)
      (quit))))

;; save a string in a file
(defun save-file(path data)
  (with-open-file (stream path :direction :output :if-exists :supersede)
		  (format stream data)))

;; simplify the str replace work
(defmacro template(before &body after)
  `(progn
     (setf output (replace-all output ,before ,@after))))

;; simplify the declaration of a new page type
(defmacro prepare(template &body code)
  `(progn
     (let ((output (load-file ,template)))
       ,@code
       output)))

;; simplify the file saving by using the layout
(defmacro generate(name &body data)
  `(progn
     (save-file ,name (generate-layout ,@data))))

;; generate the list of tags
(defun articles-by-tag()
  (let ((tag-list))
    (loop for article in *articles* do
	  (when (article-tag article) ;; we don't want an error if no tag
	    (loop for tag in (split-str (article-tag article)) do ;; for each word in tag keyword
		  (setf (getf tag-list (intern tag "KEYWORD")) ;; we create the keyword is inexistent and add ID to :value
			(list
			 :name tag
			 :value (push (article-id article) (getf (getf tag-list (intern tag "KEYWORD")) :value)))))))
    (loop for i from 1 to (length tag-list) by 2 collect ;; removing the keywords
	  (nth i tag-list))))

;; generates the html of the list of tags for an article
(defun get-tag-list-article(&optional article)
  (apply #'concatenate 'string
         (mapcar #'(lambda (item)
                     (prepare "templates/one-tag.tpl" (template "%%Name%%" item)))
                 (split-str (article-tag article)))))

;; generates the html of the whole list of tags
(defun get-tag-list()
  (apply #'concatenate 'string
         (mapcar #'(lambda (item)
                     (prepare "templates/one-tag.tpl"
                              (template "%%Name%%" (getf item :name))))
                 (articles-by-tag))))


;; generates the html of one only article
;; this is called in a loop to produce the homepage
(defun create-article(article &optional &key (tiny t) (no-text nil))
  (prepare "templates/article.tpl"
	   (template "%%Author%%" (let ((author (article-author article)))
                                    (or author (getf *config* :webmaster))))
	   (template "%%Date%%"   (article-date article))
	   (template "%%Title%%"  (article-title article))
	   (template "%%Id%%"     (article-id article))
	   (template "%%Tags%%"   (get-tag-list-article article))
	   (template "%%Text%%"   (if no-text
				      ""
                                      (if (and tiny (article-tiny article))
                                          (article-tiny article)
                                          (load-file (format nil "temp/data/~d.html" (article-id article))))))))

;; return a html string
;; produce the code of a whole page with title+layout with the parameter as the content
(defun generate-layout(body &optional &key (title nil))
  (prepare "templates/layout.tpl"
	   (template "%%Title%%" (if title title (getf *config* :title)))
	   (template "%%Tags%%" (get-tag-list))
	   (template "%%Body%%" body)
	   output))


;; html generation of index homepage
(defun generate-semi-mainpage(&key (tiny t) (no-text nil))
  (apply #'concatenate 'string
         (loop for article in *articles* collect
              (create-article article :tiny tiny :no-text no-text))))

;; html generation of a tag homepage
(defun generate-tag-mainpage(articles-in-tag)
  (apply #'concatenate 'string
         (loop for article in *articles* 
            when (member (article-id article) articles-in-tag :test #'equal)
            collect (create-article article :tiny t))))

;; xml generation of the items for the rss
(defun generate-rss-item()
  (apply #'concatenate 'string
         (loop for article in *articles*
            for i from 1 to (if (> (length *articles*) (getf *config* :rss-item-number)) (getf *config* :rss-item-number) (length *articles*))
            collect
              (prepare "templates/rss-item.tpl"
                       (template "%%Title%%" (article-title article))
                       (template "%%Description%%" (load-file (format nil "temp/data/~d.html" (article-id article))))
                       (template "%%Url%%"
                                 (format nil "~darticle-~d.html"
                                         (getf *config* :url)
                                         (article-id article)))))))

;; Generate the rss xml data
(defun generate-rss()
  (prepare "templates/rss.tpl"
	   (template "%%Description%%" (getf *config* :description))
	   (template "%%Title%%" (getf *config* :title))
	   (template "%%Url%%" (getf *config* :url))
	   (template "%%Items%%" (generate-rss-item))))

;; We do all the website
(defun create-html-site()
  ;; produce index.html
  (generate "output/html/index.html" (generate-semi-mainpage))

  ;; produce index-titles.html where there are only articles titles
  (generate "output/html/index-titles.html" (generate-semi-mainpage :no-text t))

  ;; produce each article file
  (dolist (article *articles*)
    (generate (format nil "output/html/article-~d.html" (article-id article))
	      (create-article article :tiny nil)
	      :title (concatenate 'string (getf *config* :title) " : " (article-title article))))
  
  ;; produce index file for each tag
  (loop for tag in (articles-by-tag) do
	(generate (format nil "output/html/tag-~d.html" (getf tag :NAME))
		  (generate-tag-mainpage (getf tag :VALUE))))
  
  ;;(generate-file-rss)
  (save-file "output/html/rss.xml" (generate-rss)))

;; we do all the gopher hole
(defun create-gopher-hole()

  ;; produce the gophermap file
  (save-file (concatenate 'string "output/gopher/" (getf *config* :gopher-index))
	     (let ((output (load-file "templates/gopher_head.tpl")))
	       (dolist (article *articles*)
		 (setf output
		       (string
			(concatenate 'string output
                                     (format nil (getf *config* :gopher-format)
					     ;; here we create a 80 width char string with title on the left
					     ;; and date on the right
					     ;; we truncate the article title if it's too large
					     (let ((title (format nil "~80a"
								  (if (< 80 (length (article-title article)))
								      (subseq (article-title article) 0 80)
								    (article-title article)))))
					       (replace title (article-date article) :start1 (- (length title) (length (article-date article)))))
					     
					     
					     (getf *config* :gopher-path)
					     (article-id article)
					     (getf *config* :gopher-server)
					     (getf *config* :gopher-port)
					     )))))	       
	       output))
  
  ;; produce each article file (only a copy/paste in fact)
  (dolist (article *articles*)
    (let ((id (article-id article)))
      (save-file (format nil "output/gopher/article-~d.txt" id)
		 (load-file (format nil "data/~d.md" id)))))
  
  )


;; ENGINE START !
;; This is function called when running the tool
(defun generate-site()
  (if (getf *config* :html)
      (create-html-site))
  (if (getf *config* :gopher)
      (create-gopher-hole)))


(generate-site)

(quit)
