(load "data/articles.lisp")

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
;; load a file and return it as a string
(defun slurp-file(path)
  (with-open-file (stream path)
		  (let ((data (make-string (file-length stream))))
		    (read-sequence data stream)
		    data)))

;; save a string in a file
(defun save-file(path data)
  (with-open-file (stream (concatenate 'string "output/" path) :direction :output :if-exists :supersede)
    (format stream data)))

;; simplify the str replace work
(defmacro template(before &body after)
  `(progn
     (setf output (replace-all output ,before ,@after))))

;; simplify the declaration of a new page type
(defmacro prepare(template &body code)
  `(progn
     (let ((output (slurp-file ,template)))
       ,@code
       output)))

;; generates the html of one only article
;; this is called in a loop to produce the homepage
(defun create-article(article &optional &key (tiny t))
  (prepare "template/article.tpl"
	   (template "%%Author%%" (if (member :author article) (getf article :author) (getf *config* :webmaster)))
	   (template "%%Date%%" (getf article :date))
	   (template "%%Title%%" (getf article :title))
	   (template "%%Id%%" (getf article :id))
	   (template "%%Text%%" (if (and tiny (member :tiny article))
				    (getf article :tiny) (slurp-file (format nil "data/~d.txt" (getf article :id)))))))

;; return a html string
;; produce the code of a whole page with title+layout with the parameter as the content
(defun generate-layout(body)
  (let ((output (slurp-file "template/layout.tpl")))
    (template "%%Title%%" (getf *config* :title))
    (template "%%Body%%" body)
    output))


;; Homepage generation
;; generate each article and concatenate the whole
(defun generate-mainpage()
  (format nil "~{~d~}"
	  (loop for article in *articles* collect
		(create-article article :tiny t))))


;; ENGINE START !
;; This is function called when running the tool
(defun generate-site()

  ; produce index.html
  (generate "index.html"
	    (generate-mainpage))

  ; produce each article file
  (dolist (article *articles*)
    (generate (format nil "article-~d.html" (getf article :id))
	      (create-article article :tiny nil)))

  ;;(generate-file-rss)
  ;;not done yet
  )


(generate-site)
