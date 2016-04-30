(load "data/articles.lisp")

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
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
; load a file as a string
(defun slurp-file(path)
  (with-open-file (stream path)
		  (let ((data (make-string (file-length stream))))
		    (read-sequence data stream)
		    data)))

; save a string in a file
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
(defun create-article(article &optional &key (tiny t))
  (prepare "template/article.tpl"
	   (template "%%Author%%" (if (member :author article) (getf article :author) (getf *config* :webmaster)))
	   (template "%%Date%%" (getf article :date))
	   (template "%%Title%%" (getf article :title))
	   (template "%%Id%%" (getf article :id))
	   (template "%%Text%%" (if (and tiny (member :tiny article))
				    (getf article :tiny) (slurp-file (format nil "data/~d.txt" (getf article :id)))))))

;; Layout generation
(defun generate-layout(body)
  (let ((output (slurp-file "template/layout.tpl")))
    (template "%%Title%%" (getf *config* :title))
    (template "%%Body%%" body)
    output))
  
; Homepage generation
(defun generate-mainpage()
  (format nil "~{~d~}"
	  (loop for article in *articles* collect
		(create-article article :tiny t))))

; produce index.html
(defun generate-file-index()
  (save-file "index.html"
	     (generate-layout (generate-mainpage))))

; produce html files for articles
(defun generate-file-article(article)
  (save-file (format nil "article-~d.html" (getf article :id))
	     (generate-layout (create-article article :tiny nil))))

; ENGINE START !
(defun generate-site()
  (generate-file-index)
  (dolist (article *articles*) (generate-file-article article))
  ;;(generate-file-rss)
  )


(generate-site)
