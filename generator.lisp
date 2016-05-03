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

;; load a file as a string
;; we escape ~ to avoid failures with format
(defun load-file(path)
  (replace-all 
   (with-open-file (stream path)
		   (let ((data (make-string (file-length stream))))
		     (read-sequence data stream)
		     data))
   "~" "~~"))

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
     (let ((output (load-file ,template)))
       ,@code
       output)))

;; simplify the file saving by using the layout
(defmacro generate(name &body data)
  `(progn
     (save-file ,name
		(generate-layout ,@data))))


;; generates the html of one only article
;; this is called in a loop to produce the homepage
(defun create-article(article &optional &key (tiny t))
  (prepare "template/article.tpl"
	   (template "%%Author%%" (getf article :author (getf *config* :webmaster)))
	   (template "%%Date%%" (getf article :date))
	   (template "%%Title%%" (getf article :title))
	   (template "%%Id%%" (getf article :id))
	   (template "%%Text%%" (if (and tiny (member :tiny article))
				    (getf article :tiny) (load-file (format nil "data/~d.txt" (getf article :id)))))))

;; return a html string
;; produce the code of a whole page with title+layout with the parameter as the content
(defun generate-layout(body)
  (prepare "template/layout.tpl"
	   (template "%%Title%%" (getf *config* :title))
	   (template "%%Body%%" body)
	   output))


;; Homepage generation
(defun generate-semi-mainpage()
  (format nil "~{~d~}"
	  (loop for article in *articles* collect
		(create-article article :tiny t))))


;; Generate the items for the xml
(defun generate-rss-item()
  (format nil "~{~d~}"
	  (loop for article in *articles* collect
		(prepare "template/rss-item.tpl"
			 (template "%%Title%%" (getf article :title))
			 (template "%%Description%%" (getf article :short ""))
			 (template "%%Url%%"
				   (format nil "~d/article-~d.html"
					   (getf *config* :url)
					   (getf article :id)))))))
  
;; Generate the rss xml data
(defun generate-rss()
  (prepare "template/rss.tpl"
	   (template "%%Description%%" (getf *config* :description))
	   (template "%%Title%%" (getf *config* :title))
	   (template "%%Url%%" (getf *config* :url))
	   (template "%%Items%%" (generate-rss-item))))


;; ENGINE START !
;; This is function called when running the tool
(defun generate-site()

  ;; produce index.html
  (generate "index.html" (generate-semi-mainpage))
  
  ;; produce each article file
  (dolist (article *articles*)
    (generate (format nil "article-~d.html" (getf article :id))
	      (create-article article :tiny nil)))
  
  ;;(generate-file-rss)
  (save-file "rss.xml" (generate-rss))
  )

(generate-site)
