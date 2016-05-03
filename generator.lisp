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

;; common-lisp don't have a split string function natively
;; thanks https://gist.github.com/siguremon/1174988
(defun split-str-1 (string &optional (separator " ") (r nil))
  (let ((n (position separator string
		     :from-end t
		     :test #'(lambda (x y)
			       (find y x :test #'string=)))))
    (if n
	(split-str-1 (subseq string 0 n) separator (cons (subseq string (1+ n)) r))
            (cons string r))))
(defun split-str (string &optional (separator " "))
  (split-str-1 string separator))

;; we have to remove the quotes
;; when using collect in a loop
(defun strip-quotes(input)
  (format nil "~{~d~%~}" input))

;; load a file as a string
;; we escape ~ to avoid failures with format
(defun load-file(path)
  (replace-all
   (strip-quotes
    (with-open-file (stream path)
     (loop for line = (read-line stream nil) while line collect line)))
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
     (save-file ,name (generate-layout ,@data))))

;; generate the list of tags
(defun articles-by-tag()
  (let ((tag-list))
    (loop for article in *articles* do
	  (if (getf article :tag nil) ;; we don't want an error if no tag
	      (loop for tag in (split-str (getf article :tag)) do ;; for each word in tag keyword
		    (setf (getf tag-list (intern tag "KEYWORD")) ;; we create the keyword is inexistent and add ID to :value
			  (list
			   :name tag
			   :value (push (getf article :id) (getf (getf tag-list (intern tag "KEYWORD")) :value)))))))
    (loop for i from 1 to (length tag-list) by 2 collect ;; removing the keywords
	  (nth i tag-list))))
    
;; generates the html of the list of tags for an article
(defun get-tag-list-article(&optional article)
  (strip-quotes
   (mapcar #'(lambda (item)
	       (prepare "template/one-tag.tpl" (template "%%Name%%" item)))
	   (split-str (getf article :tag)))))

;; generates the html of the whole list of tags
(defun get-tag-list()
  (strip-quotes
   (mapcar #'(lambda (item)
	       (prepare "template/one-tag.tpl"
			(template "%%Name%%" (getf item :name))))
	   (articles-by-tag))))
  

;; generates the html of one only article
;; this is called in a loop to produce the homepage
(defun create-article(article &optional &key (tiny t))
  (prepare "template/article.tpl"
	   (template "%%Author%%" (getf article :author (getf *config* :webmaster)))
	   (template "%%Date%%" (getf article :date))
	   (template "%%Title%%" (getf article :title))
	   (template "%%Id%%" (getf article :id))
	   (template "%%Tags%%" (get-tag-list-article article))
	   (template "%%Text%%" (if (and tiny (member :tiny article))
				    (getf article :tiny) (load-file (format nil "data/~d.txt" (getf article :id)))))))

;; return a html string
;; produce the code of a whole page with title+layout with the parameter as the content
(defun generate-layout(body)
  (prepare "template/layout.tpl"
	   (template "%%Title%%" (getf *config* :title))
	   (template "%%Tags%%" (get-tag-list))
	   (template "%%Body%%" body)
	   output))


;; html generation of index homepage
(defun generate-semi-mainpage()
  (strip-quotes
   (loop for article in *articles* collect
	 (create-article article :tiny t))))

;; html generation of a tag homepage
(defun generate-tag-mainpage(articles-in-tag)
  (strip-quotes
   (loop for article in *articles* 
	 when (member (getf article :id) articles-in-tag :test #'equal)
	 collect (create-article article :tiny t))))

;; xml generation of the items for the rss
(defun generate-rss-item()
  (strip-quotes
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

  ;; produce index file for each tag
  (loop for tag in (articles-by-tag) do
	(generate (format nil"tag-~d.html" (getf tag :NAME))
		  (generate-tag-mainpage (getf tag :VALUE))))
  
  ;;(generate-file-rss)
  (save-file "rss.xml" (generate-rss))
  
  )

(generate-site)
(exit)
