;; WARNING caracter "~" must be escaped when used in this file
;; you have to type ~~ for one ~ to escape it


;; define informations about your blog
;; used for the RSS generation and some variables replacements in the layout
(defvar *config*
  (list
   :webmaster "Your author name here"
   :title "Your blog title here"
   :description "Yet another website on the net"
   :url "https://my.website/~~user/"
   ))

;; describes articles (ordered on the website as they are displayed here, the first in list is the top of the website)
;; exemple => (list :id "4" :date "2015-05-04" :title "The article title" :author "Me" :tiny "Short description for home page")
;; :author can be omitted and will be replaced by webmaster value
;; :tiny can be omitted and will be replaced by the full article text
(defvar *articles*
  (list
   (list :id "2" :date "30 April 2016" :tag "lisp" :title "Another message" :short "New version available")   
   (list :id "1" :date "29 April 2016":tag "pony code" :title "My first message" :short "This is my first message" :author "Solène")
   ))

