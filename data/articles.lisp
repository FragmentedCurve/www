(defvar *config*
  (list
   :webmaster "Your author name here"
   :title "Your blog title here"
   ))

;; describes articles (ordered)
;; exemple => (list :id "4" :date "2015-05-04" :title "The article title" :author "Me" :tiny "Short description for home page")
;; :author can be omitted and will be replaced by webmaster value
;; :tiny can be omitted and will be replaced by the full article text
(defvar *articles*
  (list
   (list :id "1" :date "29 April 2016" :title "My first message" :short "This is my first message" :author "Solène")
   ))

