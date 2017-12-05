;; MIND: The tilde character "~" must be escaped like this '~~' to use it as a literal.


;; Define Your Webpage

(defvar *config*
  (list
   :webmaster       "Your autor name here"
   :title           "Your website's title."
   :description     "Yet another website on the net"
   :url             "https://my.website/~~user/"     ;; the trailing slash is mandatory! RSS links will fail without it. Notice the '~~' to produce a literal '~'
   :rss-item-number 10                               ;; limit total amount of items in RSS feed to 10
   :html   t                                         ;; 't' to enable export to a html website / 'nil' to disable
   :gopher t                                         ;; 't' to enable export to a gopher website / 'nil' to disable
   :gopher-path      "/user"                         ;; absolute path of your gopher directory
   :gopher-server    "my.website"                    ;; hostname of the gopher server
   :gopher-port      "70"                            ;; tcp port of the gopher server, 70 usually
   ))





;; Define your articles and their display-order on the website in *articles* below.
;; Display Order is 'lifo', i.e. the top entry in this list gets displayed as the topmost entry.
;; 
;; An Example Of A Minimal Definition:
;; (list :id "4" :date "2015-05-04" :title "The article title" :author "Me" :tiny "Short description for home page")
;;
;; A Note On Keywords:
;; :author  can be omitted.   If so, it's value gets replaced by the value of :webmaster.
;; :tiny    can be omitted.   If so, the article's full text gets displayed on the all-articles view. (most people don't want this.)

(defvar *articles*
  (list
   ;; README
   (list :id "README"    :date "23 November 2017"  :tag "cl-yag README"
	 :title "README" :author "lambda"     :short "cl-yag's README got reworked." :tiny "Read cl-yag's README")
   ;; 1
   (list :id "1"         :date "29 April 2016":tag "pony code"
	 :title "My first message" :short "This is my first message" :author "Solène" :tiny "Read more")
   ))

