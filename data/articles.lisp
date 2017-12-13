;; MIND: The tilde character "~" must be escaped like this '~~' to use it as a literal.


;; Define Your Webpage

(defvar *config*
  (list
   :webmaster       "Your autor name here"
   :title           "Your website's title."
   :description     "Yet another website on the net"
   :url             "https://my.website/~~user/"        ;; the trailing slash is mandatory! RSS links will fail without it. Notice the '~~' to produce a literal '~'
   :rss-item-number 10                                  ;; limit total amount of items in RSS feed to 10
   :date-format "%DayNumber %MonthName %Year"           ;; format for date %DayNumber %DayName %MonthNumber %MonthName %Year
   :html   t                                            ;; 't' to enable export to a html website / 'nil' to disable
   :gopher t                                            ;; 't' to enable export to a gopher website / 'nil' to disable
   :gopher-path      "/user"                            ;; absolute path of your gopher directory
   :gopher-server    "my.website"                       ;; hostname of the gopher server
   :gopher-port      "70"                               ;; tcp port of the gopher server, 70 usually
   :gopher-format "[0|~a|~a/article-~d.txt|~a|~a]~%~%"  ;; geomyidae
   :gopher-index "index.gph"                            ;; geomyidae
   ;; :gopher-format "0~a	~a/article-~d.txt	~a	~a~%~%" ;; gophernicus and others
   ;; :gopher-index "gophermap"                         ;; gophernicus and others
   ))


(converter :name :markdown  :extension ".md" :command "peg-markdown -o %IN")
(converter :name :markdown2 :extension ".md" :command "multimarkdown -o %IN")

;; Define your articles and their display-order on the website below.
;; Display Order is 'lifo', i.e. the top entry in this list gets displayed as the topmost entry.
;; 
;; An Example Of A Minimal Definition:
;; (post :id "4" :date "2015-12-31" :title "Happy new year" :tag "news")

;; An Example Of A Definitions With Options:
;; (post :id "4" :date "2015-05-04" :title "The article title" :tag "news" :author "Me" :tiny "Short description for home page")
;;
;; A Note On Keywords:
;; :author  can be omitted.   If so, it's value gets replaced by the value of :webmaster.
;; :tiny    can be omitted.   If so, the article's full text gets displayed on the all-articles view. (most people don't want this.)


;; CSS
(post :title "CSS For cl-yag"
      :id "css" :date "20171202" :tag "cl-yag"
      :author "lambda" :tiny "Read more")

;; README
(post :title "README"
      :id "README" :date "20171202" :tag "cl-yag"
      :author "lambda" :tiny "Read cl-yag's README")

;; 1
(post :title "My first post"
      :id "1" :date "20160429" :tag "pony"
      :tiny "This is the first message" :author "Solène")
