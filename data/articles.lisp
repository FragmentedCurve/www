;; MIND: The tilde character "~" must be escaped like this '~~' to use it as a literal.


;; Define Your Webpage

(defvar *config*
  (list
   :webmaster       "Paco Pascal"
   :title           "Paco Pascal's Writings"
   :description     "Undefined interests of yet another individual"
   :url             "https://www.pacopascal.com/"       ;; the trailing slash is mandatory! RSS links will fail without it. Notice the '~~' to produce a literal '~'
   :rss-item-number 10                                  ;; limit total amount of items in RSS feed to 10
   :date-format "%DayNumber %MonthName %Year"           ;; format for date %DayNumber %DayName %MonthNumber %MonthName %Year
   :default-converter :org-mode
   :html   t                                            ;; 't' to enable export to a html website / 'nil' to disable
   :gopher nil                                          ;; 't' to enable export to a gopher website / 'nil' to disable
   :gemini nil                                          ;; 't' to enable export to a gemini capsule / 'nil' to disable
   :gemini-path      "gemini://perso.pw/blog/"          ;; absolute path of your gemini capsule
   :gemini-index     "index.md"                         ;; filename of index file
   :gopher-path      "/user"                            ;; absolute path of your gopher directory
   :gopher-server    "my.website"                       ;; hostname of the gopher server
   :gopher-port      "70"                               ;; tcp port of the gopher server, 70 usually
   :gopher-format "[~d|~a|~a|~a|~a]~%"                  ;; menu format (geomyidae)
   :gopher-index "index.gph"                            ;; menu file   (geomyidae)
   ;; :gopher-format "~d~a	~a	~a	~a~%"   ;; menu format (gophernicus and others)
   ;; :gopher-index "gophermap"                         ;; menu file (gophernicus and others)
   ))


(converter :name :markdown  :extension ".md"  :command "peg-markdown -t html -o %OUT data/%IN")
(converter :name :markdown2 :extension ".md"  :command "multimarkdown -t html -o %OUT data/%IN")
(converter :name :org-mode  :extension ".org"
	   :command (concatenate 'string
				 "emacs data/%IN --batch --eval '(with-temp-buffer (org-mode) "
				 "(insert-file \"%IN\") (org-html-export-as-html nil nil nil t)"
				 "(princ (buffer-string)))' --kill | tee %OUT"))

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

;; Reposting "The Unintentional Invention of Meat"
(post :id "meat" :date "20111105"
      :title "The Unintentional Invention of Meat: A Story From Calais Hobbes" :Tag "literature shortstory")

;; First Post
(post :id "first" :date "20220516"
      :title "First Post" :tag "about site")
