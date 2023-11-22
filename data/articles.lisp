;; MIND: The tilde character "~" must be escaped like this '~~' to use it as a literal.


;; Define Your Webpage

(defvar *config*
  (list
   :webmaster       "Paco Pascal"
   :title           "Paco Pascal"
   :description     ""
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
(converter :name :org-mode :extension ".org"
	   :command (concatenate 'string "emacs --insert=data/%IN --batch -l compile.el --kill | tee %OUT"))

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



;; Farewell to a Childhood Hero
(post :id "mitnick" :date "20230719"
      :title "Farewell to a Childhood Hero" :tag "kevin mitnick")

;; Nesticle: Part 1
(post :id "nesticle_1" :date "20230528"
      :title "Porting NESticle for Fun and Profit: Part 1" :tag "nes emulation")

;; ChatGPT Can't Do Basic Math
(post :id "chatgpt_math" :date "20230524"
      :title "ChatGPT Can't Do Basic Math" :tag "chatgpt math")

;; Wishlist: 2022
;;(post :id "wishlist_2022" :date "20221201"
;;      :title "Wish List: 2022" :tag "wishlist")

;; I Met a Man Today
;;(post :id "i_met_a_man_today" :date "20221016"
;;      :title "I Met a Man Today" :tag "fiction short")

;; I Want a Beard
;;(post :id "i_want_a_beard" :date "20130928"
;;      :title "I Want a Beard" :tag "fiction short")

;; Summer Flowers Under Autumn Leaves
;;(post :id "summer_flowers_under_autumn_leaves" :date "20150508"
;;      :title "Summer Flowers Under Autumn Leaves" :tag "fiction short")

;; Nesticle: Part 0
(post :id "nesticle_0" :date "20220924"
      :title "Porting NESticle for Fun and Profit: Part 0" :tag "nes emulation")

;; Using Age to Publicly Post Sensitive Data
(post :id "public_data" :date "20220611"
      :title "Using Age to Publicly Post Sensitive Data" :tag "unix linux gpg age cryptography")

;; Over the Wire: Behemoth
(post :id "behemoth" :date "20220517"
      :title "Over the Wire: Behemoth" :tag "hacking")

;; Reposting "The Unintentional Invention of Meat"
;;(post :id "meat" :date "20111105"
;;      :title "The Unintentional Invention of Meat: A Story From Calais Hobbes" :tag "fiction")

;; First Post
;;(post :id "first" :date "20220516"
;;      :title "First Post" :tag "site")
