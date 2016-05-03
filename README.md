# Introduction

cl-yag stands for Common Lisp Yet Another Generator and obviously it's written in Common Lisp.

It has only one dependency : a common lisp interpreter, I recommend both sbcl or clisp which are free, open-source and multi-platform.

 
# The hierarchy

Here are the files and folder of cl-yag :
 
+ **Makefile** : exists to simplify your life (updating, cleaning) 
+ **generator.lisp** : contains all the code of the generator 
+ **templates/** : contains .tpl files which are used as template for the html structure 
+ **static/** : contains static files that need to be made public like images, css, js etc...
+ **data/** : contains what will make the content of your website different from another website (or not) 
  + **articles.lisp** : contains metadata about the website and the list of the articles with their id/title/date/(author/short description) (aren't mandatory) 
  + **${id}.txt** : contains the html text of the article ${id} that will be used when displayed 
+ **output** : this is where the websites goes when your run *make*, and where it's cleaned when you run *make clean*; You can make it a symbolic link to the web server folder.

 
# How to add an article
 
Edit data/articles.lisp and add a new line inside the *articles* variable like this

    (list :id "2" :date "29 April 2016" :title "How do I use cl-yag" :author "Solène" :short "I will explain how to use the generator" :tag "example help code")

The _:short_ field is used on the homepage. It it is defined, this is the text that will be shown on the homepage with all the others articles. If it's not defined, the whole article content will be used on the homepage. Sometimes when you have long articles, you may not want to display it entirely on the index so you can use _:short "view the article for the full text_.

The _:author_ field is used to display who wrote the article. You can omitt it, the generator will take the name from the *config* variable

The _:tag_ field is used to create a page with all the articles with the same tag. You can omitt it if you don't want it tagged. Tags can't contain spaces.

# How to hack it

I tried to make it "hacking friendly" so it's very extensible. 

## Include some file in the template

Here is an example code if you want to include a page in the template

+ Add a string for the replacement to occure, like %%Panel%% in **template/layout.tpl** (because we want the panel on every page)
+ In **generator.lisp** modify the function *generate-layout* to add "**(template "%%Panel%%" (load-file "template/panel.tpl"))**" after one template function call
+ Create **template/panel.tpl** with the html

(note : you can also directly add your text inside the layout template file instead of including another file)

## Add a new specific page

You may want to have some dedicated page for some reason, reusing the website layout, which is not the index nor an article.

In **generate-site** function we can load a file, apply the template and save it in the output. It may look like this

    (generate "somepage.html" (load-file "data/mypage.html"))
  
This will produce the file somepage.html in the output folder

 
# How to use markdown for articles

 
Here is a tip to produce html files from markdown using emacs

1. edit the article file with emacs using ham-mode which will convert the html to markdown
2. write your text with markdown syntax 
3. save your file, ham-mode will convert it back to html
4. run *make* to update your site

The generator don't do it natively because I didn't want it to have dependencies. You can use what you want to produces the html files.

# Known limitation

The application will crash if you use a single "**~**" caracter inside one data structure in **articles.lisp** files. This is due to the format function trying to interpret the ~ symbol while we just one a ~ symbol. This symbol in the others files are automatically replaced by ~~ which produce a single ~. So, if you want to have a "~" as a title/url/author/description/short/date you have to double it. It may be interestind to sanitize it in the tool maybe.
