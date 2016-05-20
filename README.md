# Introduction

cl-yag stands for Common Lisp Yet Another Generator and obviously it's written in Common Lisp.

It has only one dependency : a common lisp interpreter. It is regularly tested with sbcl, clisp and ecl which are free, open-source and multi-platform. You don't need quicklisp library manager.

**This comes with a minimalistic template**, don't expect something good looking without work. You will have to write the CSS entirely and modify the html to fit your need.

## The hierarchy

Here are the files and folder of cl-yag :
 
+ **Makefile** : exists to simplify your life (updating, cleaning) 
+ **generator.lisp** : contains all the code of the generator
+ **templates/** : contains .tpl files which are used as template for the html or xml structure 
+ **static/** : contains the static files like images, css, js etc... that will be published
+ **data/** : 
  + **articles.lisp** : contains metadata about the website and the list of the articles with their id/title/date/tag/*author*/*short description* (fields in *italic* are not mandatory)
  + **${id}.txt** : contains the html text of the article ${id} that will be used when displayed
+ **output** : folder where the website is generated when your run *make*. It is cleaned when you run *make clean*; You can make it a symbolic link to the web server folder.

# Usage

## How to add an article
 
Edit data/articles.lisp and add a new line inside the *articles* variable like this (you can do it in one line, as you prefer)

    (list :title "How do I use cl-yag" 
	      :id "2" :date "29 April 2016" 
	      :author "Solène" 
		  :short "I will explain how to use the generator" 
		  :tag "example help code")

The _:short_ field is used on the homepage. It it is defined, this is the text that will be shown on the homepage with all the others articles. If it's not defined, the whole article content will be used on the homepage. Sometimes when you have long articles, you may not want to display it entirely on the index so you can use _:short "view the article for the full text_.

The _:id_ field will be part of the filename of the file and it's also the name of the content on the disk. `:id "2"` will load file `data/2.txt`, you can use text instead of numbers if you want.

The _:author_ field is used to display who wrote the article. You can omitt it, the generator will take the name from the *config* variable

The _:tag_ field is used to create a page with all the articles with the same tag. Tags can't contain spaces.

## How to publish

There is a makefile, all you need to do is to type "make" in the folder, this will create the files in the **output/** location (which can be a symbolic link to somewhere else). If you want to use a different lisp interpreter (default is **sbcl**), you can set the variable LISP to the name of your binary. 

Example with clisp : 

`make LISP=clisp`

This way, you can easily use a git hook to type make after each change in the repo so your website is automatically updated.

# Some hacks you can do

I tried to make it "hacking friendly", you can extend if easily. If you have any idea, feel free to contact me or to send patches.

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
  
This will produce the file **somepage.html** in the output folder.

 
# How to use markdown for articles

 
Here is a tip to produce html files from markdown using emacs

1. edit the article file with emacs using ham-mode which will convert the html to markdown
2. write your text with markdown syntax 
3. save your file, ham-mode will convert it back to html
4. run *make* to update your site

The generator don't do it natively because I didn't want it to have dependencies. You can use what you want to produces the html files.

# Known limitations

## Use of ~ character

The application will crash if you use a single "**~**" caracter inside one data structure in **articles.lisp** files. This is due to the format function trying to interpret the ~ symbol while we just one a ~ symbol. This symbol in the others files are automatically replaced by ~~ which produce a single ~. So, if you want to have a "~" as a title/url/author/description/short/date you have to double it. It may be interestind to sanitize it in the tool maybe.

## Article without tag

You can have a page without a tag associated but in the default template you will have a line under the title which will displays "Tags : " and no tags after.
