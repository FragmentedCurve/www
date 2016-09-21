# Introduction

cl-yag stands for Common Lisp Yet Another Generator and obviously it's written in Common Lisp. Currently, cl-yag can generate **gopher** and **html** website.

**It needs a Common Lisp interpreter and a markdown-to-html export tool (like multimarkdown).**
It is regularly tested with sbcl, clisp and ecl which are free, open-source and multi-platform. You don't need quicklisp library manager.

**This comes with a minimalistic template**, don't expect something good looking without work. You will have to write the CSS entirely and modify the html to fit your need.

As a "demo", there is [my website](https://dataswamp.org/~solene/) using cl-yag for html version, and [my gopher](gopher://perso.pw/) for gopher version.

## The hierarchy

Here are the files and folder of cl-yag :
 
+ **Makefile** : exists to simplify your life (updating, cleaning) 
+ **generator.lisp** : contains all the code of the generator
+ **templates/** : contains .tpl files which are used as template for the html or xml structure 
+ **static/** : contains the static files like images, css, js etc... that will be published
+ **data/** : 
  + **articles.lisp** : contains metadata about the website and the list of the articles with their id/title/date/tag/*author*/*short description* (fields in *italic* are not mandatory)
  + **${id}.md** : contains the article using markdown syntax that will be used when exported
+ **output/** :
  + **gopher/** : contains the exported website for gopher
  + **html/** : contains the exported website in html

# Usage

## Configuration

In data/articles.lisp there is a ***config*** variable with the following fields :

+ **:webmaster** : The name of the default author, this is the name used when **:author** is omitted
+ **:title** : The title of the webpage
+ **:description** : This text is used in the *description* field of the Atom RSS
+ **:url** : This is the full url of the blog with the final slash. If the url contains a ~ it should be doubled (e.g. : https://mydomain/~~user/ is a valid url)
+ **:rss-item-number** : This is the number of RSS items you want to published when you generate the files, it will publish the last N articles
+ **html** : t to export html website / nil to disable
+ **gopher** : t to export gopher website / nil to disable
+ **gopher-server**: hostname of the gopher server because gopher doesn't have relative links like html, so you need to know where you put your files
+ **gopher-port** : tcp port of the gopher server, 70 is the default port, it's included in every link as explained in gopher-server

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

There is a makefile, all you need to do is to type "make" in the folder, this will create the files in the **output/** location (which can be a symbolic link to somewhere else). The Gopher website will be generated inside **output/gopher** and the html will be generated in **output/html**.

**/!\ Linux users /!\ **  you should use **bmake** (bsd make) because the Makefile isn't compatible with gmake (gnu make) which is the default in Linux.

If you want to use a different lisp interpreter (default is **sbcl**), you can set the variable LISP to the name of your binary. 

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

# Known limitations

## Use of ~ character

The application will crash if you use a single "**~**" caracter inside one data structure in **articles.lisp** files. This is due to the format function trying to interpret the ~ symbol while we just one a ~ symbol. This symbol in the others files are automatically replaced by ~~ which produce a single ~. So, if you want to have a "~" as a title/url/author/description/short/date you have to double it. It may be interestind to sanitize it in the tool maybe.

## Article without tag

You can have a page without a tag associated but in the default template you will have a line under the title which will displays "Tags : " and no tags after.
