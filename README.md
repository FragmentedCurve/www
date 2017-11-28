# README


## Introduction

cl-yag is a very lightweight, 'static site'-generator that produces **gopher** sites as well as **html** websites.
The name 'cl-yag' stands for 'Common Lisp - Yet Another website Generator'.
It runs without Quicklisp.


## Showcase

I am using cl-yag to create and maintain my websites in the
world-wide-web (visit: *[Solene'spercent](https://dataswamp.org/~solene/)*) 
as well as [in gopher-space](gopher://dataswamp.org/1/~solene/).


## Requirements

To use cl-yag you'll need:

1. A Common Lisp Interpreter
    - cl-yag's current default is **Steel Bank Common Lisp (SBCL)**.
    - **Embeddable Common Lisp (ECL)** will do fine as well.
2. A Markdown-to-HTML Converter
    - cl-yag's current default is **multimarkdown**.
3. BSD Make
    - Linux-Users, cl-yag uses a BSD Makefile syntax, that isn't compatible with GNU make's.
    - You need to install a port of the NetBSD make tool, called **bmake**.


## Usage

Go into your project's directory and type ``make``. You'll find your new website/gopher page in 'output/'.
If you want to get rid of everything in your 'output/' subdirectories,
type ``make clean``.
For further commands: read the Makefile.
Read in the follwing section where to find it.


## Overview: cl-yag's File Hierarchy

After cloning the repository, your project's directory should contain at
least the following files and folders:

.
    |-- LICENSE
    |-- Makefile
    |-- README.md
    |-- data/
    |   |-- 1.md
    |   |-- README.md
    |   `-- articles.lisp
    |-- generator.lisp
    |-- output/
    |   |-- gopher/
    |   `-- html/
    |-- static/
    |   |-- css/style.css
    |   `-- img/
    `-- templates/
        |-- article.tpl
        |-- gopher_head.tpl
        |-- layout.tpl
        |-- one-tag.tpl
        |-- rss-item.tpl
        `-- rss.tpl

- **Makefile**
    - This file exists to simplifiy the recurring execution of frequently used commands.
- **generator.lisp**
    - This is cl-yag's core library.
- **static/**
    - This directory holds content, that needs to be published without being changed (e.g. stylesheets, js-scripts).
    - If you come from 'non-static CMS'-Country: 'static/' holds, what you would put in your 'assets/' directory.
- **templates/**
    - The templates in this directory provide the structural skeleton(s) of the webpages and feeds you want to create.
- **output/**
    - cl-yag puts in this directory everything ready to get deployed.
    - Because cl-yag generates not only HTML, but gopher-compliant pages as well, output/ **holds two subdirectories**.
        - **gopher/** : contains the website for gopher,
        - **html/** : contains the website in HTML.

And there is the **data/** directory, which is important enough to get a subsubsection of its own.

### The 'data/' Directory

This directory is crucial for the usage of cl-yag.

**data/** contains

- the **articles.lisp configuration file**, which defines important metadata for posts and pages.
- It also holds **${id}.md**-files, which are holding your posts' and pages' content. You can use markdown to write them.

For more information: Read section 'Configuration'.


## Configuration

cl-yag's main configuration file is **data/articles.lisp**.  
In order to have a reliably running implementation of cl-yag, you have
to set most of the values in this file.

**data/articles.lisp** has two parts:

1. A variable called **config**. It defines global values, that define your webpage.
2. A variable called **articles**. It defines local values, that - in turn - define individual pages/posts.

Values are assigned by placing a string (e.g. "foo") or a boolean
(i.e. 't' or 'nil') behind a keyword (e.g. ':title').


### The **config** Variable

The **config** variable is used to assign the following values:

- **:webmaster**
    - The name of the default(!) author. 
    - :webmaster gets used, if **:author** is omitted. (see below: 'The **articles** variable'.)
- **:title**
    - The title of the webpage
- **:description**
    - This text is used in the *description* field of the Atom RSS
- **:url**
    - This needs to be the full(!) URL of your website, including(!) a final slash.
    - MIND: If the url contains a tilde (~), it needs to get duplicated
    - Example: https://mydomain/~~user/ is a valid url.
- **:rss-item-number**
    - This holds the number of latest(!) RSS items you want to get published when you generate the files.
- **html**
    - *t* to export html website. Set *nil* to disable.
- **gopher**
    - *t* to export gopher website. Set *nil* to disable.
- **gopher-path**
    - This is the full path of the directory to access your gopher hole.
- **gopher-server**
    - Hostname of the gopher server. Because gopher doesn't allow relative links (like html), you need to know where you put your files.
- **gopher-port**
    - tcp port of the gopher server. 70 is the default port. It need to be included in every link (see: **gopher-server**).


### The **articles** Variable

The **articles** variable holds per page/post-metadata.
Of the following fields, only the *:author* and *:short* description could be omitted.

- **:short**
    - The _:short_ field's value is used for displaying a really short description of the posts content on your homepage.
    - If _:short_ doesn't get a value, the full article gets displayed.
    - Hint: Use ``:short "view the article for the full text"``, if you don't want to display the full text of an article on your index site.
- **:id_**
    - The _:id_ field holds the filename of your post/page.
    - Example: ``:id "2"`` will load file ``data/2.md``. Use text instead of numbers, if you want to.
    - (See section: 'The **data/** Directory'.)
- **:author**
    - The _:author_ field is used to display the article' author.
    -  If you omit it, the generator will take the name from the **:webmaster** field of the *config* variable.
- **:tag**
    - _:tag_ field is used to create a "view" containing all articles of the same tag.
    -  MIND: Whitespaces are not allowed in(!) tags.


## Howto Create A New Post

Edit data/articles.lisp and add a new list to the *articles* variable:

    (list :title "How do I use cl-yag"
          :id "2"
          :date "29 April 2016"
          :author "Solène"
          :short "I will explain how to use the generator"
          :tag "example help code")

Then write a corresponding ``2.md`` file, using markdown.

## Howto Publish A Post

I prepared a Makefile to facilitate the process of generating and
publishing your static sites.

All you need to do in order to publish is to go into your cl-yag
directory and type "make".

The 'make' command does create html and gopher files in the defined
**output/** location (which can be a symbolic link pointing to some
other directory, somewhere else on your machine).


## Howto Add A New Page

You may want to have some dedicated pages besides the index or a post.
To create one, edit the **generate-site** function in cl-yag's
generator.lisp and add a function call, like this:

    (generate "somepage.html" (load-file "data/mypage.html"))

This will produce the file **somepage.html** in the output folder.


## Further Customization

### Howto Use Another Common Lisp Interpreter

cl-yags default Lisp interpreter is **sbcl**.
If you want to use a different lisp interpreter you need to set the
variable 'LISP' to the name of your binary, when calling ``make``.

    make LISP=ecl


### Using git Hooks For Publishing

You may customize your publishing-process further, e.g. by using a git
hook to call 'make' after each change in the repo so your website gets
updated automatically.


## Page-Includes

Here is an example code, if you want to include another page in the template:

1. Create **template/panel.tpl** with the html you want to include.
2. Add a string in the target file, where the replacement should occur.
   In this case, we choose **%%Panel%%** for a string, and, because we want the panel to be displayed on each page, we add this string to **template/layout.tpl**.

3. Modify the function *generate-layout* in cl-yag's **generator.lisp** accordingly.
   This is done by adding the following template function call:

    (template "%%Panel%%" (load-file "template/panel.tpl"))

(Note: You can insert your text directly into the layout template file
as well.)


## Known Limitations

### Use ~~ To Create ~

cl-yag crashes if you use a single "**~**" caracter inside one data
structure in **articles.lisp** files, because Common Lisp employs the
tilde as a prefix to indicate format specifiers in format strings.

In order to use a literal `~` - e.g. for creating a :title or :url
reference - you have to **escape** the tilde **by duplicating** it:
``~~``.
(See _:url_ in section 'Configuration').


### Posting Without Tagging

cl-yag allows posts to be 'untagged'- but with the default template
you'll get a line below your title that displays: "Tags: ".

(Note: If you are looking for a way to contribute this may be a task for you.)


### A Note On Themes

Although cl-yag **may** ship with a **minimalistic** template, cl-yag
focuses only on generating html- and gopher-compliant structural
markup - not themed layouts.

If you want some deeply refined, cross-browser compatible, responsive,
webscale style-sheet, you need to create it yourself.
However, cl-yag will work nicely with it and if you want to make your
stylesheets a part of cl-yag you're very welcome to contact me.


# Hacking cl-yag

I tried to make cl-yag easy to extend.
If you want to contribute, feel free to contact me and/or to send in a patch.

- If you are looking for a way to contribute:
    - You could find a way to "sanitize" cl-yag's behaviour regarding the tilde (see: above);
    - Also see: 'Note' in 'Posting Without Tagging';
    - Also see: 'A Note On Themes.
