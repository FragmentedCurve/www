# CSS For cl-yag

cl-yag comes with a default css-stylesheet - *clym* - and a useful
approach to administrate, tweak and test your stylesheets. *clym*
doesn't use javascript and provides *nil* javascript-features (such as
dropdown-menus).


## Where The Stylesheets Live

All of cl-yag's style sheets are located in **static/css/**. Of course
you can change that, but we recommend sticking to it. It pays to know
where your stuff is. Currently there are the following files:

	css/
	|-- clym.css
	|-- custom.css
	|-- pure_r1.0.0/
	|   |-- LICENSE.md
	|   `-- pure.css
	`-- style.css


## style.css -- One Sheet To Rule Them All

In order to keep it simple cl-yag uses **data/css/style.css** to
administrate all of its stylesheets. Use the ``@import`` rule to include
your own, or comments to get rid of what is already there - but mind the
[cascade](https://www.w3.org/TR/css-cascade-3/ "W3C: CSS Cascading and Inheritance Level 3").

Currently, **style.css** looks like this:

	/* =================================================================== */
                           /* style.css for cl-yag */
	/* =================================================================== */
	@charset "utf-8";


	/* ~                           PURE.CSS                              ~ */
	@import url("pure_r1.0.0/pure.css");


	/* ~                     Stylesheet for cl-yag                       ~ */
	@import url("clym.css");


	/* ~                             LAST ENTRY                          ~ */
	/* ~                 custom.css to override styles.                  ~ */
	@import url("custom.css");


## Pure - "A Set Of Small, Responsive CSS Modules"

cl-yag uses [Pure](https://purecss.io/ "purecss.io"), a minimal, BSD licensed css
framework. It employs the style sheet **pure.css** to provide a set of
expected features among which are usable menus and sane
resets. **pure.css** incorporates
[normalize.css](https://necolas.github.io/normalize.css/
"Normalize.css - A modern, HTML5-ready alternative to CSS resets")'s
reset rules.

To see the effects of **pure.css** uncomment the *PURE.CSS* ``@import``
rule in **static/css/style.css** and re-run ``make``.


## clym -- A Default Theme

Additionally, cl-yag comes with its first theme: *clym*.

*clym* stands for *cl-yag minimal*. It is a set of css rules designed to
work with cl-yags specific skeleton. It provides an unobtrusive color
scheme, basic typography, as well as basic responsiveness. You'll find
it in **static/css/clym.css**.

*clym* doesn't provide css-resets and menu-layouts. That's where
[Pure](https://purecss.io/ "purecss.io") steps in and does a
magnificient job.

If you don't like *clym*, put the following line in **data/css/style.css**
in comments to deactivate it:
	  
	@import url("clym.css"); 			


## **custom.css**

For information about **custom.css** read the following section "Working
With Stylesheets".

## Working With Stylesheets

Before you start working, make copies of cl-yags default layout files
and/or use a version control system, e.g. [git](https://git-scm.com/ "git - the free and open source distributed version control system").

### Current Styles And Minor Tweaks

If you are already using a combination of stylesheets but need to adjust
some parts of the layout, use cl-yag's **static/css/custom.css**. It is
currently used to override pure's default layout for horizontal menus
with *clym*'s colorscheme , so you already have a working example of
howto use **custom.css**.

#### MIND

- In order to override rules located in all previous(!) style sheets
**custom.css** needs(!) to get sourced in as the last(!)  file(!)  in
**data/css/style.css** (see section: "style.css – One Sheet To Rule Them
All").
- Respect the [cascade](https://www.w3.org/TR/css-cascade-3/ "W3C: CSS Cascading and Inheritance Level 3") :-).

### Frameworks

CSS frameworks provide an easy way to create your own full-of-features
theme. To make use of a framework's rulesets,

- their ids and classes need to get wired into cl-yag's html-skeleton and
- the skeleton needs to get used by cl-yag's **generator.lisp**.

So you need to edit cl-yag's template-files in **templates/** and -
depending on the scale of work and your approach - **generator.lisp** as
well.

#### **templates/**

To get more information about templates, read them :-).


#### **generator.lisp**

In case you've choosen to rename your template-files, you need to adjust
their corresponding paths and filenames in **generator.lisp** as well.








