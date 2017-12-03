# CSS For cl-yag

Well, now cl-yag comes with a default css-stylesheet.

## Where The Stylesheets Live

All of cl-yag's style sheets are located in **css/**. Of course you
can change that, but we recommend sticking to it. Currently there are
the following files:

	css/
	|-- clym.css
	|-- custom.css
	`-- style.css


## style.css -- One Sheet To Rule Them All

In order to keep it simple cl-yag uses **data/css/style.css** to
administrate all of its stylesheets. Use the ``@import`` rule to include
your own, or comments to get rid of what is already there.

Currently, it looks like this:

	/* ==================================================================== */
                           /* style.css for cl-yag */
	/* ==================================================================== */
	@charset "utf-8";


	/* ~                           PURE.CSS                               ~ */
	@import url("pure_r1.0.0/pure.css");
	@import url("pure_r1.0.0/grids.css");
	@import url("pure_r1.0.0/grids-responsive.css");


	/* ~                     Stylesheet for cl-yag                         ~ */
	@import url("clym.css");


	/* ~                             LAST ENTRY                            ~ */
	/* ~                 custom.css to override styles.                    ~ */
	@import url("custom.css");


## Pure.css

cl-yag uses stylesheets from [pure.css](https://purecss.io/ "Pure.css"),
"a set of small, responsive CSS modules", as a minimal css framework: It
employs **pure.css**, **grids.css** and **grids-responsive.css** to
provide a limited set of expected features, e.g. usable menus and basic
responsiveness.

Additionally, cl-yag puts its first, own stylesheet as a topping on
pure's magic: clym. 

## clym.css -- A Default Stylesheet

*clym* stands for *cl-yag minimal* and is a set af rules designed to
work with cl-yags specific skeleton. It also provides an unbtrusive
color scheme and some basic typography. You'll find it in
**static/css/clym.css**.

If you don't like it, deactivate it by uncommenting the following line
in **data/css/style.css**:
	  
	@import url("clym.css"); 			


## custom.css -- Tweak Your Style

cl-yag provides you with **data/css/custom.css** to tweak your styles.
In order to override rules located in all previous style sheets it needs
to get sourced in as the last file in **data/css/style.css**.




