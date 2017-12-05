# CSS For cl-yag

cl-yag provides you with a default theme and a useful approach to
handle CSS style sheets as well as CSS frameworks.


## Where The Style Sheets Live

All of cl-yag's style sheets are located in **static/css/**.  
Currently there are the following files:

	css/
	|-- clym.css
	|-- custom.css
	|-- pure_r1.0.0/
	|   |-- LICENSE.md
	|   `-- pure.css
	`-- style.css


## style.css -- One Sheet To Rule Them All

In order to keep it simple cl-yag uses **static/css/style.css** to
administrate all of its style sheets. Use the ``@import`` rule to
include your own, or comments to get rid of what is already there -
but mind the [cascade](https://www.w3.org/TR/css-cascade-3/ "W3C: CSS Cascading and Inheritance Level 3").

Currently, **style.css** looks like this:

	/* =================================================================== */
                           /* style.css for cl-yag */
	/* =================================================================== */
	@charset "utf-8";


	/* ~                             PURE                                ~ */
	@import url("pure_r1.0.0/pure.css");


	/* ~                             CLYM                                ~ */
	@import url("clym.css");


	/* ~                          LAST ENTRY                             ~ */
	/* ~             use custom.css for overriding rules                 ~ */
	@import url("custom.css");



## Pure -- "A Set Of Small, Responsive CSS Modules"

cl-yag uses a style sheet called **pure.css**, taken from
[Pure](https://purecss.io/ "purecss.io"), a minimal, BSD licensed CSS
framework, to provide a set of expected UI features, among which are
sane resets (such as
[normalize.css](https://necolas.github.io/normalize.css/
"Normalize.css - A modern, HTML5-ready alternative to CSS resets")'s
reset rules) and usable menus.

To deactivate Pure, put the *PURE.CSS* ``@import`` rule in
**static/css/style.css** in comments and re-run ``make``.


## clym -- A Default Theme

Additionally, cl-yag comes with its first theme: *clym*.

*clym* stands for *cl-yag minimal*. It is a set of css rules designed to
work with cl-yag's html skeleton. It provides an unobtrusive color
scheme, basic typography, as well as basic responsiveness. You'll find
it in **static/css/clym.css**.

**clym.css** doesn't provide neither css resets nor a menu layout. This
is handled by [Pure](https://purecss.io/ "purecss.io")'s
**pure.css**. Further, it doesn't need any JavaScript.

If you don't like *clym*, put comments around the line ``@import
url("clym.css");`` in **static/css/style.css**, around all pure-rules
in **static/css/custom.css**, and re-run ``make``.


## **custom.css**

For information about **custom.css** you need to read it, as well as
the following section "Working With Style Sheets".


## Working With Style Sheets

### Current Styles And Minor Tweaks

If you are already using a combination of style sheets and you need to
adjust some parts of the layout, use cl-yag's
**static/css/custom.css**. It is currently used to override Pure's
default layout for horizontal menus with *clym*'s colorscheme.

#### MIND

- In order to override rules located in all previous(!) style sheets
**custom.css** needs to get sourced in as the last(!)  file in
**static/css/style.css** (see section: "style.css – One Sheet To Rule Them
All").
- Respect the [cascade](https://www.w3.org/TR/css-cascade-3/ "W3C: CSS Cascading and Inheritance Level 3") :-).


### Frameworks

CSS frameworks provide an easy way to create your own theme. To make use
of a framework's rulesets,

- its style sheets need to get included via your **static/css/style.css**,
- their ids and classes need to get wired into cl-yag's template files and
- the template files need to get used by cl-yag's **generator.lisp**.

So you need to edit cl-yag's template files in **templates/** and - in
case you've choosen to rename your templates - you need to adjust their
corresponding paths and filenames in **generator.lisp**.






