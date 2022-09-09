<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no" />
    <meta name="generator" content="cl-yag" />
    <title>%%Title%%</title>
    <link rel="stylesheet" type="text/css" href="static/css/style.css" media="screen" />
    <link rel="alternate" type="application/rss+xml" title="%%Title%% RSS Feed" href="rss.xml" />
    <!--    <link rel="alternate" type="application/rss+xml" title="%%Title%% RSS Feed Gopher" href="rss-gopher.xml" /> -->
    <!--    <link rel="alternate" type="application/atom+xml" title="%%Title%% Atom Feed" href="atom.xml" /> -->
    <link rel="icon" type="image/x-icon" href="static/img/favicon.ico" />
  </head>

  <body>
    <div id="wrapper">
      <header>
	<nav class="pure-menu-horizontal">
	  <ul class="pure-menu-list">
	    <li class="pure-menu-item"><a href="index.html" class="pure-menu-link"><img alt src="static/img/Action_GoHome.svg"></a></li>
	    <li class="pure-menu-item"><a onclick="toggleBar('tag-bar')" class="pure-menu-link"><img alt src="static/img/App_Magnify.svg"></a></li>
	    <li class="pure-menu-item"><a onclick="toggleBar('donate-bar')"  class="pure-menu-link"><img alt src="static/img/App_Finance.svg"></a></li>
	    <li class="pure-menu-item"><a onclick="toggleBar('contact-form')" class="pure-menu-link"><img alt src="static/img/App_Mail_None.svg"></a></li>
	    <!-- <li class="pure-menu-item"><a href="links.html" class="pure-menu-link"><img alt src="static/img/App_People.svg"></a></li> -->
	    <li class="pure-menu-item"><a onclick="toggleBar('guide-bar')" class="pure-menu-link"><img alt src="static/img/Alert_Idea.svg"></a></li>
	    <li class="pure-menu-item"><a href="rss.xml" class="pure-menu-link"><img alt src="static/img/File_RSS_Feed.svg"></a></li>
	  </ul>
	</nav>
      </header>
      
      <main>

	<div id="tag-bar" class="toggle-bar-container" style="display:none">
	  <div class="toggle-bar-header">
	    Tags
	  </div>
	  
	  <div class="toggle-bar">
	    %%Tags%%
	  </div>
	</div>
	
	<div id="donate-bar" class="toggle-bar-container" style="display:none">
	  <div class="toggle-bar-header">
	    Donations
	  </div>
	  
	  <div class="toggle-bar">
	    <div class="toggle-bar-stack fill">
	      <div class="toggle-bar-frame">
		<h1><img alt src="static/img/xmr.svg"> Monero</h1>
		<mono>48ytRrWEvk5GzkxtLHDr6na5qm6oqbYRqKkspeaa4ypHcY4SFMDw474eSYKJXfUAQSAHLF3GoTKzqKccUYDQPSrg9RboArW</mono>
	      </div>

	      <div class="toggle-bar-frame">
		<h1><img alt src="static/img/btc.svg"> Bitcoin</h1>
		<mono>bc1qemzsdmtnlgxm9z6hghmg835dzqxq6f3zw0dyah</mono>
	      </div>

	      <div class="toggle-bar-frame">
		<h1><img alt src="static/img/kofi.svg"> Kofi</h1>
		Coming soon
	      </div>
	      
	      <div class="toggle-bar-frame">
		<h1><img alt src="static/img/patreon.svg"> Patreon</h1>
		Coming soon
	      </div>
	    </div>
	  </div>
	</div>
	
	<div id="contact-form" class="toggle-bar-container" style="display:none">
	  <div class="toggle-bar-header">
	    Send Mail
	  </div>
	  
	  <div class="toggle-bar">
	    <form method="post" action="http://localhost:9000/contact.cgi">
	      <div style="display:block;width:75%;margin:auto">
		<input type="text" id="input-name" name="name" placeholder="Your name..." style="display:block;width:100%;margin-bottom: 10px;padding:5px;">
		<input type="email" id="input-email" name="email" placeholder="Your email..." style="display:block;width:100%;margin-bottom:10px;padding:5px;">
		<select id="input-subject" name="subject" style="display:block;width:60%;margin-bottom:10px;padding:5px;">
		  <option value="0" selected="selected">General Inquires & Questions</option>
		  <option value="1">Content Correction</option>
		  <option value="2">Software Bug</option>
		</select>
		<textarea id="input-message" name="message" style="display:block;width:100%;max-width:100%;margin-bottom:10px;height:100px;padding:15px;"></textarea>
		<input type="hidden" id="input-referer" name="referer" value="%%Self%%">
		<button style="display:block;width:50%;height:50px;">Send</button>
	      </div>
	    </form>
	  </div>
	</div>

	
	<div id="guide-bar" class="toggle-bar-container" style="display:none">
	  <div class="toggle-bar-header">
	    Guide
	  </div>
	  <div class="toggle-bar">
	    This website is a work in progress.
	  </div>
	</div>
	
	%%Body%%
      </main>
      
      <footer>
	<a href="#"><img alt src="static/img/Action_GoUp.svg"></a>
      </footer>

    </div><!-- #wrapper -->

    <script id="MathJax-script" async
	    src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js">
    </script>

    <script src="static/js/code.js"></script>
  </body>
</html>
