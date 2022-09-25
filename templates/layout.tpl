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
	    <li class="pure-menu-item"><a onclick="toggleBar('contact-bar')" class="pure-menu-link"><img alt src="static/img/App_Mail_None.svg"></a></li>
	    <!-- <li class="pure-menu-item"><a href="links.html" class="pure-menu-link"><img alt src="static/img/App_People.svg"></a></li> -->
	    <li class="pure-menu-item"><a onclick="toggleBar('guide-bar')" class="pure-menu-link"><img alt src="static/img/Alert_Idea.svg"></a></li>
	    <li class="pure-menu-item"><a href="rss.xml" class="pure-menu-link"><img alt src="static/img/File_RSS_Feed.svg"></a></li>
	  </ul>
	</nav>
      </header>

      <main>
	<window id="tag-bar" class="window" style="display:none">
	  <titlebar class="titlebar">
	    Tags
	  </titlebar>

	  <window-box class="toggle-bar">
	    %%Tags%%
	  </window-box>
	</window>

	<window id="donate-bar" style="display:none">
	  <titlebar>
	    Donations
	  </titlebar>

	  <window-box>
	    <div class="flex vstack">
	      <div class="frame">
		<h1><img alt src="static/img/xmr.svg"> Monero</h1>
		<mono>48ytRrWEvk5GzkxtLHDr6na5qm6oqbYRqKkspeaa4ypHcY4SFMDw474eSYKJXfUAQSAHLF3GoTKzqKccUYDQPSrg9RboArW</mono>
	      </div>

	      <div class="frame">
		<h1><img alt src="static/img/btc.svg"> Bitcoin</h1>
		<mono>bc1qemzsdmtnlgxm9z6hghmg835dzqxq6f3zw0dyah</mono>
	      </div>

	      <div class="frame">
		<h1><img alt src="static/img/kofi.svg"> Kofi</h1>
		Coming soon
	      </div>

	      <div class="frame">
		<h1><img alt src="static/img/patreon.svg"> Patreon</h1>
		Coming soon
	      </div>
	    </div>
	  </window-box>
	</window>

	<window id="contact-bar" style="display:none">
	  <titlebar>
	    Send Mail
	  </titlebar>

	  <window-box>
	    <p><b>This feature is currently disabled.</b></p>
	    <form method="get" action="#">
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
	  </window-box>
	</window>

	<window id="guide-bar" style="display:none">
	  <titlebar>
	    Guide
	  </titlebar>

	  <window-box>
	    <div class="flex vstack">
	      <div class="frame">
		<h1>About</h1>

		<img src="static/img/profile.jpg" style="width: 40%; min-width: 100px; float: left; padding: 1em">

		<p>
		  I almost bought a pipe online today. But then my inner voice yelled back, "No dammit!"
		</p>

		<p>
		  That little man between my ears was right. I can't buy a tobacco pipe online. I need to go into a tobacco shop where there awaits a dirty old man. He waits to show me his domain, expressing the aesthetics of each pipe. The flavor of each tobacco. We'll climb around his tiny shop. And as we walk down each aisle, little bits of dried grease will be pulled by the wind from his hair. He'll tell a rude joke. And I'll point to the pipe and say, "That's the one."
		</p>

		<p>
		  That's how. That's how I'll buy a pipe.
		</p>
	      </div>

	      <div class="frame">
		<h1>Format Guide</h1>

		<h2>Navigation</h2>

		<div class="flex hstack --center --sp6">
		  <p style="text-align: center;"><img src="static/img/Action_GoHome.svg"><br>Go home to the full index.</p>
		  <p style="text-align: center;"><img src="static/img/App_Magnify.svg"><br>List all site tags.</p>
		  <p style="text-align: center;"><img src="static/img/App_Finance.svg"><br>Donations.</p>
		  <p style="text-align: center;"><img src="static/img/App_Mail_None.svg"><br>Send me a message.</p>
		  <p style="text-align: center;"><img src="static/img/Alert_Idea.svg"><br>Show this guide.</p>
		  <p style="text-align: center;"><img src="static/img/File_RSS_Feed.svg"><br>RSS Feed.</p>
		</div>

		<h2>Text</h2>

		<p>Text that looks like this is plan language.</p>

		<p><code>Text like this refers to valid syntax from code, an operating system, or file system.</code></p>

		<p><i>This implies a title or term that references a concept.</i></p>

		<p>$$This is mathematics. \sum^{n}_{i=1}{i}$$</p>

		<h2>Code Blocks</h2>
		<div class="frame" style="background: white;">
		  <h1></h1>
		  <pre class="example">Generic monospaced text that's not source code such as terminal IO or a hex dump.</pre>

		  <pre class="src"> Source code that can be copied into a file and compiled or executed.</pre>

		  <pre class="src src-input">Input for a terminal shell. Can be copied and pasted into a terminal given you're running the correct shell.</pre>

		  <pre class="src src-output">Terminal output from a command.</pre>

		  <pre class="src src-termio">A mixture of user input and output from a terminal. It can't be naively copied into your terminal.</pre>
		</div>
	      </div>
	    </div>

	    <div class="flex vstack">
	      <div class="frame">
		<h1>Credits</h1>
		<ul>
		  <li><a href="https://dataswamp.org/%7Esolene/index.html">Solène Rapenne</a> for the static site generator.</li>
		  <li><a href="https://github.com/ItsJonQ/hstack-vstack-css">ItsJonQ</a> for CSS stacking classes.</li>
		  <li><a href="https://github.com/darealshinji/haiku-icons">darealshinji</a> for the icons used throughout the site.</li>
		</ul>
	      </div>
	    </div>
	  </window-box>
	</window>

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
