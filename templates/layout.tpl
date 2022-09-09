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
	    <li class="pure-menu-item"><a href="index.html" class="pure-menu-link"><img src="static/img/Action_GoHome.svg"></a></li>
	    <li class="pure-menu-item"><a onclick="toggleBar('tag-bar')" class="pure-menu-link"><img src="static/img/App_Magnify.svg"></a></li>
	    <li class="pure-menu-item"><a onclick="toggleBar('donate-bar')"  class="pure-menu-link"><img src="static/img/App_Finance.svg"></a></li>
	    <li class="pure-menu-item"><a onclick="toggleBar('contact-form')" class="pure-menu-link"><img src="static/img/App_Mail_None.svg"></a></li>
	    <!-- <li class="pure-menu-item"><a href="links.html" class="pure-menu-link"><img src="static/img/App_People.svg"></a></li> -->
	    <li class="pure-menu-item"><a href="" class="pure-menu-link"><img src="static/img/Alert_Idea.svg"></a></li>
	    <li class="pure-menu-item"><a href="rss.xml" class="pure-menu-link"><img src="static/img/File_RSS_Feed.svg"></a></li>
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
	    <div class="donatecontainer">
	      <div class="donateinfo">
		<h3>
		  <img src="static/img/xmr.svg">
		  Monero
		</h3>
		<a href="static/img/wallet_xmr.png" class="pure-menu-link">
		  <img src="static/img/wallet_xmr.png">
		</a>
		<code>48ytRrWEvk5GzkxtLHDr6na5qm6oqbYRqKkspeaa4ypHcY4SFMDw474eSYKJXfUAQSAHLF3GoTKzqKccUYDQPSrg9RboArW</code>
	      </div>

	      <div class="donateinfo">
		<h3><img src="static/img/btc.svg">Bitcoin</h3>
		<a href="static/img/wallet_btc.png" class="pure-menu-link">
		  <img src="static/img/wallet_btc.png">
		</a>
		<code>bc1qemzsdmtnlgxm9z6hghmg835dzqxq6f3zw0dyah</code>
	      </div>

	      <div class="donateinfo">
		<a href="" class="pure-menu-link">
		  <img src="static/img/kofi.svg">
		</a>
	      </div>
	      
	      <div class="donateinfo">
		<a href="" class="pure-menu-link">
		  <img src="static/img/patreon.svg">
		</a>
	      </div>

	    </div>
	    <!--
	    <ul class="pure-menu-list pure-menu-horizontal" style="width:50%;margin:auto;">
	      <li class="pure-menu-item"><a href="" class="pure-menu-link"><img src="static/img/kofi.svg"> Kofi</a></li>
	      <li class="pure-menu-item"><a href="" class="pure-menu-link"><img src="static/img/patreon.svg"> Patreon</a></li>
	    </ul>
	    -->
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
	    
	    <script>
	      let errorParams = new URLSearchParams(window.location.search);
	      
	      let input_elements = [
		  { default: "name",    error: "error-name",    element: document.getElementById('input-name')    },
		  { default: "email",   error: "error-email",   element: document.getElementById('input-email')   },
		  { default: "subject", error: "error-subject", element: document.getElementById('input-subject') },
		  { default: "message", error: "error-message", element: document.getElementById('input-message') },

		  // Referral data
		  { default: "referral-src",   error: null, element: document.getElementById('input-referer') },
		  { default: "referral-title", error: null, element: document.getElementById('page-title')    }
	      ];

	      for (let i = 0; i < input_elements.length; i++) {
		  if (errorParams.has(input_elements[i].default)) {
		      try {
			  switch (input_elements[i].element.tagName) {
			  case 'HIDDEN': // Fall through
			  case 'INPUT':
			      input_elements[i].element.setAttribute('value', errorParams.get(input_elements[i].default));
			      break;
			  case 'H3': // Fall through
			  case 'TEXTAREA':
			      input_elements[i].element.innerHTML = errorParams.get(input_elements[i].default);
			      break;
			  case 'SELECT':
			      input_elements[i].element.getElementsByTagName('option')[errorParams.get(input_elements[i].default)].selected = 'selected';
			      break;
			  }
		      } catch (err) {
			  // Passively report the error and move on.
			  console.log(err);
		      }
			
		    }

		    if (errorParams.has(input_elements[i].error)) {
			input_elements[i].element.setAttribute('class', 'error-box');
		    }
		}

		function toggleBar(id) {
		    let e = document.getElementById(id);
		    if (e.getAttribute('style') == 'display:none') {
			e.setAttribute('style', 'display:block');
		    } else {
			e.setAttribute('style', 'display:none');
		    }
		}
	      </script>
	    </div>
	</div>
	
	%%Body%%
      </main>
      
      <footer>
	<a href="#"><img src="static/img/Action_GoUp.svg"></a>
      </footer>

    </div><!-- #wrapper -->

    <script id="MathJax-script" async
	    src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js">
    </script>
  </body>
</html>
