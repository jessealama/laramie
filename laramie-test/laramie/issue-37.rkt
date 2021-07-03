#lang racket/base

(require laramie)

(module+ test
  (require rackunit))

(define document #<<DOC
<!DOCTYPE html>
<html>
<head>

	<meta http-equiv="content-type" content="text/html; charset=utf-8" />
	<title></title>


	<link rel="stylesheet" href="/stylesheets/main.css?r=32" />
	<link rel="stylesheet" href="/stylesheets/autocomplete.css" />
  </head>
<body id="pinboard" onload="if (init != null) { init(); }">
<script language="javascript">
  var initHandlers = Array();
  function registerInitHandler(cb)
  {
    initHandlers.push(cb);
  }

  function init()
  {
    for (var i in initHandlers)
    {
      var cb = initHandlers[i];
      cb();
    }
  }
</script>
<div id="content">
    <div id="banner">
        <div id="logo" >
        <a href="/recent">
        <img src="/bluepin.gif" class="pin_logo"/>
        </a>


          <a id="pinboard_name" href="/">Pinboard</a>

                                <div id="timer"></div>
                   </div>

        <div id="top_menu">
 <a href="/recent/">recent</a> ‧ <a href="/popular/">popular</a>  ‧ <a href="/tour/">tour</a>  ‧ <a href="/howto/">howto</a> &nbsp;&nbsp;&nbsp;&nbsp;<a href="/">log in</a>  </div> <!-- END BANNER -->

  <div style="clear:both"></div>
</div>

         <style>

#main_welcome
{
    width:380px;
    height:480px;
    float:left;

}
#main_right_column
{
    margin-left:40px;
    width:345px;
    float:left;
    height:380px;

}
.homepage_heading
{
    font-size:2.3em;
    margin-bottom:0px;
    color:#555;
    font-family:helvetica;
    font-weight:normal;
}

.sign_in_heading
{
    color:#fff;
    font-size:1.2em;
    margin-top:30px;
}

.homepage_subheading
{
    font-weight:normal;
    color:#8899cc;
    font-size:1.2em;
    margin-top:6px;
    font-family:helvetica
}
ul
{
    margin-top:30px;
    margin-left:0px;
    padding-left:0px;
    font-size:1em;
    list-style-type:none;

}
.homepage li
{
    margin-left:0px;
    padding-left:0px;
    margin-top:20px;
    margin-bottom:10px;
    line-height:140%;
    color:#555;
}

h1.magazine_title
{
    font-size:2.2em;
    font-family:times;
    font-weight:normal;
    margin-top:0px;
    background:none;
    color:red;
}

.blurb
{
    font-size:1.4em;
    margin-bottom:0px;
    padding-bottom:10px;
    line-height:150%;
    color:#444;
    font-family:times;
}

.blurb_box {height:180px; }
.logo_box
{
    height:50px;
    vertical-align:bottom;
    display:none;
}

.blurb_box:hover { background:#ffa; }
.blurb_column
{
    width:300px;
    float:left;
    margin-right:60px;
}
#blurb_div
{
    margin-bottom:40px;
    float:left;
    line-height:140%;
    color:#555;
}
</style>

<div style="float:left;margin-top:30px;">
<p><a style="background:#ffa"  href="/?lang=en">english</a></p><p><a  href="/?lang=fr">français</a></p><p><a  href="/?lang=jp">日本語</a></p><p><a  href="/?lang=de">deutsch</a></p><p><a  href="/?lang=ru">русский</a></p><p><a  href="/?lang=pl">polski</a></p></div>
<div style="text-align:left;margin-left:auto;margin-right:auto;width:782px;">

    <div id="main_welcome">

     <h1 class="homepage_heading" >Welcome to Pinboard!</h1>
     <p class="homepage_subheading">Social Bookmarking for Introverts</p>

    <ul class="homepage">

      <li class="homepage">Pinboard is a fast, no-nonsense bookmarking site for people who value privacy and speed.</li>
    <li>There are no ads and no third-party tracking.  You pay a few bucks a year, and that's it.</li>
    <li>Pinboard lets you bookmark from any browser, connect up to three Twitter accounts (and favorites), and sync with popular services like Instapaper or Pocket.</li>
    <li>For a few more bucks a year, Pinboard offers an archiving service which saves a copy of everything you bookmark, gives you full-text search, and automatically checks your account for dead links.</li>

       <li>Take a quick <a style="text-decoration:underline" href="/tour/">tour</a> of the site to see the kinds of things it can do.</li>


    </ul>


    </div>

<div id="main_right_column">

    <div id="login" style="margin-top:30px;">
    <form name="login" action="https://pinboard.in/auth/" method="post" >
         <table width="380" border=0>
     <tr>
      <td width="99">
        <span style="color:#999">username :</span></td>
      <td>
        <input name="username" type="text" value="" size="20" /></td>
     </tr>
     <tr>
      <td>
        <span style="color:#999">password :</span></td>
      <td>
        <input name="password" type="password" size ="20" /></td>
     </tr>
     <tr>
        <td></td>
      <td><input type="submit" value="log in"/> [<a href="/password_reset/">lost password</a>]</td>
     </tr>
     </table>
    </form>

      </div>
              <a href="https://pinboard.in/signup/">
            <div class="signup_button">
                Sign up for $22 / year            </div>


        </a>


</div>


    <div id="blurb_div">
        <div class="blurb_column">
            <a href="http://content.time.com/time/specials/packages/article/0,28804,2094921_2094923_2094924-2,00.html">
           <div class="blurb_box">
                <h1 class="magazine_title">Time</h1>
                <p class="blurb">“A one-man operation can compete against million dollar corporations and thrive.”</p>            </div>
            </a>
            <a href="http://www.guardian.co.uk/technology/blog/2011/dec/16/goodbye-delicious-hello-pinboard-bookmarking-guardian">
            <div class="blurb_box">
                <h1 class="magazine_title">The Guardian</h1>
                <p class="blurb">“Pinboard is a very effective service… Sometimes, you don't need glitz; you need plumbing.”</p>

            </div>
           </a>
        </div>

        <div class="blurb_column">
         <a href="http://www.wired.com/magazine/2011/08/st_thompson_onlineads/">
            <div class="blurb_box">
                <h1 class="magazine_title">Wired</h1>
                <p class="blurb">“A clever bookmarking service that lets you organize links […] and even cache copies of entire web pages.”</p>            </div>
            </a>
                        <a href="http://www.economist.com/blogs/babbage/2011/04/price_fame">
                <div class="blurb_box">
                    <h1 class="magazine_title">The Economist</h1>
                <p class="blurb">“One dude in his underpants somewhere who has five windows open to terminal servers.”</p>                </div>
            </a>
                   </div>
        <div style="clear:both"></div>
    </div> <!-- end blurb div -->
</div>
<br/>
<div style="clear:both"></div>
<div id="footer">
&copy; Nine Fives Software.  Problems or questions?  Contact &lt;<a href="mailto:support@pinboard.in">support@pinboard.in</a>&gt;.
<br/><hr style="border-top:1px dotted #aaa;">
<a href="/tos/">TOS</a> ‧
<a href="/privacy/">privacy</a> ‧
<a href="/about/">about</a> ‧
<a href="http://blog.pinboard.in">blog</a> ‧
<a href="/faq/">FAQ</a> ‧
<a href="/resources/">resources</a> ‧
<a href="/security/">security</a>
</div>


  <div style="clear:both"></div>
  </div>
  <script src="/js/pin.js?w=xrwar"></script>
   <script src="/js/omnibus.js?opad"></script>

  </body>
</html>
DOC
)

(module+ test
  (check-not-exn (lambda () (parse document))))
