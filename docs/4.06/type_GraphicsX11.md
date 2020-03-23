<!-- ((! set title API !)) ((! set documentation !)) ((! set api !)) ((! set nobreadcrumb !)) -->
<div class="api"><header><nav class="toc brand"><a class="brand" href="https://ocaml.org/"><img src="colour-logo-gray.svg" class="svg" alt="OCaml"></a></nav><nav class="toc"><div class="toc_version"><a href="/docs" id="version-select">API Version 4.06</a></div><a href="index.html">&lt; General Index</a><div class="api_search"><input type="text" name="apisearch" id="api_search" oninput="mySearch(false);" onkeypress="this.oninput();" onclick="this.oninput();" onpaste="this.oninput();">
<img src="search_icon.svg" alt="Search" class="svg" onclick="mySearch(false)"></div>
<div id="search_results"></div><div class="toc_title"><a href="GraphicsX11.html">GraphicsX11</a></div><ul></ul></nav></header>
<code class="code"><span class="keyword">sig</span><br>
&nbsp;&nbsp;<span class="keyword">type</span>&nbsp;window_id&nbsp;=&nbsp;string<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;window_id&nbsp;:&nbsp;unit&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="constructor">GraphicsX11</span>.window_id<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;open_subwindow&nbsp;:<br>
&nbsp;&nbsp;&nbsp;&nbsp;x:int&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;y:int&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;width:int&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;height:int&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="constructor">GraphicsX11</span>.window_id<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;close_subwindow&nbsp;:&nbsp;<span class="constructor">GraphicsX11</span>.window_id&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;unit<br>
<span class="keyword">end</span></code><div class="copyright">The present documentation is copyright Institut National de Recherche en Informatique et en Automatique (INRIA). A complete version can be obtained from <a href="http://caml.inria.fr/pub/docs/manual-ocaml/">this page</a>.</div></div>