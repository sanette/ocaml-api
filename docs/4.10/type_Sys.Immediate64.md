<!-- ((! set title API !)) ((! set documentation !)) ((! set api !)) ((! set nobreadcrumb !)) -->
<div class="api"><header><nav class="toc brand"><a class="brand" href="https://ocaml.org/"><img src="colour-logo-gray.svg" class="svg" alt="OCaml"></a></nav><nav class="toc"><div class="toc_version"><a href="/docs" id="version-select">API Version 4.10</a></div><a href="index.html">&lt; General Index</a><div class="api_search"><input type="text" name="apisearch" id="api_search" oninput="mySearch(false);" onkeypress="this.oninput();" onclick="this.oninput();" onpaste="this.oninput();">
<img src="search_icon.svg" alt="Search" class="svg" onclick="mySearch(false)"></div>
<div id="search_results"></div><div class="toc_title"><a href="Sys.Immediate64.html">Sys.Immediate64</a></div><ul></ul></nav></header>
<code class="code"><span class="keyword">sig</span><br>
&nbsp;&nbsp;<span class="keyword">module</span>&nbsp;<span class="keyword">type</span>&nbsp;<span class="constructor">Non_immediate</span>&nbsp;=&nbsp;<span class="keyword">sig</span>&nbsp;<span class="keyword">type</span>&nbsp;t&nbsp;<span class="keyword">end</span><br>
&nbsp;&nbsp;<span class="keyword">module</span>&nbsp;<span class="keyword">type</span>&nbsp;<span class="constructor">Immediate</span>&nbsp;=&nbsp;<span class="keyword">sig</span>&nbsp;<span class="keyword">type</span>&nbsp;t&nbsp;[@@immediate]&nbsp;<span class="keyword">end</span><br>
&nbsp;&nbsp;<span class="keyword">module</span>&nbsp;<span class="constructor">Make</span>&nbsp;:<br>
&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">functor</span>&nbsp;(<span class="constructor">Immediate</span>&nbsp;:&nbsp;<span class="constructor">Immediate</span>)&nbsp;(<span class="constructor">Non_immediate</span>&nbsp;:&nbsp;<span class="constructor">Non_immediate</span>)&nbsp;<span class="keywordsign">-&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">sig</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">type</span>&nbsp;t&nbsp;[@@immediate64]<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">type</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;repr&nbsp;=<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="constructor">Immediate</span>&nbsp;:&nbsp;<span class="constructor">Sys</span>.<span class="constructor">Immediate64</span>.<span class="constructor">Immediate</span>.t&nbsp;<span class="constructor">Sys</span>.<span class="constructor">Immediate64</span>.<span class="constructor">Make</span>.repr<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keywordsign">|</span>&nbsp;<span class="constructor">Non_immediate</span>&nbsp;:<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="constructor">Sys</span>.<span class="constructor">Immediate64</span>.<span class="constructor">Non_immediate</span>.t&nbsp;<span class="constructor">Sys</span>.<span class="constructor">Immediate64</span>.<span class="constructor">Make</span>.repr<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;repr&nbsp;:&nbsp;<span class="constructor">Sys</span>.<span class="constructor">Immediate64</span>.<span class="constructor">Make</span>.t&nbsp;<span class="constructor">Sys</span>.<span class="constructor">Immediate64</span>.<span class="constructor">Make</span>.repr<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">end</span><br>
<span class="keyword">end</span></code>
<div class="copyright">The present documentation is copyright Institut National de Recherche en Informatique et en Automatique (INRIA). A complete version can be obtained from <a href="http://caml.inria.fr/pub/docs/manual-ocaml/">this page</a>.</div></div>