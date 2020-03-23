<!-- ((! set title API !)) ((! set documentation !)) ((! set api !)) ((! set nobreadcrumb !)) -->
<div class="api"><header><nav class="toc brand"><a class="brand" href="https://ocaml.org/"><img src="colour-logo-gray.svg" class="svg" alt="OCaml"></a></nav><nav class="toc"><div class="toc_version"><a href="/docs" id="version-select">API Version 4.09</a></div><a href="index.html">&lt; General Index</a><div class="api_search"><input type="text" name="apisearch" id="api_search" oninput="mySearch(false);" onkeypress="this.oninput();" onclick="this.oninput();" onpaste="this.oninput();">
<img src="search_icon.svg" alt="Search" class="svg" onclick="mySearch(false)"></div>
<div id="search_results"></div><div class="toc_title"><a href="Queue.html">Queue</a></div><ul></ul></nav></header>
<code class="code"><span class="keyword">sig</span><br>
&nbsp;&nbsp;<span class="keyword">type</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;t<br>
&nbsp;&nbsp;<span class="keyword">exception</span>&nbsp;<span class="constructor">Empty</span><br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;create&nbsp;:&nbsp;unit&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;add&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;unit<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;push&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;unit<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;take&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;take_opt&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;option<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;pop&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;peek&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;peek_opt&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;option<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;top&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;clear&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;unit<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;copy&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;is_empty&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;bool<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;length&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;int<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;iter&nbsp;:&nbsp;(<span class="keywordsign">'</span>a&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;unit)&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;unit<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;fold&nbsp;:&nbsp;(<span class="keywordsign">'</span>b&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>b)&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>b&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>b<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;transfer&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;unit<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;to_seq&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Stdlib</span>.<span class="constructor">Seq</span>.t<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;add_seq&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Stdlib</span>.<span class="constructor">Seq</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;unit<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;of_seq&nbsp;:&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Stdlib</span>.<span class="constructor">Seq</span>.t&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="constructor">Queue</span>.t<br>
<span class="keyword">end</span></code>
<div class="copyright">The present documentation is copyright Institut National de Recherche en Informatique et en Automatique (INRIA). A complete version can be obtained from <a href="http://caml.inria.fr/pub/docs/manual-ocaml/">this page</a>.</div></div>