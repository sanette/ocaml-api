<!-- ((! set title API !)) ((! set documentation !)) ((! set api !)) ((! set nobreadcrumb !)) -->
<div class="api"><header><nav class="toc brand"><a class="brand" href="https://ocaml.org/"><img src="colour-logo-gray.svg" class="svg" alt="OCaml"></a></nav><nav class="toc"><div class="toc_version"><a href="/docs" id="version-select">API Version 4.07</a></div><a href="index.html">&lt; General Index</a><div class="api_search"><input type="text" name="apisearch" id="api_search" oninput="mySearch(false);" onkeypress="this.oninput();" onclick="this.oninput();" onpaste="this.oninput();">
<img src="search_icon.svg" alt="Search" class="svg" onclick="mySearch(false)"></div>
<div id="search_results"></div><div class="toc_title"><a href="Scanf.html">Scanf</a></div><ul></ul></nav></header>
<code class="code"><span class="keyword">sig</span><br>
&nbsp;&nbsp;<span class="keyword">module</span>&nbsp;<span class="constructor">Scanning</span>&nbsp;:<br>
&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">sig</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">type</span>&nbsp;in_channel<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">type</span>&nbsp;scanbuf&nbsp;=&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;stdin&nbsp;:&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">type</span>&nbsp;file_name&nbsp;=&nbsp;string<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;open_in&nbsp;:&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.file_name&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;open_in_bin&nbsp;:&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.file_name&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;close_in&nbsp;:&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;unit<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;from_file&nbsp;:&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.file_name&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;from_file_bin&nbsp;:&nbsp;string&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;from_string&nbsp;:&nbsp;string&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;from_function&nbsp;:&nbsp;(unit&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;char)&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;from_channel&nbsp;:&nbsp;<span class="constructor">Pervasives</span>.in_channel&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;end_of_input&nbsp;:&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;bool<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;beginning_of_input&nbsp;:&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;bool<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;name_of_input&nbsp;:&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;string<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;stdib&nbsp;:&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel<br>
&nbsp;&nbsp;&nbsp;&nbsp;<span class="keyword">end</span><br>
&nbsp;&nbsp;<span class="keyword">type</span>&nbsp;(<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d)&nbsp;scanner&nbsp;=<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(<span class="keywordsign">'</span>a,&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>a&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>d,&nbsp;<span class="keywordsign">'</span>d)<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<span class="constructor">Pervasives</span>.format6&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>c<br>
&nbsp;&nbsp;<span class="keyword">exception</span>&nbsp;<span class="constructor">Scan_failure</span>&nbsp;<span class="keyword">of</span>&nbsp;string<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;bscanf&nbsp;:&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;(<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d)&nbsp;<span class="constructor">Scanf</span>.scanner<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;sscanf&nbsp;:&nbsp;string&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;(<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d)&nbsp;<span class="constructor">Scanf</span>.scanner<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;scanf&nbsp;:&nbsp;(<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d)&nbsp;<span class="constructor">Scanf</span>.scanner<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;kscanf&nbsp;:<br>
&nbsp;&nbsp;&nbsp;&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel&nbsp;<span class="keywordsign">-&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;(<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;exn&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>d)&nbsp;<span class="keywordsign">-&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;(<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d)&nbsp;<span class="constructor">Scanf</span>.scanner<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;ksscanf&nbsp;:<br>
&nbsp;&nbsp;&nbsp;&nbsp;string&nbsp;<span class="keywordsign">-&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;(<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;exn&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>d)&nbsp;<span class="keywordsign">-&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;(<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d)&nbsp;<span class="constructor">Scanf</span>.scanner<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;bscanf_format&nbsp;:<br>
&nbsp;&nbsp;&nbsp;&nbsp;<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel&nbsp;<span class="keywordsign">-&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;(<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d,&nbsp;<span class="keywordsign">'</span>e,&nbsp;<span class="keywordsign">'</span>f)&nbsp;<span class="constructor">Pervasives</span>.format6&nbsp;<span class="keywordsign">-&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;((<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d,&nbsp;<span class="keywordsign">'</span>e,&nbsp;<span class="keywordsign">'</span>f)&nbsp;<span class="constructor">Pervasives</span>.format6&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>g)&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>g<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;sscanf_format&nbsp;:<br>
&nbsp;&nbsp;&nbsp;&nbsp;string&nbsp;<span class="keywordsign">-&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;(<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d,&nbsp;<span class="keywordsign">'</span>e,&nbsp;<span class="keywordsign">'</span>f)&nbsp;<span class="constructor">Pervasives</span>.format6&nbsp;<span class="keywordsign">-&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;((<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d,&nbsp;<span class="keywordsign">'</span>e,&nbsp;<span class="keywordsign">'</span>f)&nbsp;<span class="constructor">Pervasives</span>.format6&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>g)&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>g<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;format_from_string&nbsp;:<br>
&nbsp;&nbsp;&nbsp;&nbsp;string&nbsp;<span class="keywordsign">-&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;(<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d,&nbsp;<span class="keywordsign">'</span>e,&nbsp;<span class="keywordsign">'</span>f)&nbsp;<span class="constructor">Pervasives</span>.format6&nbsp;<span class="keywordsign">-&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;(<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d,&nbsp;<span class="keywordsign">'</span>e,&nbsp;<span class="keywordsign">'</span>f)&nbsp;<span class="constructor">Pervasives</span>.format6<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;unescaped&nbsp;:&nbsp;string&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;string<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;fscanf&nbsp;:&nbsp;<span class="constructor">Pervasives</span>.in_channel&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;(<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d)&nbsp;<span class="constructor">Scanf</span>.scanner<br>
&nbsp;&nbsp;<span class="keyword">val</span>&nbsp;kfscanf&nbsp;:<br>
&nbsp;&nbsp;&nbsp;&nbsp;<span class="constructor">Pervasives</span>.in_channel&nbsp;<span class="keywordsign">-&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;(<span class="constructor">Scanf</span>.<span class="constructor">Scanning</span>.in_channel&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;exn&nbsp;<span class="keywordsign">-&gt;</span>&nbsp;<span class="keywordsign">'</span>d)&nbsp;<span class="keywordsign">-&gt;</span><br>
&nbsp;&nbsp;&nbsp;&nbsp;(<span class="keywordsign">'</span>a,&nbsp;<span class="keywordsign">'</span>b,&nbsp;<span class="keywordsign">'</span>c,&nbsp;<span class="keywordsign">'</span>d)&nbsp;<span class="constructor">Scanf</span>.scanner<br>
<span class="keyword">end</span></code>
<div class="copyright">The present documentation is copyright Institut National de Recherche en Informatique et en Automatique (INRIA). A complete version can be obtained from <a href="http://caml.inria.fr/pub/docs/manual-ocaml/">this page</a>.</div></div>