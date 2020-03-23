<!-- ((! set title API !)) ((! set documentation !)) ((! set api !)) ((! set nobreadcrumb !)) -->
<div class="api"><header><nav class="toc brand"><a class="brand" href="https://ocaml.org/"><img src="colour-logo-gray.svg" class="svg" alt="OCaml"></a></nav><nav class="toc"><div class="toc_version"><a href="/docs" id="version-select">API Version 4.00</a></div><a href="index.html">&lt; General Index</a><div class="api_search"><input type="text" name="apisearch" id="api_search" oninput="mySearch(false);" onkeypress="this.oninput();" onclick="this.oninput();" onpaste="this.oninput();">
<img src="search_icon.svg" alt="Search" class="svg" onclick="mySearch(false)"></div>
<div id="search_results"></div><div class="toc_title"><a href="#top">Weak</a></div><ul></ul></nav></header>

<h1>Module <a href="type_Weak.html">Weak</a></h1>
<pre><span class="keyword">module</span> Weak: <code class="code"><span class="keyword">sig</span></code> <a href="Weak.html">..</a> <code class="code"><span class="keyword">end</span></code></pre>Arrays of weak pointers and hash tables of weak pointers.<br>
<hr width="100%">
<br>
<span id="6_Lowlevelfunctions"><h6>Low-level functions</h6></span><br>
<pre><span id="TYPEt"><span class="keyword">type</span> <code class="type">'a</code> t</span> </pre>
<div class="info">
The type of arrays of weak pointers (weak arrays).  A weak
   pointer is a value that the garbage collector may erase whenever
   the value is not used any more (through normal pointers) by the
   program.  Note that finalisation functions are run after the
   weak pointers are erased.
<p>

   A weak pointer is said to be full if it points to a value,
   empty if the value was erased by the GC.
</p><p>

   Notes:</p><ul>
<li>Integers are not allocated and cannot be stored in weak arrays.</li>
<li>Weak arrays cannot be marshaled using <a href="Pervasives.html#VALoutput_value"><code class="code">output_value</code></a>
     nor the functions of the <a href="Marshal.html"><code class="code"><span class="constructor">Marshal</span></code></a> module.</li>
</ul>
<br>
</div>

<pre><span id="VALcreate"><span class="keyword">val</span> create</span> : <code class="type">int -&gt; 'a <a href="Weak.html#TYPEt">t</a></code></pre><div class="info">
<code class="code"><span class="constructor">Weak</span>.create n</code> returns a new weak array of length <code class="code">n</code>.
   All the pointers are initially empty.  Raise <code class="code"><span class="constructor">Invalid_argument</span></code>
   if <code class="code">n</code> is negative or greater than <a href="Sys.html#VALmax_array_length"><code class="code"><span class="constructor">Sys</span>.max_array_length</code></a><code class="code">-1</code>.<br>
</div>
<pre><span id="VALlength"><span class="keyword">val</span> length</span> : <code class="type">'a <a href="Weak.html#TYPEt">t</a> -&gt; int</code></pre><div class="info">
<code class="code"><span class="constructor">Weak</span>.length ar</code> returns the length (number of elements) of
   <code class="code">ar</code>.<br>
</div>
<pre><span id="VALset"><span class="keyword">val</span> set</span> : <code class="type">'a <a href="Weak.html#TYPEt">t</a> -&gt; int -&gt; 'a option -&gt; unit</code></pre><div class="info">
<code class="code"><span class="constructor">Weak</span>.set ar n (<span class="constructor">Some</span> el)</code> sets the <code class="code">n</code>th cell of <code class="code">ar</code> to be a
   (full) pointer to <code class="code">el</code>; <code class="code"><span class="constructor">Weak</span>.set ar n <span class="constructor">None</span></code> sets the <code class="code">n</code>th
   cell of <code class="code">ar</code> to empty.
   Raise <code class="code"><span class="constructor">Invalid_argument</span> <span class="string">"Weak.set"</span></code> if <code class="code">n</code> is not in the range
   0 to <a href="Weak.html#VALlength"><code class="code"><span class="constructor">Weak</span>.length</code></a><code class="code"> a - 1</code>.<br>
</div>
<pre><span id="VALget"><span class="keyword">val</span> get</span> : <code class="type">'a <a href="Weak.html#TYPEt">t</a> -&gt; int -&gt; 'a option</code></pre><div class="info">
<code class="code"><span class="constructor">Weak</span>.get ar n</code> returns None if the <code class="code">n</code>th cell of <code class="code">ar</code> is
   empty, <code class="code"><span class="constructor">Some</span> x</code> (where <code class="code">x</code> is the value) if it is full.
   Raise <code class="code"><span class="constructor">Invalid_argument</span> <span class="string">"Weak.get"</span></code> if <code class="code">n</code> is not in the range
   0 to <a href="Weak.html#VALlength"><code class="code"><span class="constructor">Weak</span>.length</code></a><code class="code"> a - 1</code>.<br>
</div>
<pre><span id="VALget_copy"><span class="keyword">val</span> get_copy</span> : <code class="type">'a <a href="Weak.html#TYPEt">t</a> -&gt; int -&gt; 'a option</code></pre><div class="info">
<code class="code"><span class="constructor">Weak</span>.get_copy ar n</code> returns None if the <code class="code">n</code>th cell of <code class="code">ar</code> is
   empty, <code class="code"><span class="constructor">Some</span> x</code> (where <code class="code">x</code> is a (shallow) copy of the value) if
   it is full.
   In addition to pitfalls with mutable values, the interesting
   difference with <code class="code">get</code> is that <code class="code">get_copy</code> does not prevent
   the incremental GC from erasing the value in its current cycle
   (<code class="code">get</code> may delay the erasure to the next GC cycle).
   Raise <code class="code"><span class="constructor">Invalid_argument</span> <span class="string">"Weak.get"</span></code> if <code class="code">n</code> is not in the range
   0 to <a href="Weak.html#VALlength"><code class="code"><span class="constructor">Weak</span>.length</code></a><code class="code"> a - 1</code>.<br>
</div>
<pre><span id="VALcheck"><span class="keyword">val</span> check</span> : <code class="type">'a <a href="Weak.html#TYPEt">t</a> -&gt; int -&gt; bool</code></pre><div class="info">
<code class="code"><span class="constructor">Weak</span>.check ar n</code> returns <code class="code"><span class="keyword">true</span></code> if the <code class="code">n</code>th cell of <code class="code">ar</code> is
   full, <code class="code"><span class="keyword">false</span></code> if it is empty.  Note that even if <code class="code"><span class="constructor">Weak</span>.check ar n</code>
   returns <code class="code"><span class="keyword">true</span></code>, a subsequent <a href="Weak.html#VALget"><code class="code"><span class="constructor">Weak</span>.get</code></a><code class="code"> ar n</code> can return <code class="code"><span class="constructor">None</span></code>.<br>
</div>
<pre><span id="VALfill"><span class="keyword">val</span> fill</span> : <code class="type">'a <a href="Weak.html#TYPEt">t</a> -&gt; int -&gt; int -&gt; 'a option -&gt; unit</code></pre><div class="info">
<code class="code"><span class="constructor">Weak</span>.fill ar ofs len el</code> sets to <code class="code">el</code> all pointers of <code class="code">ar</code> from
   <code class="code">ofs</code> to <code class="code">ofs + len - 1</code>.  Raise <code class="code"><span class="constructor">Invalid_argument</span> <span class="string">"Weak.fill"</span></code>
   if <code class="code">ofs</code> and <code class="code">len</code> do not designate a valid subarray of <code class="code">a</code>.<br>
</div>
<pre><span id="VALblit"><span class="keyword">val</span> blit</span> : <code class="type">'a <a href="Weak.html#TYPEt">t</a> -&gt; int -&gt; 'a <a href="Weak.html#TYPEt">t</a> -&gt; int -&gt; int -&gt; unit</code></pre><div class="info">
<code class="code"><span class="constructor">Weak</span>.blit ar1 off1 ar2 off2 len</code> copies <code class="code">len</code> weak pointers
   from <code class="code">ar1</code> (starting at <code class="code">off1</code>) to <code class="code">ar2</code> (starting at <code class="code">off2</code>).
   It works correctly even if <code class="code">ar1</code> and <code class="code">ar2</code> are the same.
   Raise <code class="code"><span class="constructor">Invalid_argument</span> <span class="string">"Weak.blit"</span></code> if <code class="code">off1</code> and <code class="code">len</code> do
   not designate a valid subarray of <code class="code">ar1</code>, or if <code class="code">off2</code> and <code class="code">len</code>
   do not designate a valid subarray of <code class="code">ar2</code>.<br>
</div>
<br>
<span id="6_Weakhashtables"><h6>Weak hash tables</h6></span><br>
<br>
A weak hash table is a hashed set of values.  Each value may
    magically disappear from the set when it is not used by the
    rest of the program any more.  This is normally used to share
    data structures without inducing memory leaks.
    Weak hash tables are defined on values from a <a href="Hashtbl.HashedType.html"><code class="code"><span class="constructor">Hashtbl</span>.<span class="constructor">HashedType</span></code></a>
    module; the <code class="code">equal</code> relation and <code class="code">hash</code> function are taken from that
    module.  We will say that <code class="code">v</code> is an instance of <code class="code">x</code> if <code class="code">equal x v</code>
    is <code class="code"><span class="keyword">true</span></code>.
<p>

    The <code class="code">equal</code> relation must be able to work on a shallow copy of
    the values and give the same result as with the values themselves.<br>
</p><pre><span class="keyword">module type</span> <a href="Weak.S.html">S</a> = <code class="code"><span class="keyword">sig</span></code> <a href="Weak.S.html">..</a> <code class="code"><span class="keyword">end</span></code></pre><div class="info">
The output signature of the functor <a href="Weak.Make.html"><code class="code"><span class="constructor">Weak</span>.<span class="constructor">Make</span></code></a>.
</div>
<pre><span class="keyword">module</span> <a href="Weak.Make.html">Make</a>: <div class="sig_block"><code class="code"><span class="keyword">functor</span> (</code><code class="code"><span class="constructor">H</span></code><code class="code"> : </code><code class="type"><a href="Hashtbl.HashedType.html">Hashtbl.HashedType</a></code><code class="code">) <span class="keywordsign">-&gt;</span> </code><code class="type"><a href="Weak.S.html">S</a></code><code class="type">  with type data = H.t</code></div></pre><div class="info">
Functor building an implementation of the weak hash table structure.
</div>
<div class="copyright">The present documentation is copyright Institut National de Recherche en Informatique et en Automatique (INRIA). A complete version can be obtained from <a href="http://caml.inria.fr/pub/docs/manual-ocaml/">this page</a>.</div></div>