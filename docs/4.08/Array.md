<!-- ((! set title API !)) ((! set documentation !)) ((! set api !)) ((! set nobreadcrumb !)) -->
<div class="api"><header><nav class="toc brand"><a class="brand" href="https://ocaml.org/"><img src="colour-logo-gray.svg" class="svg" alt="OCaml"></a></nav><nav class="toc"><div class="toc_version"><a href="/docs" id="version-select">API Version 4.08</a></div><a href="index.html">&lt; General Index</a><div class="api_search"><input type="text" name="apisearch" id="api_search" oninput="mySearch(false);" onkeypress="this.oninput();" onclick="this.oninput();" onpaste="this.oninput();">
<img src="search_icon.svg" alt="Search" class="svg" onclick="mySearch(false)"></div>
<div id="search_results"></div><div class="toc_title"><a href="#top">Array</a></div><ul><li><a href="#1_Iterators">Iterators</a></li><li><a href="#1_Iteratorsontwoarrays">Iterators on two arrays</a></li><li><a href="#1_Arrayscanning">Array scanning</a></li><li><a href="#1_Sorting">Sorting</a></li><li><a href="#1_Iterators">Iterators</a></li></ul></nav></header>

<h1>Module <a href="type_Array.html">Array</a></h1>

<pre><span id="MODULEArray"><span class="keyword">module</span> Array</span>: <code class="code"><span class="keyword">sig</span></code> <a href="Array.html">..</a> <code class="code"><span class="keyword">end</span></code></pre><hr width="100%">

<pre><span id="TYPEt"><span class="keyword">type</span> <code class="type">'a</code> t</span> = <code class="type">'a array</code> </pre>
<div class="info ">
<div class="info-desc">
<p>An alias for the type of arrays.</p>
</div>
</div>

<p>Array operations.</p>

<pre><span id="VALlength"><span class="keyword">val</span> length</span> : <code class="type">'a array -&gt; int</code></pre><div class="info ">
<div class="info-desc">
<p>Return the length (number of elements) of the given array.</p>
</div>
</div>

<pre><span id="VALget"><span class="keyword">val</span> get</span> : <code class="type">'a array -&gt; int -&gt; 'a</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.get&nbsp;a&nbsp;n</code> returns the element number <code class="code">n</code> of array <code class="code">a</code>.
   The first element has number 0.
   The last element has number <code class="code"><span class="constructor">Array</span>.length&nbsp;a&nbsp;-&nbsp;1</code>.
   You can also write <code class="code">a.(n)</code> instead of <code class="code"><span class="constructor">Array</span>.get&nbsp;a&nbsp;n</code>.</p>

<p>Raise <code class="code"><span class="constructor">Invalid_argument</span></code>
   if <code class="code">n</code> is outside the range 0 to <code class="code">(<span class="constructor">Array</span>.length&nbsp;a&nbsp;-&nbsp;1)</code>.</p>
</div>
</div>

<pre><span id="VALset"><span class="keyword">val</span> set</span> : <code class="type">'a array -&gt; int -&gt; 'a -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.set&nbsp;a&nbsp;n&nbsp;x</code> modifies array <code class="code">a</code> in place, replacing
   element number <code class="code">n</code> with <code class="code">x</code>.
   You can also write <code class="code">a.(n)&nbsp;&lt;-&nbsp;x</code> instead of <code class="code"><span class="constructor">Array</span>.set&nbsp;a&nbsp;n&nbsp;x</code>.</p>

<p>Raise <code class="code"><span class="constructor">Invalid_argument</span></code>
   if <code class="code">n</code> is outside the range 0 to <code class="code"><span class="constructor">Array</span>.length&nbsp;a&nbsp;-&nbsp;1</code>.</p>
</div>
</div>

<pre><span id="VALmake"><span class="keyword">val</span> make</span> : <code class="type">int -&gt; 'a -&gt; 'a array</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.make&nbsp;n&nbsp;x</code> returns a fresh array of length <code class="code">n</code>,
   initialized with <code class="code">x</code>.
   All the elements of this new array are initially
   physically equal to <code class="code">x</code> (in the sense of the <code class="code">==</code> predicate).
   Consequently, if <code class="code">x</code> is mutable, it is shared among all elements
   of the array, and modifying <code class="code">x</code> through one of the array entries
   will modify all other entries at the same time.</p>

<p>Raise <code class="code"><span class="constructor">Invalid_argument</span></code> if <code class="code">n&nbsp;&lt;&nbsp;0</code> or <code class="code">n&nbsp;&gt;&nbsp;<span class="constructor">Sys</span>.max_array_length</code>.
   If the value of <code class="code">x</code> is a floating-point number, then the maximum
   size is only <code class="code"><span class="constructor">Sys</span>.max_array_length&nbsp;/&nbsp;2</code>.</p>
</div>
</div>

<pre><span id="VALcreate"><span class="keyword">val</span> create</span> : <code class="type">int -&gt; 'a -&gt; 'a array</code></pre><div class="info ">
<div class="info-deprecated">
<span class="warning">Deprecated.</span><code class="code"><span class="constructor">Array</span>.create</code> is an alias for <a href="Array.html#VALmake"><code class="code"><span class="constructor">Array</span>.make</code></a>.</div>
</div>

<pre><span id="VALcreate_float"><span class="keyword">val</span> create_float</span> : <code class="type">int -&gt; float array</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.create_float&nbsp;n</code> returns a fresh float array of length <code class="code">n</code>,
    with uninitialized data.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.03</li>
</ul>
</div>

<pre><span id="VALmake_float"><span class="keyword">val</span> make_float</span> : <code class="type">int -&gt; float array</code></pre><div class="info ">
<div class="info-deprecated">
<span class="warning">Deprecated.</span><code class="code"><span class="constructor">Array</span>.make_float</code> is an alias for <a href="Array.html#VALcreate_float"><code class="code"><span class="constructor">Array</span>.create_float</code></a>.</div>
</div>

<pre><span id="VALinit"><span class="keyword">val</span> init</span> : <code class="type">int -&gt; (int -&gt; 'a) -&gt; 'a array</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.init&nbsp;n&nbsp;f</code> returns a fresh array of length <code class="code">n</code>,
   with element number <code class="code">i</code> initialized to the result of <code class="code">f&nbsp;i</code>.
   In other terms, <code class="code"><span class="constructor">Array</span>.init&nbsp;n&nbsp;f</code> tabulates the results of <code class="code">f</code>
   applied to the integers <code class="code">0</code> to <code class="code">n-1</code>.</p>

<p>Raise <code class="code"><span class="constructor">Invalid_argument</span></code> if <code class="code">n&nbsp;&lt;&nbsp;0</code> or <code class="code">n&nbsp;&gt;&nbsp;<span class="constructor">Sys</span>.max_array_length</code>.
   If the return type of <code class="code">f</code> is <code class="code">float</code>, then the maximum
   size is only <code class="code"><span class="constructor">Sys</span>.max_array_length&nbsp;/&nbsp;2</code>.</p>
</div>
</div>

<pre><span id="VALmake_matrix"><span class="keyword">val</span> make_matrix</span> : <code class="type">int -&gt; int -&gt; 'a -&gt; 'a array array</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.make_matrix&nbsp;dimx&nbsp;dimy&nbsp;e</code> returns a two-dimensional array
   (an array of arrays) with first dimension <code class="code">dimx</code> and
   second dimension <code class="code">dimy</code>. All the elements of this new matrix
   are initially physically equal to <code class="code">e</code>.
   The element (<code class="code">x,y</code>) of a matrix <code class="code">m</code> is accessed
   with the notation <code class="code">m.(x).(y)</code>.</p>

<p>Raise <code class="code"><span class="constructor">Invalid_argument</span></code> if <code class="code">dimx</code> or <code class="code">dimy</code> is negative or
   greater than <a href="Sys.html#VALmax_array_length"><code class="code"><span class="constructor">Sys</span>.max_array_length</code></a>.
   If the value of <code class="code">e</code> is a floating-point number, then the maximum
   size is only <code class="code"><span class="constructor">Sys</span>.max_array_length&nbsp;/&nbsp;2</code>.</p>
</div>
</div>

<pre><span id="VALcreate_matrix"><span class="keyword">val</span> create_matrix</span> : <code class="type">int -&gt; int -&gt; 'a -&gt; 'a array array</code></pre><div class="info ">
<div class="info-deprecated">
<span class="warning">Deprecated.</span><code class="code"><span class="constructor">Array</span>.create_matrix</code> is an alias for <a href="Array.html#VALmake_matrix"><code class="code"><span class="constructor">Array</span>.make_matrix</code></a>.</div>
</div>

<pre><span id="VALappend"><span class="keyword">val</span> append</span> : <code class="type">'a array -&gt; 'a array -&gt; 'a array</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.append&nbsp;v1&nbsp;v2</code> returns a fresh array containing the
   concatenation of the arrays <code class="code">v1</code> and <code class="code">v2</code>.</p>

<p>Raise <code class="code"><span class="constructor">Invalid_argument</span></code> if
   <code class="code"><span class="constructor">Array</span>.length&nbsp;v1&nbsp;+&nbsp;<span class="constructor">Array</span>.length&nbsp;v2&nbsp;&gt;&nbsp;<span class="constructor">Sys</span>.max_array_length</code>.</p>
</div>
</div>

<pre><span id="VALconcat"><span class="keyword">val</span> concat</span> : <code class="type">'a array list -&gt; 'a array</code></pre><div class="info ">
<div class="info-desc">
<p>Same as <a href="Array.html#VALappend"><code class="code"><span class="constructor">Array</span>.append</code></a>, but concatenates a list of arrays.</p>
</div>
</div>

<pre><span id="VALsub"><span class="keyword">val</span> sub</span> : <code class="type">'a array -&gt; int -&gt; int -&gt; 'a array</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.sub&nbsp;a&nbsp;start&nbsp;len</code> returns a fresh array of length <code class="code">len</code>,
   containing the elements number <code class="code">start</code> to <code class="code">start&nbsp;+&nbsp;len&nbsp;-&nbsp;1</code>
   of array <code class="code">a</code>.</p>

<p>Raise <code class="code"><span class="constructor">Invalid_argument</span></code> if <code class="code">start</code> and <code class="code">len</code> do not
   designate a valid subarray of <code class="code">a</code>; that is, if
   <code class="code">start&nbsp;&lt;&nbsp;0</code>, or <code class="code">len&nbsp;&lt;&nbsp;0</code>, or <code class="code">start&nbsp;+&nbsp;len&nbsp;&gt;&nbsp;<span class="constructor">Array</span>.length&nbsp;a</code>.</p>
</div>
</div>

<pre><span id="VALcopy"><span class="keyword">val</span> copy</span> : <code class="type">'a array -&gt; 'a array</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.copy&nbsp;a</code> returns a copy of <code class="code">a</code>, that is, a fresh array
   containing the same elements as <code class="code">a</code>.</p>
</div>
</div>

<pre><span id="VALfill"><span class="keyword">val</span> fill</span> : <code class="type">'a array -&gt; int -&gt; int -&gt; 'a -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.fill&nbsp;a&nbsp;ofs&nbsp;len&nbsp;x</code> modifies the array <code class="code">a</code> in place,
   storing <code class="code">x</code> in elements number <code class="code">ofs</code> to <code class="code">ofs&nbsp;+&nbsp;len&nbsp;-&nbsp;1</code>.</p>

<p>Raise <code class="code"><span class="constructor">Invalid_argument</span></code> if <code class="code">ofs</code> and <code class="code">len</code> do not
   designate a valid subarray of <code class="code">a</code>.</p>
</div>
</div>

<pre><span id="VALblit"><span class="keyword">val</span> blit</span> : <code class="type">'a array -&gt; int -&gt; 'a array -&gt; int -&gt; int -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.blit&nbsp;v1&nbsp;o1&nbsp;v2&nbsp;o2&nbsp;len</code> copies <code class="code">len</code> elements
   from array <code class="code">v1</code>, starting at element number <code class="code">o1</code>, to array <code class="code">v2</code>,
   starting at element number <code class="code">o2</code>. It works correctly even if
   <code class="code">v1</code> and <code class="code">v2</code> are the same array, and the source and
   destination chunks overlap.</p>

<p>Raise <code class="code"><span class="constructor">Invalid_argument</span></code> if <code class="code">o1</code> and <code class="code">len</code> do not
   designate a valid subarray of <code class="code">v1</code>, or if <code class="code">o2</code> and <code class="code">len</code> do not
   designate a valid subarray of <code class="code">v2</code>.</p>
</div>
</div>

<pre><span id="VALto_list"><span class="keyword">val</span> to_list</span> : <code class="type">'a array -&gt; 'a list</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.to_list&nbsp;a</code> returns the list of all the elements of <code class="code">a</code>.</p>
</div>
</div>

<pre><span id="VALof_list"><span class="keyword">val</span> of_list</span> : <code class="type">'a list -&gt; 'a array</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.of_list&nbsp;l</code> returns a fresh array containing the elements
   of <code class="code">l</code>.</p>

<p>Raise <code class="code"><span class="constructor">Invalid_argument</span></code> if the length of <code class="code">l</code> is greater than
   <code class="code"><span class="constructor">Sys</span>.max_array_length</code>.</p>
</div>
</div>
<h2 id="1_Iterators">Iterators</h2>
<pre><span id="VALiter"><span class="keyword">val</span> iter</span> : <code class="type">('a -&gt; unit) -&gt; 'a array -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.iter&nbsp;f&nbsp;a</code> applies function <code class="code">f</code> in turn to all
   the elements of <code class="code">a</code>.  It is equivalent to
   <code class="code">f&nbsp;a.(0);&nbsp;f&nbsp;a.(1);&nbsp;...;&nbsp;f&nbsp;a.(<span class="constructor">Array</span>.length&nbsp;a&nbsp;-&nbsp;1);&nbsp;()</code>.</p>
</div>
</div>

<pre><span id="VALiteri"><span class="keyword">val</span> iteri</span> : <code class="type">(int -&gt; 'a -&gt; unit) -&gt; 'a array -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p>Same as <a href="Array.html#VALiter"><code class="code"><span class="constructor">Array</span>.iter</code></a>, but the
   function is applied with the index of the element as first argument,
   and the element itself as second argument.</p>
</div>
</div>

<pre><span id="VALmap"><span class="keyword">val</span> map</span> : <code class="type">('a -&gt; 'b) -&gt; 'a array -&gt; 'b array</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.map&nbsp;f&nbsp;a</code> applies function <code class="code">f</code> to all the elements of <code class="code">a</code>,
   and builds an array with the results returned by <code class="code">f</code>:
   <code class="code">[|&nbsp;f&nbsp;a.(0);&nbsp;f&nbsp;a.(1);&nbsp;...;&nbsp;f&nbsp;a.(<span class="constructor">Array</span>.length&nbsp;a&nbsp;-&nbsp;1)&nbsp;|]</code>.</p>
</div>
</div>

<pre><span id="VALmapi"><span class="keyword">val</span> mapi</span> : <code class="type">(int -&gt; 'a -&gt; 'b) -&gt; 'a array -&gt; 'b array</code></pre><div class="info ">
<div class="info-desc">
<p>Same as <a href="Array.html#VALmap"><code class="code"><span class="constructor">Array</span>.map</code></a>, but the
   function is applied to the index of the element as first argument,
   and the element itself as second argument.</p>
</div>
</div>

<pre><span id="VALfold_left"><span class="keyword">val</span> fold_left</span> : <code class="type">('a -&gt; 'b -&gt; 'a) -&gt; 'a -&gt; 'b array -&gt; 'a</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.fold_left&nbsp;f&nbsp;x&nbsp;a</code> computes
   <code class="code">f&nbsp;(...&nbsp;(f&nbsp;(f&nbsp;x&nbsp;a.(0))&nbsp;a.(1))&nbsp;...)&nbsp;a.(n-1)</code>,
   where <code class="code">n</code> is the length of the array <code class="code">a</code>.</p>
</div>
</div>

<pre><span id="VALfold_right"><span class="keyword">val</span> fold_right</span> : <code class="type">('b -&gt; 'a -&gt; 'a) -&gt; 'b array -&gt; 'a -&gt; 'a</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.fold_right&nbsp;f&nbsp;a&nbsp;x</code> computes
   <code class="code">f&nbsp;a.(0)&nbsp;(f&nbsp;a.(1)&nbsp;(&nbsp;...&nbsp;(f&nbsp;a.(n-1)&nbsp;x)&nbsp;...))</code>,
   where <code class="code">n</code> is the length of the array <code class="code">a</code>.</p>
</div>
</div>
<h2 id="1_Iteratorsontwoarrays">Iterators on two arrays</h2>
<pre><span id="VALiter2"><span class="keyword">val</span> iter2</span> : <code class="type">('a -&gt; 'b -&gt; unit) -&gt; 'a array -&gt; 'b array -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.iter2&nbsp;f&nbsp;a&nbsp;b</code> applies function <code class="code">f</code> to all the elements of <code class="code">a</code>
   and <code class="code">b</code>.
   Raise <code class="code"><span class="constructor">Invalid_argument</span></code> if the arrays are not the same size.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.03.0</li>
</ul>
</div>

<pre><span id="VALmap2"><span class="keyword">val</span> map2</span> : <code class="type">('a -&gt; 'b -&gt; 'c) -&gt; 'a array -&gt; 'b array -&gt; 'c array</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.map2&nbsp;f&nbsp;a&nbsp;b</code> applies function <code class="code">f</code> to all the elements of <code class="code">a</code>
   and <code class="code">b</code>, and builds an array with the results returned by <code class="code">f</code>:
   <code class="code">[|&nbsp;f&nbsp;a.(0)&nbsp;b.(0);&nbsp;...;&nbsp;f&nbsp;a.(<span class="constructor">Array</span>.length&nbsp;a&nbsp;-&nbsp;1)&nbsp;b.(<span class="constructor">Array</span>.length&nbsp;b&nbsp;-&nbsp;1)|]</code>.
   Raise <code class="code"><span class="constructor">Invalid_argument</span></code> if the arrays are not the same size.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.03.0</li>
</ul>
</div>
<h2 id="1_Arrayscanning">Array scanning</h2>
<pre><span id="VALfor_all"><span class="keyword">val</span> for_all</span> : <code class="type">('a -&gt; bool) -&gt; 'a array -&gt; bool</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.for_all&nbsp;p&nbsp;[|a1;&nbsp;...;&nbsp;an|]</code> checks if all elements of the array
   satisfy the predicate <code class="code">p</code>. That is, it returns
   <code class="code">(p&nbsp;a1)&nbsp;<span class="keywordsign">&amp;&amp;</span>&nbsp;(p&nbsp;a2)&nbsp;<span class="keywordsign">&amp;&amp;</span>&nbsp;...&nbsp;<span class="keywordsign">&amp;&amp;</span>&nbsp;(p&nbsp;an)</code>.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.03.0</li>
</ul>
</div>

<pre><span id="VALexists"><span class="keyword">val</span> exists</span> : <code class="type">('a -&gt; bool) -&gt; 'a array -&gt; bool</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Array</span>.exists&nbsp;p&nbsp;[|a1;&nbsp;...;&nbsp;an|]</code> checks if at least one element of
    the array satisfies the predicate <code class="code">p</code>. That is, it returns
    <code class="code">(p&nbsp;a1)&nbsp;<span class="keywordsign">||</span>&nbsp;(p&nbsp;a2)&nbsp;<span class="keywordsign">||</span>&nbsp;...&nbsp;<span class="keywordsign">||</span>&nbsp;(p&nbsp;an)</code>.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.03.0</li>
</ul>
</div>

<pre><span id="VALmem"><span class="keyword">val</span> mem</span> : <code class="type">'a -&gt; 'a array -&gt; bool</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code">mem&nbsp;a&nbsp;l</code> is true if and only if <code class="code">a</code> is structurally equal
    to an element of <code class="code">l</code> (i.e. there is an <code class="code">x</code> in <code class="code">l</code> such that
    <code class="code">compare&nbsp;a&nbsp;x&nbsp;=&nbsp;0</code>).</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.03.0</li>
</ul>
</div>

<pre><span id="VALmemq"><span class="keyword">val</span> memq</span> : <code class="type">'a -&gt; 'a array -&gt; bool</code></pre><div class="info ">
<div class="info-desc">
<p>Same as <a href="Array.html#VALmem"><code class="code"><span class="constructor">Array</span>.mem</code></a>, but uses physical equality instead of structural
   equality to compare elements.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.03.0</li>
</ul>
</div>
<h2 id="1_Sorting">Sorting</h2>
<pre><span id="VALsort"><span class="keyword">val</span> sort</span> : <code class="type">('a -&gt; 'a -&gt; int) -&gt; 'a array -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p>Sort an array in increasing order according to a comparison
   function.  The comparison function must return 0 if its arguments
   compare as equal, a positive integer if the first is greater,
   and a negative integer if the first is smaller (see below for a
   complete specification).  For example, <a href="Stdlib.html#VALcompare"><code class="code">compare</code></a> is
   a suitable comparison function.  After calling <code class="code"><span class="constructor">Array</span>.sort</code>, the
   array is sorted in place in increasing order.
   <code class="code"><span class="constructor">Array</span>.sort</code> is guaranteed to run in constant heap space
   and (at most) logarithmic stack space.</p>

<p>The current implementation uses Heap Sort.  It runs in constant
   stack space.</p>

<p>Specification of the comparison function:
   Let <code class="code">a</code> be the array and <code class="code">cmp</code> the comparison function.  The following
   must be true for all <code class="code">x</code>, <code class="code">y</code>, <code class="code">z</code> in <code class="code">a</code> :</p>
<ul>
<li>  <code class="code">cmp&nbsp;x&nbsp;y</code> &gt; 0 if and only if <code class="code">cmp&nbsp;y&nbsp;x</code> &lt; 0</li>
<li>  if <code class="code">cmp&nbsp;x&nbsp;y</code> &gt;= 0 and <code class="code">cmp&nbsp;y&nbsp;z</code> &gt;= 0 then <code class="code">cmp&nbsp;x&nbsp;z</code> &gt;= 0</li>
</ul>
<p>When <code class="code"><span class="constructor">Array</span>.sort</code> returns, <code class="code">a</code> contains the same elements as before,
   reordered in such a way that for all i and j valid indices of <code class="code">a</code> :</p>
<ul>
<li>  <code class="code">cmp&nbsp;a.(i)&nbsp;a.(j)</code> &gt;= 0 if and only if i &gt;= j</li>
</ul>
</div>
</div>

<pre><span id="VALstable_sort"><span class="keyword">val</span> stable_sort</span> : <code class="type">('a -&gt; 'a -&gt; int) -&gt; 'a array -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p>Same as <a href="Array.html#VALsort"><code class="code"><span class="constructor">Array</span>.sort</code></a>, but the sorting algorithm is stable (i.e.
   elements that compare equal are kept in their original order) and
   not guaranteed to run in constant heap space.</p>

<p>The current implementation uses Merge Sort. It uses a temporary
   array of length <code class="code">n/2</code>, where <code class="code">n</code> is the length of the array.
   It is usually faster than the current implementation of <a href="Array.html#VALsort"><code class="code"><span class="constructor">Array</span>.sort</code></a>.</p>
</div>
</div>

<pre><span id="VALfast_sort"><span class="keyword">val</span> fast_sort</span> : <code class="type">('a -&gt; 'a -&gt; int) -&gt; 'a array -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p>Same as <a href="Array.html#VALsort"><code class="code"><span class="constructor">Array</span>.sort</code></a> or <a href="Array.html#VALstable_sort"><code class="code"><span class="constructor">Array</span>.stable_sort</code></a>, whichever is faster
    on typical input.</p>
</div>
</div>
<h2 id="1_Iterators">Iterators</h2>
<pre><span id="VALto_seq"><span class="keyword">val</span> to_seq</span> : <code class="type">'a array -&gt; 'a <a href="Seq.html#TYPEt">Seq.t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Iterate on the array, in increasing order. Modifications of the
    array during iteration will be reflected in the iterator.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.07</li>
</ul>
</div>

<pre><span id="VALto_seqi"><span class="keyword">val</span> to_seqi</span> : <code class="type">'a array -&gt; (int * 'a) <a href="Seq.html#TYPEt">Seq.t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Iterate on the array, in increasing order, yielding indices along elements.
    Modifications of the array during iteration will be reflected in the
    iterator.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.07</li>
</ul>
</div>

<pre><span id="VALof_seq"><span class="keyword">val</span> of_seq</span> : <code class="type">'a <a href="Seq.html#TYPEt">Seq.t</a> -&gt; 'a array</code></pre><div class="info ">
<div class="info-desc">
<p>Create an array from the generator</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.07</li>
</ul>
</div>

<div class="copyright">The present documentation is copyright Institut National de Recherche en Informatique et en Automatique (INRIA). A complete version can be obtained from <a href="http://caml.inria.fr/pub/docs/manual-ocaml/">this page</a>.</div></div>