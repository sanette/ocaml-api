<!-- ((! set title API !)) ((! set documentation !)) ((! set api !)) ((! set nobreadcrumb !)) -->
<div class="api"><header><nav class="toc brand"><a class="brand" href="https://ocaml.org/"><img src="colour-logo-gray.svg" class="svg" alt="OCaml"></a></nav><nav class="toc"><div class="toc_version"><a href="/docs" id="version-select">API Version 4.07</a></div><a href="index.html">&lt; General Index</a><div class="api_search"><input type="text" name="apisearch" id="api_search" oninput="mySearch(false);" onkeypress="this.oninput();" onclick="this.oninput();" onpaste="this.oninput();">
<img src="search_icon.svg" alt="Search" class="svg" onclick="mySearch(false)"></div>
<div id="search_results"></div><div class="toc_title"><a href="#top">Targetint</a></div><ul></ul></nav></header>

<h1>Module <a href="type_Targetint.html">Targetint</a></h1>

<pre><span id="MODULETargetint"><span class="keyword">module</span> Targetint</span>: <code class="code"><span class="keyword">sig</span></code> <a href="Targetint.html">..</a> <code class="code"><span class="keyword">end</span></code></pre><div class="info module top">
<div class="info-desc">
<p>Target processor-native integers.</p>

<p>This module provides operations on the type of
   signed 32-bit integers (on 32-bit target platforms) or
   signed 64-bit integers (on 64-bit target platforms).
   This integer type has exactly the same width as that of a
   pointer type in the C compiler.  All arithmetic operations over
   are taken modulo 2<sup class="superscript">32</sup> or 2<sup class="superscript">64</sup> depending
   on the word size of the target architecture.</p>
</div>
</div>
<hr width="100%">

<pre><span id="TYPEt"><span class="keyword">type</span> <code class="type"></code>t</span> </pre>
<div class="info ">
<div class="info-desc">
<p>The type of target integers.</p>
</div>
</div>


<pre><span id="VALzero"><span class="keyword">val</span> zero</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>The target integer 0.</p>
</div>
</div>

<pre><span id="VALone"><span class="keyword">val</span> one</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>The target integer 1.</p>
</div>
</div>

<pre><span id="VALminus_one"><span class="keyword">val</span> minus_one</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>The target integer -1.</p>
</div>
</div>

<pre><span id="VALneg"><span class="keyword">val</span> neg</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Unary negation.</p>
</div>
</div>

<pre><span id="VALadd"><span class="keyword">val</span> add</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Addition.</p>
</div>
</div>

<pre><span id="VALsub"><span class="keyword">val</span> sub</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Subtraction.</p>
</div>
</div>

<pre><span id="VALmul"><span class="keyword">val</span> mul</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Multiplication.</p>
</div>
</div>

<pre><span id="VALdiv"><span class="keyword">val</span> div</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Integer division.  Raise <code class="code"><span class="constructor">Division_by_zero</span></code> if the second
   argument is zero.  This division rounds the real quotient of
   its arguments towards zero, as specified for <a href="Pervasives.html#VAL(/)"><code class="code">(/)</code></a>.</p>
</div>
</div>

<pre><span id="VALrem"><span class="keyword">val</span> rem</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Integer remainder.  If <code class="code">y</code> is not zero, the result
   of <code class="code"><span class="constructor">Targetint</span>.rem&nbsp;x&nbsp;y</code> satisfies the following properties:
   <code class="code"><span class="constructor">Targetint</span>.zero&nbsp;&lt;=&nbsp;<span class="constructor">Nativeint</span>.rem&nbsp;x&nbsp;y&nbsp;&lt;&nbsp;<span class="constructor">Targetint</span>.abs&nbsp;y</code> and
   <code class="code">x&nbsp;=&nbsp;<span class="constructor">Targetint</span>.add&nbsp;(<span class="constructor">Targetint</span>.mul&nbsp;(<span class="constructor">Targetint</span>.div&nbsp;x&nbsp;y)&nbsp;y)<br>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;(<span class="constructor">Targetint</span>.rem&nbsp;x&nbsp;y)</code>.
   If <code class="code">y&nbsp;=&nbsp;0</code>, <code class="code"><span class="constructor">Targetint</span>.rem&nbsp;x&nbsp;y</code> raises <code class="code"><span class="constructor">Division_by_zero</span></code>.</p>
</div>
</div>

<pre><span id="VALsucc"><span class="keyword">val</span> succ</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Successor.
   <code class="code"><span class="constructor">Targetint</span>.succ&nbsp;x</code> is <code class="code"><span class="constructor">Targetint</span>.add&nbsp;x&nbsp;<span class="constructor">Targetint</span>.one</code>.</p>
</div>
</div>

<pre><span id="VALpred"><span class="keyword">val</span> pred</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Predecessor.
   <code class="code"><span class="constructor">Targetint</span>.pred&nbsp;x</code> is <code class="code"><span class="constructor">Targetint</span>.sub&nbsp;x&nbsp;<span class="constructor">Targetint</span>.one</code>.</p>
</div>
</div>

<pre><span id="VALabs"><span class="keyword">val</span> abs</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Return the absolute value of its argument.</p>
</div>
</div>

<pre><span id="VALsize"><span class="keyword">val</span> size</span> : <code class="type">int</code></pre><div class="info ">
<div class="info-desc">
<p>The size in bits of a target native integer.</p>
</div>
</div>

<pre><span id="VALmax_int"><span class="keyword">val</span> max_int</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>The greatest representable target integer,
    either 2<sup class="superscript">31</sup> - 1 on a 32-bit platform,
    or 2<sup class="superscript">63</sup> - 1 on a 64-bit platform.</p>
</div>
</div>

<pre><span id="VALmin_int"><span class="keyword">val</span> min_int</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>The smallest representable target integer,
   either -2<sup class="superscript">31</sup> on a 32-bit platform,
   or -2<sup class="superscript">63</sup> on a 64-bit platform.</p>
</div>
</div>

<pre><span id="VALlogand"><span class="keyword">val</span> logand</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Bitwise logical and.</p>
</div>
</div>

<pre><span id="VALlogor"><span class="keyword">val</span> logor</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Bitwise logical or.</p>
</div>
</div>

<pre><span id="VALlogxor"><span class="keyword">val</span> logxor</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Bitwise logical exclusive or.</p>
</div>
</div>

<pre><span id="VALlognot"><span class="keyword">val</span> lognot</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Bitwise logical negation.</p>
</div>
</div>

<pre><span id="VALshift_left"><span class="keyword">val</span> shift_left</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; int -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Targetint</span>.shift_left&nbsp;x&nbsp;y</code> shifts <code class="code">x</code> to the left by <code class="code">y</code> bits.
    The result is unspecified if <code class="code">y&nbsp;&lt;&nbsp;0</code> or <code class="code">y&nbsp;&gt;=&nbsp;bitsize</code>,
    where <code class="code">bitsize</code> is <code class="code">32</code> on a 32-bit platform and
    <code class="code">64</code> on a 64-bit platform.</p>
</div>
</div>

<pre><span id="VALshift_right"><span class="keyword">val</span> shift_right</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; int -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Targetint</span>.shift_right&nbsp;x&nbsp;y</code> shifts <code class="code">x</code> to the right by <code class="code">y</code> bits.
    This is an arithmetic shift: the sign bit of <code class="code">x</code> is replicated
    and inserted in the vacated bits.
    The result is unspecified if <code class="code">y&nbsp;&lt;&nbsp;0</code> or <code class="code">y&nbsp;&gt;=&nbsp;bitsize</code>.</p>
</div>
</div>

<pre><span id="VALshift_right_logical"><span class="keyword">val</span> shift_right_logical</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; int -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Targetint</span>.shift_right_logical&nbsp;x&nbsp;y</code> shifts <code class="code">x</code> to the right
    by <code class="code">y</code> bits.
    This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of <code class="code">x</code>.
    The result is unspecified if <code class="code">y&nbsp;&lt;&nbsp;0</code> or <code class="code">y&nbsp;&gt;=&nbsp;bitsize</code>.</p>
</div>
</div>

<pre><span id="VALof_int"><span class="keyword">val</span> of_int</span> : <code class="type">int -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Convert the given integer (type <code class="code">int</code>) to a target integer
    (type <code class="code">t</code>), module the target word size.</p>
</div>
</div>

<pre><span id="VALof_int_exn"><span class="keyword">val</span> of_int_exn</span> : <code class="type">int -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Convert the given integer (type <code class="code">int</code>) to a target integer
    (type <code class="code">t</code>).  Raises a fatal error if the conversion is not exact.</p>
</div>
</div>

<pre><span id="VALto_int"><span class="keyword">val</span> to_int</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; int</code></pre><div class="info ">
<div class="info-desc">
<p>Convert the given target integer (type <code class="code">t</code>) to an
    integer (type <code class="code">int</code>).  The high-order bit is lost during
    the conversion.</p>
</div>
</div>

<pre><span id="VALof_float"><span class="keyword">val</span> of_float</span> : <code class="type">float -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Convert the given floating-point number to a target integer,
   discarding the fractional part (truncate towards 0).
   The result of the conversion is undefined if, after truncation,
   the number is outside the range
   [<a href="Targetint.html#VALmin_int"><code class="code"><span class="constructor">Targetint</span>.min_int</code></a>, <a href="Targetint.html#VALmax_int"><code class="code"><span class="constructor">Targetint</span>.max_int</code></a>].</p>
</div>
</div>

<pre><span id="VALto_float"><span class="keyword">val</span> to_float</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; float</code></pre><div class="info ">
<div class="info-desc">
<p>Convert the given target integer to a floating-point number.</p>
</div>
</div>

<pre><span id="VALof_int32"><span class="keyword">val</span> of_int32</span> : <code class="type">int32 -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Convert the given 32-bit integer (type <code class="code">int32</code>)
    to a target integer.</p>
</div>
</div>

<pre><span id="VALto_int32"><span class="keyword">val</span> to_int32</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; int32</code></pre><div class="info ">
<div class="info-desc">
<p>Convert the given target integer to a
    32-bit integer (type <code class="code">int32</code>).  On 64-bit platforms,
    the 64-bit native integer is taken modulo 2<sup class="superscript">32</sup>,
    i.e. the top 32 bits are lost.  On 32-bit platforms,
    the conversion is exact.</p>
</div>
</div>

<pre><span id="VALof_int64"><span class="keyword">val</span> of_int64</span> : <code class="type">int64 -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Convert the given 64-bit integer (type <code class="code">int64</code>)
    to a target integer.</p>
</div>
</div>

<pre><span id="VALto_int64"><span class="keyword">val</span> to_int64</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; int64</code></pre><div class="info ">
<div class="info-desc">
<p>Convert the given target integer to a
    64-bit integer (type <code class="code">int64</code>).</p>
</div>
</div>

<pre><span id="VALof_string"><span class="keyword">val</span> of_string</span> : <code class="type">string -&gt; <a href="Targetint.html#TYPEt">t</a></code></pre><div class="info ">
<div class="info-desc">
<p>Convert the given string to a target integer.
    The string is read in decimal (by default) or in hexadecimal,
    octal or binary if the string begins with <code class="code">0x</code>, <code class="code">0o</code> or <code class="code">0b</code>
    respectively.
    Raise <code class="code"><span class="constructor">Failure</span>&nbsp;<span class="string">"int_of_string"</span></code> if the given string is not
    a valid representation of an integer, or if the integer represented
    exceeds the range of integers representable in type <code class="code">nativeint</code>.</p>
</div>
</div>

<pre><span id="VALto_string"><span class="keyword">val</span> to_string</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; string</code></pre><div class="info ">
<div class="info-desc">
<p>Return the string representation of its argument, in decimal.</p>
</div>
</div>

<pre><span id="VALcompare"><span class="keyword">val</span> compare</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a> -&gt; int</code></pre><div class="info ">
<div class="info-desc">
<p>The comparison function for target integers, with the same specification as
    <a href="Pervasives.html#VALcompare"><code class="code">compare</code></a>.  Along with the type <code class="code">t</code>, this function <code class="code">compare</code>
    allows the module <code class="code"><span class="constructor">Targetint</span></code> to be passed as argument to the functors
    <a href="Set.Make.html"><code class="code"><span class="constructor">Set</span>.<span class="constructor">Make</span></code></a> and <a href="Map.Make.html"><code class="code"><span class="constructor">Map</span>.<span class="constructor">Make</span></code></a>.</p>
</div>
</div>

<pre><span id="VALequal"><span class="keyword">val</span> equal</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPEt">t</a> -&gt; bool</code></pre><div class="info ">
<div class="info-desc">
<p>The equal function for target ints.</p>
</div>
</div>

<pre><code><span id="TYPErepr"><span class="keyword">type</span> <code class="type"></code>repr</span> = </code></pre><table class="typetable">
<tbody><tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTrepr.Int32"><span class="constructor">Int32</span></span> <span class="keyword">of</span> <code class="type">int32</code></code></td>

</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTrepr.Int64"><span class="constructor">Int64</span></span> <span class="keyword">of</span> <code class="type">int64</code></code></td>

</tr></tbody></table>



<pre><span id="VALrepr"><span class="keyword">val</span> repr</span> : <code class="type"><a href="Targetint.html#TYPEt">t</a> -&gt; <a href="Targetint.html#TYPErepr">repr</a></code></pre><div class="info ">
<div class="info-desc">
<p>The concrete representation of a native integer.</p>
</div>
</div>

<div class="copyright">The present documentation is copyright Institut National de Recherche en Informatique et en Automatique (INRIA). A complete version can be obtained from <a href="http://caml.inria.fr/pub/docs/manual-ocaml/">this page</a>.</div></div>