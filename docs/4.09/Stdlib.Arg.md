<!-- ((! set title API !)) ((! set documentation !)) ((! set api !)) ((! set nobreadcrumb !)) -->
<div class="api"><header><nav class="toc brand"><a class="brand" href="https://ocaml.org/"><img src="colour-logo-gray.svg" class="svg" alt="OCaml"></a></nav><nav class="toc"><div class="toc_version"><a href="/docs" id="version-select">API Version 4.09</a></div><a href="index.html">&lt; General Index</a><div class="api_search"><input type="text" name="apisearch" id="api_search" oninput="mySearch(false);" onkeypress="this.oninput();" onclick="this.oninput();" onpaste="this.oninput();">
<img src="search_icon.svg" alt="Search" class="svg" onclick="mySearch(false)"></div>
<div id="search_results"></div><div class="toc_title"><a href="#top">Stdlib.Arg</a></div><ul></ul></nav></header>

<h1>Module <a href="type_Stdlib.Arg.html">Stdlib.Arg</a></h1>

<pre><span id="MODULEArg"><span class="keyword">module</span> Arg</span>: <code class="type"><a href="Arg.html">Arg</a></code></pre><hr width="100%">

<pre><code><span id="TYPEspec"><span class="keyword">type</span> <code class="type"></code>spec</span> = </code></pre><table class="typetable">
<tbody><tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.Unit"><span class="constructor">Unit</span></span> <span class="keyword">of</span> <code class="type">(unit -&gt; unit)</code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>Call the function with unit argument</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.Bool"><span class="constructor">Bool</span></span> <span class="keyword">of</span> <code class="type">(bool -&gt; unit)</code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>Call the function with a bool argument</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.Set"><span class="constructor">Set</span></span> <span class="keyword">of</span> <code class="type">bool <a href="Stdlib.html#TYPEref">ref</a></code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>Set the reference to true</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.Clear"><span class="constructor">Clear</span></span> <span class="keyword">of</span> <code class="type">bool <a href="Stdlib.html#TYPEref">ref</a></code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>Set the reference to false</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.String"><span class="constructor">String</span></span> <span class="keyword">of</span> <code class="type">(string -&gt; unit)</code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>Call the function with a string argument</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.Set_string"><span class="constructor">Set_string</span></span> <span class="keyword">of</span> <code class="type">string <a href="Stdlib.html#TYPEref">ref</a></code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>Set the reference to the string argument</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.Int"><span class="constructor">Int</span></span> <span class="keyword">of</span> <code class="type">(int -&gt; unit)</code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>Call the function with an int argument</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.Set_int"><span class="constructor">Set_int</span></span> <span class="keyword">of</span> <code class="type">int <a href="Stdlib.html#TYPEref">ref</a></code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>Set the reference to the int argument</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.Float"><span class="constructor">Float</span></span> <span class="keyword">of</span> <code class="type">(float -&gt; unit)</code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>Call the function with a float argument</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.Set_float"><span class="constructor">Set_float</span></span> <span class="keyword">of</span> <code class="type">float <a href="Stdlib.html#TYPEref">ref</a></code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>Set the reference to the float argument</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.Tuple"><span class="constructor">Tuple</span></span> <span class="keyword">of</span> <code class="type"><a href="Arg.html#TYPEspec">spec</a> list</code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>Take several arguments according to the
                                   spec list</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.Symbol"><span class="constructor">Symbol</span></span> <span class="keyword">of</span> <code class="type">string list * (string -&gt; unit)</code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>Take one of the symbols as argument and
                                   call the function with the symbol</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.Rest"><span class="constructor">Rest</span></span> <span class="keyword">of</span> <code class="type">(string -&gt; unit)</code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>Stop interpreting keywords and call the
                                   function with each remaining argument</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr>
<tr>
<td align="left" valign="top">
<code><span class="keyword">|</span></code></td>
<td align="left" valign="top">
<code><span id="TYPEELTspec.Expand"><span class="constructor">Expand</span></span> <span class="keyword">of</span> <code class="type">(string -&gt; string array)</code></code></td>
<td class="typefieldcomment" align="left" valign="top"><code>(*</code></td><td class="typefieldcomment" align="left" valign="top"><div class="info ">
<div class="info-desc">
<p>If the remaining arguments to process
                                           are of the form
                                           <code class="code">[<span class="string">"-foo"</span>;&nbsp;<span class="string">"arg"</span>]&nbsp;@&nbsp;rest</code> where "foo"
                                           is registered as <code class="code"><span class="constructor">Expand</span>&nbsp;f</code>, then the
                                           arguments <code class="code">f&nbsp;<span class="string">"arg"</span>&nbsp;@&nbsp;rest</code> are
                                           processed. Only allowed in
                                           <code class="code">parse_and_expand_argv_dynamic</code>.</p>
</div>
</div>
</td><td class="typefieldcomment" align="left" valign="bottom"><code>*)</code></td>
</tr></tbody></table>

<div class="info ">
<div class="info-desc">
<p>The concrete type describing the behavior associated
   with a keyword.</p>
</div>
</div>


<pre><span id="TYPEkey"><span class="keyword">type</span> <code class="type"></code>key</span> = <code class="type">string</code> </pre>


<pre><span id="TYPEdoc"><span class="keyword">type</span> <code class="type"></code>doc</span> = <code class="type">string</code> </pre>


<pre><span id="TYPEusage_msg"><span class="keyword">type</span> <code class="type"></code>usage_msg</span> = <code class="type">string</code> </pre>


<pre><span id="TYPEanon_fun"><span class="keyword">type</span> <code class="type"></code>anon_fun</span> = <code class="type">string -&gt; unit</code> </pre>


<pre><span id="VALparse"><span class="keyword">val</span> parse</span> : <code class="type">(<a href="Arg.html#TYPEkey">key</a> * <a href="Arg.html#TYPEspec">spec</a> * <a href="Arg.html#TYPEdoc">doc</a>) list -&gt; <a href="Arg.html#TYPEanon_fun">anon_fun</a> -&gt; <a href="Arg.html#TYPEusage_msg">usage_msg</a> -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Arg</span>.parse&nbsp;speclist&nbsp;anon_fun&nbsp;usage_msg</code> parses the command line.
    <code class="code">speclist</code> is a list of triples <code class="code">(key,&nbsp;spec,&nbsp;doc)</code>.
    <code class="code">key</code> is the option keyword, it must start with a <code class="code"><span class="string">'-'</span></code> character.
    <code class="code">spec</code> gives the option type and the function to call when this option
    is found on the command line.
    <code class="code">doc</code> is a one-line description of this option.
    <code class="code">anon_fun</code> is called on anonymous arguments.
    The functions in <code class="code">spec</code> and <code class="code">anon_fun</code> are called in the same order
    as their arguments appear on the command line.</p>

<p>If an error occurs, <code class="code"><span class="constructor">Arg</span>.parse</code> exits the program, after printing
    to standard error an error message as follows:</p>
<ul>
<li>  The reason for the error: unknown option, invalid or missing argument, etc.</li>
<li>  <code class="code">usage_msg</code></li>
<li>  The list of options, each followed by the corresponding <code class="code">doc</code> string.
    Beware: options that have an empty <code class="code">doc</code> string will not be included in the
    list.</li>
</ul>
<p>For the user to be able to specify anonymous arguments starting with a
    <code class="code">-</code>, include for example <code class="code">(<span class="string">"-"</span>,&nbsp;<span class="constructor">String</span>&nbsp;anon_fun,&nbsp;doc)</code> in <code class="code">speclist</code>.</p>

<p>By default, <code class="code">parse</code> recognizes two unit options, <code class="code">-help</code> and <code class="code">--help</code>,
    which will print to standard output <code class="code">usage_msg</code> and the list of
    options, and exit the program.  You can override this behaviour
    by specifying your own <code class="code">-help</code> and <code class="code">--help</code> options in <code class="code">speclist</code>.</p>
</div>
</div>

<pre><span id="VALparse_dynamic"><span class="keyword">val</span> parse_dynamic</span> : <code class="type">(<a href="Arg.html#TYPEkey">key</a> * <a href="Arg.html#TYPEspec">spec</a> * <a href="Arg.html#TYPEdoc">doc</a>) list <a href="Stdlib.html#TYPEref">ref</a> -&gt;<br>       <a href="Arg.html#TYPEanon_fun">anon_fun</a> -&gt; <a href="Arg.html#TYPEusage_msg">usage_msg</a> -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p>Same as <a href="Arg.html#VALparse"><code class="code"><span class="constructor">Arg</span>.parse</code></a>, except that the <code class="code">speclist</code> argument is a reference
    and may be updated during the parsing. A typical use for this feature
    is to parse command lines of the form:</p>
<ul>
<li>    command subcommand <code class="code">options</code>
    where the list of options depends on the value of the subcommand argument.</li>
</ul>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.01.0</li>
</ul>
</div>

<pre><span id="VALparse_argv"><span class="keyword">val</span> parse_argv</span> : <code class="type">?current:int <a href="Stdlib.html#TYPEref">ref</a> -&gt;<br>       string array -&gt;<br>       (<a href="Arg.html#TYPEkey">key</a> * <a href="Arg.html#TYPEspec">spec</a> * <a href="Arg.html#TYPEdoc">doc</a>) list -&gt; <a href="Arg.html#TYPEanon_fun">anon_fun</a> -&gt; <a href="Arg.html#TYPEusage_msg">usage_msg</a> -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Arg</span>.parse_argv&nbsp;~current&nbsp;args&nbsp;speclist&nbsp;anon_fun&nbsp;usage_msg</code> parses
  the array <code class="code">args</code> as if it were the command line.  It uses and updates
  the value of <code class="code">~current</code> (if given), or <a href="Arg.html#VALcurrent"><code class="code"><span class="constructor">Arg</span>.current</code></a>.  You must set
  it before calling <code class="code">parse_argv</code>.  The initial value of <code class="code">current</code>
  is the index of the program name (argument 0) in the array.
  If an error occurs, <code class="code"><span class="constructor">Arg</span>.parse_argv</code> raises <a href="Arg.html#EXCEPTIONBad"><code class="code"><span class="constructor">Arg</span>.<span class="constructor">Bad</span></code></a> with
  the error message as argument.  If option <code class="code">-help</code> or <code class="code">--help</code> is
  given, <code class="code"><span class="constructor">Arg</span>.parse_argv</code> raises <a href="Arg.html#EXCEPTIONHelp"><code class="code"><span class="constructor">Arg</span>.<span class="constructor">Help</span></code></a> with the help message
  as argument.</p>
</div>
</div>

<pre><span id="VALparse_argv_dynamic"><span class="keyword">val</span> parse_argv_dynamic</span> : <code class="type">?current:int <a href="Stdlib.html#TYPEref">ref</a> -&gt;<br>       string array -&gt;<br>       (<a href="Arg.html#TYPEkey">key</a> * <a href="Arg.html#TYPEspec">spec</a> * <a href="Arg.html#TYPEdoc">doc</a>) list <a href="Stdlib.html#TYPEref">ref</a> -&gt;<br>       <a href="Arg.html#TYPEanon_fun">anon_fun</a> -&gt; string -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p>Same as <a href="Arg.html#VALparse_argv"><code class="code"><span class="constructor">Arg</span>.parse_argv</code></a>, except that the <code class="code">speclist</code> argument is a
    reference and may be updated during the parsing.
    See <a href="Arg.html#VALparse_dynamic"><code class="code"><span class="constructor">Arg</span>.parse_dynamic</code></a>.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.01.0</li>
</ul>
</div>

<pre><span id="VALparse_and_expand_argv_dynamic"><span class="keyword">val</span> parse_and_expand_argv_dynamic</span> : <code class="type">int <a href="Stdlib.html#TYPEref">ref</a> -&gt;<br>       string array <a href="Stdlib.html#TYPEref">ref</a> -&gt;<br>       (<a href="Arg.html#TYPEkey">key</a> * <a href="Arg.html#TYPEspec">spec</a> * <a href="Arg.html#TYPEdoc">doc</a>) list <a href="Stdlib.html#TYPEref">ref</a> -&gt;<br>       <a href="Arg.html#TYPEanon_fun">anon_fun</a> -&gt; string -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p>Same as <a href="Arg.html#VALparse_argv_dynamic"><code class="code"><span class="constructor">Arg</span>.parse_argv_dynamic</code></a>, except that the <code class="code">argv</code> argument is a
    reference and may be updated during the parsing of <code class="code"><span class="constructor">Expand</span></code> arguments.
    See <a href="Arg.html#VALparse_argv_dynamic"><code class="code"><span class="constructor">Arg</span>.parse_argv_dynamic</code></a>.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.05.0</li>
</ul>
</div>

<pre><span id="VALparse_expand"><span class="keyword">val</span> parse_expand</span> : <code class="type">(<a href="Arg.html#TYPEkey">key</a> * <a href="Arg.html#TYPEspec">spec</a> * <a href="Arg.html#TYPEdoc">doc</a>) list -&gt; <a href="Arg.html#TYPEanon_fun">anon_fun</a> -&gt; <a href="Arg.html#TYPEusage_msg">usage_msg</a> -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p>Same as <a href="Arg.html#VALparse"><code class="code"><span class="constructor">Arg</span>.parse</code></a>, except that the <code class="code"><span class="constructor">Expand</span></code> arguments are allowed and
    the <a href="Arg.html#VALcurrent"><code class="code"><span class="constructor">Arg</span>.current</code></a> reference is not updated.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.05.0</li>
</ul>
</div>

<pre><span id="EXCEPTIONHelp"><span class="keyword">exception</span> Help</span> <span class="keyword">of</span> <code class="type">string</code></pre>
<div class="info ">
<div class="info-desc">
<p>Raised by <code class="code"><span class="constructor">Arg</span>.parse_argv</code> when the user asks for help.</p>
</div>
</div>

<pre><span id="EXCEPTIONBad"><span class="keyword">exception</span> Bad</span> <span class="keyword">of</span> <code class="type">string</code></pre>
<div class="info ">
<div class="info-desc">
<p>Functions in <code class="code">spec</code> or <code class="code">anon_fun</code> can raise <code class="code"><span class="constructor">Arg</span>.<span class="constructor">Bad</span></code> with an error
    message to reject invalid arguments.
    <code class="code"><span class="constructor">Arg</span>.<span class="constructor">Bad</span></code> is also raised by <a href="Arg.html#VALparse_argv"><code class="code"><span class="constructor">Arg</span>.parse_argv</code></a> in case of an error.</p>
</div>
</div>

<pre><span id="VALusage"><span class="keyword">val</span> usage</span> : <code class="type">(<a href="Arg.html#TYPEkey">key</a> * <a href="Arg.html#TYPEspec">spec</a> * <a href="Arg.html#TYPEdoc">doc</a>) list -&gt; <a href="Arg.html#TYPEusage_msg">usage_msg</a> -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Arg</span>.usage&nbsp;speclist&nbsp;usage_msg</code> prints to standard error
    an error message that includes the list of valid options.  This is
    the same message that <a href="Arg.html#VALparse"><code class="code"><span class="constructor">Arg</span>.parse</code></a> prints in case of error.
    <code class="code">speclist</code> and <code class="code">usage_msg</code> are the same as for <a href="Arg.html#VALparse"><code class="code"><span class="constructor">Arg</span>.parse</code></a>.</p>
</div>
</div>

<pre><span id="VALusage_string"><span class="keyword">val</span> usage_string</span> : <code class="type">(<a href="Arg.html#TYPEkey">key</a> * <a href="Arg.html#TYPEspec">spec</a> * <a href="Arg.html#TYPEdoc">doc</a>) list -&gt; <a href="Arg.html#TYPEusage_msg">usage_msg</a> -&gt; string</code></pre><div class="info ">
<div class="info-desc">
<p>Returns the message that would have been printed by <a href="Arg.html#VALusage"><code class="code"><span class="constructor">Arg</span>.usage</code></a>,
    if provided with the same parameters.</p>
</div>
</div>

<pre><span id="VALalign"><span class="keyword">val</span> align</span> : <code class="type">?limit:int -&gt;<br>       (<a href="Arg.html#TYPEkey">key</a> * <a href="Arg.html#TYPEspec">spec</a> * <a href="Arg.html#TYPEdoc">doc</a>) list -&gt; (<a href="Arg.html#TYPEkey">key</a> * <a href="Arg.html#TYPEspec">spec</a> * <a href="Arg.html#TYPEdoc">doc</a>) list</code></pre><div class="info ">
<div class="info-desc">
<p>Align the documentation strings by inserting spaces at the first alignment
    separator (tab or, if tab is not found, space), according to the length of
    the keyword.  Use a alignment separator as the first character in a doc
    string if you want to align the whole string.  The doc strings corresponding
    to <code class="code"><span class="constructor">Symbol</span></code> arguments are aligned on the next line.</p>
</div>
</div>
<div class="param_info"><code class="code">limit</code> : options with keyword and message longer than <code class="code">limit</code> will not
    be used to compute the alignment.</div>

<pre><span id="VALcurrent"><span class="keyword">val</span> current</span> : <code class="type">int <a href="Stdlib.html#TYPEref">ref</a></code></pre><div class="info ">
<div class="info-desc">
<p>Position (in <a href="Sys.html#VALargv"><code class="code"><span class="constructor">Sys</span>.argv</code></a>) of the argument being processed.  You can
    change this value, e.g. to force <a href="Arg.html#VALparse"><code class="code"><span class="constructor">Arg</span>.parse</code></a> to skip some arguments.
    <a href="Arg.html#VALparse"><code class="code"><span class="constructor">Arg</span>.parse</code></a> uses the initial value of <a href="Arg.html#VALcurrent"><code class="code"><span class="constructor">Arg</span>.current</code></a> as the index of
    argument 0 (the program name) and starts parsing arguments
    at the next element.</p>
</div>
</div>

<pre><span id="VALread_arg"><span class="keyword">val</span> read_arg</span> : <code class="type">string -&gt; string array</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Arg</span>.read_arg&nbsp;file</code> reads newline-terminated command line arguments from
    file <code class="code">file</code>.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.05.0</li>
</ul>
</div>

<pre><span id="VALread_arg0"><span class="keyword">val</span> read_arg0</span> : <code class="type">string -&gt; string array</code></pre><div class="info ">
<div class="info-desc">
<p>Identical to <a href="Arg.html#VALread_arg"><code class="code"><span class="constructor">Arg</span>.read_arg</code></a> but assumes null character terminated command
    line arguments.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.05.0</li>
</ul>
</div>

<pre><span id="VALwrite_arg"><span class="keyword">val</span> write_arg</span> : <code class="type">string -&gt; string array -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p><code class="code"><span class="constructor">Arg</span>.write_arg&nbsp;file&nbsp;args</code> writes the arguments <code class="code">args</code> newline-terminated
    into the file <code class="code">file</code>. If the any of the arguments in <code class="code">args</code> contains a
    newline, use <a href="Arg.html#VALwrite_arg0"><code class="code"><span class="constructor">Arg</span>.write_arg0</code></a> instead.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.05.0</li>
</ul>
</div>

<pre><span id="VALwrite_arg0"><span class="keyword">val</span> write_arg0</span> : <code class="type">string -&gt; string array -&gt; unit</code></pre><div class="info ">
<div class="info-desc">
<p>Identical to <a href="Arg.html#VALwrite_arg"><code class="code"><span class="constructor">Arg</span>.write_arg</code></a> but uses the null character for terminator
    instead of newline.</p>
</div>
<ul class="info-attributes">
<li><b>Since</b> 4.05.0</li>
</ul>
</div>

<div class="copyright">The present documentation is copyright Institut National de Recherche en Informatique et en Automatique (INRIA). A complete version can be obtained from <a href="http://caml.inria.fr/pub/docs/manual-ocaml/">this page</a>.</div></div>