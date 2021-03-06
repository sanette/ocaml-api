// Searching the OCAML API.
// San VU NGOC, 2019-2020

// Thanks @steinuil for help on deferred loading.
// Thanks @osener, @UnixJunkie, @Armael for very helpful suggestions
// Thanks to all testers!

const MAX_RESULTS = 20;
const MAX_ERROR = 10;
const DESCR_INDEX = 4; // index of HTML description in index.js
const SIG_INDEX = 6; // index of HTML signature in index.js
const ERR_INDEX = 8; // length of each line in index.js. This is used
		     // for storing error, except if we don't want
		     // description and type signature, the ERR_INDEX
		     // becomes DESCR_INDEX.

let indexState = 'NOT_LOADED';

// return true if we are loading the index file
function loadingIndex (includeDescr) {

    switch (indexState) {
    case 'NOT_LOADED':
	indexState = 'LOADING';
	
	const script = document.createElement('script');
	script.src = 'index.js';
	script.addEventListener('load', () => {
	    indexState = 'HAS_LOADED';
	    mySearch(includeDescr);
	});
	document.head.appendChild(script);
	return true;
	
    case 'LOADING':
	return true;
	
    case 'HAS_LOADED':
	return false;
    }
}

// check if sub is a substring of s. The start/end of the string s are
// marked by "^" and "$", and hence these chars can be used in sub to
// refine the search.
function isSubString (sub, s) {
    // s = s.toLowerCase();
    // sub = sub.toLowerCase();
    // case sensitive is better for OCaml modules.
    let ss = "^" + s + "$";
    return (ss.indexOf(sub) !== -1);
}

// line is a string array. We check if sub is a substring of one of
// the elements of the array.
function hasSubString (sub, line) {
    return (line.findIndex(function (s) {
	return (isSubString(sub,s)); }) !== -1);
}

// Check if one of the strings in subs is a substring of one of the
// strings in line.
function hasSubStrings (subs, line) {
    return (subs.findIndex(function (sub) {
	return (hasSubString (sub, line)); }) !== -1);
}

// error of sub being a substring of s. Best if starts at 0. Except
// for strings containing "->", which is then best if the substring is
// at the most right-hand position (representing the "return type").
// markers "^" and "$" for start/end of string can be used: if they
// are not satisfied, the MAX_ERROR is returned.
function subError (sub, s) {
    let StartOnly = false;
    let EndOnly = false;
    if (sub.length>1) {
	if (sub[0] == "^") {
	    StartOnly = true;
	    sub = sub.substring(1);
	}
	if (sub[sub.length - 1] == "$") {
	    EndOnly = true;
	    sub = sub.substring(0, sub.length - 1);
	}
    }
    let err = s.indexOf(sub);
    if (err == -1 ||
	(StartOnly && err != 0) ||
	(EndOnly && err != s.length - sub.length)) {
	err = MAX_ERROR;
    } else {
	if ( sub.includes("->") ) {
	    err = Math.min(s.length - sub.length - err,1); // 0 or 1
	    // err = 0 if the substring is right-aligned
	} else {
	    err = Math.min(err,1); // 0 or 1
	    // err = 0 if the substring
	}
	err += Math.abs((s.length - sub.length) / s.length);}
    return (err)
    // between 0 and 2, except if MAX_ERROR
}

// Minimal substring error. In particular, it returns 0 if the string
// 'sub' has an exact match with one of the strings in 'line'.
function subMinError (sub, line) {
    //let strings = [line[0], line[1], line[2]];
    let errs = line.map(function (s) { return subError (sub, s); });
    return Math.min(...errs); // destructuring assignment
}


function add (acc, a) {
    return acc + a;
}

// NOT used
// return the mean of SubErrors for all substrings subs inside s
function subsError (subs, s) {
    let errs = subs.map(function (sub) { return subError (sub, s); });
    return errs.reduce(add,0) / subs.length;
}

// for each sub we compute the minimal error within 'line', and then
// take the average over all 'subs'. Thus it returns 0 if each sub has
// an exact match with one of the strings in 'line'.
function subsMinError (subs, line) {
    let errs = subs.map(function (sub) { return subMinError (sub, line); });
    return errs.reduce(add,0) / subs.length;
}
    
// This should print "true" to the console
//let line = ["abc", "123", "hello"];
//console.log (hasSubString ("ell", line));

//console.log(GENERAL_INDEX.filter(function (line) {
//    return (hasSubString("iter", line)); })[0]);

function formatLine (line) {
    let li = '<li>';
    let html = '<code class="code"><a href="' + line[1] + '"><span class="constructor">' + line[0] + '</span></a>' +
	'.' + '<a href="' + line[3] + '">' + line[2] + '</a></code>';
    if (line.length > 5) {
	if ( line[ERR_INDEX] == 0 ) {
	    li = '<li class="match">';
	}
	html += (' : ' + line[SIG_INDEX] + '</pre>');
	html = '<pre>' + html + line[DESCR_INDEX]; }
    return (li + html + "</li>\n");
}

// The initial format of an entry of the GENERAL_INDEX array is
// [ module, module_link,
//   value, value_link,
//   html_description, bare_description,
//   html_signature, bare_signature ]

// If includeDescr is true, the line is truncated to its first 4
// elements.  When searching, the search error is added at the end of
// each line.

// In order to reduce the size of the index.js file, one could create
// the bare_description on-the-fly using .textContent, see
// https://stackoverflow.com/questions/28899298/extract-the-text-out-of-html-string-using-javascript,
// but it would probably make searching slower (haven't tested).
function mySearch (includeDescr) {
    if (loadingIndex (includeDescr)) {
	return;
    }
    let text = document.getElementById('api_search').value;
    let results = [];
    let html = "";
    let count = 0;
    let err_index = DESCR_INDEX;

    if (text !== "") {
	let words = [];
	if ( includeDescr ) {
	    err_index = ERR_INDEX;
	    //console.log ('err_index=' + parseInt(err_index));
	    
	    // Split text into an array of non-empty words:
	    if ( text.includes("->") ) {
		// this must be a type, so we don't split on single space
		words = text.split("  ");
	    } else {
		words = text.split(" ");
	    };
	    words = words.filter(function (s) {
		return (s !== "")});
	    console.log (words);
	}
	
	results = GENERAL_INDEX.filter(function (line) {
	    // We remove the html hrefs and add the Module.value complete name:
	    let cleanLine = [line[0], line[2], line[0] + '.' + line[2]];
	    line.length = err_index; // This truncates the line:
	    // this removes the description part if includeDescr =
	    // false (which modifies the lines of the GENERAL_INDEX.)
	    if ( includeDescr ) {
		cleanLine.push(line[DESCR_INDEX+1]);
		cleanLine.push(line[SIG_INDEX+1]);
		// add the description and signature (txt format)
	    }
	    if ( hasSubString(text, cleanLine) ||
		 // if includeDescr, we search for all separated words
		 ( includeDescr && hasSubStrings(words, cleanLine) ) ) {
		// one could merge hasSubString and subMinError for efficiency
		let error = MAX_ERROR;
		if ( includeDescr ) {
		    error = subsMinError(words, cleanLine);
		} else {
		    error = subMinError(text, cleanLine);
		}
		line[err_index] = error;
		// we add the error as element #err_index
		return (true); }
	    else {
		return (false); }
	});
	
	// We sort the results by relevance:
	results.sort(function(line1, line2) {
	    return (line1[err_index] - line2[err_index])});
	count = results.length;
	console.log("Results = " + (count.toString()));
	results.length = Math.min(results.length, MAX_RESULTS);
	html = "no results";
	//console.log (results[0]);
    }
    // injects new html
    if (results.length > 0) {
	console.log("Best match has error=" + results[0][err_index].toString());
	html = "<ul>";
	function myIter(line, index, array) {
	    html = html + formatLine(line);
	}
	results.forEach(myIter);
	html += "</ul>";
	if (count > results.length) {
	    html += "(...)";
	}
    }
    document.getElementById("search_results").innerHTML = html;
    //document.addEventListener( 'click', function ( e ) { console.log ("click-clack"); console.log(e); });
}

