// Searching the OCAML API.
// San VU NGOC, 2019

// TODO load on demand ?
// https://stackoverflow.com/questions/10906836/javascript-to-load-another-js-file
// persistent var ?
// https://stackoverflow.com/questions/17309199/how-to-send-variables-from-one-file-to-another-in-javascript/17309679#17309679


var MAX_RESULTS = 20;
var MAX_ERROR = 10;

function isSubString (sub, s) {
    //s = s.toLowerCase();
    //sub = sub.toLowerCase();
    return (s.indexOf(sub) !== -1);
}

// line is a string array. We check if sub is a substring of one of
// the elements of the array.
function hasSubString (sub, line) {
    return (line.findIndex(function (s) {
	return (isSubString(sub,s)); }) !== -1);
}

function subError (sub, line) {
    errors = line.map(function (s)
		      { let err = s.indexOf(sub);
			if (err == -1) { err = MAX_ERROR; }
			else { err = Math.min(err,1) // 0 or 1
			       + Math.abs((s.length - sub.length) / s.length);}
			return (err)})
    return (Math.min(Math.min(errors[0], errors[1]), errors[2])); // between 0 and 3, except if MAX_ERROR
}

// This should print "true" to the console
//let line = ["abc", "123", "hello"];
//console.log (hasSubString ("ell", line));

//console.log(GENERAL_INDEX.filter(function (line) {
//    return (hasSubString("iter", line)); })[0]);

function formatLine (line) {
    let html = '<code class="code"><a href="' + line[1] + '"><span class="constructor">' + line[0] + '</span></a>' +
	'.' + '<a href="' + line[3] + '">' + line[2] + '</a></code>';
    if (line.length > 5) { html += line[4]; }
    return (html);
}
    
function mySearch (includeDescr) {
    let text = document.getElementById('api_search').value;
    let results = [];
    let html = "";
    let count = 0;
    if (text !== "") {
	results = GENERAL_INDEX.filter(function (line) {
	    if (includeDescr == false )
	    { line.length = 4;
	      // we remove the description part.
	    }
	    // We remove the html hrefs and add the Module.val complete name:
	    let cleanLine = [line[0], line[2], line[0] + '.' + line[2]];
	    if ( includeDescr ) { cleanLine.push(line[4]); } // add the description
	    //line.push(line[0] + '.' + line[2]);
	    if (hasSubString(text, cleanLine))  // one could merge hasSubString ans subError for efficiency
	    { line.push(subError(text, cleanLine)); // we add the error
	      return (true); } else
	    { return (false); }});
	// We sort the results by relevance:
	results.sort(function(line1, line2)
		     {if (includeDescr)
		      { return (line1[5] - line2[5]) } else { return (line1[4] - line2[4]) } });
	count = results.length;
	console.log("Results = " + (count.toString()));
	results.length = Math.min(results.length, MAX_RESULTS);
	html = "no results";
	//console.log (results[0]);
    }
    if (results.length > 0) {
	html = "<ul>";
	function myIter(line, index, array) {
	    html = html + "<li>" + formatLine(line) + "</li>\n";
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

