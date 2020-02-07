// Smooth scrolling only for near targets
// San Vu Ngoc, 2019

// if a link is located at distance larger than MAX_DISTANCE, we don't
// use a smooth scrolling.

console.log ("loading scroll.js");

const MAX_DISTANCE = 1000;

const url = window.location.pathname;
var filename = url.substring(url.lastIndexOf('/')+1);
if (filename == "") { filename = "index.html"; }

function localLink (link) {
    return (link.length > 0 &&
	    (link.charAt(0) == '#'
	     || link.substring(0,filename.length) == filename));
}

//aaa.html#coucou --> coucou
function getId (link) {
    return link.substring(link.lastIndexOf('#')+1);
}

// Get absolute y position of element.
// modified from:
// https://www.kirupa.com/html5/get_element_position_using_javascript.htm
function getPosition(el) {
    let yPos = 0; 
    while (el) {
	yPos += (el.offsetTop + el.clientTop);
	el = el.offsetParent;
    }
    return yPos;
}

function setSmooth () {
    let x = document.getElementsByClassName("toc_title");
    let a = document.getElementsByTagName("a");
    // let container = document.body.parentNode; // for ocaml.org
    let container = document.body; // for local
    let i;
    for (i = 0; i < a.length; i++) {
	let href = a[i].getAttribute("href");
	if (localLink(href)) {
	    //a[i].style.backgroundColor = "red";
	    //let top = getPosition(a[i]);
	    a[i].onclick = function () {
		let id = getId(href);
		//console.log(id);
		let target = document.getElementById(id);
		if (! target) { target = document.body.parentNode; }
		let top = container.scrollTop;
		let dist = top - getPosition(target)
		//console.log ("click ==> " + getId(href) + ", distance = " + parseInt(Math.abs(dist)));
		if (Math.abs(dist) < MAX_DISTANCE) {
		    target.scrollIntoView({ block: "start", inline: "nearest", behavior: 'smooth' });
		    setTimeout(function () {
		    	location.href = href;
			// this will set the "target" property.
		    }, 600);
		    return false;
		    // so we don't follow the link immediately
		}
	    }
	}
    }
}

window.onload = function() {
    setSmooth();
};
