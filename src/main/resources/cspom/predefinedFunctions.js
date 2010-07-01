function neg(x) { return -x; }

function abs(x) {
	if (x>0) return x;
	return -x;
}

function add(x,y) { return x+y; }

function sub(x,y) { return x-y; }

function mul(x,y) { return x*y; }

function div(x,y) { return Math.floor(x/y); }

function mod(x,y) { return x%y; }

function pow(x,y) { return Math.pow(x, y); }

function min(x,y) { 
	return x<y?x:y;
	if (x<y) return x;
	return y;
}

function max(x,y) { 
	if (x<y) return y;
	return x;
}

function not(x) { return !x; }

function and(x,y) { return Boolean(x) && Boolean(y); }

function p_or() { 
  var a = arguments.length/2;
  for (var i = a; --i >= 0;) {
  	if (arguments[i+a] ^ arguments[i]) { return true; }
  } 
  return false
}

function or() { 
  for (var i = arguments.length; --i >= 0;) {
  	if (arguments[i]) { return true; }
  } 
  return false
}


function xor(x,y) { return x&&!y || !x&&y; }
 
function eq(x,y) { return x == y; }

function ne(x,y) { return x!=y; }

function lt(x,y) { return x<y; }

function le(x,y) { return x<=y; }

function gt(x,y) { return x>y; }

function ge(x,y) { return x>=y; }

function ite(x,y,z) { return x?y:z; }

function iff(x,y) {
	return x==y;
}

function alldifferent() {
	for (var i = arguments.length; --i >= 0;) {
	  for (var j = i; --j >= 0;) {
	    if (arguments[i] == arguments[j]) {
	      return false;
	    }
	  }
	}
	return true;
}

function absdiff(x,y) { return Math.abs(x-y) ; }

function diffGe(x,y,z) { return x-y >= z ; }
