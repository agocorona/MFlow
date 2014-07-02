// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = new F(f);
}

function F(f) {
    this.f = f;
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        return f;
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f instanceof F) {
            return t.f = t.f.f();
        } else {
            return t.f;
        }
    } else {
        return t;
    }
}

// Export Haste, A and E. Haste because we need to preserve exports, A and E
// because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = A(f, [[0, str.charCodeAt(i)], acc]);
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = A(f, [mv.x]);
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['b']['i8'] = 'U'.charCodeAt(0);
    le['b']['i8'] = 'T'.charCodeAt(0);
    le['b']['i8'] = 'F'.charCodeAt(0);
    le['b']['i8'] = '-'.charCodeAt(0);
    le['b']['i8'] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return A(act,[0]);
    } catch(e) {
        return A(handler,[e, 0]);
    }
}

var coercionToken = undefined;

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.target.offsetLeft || 0),
	    posy - (e.target.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                A(cb,[[0,k.keyCode],0]);
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,x.button],[0,mx,my],0]);
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,mx,my],0]);
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[0,x.keyCode],0]);};
        break;        
    default:
        fun = function() {A(cb,[0]);};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {A(cb,[0]);}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);
    xhr.setRequestHeader('Cache-control', 'no-cache');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                A(cb,[[1,[0,xhr.responseText]],0]);
            } else {
                A(cb,[[0],0]); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=0,_1=function(_2,_3,_4,_5){return A(_2,[new T(function(){return function(_){var _6=jsSetAttr(E(_3)[1],toJSStr(E(_4)),toJSStr(E(_5)));return _0;};})]);},_7=2,_8=[1],_9=[0],_a=[0],_b=function(_c,_){return _a;},_d=function(_){return _a;},_e=[0,_d,_b],_f=[0,0],_g=[0,_9,_f,_7,_e,_8],_h=function(_){var _=0,_i=newMVar(),_=putMVar(_i,_g);return [0,_i];},_j=function(_k){var _l=A(_k,[_]);return E(_l);},_m=new T(function(){return _j(_h);}),_n=function(_o){return E(_o);},_p=function(_q,_r,_){var _s=jsCreateTextNode(toJSStr(E(_q))),_t=jsAppendChild(_s,E(_r)[1]);return [0,_s];},_u=[0,98],_v=[1,_u,_9],_w=function(_x,_y){var _z=new T(function(){return A(_x,[_y]);});return function(_A,_){var _B=jsCreateElem(toJSStr(_v)),_C=jsAppendChild(_B,E(_A)[1]),_D=[0,_B],_E=A(_z,[_D,_]);return _D;};},_F=unCStr("bottom of the page"),_G=new T(function(){return _w(_p,_F);}),_H=unCStr("table"),_I=unCStr("text-align:center"),_J=unCStr("style"),_K=unCStr("vertical-align:top"),_L=[0,3],_M=unCStr("id"),_N=function(_O,_P,_Q,_){var _R=E(_P),_S=A(_O,[_Q,_]),_T=A(_1,[_n,_S,_R[1],_R[2],_]);return _S;},_U=function(_V,_W){while(1){var _X=(function(_Y,_Z){var _10=E(_Z);if(!_10[0]){return E(_Y);}else{_V=function(_11,_){return _N(_Y,_10[1],_11,_);};_W=_10[2];return null;}})(_V,_W);if(_X!=null){return _X;}}},_12=unCStr("span"),_13=function(_14,_15,_){var _16=jsCreateElem(toJSStr(E(_14))),_17=jsAppendChild(_16,E(_15)[1]);return [0,_16];},_18=function(_11,_){return _13(_12,_11,_);},_19=function(_1a,_1b,_){return [0,_0,_1a];},_1c=function(_1d,_){return [0,_1d,_1d];},_1e=[0,coercionToken],_1f=function(_1g,_1h,_){var _1i=A(_1g,[_]);return A(_1h,[_]);},_1j=function(_1k,_1l,_){return _1f(_1k,_1l,_);},_1m=function(_1n,_1o,_){var _1p=A(_1n,[_]);return A(_1o,[_1p,_]);},_1q=unCStr("base"),_1r=unCStr("GHC.IO.Exception"),_1s=unCStr("IOException"),_1t=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1q,_1r,_1s],_1u=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_1t,_9],_1v=function(_1w){return E(_1u);},_1x=function(_1y){return E(E(_1y)[1]);},_1z=unCStr("Maybe.fromJust: Nothing"),_1A=new T(function(){return err(_1z);}),_1B=function(_1C,_1D,_1E){var _1F=new T(function(){var _1G=A(_1C,[_1E]),_1H=A(_1D,[new T(function(){var _1I=E(_1F);return _1I[0]==0?E(_1A):E(_1I[1]);})]),_1J=hs_eqWord64(_1G[1],_1H[1]);if(!E(_1J)){return [0];}else{var _1K=hs_eqWord64(_1G[2],_1H[2]);return E(_1K)==0?[0]:[1,_1E];}});return E(_1F);},_1L=function(_1M){var _1N=E(_1M);return _1B(_1x(_1N[1]),_1v,_1N[2]);},_1O=unCStr(": "),_1P=[0,41],_1Q=unCStr(" ("),_1R=function(_1S,_1T){var _1U=E(_1S);return _1U[0]==0?E(_1T):[1,_1U[1],new T(function(){return _1R(_1U[2],_1T);})];},_1V=unCStr("already exists"),_1W=unCStr("does not exist"),_1X=unCStr("protocol error"),_1Y=unCStr("failed"),_1Z=unCStr("invalid argument"),_20=unCStr("inappropriate type"),_21=unCStr("hardware fault"),_22=unCStr("unsupported operation"),_23=unCStr("timeout"),_24=unCStr("resource vanished"),_25=unCStr("interrupted"),_26=unCStr("resource busy"),_27=unCStr("resource exhausted"),_28=unCStr("end of file"),_29=unCStr("illegal operation"),_2a=unCStr("permission denied"),_2b=unCStr("user error"),_2c=unCStr("unsatisified constraints"),_2d=unCStr("system error"),_2e=function(_2f,_2g){switch(E(_2f)){case 0:return _1R(_1V,_2g);case 1:return _1R(_1W,_2g);case 2:return _1R(_26,_2g);case 3:return _1R(_27,_2g);case 4:return _1R(_28,_2g);case 5:return _1R(_29,_2g);case 6:return _1R(_2a,_2g);case 7:return _1R(_2b,_2g);case 8:return _1R(_2c,_2g);case 9:return _1R(_2d,_2g);case 10:return _1R(_1X,_2g);case 11:return _1R(_1Y,_2g);case 12:return _1R(_1Z,_2g);case 13:return _1R(_20,_2g);case 14:return _1R(_21,_2g);case 15:return _1R(_22,_2g);case 16:return _1R(_23,_2g);case 17:return _1R(_24,_2g);default:return _1R(_25,_2g);}},_2h=[0,125],_2i=unCStr("{handle: "),_2j=function(_2k,_2l,_2m,_2n,_2o,_2p){var _2q=new T(function(){var _2r=new T(function(){return _2e(_2l,new T(function(){var _2s=E(_2n);return _2s[0]==0?E(_2p):_1R(_1Q,new T(function(){return _1R(_2s,[1,_1P,_2p]);}));}));}),_2t=E(_2m);return _2t[0]==0?E(_2r):_1R(_2t,new T(function(){return _1R(_1O,_2r);}));}),_2u=E(_2o);if(!_2u[0]){var _2v=E(_2k);if(!_2v[0]){return E(_2q);}else{var _2w=E(_2v[1]);return _2w[0]==0?_1R(_2i,new T(function(){return _1R(_2w[1],[1,_2h,new T(function(){return _1R(_1O,_2q);})]);})):_1R(_2i,new T(function(){return _1R(_2w[1],[1,_2h,new T(function(){return _1R(_1O,_2q);})]);}));}}else{return _1R(_2u[1],new T(function(){return _1R(_1O,_2q);}));}},_2x=function(_2y){var _2z=E(_2y);return _2j(_2z[1],_2z[2],_2z[3],_2z[4],_2z[6],_9);},_2A=function(_2B,_2C){var _2D=E(_2B);return _2j(_2D[1],_2D[2],_2D[3],_2D[4],_2D[6],_2C);},_2E=[0,44],_2F=[0,93],_2G=[0,91],_2H=function(_2I,_2J,_2K){var _2L=E(_2J);return _2L[0]==0?unAppCStr("[]",_2K):[1,_2G,new T(function(){return A(_2I,[_2L[1],new T(function(){var _2M=function(_2N){var _2O=E(_2N);return _2O[0]==0?E([1,_2F,_2K]):[1,_2E,new T(function(){return A(_2I,[_2O[1],new T(function(){return _2M(_2O[2]);})]);})];};return _2M(_2L[2]);})]);})];},_2P=function(_2Q,_2R){return _2H(_2A,_2Q,_2R);},_2S=function(_2T,_2U,_2V){var _2W=E(_2U);return _2j(_2W[1],_2W[2],_2W[3],_2W[4],_2W[6],_2V);},_2X=[0,_2S,_2x,_2P],_2Y=new T(function(){return [0,_1v,_2X,_2Z,_1L];}),_2Z=function(_30){return [0,_2Y,_30];},_31=7,_32=function(_33){return [0,_a,_31,_9,_33,_a,_a];},_34=function(_35,_){return die(new T(function(){return _2Z(new T(function(){return _32(_35);}));}));},_36=function(_37,_){return _34(_37,_);},_38=function(_39,_){return _39;},_3a=[0,_1m,_1j,_38,_36],_3b=function(_3c){return E(E(_3c)[1]);},_3d=function(_3e,_3f,_3g,_3h){return A(_3b,[_3e,new T(function(){return A(_3f,[_3h]);}),function(_3i){return A(_3g,[new T(function(){return E(E(_3i)[1]);}),new T(function(){return E(E(_3i)[2]);})]);}]);},_3j=function(_3k,_3l,_3m,_3n){return A(_3b,[_3k,new T(function(){return A(_3l,[_3n]);}),function(_3o){return A(_3m,[new T(function(){return E(E(_3o)[2]);})]);}]);},_3p=function(_3q,_3r,_3s,_3t){return _3j(_3q,_3r,_3s,_3t);},_3u=function(_3v){return E(E(_3v)[4]);},_3w=function(_3x,_3y){var _3z=new T(function(){return A(_3u,[_3x,_3y]);});return function(_3A){return E(_3z);};},_3B=function(_3C){return E(E(_3C)[3]);},_3D=function(_3E){var _3F=new T(function(){return _3B(_3E);});return [0,function(_3r,_3s,_3t){return _3d(_3E,_3r,_3s,_3t);},function(_3r,_3s,_3t){return _3p(_3E,_3r,_3s,_3t);},function(_3G,_3H){return A(_3F,[[0,_3G,_3H]]);},function(_3t){return _3w(_3E,_3t);}];},_3I=new T(function(){return _3D(_3a);}),_3J=[0,112],_3K=function(_3L,_3M){var _3N=jsShowI(_3L);return _1R(fromJSStr(_3N),_3M);},_3O=[0,41],_3P=[0,40],_3Q=function(_3R,_3S,_3T){return _3S>=0?_3K(_3S,_3T):_3R<=6?_3K(_3S,_3T):[1,_3P,new T(function(){var _3U=jsShowI(_3S);return _1R(fromJSStr(_3U),[1,_3O,_3T]);})];},_3V=function(_3W,_3X,_3Y,_3Z){var _40=E(_3X);return A(_40[1],[new T(function(){var _41=E(_3W);return E(_3Y);}),function(_42){var _43=new T(function(){return E(E(_42)[2]);});return A(_40[2],[new T(function(){return A(_3Z,[new T(function(){var _44=E(new T(function(){var _45=E(_3W);return [0,coercionToken];})),_46=E(_42);return [0,_46[1],new T(function(){return [0,E(_43)[1]+1|0];}),_46[3],_46[4],_46[5]];})]);}),new T(function(){return A(_40[3],[[1,_3J,new T(function(){return _1R(_3Q(0,E(_43)[1],_9),new T(function(){return E(E(_42)[1]);}));})]]);})]);}]);},_47=new T(function(){return _3V(_1e,_3I,_1c,_19);}),_48=unCStr(" could be found!"),_49=function(_4a){return err(unAppCStr("No element with ID ",new T(function(){return _1R(_4a,_48);})));},_4b=function(_4c,_4d,_){var _4e=E(_4d),_4f=jsFind(toJSStr(_4e)),_4g=E(_4f);if(!_4g[0]){return _49(_4e);}else{var _4h=E(_4g[1]),_4i=jsClearChildren(_4h[1]),_4j=E(_m)[1],_4k=takeMVar(_4j),_4l=A(_4c,[_4k,_]),_4m=E(_4l),_4n=E(_4m[1]),_=putMVar(_4j,_4m[2]),_4o=A(_4n[1],[_4h,_]);return _4n[2];}},_4p=function(_4q,_4r,_4s,_4t,_4u,_4v,_4w,_4x,_){var _4y=E(_4w);return [0,_4y,[0,_4t,_4u,_4v,[0,function(_){return _4b(function(_4z,_){var _4A=A(_4q,[new T(function(){var _4B=E(_4z);return [0,_4B[1],_4u,_4B[3],_4B[4],_4B[5]];}),_]);return [0,[0,_38,E(E(_4A)[1])[2]],_4z];},_4s,_);},function(_4C,_){var _4D=_4b(new T(function(){return A(_4r,[_4C]);}),_4s,_),_4E=E(_4D);return _4E[0]==0?_a:A(_4y[2],[_4E[1],_]);}],_4x]];},_4F=function(_4G,_4H,_4I,_){var _4J=A(_47,[_4I,_]),_4K=E(_4J),_4L=_4K[1],_4M=E(_4K[2]),_4N=_4p(_4G,_4H,_4L,_4M[1],_4M[2],_4M[3],_4M[4],_4M[5],_),_4O=A(_4G,[new T(function(){return E(E(_4N)[2]);}),_]),_4P=E(_4O),_4Q=_4P[2],_4R=E(_4P[1]),_4S=_4R[1],_4T=new T(function(){return _U(_18,[1,[0,_M,_4L],_9]);}),_4U=E(_4R[2]);if(!_4U[0]){return [0,[0,function(_4V,_){var _4W=A(_4S,[_4V,_]),_4X=A(_4T,[_4V,_]);return _4V;},_a],new T(function(){var _4Y=E(_4Q);return [0,_4Y[1],_4Y[2],_4Y[3],new T(function(){return E(E(_4N)[1]);}),_4Y[5]];})];}else{var _4Z=A(_4H,[_4U[1],new T(function(){var _50=E(_4Q);return [0,_50[1],_50[2],_50[3],new T(function(){return E(E(_4N)[1]);}),_50[5]];}),_]),_51=E(_4Z),_52=E(_51[1]);return [0,[0,function(_53,_){var _54=A(_4S,[_53,_]),_55=A(_4T,[_53,_]),_56=A(_52[1],[_55,_]);return _53;},_52[2]],_51[2]];}},_57=[0,112],_58=[1,_57,_9],_59=function(_5a,_5b){var _5c=new T(function(){return A(_5a,[_5b]);});return function(_5d,_){var _5e=jsCreateElem(toJSStr(_58)),_5f=jsAppendChild(_5e,E(_5d)[1]),_5g=[0,_5e],_5h=A(_5c,[_5g,_]);return _5g;};},_5i=function(_5j){return _3Q(0,E(_5j)[1],_9);},_5k=unCStr("br"),_5l=function(_5m,_){var _5n=jsCreateElem(toJSStr(E(_5k))),_5o=jsAppendChild(_5n,E(_5m)[1]);return [0,_5n];},_5p=[1,_0],_5q=unCStr("result: "),_5r=function(_5s){var _5t=new T(function(){return _w(_p,new T(function(){return _5i(_5s);}));});return function(_5u,_){return [0,[0,function(_5v,_){var _5w=_5l(_5v,_),_5x=_p(_5q,_5v,_),_5y=A(_5t,[_5v,_]);return _5v;},_5p],_5u];};},_5z=unCStr(" numbers and append the result using a fold"),_5A=[0,0],_5B=[1,_5A],_5C=[0,_38,_5B],_5D=function(_5E,_){return [0,_5C,_5E];},_5F=function(_5G,_5H,_5I,_){var _5J=_13(_5G,_5I,_),_5K=A(_5H,[_5J,_]);return _5J;},_5L=unCStr("()"),_5M=unCStr("GHC.Tuple"),_5N=unCStr("ghc-prim"),_5O=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5N,_5M,_5L],_5P=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5O,_9],_5Q=function(_5R){return E(_5P);},_5S=unCStr("main"),_5T=unCStr("Haste.Perch"),_5U=unCStr("PerchM"),_5V=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_5S,_5T,_5U],_5W=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_5V,_9],_5X=function(_5Y){return E(_5W);},_5Z=function(_60){var _61=E(_60);return _61[0]==0?[0]:_1R(_61[1],new T(function(){return _5Z(_61[2]);}));},_62=function(_63,_64){var _65=E(_63);if(!_65){return [0,_9,_64];}else{var _66=E(_64);if(!_66[0]){return [0,_9,_9];}else{var _67=new T(function(){var _68=_62(_65-1|0,_66[2]);return [0,_68[1],_68[2]];});return [0,[1,_66[1],new T(function(){return E(E(_67)[1]);})],new T(function(){return E(E(_67)[2]);})];}}},_69=[0,120],_6a=[0,48],_6b=function(_6c){var _6d=new T(function(){var _6e=_62(8,new T(function(){var _6f=md5(toJSStr(E(_6c)));return fromJSStr(_6f);}));return [0,_6e[1],_6e[2]];}),_6g=parseInt([0,toJSStr([1,_6a,[1,_69,new T(function(){return E(E(_6d)[1]);})]])]),_6h=new T(function(){var _6i=_62(8,new T(function(){return E(E(_6d)[2]);}));return [0,_6i[1],_6i[2]];}),_6j=parseInt([0,toJSStr([1,_6a,[1,_69,new T(function(){return E(E(_6h)[1]);})]])]),_6k=hs_mkWord64(_6g,_6j),_6l=parseInt([0,toJSStr([1,_6a,[1,_69,new T(function(){return E(_62(8,new T(function(){return E(E(_6h)[2]);}))[1]);})]])]),_6m=hs_mkWord64(_6l,_6l);return [0,_6k,_6m];},_6n=function(_6o,_6p){var _6q=E(_6p);return _6q[0]==0?[0]:[1,new T(function(){return A(_6o,[_6q[1]]);}),new T(function(){return _6n(_6o,_6q[2]);})];},_6r=function(_6s,_6t){var _6u=jsShowI(_6s),_6v=md5(_6u);return _1R(fromJSStr(_6v),new T(function(){var _6w=jsShowI(_6t),_6x=md5(_6w);return fromJSStr(_6x);}));},_6y=function(_6z){var _6A=E(_6z);return _6r(_6A[1],_6A[2]);},_6B=function(_6C){var _6D=E(_6C);if(!_6D[0]){return [0];}else{var _6E=E(_6D[1]);return [1,[0,_6E[1],_6E[2]],new T(function(){return _6B(_6D[2]);})];}},_6F=unCStr("Prelude.undefined"),_6G=new T(function(){return err(_6F);}),_6H=function(_6I,_6J){return function(_6K){return E(new T(function(){var _6L=A(_6I,[_6G]),_6M=E(_6L[3]),_6N=_6M[1],_6O=_6M[2],_6P=_1R(_6L[4],[1,new T(function(){return A(_6J,[_6G]);}),_9]);if(!_6P[0]){return [0,_6N,_6O,_6M,_9];}else{var _6Q=_6b(new T(function(){return _5Z(_6n(_6y,[1,[0,_6N,_6O],new T(function(){return _6B(_6P);})]));}));return [0,_6Q[1],_6Q[2],_6M,_6P];}}));};},_6R=new T(function(){return _6H(_5X,_5Q);}),_6S=unCStr("value"),_6T=unCStr("onclick"),_6U=unCStr("checked"),_6V=[0,_6U,_9],_6W=[1,_6V,_9],_6X=unCStr("type"),_6Y=unCStr("input"),_6Z=function(_70,_){return _13(_6Y,_70,_);},_71=function(_72,_73,_74,_75,_76){var _77=new T(function(){var _78=new T(function(){return _U(_6Z,[1,[0,_6X,_73],[1,[0,_M,_72],[1,[0,_6S,_74],_9]]]);});return !E(_75)?E(_78):_U(_78,_6W);}),_79=E(_76);return _79[0]==0?E(_77):_U(_77,[1,[0,_6T,_79[1]],_9]);},_7a=unCStr("href"),_7b=[0,97],_7c=[1,_7b,_9],_7d=function(_7e,_){return _13(_7c,_7e,_);},_7f=function(_7g,_7h){var _7i=new T(function(){return _U(_7d,[1,[0,_7a,_7g],_9]);});return function(_7j,_){var _7k=A(_7i,[_7j,_]),_7l=A(_7h,[_7k,_]);return _7k;};},_7m=function(_7n){return _7f(_7n,function(_11,_){return _p(_7n,_11,_);});},_7o=unCStr("option"),_7p=function(_7q,_){return _13(_7o,_7q,_);},_7r=unCStr("selected"),_7s=[0,_7r,_9],_7t=[1,_7s,_9],_7u=function(_7v,_7w,_7x){var _7y=new T(function(){return _U(_7p,[1,[0,_6S,_7v],_9]);}),_7z=function(_7A,_){var _7B=A(_7y,[_7A,_]),_7C=A(_7w,[_7B,_]);return _7B;};return !E(_7x)?E(_7z):_U(_7z,_7t);},_7D=function(_7E,_7F){return _7u(_7E,function(_11,_){return _p(_7E,_11,_);},_7F);},_7G=unCStr("method"),_7H=unCStr("action"),_7I=unCStr("UTF-8"),_7J=unCStr("acceptCharset"),_7K=[0,_7J,_7I],_7L=unCStr("form"),_7M=function(_7N,_){return _13(_7L,_7N,_);},_7O=function(_7P,_7Q,_7R){var _7S=new T(function(){return _U(_7M,[1,_7K,[1,[0,_7H,_7P],[1,[0,_7G,_7Q],_9]]]);});return function(_7T,_){var _7U=A(_7S,[_7T,_]),_7V=A(_7R,[_7U,_]);return _7U;};},_7W=unCStr("select"),_7X=function(_7Y,_){return _13(_7W,_7Y,_);},_7Z=function(_80,_81){var _82=new T(function(){return _U(_7X,[1,[0,_M,_80],_9]);});return function(_83,_){var _84=A(_82,[_83,_]),_85=A(_81,[_84,_]);return _84;};},_86=unCStr("textarea"),_87=function(_88,_){return _13(_86,_88,_);},_89=function(_8a,_8b){var _8c=new T(function(){return _U(_87,[1,[0,_M,_8a],_9]);});return function(_8d,_){var _8e=A(_8c,[_8d,_]),_8f=_p(_8b,_8e,_);return _8e;};},_8g=unCStr("color:red"),_8h=unCStr("style"),_8i=[0,_8h,_8g],_8j=[1,_8i,_9],_8k=[0,98],_8l=[1,_8k,_9],_8m=function(_8n){return _U(function(_8o,_){var _8p=_13(_8l,_8o,_),_8q=A(_8n,[_8p,_]);return _8p;},_8j);},_8r=unCStr("toByteString not defined"),_8s=new T(function(){return err(_8r);}),_8t=function(_8u,_8v,_){var _8w=E(_8u);if(!_8w[0]){return _8v;}else{var _8x=A(_8w[1],[_8v,_]),_8y=_8t(_8w[2],_8v,_);return _8v;}},_8z=function(_8A,_8B,_8C,_){var _8D=A(_8A,[_8C,_]),_8E=A(_8B,[_8C,_]);return _8C;},_8F=[0,_38,_8z,_8t],_8G=[0,_8F,_6R,_8s,_p,_p,_5F,_8m,_7f,_7m,_71,_89,_7Z,_7u,_7D,_7O,_U],_8H=function(_8I,_8J,_){var _8K=A(_8J,[_]);return _8I;},_8L=function(_8M,_8N,_){var _8O=A(_8N,[_]);return new T(function(){return A(_8M,[_8O]);});},_8P=[0,_8L,_8H],_8Q=function(_8R){var _8S=E(_8R);return _8S[0]==0?0:E(_8S[1])[1]+_8Q(_8S[2])|0;},_8T=function(_8U){return [0,_8Q(_8U)];},_8V=function(_8W,_8X){return [0,E(_8W)[1]+E(_8X)[1]|0];},_8Y=[0,_5A,_8V,_8T],_8Z=function(_90,_91){var _92=E(_91);return _92[0]==0?[0]:[1,new T(function(){return A(_90,[_92[1]]);})];},_93=function(_94){return E(E(_94)[1]);},_95=function(_96){return E(E(_96)[2]);},_97=function(_98,_99,_9a,_9b,_9c,_9d){var _9e=new T(function(){return _95(_98);});return A(_99,[new T(function(){return A(_9b,[_9d]);}),function(_9f){var _9g=E(_9f),_9h=E(_9g[1]);return A(_99,[new T(function(){return A(_9c,[_9g[2]]);}),function(_9i){var _9j=E(_9i),_9k=E(_9j[1]);return A(_9a,[[0,[0,new T(function(){return A(_9e,[_9h[1],_9k[1]]);}),new T(function(){var _9l=E(_9h[2]);if(!_9l[0]){return [0];}else{var _9m=E(_9k[2]);return _9m[0]==0?[0]:[1,new T(function(){return A(_9l[1],[_9m[1]]);})];}})],_9j[2]]]);}]);}]);},_9n=function(_9o){return E(E(_9o)[1]);},_9p=function(_9q,_9r,_9s,_9t,_9u,_9v){var _9w=new T(function(){return _93(_9q);});return function(_9x){var _9y=E(_9r);return _97(_9w,_9y[1],_9y[3],function(_9z){return A(new T(function(){var _9A=new T(function(){return _95(_9t);});return A(_9n,[_9s,function(_9B){return [0,new T(function(){var _9C=E(E(_9B)[1]);return [0,_9C[1],new T(function(){return _8Z(_9A,_9C[2]);})];}),new T(function(){return E(E(_9B)[2]);})];}]);}),[new T(function(){return A(_9u,[_9z]);})]);},_9v,_9x);};},_9D=function(_9E,_9F){while(1){var _9G=(function(_9H,_9I){var _9J=E(_9I);if(!_9J[0]){return E(_9H);}else{_9E=new T(function(){return _9p(_8G,_3a,_8P,_8Y,_9H,_9J[1]);});_9F=_9J[2];return null;}})(_9E,_9F);if(_9G!=null){return _9G;}}},_9K=[13,coercionToken],_9L=unCStr("text"),_9M=[0,_3a,_n],_9N=unCStr("base"),_9O=unCStr("Control.Exception.Base"),_9P=unCStr("PatternMatchFail"),_9Q=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9N,_9O,_9P],_9R=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9Q,_9],_9S=function(_9T){return E(_9R);},_9U=function(_9V){var _9W=E(_9V);return _1B(_1x(_9W[1]),_9S,_9W[2]);},_9X=function(_9Y){return E(E(_9Y)[1]);},_9Z=function(_a0,_a1){return _1R(E(_a0)[1],_a1);},_a2=function(_a3,_a4){return _2H(_9Z,_a3,_a4);},_a5=function(_a6,_a7,_a8){return _1R(E(_a7)[1],_a8);},_a9=[0,_a5,_9X,_a2],_aa=new T(function(){return [0,_9S,_a9,_ab,_9U];}),_ab=function(_ac){return [0,_aa,_ac];},_ad=unCStr("Non-exhaustive patterns in"),_ae=function(_af,_ag){return die(new T(function(){return A(_ag,[_af]);}));},_ah=function(_ai,_aj){var _ak=E(_aj);if(!_ak[0]){return [0,_9,_9];}else{var _al=_ak[1];if(!A(_ai,[_al])){return [0,_9,_ak];}else{var _am=new T(function(){var _an=_ah(_ai,_ak[2]);return [0,_an[1],_an[2]];});return [0,[1,_al,new T(function(){return E(E(_am)[1]);})],new T(function(){return E(E(_am)[2]);})];}}},_ao=[0,32],_ap=[0,10],_aq=[1,_ap,_9],_ar=function(_as){return E(E(_as)[1])==124?false:true;},_at=function(_au,_av){var _aw=_ah(_ar,unCStr(_au)),_ax=_aw[1],_ay=function(_az,_aA){return _1R(_az,new T(function(){return unAppCStr(": ",new T(function(){return _1R(_av,new T(function(){return _1R(_aA,_aq);}));}));}));},_aB=E(_aw[2]);return _aB[0]==0?_ay(_ax,_9):E(E(_aB[1])[1])==124?_ay(_ax,[1,_ao,_aB[2]]):_ay(_ax,_9);},_aC=function(_aD){return _ae([0,new T(function(){return _at(_aD,_ad);})],_ab);},_aE=new T(function(){return _aC("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_aF=function(_aG,_aH){while(1){var _aI=(function(_aJ,_aK){var _aL=E(_aJ);switch(_aL[0]){case 0:var _aM=E(_aK);if(!_aM[0]){return [0];}else{_aG=A(_aL[1],[_aM[1]]);_aH=_aM[2];return null;}break;case 1:var _aN=A(_aL[1],[_aK]),_aO=_aK;_aG=_aN;_aH=_aO;return null;case 2:return [0];case 3:return [1,[0,_aL[1],_aK],new T(function(){return _aF(_aL[2],_aK);})];default:return E(_aL[1]);}})(_aG,_aH);if(_aI!=null){return _aI;}}},_aP=function(_aQ,_aR){var _aS=new T(function(){var _aT=E(_aR);if(_aT[0]==3){return [3,_aT[1],new T(function(){return _aP(_aQ,_aT[2]);})];}else{var _aU=E(_aQ);if(_aU[0]==2){return E(_aT);}else{var _aV=E(_aT);if(_aV[0]==2){return E(_aU);}else{var _aW=new T(function(){var _aX=E(_aV);if(_aX[0]==4){return [1,function(_aY){return [4,new T(function(){return _1R(_aF(_aU,_aY),_aX[1]);})];}];}else{var _aZ=E(_aU);if(_aZ[0]==1){var _b0=_aZ[1],_b1=E(_aX);return _b1[0]==0?[1,function(_b2){return _aP(A(_b0,[_b2]),_b1);}]:[1,function(_b3){return _aP(A(_b0,[_b3]),new T(function(){return A(_b1[1],[_b3]);}));}];}else{var _b4=E(_aX);return _b4[0]==0?E(_aE):[1,function(_b5){return _aP(_aZ,new T(function(){return A(_b4[1],[_b5]);}));}];}}}),_b6=E(_aU);switch(_b6[0]){case 1:var _b7=E(_aV);return _b7[0]==4?[1,function(_b8){return [4,new T(function(){return _1R(_aF(A(_b6[1],[_b8]),_b8),_b7[1]);})];}]:E(_aW);case 4:var _b9=_b6[1],_ba=E(_aV);switch(_ba[0]){case 0:return [1,function(_bb){return [4,new T(function(){return _1R(_b9,new T(function(){return _aF(_ba,_bb);}));})];}];case 1:return [1,function(_bc){return [4,new T(function(){return _1R(_b9,new T(function(){return _aF(A(_ba[1],[_bc]),_bc);}));})];}];default:return [4,new T(function(){return _1R(_b9,_ba[1]);})];}break;default:return E(_aW);}}}}}),_bd=E(_aQ);switch(_bd[0]){case 0:var _be=E(_aR);return _be[0]==0?[0,function(_bf){return _aP(A(_bd[1],[_bf]),new T(function(){return A(_be[1],[_bf]);}));}]:E(_aS);case 3:return [3,_bd[1],new T(function(){return _aP(_bd[2],_aR);})];default:return E(_aS);}},_bg=function(_bh,_bi){return E(_bh)[1]!=E(_bi)[1];},_bj=function(_bk,_bl){return E(_bk)[1]==E(_bl)[1];},_bm=[0,_bj,_bg],_bn=function(_bo){return E(E(_bo)[1]);},_bp=function(_bq,_br,_bs){while(1){var _bt=E(_br);if(!_bt[0]){return E(_bs)[0]==0?true:false;}else{var _bu=E(_bs);if(!_bu[0]){return false;}else{if(!A(_bn,[_bq,_bt[1],_bu[1]])){return false;}else{_br=_bt[2];_bs=_bu[2];continue;}}}}},_bv=function(_bw,_bx,_by){return !_bp(_bw,_bx,_by)?true:false;},_bz=function(_bA){return [0,function(_bB,_bC){return _bp(_bA,_bB,_bC);},function(_bB,_bC){return _bv(_bA,_bB,_bC);}];},_bD=new T(function(){return _bz(_bm);}),_bE=function(_bF,_bG){var _bH=E(_bF);switch(_bH[0]){case 0:return [0,function(_bI){return _bE(A(_bH[1],[_bI]),_bG);}];case 1:return [1,function(_bJ){return _bE(A(_bH[1],[_bJ]),_bG);}];case 2:return [2];case 3:return _aP(A(_bG,[_bH[1]]),new T(function(){return _bE(_bH[2],_bG);}));default:var _bK=function(_bL){var _bM=E(_bL);if(!_bM[0]){return [0];}else{var _bN=E(_bM[1]);return _1R(_aF(A(_bG,[_bN[1]]),_bN[2]),new T(function(){return _bK(_bM[2]);}));}},_bO=_bK(_bH[1]);return _bO[0]==0?[2]:[4,_bO];}},_bP=[2],_bQ=function(_bR){return [3,_bR,_bP];},_bS=function(_bT,_bU){var _bV=E(_bT);if(!_bV){return A(_bU,[_0]);}else{var _bW=new T(function(){return _bS(_bV-1|0,_bU);});return [0,function(_bX){return E(_bW);}];}},_bY=function(_bZ,_c0,_c1){var _c2=new T(function(){return A(_bZ,[_bQ]);});return [1,function(_c3){return A(function(_c4,_c5,_c6){while(1){var _c7=(function(_c8,_c9,_ca){var _cb=E(_c8);switch(_cb[0]){case 0:var _cc=E(_c9);if(!_cc[0]){return E(_c0);}else{_c4=A(_cb[1],[_cc[1]]);_c5=_cc[2];var _cd=_ca+1|0;_c6=_cd;return null;}break;case 1:var _ce=A(_cb[1],[_c9]),_cf=_c9,_cd=_ca;_c4=_ce;_c5=_cf;_c6=_cd;return null;case 2:return E(_c0);case 3:return function(_cg){var _ch=new T(function(){return _bE(_cb,_cg);});return _bS(_ca,function(_ci){return E(_ch);});};default:return function(_cj){return _bE(_cb,_cj);};}})(_c4,_c5,_c6);if(_c7!=null){return _c7;}}},[_c2,_c3,0,_c1]);}];},_ck=[6],_cl=unCStr("valDig: Bad base"),_cm=new T(function(){return err(_cl);}),_cn=function(_co,_cp){var _cq=function(_cr,_cs){var _ct=E(_cr);if(!_ct[0]){var _cu=new T(function(){return A(_cs,[_9]);});return function(_cv){return A(_cv,[_cu]);};}else{var _cw=E(_ct[1])[1],_cx=function(_cy){var _cz=new T(function(){return _cq(_ct[2],function(_cA){return A(_cs,[[1,_cy,_cA]]);});});return function(_cB){var _cC=new T(function(){return A(_cz,[_cB]);});return [0,function(_cD){return E(_cC);}];};};switch(E(E(_co)[1])){case 8:if(48>_cw){var _cE=new T(function(){return A(_cs,[_9]);});return function(_cF){return A(_cF,[_cE]);};}else{if(_cw>55){var _cG=new T(function(){return A(_cs,[_9]);});return function(_cH){return A(_cH,[_cG]);};}else{return _cx([0,_cw-48|0]);}}break;case 10:if(48>_cw){var _cI=new T(function(){return A(_cs,[_9]);});return function(_cJ){return A(_cJ,[_cI]);};}else{if(_cw>57){var _cK=new T(function(){return A(_cs,[_9]);});return function(_cL){return A(_cL,[_cK]);};}else{return _cx([0,_cw-48|0]);}}break;case 16:var _cM=new T(function(){return 97>_cw?65>_cw?[0]:_cw>70?[0]:[1,[0,(_cw-65|0)+10|0]]:_cw>102?65>_cw?[0]:_cw>70?[0]:[1,[0,(_cw-65|0)+10|0]]:[1,[0,(_cw-97|0)+10|0]];});if(48>_cw){var _cN=E(_cM);if(!_cN[0]){var _cO=new T(function(){return A(_cs,[_9]);});return function(_cP){return A(_cP,[_cO]);};}else{return _cx(_cN[1]);}}else{if(_cw>57){var _cQ=E(_cM);if(!_cQ[0]){var _cR=new T(function(){return A(_cs,[_9]);});return function(_cS){return A(_cS,[_cR]);};}else{return _cx(_cQ[1]);}}else{return _cx([0,_cw-48|0]);}}break;default:return E(_cm);}}};return [1,function(_cT){return A(_cq,[_cT,_n,function(_cU){var _cV=E(_cU);return _cV[0]==0?[2]:A(_cp,[_cV]);}]);}];},_cW=[0,10],_cX=[0,1],_cY=[0,2147483647],_cZ=function(_d0,_d1){while(1){var _d2=E(_d0);if(!_d2[0]){var _d3=_d2[1],_d4=E(_d1);if(!_d4[0]){var _d5=_d4[1],_d6=addC(_d3,_d5);if(!E(_d6[2])){return [0,_d6[1]];}else{_d0=[1,I_fromInt(_d3)];_d1=[1,I_fromInt(_d5)];continue;}}else{_d0=[1,I_fromInt(_d3)];_d1=_d4;continue;}}else{var _d7=E(_d1);if(!_d7[0]){_d0=_d2;_d1=[1,I_fromInt(_d7[1])];continue;}else{return [1,I_add(_d2[1],_d7[1])];}}}},_d8=new T(function(){return _cZ(_cY,_cX);}),_d9=function(_da){var _db=E(_da);if(!_db[0]){var _dc=E(_db[1]);return _dc==(-2147483648)?E(_d8):[0, -_dc];}else{return [1,I_negate(_db[1])];}},_dd=[0,10],_de=[0,0],_df=function(_dg,_dh){while(1){var _di=E(_dg);if(!_di[0]){var _dj=_di[1],_dk=E(_dh);if(!_dk[0]){var _dl=_dk[1];if(!(imul(_dj,_dl)|0)){return [0,imul(_dj,_dl)|0];}else{_dg=[1,I_fromInt(_dj)];_dh=[1,I_fromInt(_dl)];continue;}}else{_dg=[1,I_fromInt(_dj)];_dh=_dk;continue;}}else{var _dm=E(_dh);if(!_dm[0]){_dg=_di;_dh=[1,I_fromInt(_dm[1])];continue;}else{return [1,I_mul(_di[1],_dm[1])];}}}},_dn=function(_do,_dp,_dq){while(1){var _dr=E(_dq);if(!_dr[0]){return E(_dp);}else{var _ds=_cZ(_df(_dp,_do),_dr[1]);_dq=_dr[2];_dp=_ds;continue;}}},_dt=function(_du){var _dv=new T(function(){return _aP(_aP([0,function(_dw){return E(E(_dw)[1])==45?_cn(_cW,function(_dx){return A(_du,[[1,new T(function(){return _d9(_dn(_dd,_de,_dx));})]]);}):[2];}],[0,function(_dy){return E(E(_dy)[1])==43?_cn(_cW,function(_dz){return A(_du,[[1,new T(function(){return _dn(_dd,_de,_dz);})]]);}):[2];}]),new T(function(){return _cn(_cW,function(_dA){return A(_du,[[1,new T(function(){return _dn(_dd,_de,_dA);})]]);});}));});return _aP([0,function(_dB){return E(E(_dB)[1])==101?E(_dv):[2];}],[0,function(_dC){return E(E(_dC)[1])==69?E(_dv):[2];}]);},_dD=function(_dE){return A(_dE,[_a]);},_dF=function(_dG){return A(_dG,[_a]);},_dH=function(_dI){var _dJ=new T(function(){return _cn(_cW,function(_dK){return A(_dI,[[1,_dK]]);});});return [0,function(_dL){return E(E(_dL)[1])==46?E(_dJ):[2];}];},_dM=function(_dN){return _cn(_cW,function(_dO){return _bY(_dH,_dD,function(_dP){return _bY(_dt,_dF,function(_dQ){return A(_dN,[[5,[1,_dO,_dP,_dQ]]]);});});});},_dR=function(_dS,_dT,_dU){while(1){var _dV=E(_dU);if(!_dV[0]){return false;}else{if(!A(_bn,[_dS,_dT,_dV[1]])){_dU=_dV[2];continue;}else{return true;}}}},_dW=unCStr("!@#$%&*+./<=>?\\^|:-~"),_dX=function(_dY){return _dR(_bm,_dY,_dW);},_dZ=[0,8],_e0=[0,16],_e1=function(_e2){var _e3=new T(function(){return _cn(_e0,function(_e4){return A(_e2,[[5,[0,_e0,_e4]]]);});}),_e5=new T(function(){return _cn(_dZ,function(_e6){return A(_e2,[[5,[0,_dZ,_e6]]]);});}),_e7=new T(function(){return _cn(_e0,function(_e8){return A(_e2,[[5,[0,_e0,_e8]]]);});}),_e9=new T(function(){return _cn(_dZ,function(_ea){return A(_e2,[[5,[0,_dZ,_ea]]]);});});return [0,function(_eb){return E(E(_eb)[1])==48?E([0,function(_ec){switch(E(E(_ec)[1])){case 79:return E(_e9);case 88:return E(_e7);case 111:return E(_e5);case 120:return E(_e3);default:return [2];}}]):[2];}];},_ed=false,_ee=true,_ef=function(_eg){var _eh=new T(function(){return A(_eg,[_e0]);}),_ei=new T(function(){return A(_eg,[_dZ]);}),_ej=new T(function(){return A(_eg,[_e0]);}),_ek=new T(function(){return A(_eg,[_dZ]);});return [0,function(_el){switch(E(E(_el)[1])){case 79:return E(_ek);case 88:return E(_ej);case 111:return E(_ei);case 120:return E(_eh);default:return [2];}}];},_em=function(_en){return A(_en,[_cW]);},_eo=function(_ep){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _3Q(9,_ep,_9);})));},_eq=function(_er){var _es=E(_er);return _es[0]==0?E(_es[1]):I_toInt(_es[1]);},_et=function(_eu,_ev){var _ew=E(_eu);if(!_ew[0]){var _ex=_ew[1],_ey=E(_ev);return _ey[0]==0?_ex<=_ey[1]:I_compareInt(_ey[1],_ex)>=0;}else{var _ez=_ew[1],_eA=E(_ev);return _eA[0]==0?I_compareInt(_ez,_eA[1])<=0:I_compare(_ez,_eA[1])<=0;}},_eB=function(_eC){return [2];},_eD=function(_eE){var _eF=E(_eE);if(!_eF[0]){return E(_eB);}else{var _eG=_eF[1],_eH=E(_eF[2]);if(!_eH[0]){return E(_eG);}else{var _eI=new T(function(){return _eD(_eH);});return function(_eJ){return _aP(A(_eG,[_eJ]),new T(function(){return A(_eI,[_eJ]);}));};}}},_eK=unCStr("NUL"),_eL=function(_eM){return [2];},_eN=function(_eO){return _eL(_eO);},_eP=function(_eQ,_eR){var _eS=function(_eT,_eU){var _eV=E(_eT);if(!_eV[0]){return function(_eW){return A(_eW,[_eQ]);};}else{var _eX=E(_eU);if(!_eX[0]){return E(_eL);}else{if(E(_eV[1])[1]!=E(_eX[1])[1]){return E(_eN);}else{var _eY=new T(function(){return _eS(_eV[2],_eX[2]);});return function(_eZ){var _f0=new T(function(){return A(_eY,[_eZ]);});return [0,function(_f1){return E(_f0);}];};}}}};return [1,function(_f2){return A(_eS,[_eQ,_f2,_eR]);}];},_f3=[0,0],_f4=function(_f5){var _f6=new T(function(){return A(_f5,[_f3]);});return _eP(_eK,function(_f7){return E(_f6);});},_f8=unCStr("STX"),_f9=[0,2],_fa=function(_fb){var _fc=new T(function(){return A(_fb,[_f9]);});return _eP(_f8,function(_fd){return E(_fc);});},_fe=unCStr("ETX"),_ff=[0,3],_fg=function(_fh){var _fi=new T(function(){return A(_fh,[_ff]);});return _eP(_fe,function(_fj){return E(_fi);});},_fk=unCStr("EOT"),_fl=[0,4],_fm=function(_fn){var _fo=new T(function(){return A(_fn,[_fl]);});return _eP(_fk,function(_fp){return E(_fo);});},_fq=unCStr("ENQ"),_fr=[0,5],_fs=function(_ft){var _fu=new T(function(){return A(_ft,[_fr]);});return _eP(_fq,function(_fv){return E(_fu);});},_fw=unCStr("ACK"),_fx=[0,6],_fy=function(_fz){var _fA=new T(function(){return A(_fz,[_fx]);});return _eP(_fw,function(_fB){return E(_fA);});},_fC=unCStr("BEL"),_fD=[0,7],_fE=function(_fF){var _fG=new T(function(){return A(_fF,[_fD]);});return _eP(_fC,function(_fH){return E(_fG);});},_fI=unCStr("BS"),_fJ=[0,8],_fK=function(_fL){var _fM=new T(function(){return A(_fL,[_fJ]);});return _eP(_fI,function(_fN){return E(_fM);});},_fO=unCStr("HT"),_fP=[0,9],_fQ=function(_fR){var _fS=new T(function(){return A(_fR,[_fP]);});return _eP(_fO,function(_fT){return E(_fS);});},_fU=unCStr("LF"),_fV=[0,10],_fW=function(_fX){var _fY=new T(function(){return A(_fX,[_fV]);});return _eP(_fU,function(_fZ){return E(_fY);});},_g0=unCStr("VT"),_g1=[0,11],_g2=function(_g3){var _g4=new T(function(){return A(_g3,[_g1]);});return _eP(_g0,function(_g5){return E(_g4);});},_g6=unCStr("FF"),_g7=[0,12],_g8=function(_g9){var _ga=new T(function(){return A(_g9,[_g7]);});return _eP(_g6,function(_gb){return E(_ga);});},_gc=unCStr("CR"),_gd=[0,13],_ge=function(_gf){var _gg=new T(function(){return A(_gf,[_gd]);});return _eP(_gc,function(_gh){return E(_gg);});},_gi=unCStr("SI"),_gj=[0,15],_gk=function(_gl){var _gm=new T(function(){return A(_gl,[_gj]);});return _eP(_gi,function(_gn){return E(_gm);});},_go=unCStr("DLE"),_gp=[0,16],_gq=function(_gr){var _gs=new T(function(){return A(_gr,[_gp]);});return _eP(_go,function(_gt){return E(_gs);});},_gu=unCStr("DC1"),_gv=[0,17],_gw=function(_gx){var _gy=new T(function(){return A(_gx,[_gv]);});return _eP(_gu,function(_gz){return E(_gy);});},_gA=unCStr("DC2"),_gB=[0,18],_gC=function(_gD){var _gE=new T(function(){return A(_gD,[_gB]);});return _eP(_gA,function(_gF){return E(_gE);});},_gG=unCStr("DC3"),_gH=[0,19],_gI=function(_gJ){var _gK=new T(function(){return A(_gJ,[_gH]);});return _eP(_gG,function(_gL){return E(_gK);});},_gM=unCStr("DC4"),_gN=[0,20],_gO=function(_gP){var _gQ=new T(function(){return A(_gP,[_gN]);});return _eP(_gM,function(_gR){return E(_gQ);});},_gS=unCStr("NAK"),_gT=[0,21],_gU=function(_gV){var _gW=new T(function(){return A(_gV,[_gT]);});return _eP(_gS,function(_gX){return E(_gW);});},_gY=unCStr("SYN"),_gZ=[0,22],_h0=function(_h1){var _h2=new T(function(){return A(_h1,[_gZ]);});return _eP(_gY,function(_h3){return E(_h2);});},_h4=unCStr("ETB"),_h5=[0,23],_h6=function(_h7){var _h8=new T(function(){return A(_h7,[_h5]);});return _eP(_h4,function(_h9){return E(_h8);});},_ha=unCStr("CAN"),_hb=[0,24],_hc=function(_hd){var _he=new T(function(){return A(_hd,[_hb]);});return _eP(_ha,function(_hf){return E(_he);});},_hg=unCStr("EM"),_hh=[0,25],_hi=function(_hj){var _hk=new T(function(){return A(_hj,[_hh]);});return _eP(_hg,function(_hl){return E(_hk);});},_hm=unCStr("SUB"),_hn=[0,26],_ho=function(_hp){var _hq=new T(function(){return A(_hp,[_hn]);});return _eP(_hm,function(_hr){return E(_hq);});},_hs=unCStr("ESC"),_ht=[0,27],_hu=function(_hv){var _hw=new T(function(){return A(_hv,[_ht]);});return _eP(_hs,function(_hx){return E(_hw);});},_hy=unCStr("FS"),_hz=[0,28],_hA=function(_hB){var _hC=new T(function(){return A(_hB,[_hz]);});return _eP(_hy,function(_hD){return E(_hC);});},_hE=unCStr("GS"),_hF=[0,29],_hG=function(_hH){var _hI=new T(function(){return A(_hH,[_hF]);});return _eP(_hE,function(_hJ){return E(_hI);});},_hK=unCStr("RS"),_hL=[0,30],_hM=function(_hN){var _hO=new T(function(){return A(_hN,[_hL]);});return _eP(_hK,function(_hP){return E(_hO);});},_hQ=unCStr("US"),_hR=[0,31],_hS=function(_hT){var _hU=new T(function(){return A(_hT,[_hR]);});return _eP(_hQ,function(_hV){return E(_hU);});},_hW=unCStr("SP"),_hX=[0,32],_hY=function(_hZ){var _i0=new T(function(){return A(_hZ,[_hX]);});return _eP(_hW,function(_i1){return E(_i0);});},_i2=unCStr("DEL"),_i3=[0,127],_i4=function(_i5){var _i6=new T(function(){return A(_i5,[_i3]);});return _eP(_i2,function(_i7){return E(_i6);});},_i8=[1,_i4,_9],_i9=[1,_hY,_i8],_ia=[1,_hS,_i9],_ib=[1,_hM,_ia],_ic=[1,_hG,_ib],_id=[1,_hA,_ic],_ie=[1,_hu,_id],_if=[1,_ho,_ie],_ig=[1,_hi,_if],_ih=[1,_hc,_ig],_ii=[1,_h6,_ih],_ij=[1,_h0,_ii],_ik=[1,_gU,_ij],_il=[1,_gO,_ik],_im=[1,_gI,_il],_in=[1,_gC,_im],_io=[1,_gw,_in],_ip=[1,_gq,_io],_iq=[1,_gk,_ip],_ir=[1,_ge,_iq],_is=[1,_g8,_ir],_it=[1,_g2,_is],_iu=[1,_fW,_it],_iv=[1,_fQ,_iu],_iw=[1,_fK,_iv],_ix=[1,_fE,_iw],_iy=[1,_fy,_ix],_iz=[1,_fs,_iy],_iA=[1,_fm,_iz],_iB=[1,_fg,_iA],_iC=[1,_fa,_iB],_iD=[1,_f4,_iC],_iE=unCStr("SOH"),_iF=[0,1],_iG=function(_iH){var _iI=new T(function(){return A(_iH,[_iF]);});return _eP(_iE,function(_iJ){return E(_iI);});},_iK=unCStr("SO"),_iL=[0,14],_iM=function(_iN){var _iO=new T(function(){return A(_iN,[_iL]);});return _eP(_iK,function(_iP){return E(_iO);});},_iQ=function(_iR){return _bY(_iG,_iM,_iR);},_iS=[1,_iQ,_iD],_iT=new T(function(){return _eD(_iS);}),_iU=[0,1114111],_iV=[0,34],_iW=[0,_iV,_ee],_iX=[0,39],_iY=[0,_iX,_ee],_iZ=[0,92],_j0=[0,_iZ,_ee],_j1=[0,_fD,_ee],_j2=[0,_fJ,_ee],_j3=[0,_g7,_ee],_j4=[0,_fV,_ee],_j5=[0,_gd,_ee],_j6=[0,_fP,_ee],_j7=[0,_g1,_ee],_j8=[0,_f3,_ee],_j9=[0,_iF,_ee],_ja=[0,_f9,_ee],_jb=[0,_ff,_ee],_jc=[0,_fl,_ee],_jd=[0,_fr,_ee],_je=[0,_fx,_ee],_jf=[0,_fD,_ee],_jg=[0,_fJ,_ee],_jh=[0,_fP,_ee],_ji=[0,_fV,_ee],_jj=[0,_g1,_ee],_jk=[0,_g7,_ee],_jl=[0,_gd,_ee],_jm=[0,_iL,_ee],_jn=[0,_gj,_ee],_jo=[0,_gp,_ee],_jp=[0,_gv,_ee],_jq=[0,_gB,_ee],_jr=[0,_gH,_ee],_js=[0,_gN,_ee],_jt=[0,_gT,_ee],_ju=[0,_gZ,_ee],_jv=[0,_h5,_ee],_jw=[0,_hb,_ee],_jx=[0,_hh,_ee],_jy=[0,_hn,_ee],_jz=[0,_ht,_ee],_jA=[0,_hz,_ee],_jB=[0,_hF,_ee],_jC=[0,_hL,_ee],_jD=[0,_hR,_ee],_jE=function(_jF){return [0,_jF];},_jG=function(_jH){var _jI=new T(function(){return A(_jH,[_j7]);}),_jJ=new T(function(){return A(_jH,[_j6]);}),_jK=new T(function(){return A(_jH,[_j5]);}),_jL=new T(function(){return A(_jH,[_j4]);}),_jM=new T(function(){return A(_jH,[_j3]);}),_jN=new T(function(){return A(_jH,[_j2]);}),_jO=new T(function(){return A(_jH,[_j1]);}),_jP=new T(function(){return A(_jH,[_j0]);}),_jQ=new T(function(){return A(_jH,[_iY]);}),_jR=new T(function(){return A(_jH,[_iW]);});return _aP([0,function(_jS){switch(E(E(_jS)[1])){case 34:return E(_jR);case 39:return E(_jQ);case 92:return E(_jP);case 97:return E(_jO);case 98:return E(_jN);case 102:return E(_jM);case 110:return E(_jL);case 114:return E(_jK);case 116:return E(_jJ);case 118:return E(_jI);default:return [2];}}],new T(function(){return _aP(_bY(_ef,_em,function(_jT){var _jU=new T(function(){return _jE(E(_jT)[1]);});return _cn(_jT,function(_jV){var _jW=_dn(_jU,_de,_jV);return !_et(_jW,_iU)?[2]:A(_jH,[[0,new T(function(){var _jX=_eq(_jW);return _jX>>>0>1114111?_eo(_jX):[0,_jX];}),_ee]]);});}),new T(function(){var _jY=new T(function(){return A(_jH,[_jD]);}),_jZ=new T(function(){return A(_jH,[_jC]);}),_k0=new T(function(){return A(_jH,[_jB]);}),_k1=new T(function(){return A(_jH,[_jA]);}),_k2=new T(function(){return A(_jH,[_jz]);}),_k3=new T(function(){return A(_jH,[_jy]);}),_k4=new T(function(){return A(_jH,[_jx]);}),_k5=new T(function(){return A(_jH,[_jw]);}),_k6=new T(function(){return A(_jH,[_jv]);}),_k7=new T(function(){return A(_jH,[_ju]);}),_k8=new T(function(){return A(_jH,[_jt]);}),_k9=new T(function(){return A(_jH,[_js]);}),_ka=new T(function(){return A(_jH,[_jr]);}),_kb=new T(function(){return A(_jH,[_jq]);}),_kc=new T(function(){return A(_jH,[_jp]);}),_kd=new T(function(){return A(_jH,[_jo]);}),_ke=new T(function(){return A(_jH,[_jn]);}),_kf=new T(function(){return A(_jH,[_jm]);}),_kg=new T(function(){return A(_jH,[_jl]);}),_kh=new T(function(){return A(_jH,[_jk]);}),_ki=new T(function(){return A(_jH,[_jj]);}),_kj=new T(function(){return A(_jH,[_ji]);}),_kk=new T(function(){return A(_jH,[_jh]);}),_kl=new T(function(){return A(_jH,[_jg]);}),_km=new T(function(){return A(_jH,[_jf]);}),_kn=new T(function(){return A(_jH,[_je]);}),_ko=new T(function(){return A(_jH,[_jd]);}),_kp=new T(function(){return A(_jH,[_jc]);}),_kq=new T(function(){return A(_jH,[_jb]);}),_kr=new T(function(){return A(_jH,[_ja]);}),_ks=new T(function(){return A(_jH,[_j9]);}),_kt=new T(function(){return A(_jH,[_j8]);});return _aP([0,function(_ku){return E(E(_ku)[1])==94?E([0,function(_kv){switch(E(E(_kv)[1])){case 64:return E(_kt);case 65:return E(_ks);case 66:return E(_kr);case 67:return E(_kq);case 68:return E(_kp);case 69:return E(_ko);case 70:return E(_kn);case 71:return E(_km);case 72:return E(_kl);case 73:return E(_kk);case 74:return E(_kj);case 75:return E(_ki);case 76:return E(_kh);case 77:return E(_kg);case 78:return E(_kf);case 79:return E(_ke);case 80:return E(_kd);case 81:return E(_kc);case 82:return E(_kb);case 83:return E(_ka);case 84:return E(_k9);case 85:return E(_k8);case 86:return E(_k7);case 87:return E(_k6);case 88:return E(_k5);case 89:return E(_k4);case 90:return E(_k3);case 91:return E(_k2);case 92:return E(_k1);case 93:return E(_k0);case 94:return E(_jZ);case 95:return E(_jY);default:return [2];}}]):[2];}],new T(function(){return A(_iT,[function(_kw){return A(_jH,[[0,_kw,_ee]]);}]);}));}));}));},_kx=function(_ky){return A(_ky,[_0]);},_kz=function(_kA){var _kB=E(_kA);if(!_kB[0]){return E(_kx);}else{var _kC=_kB[2],_kD=E(E(_kB[1])[1]);switch(_kD){case 9:var _kE=new T(function(){return _kz(_kC);});return function(_kF){var _kG=new T(function(){return A(_kE,[_kF]);});return [0,function(_kH){return E(_kG);}];};case 10:var _kI=new T(function(){return _kz(_kC);});return function(_kJ){var _kK=new T(function(){return A(_kI,[_kJ]);});return [0,function(_kL){return E(_kK);}];};case 11:var _kM=new T(function(){return _kz(_kC);});return function(_kN){var _kO=new T(function(){return A(_kM,[_kN]);});return [0,function(_kP){return E(_kO);}];};case 12:var _kQ=new T(function(){return _kz(_kC);});return function(_kR){var _kS=new T(function(){return A(_kQ,[_kR]);});return [0,function(_kT){return E(_kS);}];};case 13:var _kU=new T(function(){return _kz(_kC);});return function(_kV){var _kW=new T(function(){return A(_kU,[_kV]);});return [0,function(_kX){return E(_kW);}];};case 32:var _kY=new T(function(){return _kz(_kC);});return function(_kZ){var _l0=new T(function(){return A(_kY,[_kZ]);});return [0,function(_l1){return E(_l0);}];};case 160:var _l2=new T(function(){return _kz(_kC);});return function(_l3){var _l4=new T(function(){return A(_l2,[_l3]);});return [0,function(_l5){return E(_l4);}];};default:var _l6=u_iswspace(_kD);if(!E(_l6)){return E(_kx);}else{var _l7=new T(function(){return _kz(_kC);});return function(_l8){var _l9=new T(function(){return A(_l7,[_l8]);});return [0,function(_la){return E(_l9);}];};}}}},_lb=function(_lc){var _ld=new T(function(){return _jG(_lc);}),_le=new T(function(){return _lb(_lc);}),_lf=[1,function(_lg){return A(_kz,[_lg,function(_lh){return E([0,function(_li){return E(E(_li)[1])==92?E(_le):[2];}]);}]);}];return _aP([0,function(_lj){return E(E(_lj)[1])==92?E([0,function(_lk){var _ll=E(E(_lk)[1]);switch(_ll){case 9:return E(_lf);case 10:return E(_lf);case 11:return E(_lf);case 12:return E(_lf);case 13:return E(_lf);case 32:return E(_lf);case 38:return E(_le);case 160:return E(_lf);default:var _lm=u_iswspace(_ll);return E(_lm)==0?[2]:E(_lf);}}]):[2];}],[0,function(_ln){var _lo=E(_ln);return E(_lo[1])==92?E(_ld):A(_lc,[[0,_lo,_ed]]);}]);},_lp=function(_lq,_lr){var _ls=new T(function(){return A(_lr,[[1,new T(function(){return A(_lq,[_9]);})]]);});return _lb(function(_lt){var _lu=E(_lt),_lv=E(_lu[1]);return E(_lv[1])==34?!E(_lu[2])?E(_ls):_lp(function(_lw){return A(_lq,[[1,_lv,_lw]]);},_lr):_lp(function(_lx){return A(_lq,[[1,_lv,_lx]]);},_lr);});},_ly=unCStr("_\'"),_lz=function(_lA){var _lB=u_iswalnum(_lA);return E(_lB)==0?_dR(_bm,[0,_lA],_ly):true;},_lC=function(_lD){return _lz(E(_lD)[1]);},_lE=unCStr(",;()[]{}`"),_lF=function(_lG){return A(_lG,[_9]);},_lH=function(_lI,_lJ){var _lK=function(_lL){var _lM=E(_lL);if(!_lM[0]){return E(_lF);}else{var _lN=_lM[1];if(!A(_lI,[_lN])){return E(_lF);}else{var _lO=new T(function(){return _lK(_lM[2]);});return function(_lP){var _lQ=new T(function(){return A(_lO,[function(_lR){return A(_lP,[[1,_lN,_lR]]);}]);});return [0,function(_lS){return E(_lQ);}];};}}};return [1,function(_lT){return A(_lK,[_lT,_lJ]);}];},_lU=unCStr(".."),_lV=unCStr("::"),_lW=unCStr("->"),_lX=[0,64],_lY=[1,_lX,_9],_lZ=[0,126],_m0=[1,_lZ,_9],_m1=unCStr("=>"),_m2=[1,_m1,_9],_m3=[1,_m0,_m2],_m4=[1,_lY,_m3],_m5=[1,_lW,_m4],_m6=unCStr("<-"),_m7=[1,_m6,_m5],_m8=[0,124],_m9=[1,_m8,_9],_ma=[1,_m9,_m7],_mb=[1,_iZ,_9],_mc=[1,_mb,_ma],_md=[0,61],_me=[1,_md,_9],_mf=[1,_me,_mc],_mg=[1,_lV,_mf],_mh=[1,_lU,_mg],_mi=function(_mj){var _mk=new T(function(){return A(_mj,[_ck]);});return _aP([1,function(_ml){return E(_ml)[0]==0?E(_mk):[2];}],new T(function(){var _mm=new T(function(){return _jG(function(_mn){var _mo=E(_mn);return (function(_mp,_mq){var _mr=new T(function(){return A(_mj,[[0,_mp]]);});return !E(_mq)?E(E(_mp)[1])==39?[2]:[0,function(_ms){return E(E(_ms)[1])==39?E(_mr):[2];}]:[0,function(_mt){return E(E(_mt)[1])==39?E(_mr):[2];}];})(_mo[1],_mo[2]);});});return _aP([0,function(_mu){return E(E(_mu)[1])==39?E([0,function(_mv){var _mw=E(_mv);switch(E(_mw[1])){case 39:return [2];case 92:return E(_mm);default:var _mx=new T(function(){return A(_mj,[[0,_mw]]);});return [0,function(_my){return E(E(_my)[1])==39?E(_mx):[2];}];}}]):[2];}],new T(function(){var _mz=new T(function(){return _lp(_n,_mj);});return _aP([0,function(_mA){return E(E(_mA)[1])==34?E(_mz):[2];}],new T(function(){return _aP([0,function(_mB){return !_dR(_bm,_mB,_lE)?[2]:A(_mj,[[2,[1,_mB,_9]]]);}],new T(function(){return _aP([0,function(_mC){return !_dR(_bm,_mC,_dW)?[2]:_lH(_dX,function(_mD){var _mE=[1,_mC,_mD];return !_dR(_bD,_mE,_mh)?A(_mj,[[4,_mE]]):A(_mj,[[2,_mE]]);});}],new T(function(){return _aP([0,function(_mF){var _mG=E(_mF),_mH=_mG[1],_mI=u_iswalpha(_mH);return E(_mI)==0?E(_mH)==95?_lH(_lC,function(_mJ){return A(_mj,[[3,[1,_mG,_mJ]]]);}):[2]:_lH(_lC,function(_mK){return A(_mj,[[3,[1,_mG,_mK]]]);});}],new T(function(){return _bY(_e1,_dM,_mj);}));}));}));}));}));}));},_mL=function(_mM){var _mN=new T(function(){return _mi(_mM);});return [1,function(_mO){return A(_kz,[_mO,function(_mP){return E(_mN);}]);}];},_mQ=[0,0],_mR=function(_mS,_mT){var _mU=new T(function(){return A(_mS,[_mQ,function(_mV){var _mW=new T(function(){return A(_mT,[_mV]);});return _mL(function(_mX){var _mY=E(_mX);if(_mY[0]==2){var _mZ=E(_mY[1]);return _mZ[0]==0?[2]:E(E(_mZ[1])[1])==41?E(_mZ[2])[0]==0?E(_mW):[2]:[2];}else{return [2];}});}]);});return _mL(function(_n0){var _n1=E(_n0);if(_n1[0]==2){var _n2=E(_n1[1]);return _n2[0]==0?[2]:E(E(_n2[1])[1])==40?E(_n2[2])[0]==0?E(_mU):[2]:[2];}else{return [2];}});},_n3=function(_n4,_n5,_n6){var _n7=function(_n8,_n9){var _na=new T(function(){return _mi(function(_nb){return A(_n4,[_nb,_n8,function(_nc){return A(_n9,[new T(function(){return [0, -E(_nc)[1]];})]);}]);});});return _aP(_mL(function(_nd){var _ne=E(_nd);if(_ne[0]==4){var _nf=E(_ne[1]);return _nf[0]==0?A(_n4,[_ne,_n8,_n9]):E(E(_nf[1])[1])==45?E(_nf[2])[0]==0?E([1,function(_ng){return A(_kz,[_ng,function(_nh){return E(_na);}]);}]):A(_n4,[_ne,_n8,_n9]):A(_n4,[_ne,_n8,_n9]);}else{return A(_n4,[_ne,_n8,_n9]);}}),new T(function(){return _mR(_n7,_n9);}));};return _n7(_n5,_n6);},_ni=function(_nj,_nk){return [2];},_nl=function(_nm,_nn){return _ni(_nm,_nn);},_no=function(_np){var _nq=E(_np);return _nq[0]==0?[1,new T(function(){return _dn(new T(function(){return _jE(E(_nq[1])[1]);}),_de,_nq[2]);})]:E(_nq[2])[0]==0?E(_nq[3])[0]==0?[1,new T(function(){return _dn(_dd,_de,_nq[1]);})]:[0]:[0];},_nr=function(_ns){var _nt=E(_ns);if(_nt[0]==5){var _nu=_no(_nt[1]);if(!_nu[0]){return E(_ni);}else{var _nv=new T(function(){return [0,_eq(_nu[1])];});return function(_nw,_nx){return A(_nx,[_nv]);};}}else{return E(_nl);}},_ny=function(_nm,_nn){return _n3(_nr,_nm,_nn);},_nz=function(_nA,_nB){var _nC=function(_nD,_nE){var _nF=new T(function(){return A(_nE,[_9]);}),_nG=new T(function(){return A(_nA,[_mQ,function(_nH){return _nC(_ee,function(_nI){return A(_nE,[[1,_nH,_nI]]);});}]);});return _mL(function(_nJ){var _nK=E(_nJ);if(_nK[0]==2){var _nL=E(_nK[1]);if(!_nL[0]){return [2];}else{var _nM=_nL[2];switch(E(E(_nL[1])[1])){case 44:return E(_nM)[0]==0?!E(_nD)?[2]:E(_nG):[2];case 93:return E(_nM)[0]==0?E(_nF):[2];default:return [2];}}}else{return [2];}});},_nN=function(_nO){var _nP=new T(function(){return _aP(_nC(_ed,_nO),new T(function(){return A(_nA,[_mQ,function(_nQ){return _nC(_ee,function(_nR){return A(_nO,[[1,_nQ,_nR]]);});}]);}));});return _aP(_mL(function(_nS){var _nT=E(_nS);if(_nT[0]==2){var _nU=E(_nT[1]);return _nU[0]==0?[2]:E(E(_nU[1])[1])==91?E(_nU[2])[0]==0?E(_nP):[2]:[2];}else{return [2];}}),new T(function(){return _mR(function(_nV,_nW){return _nN(_nW);},_nO);}));};return _nN(_nB);},_nX=function(_nY,_nZ){return _nz(_ny,_nZ);},_o0=new T(function(){return _nz(_ny,_bQ);}),_o1=function(_nn){return _aF(_o0,_nn);},_o2=function(_o3){var _o4=new T(function(){return _n3(_nr,_o3,_bQ);});return function(_cj){return _aF(_o4,_cj);};},_o5=[0,_o2,_o1,_ny,_nX],_o6=function(_o7,_o8){return _3Q(0,E(_o7)[1],_o8);},_o9=function(_oa,_ob){return _2H(_o6,_oa,_ob);},_oc=function(_od,_oe,_of){return _3Q(E(_od)[1],E(_oe)[1],_of);},_og=[0,_oc,_5i,_o9],_oh=unCStr("GHC.Types"),_oi=unCStr("Int"),_oj=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_5N,_oh,_oi],_ok=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_oj,_9],_ol=function(_om){return E(_ok);},_on=function(_oo){return E(E(_oo)[1]);},_op=function(_oq){return E(E(_oq)[2]);},_or=function(_os,_ot){var _ou=new T(function(){return A(_op,[_os,_ot]);}),_ov=new T(function(){return _on(_os);}),_ow=new T(function(){return _3B(_ov);}),_ox=new T(function(){return _3b(_ov);});return function(_oy){return A(_ox,[_ou,function(_oz){return A(_ow,[[0,_oz,_oy]]);}]);};},_oA=function(_oB,_oC){return [0,_oB,function(_oD){return _or(_oC,_oD);}];},_oE=function(_oF,_oG){return A(_3B,[_oF,[0,_oG,_oG]]);},_oH=function(_oI,_oJ,_oK){return A(_3B,[_oI,[0,_0,_oJ]]);},_oL=function(_oM,_oN){return [0,_oM,function(_oO){return _oE(_oN,_oO);},function(_oP,_oQ){return _oH(_oN,_oP,_oQ);}];},_oR=function(_oS,_oT){return A(_oS,[function(_){return jsFind(toJSStr(E(_oT)));}]);},_oU=function(_oV){return E(E(_oV)[4]);},_oW=unCStr("[]"),_oX=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_5N,_oh,_oW],_oY=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_oX,_9],_oZ=function(_p0){return E(_oY);},_p1=unCStr("Char"),_p2=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_5N,_oh,_p1],_p3=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_p2,_9],_p4=function(_p5){return E(_p3);},_p6=new T(function(){return _6H(_oZ,_p4);}),_p7=new T(function(){return A(_p6,[_6G]);}),_p8=new T(function(){return E(_6G);}),_p9=function(_pa){return E(E(_pa)[7]);},_pb=function(_pc){return E(E(_pc)[1]);},_pd=[0,0],_pe=[0,32],_pf=[0,10],_pg=function(_ph){var _pi=E(_ph);if(!_pi[0]){return E(_n);}else{var _pj=_pi[1],_pk=E(_pi[2]);if(!_pk[0]){return _pl(_pf,_pj);}else{var _pm=new T(function(){return _pg(_pk);}),_pn=new T(function(){return _pl(_pf,_pj);});return function(_po){return A(_pn,[[1,_pe,new T(function(){return A(_pm,[_po]);})]]);};}}},_pp=unCStr("->"),_pq=[1,_pp,_9],_pr=[1,_oh,_pq],_ps=[1,_5N,_pr],_pt=[0,32],_pu=function(_pv){var _pw=E(_pv);if(!_pw[0]){return [0];}else{var _px=_pw[1],_py=E(_pw[2]);return _py[0]==0?E(_px):_1R(_px,[1,_pt,new T(function(){return _pu(_py);})]);}},_pz=new T(function(){return _pu(_ps);}),_pA=new T(function(){var _pB=_6b(_pz);return [0,_pB[1],_pB[2],_5N,_oh,_pp];}),_pC=function(_pD,_pE){var _pF=E(_pD);return _pF[0]==0?E(_pE):A(_pF[1],[new T(function(){return _pC(_pF[2],_pE);})]);},_pG=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_pH=[1,_5P,_9],_pI=function(_pJ){var _pK=E(_pJ);if(!_pK[0]){return [0];}else{var _pL=E(_pK[1]);return [1,[0,_pL[1],_pL[2]],new T(function(){return _pI(_pK[2]);})];}},_pM=new T(function(){var _pN=_1R(_9,_pH);if(!_pN[0]){return E(_oX);}else{var _pO=_6b(new T(function(){return _5Z(_6n(_6y,[1,_pG,new T(function(){return _pI(_pN);})]));}));return E(_oX);}}),_pP=[0,40],_pQ=function(_pR){return _pl(_pf,_pR);},_pS=[0,8],_pT=unCStr(" -> "),_pU=[0,9],_pV=[0,93],_pW=[0,91],_pX=[0,41],_pY=[0,44],_pZ=function(_pR){return [1,_pY,_pR];},_q0=function(_q1,_q2){var _q3=E(_q2);return _q3[0]==0?[0]:[1,_q1,[1,_q3[1],new T(function(){return _q0(_q1,_q3[2]);})]];},_pl=function(_q4,_q5){var _q6=E(_q5),_q7=_q6[3],_q8=E(_q6[4]);if(!_q8[0]){return function(_q9){return _1R(E(_q7)[5],_q9);};}else{var _qa=_q8[1],_qb=new T(function(){var _qc=E(_q7)[5],_qd=new T(function(){return _pg(_q8);}),_qe=new T(function(){return E(_q4)[1]<=9?function(_qf){return _1R(_qc,[1,_pe,new T(function(){return A(_qd,[_qf]);})]);}:function(_qg){return [1,_3P,new T(function(){return _1R(_qc,[1,_pe,new T(function(){return A(_qd,[[1,_3O,_qg]]);})]);})];};}),_qh=E(_qc);if(!_qh[0]){return E(_qe);}else{if(E(E(_qh[1])[1])==40){var _qi=E(_qh[2]);return _qi[0]==0?E(_qe):E(E(_qi[1])[1])==44?function(_qj){return [1,_pP,new T(function(){return A(new T(function(){var _qk=_6n(_pQ,_q8);if(!_qk[0]){return E(_n);}else{var _ql=new T(function(){return _q0(_pZ,_qk[2]);});return function(_cj){return _pC([1,_qk[1],_ql],_cj);};}}),[[1,_pX,_qj]]);})];}:E(_qe);}else{return E(_qe);}}}),_qm=E(_q8[2]);if(!_qm[0]){var _qn=E(_q7),_qo=E(_pM),_qp=hs_eqWord64(_qn[1],_qo[1]);if(!E(_qp)){return E(_qb);}else{var _qq=hs_eqWord64(_qn[2],_qo[2]);if(!E(_qq)){return E(_qb);}else{var _qr=new T(function(){return _pl(_pd,_qa);});return function(_qs){return [1,_pW,new T(function(){return A(_qr,[[1,_pV,_qs]]);})];};}}}else{if(!E(_qm[2])[0]){var _qt=E(_q7),_qu=E(_pA),_qv=hs_eqWord64(_qt[1],_qu[1]);if(!E(_qv)){return E(_qb);}else{var _qw=hs_eqWord64(_qt[2],_qu[2]);if(!E(_qw)){return E(_qb);}else{var _qx=new T(function(){return _pl(_pS,_qm[1]);}),_qy=new T(function(){return _pl(_pU,_qa);});return E(_q4)[1]<=8?function(_qz){return A(_qy,[new T(function(){return _1R(_pT,new T(function(){return A(_qx,[_qz]);}));})]);}:function(_qA){return [1,_3P,new T(function(){return A(_qy,[new T(function(){return _1R(_pT,new T(function(){return A(_qx,[[1,_3O,_qA]]);}));})]);})];};}}}else{return E(_qb);}}}},_qB=function(_qC,_qD,_qE,_qF){var _qG=new T(function(){return _3B(_qC);}),_qH=new T(function(){return _oU(_qF);}),_qI=new T(function(){return _p9(_qF);}),_qJ=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_pl,[_pd,A(_qD,[_p8]),_9]);}));}),_qK=new T(function(){return A(_pb,[_qE,_f]);});return function(_qL){if(!E(new T(function(){var _qM=A(_qD,[_p8]),_qN=E(_p7),_qO=hs_eqWord64(_qM[1],_qN[1]);if(!E(_qO)){return false;}else{var _qP=hs_eqWord64(_qM[2],_qN[2]);return E(_qP)==0?false:true;}}))){var _qQ=new T(function(){return A(_qG,[[1,_qL,new T(function(){return A(_qI,[new T(function(){return A(_qH,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _1R(_qL,_qJ);}));})]);})]);})]]);}),_qR=A(_qK,[_qL]);if(!_qR[0]){return E(_qQ);}else{var _qS=E(_qR[1]);return E(_qS[2])[0]==0?E(_qR[2])[0]==0?A(_qG,[[2,_qS[1]]]):E(_qQ):E(_qQ);}}else{return A(_qG,[[2,_qL]]);}};},_qT=[0],_qU=new T(function(){return [0,"value"];}),_qV=function(_qW,_qX,_qY,_qZ,_r0,_r1){var _r2=E(_qW),_r3=_r2[1],_r4=new T(function(){return A(_r2[3],[_qT]);}),_r5=new T(function(){return _qB(_r2,_qY,_qZ,_r0);});return A(_r3,[new T(function(){return _oR(_qX,_r1);}),function(_r6){var _r7=E(_r6);return _r7[0]==0?E(_r4):A(_r3,[new T(function(){return A(_qX,[function(_){var _r8=jsGet(E(_r7[1])[1],E(_qU)[1]);return [1,new T(function(){return fromJSStr(_r8);})];}]);}),function(_r9){var _ra=E(_r9);return _ra[0]==0?E(_r4):A(_r5,[_ra[1]]);}]);}]);},_rb=1,_rc=function(_rd){return E(E(_rd)[10]);},_re=function(_rf){return E(E(_rf)[2]);},_rg=function(_rh){return E(E(_rh)[3]);},_ri=function(_rj){return E(E(_rj)[2]);},_rk=function(_rl,_rm,_rn,_ro,_rp,_rq,_rr,_rs,_rt,_ru,_rv,_rw){var _rx=_on(_rq),_ry=_rx[1],_rz=_rx[3],_rA=new T(function(){return _93(_rs);}),_rB=new T(function(){return _95(_rA);}),_rC=new T(function(){return _rg(_rr);}),_rD=new T(function(){return _ri(_rm);}),_rE=new T(function(){return _rc(_rs);});return A(_ry,[new T(function(){var _rF=E(_ru);if(!_rF[0]){var _rG=E(_rr);return _3V(_rt,_rG[1],_rG[2],_rG[3]);}else{return A(_rz,[_rF[1]]);}}),function(_rH){return A(_ry,[new T(function(){var _rI=E(_rt);return _re(_rr);}),function(_rJ){return A(_rx[2],[new T(function(){return A(_rC,[new T(function(){var _rK=E(new T(function(){var _rL=E(_rt);return [0,coercionToken];})),_rM=E(_rJ);return [0,_rM[1],_rM[2],_rb,_rM[4],_rM[5]];})]);}),new T(function(){var _rN=new T(function(){return A(_rz,[[0,new T(function(){return A(_rE,[_rH,_rv,new T(function(){var _rO=E(_rw);if(!_rO[0]){return [0];}else{var _rP=_rO[1],_rQ=_1B(_rp,_p6,_rP);return _rQ[0]==0?A(_ri,[_rn,_rP]):E(_rQ[1]);}}),_ed,_a]);}),_a]]);});return A(_ry,[new T(function(){var _rR=E(_rq);return _qV(_rR[1],_rR[2],_ro,_rl,_rs,_rH);}),function(_rS){var _rT=E(_rS);switch(_rT[0]){case 0:return E(_rN);case 1:return A(_rz,[[0,new T(function(){return A(_rB,[new T(function(){return A(_rE,[_rH,_rv,_rT[1],_ed,_a]);}),_rT[2]]);}),_a]]);default:var _rU=_rT[1];return A(_rz,[[0,new T(function(){return A(_rE,[_rH,_rv,new T(function(){var _rV=_1B(_ro,_p6,_rU);return _rV[0]==0?A(_rD,[_rU]):E(_rV[1]);}),_ed,_a]);}),[1,_rU]]]);}}]);})]);}]);}]);},_rW=function(_rX,_rY,_rZ,_s0,_s1){var _s2=new T(function(){return _on(_rY);}),_s3=new T(function(){return _3D(_s2);}),_s4=new T(function(){return _oL(_s3,_s2);}),_s5=new T(function(){return _oA(_s3,_rY);});return function(_cj,_s6,_s7){return _rk(_s1,_s0,_s0,_rZ,_rZ,_s5,_s4,_rX,[0,coercionToken],_cj,_s6,_s7);};},_s8=new T(function(){return _rW(_8G,_9M,_ol,_og,_o5);}),_s9=new T(function(){return A(_s8,[_a,_9L,_a]);}),_sa=unCStr("true"),_sb=unCStr("hasevent"),_sc=function(_sd,_se){while(1){var _sf=E(_sd);if(!_sf[0]){return E(_se)[0]==0?true:false;}else{var _sg=E(_se);if(!_sg[0]){return false;}else{if(E(_sf[1])[1]!=E(_sg[1])[1]){return false;}else{_sd=_sf[2];_se=_sg[2];continue;}}}}},_sh=new T(function(){return [0,"keydown"];}),_si=new T(function(){return [0,"mousemove"];}),_sj=new T(function(){return [0,"blur"];}),_sk=new T(function(){return [0,"focus"];}),_sl=new T(function(){return [0,"change"];}),_sm=new T(function(){return [0,"unload"];}),_sn=new T(function(){return [0,"load"];}),_so=new T(function(){return [0,"keyup"];}),_sp=new T(function(){return [0,"keypress"];}),_sq=new T(function(){return [0,"mouseup"];}),_sr=new T(function(){return [0,"mousedown"];}),_ss=new T(function(){return [0,"dblclick"];}),_st=new T(function(){return [0,"click"];}),_su=new T(function(){return [0,"mouseout"];}),_sv=new T(function(){return [0,"mouseover"];}),_sw=function(_sx){switch(E(_sx)[0]){case 0:return E(_sn);case 1:return E(_sm);case 2:return E(_sl);case 3:return E(_sk);case 4:return E(_sj);case 5:return E(_si);case 6:return E(_sv);case 7:return E(_su);case 8:return E(_st);case 9:return E(_ss);case 10:return E(_sr);case 11:return E(_sq);case 12:return E(_sp);case 13:return E(_so);default:return E(_sh);}},_sy=function(_sz,_sA,_sB,_sC,_){var _sD=A(_sz,[_sC,_]),_sE=E(_sD),_sF=_sE[1],_sG=E(_sb),_sH=jsGetAttr(_sF,toJSStr(_sG));if(!_sc(fromJSStr(_sH),_sa)){var _sI=E(_sB),_sJ=jsSetCB(_sF,_sw(_sA)[1],_sB),_sK=A(_1,[_n,_sE,_sG,_sa,_]);return _sE;}else{return _sE;}},_sL=function(_){return _a;},_sM=function(_){var _=0,_sN=newMVar(),_=putMVar(_sN,_sL);return [0,_sN];},_sO=new T(function(){return _j(_sM);}),_sP=function(_){var _sQ=E(_sO)[1],_sR=takeMVar(_sQ),_=putMVar(_sQ,_sR);return _sR;},_sS=function(_){var _sT=0;if(!E(_sT)){var _sU=_sP();return A(_sU,[_]);}else{var _sV=E(_sO)[1],_sW=takeMVar(_sV),_=putMVar(_sV,_sW);return A(_sW,[_]);}},_sX=function(_sY,_sZ,_t0,_){var _t1=E(_t0),_t2=E(_t1[4]),_t3=A(_sY,[_t1,_]),_t4=E(_t3),_t5=E(_t4[1]);return [0,[0,function(_11,_){return _sy(_t5[1],_sZ,function(_){var _t6=A(_t2[1],[_]),_t7=E(_t6);if(!_t7[0]){var _t8=_sS(_);return _0;}else{var _t9=A(_t2[2],[_t7[1],_]),_ta=_sS(_);return _0;}},_11,_);},_t5[2]],_t4[2]];},_tb=function(_tc,_){var _td=_sX(_s9,_9K,_tc,_),_te=E(_td),_tf=E(_te[1]);return [0,[0,function(_tg,_){var _th=A(_tf[1],[_tg,_]),_ti=_5l(_tg,_);return _tg;},_tf[2]],_te[2]];},_tj=new T(function(){return [1,_tb,_tj];}),_tk=function(_tl,_tm){var _tn=E(_tl);if(!_tn){return [0];}else{var _to=E(_tm);return _to[0]==0?[0]:[1,_to[1],new T(function(){return _tk(_tn-1|0,_to[2]);})];}},_tp=function(_tq,_tr){return _tq<0?[0]:_tk(_tq,_tr);},_ts=function(_tt,_tu){var _tv=E(_tt)[1];return _tv>0?_tp(_tv,_tu):[0];},_tw=function(_tx){return E(_tx);},_ty=function(_tz){var _tA=new T(function(){return _9D(_5D,_ts(_tz,_tj));}),_tB=new T(function(){return _59(_p,new T(function(){return unAppCStr("This widget sum ",new T(function(){return _1R(_3Q(0,E(_tz)[1],_9),_5z);}));}));});return function(_tC,_){var _tD=_4F(_tA,_5r,_tC,_),_tE=E(_tD),_tF=E(_tE[1]),_tG=new T(function(){return _59(_tw,_tF[1]);});return [0,[0,function(_tH,_){var _tI=A(_tB,[_tH,_]),_tJ=A(_tG,[_tH,_]);return _tH;},_tF[2]],_tE[2]];};},_tK=new T(function(){return _ty(_L);}),_tL=unCStr(" A counter. wcallback erases the previous rendering of the widget an regenerates it again "),_tM=new T(function(){return _59(_p,_tL);}),_tN=[8,coercionToken],_tO=function(_tP){return _aP(_mL(function(_tQ){var _tR=E(_tQ);return _tR[0]==0?A(_tP,[_tR[1]]):[2];}),new T(function(){return _mR(_tS,_tP);}));},_tS=function(_tT,_tU){return _tO(_tU);},_tV=function(_tW){return _aP(_aP(_mL(function(_tX){var _tY=E(_tX);return _tY[0]==1?A(_tW,[_tY[1]]):[2];}),new T(function(){return _nz(_tS,_tW);})),new T(function(){return _mR(_tZ,_tW);}));},_tZ=function(_u0,_u1){return _tV(_u1);},_u2=new T(function(){return _mR(_tZ,_bQ);}),_u3=new T(function(){return _nz(_tS,_bQ);}),_u4=function(_u5){var _u6=E(_u5);return _u6[0]==1?[3,_u6[1],_bP]:[2];},_u7=new T(function(){return _mi(_u4);}),_u8=function(_u9){return E(_u7);},_ua=function(_ub){return A(_kz,[_ub,_u8]);},_uc=[1,_ua],_ud=new T(function(){return _aP(_uc,_u3);}),_ue=new T(function(){return _aP(_ud,_u2);}),_uf=function(_nn){return _aF(_ue,_nn);},_ug=new T(function(){return _tO(_bQ);}),_uh=function(_nn){return _aF(_ug,_nn);},_ui=function(_uj){return E(_uh);},_uk=[0,_ui,_uf,_tS,_tZ],_ul=function(_um){return E(E(_um)[4]);},_un=function(_uo,_up,_uq){return _nz(new T(function(){return _ul(_uo);}),_uq);},_ur=function(_us){var _ut=new T(function(){return _nz(new T(function(){return _ul(_us);}),_bQ);});return function(_cj){return _aF(_ut,_cj);};},_uu=function(_uv,_uw){var _ux=new T(function(){return A(_ul,[_uv,_uw,_bQ]);});return function(_cj){return _aF(_ux,_cj);};},_uy=function(_uz){return [0,function(_nn){return _uu(_uz,_nn);},new T(function(){return _ur(_uz);}),new T(function(){return _ul(_uz);}),function(_nm,_nn){return _un(_uz,_nm,_nn);}];},_uA=new T(function(){return _uy(_uk);}),_uB=unCStr("Prelude.(!!): negative index\n"),_uC=new T(function(){return err(_uB);}),_uD=unCStr("Prelude.(!!): index too large\n"),_uE=new T(function(){return err(_uD);}),_uF=function(_uG,_uH){while(1){var _uI=E(_uG);if(!_uI[0]){return E(_uE);}else{var _uJ=E(_uH);if(!_uJ){return E(_uI[1]);}else{_uG=_uI[2];_uH=_uJ-1|0;continue;}}}},_uK=unCStr("ACK"),_uL=unCStr("BEL"),_uM=unCStr("BS"),_uN=unCStr("SP"),_uO=[1,_uN,_9],_uP=unCStr("US"),_uQ=[1,_uP,_uO],_uR=unCStr("RS"),_uS=[1,_uR,_uQ],_uT=unCStr("GS"),_uU=[1,_uT,_uS],_uV=unCStr("FS"),_uW=[1,_uV,_uU],_uX=unCStr("ESC"),_uY=[1,_uX,_uW],_uZ=unCStr("SUB"),_v0=[1,_uZ,_uY],_v1=unCStr("EM"),_v2=[1,_v1,_v0],_v3=unCStr("CAN"),_v4=[1,_v3,_v2],_v5=unCStr("ETB"),_v6=[1,_v5,_v4],_v7=unCStr("SYN"),_v8=[1,_v7,_v6],_v9=unCStr("NAK"),_va=[1,_v9,_v8],_vb=unCStr("DC4"),_vc=[1,_vb,_va],_vd=unCStr("DC3"),_ve=[1,_vd,_vc],_vf=unCStr("DC2"),_vg=[1,_vf,_ve],_vh=unCStr("DC1"),_vi=[1,_vh,_vg],_vj=unCStr("DLE"),_vk=[1,_vj,_vi],_vl=unCStr("SI"),_vm=[1,_vl,_vk],_vn=unCStr("SO"),_vo=[1,_vn,_vm],_vp=unCStr("CR"),_vq=[1,_vp,_vo],_vr=unCStr("FF"),_vs=[1,_vr,_vq],_vt=unCStr("VT"),_vu=[1,_vt,_vs],_vv=unCStr("LF"),_vw=[1,_vv,_vu],_vx=unCStr("HT"),_vy=[1,_vx,_vw],_vz=[1,_uM,_vy],_vA=[1,_uL,_vz],_vB=[1,_uK,_vA],_vC=unCStr("ENQ"),_vD=[1,_vC,_vB],_vE=unCStr("EOT"),_vF=[1,_vE,_vD],_vG=unCStr("ETX"),_vH=[1,_vG,_vF],_vI=unCStr("STX"),_vJ=[1,_vI,_vH],_vK=unCStr("SOH"),_vL=[1,_vK,_vJ],_vM=unCStr("NUL"),_vN=[1,_vM,_vL],_vO=[0,92],_vP=unCStr("\\DEL"),_vQ=unCStr("\\a"),_vR=unCStr("\\\\"),_vS=unCStr("\\SO"),_vT=unCStr("\\r"),_vU=unCStr("\\f"),_vV=unCStr("\\v"),_vW=unCStr("\\n"),_vX=unCStr("\\t"),_vY=unCStr("\\b"),_vZ=function(_w0,_w1){if(_w0<=127){var _w2=E(_w0);switch(_w2){case 92:return _1R(_vR,_w1);case 127:return _1R(_vP,_w1);default:if(_w2<32){var _w3=E(_w2);switch(_w3){case 7:return _1R(_vQ,_w1);case 8:return _1R(_vY,_w1);case 9:return _1R(_vX,_w1);case 10:return _1R(_vW,_w1);case 11:return _1R(_vV,_w1);case 12:return _1R(_vU,_w1);case 13:return _1R(_vT,_w1);case 14:return _1R(_vS,new T(function(){var _w4=E(_w1);return _w4[0]==0?[0]:E(E(_w4[1])[1])==72?unAppCStr("\\&",_w4):E(_w4);}));default:return _1R([1,_vO,new T(function(){var _w5=_w3;return _w5>=0?_uF(_vN,_w5):E(_uC);})],_w1);}}else{return [1,[0,_w2],_w1];}}}else{return [1,_vO,new T(function(){var _w6=jsShowI(_w0);return _1R(fromJSStr(_w6),new T(function(){var _w7=E(_w1);if(!_w7[0]){return [0];}else{var _w8=E(_w7[1])[1];return _w8<48?E(_w7):_w8>57?E(_w7):unAppCStr("\\&",_w7);}}));})];}},_w9=[0,39],_wa=[1,_w9,_9],_wb=unCStr("\'\\\'\'"),_wc=function(_wd){var _we=E(E(_wd)[1]);return _we==39?E(_wb):[1,_w9,new T(function(){return _vZ(_we,_wa);})];},_wf=[0,34],_wg=unCStr("\\\""),_wh=function(_wi,_wj){var _wk=E(_wi);if(!_wk[0]){return E(_wj);}else{var _wl=_wk[2],_wm=E(E(_wk[1])[1]);return _wm==34?_1R(_wg,new T(function(){return _wh(_wl,_wj);})):_vZ(_wm,new T(function(){return _wh(_wl,_wj);}));}},_wn=function(_wo,_wp){return [1,_wf,new T(function(){return _wh(_wo,[1,_wf,_wp]);})];},_wq=function(_wr){return _1R(_wb,_wr);},_ws=function(_wt,_wu){var _wv=E(E(_wu)[1]);return _wv==39?E(_wq):function(_ww){return [1,_w9,new T(function(){return _vZ(_wv,[1,_w9,_ww]);})];};},_wx=[0,_ws,_wc,_wn],_wy=function(_wz){return E(E(_wz)[3]);},_wA=function(_wB,_wC){return A(_wy,[_wB,_wC,_9]);},_wD=function(_wE,_wF,_wG){return _2H(new T(function(){return _wy(_wE);}),_wF,_wG);},_wH=function(_wI){var _wJ=new T(function(){return _wy(_wI);});return [0,function(_wK){return E(_wJ);},function(_wr){return _wA(_wI,_wr);},function(_wL,_wr){return _wD(_wI,_wL,_wr);}];},_wM=new T(function(){return _wH(_wx);}),_wN=new T(function(){return _rW(_8G,_9M,_p6,_wM,_uA);}),_wO=unCStr("submit"),_wP=new T(function(){return A(_wN,[_a,_wO]);}),_wQ=[0,43],_wR=[1,_wQ,_9],_wS=[1,_wR],_wT=new T(function(){return A(_wP,[_wS]);}),_wU=function(_11,_){return _13(_12,_11,_);},_wV=function(_wW,_wX,_wY,_){var _wZ=A(_wX,[_wY,_]),_x0=E(_wZ),_x1=E(_x0[1]);return [0,[0,function(_x2,_){var _x3=_13(_12,_x2,_),_x4=A(_1,[_n,_x3,_M,_wW,_]),_x5=A(_x1[1],[_x3,_]);return _x3;},_x1[2]],_x0[2]];},_x6=new T(function(){return _3V(_1e,_3I,_1c,_19);}),_x7=new T(function(){return _3V(_1e,_3I,_1c,_19);}),_x8=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_x9=function(_xa){return _j(function(_){var _=0;return eval(E(_xa)[1]);});},_xb=new T(function(){return _x9(_x8);}),_xc=function(_xd,_xe,_xf,_){var _xg=A(_x6,[_xf,_]),_xh=A(_x7,[new T(function(){return E(E(_xg)[2]);}),_]),_xi=E(_xh),_xj=_xi[1],_xk=E(_xi[2]),_xl=_xk[2],_xm=E(_xk[4]),_xn=new T(function(){return E(E(_xg)[1]);}),_xo=function(_xp){var _xq=new T(function(){return A(_xe,[_xp]);});return function(_xr,_){var _xs=A(_xq,[_xr,_]),_xt=E(_xs),_xu=E(_xt[1]);return [0,[0,function(_xv,_){var _xw=A(_xu[1],[_xv,_]),_xx=E(_xn),_xy=jsFind(toJSStr(_xx)),_xz=E(_xy);if(!_xz[0]){return _49(_xx);}else{var _xA=E(_xz[1]),_xB=A(_xb,[E(_xA[1]),_]),_xC=jsKillChild(E(_xA)[1],_xB);return _xv;}},_xu[2]],_xt[2]];};},_xD=_wV(_xn,_xd,[0,_xk[1],_xl,_xk[3],[0,function(_){return _4b(function(_xE,_){var _xF=_wV(_xn,_xd,new T(function(){var _xG=E(_xE);return [0,_xG[1],_xl,_xG[3],_xG[4],_xG[5]];}),_);return [0,[0,_38,E(E(_xF)[1])[2]],_xE];},_xj,_);},function(_xH,_){var _xI=_4b(new T(function(){return _xo(_xH);}),_xj,_),_xJ=E(_xI);return _xJ[0]==0?_a:A(_xm[2],[_xJ[1],_]);}],_xk[5]],_),_xK=E(_xD),_xL=_xK[2],_xM=E(_xK[1]),_xN=_xM[1],_xO=new T(function(){return _U(_wU,[1,[0,_M,_xj],_9]);}),_xP=E(_xM[2]);if(!_xP[0]){return [0,[0,function(_xQ,_){var _xR=A(_xN,[_xQ,_]),_xS=A(_xO,[_xQ,_]);return _xQ;},_a],new T(function(){var _xT=E(_xL);return [0,_xT[1],_xT[2],_xT[3],_xm,_xT[5]];})];}else{var _xU=A(_xo,[_xP[1],new T(function(){var _xV=E(_xL);return [0,_xV[1],_xV[2],_xV[3],_xm,_xV[5]];}),_]),_xW=E(_xU),_xX=E(_xW[1]);return [0,[0,function(_xY,_){var _xZ=A(_xN,[_xY,_]),_y0=A(_xO,[_xY,_]),_y1=A(_xX[1],[_y0,_]);return _xY;},_xX[2]],_xW[2]];}},_y2=function(_y3){var _y4=new T(function(){return _y2(new T(function(){return [0,E(_y3)[1]+1|0];}));}),_y5=new T(function(){return _w(_p,new T(function(){return _5i(_y3);}));});return function(_cj,_s6){return _xc(function(_y6,_){var _y7=_sX(_wT,_tN,_y6,_),_y8=E(_y7),_y9=E(_y8[1]);return [0,[0,function(_ya,_){var _yb=A(_y5,[_ya,_]),_yc=A(_y9[1],[_ya,_]);return _ya;},_y9[2]],_y8[2]];},function(_yd){return E(_y4);},_cj,_s6);};},_ye=function(_yf){var _yg=new T(function(){return _y2(_yf);});return function(_yh,_){var _yi=A(_yg,[_yh,_]),_yj=E(_yi),_yk=E(_yj[1]);return [0,[0,function(_yl,_){var _ym=A(_tM,[_yl,_]),_yn=_5l(_yl,_),_yo=A(_yk[1],[_yl,_]);return _yl;},_yk[2]],_yj[2]];};},_yp=new T(function(){return _ye(_L);}),_yq=[0,4],_yr=function(_ys,_yt){return [1,_yt,new T(function(){return _yr(_ys,new T(function(){return A(_ys,[_yt]);}));})];},_yu=[0,1],_yv=[1,_yu,_9],_yw=[1,_5A,_9],_yx=function(_yy,_yz,_yA){var _yB=E(_yz);if(!_yB[0]){return [0];}else{var _yC=E(_yA);return _yC[0]==0?[0]:[1,new T(function(){return A(_yy,[_yB[1],_yC[1]]);}),new T(function(){return _yx(_yy,_yB[2],_yC[2]);})];}},_yD=function(_yE){return _yx(_8V,[1,_5A,_yE],new T(function(){return _1R(_yE,_yw);}));},_yF=new T(function(){return _yr(_yD,_yv);}),_yG=unCStr(" rows of the Pascal triangle "),_yH=function(_yI){var _yJ=new T(function(){return _2H(_o6,_yI,_9);});return function(_cj,_s6){return _p(_yJ,_cj,_s6);};},_yK=function(_yL,_yM){var _yN=new T(function(){return _59(_yH,_yL);});return [1,function(_yO,_){var _yP=A(_yN,[_yO,_]),_yQ=A(_1,[_n,_yP,_J,_I,_]);return _yP;},_yM];},_yR=function(_yS,_yT){var _yU=E(_yS);if(!_yU[0]){return [0];}else{var _yV=_yU[1];return _yT>1?_yK(_yV,new T(function(){return _yR(_yU[2],_yT-1|0);})):_yK(_yV,_9);}},_yW=function(_yX){var _yY=new T(function(){return _59(_p,new T(function(){return unAppCStr("Show ",new T(function(){return _1R(_3Q(0,E(_yX)[1],_9),_yG);}));}));});return function(_yZ,_){return [0,[0,function(_z0,_){var _z1=A(_yY,[_z0,_]),_z2=_8t(new T(function(){var _z3=E(_yX)[1];return _z3>0?_yR(_yF,_z3):[0];}),_z0,_);return _z0;},_a],_yZ];};},_z4=new T(function(){return _yW(_yq);}),_z5=unCStr("center"),_z6=function(_z7,_z8){var _z9=new T(function(){return A(_z7,[_z8]);});return function(_za,_){var _zb=jsCreateElem(toJSStr(E(_z5))),_zc=jsAppendChild(_zb,E(_za)[1]),_zd=[0,_zb],_ze=A(_z9,[_zd,_]);return _zd;};},_zf=unCStr("This example draw a function of x between 10 and -10. You can define the function using javascript expressions"),_zg=new T(function(){return _59(_p,_zf);}),_zh=function(_zi){var _zj=jsShow(E(_zi)[1]);return fromJSStr(_zj);},_zk=function(_zl){var _zm=new T(function(){return _zh(_zl);});return function(_cj){return _1R(_zm,_cj);};},_zn=function(_zo,_zp,_zq){var _zr=E(_zq);if(!_zr[0]){return [0];}else{var _zs=_zr[2],_zt=E(_zr[1]);return _zo!=_zt[1]?[1,_zt,new T(function(){return _zn(_zo,_zp,_zs);})]:_1R(_zp,new T(function(){return _zn(_zo,_zp,_zs);}));}},_zu=[0,45],_zv=function(_zw,_zx,_zy){var _zz=new T(function(){return A(_zw,[[0, -_zy]]);}),_zA=new T(function(){return E(_zx)[1]<=6?function(_zB){return [1,_zu,new T(function(){return A(_zz,[_zB]);})];}:function(_zC){return [1,_3P,[1,_zu,new T(function(){return A(_zz,[[1,_3O,_zC]]);})]];};});if(_zy>=0){var _zD=isDoubleNegativeZero(_zy);return E(_zD)==0?A(_zw,[[0,_zy]]):E(_zA);}else{return E(_zA);}},_zE=unCStr("canvas"),_zF=unCStr("id"),_zG=unCStr("canvas"),_zH=function(_zI,_zJ){var _zK=new T(function(){return A(_zI,[_zJ]);});return function(_zL,_){var _zM=jsCreateElem(toJSStr(E(_zG))),_zN=jsAppendChild(_zM,E(_zL)[1]),_zO=[0,_zM],_zP=A(_zK,[_zO,_]);return _zO;};},_zQ=new T(function(){return _zH(_tw,_38);}),_zR=function(_zS,_){var _zT=A(_zQ,[_zS,_]),_zU=A(_1,[_n,_zT,_zF,_zE,_]);return _zT;},_zV=[1,_0],_zW=[0,_zR,_zV],_zX=function(_zY,_){return [0,_zW,_zY];},_zZ=unCStr("Pattern match failure in do expression at main.hs:170:5-12"),_A0=new T(function(){return [0,"(function(exp){ return eval(exp);})"];}),_A1=new T(function(){return _x9(_A0);}),_A2=function(_A3,_){var _A4=jsHasCtx2D(_A3);if(!E(_A4)){return _a;}else{var _A5=jsGetCtx2D(_A3);return [1,[0,[0,_A5],[0,_A3]]];}},_A6=function(_A7,_){return _A2(E(_A7)[1],_);},_A8=function(_A9,_Aa){return A(_A9,[function(_){var _Ab=jsFind(toJSStr(E(_Aa))),_Ac=E(_Ab);return _Ac[0]==0?_a:_A6(_Ac[1],_);}]);},_Ad=new T(function(){return _A8(_n,_zE);}),_Ae=[0,-10],_Af=[0,0],_Ag=[0,_Ae,_Af],_Ah=[0,10],_Ai=[0,_Ah,_Af],_Aj=[1,_Ai,_9],_Ak=[1,_Ag,_Aj],_Al=function(_Am,_){return _0;},_An=function(_Ao){var _Ap=E(_Ao);if(!_Ap[0]){return E(_Al);}else{var _Aq=E(_Ap[1]);return function(_Ar,_){var _As=E(_Ar)[1],_At=jsMoveTo(_As,E(_Aq[1])[1],E(_Aq[2])[1]);return (function(_Au,_){while(1){var _Av=E(_Au);if(!_Av[0]){return _0;}else{var _Aw=E(_Av[1]),_Ax=jsLineTo(_As,E(_Aw[1])[1],E(_Aw[2])[1]);_Au=_Av[2];continue;}}})(_Ap[2],_);};}},_Ay=new T(function(){return _An(_Ak);}),_Az=[0,30],_AA=[0,_Af,_Az],_AB=[0,-30],_AC=[0,_Af,_AB],_AD=[1,_AC,_9],_AE=[1,_AA,_AD],_AF=new T(function(){return _An(_AE);}),_AG=new T(function(){return [0,0/0];}),_AH=new T(function(){return [0,-1/0];}),_AI=new T(function(){return [0,1/0];}),_AJ=[0,0],_AK=function(_AL,_AM){while(1){var _AN=E(_AL);if(!_AN[0]){_AL=[1,I_fromInt(_AN[1])];continue;}else{var _AO=E(_AM);if(!_AO[0]){_AL=_AN;_AM=[1,I_fromInt(_AO[1])];continue;}else{return I_fromRat(_AN[1],_AO[1]);}}}},_AP=function(_AQ,_AR){var _AS=E(_AQ);if(!_AS[0]){var _AT=_AS[1],_AU=E(_AR);return _AU[0]==0?_AT==_AU[1]:I_compareInt(_AU[1],_AT)==0?true:false;}else{var _AV=_AS[1],_AW=E(_AR);return _AW[0]==0?I_compareInt(_AV,_AW[1])==0?true:false:I_compare(_AV,_AW[1])==0?true:false;}},_AX=function(_AY,_AZ){var _B0=E(_AY);if(!_B0[0]){var _B1=_B0[1],_B2=E(_AZ);return _B2[0]==0?_B1<_B2[1]:I_compareInt(_B2[1],_B1)>0;}else{var _B3=_B0[1],_B4=E(_AZ);return _B4[0]==0?I_compareInt(_B3,_B4[1])<0:I_compare(_B3,_B4[1])<0;}},_B5=function(_B6,_B7){return !_AP(_B7,_AJ)?[0,_AK(_B6,_B7)]:!_AP(_B6,_AJ)?!_AX(_B6,_AJ)?E(_AI):E(_AH):E(_AG);},_B8=function(_B9){var _Ba=E(_B9);return _B5(_Ba[1],_Ba[2]);},_Bb=function(_Bc){return [0,1/E(_Bc)[1]];},_Bd=function(_Be){var _Bf=E(_Be),_Bg=_Bf[1];return _Bg<0?[0, -_Bg]:E(_Bf);},_Bh=function(_Bi){var _Bj=E(_Bi);return _Bj[0]==0?_Bj[1]:I_toNumber(_Bj[1]);},_Bk=function(_Bl){return [0,_Bh(_Bl)];},_Bm=[0,0],_Bn=[0,1],_Bo=[0,-1],_Bp=function(_Bq){var _Br=E(_Bq)[1];return _Br!=0?_Br<=0?E(_Bo):E(_Bn):E(_Bm);},_Bs=function(_Bt,_Bu){return [0,E(_Bt)[1]-E(_Bu)[1]];},_Bv=function(_Bw){return [0, -E(_Bw)[1]];},_Bx=function(_By,_Bz){return [0,E(_By)[1]+E(_Bz)[1]];},_BA=function(_BB,_BC){return [0,E(_BB)[1]*E(_BC)[1]];},_BD=[0,_Bx,_BA,_Bs,_Bv,_Bd,_Bp,_Bk],_BE=function(_BF,_BG){return [0,E(_BF)[1]/E(_BG)[1]];},_BH=[0,_BD,_BE,_Bb,_B8],_BI=function(_BJ,_BK){return E(_BJ)[1]!=E(_BK)[1]?true:false;},_BL=function(_BM,_BN){return E(_BM)[1]==E(_BN)[1];},_BO=[0,_BL,_BI],_BP=function(_BQ,_BR){return E(_BQ)[1]<E(_BR)[1];},_BS=function(_BT,_BU){return E(_BT)[1]<=E(_BU)[1];},_BV=function(_BW,_BX){return E(_BW)[1]>E(_BX)[1];},_BY=function(_BZ,_C0){return E(_BZ)[1]>=E(_C0)[1];},_C1=function(_C2,_C3){var _C4=E(_C2)[1],_C5=E(_C3)[1];return _C4>=_C5?_C4!=_C5?2:1:0;},_C6=function(_C7,_C8){var _C9=E(_C7),_Ca=E(_C8);return _C9[1]>_Ca[1]?E(_C9):E(_Ca);},_Cb=function(_Cc,_Cd){var _Ce=E(_Cc),_Cf=E(_Cd);return _Ce[1]>_Cf[1]?E(_Cf):E(_Ce);},_Cg=[0,_BO,_C1,_BP,_BY,_BV,_BS,_C6,_Cb],_Ch=[0,1],_Ci=function(_Cj){return E(E(_Cj)[1]);},_Ck=function(_Cl){return E(E(_Cl)[2]);},_Cm=function(_Cn){return E(E(_Cn)[6]);},_Co=[0,2],_Cp=function(_Cq,_Cr){var _Cs=E(_Cr);return [1,_Cs,new T(function(){var _Ct=_Ci(_Cq);return _Cp(_Cq,A(_Ct[1],[_Cs,new T(function(){return A(_Ct[7],[_Ch]);})]));})];},_Cu=function(_Cv,_Cw){var _Cx=E(_Cw);if(!_Cx[0]){return [0];}else{var _Cy=_Cx[1];return !A(_Cv,[_Cy])?[0]:[1,_Cy,new T(function(){return _Cu(_Cv,_Cx[2]);})];}},_Cz=function(_CA,_CB,_CC,_CD){var _CE=new T(function(){return _Cm(_CA);});return _Cu(function(_CF){return A(_CE,[_CF,new T(function(){var _CG=_Ci(_CB),_CH=_CG[7];return A(_CG[1],[_CD,new T(function(){return A(_Ck,[_CB,new T(function(){return A(_CH,[_Ch]);}),new T(function(){return A(_CH,[_Co]);})]);})]);})]);},_Cp(_CB,_CC));},_CI=new T(function(){return _Cz(_Cg,_BH,_Ae,_Ah);}),_CJ=function(_CK,_CL){var _CM=E(_CK);if(!_CM[0]){return [0];}else{var _CN=E(_CL);return _CN[0]==0?[0]:[1,[0,_CM[1],_CN[1]],new T(function(){return _CJ(_CM[2],_CN[2]);})];}},_CO=function(_CP,_CQ,_){var _CR=function(_CS,_){var _CT=E(_CS);if(!_CT[0]){return _9;}else{var _CU=A(_A1,[E(toJSStr(_zn(120,new T(function(){return A(_zv,[_zk,_pd,E(_CT[1])[1],_9]);}),_CP))),_]),_CV=_CR(_CT[2],_);return [1,[0,_CU],_CV];}};return _4F(_zX,function(_CW,_CX,_){return (function(_CY,_){return [0,[0,function(_CZ,_){var _D0=A(_Ad,[_]),_D1=E(_D0);if(!_D1[0]){var _D2=_36(_zZ,_);return _CZ;}else{var _D3=_CR(_CI,_),_D4=E(_D1[1]),_D5=jsResetCanvas(E(_D4[2])[1]),_D6=E(_D4[1]),_D7=_D6[1],_D8=jsPushState(_D7),_D9=jsScale(_D7,3,1),_Da=jsPushState(_D7),_Db=jsTranslate(_D7,50,130),_Dc=jsPushState(_D7),_Dd=jsRotate(_D7,3.141592653589793),_De=jsBeginPath(_D7),_Df=A(_Ay,[_D6,_]),_Dg=A(_AF,[_D6,_]),_Dh=A(_An,[_CJ(_CI,_D3),_D6,_]),_Di=jsStroke(_D7),_Dj=jsPopState(_D7),_Dk=jsPopState(_D7),_Dl=jsPopState(_D7);return _CZ;}},_zV],_CY];})(_CX,_);},_CQ,_);},_Dm=unCStr("Math.pow(x,2)+x+10;"),_Dn=[1,_Dm],_Do=function(_Dp,_Dq,_){return [0,[0,_38,[1,_Dp]],_Dq];},_Dr=function(_Ds,_Dt,_Du,_){return _4F(_Ds,function(_Dv){return E(_Dt);},_Du,_);},_Dw=function(_Dx,_Dy,_11,_){return _Dr(_Dx,_Dy,_11,_);},_Dz=function(_DA){return err(_DA);},_DB=[0,_4F,_Dw,_Do,_Dz],_DC=function(_DD){return E(E(_DD)[1]);},_DE=function(_DF,_DG,_DH,_DI,_DJ){var _DK=new T(function(){return _93(_DF);}),_DL=new T(function(){return _DC(_DK);}),_DM=new T(function(){return _3B(_DG);}),_DN=new T(function(){return _p9(_DF);}),_DO=new T(function(){return _3b(_DG);}),_DP=new T(function(){return _3B(_DG);}),_DQ=new T(function(){return _3b(_DG);});return A(_DH,[function(_DR){return A(_DQ,[new T(function(){return A(_DI,[_DR]);}),function(_DS){var _DT=E(_DS),_DU=E(_DT[1]);return A(_DP,[[0,[0,_DU[1],[1,_DU[2]]],_DT[2]]]);}]);},function(_DV){var _DW=E(_DV);if(!_DW[0]){return function(_DX){return A(_DM,[[0,[0,_DL,_a],_DX]]);};}else{var _DY=new T(function(){return A(_DJ,[_DW[1]]);});return function(_DZ){return A(_DO,[new T(function(){return A(_DY,[_DZ]);}),function(_E0){var _E1=E(_E0),_E2=_E1[2],_E3=E(_E1[1]);return _E3[0]==0?A(_DM,[[0,[0,_DL,_DW],_E2]]):A(_DM,[[0,[0,new T(function(){return A(_DN,[_E3[1]]);}),_a],_E2]]);}]);};}}]);},_E4=function(_E5,_E6,_E7){var _E8=new T(function(){return A(_oU,[_E5,_9]);}),_E9=new T(function(){return _on(_E7);}),_Ea=new T(function(){return _3B(_E9);}),_Eb=new T(function(){return _rW(_E5,_E7,_p6,_wM,_uA);});return function(_Ec){return _DE(_E5,_E9,E(_E6)[1],new T(function(){return A(_Eb,[_a,_9L,_Ec]);}),function(_Ed,_Ee){return E(_Ed)[0]==0?A(_Ea,[[0,[1,_E8],_Ee]]):A(_Ea,[[0,_a,_Ee]]);});};},_Ef=new T(function(){return _E4(_8G,_DB,_9M);}),_Eg=new T(function(){return A(_Ef,[_Dn]);}),_Eh=function(_Ei,_){var _Ej=_sX(_Eg,_9K,_Ei,_),_Ek=E(_Ej),_El=E(_Ek[1]);return [0,[0,function(_Em,_){var _En=A(_El[1],[_Em,_]),_Eo=_5l(_Em,_);return _Em;},new T(function(){var _Ep=E(_El[2]);return _Ep[0]==0?E(_Dn):E(_Ep);})],_Ek[2]];},_Eq=function(_Er,_){var _Es=_4F(_Eh,_CO,_Er,_),_Et=E(_Es),_Eu=E(_Et[1]),_Ev=new T(function(){return _z6(_tw,_Eu[1]);});return [0,[0,function(_Ew,_){var _Ex=A(_zg,[_Ew,_]),_Ey=A(_Ev,[_Ew,_]);return _Ew;},_Eu[2]],_Et[2]];},_Ez=unCStr("this example show a image gallery. It advances each 20 seconds and by pressing the button"),_EA=new T(function(){return _59(_p,_Ez);}),_EB=function(_EC,_){return _EC;},_ED=[1,_5A],_EE=unCStr("main"),_EF=unCStr("Main"),_EG=unCStr("GalleryIndex"),_EH=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_EE,_EF,_EG],_EI=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_EH,_9],_EJ=function(_EK){return E(_EI);},_EL=function(_EM,_EN){var _EO=hs_leWord64(_EM,_EN);return E(_EO)==0?false:true;},_EP=function(_EQ,_ER,_ES,_ET){var _EU=hs_eqWord64(_EQ,_ES);if(!E(_EU)){var _EV=hs_leWord64(_EQ,_ES);return E(_EV)==0?false:true;}else{return _EL(_ER,_ET);}},_EW=function(_EX,_EY){var _EZ=E(_EX),_F0=_EZ[1],_F1=_EZ[2],_F2=E(_EY),_F3=_F2[1],_F4=_F2[2],_F5=hs_eqWord64(_F0,_F3);if(!E(_F5)){return !_EP(_F0,_F1,_F3,_F4)?2:0;}else{var _F6=hs_eqWord64(_F1,_F4);return E(_F6)==0?!_EP(_F0,_F1,_F3,_F4)?2:0:1;}},_F7=function(_F8,_F9,_Fa,_Fb,_Fc){while(1){var _Fd=E(_Fc);if(!_Fd[0]){switch(_EW([0,_F8,_F9,_Fa,_Fb],_Fd[2])){case 0:_Fc=_Fd[4];continue;case 1:return [1,_Fd[3]];default:_Fc=_Fd[5];continue;}}else{return [0];}}},_Fe=function(_Ff,_Fg){var _Fh=E(_Ff),_Fi=_Fh[1],_Fj=_Fh[2],_Fk=_Fh[3],_Fl=_Fh[4],_Fm=E(_Fg);if(!_Fm[0]){switch(_EW(_Fh,_Fm[2])){case 0:return _F7(_Fi,_Fj,_Fk,_Fl,_Fm[4]);case 1:return [1,_Fm[3]];default:return _F7(_Fi,_Fj,_Fk,_Fl,_Fm[5]);}}else{return [0];}},_Fn=function(_Fo,_Fp,_Fq,_Fr){var _Fs=E(_Fp),_Ft=_Fs[1],_Fu=_Fs[3],_Fv=new T(function(){return A(_Fr,[_p8]);}),_Fw=new T(function(){return A(_Fu,[_a]);});return A(_Ft,[new T(function(){return A(_Ft,[_Fq,function(_Fx){return A(_Fu,[new T(function(){var _Fy=E(_Fo);return E(E(_Fx)[5]);})]);}]);}),function(_Fz){var _FA=_Fe(_Fv,_Fz);return _FA[0]==0?E(_Fw):A(_Fu,[[1,_FA[1]]]);}]);},_FB=new T(function(){return _Fn(_1e,_3I,_1c,_EJ);}),_FC=function(_FD,_){var _FE=A(_FB,[_FD,_]);return [0,[0,_EB,new T(function(){var _FF=E(E(_FE)[1]);return _FF[0]==0?E(_ED):E(_FF);})],new T(function(){return E(E(_FE)[2]);})];},_FG=unCStr("Failure in Data.Map.balanceL"),_FH=new T(function(){return err(_FG);}),_FI=function(_FJ,_FK,_FL,_FM){var _FN=E(_FM);if(!_FN[0]){var _FO=_FN[1],_FP=E(_FL);if(!_FP[0]){var _FQ=_FP[1],_FR=_FP[2],_FS=_FP[3];if(_FQ<=(imul(3,_FO)|0)){return [0,(1+_FQ|0)+_FO|0,E(E(_FJ)),_FK,E(_FP),E(_FN)];}else{var _FT=E(_FP[4]);if(!_FT[0]){var _FU=_FT[1],_FV=E(_FP[5]);if(!_FV[0]){var _FW=_FV[1],_FX=_FV[2],_FY=_FV[3],_FZ=_FV[4];if(_FW>=(imul(2,_FU)|0)){var _G0=function(_G1){var _G2=E(_FV[5]);return _G2[0]==0?[0,(1+_FQ|0)+_FO|0,E(_FX),_FY,E([0,(1+_FU|0)+_G1|0,E(_FR),_FS,E(_FT),E(_FZ)]),E([0,(1+_FO|0)+_G2[1]|0,E(E(_FJ)),_FK,E(_G2),E(_FN)])]:[0,(1+_FQ|0)+_FO|0,E(_FX),_FY,E([0,(1+_FU|0)+_G1|0,E(_FR),_FS,E(_FT),E(_FZ)]),E([0,1+_FO|0,E(E(_FJ)),_FK,E(_8),E(_FN)])];},_G3=E(_FZ);return _G3[0]==0?_G0(_G3[1]):_G0(0);}else{return [0,(1+_FQ|0)+_FO|0,E(_FR),_FS,E(_FT),E([0,(1+_FO|0)+_FW|0,E(E(_FJ)),_FK,E(_FV),E(_FN)])];}}else{return E(_FH);}}else{return E(_FH);}}}else{return [0,1+_FO|0,E(E(_FJ)),_FK,E(_8),E(_FN)];}}else{var _G4=E(_FL);if(!_G4[0]){var _G5=_G4[1],_G6=_G4[2],_G7=_G4[3],_G8=_G4[5],_G9=E(_G4[4]);if(!_G9[0]){var _Ga=_G9[1],_Gb=E(_G8);if(!_Gb[0]){var _Gc=_Gb[1],_Gd=_Gb[2],_Ge=_Gb[3],_Gf=_Gb[4];if(_Gc>=(imul(2,_Ga)|0)){var _Gg=function(_Gh){var _Gi=E(_Gb[5]);return _Gi[0]==0?[0,1+_G5|0,E(_Gd),_Ge,E([0,(1+_Ga|0)+_Gh|0,E(_G6),_G7,E(_G9),E(_Gf)]),E([0,1+_Gi[1]|0,E(E(_FJ)),_FK,E(_Gi),E(_8)])]:[0,1+_G5|0,E(_Gd),_Ge,E([0,(1+_Ga|0)+_Gh|0,E(_G6),_G7,E(_G9),E(_Gf)]),E([0,1,E(E(_FJ)),_FK,E(_8),E(_8)])];},_Gj=E(_Gf);return _Gj[0]==0?_Gg(_Gj[1]):_Gg(0);}else{return [0,1+_G5|0,E(_G6),_G7,E(_G9),E([0,1+_Gc|0,E(E(_FJ)),_FK,E(_Gb),E(_8)])];}}else{return [0,3,E(_G6),_G7,E(_G9),E([0,1,E(E(_FJ)),_FK,E(_8),E(_8)])];}}else{var _Gk=E(_G8);return _Gk[0]==0?[0,3,E(_Gk[2]),_Gk[3],E([0,1,E(_G6),_G7,E(_8),E(_8)]),E([0,1,E(E(_FJ)),_FK,E(_8),E(_8)])]:[0,2,E(E(_FJ)),_FK,E(_G4),E(_8)];}}else{return [0,1,E(E(_FJ)),_FK,E(_8),E(_8)];}}},_Gl=unCStr("Failure in Data.Map.balanceR"),_Gm=new T(function(){return err(_Gl);}),_Gn=function(_Go,_Gp,_Gq,_Gr){var _Gs=E(_Gq);if(!_Gs[0]){var _Gt=_Gs[1],_Gu=E(_Gr);if(!_Gu[0]){var _Gv=_Gu[1],_Gw=_Gu[2],_Gx=_Gu[3];if(_Gv<=(imul(3,_Gt)|0)){return [0,(1+_Gt|0)+_Gv|0,E(E(_Go)),_Gp,E(_Gs),E(_Gu)];}else{var _Gy=E(_Gu[4]);if(!_Gy[0]){var _Gz=_Gy[1],_GA=_Gy[2],_GB=_Gy[3],_GC=_Gy[4],_GD=E(_Gu[5]);if(!_GD[0]){var _GE=_GD[1];if(_Gz>=(imul(2,_GE)|0)){var _GF=function(_GG){var _GH=E(_Go),_GI=E(_Gy[5]);return _GI[0]==0?[0,(1+_Gt|0)+_Gv|0,E(_GA),_GB,E([0,(1+_Gt|0)+_GG|0,E(_GH),_Gp,E(_Gs),E(_GC)]),E([0,(1+_GE|0)+_GI[1]|0,E(_Gw),_Gx,E(_GI),E(_GD)])]:[0,(1+_Gt|0)+_Gv|0,E(_GA),_GB,E([0,(1+_Gt|0)+_GG|0,E(_GH),_Gp,E(_Gs),E(_GC)]),E([0,1+_GE|0,E(_Gw),_Gx,E(_8),E(_GD)])];},_GJ=E(_GC);return _GJ[0]==0?_GF(_GJ[1]):_GF(0);}else{return [0,(1+_Gt|0)+_Gv|0,E(_Gw),_Gx,E([0,(1+_Gt|0)+_Gz|0,E(E(_Go)),_Gp,E(_Gs),E(_Gy)]),E(_GD)];}}else{return E(_Gm);}}else{return E(_Gm);}}}else{return [0,1+_Gt|0,E(E(_Go)),_Gp,E(_Gs),E(_8)];}}else{var _GK=E(_Gr);if(!_GK[0]){var _GL=_GK[1],_GM=_GK[2],_GN=_GK[3],_GO=_GK[5],_GP=E(_GK[4]);if(!_GP[0]){var _GQ=_GP[1],_GR=_GP[2],_GS=_GP[3],_GT=_GP[4],_GU=E(_GO);if(!_GU[0]){var _GV=_GU[1];if(_GQ>=(imul(2,_GV)|0)){var _GW=function(_GX){var _GY=E(_Go),_GZ=E(_GP[5]);return _GZ[0]==0?[0,1+_GL|0,E(_GR),_GS,E([0,1+_GX|0,E(_GY),_Gp,E(_8),E(_GT)]),E([0,(1+_GV|0)+_GZ[1]|0,E(_GM),_GN,E(_GZ),E(_GU)])]:[0,1+_GL|0,E(_GR),_GS,E([0,1+_GX|0,E(_GY),_Gp,E(_8),E(_GT)]),E([0,1+_GV|0,E(_GM),_GN,E(_8),E(_GU)])];},_H0=E(_GT);return _H0[0]==0?_GW(_H0[1]):_GW(0);}else{return [0,1+_GL|0,E(_GM),_GN,E([0,1+_GQ|0,E(E(_Go)),_Gp,E(_8),E(_GP)]),E(_GU)];}}else{return [0,3,E(_GR),_GS,E([0,1,E(E(_Go)),_Gp,E(_8),E(_8)]),E([0,1,E(_GM),_GN,E(_8),E(_8)])];}}else{var _H1=E(_GO);return _H1[0]==0?[0,3,E(_GM),_GN,E([0,1,E(E(_Go)),_Gp,E(_8),E(_8)]),E(_H1)]:[0,2,E(E(_Go)),_Gp,E(_8),E(_GK)];}}else{return [0,1,E(E(_Go)),_Gp,E(_8),E(_8)];}}},_H2=function(_H3,_H4,_H5,_H6,_H7,_H8){var _H9=E(_H8);if(!_H9[0]){var _Ha=_H9[2],_Hb=_H9[3],_Hc=_H9[4],_Hd=_H9[5];switch(_EW([0,_H3,_H4,_H5,_H6],_Ha)){case 0:return _FI(_Ha,_Hb,_H2(_H3,_H4,_H5,_H6,_H7,_Hc),_Hd);case 1:return [0,_H9[1],E([0,_H3,_H4,_H5,_H6]),_H7,E(_Hc),E(_Hd)];default:return _Gn(_Ha,_Hb,_Hc,_H2(_H3,_H4,_H5,_H6,_H7,_Hd));}}else{return [0,1,E([0,_H3,_H4,_H5,_H6]),_H7,E(_8),E(_8)];}},_He=unCStr("100%"),_Hf=[0,62],_Hg=[1,_Hf,_9],_Hh=[1,_Hg],_Hi=new T(function(){return A(_wN,[_a,_wO]);}),_Hj=new T(function(){return A(_Hi,[_Hh]);}),_Hk=function(_CX,_){return _sX(_Hj,_tN,_CX,_);},_Hl=function(_Hm){return E(_Hk);},_Hn=unCStr("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRAgKkpDyzk8kdIqk5ECsZ14XgbpBzyWFvrCrHombkSBAUn6jFo"),_Ho=[1,_Hn,_9],_Hp=unCStr("https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSfP70npv4FOrkBjScP0tVu2t3veSNoFQ6MMxX6LDO8kldNeu-DxQ"),_Hq=[1,_Hp,_Ho],_Hr=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcS53axpzkDyzEUAdaIP3YsaHuR-_YqN9qFK3W4bp_D2OBZfW5BU_Q"),_Hs=[1,_Hr,_Hq],_Ht=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQ_ywj-zxDq3h_B4l48XHsjTywrdbK5egxvhxkYJ1HOkDFXd_-H"),_Hu=[1,_Ht,_Hs],_Hv=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQmmC4kV3NPFIpGL_x4H_iHG_p-c93DGjWfkxVtjxEFVng7A8o-nw"),_Hw=[1,_Hv,_Hu],_Hx=unCStr("http://almaer.com/blog/uploads/interview-haskell.png"),_Hy=[1,_Hx,_Hw],_Hz=unCStr("height"),_HA=function(_HB,_HC){while(1){var _HD=E(_HB);if(!_HD[0]){return E(_HC);}else{_HB=_HD[2];var _HE=_HC+1|0;_HC=_HE;continue;}}},_HF=new T(function(){return [0,_HA(_Hy,0)-1|0];}),_HG=function(_HH,_){return [0,[0,_38,[1,_HH]],_HH];},_HI=[0,_38,_5p],_HJ=unCStr("src"),_HK=unCStr("img"),_HL=function(_HM,_HN){var _HO=new T(function(){return A(_HM,[_HN]);});return function(_HP,_){var _HQ=jsCreateElem(toJSStr(E(_HK))),_HR=jsAppendChild(_HQ,E(_HP)[1]),_HS=[0,_HQ],_HT=A(_HO,[_HS,_]);return _HS;};},_HU=new T(function(){return _HL(_tw,_38);}),_HV=unCStr("width"),_HW=function(_HX){return function(_cj,_s6){return _4F(function(_CX,_){return _4F(_HG,function(_HY){return function(_HZ,_){return [0,_HI,new T(function(){var _I0=E(_HY);return [0,_I0[1],_I0[2],_I0[3],_I0[4],new T(function(){return _H2(I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_EH,_9,new T(function(){var _I1=E(_HX)[1];return _I1!=E(_HF)[1]?[0,_I1+1|0]:E(_5A);}),_I0[5]);})];})];};},_CX,_);},function(_I2,_CX,_){return (function(_CX,_){return _4F(function(_I3,_){return [0,[0,function(_I4,_){var _I5=A(_HU,[_I4,_]),_I6=A(_1,[_n,_I5,_HJ,new T(function(){var _I7=E(_HX)[1];return _I7>=0?_uF(_Hy,_I7):E(_uC);}),_]),_I8=A(_1,[_n,_I5,_HV,_He,_]),_I9=A(_1,[_n,_I5,_Hz,_He,_]),_Ia=_5l(_I4,_);return _I4;},_zV],_I3];},_Hl,_CX,_);})(_CX,_);},_cj,_s6);};},_Ib=function(_CX,_){return _4F(_FC,_HW,_CX,_);},_Ic=function(_Id,_Ie,_){return _If(_Ie,_);},_Ig=function(_CX,_){return _xc(_Ib,_Ic,_CX,_);},_Ih=[0,20000],_Ii=new T(function(){return _3V(_1e,_3I,_1c,_19);}),_Ij=function(_Ik,_Il,_Im,_){var _In=A(_Ii,[_Im,_]),_Io=new T(function(){return E(E(_In)[1]);}),_Ip=function(_){var _Iq=jsSetTimeout(E(_Ik)[1],function(_){var _Ir=_4b(_Il,_Io,_),_Is=E(_Ir);if(!_Is[0]){return _Ip(_);}else{var _It=E(_Is[1]);return _0;}});return _0;},_Iu=_Ip(_);return _wV(_Io,_Il,new T(function(){return E(E(_In)[2]);}),_);},_If=function(_Iv,_){var _Iw=_Ij(_Ih,_Ig,_Iv,_),_Ix=E(_Iw),_Iy=E(_Ix[1]);return [0,[0,function(_Iz,_){var _IA=A(_EA,[_Iz,_]),_IB=A(_Iy[1],[_Iz,_]);return _Iz;},_Iy[2]],_Ix[2]];},_IC=unCStr("style"),_ID=unCStr("This widget sum recursively n numbers, but remember the previos entries when one entry is edited"),_IE=new T(function(){return _59(_p,_ID);}),_IF=function(_IG,_IH,_II){var _IJ=E(_II);if(!_IJ[0]){var _IK=_IJ[3],_IL=_IJ[4],_IM=_IJ[5],_IN=E(_IJ[2]),_IO=_IN[1];return _IG>=_IO?_IG!=_IO?_Gn(_IN,_IK,_IL,_IF(_IG,_IH,_IM)):[0,_IJ[1],E([0,_IG]),_IH,E(_IL),E(_IM)]:_FI(_IN,_IK,_IF(_IG,_IH,_IL),_IM);}else{return [0,1,E([0,_IG]),_IH,E(_8),E(_8)];}},_IP=function(_IQ,_IR,_IS){var _IT=E(_IQ),_IU=_IT[1],_IV=E(_IS);if(!_IV[0]){var _IW=_IV[3],_IX=_IV[4],_IY=_IV[5],_IZ=E(_IV[2]),_J0=_IZ[1];return _IU>=_J0?_IU!=_J0?_Gn(_IZ,_IW,_IX,_IF(_IU,_IR,_IY)):[0,_IV[1],E(_IT),_IR,E(_IX),E(_IY)]:_FI(_IZ,_IW,_IF(_IU,_IR,_IX),_IY);}else{return [0,1,E(_IT),_IR,E(_8),E(_8)];}},_J1=function(_J2,_J3,_J4){var _J5=E(_J2),_J6=_J5[1],_J7=_J5[2],_J8=_J5[3],_J9=_J5[4],_Ja=E(_J4);if(!_Ja[0]){var _Jb=_Ja[2],_Jc=_Ja[3],_Jd=_Ja[4],_Je=_Ja[5];switch(_EW(_J5,_Jb)){case 0:return _FI(_Jb,_Jc,_H2(_J6,_J7,_J8,_J9,_J3,_Jd),_Je);case 1:return [0,_Ja[1],E(_J5),_J3,E(_Jd),E(_Je)];default:return _Gn(_Jb,_Jc,_Jd,_H2(_J6,_J7,_J8,_J9,_J3,_Je));}}else{return [0,1,E(_J5),_J3,E(_8),E(_8)];}},_Jf=function(_Jg,_Jh){while(1){var _Ji=E(_Jh);if(!_Ji[0]){var _Jj=E(_Ji[2])[1];if(_Jg>=_Jj){if(_Jg!=_Jj){_Jh=_Ji[5];continue;}else{return [1,_Ji[3]];}}else{_Jh=_Ji[4];continue;}}else{return [0];}}},_Jk=[0,_38,_a],_Jl=function(_Jm,_){return [0,_Jk,_Jm];},_Jn=unCStr("containers-0.5.5.1"),_Jo=unCStr("Data.Map.Base"),_Jp=unCStr("Map"),_Jq=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_Jn,_Jo,_Jp],_Jr=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_Jq,_9],_Js=function(_Jt){return E(_Jr);},_Ju=function(_Jv){var _Jw=E(_Jv);if(!_Jw[0]){return [0];}else{var _Jx=E(_Jw[1]);return [1,[0,_Jx[1],_Jx[2]],new T(function(){return _Ju(_Jw[2]);})];}},_Jy=function(_Jz,_JA){return function(_JB){return E(new T(function(){var _JC=A(_Jz,[_6G]),_JD=E(_JC[3]),_JE=_JD[1],_JF=_JD[2],_JG=_1R(_JC[4],[1,new T(function(){return A(_JA,[_6G]);}),_9]);if(!_JG[0]){return [0,_JE,_JF,_JD,_9];}else{var _JH=_6b(new T(function(){return _5Z(_6n(_6y,[1,[0,_JE,_JF],new T(function(){return _Ju(_JG);})]));}));return [0,_JH[1],_JH[2],_JD,_JG];}}));};},_JI=new T(function(){return _Jy(_Js,_ol);}),_JJ=new T(function(){return _6H(_JI,_ol);}),_JK=new T(function(){return _Fn(_1e,_3I,_1c,_JJ);}),_JL=function(_JM,_){var _JN=A(_JK,[_JM,_]);return [0,[0,_38,new T(function(){return E(E(_JN)[1]);})],new T(function(){return E(E(_JN)[2]);})];},_JO=new T(function(){return _6H(_JI,_ol);}),_JP=[1,_8],_JQ=new T(function(){return _Fn(_1e,_3I,_1c,_JO);}),_JR=function(_JS,_){var _JT=A(_JQ,[_JS,_]);return [0,[0,_EB,new T(function(){var _JU=E(E(_JT)[1]);return _JU[0]==0?E(_JP):E(_JU);})],new T(function(){return E(E(_JT)[2]);})];},_JV=[0,_38,_5p],_JW=[1,_a],_JX=function(_JY,_JZ){var _K0=new T(function(){return [0,E(_JY)[1]+1|0];});return function(_cj,_s6){return _4F(function(_CX,_){return _4F(function(_K1,_){var _K2=_4F(_JL,function(_K3){var _K4=_Jf(E(_JY)[1],_K3);return _K4[0]==0?E(_Jl):function(_K5,_){return [0,[0,_38,_K4],_K5];};},_K1,_),_K6=E(_K2),_K7=E(_K6[1]);return [0,[0,function(_K8,_){var _K9=A(_K7[1],[_K8,_]);return _K8;},new T(function(){var _Ka=E(_K7[2]);return _Ka[0]==0?E(_JW):[1,_Ka];})],_K6[2]];},function(_Kb){var _Kc=new T(function(){return A(_s8,[_a,_9L,_Kb]);});return function(_cj,_s6){return _4F(function(_Kd,_){var _Ke=_sX(_Kc,_9K,_Kd,_),_Kf=E(_Ke),_Kg=_Kf[2],_Kh=E(_Kf[1]),_Ki=_Kh[1],_Kj=_Kh[2],_Kk=E(_Kb);return _Kk[0]==0?[0,[0,function(_Kl,_){var _Km=A(_Ki,[_Kl,_]);return _Kl;},_Kj],_Kg]:[0,[0,function(_Kn,_){var _Ko=A(_Ki,[_Kn,_]);return _Kn;},new T(function(){var _Kp=E(_Kj);return _Kp[0]==0?E(_Kk):E(_Kp);})],_Kg];},function(_Kq,_Kr,_){return _4F(function(_CX,_){return _4F(_JR,function(_Ks){var _Kt=new T(function(){return _IP(_JY,_Kq,_Ks);}),_Ku=new T(function(){return A(_JO,[_Kt]);});return function(_cj,_s6){return _4F(_HG,function(_Kv){return function(_Kw,_){return [0,_JV,new T(function(){var _Kx=E(_Kv);return [0,_Kx[1],_Kx[2],_Kx[3],_Kx[4],new T(function(){return _J1(_Ku,_Kt,_Kx[5]);})];})];};},_cj,_s6);};},_CX,_);},function(_Ky,_CX,_){return (function(_Kz,_){return [0,[0,_38,[1,_Kq]],_Kz];})(_CX,_);},_Kr,_);},_cj,_s6);};},_CX,_);},function(_KA){var _KB=new T(function(){return _JX(_K0,new T(function(){return _8V(_JZ,_KA);}));}),_KC=new T(function(){return _w(_p,new T(function(){return _3Q(0,E(_JZ)[1]+E(_KA)[1]|0,_9);}));});return function(_cj,_s6){return _4F(function(_KD,_){return [0,[0,function(_KE,_){var _KF=A(_KC,[_KE,_]),_KG=_5l(_KE,_);return _KE;},_5p],_KD];},function(_KH){return E(_KB);},_cj,_s6);};},_cj,_s6);};},_KI=new T(function(){return _JX(_5A,_5A);}),_KJ=unCStr("This widget sum recursively n numbers. When enters 0, present the result"),_KK=new T(function(){return _59(_p,_KJ);}),_KL=new T(function(){return A(_s8,[_a,_9L,_a]);}),_KM=function(_CX,_){return _sX(_KL,_9K,_CX,_);},_KN=function(_KO){var _KP=new T(function(){return _w(_p,new T(function(){return _5i(_KO);}));});return function(_cj,_s6){return _4F(_KM,function(_KQ){var _KR=E(E(_KQ)[1]);if(!_KR){return function(_KS,_){return [0,[0,function(_KT,_){var _KU=_5l(_KT,_),_KV=_p(_5q,_KT,_),_KW=A(_KP,[_KT,_]);return _KT;},_a],_KS];};}else{var _KX=new T(function(){return _KN(new T(function(){return [0,E(_KO)[1]+_KR|0];}));}),_KY=new T(function(){return _w(_p,new T(function(){return _3Q(0,E(_KO)[1]+_KR|0,_9);}));});return function(_cj,_s6){return _4F(function(_KZ,_){return [0,[0,function(_L0,_){var _L1=A(_KY,[_L0,_]),_L2=_5l(_L0,_);return _L0;},_5p],_KZ];},function(_L3){return E(_KX);},_cj,_s6);};}},_cj,_s6);};},_L4=new T(function(){return _KN(_5A);}),_L5=unCStr("This widget sum two numbers and append the result. Using applicative and monadic expressions"),_L6=new T(function(){return _59(_p,_L5);}),_L7=function(_L8){return function(_L9,_){return [0,[0,new T(function(){var _La=new T(function(){return _w(_p,new T(function(){return _5i(_L8);}));});return _59(_tw,function(_Lb,_){var _Lc=_p(_5q,_Lb,_),_Ld=A(_La,[_Lb,_]);return _Lb;});}),_5p],_L9];};},_Le=new T(function(){return A(_s8,[_a,_9L,_a]);}),_Lf=new T(function(){return A(_s8,[_a,_9L,_a]);}),_Lg=unCStr("second number "),_Lh=unCStr("first number"),_Li=function(_Lj,_){var _Lk=_sX(_Lf,_9K,_Lj,_),_Ll=E(_Lk),_Lm=E(_Ll[1]),_Ln=_sX(_Le,_9K,_Ll[2],_),_Lo=E(_Ln),_Lp=E(_Lo[1]);return [0,[0,function(_Lq,_){var _Lr=_p(_Lh,_Lq,_),_Ls=_5l(_Lq,_),_Lt=A(_Lm[1],[_Lq,_]),_Lu=_5l(_Lq,_),_Lv=_p(_Lg,_Lq,_),_Lw=_5l(_Lq,_),_Lx=A(_Lp[1],[_Lq,_]),_Ly=_5l(_Lq,_);return _Lq;},new T(function(){var _Lz=E(_Lm[2]);if(!_Lz[0]){return [0];}else{var _LA=E(_Lp[2]);return _LA[0]==0?[0]:[1,new T(function(){return _8V(_Lz[1],_LA[1]);})];}})],_Lo[2]];},_LB=function(_LC,_){var _LD=_4F(_Li,_L7,_LC,_),_LE=E(_LD),_LF=E(_LE[1]),_LG=new T(function(){return _59(_tw,_LF[1]);});return [0,[0,function(_LH,_){var _LI=A(_L6,[_LH,_]),_LJ=A(_LG,[_LH,_]);return _LH;},_LF[2]],_LE[2]];},_LK=unCStr("td"),_LL=unCStr("tr"),_LM=function(_LN,_){var _LO=_LB(_LN,_),_LP=E(_LO),_LQ=E(_LP[1]),_LR=A(_tK,[_LP[2],_]),_LS=E(_LR),_LT=E(_LS[1]),_LU=A(_L4,[_LS[2],_]),_LV=E(_LU),_LW=E(_LV[1]),_LX=A(_yp,[_LV[2],_]),_LY=E(_LX),_LZ=E(_LY[1]),_M0=A(_KI,[_LY[2],_]),_M1=E(_M0),_M2=E(_M1[1]),_M3=A(_z4,[_M1[2],_]),_M4=E(_M3),_M5=E(_M4[1]),_M6=_Eq(_M4[2],_),_M7=E(_M6),_M8=E(_M7[1]),_M9=_If(_M7[2],_),_Ma=E(_M9),_Mb=E(_Ma[1]);return [0,[0,function(_Mc,_){var _Md=_13(_LL,_Mc,_),_Me=_13(_LK,_Md,_),_Mf=A(_LQ[1],[_Me,_]),_Mg=_13(_LK,_Md,_),_Mh=A(_LT[1],[_Mg,_]),_Mi=_13(_LK,_Md,_),_Mj=A(_KK,[_Mi,_]),_Mk=A(_LW[1],[_Mi,_]),_Ml=A(_1,[_n,_Md,_J,_K,_]),_Mm=_13(_LL,_Mc,_),_Mn=_13(_LK,_Mm,_),_Mo=A(_LZ[1],[_Mn,_]),_Mp=_13(_LK,_Mm,_),_Mq=A(_IE,[_Mp,_]),_Mr=A(_M2[1],[_Mp,_]),_Ms=_13(_LK,_Mm,_),_Mt=A(_M5[1],[_Ms,_]),_Mu=A(_1,[_n,_Mm,_J,_K,_]),_Mv=_13(_LL,_Mc,_),_Mw=_13(_LK,_Mv,_),_Mx=A(_M8[1],[_Mw,_]),_My=_13(_LK,_Mv,_),_Mz=A(_Mb[1],[_My,_]),_MA=A(_1,[_n,_Mv,_IC,_K,_]);return _Mc;},new T(function(){var _MB=E(_LQ[2]);if(!_MB[0]){var _MC=E(_LT[2]);if(!_MC[0]){var _MD=E(_LW[2]);if(!_MD[0]){var _ME=E(_LZ[2]);if(!_ME[0]){var _MF=E(_M2[2]);if(!_MF[0]){var _MG=E(_M5[2]);if(!_MG[0]){var _MH=E(_M8[2]);return _MH[0]==0?E(_Mb[2]):E(_MH);}else{return E(_MG);}}else{return E(_MF);}}else{return E(_ME);}}else{return E(_MD);}}else{return E(_MC);}}else{return E(_MB);}})],_Ma[2]];},_MI=unCStr("h1"),_MJ=function(_MK,_ML){var _MM=new T(function(){return A(_MK,[_ML]);});return function(_MN,_){var _MO=jsCreateElem(toJSStr(E(_MI))),_MP=jsAppendChild(_MO,E(_MN)[1]),_MQ=[0,_MO],_MR=A(_MM,[_MQ,_]);return _MQ;};},_MS=unCStr("hplayground examples"),_MT=new T(function(){return _MJ(_p,_MS);}),_MU=unCStr("idelem"),_MV=function(_){var _MW=E(_MU),_MX=jsFind(toJSStr(_MW)),_MY=E(_MX);if(!_MY[0]){return _49(_MW);}else{var _MZ=_MY[1],_N0=E(_m)[1],_N1=takeMVar(_N0),_N2=_LM(_N1,_),_N3=E(_N2),_N4=E(_N3[1]),_=putMVar(_N0,_N3[2]),_N5=A(_MT,[_MZ,_]),_N6=A(_1,[_n,_N5,_IC,_I,_]),_N7=_13(_H,_MZ,_),_N8=A(_N4[1],[_N7,_]),_N9=A(_G,[_MZ,_]);return _N4[2];}},_Na=function(_){return _MV(_);};
var hasteMain = function() {A(_Na, [0]);};window.onload = hasteMain;