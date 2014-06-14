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

var _0=0,_1=function(_2,_3,_4,_5){return A(_2,[new T(function(){return function(_){var _6=jsSetAttr(E(_3)[1],toJSStr(E(_4)),toJSStr(E(_5)));return _0;};})]);},_7=function(_8){return E(_8);},_9=[0,0],_a=unCStr("id"),_b=[0],_c=unCStr("span"),_d=function(_e,_f,_){var _g=A(_e,[_]);return A(_f,[_]);},_h=function(_i,_j,_){return _d(_i,_j,_);},_k=function(_l,_m,_){var _n=A(_l,[_]);return A(_m,[_n,_]);},_o=unCStr("base"),_p=unCStr("GHC.IO.Exception"),_q=unCStr("IOException"),_r=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_o,_p,_q],_s=[0],_t=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_r,_s],_u=function(_v){return E(_t);},_w=function(_x){return E(E(_x)[1]);},_y=unCStr("Maybe.fromJust: Nothing"),_z=new T(function(){return err(_y);}),_A=function(_B,_C,_D){var _E=new T(function(){var _F=A(_B,[_D]),_G=A(_C,[new T(function(){var _H=E(_E);return _H[0]==0?E(_z):E(_H[1]);})]),_I=hs_eqWord64(_F[1],_G[1]);if(!E(_I)){return [0];}else{var _J=hs_eqWord64(_F[2],_G[2]);return E(_J)==0?[0]:[1,_D];}});return E(_E);},_K=function(_L){var _M=E(_L);return _A(_w(_M[1]),_u,_M[2]);},_N=unCStr(": "),_O=[0,41],_P=unCStr(" ("),_Q=function(_R,_S){var _T=E(_R);return _T[0]==0?E(_S):[1,_T[1],new T(function(){return _Q(_T[2],_S);})];},_U=unCStr("already exists"),_V=unCStr("does not exist"),_W=unCStr("protocol error"),_X=unCStr("failed"),_Y=unCStr("invalid argument"),_Z=unCStr("inappropriate type"),_10=unCStr("hardware fault"),_11=unCStr("unsupported operation"),_12=unCStr("timeout"),_13=unCStr("resource vanished"),_14=unCStr("interrupted"),_15=unCStr("resource busy"),_16=unCStr("resource exhausted"),_17=unCStr("end of file"),_18=unCStr("illegal operation"),_19=unCStr("permission denied"),_1a=unCStr("user error"),_1b=unCStr("unsatisified constraints"),_1c=unCStr("system error"),_1d=function(_1e,_1f){switch(E(_1e)){case 0:return _Q(_U,_1f);case 1:return _Q(_V,_1f);case 2:return _Q(_15,_1f);case 3:return _Q(_16,_1f);case 4:return _Q(_17,_1f);case 5:return _Q(_18,_1f);case 6:return _Q(_19,_1f);case 7:return _Q(_1a,_1f);case 8:return _Q(_1b,_1f);case 9:return _Q(_1c,_1f);case 10:return _Q(_W,_1f);case 11:return _Q(_X,_1f);case 12:return _Q(_Y,_1f);case 13:return _Q(_Z,_1f);case 14:return _Q(_10,_1f);case 15:return _Q(_11,_1f);case 16:return _Q(_12,_1f);case 17:return _Q(_13,_1f);default:return _Q(_14,_1f);}},_1g=[0,125],_1h=unCStr("{handle: "),_1i=function(_1j,_1k,_1l,_1m,_1n,_1o){var _1p=new T(function(){var _1q=new T(function(){return _1d(_1k,new T(function(){var _1r=E(_1m);return _1r[0]==0?E(_1o):_Q(_P,new T(function(){return _Q(_1r,[1,_O,_1o]);}));}));}),_1s=E(_1l);return _1s[0]==0?E(_1q):_Q(_1s,new T(function(){return _Q(_N,_1q);}));}),_1t=E(_1n);if(!_1t[0]){var _1u=E(_1j);if(!_1u[0]){return E(_1p);}else{var _1v=E(_1u[1]);return _1v[0]==0?_Q(_1h,new T(function(){return _Q(_1v[1],[1,_1g,new T(function(){return _Q(_N,_1p);})]);})):_Q(_1h,new T(function(){return _Q(_1v[1],[1,_1g,new T(function(){return _Q(_N,_1p);})]);}));}}else{return _Q(_1t[1],new T(function(){return _Q(_N,_1p);}));}},_1w=function(_1x){var _1y=E(_1x);return _1i(_1y[1],_1y[2],_1y[3],_1y[4],_1y[6],_s);},_1z=function(_1A,_1B){var _1C=E(_1A);return _1i(_1C[1],_1C[2],_1C[3],_1C[4],_1C[6],_1B);},_1D=[0,44],_1E=[0,93],_1F=[0,91],_1G=function(_1H,_1I,_1J){var _1K=E(_1I);return _1K[0]==0?unAppCStr("[]",_1J):[1,_1F,new T(function(){return A(_1H,[_1K[1],new T(function(){var _1L=function(_1M){var _1N=E(_1M);return _1N[0]==0?E([1,_1E,_1J]):[1,_1D,new T(function(){return A(_1H,[_1N[1],new T(function(){return _1L(_1N[2]);})]);})];};return _1L(_1K[2]);})]);})];},_1O=function(_1P,_1Q){return _1G(_1z,_1P,_1Q);},_1R=function(_1S,_1T,_1U){var _1V=E(_1T);return _1i(_1V[1],_1V[2],_1V[3],_1V[4],_1V[6],_1U);},_1W=[0,_1R,_1w,_1O],_1X=new T(function(){return [0,_u,_1W,_1Y,_K];}),_1Y=function(_1Z){return [0,_1X,_1Z];},_20=7,_21=function(_22){return [0,_b,_20,_s,_22,_b,_b];},_23=function(_24,_){return die(new T(function(){return _1Y(new T(function(){return _21(_24);}));}));},_25=function(_26,_){return _23(_26,_);},_27=function(_28,_){return _28;},_29=[0,_k,_h,_27,_25],_2a=function(_2b){return E(E(_2b)[1]);},_2c=function(_2d,_2e,_2f,_2g){return A(_2a,[_2d,new T(function(){return A(_2e,[_2g]);}),function(_2h){return A(_2f,[new T(function(){return E(E(_2h)[1]);}),new T(function(){return E(E(_2h)[2]);})]);}]);},_2i=function(_2j,_2k,_2l,_2m){return A(_2a,[_2j,new T(function(){return A(_2k,[_2m]);}),function(_2n){return A(_2l,[new T(function(){return E(E(_2n)[2]);})]);}]);},_2o=function(_2p,_2q,_2r,_2s){return _2i(_2p,_2q,_2r,_2s);},_2t=function(_2u){return E(E(_2u)[4]);},_2v=function(_2w,_2x){var _2y=new T(function(){return A(_2t,[_2w,_2x]);});return function(_2z){return E(_2y);};},_2A=function(_2B){return E(E(_2B)[3]);},_2C=function(_2D){var _2E=new T(function(){return _2A(_2D);});return [0,function(_2q,_2r,_2s){return _2c(_2D,_2q,_2r,_2s);},function(_2q,_2r,_2s){return _2o(_2D,_2q,_2r,_2s);},function(_2F,_2G){return A(_2E,[[0,_2F,_2G]]);},function(_2s){return _2v(_2D,_2s);}];},_2H=new T(function(){return _2C(_29);}),_2I=function(_2J,_2K){var _2L=jsShowI(_2J);return _Q(fromJSStr(_2L),_2K);},_2M=[0,41],_2N=[0,40],_2O=function(_2P,_2Q,_2R){return _2Q>=0?_2I(_2Q,_2R):_2P<=6?_2I(_2Q,_2R):[1,_2N,new T(function(){var _2S=jsShowI(_2Q);return _Q(fromJSStr(_2S),[1,_2M,_2R]);})];},_2T=[0,112],_2U=function(_2V,_2W,_2X,_2Y){var _2Z=E(_2W);return A(_2Z[1],[new T(function(){var _30=E(_2V);return E(_2X);}),function(_31){var _32=new T(function(){return E(E(_31)[2]);});return A(_2Z[2],[new T(function(){return A(_2Y,[new T(function(){var _33=E(new T(function(){var _34=E(_2V);return [0,coercionToken];})),_35=E(_31);return [0,_35[1],new T(function(){return [0,E(_32)[1]+1|0];}),_35[3],_35[4]];})]);}),new T(function(){return A(_2Z[3],[[1,_2T,new T(function(){return _Q(_2O(0,E(_32)[1],_s),new T(function(){return E(E(_31)[1]);}));})]]);})]);}]);},_36=[0,coercionToken],_37=function(_38,_){return [0,_38,_38];},_39=function(_3a,_3b,_){return [0,_0,_3a];},_3c=new T(function(){return _2U(_36,_2H,_37,_39);}),_3d=2,_3e=[0,0],_3f=function(_){var _=0,_3g=nMV(_3e);return [0,_3g];},_3h=function(_3i){var _3j=A(_3i,[_]);return E(_3j);},_3k=new T(function(){return _3h(_3f);}),_3l=function(_){return _0;},_3m=[1,_0],_3n=[0,_27,_3m],_3o=function(_3p,_3q,_){return [0,[0,_27,[1,new T(function(){return E(E(_3p)[2]);})]],_3q];},_3r=function(_3s,_){return [0,[0,_27,[1,_3s]],_3s];},_3t=function(_3u,_){return _3v(_3r,_3o,_3u,_);},_3w=unCStr(" could be found!"),_3x=function(_3y){return err(unAppCStr("No element with ID ",new T(function(){return _Q(_3y,_3w);})));},_3z=function(_3A,_3B,_){var _3C=E(_3k)[1],_3D=rMV(_3C),_3E=E(_3B),_3F=jsFind(toJSStr(_3E)),_3G=E(_3F);if(!_3G[0]){return _3x(_3E);}else{var _3H=E(_3G[1]),_3I=jsClearChildren(_3H[1]),_3J=A(_3A,[[0,_s,_3D,_3d,_3l],_]),_3K=E(_3J),_3L=function(_3M,_3N,_){return _3O(function(_3P,_){var _=wMV(_3C,_3M);return [0,_3n,_3P];},_3N,_,coercionToken);},_3O=function(_3Q,_3R,_,_3S){var _3T=A(_3c,[_3R,_]),_3U=new T(function(){return E(E(_3T)[1]);}),_3V=A(_3Q,[new T(function(){var _3W=E(E(_3T)[2]);return [0,_3W[1],_3W[2],_3W[3],function(_){var _3X=rMV(_3C),_3Y=E(_3U),_3Z=jsFind(toJSStr(_3Y)),_40=E(_3Z);if(!_40[0]){return _3x(_3Y);}else{var _41=E(_40[1]),_42=jsClearChildren(_41[1]),_43=_3O(function(_44,_){var _45=A(_3Q,[_3W,_]);return [0,[0,_27,E(E(_45)[1])[2]],_44];},[0,_s,_3X,_3d,_3l],_,coercionToken),_46=E(_43),_47=_48(_3t,_3L,_46[2],_),_49=A(E(_46[1])[1],[_41,_]),_4a=A(E(E(_47)[1])[1],[_41,_]);return _0;}}];}),_]),_4b=E(_3V),_4c=_4b[2],_4d=E(_4b[1]),_4e=_4d[1];return E(_4d[2])[0]==0?[0,[0,function(_4f,_){var _4g=A(_4e,[_4f,_]),_4h=E(_3U),_4i=jsFind(toJSStr(_4h));if(!E(_4i)[0]){var _4j=jsCreateElem(toJSStr(E(_c))),_4k=A(_1,[_7,[0,_4j],_a,_4h,_]),_4l=E(_4f),_4m=jsAppendChild(_4j,_4l[1]);return _4l;}else{return _4f;}},_b],_4c]:[0,[0,function(_4n,_){var _4o=A(_4e,[_4n,_]),_4p=E(_3U),_4q=jsFind(toJSStr(_4p));if(!E(_4q)[0]){var _4r=jsCreateElem(toJSStr(E(_c))),_4s=A(_1,[_7,[0,_4r],_a,_4p,_]),_4t=E(_4n),_4u=jsAppendChild(_4r,_4t[1]);return _4t;}else{return _4n;}},_3m],_4c];},_48=function(_4v,_4w,_4x,_){var _4y=A(_3c,[_4x,_]),_4z=new T(function(){return E(E(_4y)[1]);}),_4A=A(_4v,[new T(function(){var _4B=E(E(_4y)[2]);return [0,_4B[1],_4B[2],_4B[3],function(_){var _4C=rMV(_3C),_4D=E(_4z),_4E=jsFind(toJSStr(_4D)),_4F=E(_4E);if(!_4F[0]){return _3x(_4D);}else{var _4G=E(_4F[1]),_4H=jsClearChildren(_4G[1]),_4I=_48(function(_4J,_){var _4K=A(_4v,[_4B,_]);return [0,[0,_27,E(E(_4K)[1])[2]],_4J];},_4w,[0,_s,_4C,_3d,_3l],_),_4L=E(_4I),_4M=_48(_3t,_3L,_4L[2],_),_4N=A(E(_4L[1])[1],[_4G,_]),_4O=A(E(E(_4M)[1])[1],[_4G,_]);return _0;}}];}),_]),_4P=E(_4A),_4Q=_4P[2],_4R=E(_4P[1]),_4S=_4R[1],_4T=E(_4R[2]);if(!_4T[0]){return [0,[0,function(_4U,_){var _4V=A(_4S,[_4U,_]),_4W=E(_4z),_4X=jsFind(toJSStr(_4W));if(!E(_4X)[0]){var _4Y=jsCreateElem(toJSStr(E(_c))),_4Z=A(_1,[_7,[0,_4Y],_a,_4W,_]),_50=E(_4U),_51=jsAppendChild(_4Y,_50[1]);return _50;}else{return _4U;}},_b],_4Q];}else{var _52=A(_4w,[_4T[1],_4Q,_]),_53=E(_52),_54=E(_53[1]),_55=_54[1];return [0,[0,function(_56,_){var _57=A(_4S,[_56,_]),_58=E(_4z),_59=jsFind(toJSStr(_58));if(!E(_59)[0]){var _5a=jsCreateElem(toJSStr(E(_c))),_5b=A(_1,[_7,[0,_5a],_a,_58,_]),_5c=E(_56),_5d=jsAppendChild(_5a,_5c[1]),_5e=A(_55,[_5c,_]);return _5c;}else{var _5f=A(_55,[_56,_]);return _56;}},_54[2]],_53[2]];}},_5g=function(_5h,_5i,_){return _5j(function(_5k,_){var _=wMV(_3C,_5h);return [0,_3n,_5k];},_5i,_,coercionToken);},_5j=function(_5l,_5m,_,_5n){var _5o=A(_3c,[_5m,_]),_5p=new T(function(){return E(E(_5o)[1]);}),_5q=A(_5l,[new T(function(){var _5r=E(E(_5o)[2]);return [0,_5r[1],_5r[2],_5r[3],function(_){var _5s=rMV(_3C),_5t=E(_5p),_5u=jsFind(toJSStr(_5t)),_5v=E(_5u);if(!_5v[0]){return _3x(_5t);}else{var _5w=E(_5v[1]),_5x=jsClearChildren(_5w[1]),_5y=_5j(function(_5z,_){var _5A=A(_5l,[_5r,_]);return [0,[0,_27,E(E(_5A)[1])[2]],_5z];},[0,_s,_5s,_3d,_3l],_,coercionToken),_5B=E(_5y),_5C=_5D(_3t,_5g,_5B[2],_),_5E=A(E(_5B[1])[1],[_5w,_]),_5F=A(E(E(_5C)[1])[1],[_5w,_]);return _0;}}];}),_]),_5G=E(_5q),_5H=_5G[2],_5I=E(_5G[1]),_5J=_5I[1];return E(_5I[2])[0]==0?[0,[0,function(_5K,_){var _5L=A(_5J,[_5K,_]),_5M=E(_5p),_5N=jsFind(toJSStr(_5M));if(!E(_5N)[0]){var _5O=jsCreateElem(toJSStr(E(_c))),_5P=A(_1,[_7,[0,_5O],_a,_5M,_]),_5Q=E(_5K),_5R=jsAppendChild(_5O,_5Q[1]);return _5Q;}else{return _5K;}},_b],_5H]:[0,[0,function(_5S,_){var _5T=A(_5J,[_5S,_]),_5U=E(_5p),_5V=jsFind(toJSStr(_5U));if(!E(_5V)[0]){var _5W=jsCreateElem(toJSStr(E(_c))),_5X=A(_1,[_7,[0,_5W],_a,_5U,_]),_5Y=E(_5S),_5Z=jsAppendChild(_5W,_5Y[1]);return _5Y;}else{return _5S;}},_3m],_5H];},_5D=function(_60,_61,_62,_){var _63=A(_3c,[_62,_]),_64=new T(function(){return E(E(_63)[1]);}),_65=A(_60,[new T(function(){var _66=E(E(_63)[2]);return [0,_66[1],_66[2],_66[3],function(_){var _67=rMV(_3C),_68=E(_64),_69=jsFind(toJSStr(_68)),_6a=E(_69);if(!_6a[0]){return _3x(_68);}else{var _6b=E(_6a[1]),_6c=jsClearChildren(_6b[1]),_6d=_5D(function(_6e,_){var _6f=A(_60,[_66,_]);return [0,[0,_27,E(E(_6f)[1])[2]],_6e];},_61,[0,_s,_67,_3d,_3l],_),_6g=E(_6d),_6h=_5D(_3t,_5g,_6g[2],_),_6i=A(E(_6g[1])[1],[_6b,_]),_6j=A(E(E(_6h)[1])[1],[_6b,_]);return _0;}}];}),_]),_6k=E(_65),_6l=_6k[2],_6m=E(_6k[1]),_6n=_6m[1],_6o=E(_6m[2]);if(!_6o[0]){return [0,[0,function(_6p,_){var _6q=A(_6n,[_6p,_]),_6r=E(_64),_6s=jsFind(toJSStr(_6r));if(!E(_6s)[0]){var _6t=jsCreateElem(toJSStr(E(_c))),_6u=A(_1,[_7,[0,_6t],_a,_6r,_]),_6v=E(_6p),_6w=jsAppendChild(_6t,_6v[1]);return _6v;}else{return _6p;}},_b],_6l];}else{var _6x=A(_61,[_6o[1],_6l,_]),_6y=E(_6x),_6z=E(_6y[1]),_6A=_6z[1];return [0,[0,function(_6B,_){var _6C=A(_6n,[_6B,_]),_6D=E(_64),_6E=jsFind(toJSStr(_6D));if(!E(_6E)[0]){var _6F=jsCreateElem(toJSStr(E(_c))),_6G=A(_1,[_7,[0,_6F],_a,_6D,_]),_6H=E(_6B),_6I=jsAppendChild(_6F,_6H[1]),_6J=A(_6A,[_6H,_]);return _6H;}else{var _6K=A(_6A,[_6B,_]);return _6B;}},_6z[2]],_6y[2]];}},_6L=_48(_3t,_5g,_3K[2],_),_6M=A(E(_3K[1])[1],[_3H,_]),_6N=A(E(E(_6L)[1])[1],[_3H,_]);return _0;}},_3v=function(_6O,_6P,_6Q,_){var _6R=A(_3c,[_6Q,_]),_6S=new T(function(){return E(E(_6R)[1]);}),_6T=A(_6O,[new T(function(){var _6U=E(E(_6R)[2]);return [0,_6U[1],_6U[2],_6U[3],function(_){return _3z(function(_3u,_){return _3v(function(_6V,_){var _6W=A(_6O,[_6U,_]);return [0,[0,_27,E(E(_6W)[1])[2]],_6V];},_6P,_3u,_);},_6S,_);}];}),_]),_6X=E(_6T),_6Y=_6X[2],_6Z=E(_6X[1]),_70=_6Z[1],_71=E(_6Z[2]);if(!_71[0]){return [0,[0,function(_72,_){var _73=A(_70,[_72,_]),_74=E(_6S),_75=jsFind(toJSStr(_74));if(!E(_75)[0]){var _76=jsCreateElem(toJSStr(E(_c))),_77=A(_1,[_7,[0,_76],_a,_74,_]),_78=E(_72),_79=jsAppendChild(_76,_78[1]);return _78;}else{return _72;}},_b],_6Y];}else{var _7a=A(_6P,[_71[1],_6Y,_]),_7b=E(_7a),_7c=E(_7b[1]),_7d=_7c[1];return [0,[0,function(_7e,_){var _7f=A(_70,[_7e,_]),_7g=E(_6S),_7h=jsFind(toJSStr(_7g));if(!E(_7h)[0]){var _7i=jsCreateElem(toJSStr(E(_c))),_7j=A(_1,[_7,[0,_7i],_a,_7g,_]),_7k=E(_7e),_7l=jsAppendChild(_7i,_7k[1]),_7m=A(_7d,[_7k,_]);return _7k;}else{var _7n=A(_7d,[_7e,_]);return _7e;}},_7c[2]],_7b[2]];}},_7o=function(_7p){return _2O(0,E(_7p)[1],_s);},_7q=function(_7r,_7s,_){var _7t=jsCreateTextNode(toJSStr(E(_7r))),_7u=jsAppendChild(_7t,E(_7s)[1]);return [0,_7t];},_7v=[13,coercionToken],_7w=unCStr("true"),_7x=unCStr("hasevent"),_7y=function(_7z,_7A){while(1){var _7B=E(_7z);if(!_7B[0]){return E(_7A)[0]==0?true:false;}else{var _7C=E(_7A);if(!_7C[0]){return false;}else{if(E(_7B[1])[1]!=E(_7C[1])[1]){return false;}else{_7z=_7B[2];_7A=_7C[2];continue;}}}}},_7D=new T(function(){return [0,"keydown"];}),_7E=new T(function(){return [0,"mousemove"];}),_7F=new T(function(){return [0,"blur"];}),_7G=new T(function(){return [0,"focus"];}),_7H=new T(function(){return [0,"change"];}),_7I=new T(function(){return [0,"unload"];}),_7J=new T(function(){return [0,"load"];}),_7K=new T(function(){return [0,"keyup"];}),_7L=new T(function(){return [0,"keypress"];}),_7M=new T(function(){return [0,"mouseup"];}),_7N=new T(function(){return [0,"mousedown"];}),_7O=new T(function(){return [0,"dblclick"];}),_7P=new T(function(){return [0,"click"];}),_7Q=new T(function(){return [0,"mouseout"];}),_7R=new T(function(){return [0,"mouseover"];}),_7S=function(_7T){switch(E(_7T)[0]){case 0:return E(_7J);case 1:return E(_7I);case 2:return E(_7H);case 3:return E(_7G);case 4:return E(_7F);case 5:return E(_7E);case 6:return E(_7R);case 7:return E(_7Q);case 8:return E(_7P);case 9:return E(_7O);case 10:return E(_7N);case 11:return E(_7M);case 12:return E(_7L);case 13:return E(_7K);default:return E(_7D);}},_7U=function(_7V,_7W,_7X,_7Y,_){var _7Z=A(_7V,[_7Y,_]),_80=E(_7Z),_81=_80[1],_82=E(_7x),_83=jsGetAttr(_81,toJSStr(_82));if(!_7y(fromJSStr(_83),_7w)){var _84=E(_7X),_85=jsSetCB(_81,_7S(_7W)[1],_7X),_86=A(_1,[_7,_80,_82,_7w,_]);return _80;}else{return _80;}},_87=unCStr("text"),_88=function(_89,_8a,_){var _8b=jsCreateElem(toJSStr(E(_89))),_8c=jsAppendChild(_8b,E(_8a)[1]);return [0,_8b];},_8d=function(_8e,_8f,_8g,_){var _8h=_88(_8e,_8g,_),_8i=A(_8f,[_8h,_]);return _8h;},_8j=unCStr("()"),_8k=unCStr("GHC.Tuple"),_8l=unCStr("ghc-prim"),_8m=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_8l,_8k,_8j],_8n=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_8m,_s],_8o=function(_8p){return E(_8n);},_8q=unCStr("main"),_8r=unCStr("Builder"),_8s=unCStr("JSBuilderM"),_8t=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_8q,_8r,_8s],_8u=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_8t,_s],_8v=function(_8w){return E(_8u);},_8x=function(_8y){var _8z=E(_8y);return _8z[0]==0?[0]:_Q(_8z[1],new T(function(){return _8x(_8z[2]);}));},_8A=function(_8B,_8C){var _8D=E(_8B);if(!_8D){return [0,_s,_8C];}else{var _8E=E(_8C);if(!_8E[0]){return [0,_s,_s];}else{var _8F=new T(function(){var _8G=_8A(_8D-1|0,_8E[2]);return [0,_8G[1],_8G[2]];});return [0,[1,_8E[1],new T(function(){return E(E(_8F)[1]);})],new T(function(){return E(E(_8F)[2]);})];}}},_8H=[0,120],_8I=[0,48],_8J=function(_8K){var _8L=new T(function(){var _8M=_8A(8,new T(function(){var _8N=md5(toJSStr(E(_8K)));return fromJSStr(_8N);}));return [0,_8M[1],_8M[2]];}),_8O=parseInt([0,toJSStr([1,_8I,[1,_8H,new T(function(){return E(E(_8L)[1]);})]])]),_8P=new T(function(){var _8Q=_8A(8,new T(function(){return E(E(_8L)[2]);}));return [0,_8Q[1],_8Q[2]];}),_8R=parseInt([0,toJSStr([1,_8I,[1,_8H,new T(function(){return E(E(_8P)[1]);})]])]),_8S=hs_mkWord64(_8O,_8R),_8T=parseInt([0,toJSStr([1,_8I,[1,_8H,new T(function(){return E(_8A(8,new T(function(){return E(E(_8P)[2]);}))[1]);})]])]),_8U=hs_mkWord64(_8T,_8T);return [0,_8S,_8U];},_8V=function(_8W,_8X){var _8Y=E(_8X);return _8Y[0]==0?[0]:[1,new T(function(){return A(_8W,[_8Y[1]]);}),new T(function(){return _8V(_8W,_8Y[2]);})];},_8Z=function(_90,_91){var _92=jsShowI(_90),_93=md5(_92);return _Q(fromJSStr(_93),new T(function(){var _94=jsShowI(_91),_95=md5(_94);return fromJSStr(_95);}));},_96=function(_97){var _98=E(_97);return _8Z(_98[1],_98[2]);},_99=function(_9a){var _9b=E(_9a);if(!_9b[0]){return [0];}else{var _9c=E(_9b[1]);return [1,[0,_9c[1],_9c[2]],new T(function(){return _99(_9b[2]);})];}},_9d=unCStr("Prelude.undefined"),_9e=new T(function(){return err(_9d);}),_9f=function(_9g,_9h){return function(_9i){return E(new T(function(){var _9j=A(_9g,[_9e]),_9k=E(_9j[3]),_9l=_9k[1],_9m=_9k[2],_9n=_Q(_9j[4],[1,new T(function(){return A(_9h,[_9e]);}),_s]);if(!_9n[0]){return [0,_9l,_9m,_9k,_s];}else{var _9o=_8J(new T(function(){return _8x(_8V(_96,[1,[0,_9l,_9m],new T(function(){return _99(_9n);})]));}));return [0,_9o[1],_9o[2],_9k,_9n];}}));};},_9p=new T(function(){return _9f(_8v,_8o);}),_9q=function(_9r,_9s,_9t,_){var _9u=E(_9s),_9v=A(_9r,[_9t,_]),_9w=A(_1,[_7,_9v,_9u[1],_9u[2],_]);return _9v;},_9x=function(_9y,_9z){while(1){var _9A=(function(_9B,_9C){var _9D=E(_9C);if(!_9D[0]){return E(_9B);}else{_9y=function(_3u,_){return _9q(_9B,_9D[1],_3u,_);};_9z=_9D[2];return null;}})(_9y,_9z);if(_9A!=null){return _9A;}}},_9E=unCStr("value"),_9F=unCStr("onclick"),_9G=unCStr("checked"),_9H=[0,_9G,_s],_9I=[1,_9H,_s],_9J=unCStr("type"),_9K=unCStr("input"),_9L=function(_9M,_){return _88(_9K,_9M,_);},_9N=function(_9O,_9P,_9Q,_9R,_9S){var _9T=new T(function(){var _9U=new T(function(){return _9x(_9L,[1,[0,_9J,_9P],[1,[0,_a,_9O],[1,[0,_9E,_9Q],_s]]]);});return !E(_9R)?E(_9U):_9x(_9U,_9I);}),_9V=E(_9S);return _9V[0]==0?E(_9T):_9x(_9T,[1,[0,_9F,_9V[1]],_s]);},_9W=unCStr("href"),_9X=[0,97],_9Y=[1,_9X,_s],_9Z=function(_a0,_){return _88(_9Y,_a0,_);},_a1=function(_a2,_a3){var _a4=new T(function(){return _9x(_9Z,[1,[0,_9W,_a2],_s]);});return function(_a5,_){var _a6=A(_a4,[_a5,_]),_a7=A(_a3,[_a6,_]);return _a6;};},_a8=function(_a9){return _a1(_a9,function(_3u,_){return _7q(_a9,_3u,_);});},_aa=unCStr("option"),_ab=function(_ac,_){return _88(_aa,_ac,_);},_ad=unCStr("selected"),_ae=[0,_ad,_s],_af=[1,_ae,_s],_ag=function(_ah,_ai,_aj){var _ak=new T(function(){return _9x(_ab,[1,[0,_9E,_ah],_s]);}),_al=function(_am,_){var _an=A(_ak,[_am,_]),_ao=A(_ai,[_an,_]);return _an;};return !E(_aj)?E(_al):_9x(_al,_af);},_ap=function(_aq,_ar){return _ag(_aq,function(_3u,_){return _7q(_aq,_3u,_);},_ar);},_as=unCStr("method"),_at=unCStr("action"),_au=unCStr("UTF-8"),_av=unCStr("acceptCharset"),_aw=[0,_av,_au],_ax=unCStr("form"),_ay=function(_az,_){return _88(_ax,_az,_);},_aA=function(_aB,_aC,_aD){var _aE=new T(function(){return _9x(_ay,[1,_aw,[1,[0,_at,_aB],[1,[0,_as,_aC],_s]]]);});return function(_aF,_){var _aG=A(_aE,[_aF,_]),_aH=A(_aD,[_aG,_]);return _aG;};},_aI=unCStr("select"),_aJ=function(_aK,_){return _88(_aI,_aK,_);},_aL=function(_aM,_aN){var _aO=new T(function(){return _9x(_aJ,[1,[0,_a,_aM],_s]);});return function(_aP,_){var _aQ=A(_aO,[_aP,_]),_aR=A(_aN,[_aQ,_]);return _aQ;};},_aS=unCStr("textarea"),_aT=function(_aU,_){return _88(_aS,_aU,_);},_aV=function(_aW,_aX){var _aY=new T(function(){return _9x(_aT,[1,[0,_a,_aW],_s]);});return function(_aZ,_){var _b0=A(_aY,[_aZ,_]),_b1=_7q(_aX,_b0,_);return _b0;};},_b2=unCStr("color:red"),_b3=unCStr("style"),_b4=[0,_b3,_b2],_b5=[1,_b4,_s],_b6=[0,98],_b7=[1,_b6,_s],_b8=function(_b9){return _9x(function(_ba,_){var _bb=_88(_b7,_ba,_),_bc=A(_b9,[_bb,_]);return _bb;},_b5);},_bd=unCStr("toByteString not defined"),_be=new T(function(){return err(_bd);}),_bf=function(_bg,_bh,_){var _bi=E(_bg);if(!_bi[0]){return _bh;}else{var _bj=A(_bi[1],[_bh,_]),_bk=_bf(_bi[2],_bh,_);return _bh;}},_bl=function(_bm,_bn,_bo,_){var _bp=A(_bm,[_bo,_]),_bq=A(_bn,[_bo,_]);return _bo;},_br=[0,_27,_bl,_bf],_bs=[0,_br,_9p,_be,_7q,_7q,_8d,_b8,_a1,_a8,_9N,_aV,_aL,_ag,_ap,_aA,_9x],_bt=[0,_29,_7],_bu=unCStr("base"),_bv=unCStr("Control.Exception.Base"),_bw=unCStr("PatternMatchFail"),_bx=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_bu,_bv,_bw],_by=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_bx,_s],_bz=function(_bA){return E(_by);},_bB=function(_bC){var _bD=E(_bC);return _A(_w(_bD[1]),_bz,_bD[2]);},_bE=function(_bF){return E(E(_bF)[1]);},_bG=function(_bH,_bI){return _Q(E(_bH)[1],_bI);},_bJ=function(_bK,_bL){return _1G(_bG,_bK,_bL);},_bM=function(_bN,_bO,_bP){return _Q(E(_bO)[1],_bP);},_bQ=[0,_bM,_bE,_bJ],_bR=new T(function(){return [0,_bz,_bQ,_bS,_bB];}),_bS=function(_bT){return [0,_bR,_bT];},_bU=unCStr("Non-exhaustive patterns in"),_bV=function(_bW,_bX){return die(new T(function(){return A(_bX,[_bW]);}));},_bY=function(_bZ,_c0){var _c1=E(_c0);if(!_c1[0]){return [0,_s,_s];}else{var _c2=_c1[1];if(!A(_bZ,[_c2])){return [0,_s,_c1];}else{var _c3=new T(function(){var _c4=_bY(_bZ,_c1[2]);return [0,_c4[1],_c4[2]];});return [0,[1,_c2,new T(function(){return E(E(_c3)[1]);})],new T(function(){return E(E(_c3)[2]);})];}}},_c5=[0,32],_c6=[0,10],_c7=[1,_c6,_s],_c8=function(_c9){return E(E(_c9)[1])==124?false:true;},_ca=function(_cb,_cc){var _cd=_bY(_c8,unCStr(_cb)),_ce=_cd[1],_cf=function(_cg,_ch){return _Q(_cg,new T(function(){return unAppCStr(": ",new T(function(){return _Q(_cc,new T(function(){return _Q(_ch,_c7);}));}));}));},_ci=E(_cd[2]);return _ci[0]==0?_cf(_ce,_s):E(E(_ci[1])[1])==124?_cf(_ce,[1,_c5,_ci[2]]):_cf(_ce,_s);},_cj=function(_ck){return _bV([0,new T(function(){return _ca(_ck,_bU);})],_bS);},_cl=new T(function(){return _cj("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_cm=function(_cn,_co){while(1){var _cp=(function(_cq,_cr){var _cs=E(_cq);switch(_cs[0]){case 0:var _ct=E(_cr);if(!_ct[0]){return [0];}else{_cn=A(_cs[1],[_ct[1]]);_co=_ct[2];return null;}break;case 1:var _cu=A(_cs[1],[_cr]),_cv=_cr;_cn=_cu;_co=_cv;return null;case 2:return [0];case 3:return [1,[0,_cs[1],_cr],new T(function(){return _cm(_cs[2],_cr);})];default:return E(_cs[1]);}})(_cn,_co);if(_cp!=null){return _cp;}}},_cw=function(_cx,_cy){var _cz=new T(function(){var _cA=E(_cy);if(_cA[0]==3){return [3,_cA[1],new T(function(){return _cw(_cx,_cA[2]);})];}else{var _cB=E(_cx);if(_cB[0]==2){return E(_cA);}else{var _cC=E(_cA);if(_cC[0]==2){return E(_cB);}else{var _cD=new T(function(){var _cE=E(_cC);if(_cE[0]==4){return [1,function(_cF){return [4,new T(function(){return _Q(_cm(_cB,_cF),_cE[1]);})];}];}else{var _cG=E(_cB);if(_cG[0]==1){var _cH=_cG[1],_cI=E(_cE);return _cI[0]==0?[1,function(_cJ){return _cw(A(_cH,[_cJ]),_cI);}]:[1,function(_cK){return _cw(A(_cH,[_cK]),new T(function(){return A(_cI[1],[_cK]);}));}];}else{var _cL=E(_cE);return _cL[0]==0?E(_cl):[1,function(_cM){return _cw(_cG,new T(function(){return A(_cL[1],[_cM]);}));}];}}}),_cN=E(_cB);switch(_cN[0]){case 1:var _cO=E(_cC);return _cO[0]==4?[1,function(_cP){return [4,new T(function(){return _Q(_cm(A(_cN[1],[_cP]),_cP),_cO[1]);})];}]:E(_cD);case 4:var _cQ=_cN[1],_cR=E(_cC);switch(_cR[0]){case 0:return [1,function(_cS){return [4,new T(function(){return _Q(_cQ,new T(function(){return _cm(_cR,_cS);}));})];}];case 1:return [1,function(_cT){return [4,new T(function(){return _Q(_cQ,new T(function(){return _cm(A(_cR[1],[_cT]),_cT);}));})];}];default:return [4,new T(function(){return _Q(_cQ,_cR[1]);})];}break;default:return E(_cD);}}}}}),_cU=E(_cx);switch(_cU[0]){case 0:var _cV=E(_cy);return _cV[0]==0?[0,function(_cW){return _cw(A(_cU[1],[_cW]),new T(function(){return A(_cV[1],[_cW]);}));}]:E(_cz);case 3:return [3,_cU[1],new T(function(){return _cw(_cU[2],_cy);})];default:return E(_cz);}},_cX=function(_cY,_cZ){return E(_cY)[1]!=E(_cZ)[1];},_d0=function(_d1,_d2){return E(_d1)[1]==E(_d2)[1];},_d3=[0,_d0,_cX],_d4=function(_d5){return E(E(_d5)[1]);},_d6=function(_d7,_d8,_d9){while(1){var _da=E(_d8);if(!_da[0]){return E(_d9)[0]==0?true:false;}else{var _db=E(_d9);if(!_db[0]){return false;}else{if(!A(_d4,[_d7,_da[1],_db[1]])){return false;}else{_d8=_da[2];_d9=_db[2];continue;}}}}},_dc=function(_dd,_de,_df){return !_d6(_dd,_de,_df)?true:false;},_dg=function(_dh){return [0,function(_di,_dj){return _d6(_dh,_di,_dj);},function(_di,_dj){return _dc(_dh,_di,_dj);}];},_dk=new T(function(){return _dg(_d3);}),_dl=function(_dm,_dn){var _do=E(_dm);switch(_do[0]){case 0:return [0,function(_dp){return _dl(A(_do[1],[_dp]),_dn);}];case 1:return [1,function(_dq){return _dl(A(_do[1],[_dq]),_dn);}];case 2:return [2];case 3:return _cw(A(_dn,[_do[1]]),new T(function(){return _dl(_do[2],_dn);}));default:var _dr=function(_ds){var _dt=E(_ds);if(!_dt[0]){return [0];}else{var _du=E(_dt[1]);return _Q(_cm(A(_dn,[_du[1]]),_du[2]),new T(function(){return _dr(_dt[2]);}));}},_dv=_dr(_do[1]);return _dv[0]==0?[2]:[4,_dv];}},_dw=[2],_dx=function(_dy){return [3,_dy,_dw];},_dz=function(_dA,_dB){var _dC=E(_dA);if(!_dC){return A(_dB,[_0]);}else{var _dD=new T(function(){return _dz(_dC-1|0,_dB);});return [0,function(_dE){return E(_dD);}];}},_dF=function(_dG,_dH,_dI){var _dJ=new T(function(){return A(_dG,[_dx]);});return [1,function(_dK){return A(function(_dL,_dM,_dN){while(1){var _dO=(function(_dP,_dQ,_dR){var _dS=E(_dP);switch(_dS[0]){case 0:var _dT=E(_dQ);if(!_dT[0]){return E(_dH);}else{_dL=A(_dS[1],[_dT[1]]);_dM=_dT[2];var _dU=_dR+1|0;_dN=_dU;return null;}break;case 1:var _dV=A(_dS[1],[_dQ]),_dW=_dQ,_dU=_dR;_dL=_dV;_dM=_dW;_dN=_dU;return null;case 2:return E(_dH);case 3:return function(_dX){var _dY=new T(function(){return _dl(_dS,_dX);});return _dz(_dR,function(_dZ){return E(_dY);});};default:return function(_e0){return _dl(_dS,_e0);};}})(_dL,_dM,_dN);if(_dO!=null){return _dO;}}},[_dJ,_dK,0,_dI]);}];},_e1=[6],_e2=unCStr("valDig: Bad base"),_e3=new T(function(){return err(_e2);}),_e4=function(_e5,_e6){var _e7=function(_e8,_e9){var _ea=E(_e8);if(!_ea[0]){var _eb=new T(function(){return A(_e9,[_s]);});return function(_ec){return A(_ec,[_eb]);};}else{var _ed=E(_ea[1])[1],_ee=function(_ef){var _eg=new T(function(){return _e7(_ea[2],function(_eh){return A(_e9,[[1,_ef,_eh]]);});});return function(_ei){var _ej=new T(function(){return A(_eg,[_ei]);});return [0,function(_ek){return E(_ej);}];};};switch(E(E(_e5)[1])){case 8:if(48>_ed){var _el=new T(function(){return A(_e9,[_s]);});return function(_em){return A(_em,[_el]);};}else{if(_ed>55){var _en=new T(function(){return A(_e9,[_s]);});return function(_eo){return A(_eo,[_en]);};}else{return _ee([0,_ed-48|0]);}}break;case 10:if(48>_ed){var _ep=new T(function(){return A(_e9,[_s]);});return function(_eq){return A(_eq,[_ep]);};}else{if(_ed>57){var _er=new T(function(){return A(_e9,[_s]);});return function(_es){return A(_es,[_er]);};}else{return _ee([0,_ed-48|0]);}}break;case 16:var _et=new T(function(){return 97>_ed?65>_ed?[0]:_ed>70?[0]:[1,[0,(_ed-65|0)+10|0]]:_ed>102?65>_ed?[0]:_ed>70?[0]:[1,[0,(_ed-65|0)+10|0]]:[1,[0,(_ed-97|0)+10|0]];});if(48>_ed){var _eu=E(_et);if(!_eu[0]){var _ev=new T(function(){return A(_e9,[_s]);});return function(_ew){return A(_ew,[_ev]);};}else{return _ee(_eu[1]);}}else{if(_ed>57){var _ex=E(_et);if(!_ex[0]){var _ey=new T(function(){return A(_e9,[_s]);});return function(_ez){return A(_ez,[_ey]);};}else{return _ee(_ex[1]);}}else{return _ee([0,_ed-48|0]);}}break;default:return E(_e3);}}};return [1,function(_eA){return A(_e7,[_eA,_7,function(_eB){var _eC=E(_eB);return _eC[0]==0?[2]:A(_e6,[_eC]);}]);}];},_eD=[0,10],_eE=[0,1],_eF=[0,2147483647],_eG=function(_eH,_eI){while(1){var _eJ=E(_eH);if(!_eJ[0]){var _eK=_eJ[1],_eL=E(_eI);if(!_eL[0]){var _eM=_eL[1],_eN=addC(_eK,_eM);if(!E(_eN[2])){return [0,_eN[1]];}else{_eH=[1,I_fromInt(_eK)];_eI=[1,I_fromInt(_eM)];continue;}}else{_eH=[1,I_fromInt(_eK)];_eI=_eL;continue;}}else{var _eO=E(_eI);if(!_eO[0]){_eH=_eJ;_eI=[1,I_fromInt(_eO[1])];continue;}else{return [1,I_add(_eJ[1],_eO[1])];}}}},_eP=new T(function(){return _eG(_eF,_eE);}),_eQ=function(_eR){var _eS=E(_eR);if(!_eS[0]){var _eT=E(_eS[1]);return _eT==(-2147483648)?E(_eP):[0, -_eT];}else{return [1,I_negate(_eS[1])];}},_eU=[0,10],_eV=[0,0],_eW=function(_eX,_eY){while(1){var _eZ=E(_eX);if(!_eZ[0]){var _f0=_eZ[1],_f1=E(_eY);if(!_f1[0]){var _f2=_f1[1];if(!(imul(_f0,_f2)|0)){return [0,imul(_f0,_f2)|0];}else{_eX=[1,I_fromInt(_f0)];_eY=[1,I_fromInt(_f2)];continue;}}else{_eX=[1,I_fromInt(_f0)];_eY=_f1;continue;}}else{var _f3=E(_eY);if(!_f3[0]){_eX=_eZ;_eY=[1,I_fromInt(_f3[1])];continue;}else{return [1,I_mul(_eZ[1],_f3[1])];}}}},_f4=function(_f5,_f6,_f7){while(1){var _f8=E(_f7);if(!_f8[0]){return E(_f6);}else{var _f9=_eG(_eW(_f6,_f5),_f8[1]);_f7=_f8[2];_f6=_f9;continue;}}},_fa=function(_fb){var _fc=new T(function(){return _cw(_cw([0,function(_fd){return E(E(_fd)[1])==45?_e4(_eD,function(_fe){return A(_fb,[[1,new T(function(){return _eQ(_f4(_eU,_eV,_fe));})]]);}):[2];}],[0,function(_ff){return E(E(_ff)[1])==43?_e4(_eD,function(_fg){return A(_fb,[[1,new T(function(){return _f4(_eU,_eV,_fg);})]]);}):[2];}]),new T(function(){return _e4(_eD,function(_fh){return A(_fb,[[1,new T(function(){return _f4(_eU,_eV,_fh);})]]);});}));});return _cw([0,function(_fi){return E(E(_fi)[1])==101?E(_fc):[2];}],[0,function(_fj){return E(E(_fj)[1])==69?E(_fc):[2];}]);},_fk=function(_fl){return A(_fl,[_b]);},_fm=function(_fn){return A(_fn,[_b]);},_fo=function(_fp){var _fq=new T(function(){return _e4(_eD,function(_fr){return A(_fp,[[1,_fr]]);});});return [0,function(_fs){return E(E(_fs)[1])==46?E(_fq):[2];}];},_ft=function(_fu){return _e4(_eD,function(_fv){return _dF(_fo,_fk,function(_fw){return _dF(_fa,_fm,function(_fx){return A(_fu,[[5,[1,_fv,_fw,_fx]]]);});});});},_fy=function(_fz,_fA,_fB){while(1){var _fC=E(_fB);if(!_fC[0]){return false;}else{if(!A(_d4,[_fz,_fA,_fC[1]])){_fB=_fC[2];continue;}else{return true;}}}},_fD=unCStr("!@#$%&*+./<=>?\\^|:-~"),_fE=function(_fF){return _fy(_d3,_fF,_fD);},_fG=[0,8],_fH=[0,16],_fI=function(_fJ){var _fK=new T(function(){return _e4(_fH,function(_fL){return A(_fJ,[[5,[0,_fH,_fL]]]);});}),_fM=new T(function(){return _e4(_fG,function(_fN){return A(_fJ,[[5,[0,_fG,_fN]]]);});}),_fO=new T(function(){return _e4(_fH,function(_fP){return A(_fJ,[[5,[0,_fH,_fP]]]);});}),_fQ=new T(function(){return _e4(_fG,function(_fR){return A(_fJ,[[5,[0,_fG,_fR]]]);});});return [0,function(_fS){return E(E(_fS)[1])==48?E([0,function(_fT){switch(E(E(_fT)[1])){case 79:return E(_fQ);case 88:return E(_fO);case 111:return E(_fM);case 120:return E(_fK);default:return [2];}}]):[2];}];},_fU=false,_fV=true,_fW=function(_fX){var _fY=new T(function(){return A(_fX,[_fH]);}),_fZ=new T(function(){return A(_fX,[_fG]);}),_g0=new T(function(){return A(_fX,[_fH]);}),_g1=new T(function(){return A(_fX,[_fG]);});return [0,function(_g2){switch(E(E(_g2)[1])){case 79:return E(_g1);case 88:return E(_g0);case 111:return E(_fZ);case 120:return E(_fY);default:return [2];}}];},_g3=function(_g4){return A(_g4,[_eD]);},_g5=function(_g6){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _2O(9,_g6,_s);})));},_g7=function(_g8){var _g9=E(_g8);return _g9[0]==0?E(_g9[1]):I_toInt(_g9[1]);},_ga=function(_gb,_gc){var _gd=E(_gb);if(!_gd[0]){var _ge=_gd[1],_gf=E(_gc);return _gf[0]==0?_ge<=_gf[1]:I_compareInt(_gf[1],_ge)>=0;}else{var _gg=_gd[1],_gh=E(_gc);return _gh[0]==0?I_compareInt(_gg,_gh[1])<=0:I_compare(_gg,_gh[1])<=0;}},_gi=function(_gj){return [2];},_gk=function(_gl){var _gm=E(_gl);if(!_gm[0]){return E(_gi);}else{var _gn=_gm[1],_go=E(_gm[2]);if(!_go[0]){return E(_gn);}else{var _gp=new T(function(){return _gk(_go);});return function(_gq){return _cw(A(_gn,[_gq]),new T(function(){return A(_gp,[_gq]);}));};}}},_gr=unCStr("NUL"),_gs=function(_gt){return [2];},_gu=function(_gv){return _gs(_gv);},_gw=function(_gx,_gy){var _gz=function(_gA,_gB){var _gC=E(_gA);if(!_gC[0]){return function(_gD){return A(_gD,[_gx]);};}else{var _gE=E(_gB);if(!_gE[0]){return E(_gs);}else{if(E(_gC[1])[1]!=E(_gE[1])[1]){return E(_gu);}else{var _gF=new T(function(){return _gz(_gC[2],_gE[2]);});return function(_gG){var _gH=new T(function(){return A(_gF,[_gG]);});return [0,function(_gI){return E(_gH);}];};}}}};return [1,function(_gJ){return A(_gz,[_gx,_gJ,_gy]);}];},_gK=[0,0],_gL=function(_gM){var _gN=new T(function(){return A(_gM,[_gK]);});return _gw(_gr,function(_gO){return E(_gN);});},_gP=unCStr("STX"),_gQ=[0,2],_gR=function(_gS){var _gT=new T(function(){return A(_gS,[_gQ]);});return _gw(_gP,function(_gU){return E(_gT);});},_gV=unCStr("ETX"),_gW=[0,3],_gX=function(_gY){var _gZ=new T(function(){return A(_gY,[_gW]);});return _gw(_gV,function(_h0){return E(_gZ);});},_h1=unCStr("EOT"),_h2=[0,4],_h3=function(_h4){var _h5=new T(function(){return A(_h4,[_h2]);});return _gw(_h1,function(_h6){return E(_h5);});},_h7=unCStr("ENQ"),_h8=[0,5],_h9=function(_ha){var _hb=new T(function(){return A(_ha,[_h8]);});return _gw(_h7,function(_hc){return E(_hb);});},_hd=unCStr("ACK"),_he=[0,6],_hf=function(_hg){var _hh=new T(function(){return A(_hg,[_he]);});return _gw(_hd,function(_hi){return E(_hh);});},_hj=unCStr("BEL"),_hk=[0,7],_hl=function(_hm){var _hn=new T(function(){return A(_hm,[_hk]);});return _gw(_hj,function(_ho){return E(_hn);});},_hp=unCStr("BS"),_hq=[0,8],_hr=function(_hs){var _ht=new T(function(){return A(_hs,[_hq]);});return _gw(_hp,function(_hu){return E(_ht);});},_hv=unCStr("HT"),_hw=[0,9],_hx=function(_hy){var _hz=new T(function(){return A(_hy,[_hw]);});return _gw(_hv,function(_hA){return E(_hz);});},_hB=unCStr("LF"),_hC=[0,10],_hD=function(_hE){var _hF=new T(function(){return A(_hE,[_hC]);});return _gw(_hB,function(_hG){return E(_hF);});},_hH=unCStr("VT"),_hI=[0,11],_hJ=function(_hK){var _hL=new T(function(){return A(_hK,[_hI]);});return _gw(_hH,function(_hM){return E(_hL);});},_hN=unCStr("FF"),_hO=[0,12],_hP=function(_hQ){var _hR=new T(function(){return A(_hQ,[_hO]);});return _gw(_hN,function(_hS){return E(_hR);});},_hT=unCStr("CR"),_hU=[0,13],_hV=function(_hW){var _hX=new T(function(){return A(_hW,[_hU]);});return _gw(_hT,function(_hY){return E(_hX);});},_hZ=unCStr("SI"),_i0=[0,15],_i1=function(_i2){var _i3=new T(function(){return A(_i2,[_i0]);});return _gw(_hZ,function(_i4){return E(_i3);});},_i5=unCStr("DLE"),_i6=[0,16],_i7=function(_i8){var _i9=new T(function(){return A(_i8,[_i6]);});return _gw(_i5,function(_ia){return E(_i9);});},_ib=unCStr("DC1"),_ic=[0,17],_id=function(_ie){var _if=new T(function(){return A(_ie,[_ic]);});return _gw(_ib,function(_ig){return E(_if);});},_ih=unCStr("DC2"),_ii=[0,18],_ij=function(_ik){var _il=new T(function(){return A(_ik,[_ii]);});return _gw(_ih,function(_im){return E(_il);});},_in=unCStr("DC3"),_io=[0,19],_ip=function(_iq){var _ir=new T(function(){return A(_iq,[_io]);});return _gw(_in,function(_is){return E(_ir);});},_it=unCStr("DC4"),_iu=[0,20],_iv=function(_iw){var _ix=new T(function(){return A(_iw,[_iu]);});return _gw(_it,function(_iy){return E(_ix);});},_iz=unCStr("NAK"),_iA=[0,21],_iB=function(_iC){var _iD=new T(function(){return A(_iC,[_iA]);});return _gw(_iz,function(_iE){return E(_iD);});},_iF=unCStr("SYN"),_iG=[0,22],_iH=function(_iI){var _iJ=new T(function(){return A(_iI,[_iG]);});return _gw(_iF,function(_iK){return E(_iJ);});},_iL=unCStr("ETB"),_iM=[0,23],_iN=function(_iO){var _iP=new T(function(){return A(_iO,[_iM]);});return _gw(_iL,function(_iQ){return E(_iP);});},_iR=unCStr("CAN"),_iS=[0,24],_iT=function(_iU){var _iV=new T(function(){return A(_iU,[_iS]);});return _gw(_iR,function(_iW){return E(_iV);});},_iX=unCStr("EM"),_iY=[0,25],_iZ=function(_j0){var _j1=new T(function(){return A(_j0,[_iY]);});return _gw(_iX,function(_j2){return E(_j1);});},_j3=unCStr("SUB"),_j4=[0,26],_j5=function(_j6){var _j7=new T(function(){return A(_j6,[_j4]);});return _gw(_j3,function(_j8){return E(_j7);});},_j9=unCStr("ESC"),_ja=[0,27],_jb=function(_jc){var _jd=new T(function(){return A(_jc,[_ja]);});return _gw(_j9,function(_je){return E(_jd);});},_jf=unCStr("FS"),_jg=[0,28],_jh=function(_ji){var _jj=new T(function(){return A(_ji,[_jg]);});return _gw(_jf,function(_jk){return E(_jj);});},_jl=unCStr("GS"),_jm=[0,29],_jn=function(_jo){var _jp=new T(function(){return A(_jo,[_jm]);});return _gw(_jl,function(_jq){return E(_jp);});},_jr=unCStr("RS"),_js=[0,30],_jt=function(_ju){var _jv=new T(function(){return A(_ju,[_js]);});return _gw(_jr,function(_jw){return E(_jv);});},_jx=unCStr("US"),_jy=[0,31],_jz=function(_jA){var _jB=new T(function(){return A(_jA,[_jy]);});return _gw(_jx,function(_jC){return E(_jB);});},_jD=unCStr("SP"),_jE=[0,32],_jF=function(_jG){var _jH=new T(function(){return A(_jG,[_jE]);});return _gw(_jD,function(_jI){return E(_jH);});},_jJ=unCStr("DEL"),_jK=[0,127],_jL=function(_jM){var _jN=new T(function(){return A(_jM,[_jK]);});return _gw(_jJ,function(_jO){return E(_jN);});},_jP=[1,_jL,_s],_jQ=[1,_jF,_jP],_jR=[1,_jz,_jQ],_jS=[1,_jt,_jR],_jT=[1,_jn,_jS],_jU=[1,_jh,_jT],_jV=[1,_jb,_jU],_jW=[1,_j5,_jV],_jX=[1,_iZ,_jW],_jY=[1,_iT,_jX],_jZ=[1,_iN,_jY],_k0=[1,_iH,_jZ],_k1=[1,_iB,_k0],_k2=[1,_iv,_k1],_k3=[1,_ip,_k2],_k4=[1,_ij,_k3],_k5=[1,_id,_k4],_k6=[1,_i7,_k5],_k7=[1,_i1,_k6],_k8=[1,_hV,_k7],_k9=[1,_hP,_k8],_ka=[1,_hJ,_k9],_kb=[1,_hD,_ka],_kc=[1,_hx,_kb],_kd=[1,_hr,_kc],_ke=[1,_hl,_kd],_kf=[1,_hf,_ke],_kg=[1,_h9,_kf],_kh=[1,_h3,_kg],_ki=[1,_gX,_kh],_kj=[1,_gR,_ki],_kk=[1,_gL,_kj],_kl=unCStr("SOH"),_km=[0,1],_kn=function(_ko){var _kp=new T(function(){return A(_ko,[_km]);});return _gw(_kl,function(_kq){return E(_kp);});},_kr=unCStr("SO"),_ks=[0,14],_kt=function(_ku){var _kv=new T(function(){return A(_ku,[_ks]);});return _gw(_kr,function(_kw){return E(_kv);});},_kx=function(_ky){return _dF(_kn,_kt,_ky);},_kz=[1,_kx,_kk],_kA=new T(function(){return _gk(_kz);}),_kB=[0,1114111],_kC=[0,34],_kD=[0,_kC,_fV],_kE=[0,39],_kF=[0,_kE,_fV],_kG=[0,92],_kH=[0,_kG,_fV],_kI=[0,_hk,_fV],_kJ=[0,_hq,_fV],_kK=[0,_hO,_fV],_kL=[0,_hC,_fV],_kM=[0,_hU,_fV],_kN=[0,_hw,_fV],_kO=[0,_hI,_fV],_kP=[0,_gK,_fV],_kQ=[0,_km,_fV],_kR=[0,_gQ,_fV],_kS=[0,_gW,_fV],_kT=[0,_h2,_fV],_kU=[0,_h8,_fV],_kV=[0,_he,_fV],_kW=[0,_hk,_fV],_kX=[0,_hq,_fV],_kY=[0,_hw,_fV],_kZ=[0,_hC,_fV],_l0=[0,_hI,_fV],_l1=[0,_hO,_fV],_l2=[0,_hU,_fV],_l3=[0,_ks,_fV],_l4=[0,_i0,_fV],_l5=[0,_i6,_fV],_l6=[0,_ic,_fV],_l7=[0,_ii,_fV],_l8=[0,_io,_fV],_l9=[0,_iu,_fV],_la=[0,_iA,_fV],_lb=[0,_iG,_fV],_lc=[0,_iM,_fV],_ld=[0,_iS,_fV],_le=[0,_iY,_fV],_lf=[0,_j4,_fV],_lg=[0,_ja,_fV],_lh=[0,_jg,_fV],_li=[0,_jm,_fV],_lj=[0,_js,_fV],_lk=[0,_jy,_fV],_ll=function(_lm){return [0,_lm];},_ln=function(_lo){var _lp=new T(function(){return A(_lo,[_kO]);}),_lq=new T(function(){return A(_lo,[_kN]);}),_lr=new T(function(){return A(_lo,[_kM]);}),_ls=new T(function(){return A(_lo,[_kL]);}),_lt=new T(function(){return A(_lo,[_kK]);}),_lu=new T(function(){return A(_lo,[_kJ]);}),_lv=new T(function(){return A(_lo,[_kI]);}),_lw=new T(function(){return A(_lo,[_kH]);}),_lx=new T(function(){return A(_lo,[_kF]);}),_ly=new T(function(){return A(_lo,[_kD]);});return _cw([0,function(_lz){switch(E(E(_lz)[1])){case 34:return E(_ly);case 39:return E(_lx);case 92:return E(_lw);case 97:return E(_lv);case 98:return E(_lu);case 102:return E(_lt);case 110:return E(_ls);case 114:return E(_lr);case 116:return E(_lq);case 118:return E(_lp);default:return [2];}}],new T(function(){return _cw(_dF(_fW,_g3,function(_lA){var _lB=new T(function(){return _ll(E(_lA)[1]);});return _e4(_lA,function(_lC){var _lD=_f4(_lB,_eV,_lC);return !_ga(_lD,_kB)?[2]:A(_lo,[[0,new T(function(){var _lE=_g7(_lD);return _lE>>>0>1114111?_g5(_lE):[0,_lE];}),_fV]]);});}),new T(function(){var _lF=new T(function(){return A(_lo,[_lk]);}),_lG=new T(function(){return A(_lo,[_lj]);}),_lH=new T(function(){return A(_lo,[_li]);}),_lI=new T(function(){return A(_lo,[_lh]);}),_lJ=new T(function(){return A(_lo,[_lg]);}),_lK=new T(function(){return A(_lo,[_lf]);}),_lL=new T(function(){return A(_lo,[_le]);}),_lM=new T(function(){return A(_lo,[_ld]);}),_lN=new T(function(){return A(_lo,[_lc]);}),_lO=new T(function(){return A(_lo,[_lb]);}),_lP=new T(function(){return A(_lo,[_la]);}),_lQ=new T(function(){return A(_lo,[_l9]);}),_lR=new T(function(){return A(_lo,[_l8]);}),_lS=new T(function(){return A(_lo,[_l7]);}),_lT=new T(function(){return A(_lo,[_l6]);}),_lU=new T(function(){return A(_lo,[_l5]);}),_lV=new T(function(){return A(_lo,[_l4]);}),_lW=new T(function(){return A(_lo,[_l3]);}),_lX=new T(function(){return A(_lo,[_l2]);}),_lY=new T(function(){return A(_lo,[_l1]);}),_lZ=new T(function(){return A(_lo,[_l0]);}),_m0=new T(function(){return A(_lo,[_kZ]);}),_m1=new T(function(){return A(_lo,[_kY]);}),_m2=new T(function(){return A(_lo,[_kX]);}),_m3=new T(function(){return A(_lo,[_kW]);}),_m4=new T(function(){return A(_lo,[_kV]);}),_m5=new T(function(){return A(_lo,[_kU]);}),_m6=new T(function(){return A(_lo,[_kT]);}),_m7=new T(function(){return A(_lo,[_kS]);}),_m8=new T(function(){return A(_lo,[_kR]);}),_m9=new T(function(){return A(_lo,[_kQ]);}),_ma=new T(function(){return A(_lo,[_kP]);});return _cw([0,function(_mb){return E(E(_mb)[1])==94?E([0,function(_mc){switch(E(E(_mc)[1])){case 64:return E(_ma);case 65:return E(_m9);case 66:return E(_m8);case 67:return E(_m7);case 68:return E(_m6);case 69:return E(_m5);case 70:return E(_m4);case 71:return E(_m3);case 72:return E(_m2);case 73:return E(_m1);case 74:return E(_m0);case 75:return E(_lZ);case 76:return E(_lY);case 77:return E(_lX);case 78:return E(_lW);case 79:return E(_lV);case 80:return E(_lU);case 81:return E(_lT);case 82:return E(_lS);case 83:return E(_lR);case 84:return E(_lQ);case 85:return E(_lP);case 86:return E(_lO);case 87:return E(_lN);case 88:return E(_lM);case 89:return E(_lL);case 90:return E(_lK);case 91:return E(_lJ);case 92:return E(_lI);case 93:return E(_lH);case 94:return E(_lG);case 95:return E(_lF);default:return [2];}}]):[2];}],new T(function(){return A(_kA,[function(_md){return A(_lo,[[0,_md,_fV]]);}]);}));}));}));},_me=function(_mf){return A(_mf,[_0]);},_mg=function(_mh){var _mi=E(_mh);if(!_mi[0]){return E(_me);}else{var _mj=_mi[2],_mk=E(E(_mi[1])[1]);switch(_mk){case 9:var _ml=new T(function(){return _mg(_mj);});return function(_mm){var _mn=new T(function(){return A(_ml,[_mm]);});return [0,function(_mo){return E(_mn);}];};case 10:var _mp=new T(function(){return _mg(_mj);});return function(_mq){var _mr=new T(function(){return A(_mp,[_mq]);});return [0,function(_ms){return E(_mr);}];};case 11:var _mt=new T(function(){return _mg(_mj);});return function(_mu){var _mv=new T(function(){return A(_mt,[_mu]);});return [0,function(_mw){return E(_mv);}];};case 12:var _mx=new T(function(){return _mg(_mj);});return function(_my){var _mz=new T(function(){return A(_mx,[_my]);});return [0,function(_mA){return E(_mz);}];};case 13:var _mB=new T(function(){return _mg(_mj);});return function(_mC){var _mD=new T(function(){return A(_mB,[_mC]);});return [0,function(_mE){return E(_mD);}];};case 32:var _mF=new T(function(){return _mg(_mj);});return function(_mG){var _mH=new T(function(){return A(_mF,[_mG]);});return [0,function(_mI){return E(_mH);}];};case 160:var _mJ=new T(function(){return _mg(_mj);});return function(_mK){var _mL=new T(function(){return A(_mJ,[_mK]);});return [0,function(_mM){return E(_mL);}];};default:var _mN=u_iswspace(_mk);if(!E(_mN)){return E(_me);}else{var _mO=new T(function(){return _mg(_mj);});return function(_mP){var _mQ=new T(function(){return A(_mO,[_mP]);});return [0,function(_mR){return E(_mQ);}];};}}}},_mS=function(_mT){var _mU=new T(function(){return _ln(_mT);}),_mV=new T(function(){return _mS(_mT);}),_mW=[1,function(_mX){return A(_mg,[_mX,function(_mY){return E([0,function(_mZ){return E(E(_mZ)[1])==92?E(_mV):[2];}]);}]);}];return _cw([0,function(_n0){return E(E(_n0)[1])==92?E([0,function(_n1){var _n2=E(E(_n1)[1]);switch(_n2){case 9:return E(_mW);case 10:return E(_mW);case 11:return E(_mW);case 12:return E(_mW);case 13:return E(_mW);case 32:return E(_mW);case 38:return E(_mV);case 160:return E(_mW);default:var _n3=u_iswspace(_n2);return E(_n3)==0?[2]:E(_mW);}}]):[2];}],[0,function(_n4){var _n5=E(_n4);return E(_n5[1])==92?E(_mU):A(_mT,[[0,_n5,_fU]]);}]);},_n6=function(_n7,_n8){var _n9=new T(function(){return A(_n8,[[1,new T(function(){return A(_n7,[_s]);})]]);});return _mS(function(_na){var _nb=E(_na),_nc=E(_nb[1]);return E(_nc[1])==34?!E(_nb[2])?E(_n9):_n6(function(_nd){return A(_n7,[[1,_nc,_nd]]);},_n8):_n6(function(_ne){return A(_n7,[[1,_nc,_ne]]);},_n8);});},_nf=unCStr("_\'"),_ng=function(_nh){var _ni=u_iswalnum(_nh);return E(_ni)==0?_fy(_d3,[0,_nh],_nf):true;},_nj=function(_nk){return _ng(E(_nk)[1]);},_nl=unCStr(",;()[]{}`"),_nm=function(_nn){return A(_nn,[_s]);},_no=function(_np,_nq){var _nr=function(_ns){var _nt=E(_ns);if(!_nt[0]){return E(_nm);}else{var _nu=_nt[1];if(!A(_np,[_nu])){return E(_nm);}else{var _nv=new T(function(){return _nr(_nt[2]);});return function(_nw){var _nx=new T(function(){return A(_nv,[function(_ny){return A(_nw,[[1,_nu,_ny]]);}]);});return [0,function(_nz){return E(_nx);}];};}}};return [1,function(_nA){return A(_nr,[_nA,_nq]);}];},_nB=unCStr(".."),_nC=unCStr("::"),_nD=unCStr("->"),_nE=[0,64],_nF=[1,_nE,_s],_nG=[0,126],_nH=[1,_nG,_s],_nI=unCStr("=>"),_nJ=[1,_nI,_s],_nK=[1,_nH,_nJ],_nL=[1,_nF,_nK],_nM=[1,_nD,_nL],_nN=unCStr("<-"),_nO=[1,_nN,_nM],_nP=[0,124],_nQ=[1,_nP,_s],_nR=[1,_nQ,_nO],_nS=[1,_kG,_s],_nT=[1,_nS,_nR],_nU=[0,61],_nV=[1,_nU,_s],_nW=[1,_nV,_nT],_nX=[1,_nC,_nW],_nY=[1,_nB,_nX],_nZ=function(_o0){var _o1=new T(function(){return A(_o0,[_e1]);});return _cw([1,function(_o2){return E(_o2)[0]==0?E(_o1):[2];}],new T(function(){var _o3=new T(function(){return _ln(function(_o4){var _o5=E(_o4);return (function(_o6,_o7){var _o8=new T(function(){return A(_o0,[[0,_o6]]);});return !E(_o7)?E(E(_o6)[1])==39?[2]:[0,function(_o9){return E(E(_o9)[1])==39?E(_o8):[2];}]:[0,function(_oa){return E(E(_oa)[1])==39?E(_o8):[2];}];})(_o5[1],_o5[2]);});});return _cw([0,function(_ob){return E(E(_ob)[1])==39?E([0,function(_oc){var _od=E(_oc);switch(E(_od[1])){case 39:return [2];case 92:return E(_o3);default:var _oe=new T(function(){return A(_o0,[[0,_od]]);});return [0,function(_of){return E(E(_of)[1])==39?E(_oe):[2];}];}}]):[2];}],new T(function(){var _og=new T(function(){return _n6(_7,_o0);});return _cw([0,function(_oh){return E(E(_oh)[1])==34?E(_og):[2];}],new T(function(){return _cw([0,function(_oi){return !_fy(_d3,_oi,_nl)?[2]:A(_o0,[[2,[1,_oi,_s]]]);}],new T(function(){return _cw([0,function(_oj){return !_fy(_d3,_oj,_fD)?[2]:_no(_fE,function(_ok){var _ol=[1,_oj,_ok];return !_fy(_dk,_ol,_nY)?A(_o0,[[4,_ol]]):A(_o0,[[2,_ol]]);});}],new T(function(){return _cw([0,function(_om){var _on=E(_om),_oo=_on[1],_op=u_iswalpha(_oo);return E(_op)==0?E(_oo)==95?_no(_nj,function(_oq){return A(_o0,[[3,[1,_on,_oq]]]);}):[2]:_no(_nj,function(_or){return A(_o0,[[3,[1,_on,_or]]]);});}],new T(function(){return _dF(_fI,_ft,_o0);}));}));}));}));}));}));},_os=function(_ot){var _ou=new T(function(){return _nZ(_ot);});return [1,function(_ov){return A(_mg,[_ov,function(_ow){return E(_ou);}]);}];},_ox=[0,0],_oy=function(_oz,_oA){var _oB=new T(function(){return A(_oz,[_ox,function(_oC){var _oD=new T(function(){return A(_oA,[_oC]);});return _os(function(_oE){var _oF=E(_oE);if(_oF[0]==2){var _oG=E(_oF[1]);return _oG[0]==0?[2]:E(E(_oG[1])[1])==41?E(_oG[2])[0]==0?E(_oD):[2]:[2];}else{return [2];}});}]);});return _os(function(_oH){var _oI=E(_oH);if(_oI[0]==2){var _oJ=E(_oI[1]);return _oJ[0]==0?[2]:E(E(_oJ[1])[1])==40?E(_oJ[2])[0]==0?E(_oB):[2]:[2];}else{return [2];}});},_oK=function(_oL,_oM,_oN){var _oO=function(_oP,_oQ){var _oR=new T(function(){return _nZ(function(_oS){return A(_oL,[_oS,_oP,function(_oT){return A(_oQ,[new T(function(){return [0, -E(_oT)[1]];})]);}]);});});return _cw(_os(function(_oU){var _oV=E(_oU);if(_oV[0]==4){var _oW=E(_oV[1]);return _oW[0]==0?A(_oL,[_oV,_oP,_oQ]):E(E(_oW[1])[1])==45?E(_oW[2])[0]==0?E([1,function(_oX){return A(_mg,[_oX,function(_oY){return E(_oR);}]);}]):A(_oL,[_oV,_oP,_oQ]):A(_oL,[_oV,_oP,_oQ]);}else{return A(_oL,[_oV,_oP,_oQ]);}}),new T(function(){return _oy(_oO,_oQ);}));};return _oO(_oM,_oN);},_oZ=function(_p0,_p1){return [2];},_p2=function(_p3,_p4){return _oZ(_p3,_p4);},_p5=function(_p6){var _p7=E(_p6);return _p7[0]==0?[1,new T(function(){return _f4(new T(function(){return _ll(E(_p7[1])[1]);}),_eV,_p7[2]);})]:E(_p7[2])[0]==0?E(_p7[3])[0]==0?[1,new T(function(){return _f4(_eU,_eV,_p7[1]);})]:[0]:[0];},_p8=function(_p9){var _pa=E(_p9);if(_pa[0]==5){var _pb=_p5(_pa[1]);if(!_pb[0]){return E(_oZ);}else{var _pc=new T(function(){return [0,_g7(_pb[1])];});return function(_pd,_pe){return A(_pe,[_pc]);};}}else{return E(_p2);}},_pf=function(_p3,_p4){return _oK(_p8,_p3,_p4);},_pg=function(_ph,_pi){var _pj=function(_pk,_pl){var _pm=new T(function(){return A(_pl,[_s]);}),_pn=new T(function(){return A(_ph,[_ox,function(_po){return _pj(_fV,function(_pp){return A(_pl,[[1,_po,_pp]]);});}]);});return _os(function(_pq){var _pr=E(_pq);if(_pr[0]==2){var _ps=E(_pr[1]);if(!_ps[0]){return [2];}else{var _pt=_ps[2];switch(E(E(_ps[1])[1])){case 44:return E(_pt)[0]==0?!E(_pk)?[2]:E(_pn):[2];case 93:return E(_pt)[0]==0?E(_pm):[2];default:return [2];}}}else{return [2];}});},_pu=function(_pv){var _pw=new T(function(){return _cw(_pj(_fU,_pv),new T(function(){return A(_ph,[_ox,function(_px){return _pj(_fV,function(_py){return A(_pv,[[1,_px,_py]]);});}]);}));});return _cw(_os(function(_pz){var _pA=E(_pz);if(_pA[0]==2){var _pB=E(_pA[1]);return _pB[0]==0?[2]:E(E(_pB[1])[1])==91?E(_pB[2])[0]==0?E(_pw):[2]:[2];}else{return [2];}}),new T(function(){return _oy(function(_pC,_pD){return _pu(_pD);},_pv);}));};return _pu(_pi);},_pE=function(_pF,_pG){return _pg(_pf,_pG);},_pH=new T(function(){return _pg(_pf,_dx);}),_pI=function(_p4){return _cm(_pH,_p4);},_pJ=function(_pK){var _pL=new T(function(){return _oK(_p8,_pK,_dx);});return function(_e0){return _cm(_pL,_e0);};},_pM=[0,_pJ,_pI,_pf,_pE],_pN=function(_pO,_pP){return _2O(0,E(_pO)[1],_pP);},_pQ=function(_pR,_pS){return _1G(_pN,_pR,_pS);},_pT=function(_pU,_pV,_pW){return _2O(E(_pU)[1],E(_pV)[1],_pW);},_pX=[0,_pT,_7o,_pQ],_pY=unCStr("GHC.Types"),_pZ=unCStr("Int"),_q0=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_8l,_pY,_pZ],_q1=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_q0,_s],_q2=function(_q3){return E(_q1);},_q4=function(_q5){return E(E(_q5)[1]);},_q6=function(_q7){return E(E(_q7)[1]);},_q8=function(_q9){return E(E(_q9)[2]);},_qa=function(_qb,_qc){var _qd=new T(function(){return A(_q8,[_qb,_qc]);}),_qe=new T(function(){return _q6(_qb);}),_qf=new T(function(){return _2A(_qe);}),_qg=new T(function(){return _2a(_qe);});return function(_qh){return A(_qg,[_qd,function(_qi){return A(_qf,[[0,_qi,_qh]]);}]);};},_qj=function(_qk,_ql){return A(_qk,[function(_){return jsFind(toJSStr(E(_ql)));}]);},_qm=function(_qn){return E(E(_qn)[4]);},_qo=unCStr("[]"),_qp=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_8l,_pY,_qo],_qq=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_qp,_s],_qr=function(_qs){return E(_qq);},_qt=unCStr("Char"),_qu=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_8l,_pY,_qt],_qv=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_qu,_s],_qw=function(_qx){return E(_qv);},_qy=new T(function(){return _9f(_qr,_qw);}),_qz=new T(function(){return A(_qy,[_9e]);}),_qA=new T(function(){return E(_9e);}),_qB=function(_qC){return E(E(_qC)[7]);},_qD=function(_qE){return E(E(_qE)[1]);},_qF=[0,0],_qG=[0,32],_qH=[0,10],_qI=function(_qJ){var _qK=E(_qJ);if(!_qK[0]){return E(_7);}else{var _qL=_qK[1],_qM=E(_qK[2]);if(!_qM[0]){return _qN(_qH,_qL);}else{var _qO=new T(function(){return _qI(_qM);}),_qP=new T(function(){return _qN(_qH,_qL);});return function(_qQ){return A(_qP,[[1,_qG,new T(function(){return A(_qO,[_qQ]);})]]);};}}},_qR=unCStr("->"),_qS=[1,_qR,_s],_qT=[1,_pY,_qS],_qU=[1,_8l,_qT],_qV=[0,32],_qW=function(_qX){var _qY=E(_qX);if(!_qY[0]){return [0];}else{var _qZ=_qY[1],_r0=E(_qY[2]);return _r0[0]==0?E(_qZ):_Q(_qZ,[1,_qV,new T(function(){return _qW(_r0);})]);}},_r1=new T(function(){return _qW(_qU);}),_r2=new T(function(){var _r3=_8J(_r1);return [0,_r3[1],_r3[2],_8l,_pY,_qR];}),_r4=function(_r5,_r6){var _r7=E(_r5);return _r7[0]==0?E(_r6):A(_r7[1],[new T(function(){return _r4(_r7[2],_r6);})]);},_r8=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_r9=[1,_8n,_s],_ra=function(_rb){var _rc=E(_rb);if(!_rc[0]){return [0];}else{var _rd=E(_rc[1]);return [1,[0,_rd[1],_rd[2]],new T(function(){return _ra(_rc[2]);})];}},_re=new T(function(){var _rf=_Q(_s,_r9);if(!_rf[0]){return E(_qp);}else{var _rg=_8J(new T(function(){return _8x(_8V(_96,[1,_r8,new T(function(){return _ra(_rf);})]));}));return E(_qp);}}),_rh=[0,40],_ri=function(_rj){return _qN(_qH,_rj);},_rk=[0,8],_rl=unCStr(" -> "),_rm=[0,9],_rn=[0,93],_ro=[0,91],_rp=[0,41],_rq=[0,44],_rr=function(_rj){return [1,_rq,_rj];},_rs=function(_rt,_ru){var _rv=E(_ru);return _rv[0]==0?[0]:[1,_rt,[1,_rv[1],new T(function(){return _rs(_rt,_rv[2]);})]];},_qN=function(_rw,_rx){var _ry=E(_rx),_rz=_ry[3],_rA=E(_ry[4]);if(!_rA[0]){return function(_rB){return _Q(E(_rz)[5],_rB);};}else{var _rC=_rA[1],_rD=new T(function(){var _rE=E(_rz)[5],_rF=new T(function(){return _qI(_rA);}),_rG=new T(function(){return E(_rw)[1]<=9?function(_rH){return _Q(_rE,[1,_qG,new T(function(){return A(_rF,[_rH]);})]);}:function(_rI){return [1,_2N,new T(function(){return _Q(_rE,[1,_qG,new T(function(){return A(_rF,[[1,_2M,_rI]]);})]);})];};}),_rJ=E(_rE);if(!_rJ[0]){return E(_rG);}else{if(E(E(_rJ[1])[1])==40){var _rK=E(_rJ[2]);return _rK[0]==0?E(_rG):E(E(_rK[1])[1])==44?function(_rL){return [1,_rh,new T(function(){return A(new T(function(){var _rM=_8V(_ri,_rA);if(!_rM[0]){return E(_7);}else{var _rN=new T(function(){return _rs(_rr,_rM[2]);});return function(_e0){return _r4([1,_rM[1],_rN],_e0);};}}),[[1,_rp,_rL]]);})];}:E(_rG);}else{return E(_rG);}}}),_rO=E(_rA[2]);if(!_rO[0]){var _rP=E(_rz),_rQ=E(_re),_rR=hs_eqWord64(_rP[1],_rQ[1]);if(!E(_rR)){return E(_rD);}else{var _rS=hs_eqWord64(_rP[2],_rQ[2]);if(!E(_rS)){return E(_rD);}else{var _rT=new T(function(){return _qN(_qF,_rC);});return function(_rU){return [1,_ro,new T(function(){return A(_rT,[[1,_rn,_rU]]);})];};}}}else{if(!E(_rO[2])[0]){var _rV=E(_rz),_rW=E(_r2),_rX=hs_eqWord64(_rV[1],_rW[1]);if(!E(_rX)){return E(_rD);}else{var _rY=hs_eqWord64(_rV[2],_rW[2]);if(!E(_rY)){return E(_rD);}else{var _rZ=new T(function(){return _qN(_rk,_rO[1]);}),_s0=new T(function(){return _qN(_rm,_rC);});return E(_rw)[1]<=8?function(_s1){return A(_s0,[new T(function(){return _Q(_rl,new T(function(){return A(_rZ,[_s1]);}));})]);}:function(_s2){return [1,_2N,new T(function(){return A(_s0,[new T(function(){return _Q(_rl,new T(function(){return A(_rZ,[[1,_2M,_s2]]);}));})]);})];};}}}else{return E(_rD);}}}},_s3=function(_s4,_s5,_s6,_s7){var _s8=new T(function(){return _2A(_s4);}),_s9=new T(function(){return _qm(_s7);}),_sa=new T(function(){return _qB(_s7);}),_sb=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_qN,[_qF,A(_s5,[_qA]),_s]);}));}),_sc=new T(function(){return A(_qD,[_s6,_3e]);});return function(_sd){if(!E(new T(function(){var _se=A(_s5,[_qA]),_sf=E(_qz),_sg=hs_eqWord64(_se[1],_sf[1]);if(!E(_sg)){return false;}else{var _sh=hs_eqWord64(_se[2],_sf[2]);return E(_sh)==0?false:true;}}))){var _si=new T(function(){return A(_s8,[[1,_sd,new T(function(){return A(_sa,[new T(function(){return A(_s9,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _Q(_sd,_sb);}));})]);})]);})]]);}),_sj=A(_sc,[_sd]);if(!_sj[0]){return E(_si);}else{var _sk=E(_sj[1]);return E(_sk[2])[0]==0?E(_sj[2])[0]==0?A(_s8,[[2,_sk[1]]]):E(_si):E(_si);}}else{return A(_s8,[[2,_sd]]);}};},_sl=[0],_sm=new T(function(){return [0,"value"];}),_sn=function(_so,_sp,_sq,_sr,_ss,_st){var _su=E(_so),_sv=_su[1],_sw=new T(function(){return A(_su[3],[_sl]);}),_sx=new T(function(){return _s3(_su,_sq,_sr,_ss);});return A(_sv,[new T(function(){return _qj(_sp,_st);}),function(_sy){var _sz=E(_sy);return _sz[0]==0?E(_sw):A(_sv,[new T(function(){return A(_sp,[function(_){var _sA=jsGet(E(_sz[1])[1],E(_sm)[1]);return [1,new T(function(){return fromJSStr(_sA);})];}]);}),function(_sB){var _sC=E(_sB);return _sC[0]==0?E(_sw):A(_sx,[_sC[1]]);}]);}]);},_sD=1,_sE=function(_sF){return E(E(_sF)[10]);},_sG=function(_sH,_sI){return A(_2A,[_sH,[0,_sI,_sI]]);},_sJ=function(_sK){return E(E(_sK)[2]);},_sL=function(_sM,_sN,_sO){return A(_2A,[_sM,[0,_0,_sN]]);},_sP=function(_sQ){return E(E(_sQ)[2]);},_sR=function(_sS,_sT,_sU,_sV,_sW){var _sX=new T(function(){return _q4(_sS);}),_sY=new T(function(){return _sJ(_sX);}),_sZ=new T(function(){return _q6(_sT);}),_t0=new T(function(){return _2C(_sZ);}),_t1=new T(function(){return _2U([0,coercionToken],_t0,function(_t2){return _sG(_sZ,_t2);},function(_t3,_t4){return _sL(_sZ,_t3,_t4);});}),_t5=new T(function(){return _2A(_sZ);}),_t6=new T(function(){return _2a(_sZ);}),_t7=new T(function(){return _2A(_sZ);}),_t8=new T(function(){return _2a(_sZ);}),_t9=new T(function(){return _2A(_sZ);}),_ta=new T(function(){return _2a(_sZ);}),_tb=new T(function(){return _2A(_sZ);}),_tc=new T(function(){return _2a(_sZ);}),_td=new T(function(){return _sP(_sV);}),_te=new T(function(){return _sE(_sS);});return function(_tf,_tg,_th){return function(_ti){return A(_tc,[new T(function(){var _tj=E(_tf);return _tj[0]==0?A(_t1,[_ti]):A(_tb,[[0,_tj[1],_ti]]);}),function(_tk){var _tl=new T(function(){return E(E(_tk)[1]);}),_tm=new T(function(){return _sn(_t0,function(_tn){return _qa(_sT,_tn);},_sU,_sW,_sS,_tl);}),_to=new T(function(){return A(_te,[_tl,_tg,new T(function(){var _tp=E(_th);if(!_tp[0]){return [0];}else{var _tq=_tp[1],_tr=_A(_sU,_qy,_tq);return _tr[0]==0?A(_td,[_tq]):E(_tr[1]);}}),_fU,_b]);});return A(_ta,[new T(function(){var _ts=new T(function(){return E(E(_tk)[2]);});return A(_t9,[[0,_ts,_ts]]);}),function(_tt){return A(_t8,[new T(function(){return A(_t7,[[0,_0,new T(function(){var _tu=E(E(_tt)[1]);return [0,_tu[1],_tu[2],_sD,_tu[4]];})]]);}),function(_tv){return A(_t6,[new T(function(){return A(_tm,[new T(function(){return E(E(_tv)[2]);})]);}),function(_tw){var _tx=E(_tw),_ty=_tx[2],_tz=E(_tx[1]);switch(_tz[0]){case 0:return A(_t5,[[0,[0,_to,_b],_ty]]);case 1:return A(_t5,[[0,[0,new T(function(){return A(_sY,[new T(function(){return A(_te,[_tl,_tg,_tz[1],_fU,_b]);}),_tz[2]]);}),_b],_ty]]);default:var _tA=_tz[1];return A(_t5,[[0,[0,new T(function(){return A(_te,[_tl,_tg,new T(function(){var _tB=_A(_sU,_qy,_tA);return _tB[0]==0?A(_td,[_tA]):E(_tB[1]);}),_fU,_b]);}),[1,_tA]],_ty]]);}}]);}]);}]);}]);};};},_tC=new T(function(){return _sR(_bs,_bt,_q2,_pX,_pM);}),_tD=new T(function(){return A(_tC,[_b,_87,_b]);}),_tE=function(_tF,_){var _tG=A(_tD,[_tF,_]),_tH=E(_tG),_tI=E(_tH[1]),_tJ=new T(function(){return E(E(_tF)[4]);});return [0,[0,function(_tK,_){return _7U(_tI[1],_7v,_tJ,_tK,_);},_tI[2]],_tH[2]];},_tL=[0,98],_tM=[1,_tL,_s],_tN=function(_tO,_tP){var _tQ=new T(function(){return A(_tO,[_tP]);});return function(_tR,_){var _tS=jsCreateElem(toJSStr(_tM)),_tT=jsAppendChild(_tS,E(_tR)[1]),_tU=[0,_tS],_tV=A(_tQ,[_tU,_]);return _tU;};},_tW=unCStr("br"),_tX=function(_tY,_){var _tZ=jsCreateElem(toJSStr(E(_tW))),_u0=jsAppendChild(_tZ,E(_tY)[1]);return [0,_tZ];},_u1=[1,_0],_u2=unCStr("result: "),_u3=function(_u4){var _u5=new T(function(){return _tN(_7q,new T(function(){return _7o(_u4);}));});return function(_e0,_u6){return _3v(_tE,function(_u7){var _u8=E(E(_u7)[1]);if(!_u8){return function(_u9,_){return [0,[0,function(_ua,_){var _ub=_tX(_ua,_),_uc=_7q(_u2,_ua,_),_ud=A(_u5,[_ua,_]);return _ua;},_b],_u9];};}else{var _ue=new T(function(){return _u3(new T(function(){return [0,E(_u4)[1]+_u8|0];}));}),_uf=new T(function(){return _tN(_7q,new T(function(){return _2O(0,E(_u4)[1]+_u8|0,_s);}));});return function(_e0,_u6){return _3v(function(_ug,_){return [0,[0,function(_uh,_){var _ui=A(_uf,[_uh,_]),_uj=_tX(_uh,_);return _uh;},_u1],_ug];},function(_uk){return E(_ue);},_e0,_u6);};}},_e0,_u6);};},_ul=new T(function(){return _u3(_9);}),_um=[0,3],_un=unCStr("This widget sums three numbers and append the result using a fold"),_uo=[0,112],_up=[1,_uo,_s],_uq=function(_ur,_us){var _ut=new T(function(){return A(_ur,[_us]);});return function(_uu,_){var _uv=jsCreateElem(toJSStr(_up)),_uw=jsAppendChild(_uv,E(_uu)[1]),_ux=[0,_uv],_uy=A(_ut,[_ux,_]);return _ux;};},_uz=new T(function(){return _uq(_7q,_un);}),_uA=function(_uB){var _uC=new T(function(){return _tN(_7q,new T(function(){return _7o(_uB);}));});return function(_uD,_){return [0,[0,function(_uE,_){var _uF=_tX(_uE,_),_uG=_7q(_u2,_uE,_),_uH=A(_uC,[_uE,_]);return _uE;},_u1],_uD];};},_uI=[1,_9],_uJ=[0,_27,_uI],_uK=function(_uL,_){return [0,_uJ,_uL];},_uM=function(_uN,_uO,_){var _uP=A(_uO,[_]);return _uN;},_uQ=function(_uR,_uS,_){var _uT=A(_uS,[_]);return new T(function(){return A(_uR,[_uT]);});},_uU=[0,_uQ,_uM],_uV=function(_uW){var _uX=E(_uW);return _uX[0]==0?0:E(_uX[1])[1]+_uV(_uX[2])|0;},_uY=function(_uZ){return [0,_uV(_uZ)];},_v0=function(_v1,_v2){return [0,E(_v1)[1]+E(_v2)[1]|0];},_v3=[0,_9,_v0,_uY],_v4=function(_v5,_v6){var _v7=E(_v6);return _v7[0]==0?[0]:[1,new T(function(){return A(_v5,[_v7[1]]);})];},_v8=function(_v9,_va,_vb,_vc,_vd,_ve){var _vf=new T(function(){return _sJ(_v9);});return A(_va,[new T(function(){return A(_vc,[_ve]);}),function(_vg){var _vh=E(_vg),_vi=E(_vh[1]);return A(_va,[new T(function(){return A(_vd,[_vh[2]]);}),function(_vj){var _vk=E(_vj),_vl=E(_vk[1]);return A(_vb,[[0,[0,new T(function(){return A(_vf,[_vi[1],_vl[1]]);}),new T(function(){var _vm=E(_vi[2]);if(!_vm[0]){return [0];}else{var _vn=E(_vl[2]);return _vn[0]==0?[0]:[1,new T(function(){return A(_vm[1],[_vn[1]]);})];}})],_vk[2]]]);}]);}]);},_vo=function(_vp){return E(E(_vp)[1]);},_vq=function(_vr,_vs,_vt,_vu,_vv,_vw){var _vx=new T(function(){return _q4(_vr);});return function(_vy){var _vz=E(_vs);return _v8(_vx,_vz[1],_vz[3],function(_vA){return A(new T(function(){var _vB=new T(function(){return _sJ(_vu);});return A(_vo,[_vt,function(_vC){return [0,new T(function(){var _vD=E(E(_vC)[1]);return [0,_vD[1],new T(function(){return _v4(_vB,_vD[2]);})];}),new T(function(){return E(E(_vC)[2]);})];}]);}),[new T(function(){return A(_vv,[_vA]);})]);},_vw,_vy);};},_vE=function(_vF,_vG){while(1){var _vH=(function(_vI,_vJ){var _vK=E(_vJ);if(!_vK[0]){return E(_vI);}else{_vF=new T(function(){return _vq(_bs,_29,_uU,_v3,_vI,_vK[1]);});_vG=_vK[2];return null;}})(_vF,_vG);if(_vH!=null){return _vH;}}},_vL=new T(function(){return A(_tC,[_b,_87,_b]);}),_vM=function(_vN,_){var _vO=A(_vL,[_vN,_]),_vP=E(_vO),_vQ=E(_vP[1]),_vR=new T(function(){return E(E(_vN)[4]);});return [0,[0,function(_vS,_){var _vT=_7U(_vQ[1],_7v,_vR,_vS,_),_vU=_tX(_vS,_);return _vS;},_vQ[2]],_vP[2]];},_vV=new T(function(){return [1,_vM,_vV];}),_vW=function(_vX,_vY){var _vZ=E(_vX);if(!_vZ){return [0];}else{var _w0=E(_vY);return _w0[0]==0?[0]:[1,_w0[1],new T(function(){return _vW(_vZ-1|0,_w0[2]);})];}},_w1=function(_w2,_w3){return _w2<0?[0]:_vW(_w2,_w3);},_w4=function(_w5,_w6){var _w7=E(_w5)[1];return _w7>0?_w1(_w7,_w6):[0];},_w8=function(_w9){return E(_w9);},_wa=function(_wb){var _wc=new T(function(){return _vE(_uK,_w4(_wb,_vV));});return function(_wd,_){var _we=_3v(_wc,_uA,_wd,_),_wf=E(_we),_wg=E(_wf[1]),_wh=new T(function(){return _uq(_w8,_wg[1]);});return [0,[0,function(_wi,_){var _wj=A(_uz,[_wi,_]),_wk=A(_wh,[_wi,_]);return _wi;},_wg[2]],_wf[2]];};},_wl=new T(function(){return _wa(_um);}),_wm=unCStr("This widget sums two numbers and append the result. Using applicative and monadic expressions"),_wn=new T(function(){return _uq(_7q,_wm);}),_wo=function(_wp){return function(_wq,_){return [0,[0,new T(function(){var _wr=new T(function(){return _tN(_7q,new T(function(){return _7o(_wp);}));});return _uq(_w8,function(_ws,_){var _wt=_7q(_u2,_ws,_),_wu=A(_wr,[_ws,_]);return _ws;});}),_u1],_wq];};},_wv=unCStr("second number "),_ww=unCStr("first number"),_wx=new T(function(){return A(_tC,[_b,_87,_b]);}),_wy=new T(function(){return A(_tC,[_b,_87,_b]);}),_wz=function(_wA,_){var _wB=A(_wy,[_wA,_]),_wC=E(_wB),_wD=_wC[2],_wE=E(_wC[1]),_wF=A(_wx,[_wD,_]),_wG=E(_wF),_wH=E(_wG[1]),_wI=new T(function(){return E(E(_wD)[4]);}),_wJ=new T(function(){return E(E(_wA)[4]);});return [0,[0,function(_wK,_){var _wL=_7q(_ww,_wK,_),_wM=_tX(_wK,_),_wN=_7U(_wE[1],_7v,_wJ,_wK,_),_wO=_tX(_wK,_),_wP=_7q(_wv,_wK,_),_wQ=_tX(_wK,_),_wR=_7U(_wH[1],_7v,_wI,_wK,_),_wS=_tX(_wK,_);return _wK;},new T(function(){var _wT=E(_wE[2]);if(!_wT[0]){return [0];}else{var _wU=E(_wH[2]);return _wU[0]==0?[0]:[1,new T(function(){return _v0(_wT[1],_wU[1]);})];}})],_wG[2]];},_wV=function(_wW,_){var _wX=_3v(_wz,_wo,_wW,_),_wY=E(_wX),_wZ=E(_wY[1]),_x0=new T(function(){return _uq(_w8,_wZ[1]);});return [0,[0,function(_x1,_){var _x2=A(_wn,[_x1,_]),_x3=A(_x0,[_x1,_]);return _x1;},_wZ[2]],_wY[2]];},_x4=unCStr("vertical-align:top"),_x5=unCStr("style"),_x6=unCStr("This widget sums recursively n numbers. When enters 0, present the result"),_x7=new T(function(){return _uq(_7q,_x6);}),_x8=unCStr("td"),_x9=unCStr("tr"),_xa=unCStr("table"),_xb=function(_xc,_){var _xd=_wV(_xc,_),_xe=E(_xd),_xf=A(_wl,[_xe[2],_]),_xg=E(_xf),_xh=A(_ul,[_xg[2],_]),_xi=E(_xh),_xj=E(_xi[1]);return [0,[0,function(_xk,_){var _xl=_88(_xa,_xk,_),_xm=_88(_x9,_xl,_),_xn=_88(_x8,_xm,_),_xo=A(E(_xe[1])[1],[_xn,_]),_xp=_88(_x8,_xm,_),_xq=A(E(_xg[1])[1],[_xp,_]),_xr=_88(_x8,_xm,_),_xs=A(_x7,[_xr,_]),_xt=A(_xj[1],[_xr,_]),_xu=A(_1,[_7,_xm,_x5,_x4,_]);return _xl;},_xj[2]],_xi[2]];},_xv=unCStr("idelem"),_xw=[0,_27,_3m],_xx=function(_xy,_xz,_){return [0,_xw,_xz];},_xA=function(_xB,_xC,_){return _3v(function(_xD,_){var _=wMV(E(_3k)[1],_xB);return [0,_3n,_xD];},_xx,_xC,_);},_xE=function(_xF,_xG,_){var _xH=E(_3k)[1],_xI=rMV(_xH),_xJ=A(_xF,[[0,_s,_xI,_3d,function(_){var _xK=function(_xL,_xM,_){var _xN=rMV(_xH),_xO=A(_xL,[[0,_s,_xN,_3d,function(_){return _xK(_xL,_xM,_);}],_]),_xP=E(_xO),_xQ=_3v(_3t,_xA,_xP[2],_),_xR=A(E(_xP[1])[1],[_xM,_]),_xS=A(E(E(_xQ)[1])[1],[_xM,_]);return _0;};return _xK(_xF,_xG,_);}],_]),_xT=E(_xJ),_xU=_3v(_3t,_xA,_xT[2],_),_xV=A(E(_xT[1])[1],[_xG,_]),_xW=A(E(E(_xU)[1])[1],[_xG,_]);return _0;},_xX=function(_){var _xY=E(_xv),_xZ=jsFind(toJSStr(_xY)),_y0=E(_xZ);return _y0[0]==0?_3x(_xY):_xE(_xb,_y0[1],_);},_y1=function(_){return _xX(_);};
var hasteMain = function() {A(_y1, [0]);};window.onload = hasteMain;