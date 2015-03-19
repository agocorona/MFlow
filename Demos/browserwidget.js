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

var _0=0,_1=function(_2,_3,_4,_5){return A(_2,[new T(function(){return function(_){var _6=jsSetAttr(E(_3)[1],toJSStr(E(_4)),toJSStr(E(_5)));return _0;};})]);},_7=2,_8=[1],_9=[0],_a=[0],_b=function(_c,_){return _a;},_d=function(_){return _a;},_e=[0,_d,_b],_f=[0,0],_g=[0,_9,_f,_7,_e,_8],_h=function(_){var _=0,_i=newMVar(),_=putMVar(_i,_g);return [0,_i];},_j=function(_k){var _l=A(_k,[_]);return E(_l);},_m=new T(function(){return _j(_h);}),_n=function(_o){return E(_o);},_p=unCStr("text-align:center"),_q=unCStr("id"),_r=function(_s,_t,_u,_){var _v=E(_t),_w=A(_s,[_u,_]),_x=A(_1,[_n,_w,_v[1],_v[2],_]);return _w;},_y=function(_z,_A){while(1){var _B=(function(_C,_D){var _E=E(_D);if(!_E[0]){return E(_C);}else{_z=function(_F,_){return _r(_C,_E[1],_F,_);};_A=_E[2];return null;}})(_z,_A);if(_B!=null){return _B;}}},_G=unCStr("span"),_H=function(_I,_J,_){var _K=jsCreateElem(toJSStr(E(_I))),_L=jsAppendChild(_K,E(_J)[1]);return [0,_K];},_M=function(_F,_){return _H(_G,_F,_);},_N=function(_O,_P,_){return [0,_0,_O];},_Q=function(_R,_){return [0,_R,_R];},_S=[0,coercionToken],_T=function(_U,_V,_){var _W=A(_U,[_]);return A(_V,[_]);},_X=function(_Y,_Z,_){return _T(_Y,_Z,_);},_10=function(_11,_12,_){var _13=A(_11,[_]);return A(_12,[_13,_]);},_14=unCStr("base"),_15=unCStr("GHC.IO.Exception"),_16=unCStr("IOException"),_17=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_14,_15,_16],_18=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_17,_9],_19=function(_1a){return E(_18);},_1b=function(_1c){return E(E(_1c)[1]);},_1d=unCStr("Maybe.fromJust: Nothing"),_1e=new T(function(){return err(_1d);}),_1f=function(_1g,_1h,_1i){var _1j=new T(function(){var _1k=A(_1g,[_1i]),_1l=A(_1h,[new T(function(){var _1m=E(_1j);return _1m[0]==0?E(_1e):E(_1m[1]);})]),_1n=hs_eqWord64(_1k[1],_1l[1]);if(!E(_1n)){return [0];}else{var _1o=hs_eqWord64(_1k[2],_1l[2]);return E(_1o)==0?[0]:[1,_1i];}});return E(_1j);},_1p=function(_1q){var _1r=E(_1q);return _1f(_1b(_1r[1]),_19,_1r[2]);},_1s=unCStr(": "),_1t=[0,41],_1u=unCStr(" ("),_1v=function(_1w,_1x){var _1y=E(_1w);return _1y[0]==0?E(_1x):[1,_1y[1],new T(function(){return _1v(_1y[2],_1x);})];},_1z=unCStr("already exists"),_1A=unCStr("does not exist"),_1B=unCStr("protocol error"),_1C=unCStr("failed"),_1D=unCStr("invalid argument"),_1E=unCStr("inappropriate type"),_1F=unCStr("hardware fault"),_1G=unCStr("unsupported operation"),_1H=unCStr("timeout"),_1I=unCStr("resource vanished"),_1J=unCStr("interrupted"),_1K=unCStr("resource busy"),_1L=unCStr("resource exhausted"),_1M=unCStr("end of file"),_1N=unCStr("illegal operation"),_1O=unCStr("permission denied"),_1P=unCStr("user error"),_1Q=unCStr("unsatisified constraints"),_1R=unCStr("system error"),_1S=function(_1T,_1U){switch(E(_1T)){case 0:return _1v(_1z,_1U);case 1:return _1v(_1A,_1U);case 2:return _1v(_1K,_1U);case 3:return _1v(_1L,_1U);case 4:return _1v(_1M,_1U);case 5:return _1v(_1N,_1U);case 6:return _1v(_1O,_1U);case 7:return _1v(_1P,_1U);case 8:return _1v(_1Q,_1U);case 9:return _1v(_1R,_1U);case 10:return _1v(_1B,_1U);case 11:return _1v(_1C,_1U);case 12:return _1v(_1D,_1U);case 13:return _1v(_1E,_1U);case 14:return _1v(_1F,_1U);case 15:return _1v(_1G,_1U);case 16:return _1v(_1H,_1U);case 17:return _1v(_1I,_1U);default:return _1v(_1J,_1U);}},_1V=[0,125],_1W=unCStr("{handle: "),_1X=function(_1Y,_1Z,_20,_21,_22,_23){var _24=new T(function(){var _25=new T(function(){return _1S(_1Z,new T(function(){var _26=E(_21);return _26[0]==0?E(_23):_1v(_1u,new T(function(){return _1v(_26,[1,_1t,_23]);}));}));}),_27=E(_20);return _27[0]==0?E(_25):_1v(_27,new T(function(){return _1v(_1s,_25);}));}),_28=E(_22);if(!_28[0]){var _29=E(_1Y);if(!_29[0]){return E(_24);}else{var _2a=E(_29[1]);return _2a[0]==0?_1v(_1W,new T(function(){return _1v(_2a[1],[1,_1V,new T(function(){return _1v(_1s,_24);})]);})):_1v(_1W,new T(function(){return _1v(_2a[1],[1,_1V,new T(function(){return _1v(_1s,_24);})]);}));}}else{return _1v(_28[1],new T(function(){return _1v(_1s,_24);}));}},_2b=function(_2c){var _2d=E(_2c);return _1X(_2d[1],_2d[2],_2d[3],_2d[4],_2d[6],_9);},_2e=function(_2f,_2g){var _2h=E(_2f);return _1X(_2h[1],_2h[2],_2h[3],_2h[4],_2h[6],_2g);},_2i=[0,44],_2j=[0,93],_2k=[0,91],_2l=function(_2m,_2n,_2o){var _2p=E(_2n);return _2p[0]==0?unAppCStr("[]",_2o):[1,_2k,new T(function(){return A(_2m,[_2p[1],new T(function(){var _2q=function(_2r){var _2s=E(_2r);return _2s[0]==0?E([1,_2j,_2o]):[1,_2i,new T(function(){return A(_2m,[_2s[1],new T(function(){return _2q(_2s[2]);})]);})];};return _2q(_2p[2]);})]);})];},_2t=function(_2u,_2v){return _2l(_2e,_2u,_2v);},_2w=function(_2x,_2y,_2z){var _2A=E(_2y);return _1X(_2A[1],_2A[2],_2A[3],_2A[4],_2A[6],_2z);},_2B=[0,_2w,_2b,_2t],_2C=new T(function(){return [0,_19,_2B,_2D,_1p];}),_2D=function(_2E){return [0,_2C,_2E];},_2F=7,_2G=function(_2H){return [0,_a,_2F,_9,_2H,_a,_a];},_2I=function(_2J,_){return die(new T(function(){return _2D(new T(function(){return _2G(_2J);}));}));},_2K=function(_2L,_){return _2I(_2L,_);},_2M=function(_2N,_){return _2N;},_2O=[0,_10,_X,_2M,_2K],_2P=function(_2Q){return E(E(_2Q)[1]);},_2R=function(_2S,_2T,_2U,_2V){return A(_2P,[_2S,new T(function(){return A(_2T,[_2V]);}),function(_2W){return A(_2U,[new T(function(){return E(E(_2W)[1]);}),new T(function(){return E(E(_2W)[2]);})]);}]);},_2X=function(_2Y,_2Z,_30,_31){return A(_2P,[_2Y,new T(function(){return A(_2Z,[_31]);}),function(_32){return A(_30,[new T(function(){return E(E(_32)[2]);})]);}]);},_33=function(_34,_35,_36,_37){return _2X(_34,_35,_36,_37);},_38=function(_39){return E(E(_39)[4]);},_3a=function(_3b,_3c){var _3d=new T(function(){return A(_38,[_3b,_3c]);});return function(_3e){return E(_3d);};},_3f=function(_3g){return E(E(_3g)[3]);},_3h=function(_3i){var _3j=new T(function(){return _3f(_3i);});return [0,function(_35,_36,_37){return _2R(_3i,_35,_36,_37);},function(_35,_36,_37){return _33(_3i,_35,_36,_37);},function(_3k,_3l){return A(_3j,[[0,_3k,_3l]]);},function(_37){return _3a(_3i,_37);}];},_3m=new T(function(){return _3h(_2O);}),_3n=[0,112],_3o=function(_3p,_3q){var _3r=jsShowI(_3p);return _1v(fromJSStr(_3r),_3q);},_3s=[0,41],_3t=[0,40],_3u=function(_3v,_3w,_3x){return _3w>=0?_3o(_3w,_3x):_3v<=6?_3o(_3w,_3x):[1,_3t,new T(function(){var _3y=jsShowI(_3w);return _1v(fromJSStr(_3y),[1,_3s,_3x]);})];},_3z=function(_3A,_3B,_3C,_3D){var _3E=E(_3B);return A(_3E[1],[new T(function(){var _3F=E(_3A);return E(_3C);}),function(_3G){var _3H=new T(function(){return E(E(_3G)[2]);});return A(_3E[2],[new T(function(){return A(_3D,[new T(function(){var _3I=E(new T(function(){var _3J=E(_3A);return [0,coercionToken];})),_3K=E(_3G);return [0,_3K[1],new T(function(){return [0,E(_3H)[1]+1|0];}),_3K[3],_3K[4],_3K[5]];})]);}),new T(function(){return A(_3E[3],[[1,_3n,new T(function(){return _1v(_3u(0,E(_3H)[1],_9),new T(function(){return E(E(_3G)[1]);}));})]]);})]);}]);},_3L=new T(function(){return _3z(_S,_3m,_Q,_N);}),_3M=unCStr(" could be found!"),_3N=function(_3O){return err(unAppCStr("No element with ID ",new T(function(){return _1v(_3O,_3M);})));},_3P=function(_3Q,_3R,_){var _3S=E(_3R),_3T=jsFind(toJSStr(_3S)),_3U=E(_3T);if(!_3U[0]){return _3N(_3S);}else{var _3V=E(_3U[1]),_3W=jsClearChildren(_3V[1]),_3X=E(_m)[1],_3Y=takeMVar(_3X),_3Z=A(_3Q,[_3Y,_]),_40=E(_3Z),_41=E(_40[1]),_=putMVar(_3X,_40[2]),_42=A(_41[1],[_3V,_]);return _41[2];}},_43=function(_44,_45,_46,_47,_48,_49,_4a,_4b,_){var _4c=E(_4a);return [0,_4c,[0,_47,_48,_49,[0,function(_){return _3P(function(_4d,_){var _4e=A(_44,[new T(function(){var _4f=E(_4d);return [0,_4f[1],_48,_4f[3],_4f[4],_4f[5]];}),_]);return [0,[0,_2M,E(E(_4e)[1])[2]],_4d];},_46,_);},function(_4g,_){var _4h=_3P(new T(function(){return A(_45,[_4g]);}),_46,_),_4i=E(_4h);return _4i[0]==0?_a:A(_4c[2],[_4i[1],_]);}],_4b]];},_4j=function(_4k,_4l,_4m,_){var _4n=A(_3L,[_4m,_]),_4o=E(_4n),_4p=_4o[1],_4q=E(_4o[2]),_4r=_43(_4k,_4l,_4p,_4q[1],_4q[2],_4q[3],_4q[4],_4q[5],_),_4s=A(_4k,[new T(function(){return E(E(_4r)[2]);}),_]),_4t=E(_4s),_4u=_4t[2],_4v=E(_4t[1]),_4w=_4v[1],_4x=new T(function(){return _y(_M,[1,[0,_q,_4p],_9]);}),_4y=E(_4v[2]);if(!_4y[0]){return [0,[0,function(_4z,_){var _4A=A(_4w,[_4z,_]),_4B=A(_4x,[_4z,_]);return _4z;},_a],new T(function(){var _4C=E(_4u);return [0,_4C[1],_4C[2],_4C[3],new T(function(){return E(E(_4r)[1]);}),_4C[5]];})];}else{var _4D=A(_4l,[_4y[1],new T(function(){var _4E=E(_4u);return [0,_4E[1],_4E[2],_4E[3],new T(function(){return E(E(_4r)[1]);}),_4E[5]];}),_]),_4F=E(_4D),_4G=E(_4F[1]);return [0,[0,function(_4H,_){var _4I=A(_4w,[_4H,_]),_4J=A(_4x,[_4H,_]),_4K=A(_4G[1],[_4J,_]);return _4H;},_4G[2]],_4F[2]];}},_4L=unCStr("padding:15px;border-style:dotted"),_4M=unCStr("vertical-align:top"),_4N=[0,3],_4O=function(_4P,_4Q,_){var _4R=jsCreateTextNode(toJSStr(E(_4P))),_4S=jsAppendChild(_4R,E(_4Q)[1]);return [0,_4R];},_4T=[0,112],_4U=[1,_4T,_9],_4V=function(_4W,_4X){var _4Y=new T(function(){return A(_4W,[_4X]);});return function(_4Z,_){var _50=jsCreateElem(toJSStr(_4U)),_51=jsAppendChild(_50,E(_4Z)[1]),_52=[0,_50],_53=A(_4Y,[_52,_]);return _52;};},_54=function(_55){return _3u(0,E(_55)[1],_9);},_56=[0,98],_57=[1,_56,_9],_58=function(_59,_5a){var _5b=new T(function(){return A(_59,[_5a]);});return function(_5c,_){var _5d=jsCreateElem(toJSStr(_57)),_5e=jsAppendChild(_5d,E(_5c)[1]),_5f=[0,_5d],_5g=A(_5b,[_5f,_]);return _5f;};},_5h=unCStr("br"),_5i=function(_5j,_){var _5k=jsCreateElem(toJSStr(E(_5h))),_5l=jsAppendChild(_5k,E(_5j)[1]);return [0,_5k];},_5m=[1,_0],_5n=unCStr("result: "),_5o=function(_5p){var _5q=new T(function(){return _58(_4O,new T(function(){return _54(_5p);}));});return function(_5r,_){return [0,[0,function(_5s,_){var _5t=_5i(_5s,_),_5u=_4O(_5n,_5s,_),_5v=A(_5q,[_5s,_]);return _5s;},_5m],_5r];};},_5w=unCStr(" numbers and append the result using a fold"),_5x=[0,0],_5y=[1,_5x],_5z=[0,_2M,_5y],_5A=function(_5B,_){return [0,_5z,_5B];},_5C=function(_5D,_5E,_5F,_){var _5G=_H(_5D,_5F,_),_5H=A(_5E,[_5G,_]);return _5G;},_5I=unCStr("()"),_5J=unCStr("GHC.Tuple"),_5K=unCStr("ghc-prim"),_5L=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5K,_5J,_5I],_5M=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5L,_9],_5N=function(_5O){return E(_5M);},_5P=unCStr("main"),_5Q=unCStr("Haste.Perch"),_5R=unCStr("PerchM"),_5S=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_5P,_5Q,_5R],_5T=[0,I_fromBits([2789178401,3929829800]),I_fromBits([1789647524,191521542]),_5S,_9],_5U=function(_5V){return E(_5T);},_5W=function(_5X){var _5Y=E(_5X);return _5Y[0]==0?[0]:_1v(_5Y[1],new T(function(){return _5W(_5Y[2]);}));},_5Z=function(_60,_61){var _62=E(_60);if(!_62){return [0,_9,_61];}else{var _63=E(_61);if(!_63[0]){return [0,_9,_9];}else{var _64=new T(function(){var _65=_5Z(_62-1|0,_63[2]);return [0,_65[1],_65[2]];});return [0,[1,_63[1],new T(function(){return E(E(_64)[1]);})],new T(function(){return E(E(_64)[2]);})];}}},_66=[0,120],_67=[0,48],_68=function(_69){var _6a=new T(function(){var _6b=_5Z(8,new T(function(){var _6c=md5(toJSStr(E(_69)));return fromJSStr(_6c);}));return [0,_6b[1],_6b[2]];}),_6d=parseInt([0,toJSStr([1,_67,[1,_66,new T(function(){return E(E(_6a)[1]);})]])]),_6e=new T(function(){var _6f=_5Z(8,new T(function(){return E(E(_6a)[2]);}));return [0,_6f[1],_6f[2]];}),_6g=parseInt([0,toJSStr([1,_67,[1,_66,new T(function(){return E(E(_6e)[1]);})]])]),_6h=hs_mkWord64(_6d,_6g),_6i=parseInt([0,toJSStr([1,_67,[1,_66,new T(function(){return E(_5Z(8,new T(function(){return E(E(_6e)[2]);}))[1]);})]])]),_6j=hs_mkWord64(_6i,_6i);return [0,_6h,_6j];},_6k=function(_6l,_6m){var _6n=E(_6m);return _6n[0]==0?[0]:[1,new T(function(){return A(_6l,[_6n[1]]);}),new T(function(){return _6k(_6l,_6n[2]);})];},_6o=function(_6p,_6q){var _6r=jsShowI(_6p),_6s=md5(_6r);return _1v(fromJSStr(_6s),new T(function(){var _6t=jsShowI(_6q),_6u=md5(_6t);return fromJSStr(_6u);}));},_6v=function(_6w){var _6x=E(_6w);return _6o(_6x[1],_6x[2]);},_6y=function(_6z){var _6A=E(_6z);if(!_6A[0]){return [0];}else{var _6B=E(_6A[1]);return [1,[0,_6B[1],_6B[2]],new T(function(){return _6y(_6A[2]);})];}},_6C=unCStr("Prelude.undefined"),_6D=new T(function(){return err(_6C);}),_6E=function(_6F,_6G){return function(_6H){return E(new T(function(){var _6I=A(_6F,[_6D]),_6J=E(_6I[3]),_6K=_6J[1],_6L=_6J[2],_6M=_1v(_6I[4],[1,new T(function(){return A(_6G,[_6D]);}),_9]);if(!_6M[0]){return [0,_6K,_6L,_6J,_9];}else{var _6N=_68(new T(function(){return _5W(_6k(_6v,[1,[0,_6K,_6L],new T(function(){return _6y(_6M);})]));}));return [0,_6N[1],_6N[2],_6J,_6M];}}));};},_6O=new T(function(){return _6E(_5U,_5N);}),_6P=unCStr("value"),_6Q=unCStr("onclick"),_6R=unCStr("checked"),_6S=[0,_6R,_9],_6T=[1,_6S,_9],_6U=unCStr("type"),_6V=unCStr("input"),_6W=function(_6X,_){return _H(_6V,_6X,_);},_6Y=function(_6Z,_70,_71,_72,_73){var _74=new T(function(){var _75=new T(function(){return _y(_6W,[1,[0,_6U,_70],[1,[0,_q,_6Z],[1,[0,_6P,_71],_9]]]);});return !E(_72)?E(_75):_y(_75,_6T);}),_76=E(_73);return _76[0]==0?E(_74):_y(_74,[1,[0,_6Q,_76[1]],_9]);},_77=unCStr("href"),_78=[0,97],_79=[1,_78,_9],_7a=function(_7b,_){return _H(_79,_7b,_);},_7c=function(_7d,_7e){var _7f=new T(function(){return _y(_7a,[1,[0,_77,_7d],_9]);});return function(_7g,_){var _7h=A(_7f,[_7g,_]),_7i=A(_7e,[_7h,_]);return _7h;};},_7j=function(_7k){return _7c(_7k,function(_F,_){return _4O(_7k,_F,_);});},_7l=unCStr("option"),_7m=function(_7n,_){return _H(_7l,_7n,_);},_7o=unCStr("selected"),_7p=[0,_7o,_9],_7q=[1,_7p,_9],_7r=function(_7s,_7t,_7u){var _7v=new T(function(){return _y(_7m,[1,[0,_6P,_7s],_9]);}),_7w=function(_7x,_){var _7y=A(_7v,[_7x,_]),_7z=A(_7t,[_7y,_]);return _7y;};return !E(_7u)?E(_7w):_y(_7w,_7q);},_7A=function(_7B,_7C){return _7r(_7B,function(_F,_){return _4O(_7B,_F,_);},_7C);},_7D=unCStr("method"),_7E=unCStr("action"),_7F=unCStr("UTF-8"),_7G=unCStr("acceptCharset"),_7H=[0,_7G,_7F],_7I=unCStr("form"),_7J=function(_7K,_){return _H(_7I,_7K,_);},_7L=function(_7M,_7N,_7O){var _7P=new T(function(){return _y(_7J,[1,_7H,[1,[0,_7E,_7M],[1,[0,_7D,_7N],_9]]]);});return function(_7Q,_){var _7R=A(_7P,[_7Q,_]),_7S=A(_7O,[_7R,_]);return _7R;};},_7T=unCStr("select"),_7U=function(_7V,_){return _H(_7T,_7V,_);},_7W=function(_7X,_7Y){var _7Z=new T(function(){return _y(_7U,[1,[0,_q,_7X],_9]);});return function(_80,_){var _81=A(_7Z,[_80,_]),_82=A(_7Y,[_81,_]);return _81;};},_83=unCStr("textarea"),_84=function(_85,_){return _H(_83,_85,_);},_86=function(_87,_88){var _89=new T(function(){return _y(_84,[1,[0,_q,_87],_9]);});return function(_8a,_){var _8b=A(_89,[_8a,_]),_8c=_4O(_88,_8b,_);return _8b;};},_8d=unCStr("color:red"),_8e=unCStr("style"),_8f=[0,_8e,_8d],_8g=[1,_8f,_9],_8h=[0,98],_8i=[1,_8h,_9],_8j=function(_8k){return _y(function(_8l,_){var _8m=_H(_8i,_8l,_),_8n=A(_8k,[_8m,_]);return _8m;},_8g);},_8o=function(_8p,_8q,_){var _8r=E(_8p);if(!_8r[0]){return _8q;}else{var _8s=A(_8r[1],[_8q,_]),_8t=_8o(_8r[2],_8q,_);return _8q;}},_8u=function(_8v,_8w,_8x,_){var _8y=A(_8v,[_8x,_]),_8z=A(_8w,[_8x,_]);return _8x;},_8A=[0,_2M,_8u,_8o],_8B=[0,_8A,_6O,_4O,_4O,_5C,_8j,_7c,_7j,_6Y,_86,_7W,_7r,_7A,_7L,_y],_8C=function(_8D,_8E,_){var _8F=A(_8E,[_]);return _8D;},_8G=function(_8H,_8I,_){var _8J=A(_8I,[_]);return new T(function(){return A(_8H,[_8J]);});},_8K=[0,_8G,_8C],_8L=function(_8M){var _8N=E(_8M);return _8N[0]==0?0:E(_8N[1])[1]+_8L(_8N[2])|0;},_8O=function(_8P){return [0,_8L(_8P)];},_8Q=function(_8R,_8S){return [0,E(_8R)[1]+E(_8S)[1]|0];},_8T=[0,_5x,_8Q,_8O],_8U=function(_8V,_8W){var _8X=E(_8W);return _8X[0]==0?[0]:[1,new T(function(){return A(_8V,[_8X[1]]);})];},_8Y=function(_8Z){return E(E(_8Z)[1]);},_90=function(_91){return E(E(_91)[2]);},_92=function(_93,_94,_95,_96,_97,_98){var _99=new T(function(){return _90(_93);});return A(_94,[new T(function(){return A(_96,[_98]);}),function(_9a){var _9b=E(_9a),_9c=E(_9b[1]);return A(_94,[new T(function(){return A(_97,[_9b[2]]);}),function(_9d){var _9e=E(_9d),_9f=E(_9e[1]);return A(_95,[[0,[0,new T(function(){return A(_99,[_9c[1],_9f[1]]);}),new T(function(){var _9g=E(_9c[2]);if(!_9g[0]){return [0];}else{var _9h=E(_9f[2]);return _9h[0]==0?[0]:[1,new T(function(){return A(_9g[1],[_9h[1]]);})];}})],_9e[2]]]);}]);}]);},_9i=function(_9j){return E(E(_9j)[1]);},_9k=function(_9l,_9m,_9n,_9o,_9p,_9q){var _9r=new T(function(){return _8Y(_9l);});return function(_9s){var _9t=E(_9m);return _92(_9r,_9t[1],_9t[3],function(_9u){return A(new T(function(){var _9v=new T(function(){return _90(_9o);});return A(_9i,[_9n,function(_9w){return [0,new T(function(){var _9x=E(E(_9w)[1]);return [0,_9x[1],new T(function(){return _8U(_9v,_9x[2]);})];}),new T(function(){return E(E(_9w)[2]);})];}]);}),[new T(function(){return A(_9p,[_9u]);})]);},_9q,_9s);};},_9y=function(_9z,_9A){while(1){var _9B=(function(_9C,_9D){var _9E=E(_9D);if(!_9E[0]){return E(_9C);}else{_9z=new T(function(){return _9k(_8B,_2O,_8K,_8T,_9C,_9E[1]);});_9A=_9E[2];return null;}})(_9z,_9A);if(_9B!=null){return _9B;}}},_9F=[13,coercionToken],_9G=unCStr("text"),_9H=[0,_2O,_n],_9I=unCStr("base"),_9J=unCStr("Control.Exception.Base"),_9K=unCStr("PatternMatchFail"),_9L=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9I,_9J,_9K],_9M=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_9L,_9],_9N=function(_9O){return E(_9M);},_9P=function(_9Q){var _9R=E(_9Q);return _1f(_1b(_9R[1]),_9N,_9R[2]);},_9S=function(_9T){return E(E(_9T)[1]);},_9U=function(_9V,_9W){return _1v(E(_9V)[1],_9W);},_9X=function(_9Y,_9Z){return _2l(_9U,_9Y,_9Z);},_a0=function(_a1,_a2,_a3){return _1v(E(_a2)[1],_a3);},_a4=[0,_a0,_9S,_9X],_a5=new T(function(){return [0,_9N,_a4,_a6,_9P];}),_a6=function(_a7){return [0,_a5,_a7];},_a8=unCStr("Non-exhaustive patterns in"),_a9=function(_aa,_ab){return die(new T(function(){return A(_ab,[_aa]);}));},_ac=function(_ad,_ae){var _af=E(_ae);if(!_af[0]){return [0,_9,_9];}else{var _ag=_af[1];if(!A(_ad,[_ag])){return [0,_9,_af];}else{var _ah=new T(function(){var _ai=_ac(_ad,_af[2]);return [0,_ai[1],_ai[2]];});return [0,[1,_ag,new T(function(){return E(E(_ah)[1]);})],new T(function(){return E(E(_ah)[2]);})];}}},_aj=[0,32],_ak=[0,10],_al=[1,_ak,_9],_am=function(_an){return E(E(_an)[1])==124?false:true;},_ao=function(_ap,_aq){var _ar=_ac(_am,unCStr(_ap)),_as=_ar[1],_at=function(_au,_av){return _1v(_au,new T(function(){return unAppCStr(": ",new T(function(){return _1v(_aq,new T(function(){return _1v(_av,_al);}));}));}));},_aw=E(_ar[2]);return _aw[0]==0?_at(_as,_9):E(E(_aw[1])[1])==124?_at(_as,[1,_aj,_aw[2]]):_at(_as,_9);},_ax=function(_ay){return _a9([0,new T(function(){return _ao(_ay,_a8);})],_a6);},_az=new T(function(){return _ax("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_aA=function(_aB,_aC){while(1){var _aD=(function(_aE,_aF){var _aG=E(_aE);switch(_aG[0]){case 0:var _aH=E(_aF);if(!_aH[0]){return [0];}else{_aB=A(_aG[1],[_aH[1]]);_aC=_aH[2];return null;}break;case 1:var _aI=A(_aG[1],[_aF]),_aJ=_aF;_aB=_aI;_aC=_aJ;return null;case 2:return [0];case 3:return [1,[0,_aG[1],_aF],new T(function(){return _aA(_aG[2],_aF);})];default:return E(_aG[1]);}})(_aB,_aC);if(_aD!=null){return _aD;}}},_aK=function(_aL,_aM){var _aN=new T(function(){var _aO=E(_aM);if(_aO[0]==3){return [3,_aO[1],new T(function(){return _aK(_aL,_aO[2]);})];}else{var _aP=E(_aL);if(_aP[0]==2){return E(_aO);}else{var _aQ=E(_aO);if(_aQ[0]==2){return E(_aP);}else{var _aR=new T(function(){var _aS=E(_aQ);if(_aS[0]==4){return [1,function(_aT){return [4,new T(function(){return _1v(_aA(_aP,_aT),_aS[1]);})];}];}else{var _aU=E(_aP);if(_aU[0]==1){var _aV=_aU[1],_aW=E(_aS);return _aW[0]==0?[1,function(_aX){return _aK(A(_aV,[_aX]),_aW);}]:[1,function(_aY){return _aK(A(_aV,[_aY]),new T(function(){return A(_aW[1],[_aY]);}));}];}else{var _aZ=E(_aS);return _aZ[0]==0?E(_az):[1,function(_b0){return _aK(_aU,new T(function(){return A(_aZ[1],[_b0]);}));}];}}}),_b1=E(_aP);switch(_b1[0]){case 1:var _b2=E(_aQ);return _b2[0]==4?[1,function(_b3){return [4,new T(function(){return _1v(_aA(A(_b1[1],[_b3]),_b3),_b2[1]);})];}]:E(_aR);case 4:var _b4=_b1[1],_b5=E(_aQ);switch(_b5[0]){case 0:return [1,function(_b6){return [4,new T(function(){return _1v(_b4,new T(function(){return _aA(_b5,_b6);}));})];}];case 1:return [1,function(_b7){return [4,new T(function(){return _1v(_b4,new T(function(){return _aA(A(_b5[1],[_b7]),_b7);}));})];}];default:return [4,new T(function(){return _1v(_b4,_b5[1]);})];}break;default:return E(_aR);}}}}}),_b8=E(_aL);switch(_b8[0]){case 0:var _b9=E(_aM);return _b9[0]==0?[0,function(_ba){return _aK(A(_b8[1],[_ba]),new T(function(){return A(_b9[1],[_ba]);}));}]:E(_aN);case 3:return [3,_b8[1],new T(function(){return _aK(_b8[2],_aM);})];default:return E(_aN);}},_bb=function(_bc,_bd){return E(_bc)[1]!=E(_bd)[1];},_be=function(_bf,_bg){return E(_bf)[1]==E(_bg)[1];},_bh=[0,_be,_bb],_bi=function(_bj){return E(E(_bj)[1]);},_bk=function(_bl,_bm,_bn){while(1){var _bo=E(_bm);if(!_bo[0]){return E(_bn)[0]==0?true:false;}else{var _bp=E(_bn);if(!_bp[0]){return false;}else{if(!A(_bi,[_bl,_bo[1],_bp[1]])){return false;}else{_bm=_bo[2];_bn=_bp[2];continue;}}}}},_bq=function(_br,_bs,_bt){return !_bk(_br,_bs,_bt)?true:false;},_bu=function(_bv){return [0,function(_bw,_bx){return _bk(_bv,_bw,_bx);},function(_bw,_bx){return _bq(_bv,_bw,_bx);}];},_by=new T(function(){return _bu(_bh);}),_bz=function(_bA,_bB){var _bC=E(_bA);switch(_bC[0]){case 0:return [0,function(_bD){return _bz(A(_bC[1],[_bD]),_bB);}];case 1:return [1,function(_bE){return _bz(A(_bC[1],[_bE]),_bB);}];case 2:return [2];case 3:return _aK(A(_bB,[_bC[1]]),new T(function(){return _bz(_bC[2],_bB);}));default:var _bF=function(_bG){var _bH=E(_bG);if(!_bH[0]){return [0];}else{var _bI=E(_bH[1]);return _1v(_aA(A(_bB,[_bI[1]]),_bI[2]),new T(function(){return _bF(_bH[2]);}));}},_bJ=_bF(_bC[1]);return _bJ[0]==0?[2]:[4,_bJ];}},_bK=[2],_bL=function(_bM){return [3,_bM,_bK];},_bN=function(_bO,_bP){var _bQ=E(_bO);if(!_bQ){return A(_bP,[_0]);}else{var _bR=new T(function(){return _bN(_bQ-1|0,_bP);});return [0,function(_bS){return E(_bR);}];}},_bT=function(_bU,_bV,_bW){var _bX=new T(function(){return A(_bU,[_bL]);});return [1,function(_bY){return A(function(_bZ,_c0,_c1){while(1){var _c2=(function(_c3,_c4,_c5){var _c6=E(_c3);switch(_c6[0]){case 0:var _c7=E(_c4);if(!_c7[0]){return E(_bV);}else{_bZ=A(_c6[1],[_c7[1]]);_c0=_c7[2];var _c8=_c5+1|0;_c1=_c8;return null;}break;case 1:var _c9=A(_c6[1],[_c4]),_ca=_c4,_c8=_c5;_bZ=_c9;_c0=_ca;_c1=_c8;return null;case 2:return E(_bV);case 3:return function(_cb){var _cc=new T(function(){return _bz(_c6,_cb);});return _bN(_c5,function(_cd){return E(_cc);});};default:return function(_ce){return _bz(_c6,_ce);};}})(_bZ,_c0,_c1);if(_c2!=null){return _c2;}}},[_bX,_bY,0,_bW]);}];},_cf=[6],_cg=unCStr("valDig: Bad base"),_ch=new T(function(){return err(_cg);}),_ci=function(_cj,_ck){var _cl=function(_cm,_cn){var _co=E(_cm);if(!_co[0]){var _cp=new T(function(){return A(_cn,[_9]);});return function(_cq){return A(_cq,[_cp]);};}else{var _cr=E(_co[1])[1],_cs=function(_ct){var _cu=new T(function(){return _cl(_co[2],function(_cv){return A(_cn,[[1,_ct,_cv]]);});});return function(_cw){var _cx=new T(function(){return A(_cu,[_cw]);});return [0,function(_cy){return E(_cx);}];};};switch(E(E(_cj)[1])){case 8:if(48>_cr){var _cz=new T(function(){return A(_cn,[_9]);});return function(_cA){return A(_cA,[_cz]);};}else{if(_cr>55){var _cB=new T(function(){return A(_cn,[_9]);});return function(_cC){return A(_cC,[_cB]);};}else{return _cs([0,_cr-48|0]);}}break;case 10:if(48>_cr){var _cD=new T(function(){return A(_cn,[_9]);});return function(_cE){return A(_cE,[_cD]);};}else{if(_cr>57){var _cF=new T(function(){return A(_cn,[_9]);});return function(_cG){return A(_cG,[_cF]);};}else{return _cs([0,_cr-48|0]);}}break;case 16:var _cH=new T(function(){return 97>_cr?65>_cr?[0]:_cr>70?[0]:[1,[0,(_cr-65|0)+10|0]]:_cr>102?65>_cr?[0]:_cr>70?[0]:[1,[0,(_cr-65|0)+10|0]]:[1,[0,(_cr-97|0)+10|0]];});if(48>_cr){var _cI=E(_cH);if(!_cI[0]){var _cJ=new T(function(){return A(_cn,[_9]);});return function(_cK){return A(_cK,[_cJ]);};}else{return _cs(_cI[1]);}}else{if(_cr>57){var _cL=E(_cH);if(!_cL[0]){var _cM=new T(function(){return A(_cn,[_9]);});return function(_cN){return A(_cN,[_cM]);};}else{return _cs(_cL[1]);}}else{return _cs([0,_cr-48|0]);}}break;default:return E(_ch);}}};return [1,function(_cO){return A(_cl,[_cO,_n,function(_cP){var _cQ=E(_cP);return _cQ[0]==0?[2]:A(_ck,[_cQ]);}]);}];},_cR=[0,10],_cS=[0,1],_cT=[0,2147483647],_cU=function(_cV,_cW){while(1){var _cX=E(_cV);if(!_cX[0]){var _cY=_cX[1],_cZ=E(_cW);if(!_cZ[0]){var _d0=_cZ[1],_d1=addC(_cY,_d0);if(!E(_d1[2])){return [0,_d1[1]];}else{_cV=[1,I_fromInt(_cY)];_cW=[1,I_fromInt(_d0)];continue;}}else{_cV=[1,I_fromInt(_cY)];_cW=_cZ;continue;}}else{var _d2=E(_cW);if(!_d2[0]){_cV=_cX;_cW=[1,I_fromInt(_d2[1])];continue;}else{return [1,I_add(_cX[1],_d2[1])];}}}},_d3=new T(function(){return _cU(_cT,_cS);}),_d4=function(_d5){var _d6=E(_d5);if(!_d6[0]){var _d7=E(_d6[1]);return _d7==(-2147483648)?E(_d3):[0, -_d7];}else{return [1,I_negate(_d6[1])];}},_d8=[0,10],_d9=[0,0],_da=function(_db,_dc){while(1){var _dd=E(_db);if(!_dd[0]){var _de=_dd[1],_df=E(_dc);if(!_df[0]){var _dg=_df[1];if(!(imul(_de,_dg)|0)){return [0,imul(_de,_dg)|0];}else{_db=[1,I_fromInt(_de)];_dc=[1,I_fromInt(_dg)];continue;}}else{_db=[1,I_fromInt(_de)];_dc=_df;continue;}}else{var _dh=E(_dc);if(!_dh[0]){_db=_dd;_dc=[1,I_fromInt(_dh[1])];continue;}else{return [1,I_mul(_dd[1],_dh[1])];}}}},_di=function(_dj,_dk,_dl){while(1){var _dm=E(_dl);if(!_dm[0]){return E(_dk);}else{var _dn=_cU(_da(_dk,_dj),_dm[1]);_dl=_dm[2];_dk=_dn;continue;}}},_do=function(_dp){var _dq=new T(function(){return _aK(_aK([0,function(_dr){return E(E(_dr)[1])==45?_ci(_cR,function(_ds){return A(_dp,[[1,new T(function(){return _d4(_di(_d8,_d9,_ds));})]]);}):[2];}],[0,function(_dt){return E(E(_dt)[1])==43?_ci(_cR,function(_du){return A(_dp,[[1,new T(function(){return _di(_d8,_d9,_du);})]]);}):[2];}]),new T(function(){return _ci(_cR,function(_dv){return A(_dp,[[1,new T(function(){return _di(_d8,_d9,_dv);})]]);});}));});return _aK([0,function(_dw){return E(E(_dw)[1])==101?E(_dq):[2];}],[0,function(_dx){return E(E(_dx)[1])==69?E(_dq):[2];}]);},_dy=function(_dz){return A(_dz,[_a]);},_dA=function(_dB){return A(_dB,[_a]);},_dC=function(_dD){var _dE=new T(function(){return _ci(_cR,function(_dF){return A(_dD,[[1,_dF]]);});});return [0,function(_dG){return E(E(_dG)[1])==46?E(_dE):[2];}];},_dH=function(_dI){return _ci(_cR,function(_dJ){return _bT(_dC,_dy,function(_dK){return _bT(_do,_dA,function(_dL){return A(_dI,[[5,[1,_dJ,_dK,_dL]]]);});});});},_dM=function(_dN,_dO,_dP){while(1){var _dQ=E(_dP);if(!_dQ[0]){return false;}else{if(!A(_bi,[_dN,_dO,_dQ[1]])){_dP=_dQ[2];continue;}else{return true;}}}},_dR=unCStr("!@#$%&*+./<=>?\\^|:-~"),_dS=function(_dT){return _dM(_bh,_dT,_dR);},_dU=[0,8],_dV=[0,16],_dW=function(_dX){var _dY=new T(function(){return _ci(_dV,function(_dZ){return A(_dX,[[5,[0,_dV,_dZ]]]);});}),_e0=new T(function(){return _ci(_dU,function(_e1){return A(_dX,[[5,[0,_dU,_e1]]]);});}),_e2=new T(function(){return _ci(_dV,function(_e3){return A(_dX,[[5,[0,_dV,_e3]]]);});}),_e4=new T(function(){return _ci(_dU,function(_e5){return A(_dX,[[5,[0,_dU,_e5]]]);});});return [0,function(_e6){return E(E(_e6)[1])==48?E([0,function(_e7){switch(E(E(_e7)[1])){case 79:return E(_e4);case 88:return E(_e2);case 111:return E(_e0);case 120:return E(_dY);default:return [2];}}]):[2];}];},_e8=false,_e9=true,_ea=function(_eb){var _ec=new T(function(){return A(_eb,[_dV]);}),_ed=new T(function(){return A(_eb,[_dU]);}),_ee=new T(function(){return A(_eb,[_dV]);}),_ef=new T(function(){return A(_eb,[_dU]);});return [0,function(_eg){switch(E(E(_eg)[1])){case 79:return E(_ef);case 88:return E(_ee);case 111:return E(_ed);case 120:return E(_ec);default:return [2];}}];},_eh=function(_ei){return A(_ei,[_cR]);},_ej=function(_ek){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _3u(9,_ek,_9);})));},_el=function(_em){var _en=E(_em);return _en[0]==0?E(_en[1]):I_toInt(_en[1]);},_eo=function(_ep,_eq){var _er=E(_ep);if(!_er[0]){var _es=_er[1],_et=E(_eq);return _et[0]==0?_es<=_et[1]:I_compareInt(_et[1],_es)>=0;}else{var _eu=_er[1],_ev=E(_eq);return _ev[0]==0?I_compareInt(_eu,_ev[1])<=0:I_compare(_eu,_ev[1])<=0;}},_ew=function(_ex){return [2];},_ey=function(_ez){var _eA=E(_ez);if(!_eA[0]){return E(_ew);}else{var _eB=_eA[1],_eC=E(_eA[2]);if(!_eC[0]){return E(_eB);}else{var _eD=new T(function(){return _ey(_eC);});return function(_eE){return _aK(A(_eB,[_eE]),new T(function(){return A(_eD,[_eE]);}));};}}},_eF=unCStr("NUL"),_eG=function(_eH){return [2];},_eI=function(_eJ){return _eG(_eJ);},_eK=function(_eL,_eM){var _eN=function(_eO,_eP){var _eQ=E(_eO);if(!_eQ[0]){return function(_eR){return A(_eR,[_eL]);};}else{var _eS=E(_eP);if(!_eS[0]){return E(_eG);}else{if(E(_eQ[1])[1]!=E(_eS[1])[1]){return E(_eI);}else{var _eT=new T(function(){return _eN(_eQ[2],_eS[2]);});return function(_eU){var _eV=new T(function(){return A(_eT,[_eU]);});return [0,function(_eW){return E(_eV);}];};}}}};return [1,function(_eX){return A(_eN,[_eL,_eX,_eM]);}];},_eY=[0,0],_eZ=function(_f0){var _f1=new T(function(){return A(_f0,[_eY]);});return _eK(_eF,function(_f2){return E(_f1);});},_f3=unCStr("STX"),_f4=[0,2],_f5=function(_f6){var _f7=new T(function(){return A(_f6,[_f4]);});return _eK(_f3,function(_f8){return E(_f7);});},_f9=unCStr("ETX"),_fa=[0,3],_fb=function(_fc){var _fd=new T(function(){return A(_fc,[_fa]);});return _eK(_f9,function(_fe){return E(_fd);});},_ff=unCStr("EOT"),_fg=[0,4],_fh=function(_fi){var _fj=new T(function(){return A(_fi,[_fg]);});return _eK(_ff,function(_fk){return E(_fj);});},_fl=unCStr("ENQ"),_fm=[0,5],_fn=function(_fo){var _fp=new T(function(){return A(_fo,[_fm]);});return _eK(_fl,function(_fq){return E(_fp);});},_fr=unCStr("ACK"),_fs=[0,6],_ft=function(_fu){var _fv=new T(function(){return A(_fu,[_fs]);});return _eK(_fr,function(_fw){return E(_fv);});},_fx=unCStr("BEL"),_fy=[0,7],_fz=function(_fA){var _fB=new T(function(){return A(_fA,[_fy]);});return _eK(_fx,function(_fC){return E(_fB);});},_fD=unCStr("BS"),_fE=[0,8],_fF=function(_fG){var _fH=new T(function(){return A(_fG,[_fE]);});return _eK(_fD,function(_fI){return E(_fH);});},_fJ=unCStr("HT"),_fK=[0,9],_fL=function(_fM){var _fN=new T(function(){return A(_fM,[_fK]);});return _eK(_fJ,function(_fO){return E(_fN);});},_fP=unCStr("LF"),_fQ=[0,10],_fR=function(_fS){var _fT=new T(function(){return A(_fS,[_fQ]);});return _eK(_fP,function(_fU){return E(_fT);});},_fV=unCStr("VT"),_fW=[0,11],_fX=function(_fY){var _fZ=new T(function(){return A(_fY,[_fW]);});return _eK(_fV,function(_g0){return E(_fZ);});},_g1=unCStr("FF"),_g2=[0,12],_g3=function(_g4){var _g5=new T(function(){return A(_g4,[_g2]);});return _eK(_g1,function(_g6){return E(_g5);});},_g7=unCStr("CR"),_g8=[0,13],_g9=function(_ga){var _gb=new T(function(){return A(_ga,[_g8]);});return _eK(_g7,function(_gc){return E(_gb);});},_gd=unCStr("SI"),_ge=[0,15],_gf=function(_gg){var _gh=new T(function(){return A(_gg,[_ge]);});return _eK(_gd,function(_gi){return E(_gh);});},_gj=unCStr("DLE"),_gk=[0,16],_gl=function(_gm){var _gn=new T(function(){return A(_gm,[_gk]);});return _eK(_gj,function(_go){return E(_gn);});},_gp=unCStr("DC1"),_gq=[0,17],_gr=function(_gs){var _gt=new T(function(){return A(_gs,[_gq]);});return _eK(_gp,function(_gu){return E(_gt);});},_gv=unCStr("DC2"),_gw=[0,18],_gx=function(_gy){var _gz=new T(function(){return A(_gy,[_gw]);});return _eK(_gv,function(_gA){return E(_gz);});},_gB=unCStr("DC3"),_gC=[0,19],_gD=function(_gE){var _gF=new T(function(){return A(_gE,[_gC]);});return _eK(_gB,function(_gG){return E(_gF);});},_gH=unCStr("DC4"),_gI=[0,20],_gJ=function(_gK){var _gL=new T(function(){return A(_gK,[_gI]);});return _eK(_gH,function(_gM){return E(_gL);});},_gN=unCStr("NAK"),_gO=[0,21],_gP=function(_gQ){var _gR=new T(function(){return A(_gQ,[_gO]);});return _eK(_gN,function(_gS){return E(_gR);});},_gT=unCStr("SYN"),_gU=[0,22],_gV=function(_gW){var _gX=new T(function(){return A(_gW,[_gU]);});return _eK(_gT,function(_gY){return E(_gX);});},_gZ=unCStr("ETB"),_h0=[0,23],_h1=function(_h2){var _h3=new T(function(){return A(_h2,[_h0]);});return _eK(_gZ,function(_h4){return E(_h3);});},_h5=unCStr("CAN"),_h6=[0,24],_h7=function(_h8){var _h9=new T(function(){return A(_h8,[_h6]);});return _eK(_h5,function(_ha){return E(_h9);});},_hb=unCStr("EM"),_hc=[0,25],_hd=function(_he){var _hf=new T(function(){return A(_he,[_hc]);});return _eK(_hb,function(_hg){return E(_hf);});},_hh=unCStr("SUB"),_hi=[0,26],_hj=function(_hk){var _hl=new T(function(){return A(_hk,[_hi]);});return _eK(_hh,function(_hm){return E(_hl);});},_hn=unCStr("ESC"),_ho=[0,27],_hp=function(_hq){var _hr=new T(function(){return A(_hq,[_ho]);});return _eK(_hn,function(_hs){return E(_hr);});},_ht=unCStr("FS"),_hu=[0,28],_hv=function(_hw){var _hx=new T(function(){return A(_hw,[_hu]);});return _eK(_ht,function(_hy){return E(_hx);});},_hz=unCStr("GS"),_hA=[0,29],_hB=function(_hC){var _hD=new T(function(){return A(_hC,[_hA]);});return _eK(_hz,function(_hE){return E(_hD);});},_hF=unCStr("RS"),_hG=[0,30],_hH=function(_hI){var _hJ=new T(function(){return A(_hI,[_hG]);});return _eK(_hF,function(_hK){return E(_hJ);});},_hL=unCStr("US"),_hM=[0,31],_hN=function(_hO){var _hP=new T(function(){return A(_hO,[_hM]);});return _eK(_hL,function(_hQ){return E(_hP);});},_hR=unCStr("SP"),_hS=[0,32],_hT=function(_hU){var _hV=new T(function(){return A(_hU,[_hS]);});return _eK(_hR,function(_hW){return E(_hV);});},_hX=unCStr("DEL"),_hY=[0,127],_hZ=function(_i0){var _i1=new T(function(){return A(_i0,[_hY]);});return _eK(_hX,function(_i2){return E(_i1);});},_i3=[1,_hZ,_9],_i4=[1,_hT,_i3],_i5=[1,_hN,_i4],_i6=[1,_hH,_i5],_i7=[1,_hB,_i6],_i8=[1,_hv,_i7],_i9=[1,_hp,_i8],_ia=[1,_hj,_i9],_ib=[1,_hd,_ia],_ic=[1,_h7,_ib],_id=[1,_h1,_ic],_ie=[1,_gV,_id],_if=[1,_gP,_ie],_ig=[1,_gJ,_if],_ih=[1,_gD,_ig],_ii=[1,_gx,_ih],_ij=[1,_gr,_ii],_ik=[1,_gl,_ij],_il=[1,_gf,_ik],_im=[1,_g9,_il],_in=[1,_g3,_im],_io=[1,_fX,_in],_ip=[1,_fR,_io],_iq=[1,_fL,_ip],_ir=[1,_fF,_iq],_is=[1,_fz,_ir],_it=[1,_ft,_is],_iu=[1,_fn,_it],_iv=[1,_fh,_iu],_iw=[1,_fb,_iv],_ix=[1,_f5,_iw],_iy=[1,_eZ,_ix],_iz=unCStr("SOH"),_iA=[0,1],_iB=function(_iC){var _iD=new T(function(){return A(_iC,[_iA]);});return _eK(_iz,function(_iE){return E(_iD);});},_iF=unCStr("SO"),_iG=[0,14],_iH=function(_iI){var _iJ=new T(function(){return A(_iI,[_iG]);});return _eK(_iF,function(_iK){return E(_iJ);});},_iL=function(_iM){return _bT(_iB,_iH,_iM);},_iN=[1,_iL,_iy],_iO=new T(function(){return _ey(_iN);}),_iP=[0,1114111],_iQ=[0,34],_iR=[0,_iQ,_e9],_iS=[0,39],_iT=[0,_iS,_e9],_iU=[0,92],_iV=[0,_iU,_e9],_iW=[0,_fy,_e9],_iX=[0,_fE,_e9],_iY=[0,_g2,_e9],_iZ=[0,_fQ,_e9],_j0=[0,_g8,_e9],_j1=[0,_fK,_e9],_j2=[0,_fW,_e9],_j3=[0,_eY,_e9],_j4=[0,_iA,_e9],_j5=[0,_f4,_e9],_j6=[0,_fa,_e9],_j7=[0,_fg,_e9],_j8=[0,_fm,_e9],_j9=[0,_fs,_e9],_ja=[0,_fy,_e9],_jb=[0,_fE,_e9],_jc=[0,_fK,_e9],_jd=[0,_fQ,_e9],_je=[0,_fW,_e9],_jf=[0,_g2,_e9],_jg=[0,_g8,_e9],_jh=[0,_iG,_e9],_ji=[0,_ge,_e9],_jj=[0,_gk,_e9],_jk=[0,_gq,_e9],_jl=[0,_gw,_e9],_jm=[0,_gC,_e9],_jn=[0,_gI,_e9],_jo=[0,_gO,_e9],_jp=[0,_gU,_e9],_jq=[0,_h0,_e9],_jr=[0,_h6,_e9],_js=[0,_hc,_e9],_jt=[0,_hi,_e9],_ju=[0,_ho,_e9],_jv=[0,_hu,_e9],_jw=[0,_hA,_e9],_jx=[0,_hG,_e9],_jy=[0,_hM,_e9],_jz=function(_jA){return [0,_jA];},_jB=function(_jC){var _jD=new T(function(){return A(_jC,[_j2]);}),_jE=new T(function(){return A(_jC,[_j1]);}),_jF=new T(function(){return A(_jC,[_j0]);}),_jG=new T(function(){return A(_jC,[_iZ]);}),_jH=new T(function(){return A(_jC,[_iY]);}),_jI=new T(function(){return A(_jC,[_iX]);}),_jJ=new T(function(){return A(_jC,[_iW]);}),_jK=new T(function(){return A(_jC,[_iV]);}),_jL=new T(function(){return A(_jC,[_iT]);}),_jM=new T(function(){return A(_jC,[_iR]);});return _aK([0,function(_jN){switch(E(E(_jN)[1])){case 34:return E(_jM);case 39:return E(_jL);case 92:return E(_jK);case 97:return E(_jJ);case 98:return E(_jI);case 102:return E(_jH);case 110:return E(_jG);case 114:return E(_jF);case 116:return E(_jE);case 118:return E(_jD);default:return [2];}}],new T(function(){return _aK(_bT(_ea,_eh,function(_jO){var _jP=new T(function(){return _jz(E(_jO)[1]);});return _ci(_jO,function(_jQ){var _jR=_di(_jP,_d9,_jQ);return !_eo(_jR,_iP)?[2]:A(_jC,[[0,new T(function(){var _jS=_el(_jR);return _jS>>>0>1114111?_ej(_jS):[0,_jS];}),_e9]]);});}),new T(function(){var _jT=new T(function(){return A(_jC,[_jy]);}),_jU=new T(function(){return A(_jC,[_jx]);}),_jV=new T(function(){return A(_jC,[_jw]);}),_jW=new T(function(){return A(_jC,[_jv]);}),_jX=new T(function(){return A(_jC,[_ju]);}),_jY=new T(function(){return A(_jC,[_jt]);}),_jZ=new T(function(){return A(_jC,[_js]);}),_k0=new T(function(){return A(_jC,[_jr]);}),_k1=new T(function(){return A(_jC,[_jq]);}),_k2=new T(function(){return A(_jC,[_jp]);}),_k3=new T(function(){return A(_jC,[_jo]);}),_k4=new T(function(){return A(_jC,[_jn]);}),_k5=new T(function(){return A(_jC,[_jm]);}),_k6=new T(function(){return A(_jC,[_jl]);}),_k7=new T(function(){return A(_jC,[_jk]);}),_k8=new T(function(){return A(_jC,[_jj]);}),_k9=new T(function(){return A(_jC,[_ji]);}),_ka=new T(function(){return A(_jC,[_jh]);}),_kb=new T(function(){return A(_jC,[_jg]);}),_kc=new T(function(){return A(_jC,[_jf]);}),_kd=new T(function(){return A(_jC,[_je]);}),_ke=new T(function(){return A(_jC,[_jd]);}),_kf=new T(function(){return A(_jC,[_jc]);}),_kg=new T(function(){return A(_jC,[_jb]);}),_kh=new T(function(){return A(_jC,[_ja]);}),_ki=new T(function(){return A(_jC,[_j9]);}),_kj=new T(function(){return A(_jC,[_j8]);}),_kk=new T(function(){return A(_jC,[_j7]);}),_kl=new T(function(){return A(_jC,[_j6]);}),_km=new T(function(){return A(_jC,[_j5]);}),_kn=new T(function(){return A(_jC,[_j4]);}),_ko=new T(function(){return A(_jC,[_j3]);});return _aK([0,function(_kp){return E(E(_kp)[1])==94?E([0,function(_kq){switch(E(E(_kq)[1])){case 64:return E(_ko);case 65:return E(_kn);case 66:return E(_km);case 67:return E(_kl);case 68:return E(_kk);case 69:return E(_kj);case 70:return E(_ki);case 71:return E(_kh);case 72:return E(_kg);case 73:return E(_kf);case 74:return E(_ke);case 75:return E(_kd);case 76:return E(_kc);case 77:return E(_kb);case 78:return E(_ka);case 79:return E(_k9);case 80:return E(_k8);case 81:return E(_k7);case 82:return E(_k6);case 83:return E(_k5);case 84:return E(_k4);case 85:return E(_k3);case 86:return E(_k2);case 87:return E(_k1);case 88:return E(_k0);case 89:return E(_jZ);case 90:return E(_jY);case 91:return E(_jX);case 92:return E(_jW);case 93:return E(_jV);case 94:return E(_jU);case 95:return E(_jT);default:return [2];}}]):[2];}],new T(function(){return A(_iO,[function(_kr){return A(_jC,[[0,_kr,_e9]]);}]);}));}));}));},_ks=function(_kt){return A(_kt,[_0]);},_ku=function(_kv){var _kw=E(_kv);if(!_kw[0]){return E(_ks);}else{var _kx=_kw[2],_ky=E(E(_kw[1])[1]);switch(_ky){case 9:var _kz=new T(function(){return _ku(_kx);});return function(_kA){var _kB=new T(function(){return A(_kz,[_kA]);});return [0,function(_kC){return E(_kB);}];};case 10:var _kD=new T(function(){return _ku(_kx);});return function(_kE){var _kF=new T(function(){return A(_kD,[_kE]);});return [0,function(_kG){return E(_kF);}];};case 11:var _kH=new T(function(){return _ku(_kx);});return function(_kI){var _kJ=new T(function(){return A(_kH,[_kI]);});return [0,function(_kK){return E(_kJ);}];};case 12:var _kL=new T(function(){return _ku(_kx);});return function(_kM){var _kN=new T(function(){return A(_kL,[_kM]);});return [0,function(_kO){return E(_kN);}];};case 13:var _kP=new T(function(){return _ku(_kx);});return function(_kQ){var _kR=new T(function(){return A(_kP,[_kQ]);});return [0,function(_kS){return E(_kR);}];};case 32:var _kT=new T(function(){return _ku(_kx);});return function(_kU){var _kV=new T(function(){return A(_kT,[_kU]);});return [0,function(_kW){return E(_kV);}];};case 160:var _kX=new T(function(){return _ku(_kx);});return function(_kY){var _kZ=new T(function(){return A(_kX,[_kY]);});return [0,function(_l0){return E(_kZ);}];};default:var _l1=u_iswspace(_ky);if(!E(_l1)){return E(_ks);}else{var _l2=new T(function(){return _ku(_kx);});return function(_l3){var _l4=new T(function(){return A(_l2,[_l3]);});return [0,function(_l5){return E(_l4);}];};}}}},_l6=function(_l7){var _l8=new T(function(){return _jB(_l7);}),_l9=new T(function(){return _l6(_l7);}),_la=[1,function(_lb){return A(_ku,[_lb,function(_lc){return E([0,function(_ld){return E(E(_ld)[1])==92?E(_l9):[2];}]);}]);}];return _aK([0,function(_le){return E(E(_le)[1])==92?E([0,function(_lf){var _lg=E(E(_lf)[1]);switch(_lg){case 9:return E(_la);case 10:return E(_la);case 11:return E(_la);case 12:return E(_la);case 13:return E(_la);case 32:return E(_la);case 38:return E(_l9);case 160:return E(_la);default:var _lh=u_iswspace(_lg);return E(_lh)==0?[2]:E(_la);}}]):[2];}],[0,function(_li){var _lj=E(_li);return E(_lj[1])==92?E(_l8):A(_l7,[[0,_lj,_e8]]);}]);},_lk=function(_ll,_lm){var _ln=new T(function(){return A(_lm,[[1,new T(function(){return A(_ll,[_9]);})]]);});return _l6(function(_lo){var _lp=E(_lo),_lq=E(_lp[1]);return E(_lq[1])==34?!E(_lp[2])?E(_ln):_lk(function(_lr){return A(_ll,[[1,_lq,_lr]]);},_lm):_lk(function(_ls){return A(_ll,[[1,_lq,_ls]]);},_lm);});},_lt=unCStr("_\'"),_lu=function(_lv){var _lw=u_iswalnum(_lv);return E(_lw)==0?_dM(_bh,[0,_lv],_lt):true;},_lx=function(_ly){return _lu(E(_ly)[1]);},_lz=unCStr(",;()[]{}`"),_lA=function(_lB){return A(_lB,[_9]);},_lC=function(_lD,_lE){var _lF=function(_lG){var _lH=E(_lG);if(!_lH[0]){return E(_lA);}else{var _lI=_lH[1];if(!A(_lD,[_lI])){return E(_lA);}else{var _lJ=new T(function(){return _lF(_lH[2]);});return function(_lK){var _lL=new T(function(){return A(_lJ,[function(_lM){return A(_lK,[[1,_lI,_lM]]);}]);});return [0,function(_lN){return E(_lL);}];};}}};return [1,function(_lO){return A(_lF,[_lO,_lE]);}];},_lP=unCStr(".."),_lQ=unCStr("::"),_lR=unCStr("->"),_lS=[0,64],_lT=[1,_lS,_9],_lU=[0,126],_lV=[1,_lU,_9],_lW=unCStr("=>"),_lX=[1,_lW,_9],_lY=[1,_lV,_lX],_lZ=[1,_lT,_lY],_m0=[1,_lR,_lZ],_m1=unCStr("<-"),_m2=[1,_m1,_m0],_m3=[0,124],_m4=[1,_m3,_9],_m5=[1,_m4,_m2],_m6=[1,_iU,_9],_m7=[1,_m6,_m5],_m8=[0,61],_m9=[1,_m8,_9],_ma=[1,_m9,_m7],_mb=[1,_lQ,_ma],_mc=[1,_lP,_mb],_md=function(_me){var _mf=new T(function(){return A(_me,[_cf]);});return _aK([1,function(_mg){return E(_mg)[0]==0?E(_mf):[2];}],new T(function(){var _mh=new T(function(){return _jB(function(_mi){var _mj=E(_mi);return (function(_mk,_ml){var _mm=new T(function(){return A(_me,[[0,_mk]]);});return !E(_ml)?E(E(_mk)[1])==39?[2]:[0,function(_mn){return E(E(_mn)[1])==39?E(_mm):[2];}]:[0,function(_mo){return E(E(_mo)[1])==39?E(_mm):[2];}];})(_mj[1],_mj[2]);});});return _aK([0,function(_mp){return E(E(_mp)[1])==39?E([0,function(_mq){var _mr=E(_mq);switch(E(_mr[1])){case 39:return [2];case 92:return E(_mh);default:var _ms=new T(function(){return A(_me,[[0,_mr]]);});return [0,function(_mt){return E(E(_mt)[1])==39?E(_ms):[2];}];}}]):[2];}],new T(function(){var _mu=new T(function(){return _lk(_n,_me);});return _aK([0,function(_mv){return E(E(_mv)[1])==34?E(_mu):[2];}],new T(function(){return _aK([0,function(_mw){return !_dM(_bh,_mw,_lz)?[2]:A(_me,[[2,[1,_mw,_9]]]);}],new T(function(){return _aK([0,function(_mx){return !_dM(_bh,_mx,_dR)?[2]:_lC(_dS,function(_my){var _mz=[1,_mx,_my];return !_dM(_by,_mz,_mc)?A(_me,[[4,_mz]]):A(_me,[[2,_mz]]);});}],new T(function(){return _aK([0,function(_mA){var _mB=E(_mA),_mC=_mB[1],_mD=u_iswalpha(_mC);return E(_mD)==0?E(_mC)==95?_lC(_lx,function(_mE){return A(_me,[[3,[1,_mB,_mE]]]);}):[2]:_lC(_lx,function(_mF){return A(_me,[[3,[1,_mB,_mF]]]);});}],new T(function(){return _bT(_dW,_dH,_me);}));}));}));}));}));}));},_mG=function(_mH){var _mI=new T(function(){return _md(_mH);});return [1,function(_mJ){return A(_ku,[_mJ,function(_mK){return E(_mI);}]);}];},_mL=[0,0],_mM=function(_mN,_mO){var _mP=new T(function(){return A(_mN,[_mL,function(_mQ){var _mR=new T(function(){return A(_mO,[_mQ]);});return _mG(function(_mS){var _mT=E(_mS);if(_mT[0]==2){var _mU=E(_mT[1]);return _mU[0]==0?[2]:E(E(_mU[1])[1])==41?E(_mU[2])[0]==0?E(_mR):[2]:[2];}else{return [2];}});}]);});return _mG(function(_mV){var _mW=E(_mV);if(_mW[0]==2){var _mX=E(_mW[1]);return _mX[0]==0?[2]:E(E(_mX[1])[1])==40?E(_mX[2])[0]==0?E(_mP):[2]:[2];}else{return [2];}});},_mY=function(_mZ,_n0,_n1){var _n2=function(_n3,_n4){var _n5=new T(function(){return _md(function(_n6){return A(_mZ,[_n6,_n3,function(_n7){return A(_n4,[new T(function(){return [0, -E(_n7)[1]];})]);}]);});});return _aK(_mG(function(_n8){var _n9=E(_n8);if(_n9[0]==4){var _na=E(_n9[1]);return _na[0]==0?A(_mZ,[_n9,_n3,_n4]):E(E(_na[1])[1])==45?E(_na[2])[0]==0?E([1,function(_nb){return A(_ku,[_nb,function(_nc){return E(_n5);}]);}]):A(_mZ,[_n9,_n3,_n4]):A(_mZ,[_n9,_n3,_n4]);}else{return A(_mZ,[_n9,_n3,_n4]);}}),new T(function(){return _mM(_n2,_n4);}));};return _n2(_n0,_n1);},_nd=function(_ne,_nf){return [2];},_ng=function(_nh,_ni){return _nd(_nh,_ni);},_nj=function(_nk){var _nl=E(_nk);return _nl[0]==0?[1,new T(function(){return _di(new T(function(){return _jz(E(_nl[1])[1]);}),_d9,_nl[2]);})]:E(_nl[2])[0]==0?E(_nl[3])[0]==0?[1,new T(function(){return _di(_d8,_d9,_nl[1]);})]:[0]:[0];},_nm=function(_nn){var _no=E(_nn);if(_no[0]==5){var _np=_nj(_no[1]);if(!_np[0]){return E(_nd);}else{var _nq=new T(function(){return [0,_el(_np[1])];});return function(_nr,_ns){return A(_ns,[_nq]);};}}else{return E(_ng);}},_nt=function(_nh,_ni){return _mY(_nm,_nh,_ni);},_nu=function(_nv,_nw){var _nx=function(_ny,_nz){var _nA=new T(function(){return A(_nz,[_9]);}),_nB=new T(function(){return A(_nv,[_mL,function(_nC){return _nx(_e9,function(_nD){return A(_nz,[[1,_nC,_nD]]);});}]);});return _mG(function(_nE){var _nF=E(_nE);if(_nF[0]==2){var _nG=E(_nF[1]);if(!_nG[0]){return [2];}else{var _nH=_nG[2];switch(E(E(_nG[1])[1])){case 44:return E(_nH)[0]==0?!E(_ny)?[2]:E(_nB):[2];case 93:return E(_nH)[0]==0?E(_nA):[2];default:return [2];}}}else{return [2];}});},_nI=function(_nJ){var _nK=new T(function(){return _aK(_nx(_e8,_nJ),new T(function(){return A(_nv,[_mL,function(_nL){return _nx(_e9,function(_nM){return A(_nJ,[[1,_nL,_nM]]);});}]);}));});return _aK(_mG(function(_nN){var _nO=E(_nN);if(_nO[0]==2){var _nP=E(_nO[1]);return _nP[0]==0?[2]:E(E(_nP[1])[1])==91?E(_nP[2])[0]==0?E(_nK):[2]:[2];}else{return [2];}}),new T(function(){return _mM(function(_nQ,_nR){return _nI(_nR);},_nJ);}));};return _nI(_nw);},_nS=function(_nT,_nU){return _nu(_nt,_nU);},_nV=new T(function(){return _nu(_nt,_bL);}),_nW=function(_ni){return _aA(_nV,_ni);},_nX=function(_nY){var _nZ=new T(function(){return _mY(_nm,_nY,_bL);});return function(_ce){return _aA(_nZ,_ce);};},_o0=[0,_nX,_nW,_nt,_nS],_o1=function(_o2,_o3){return _3u(0,E(_o2)[1],_o3);},_o4=function(_o5,_o6){return _2l(_o1,_o5,_o6);},_o7=function(_o8,_o9,_oa){return _3u(E(_o8)[1],E(_o9)[1],_oa);},_ob=[0,_o7,_54,_o4],_oc=unCStr("GHC.Types"),_od=unCStr("Int"),_oe=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_5K,_oc,_od],_of=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_oe,_9],_og=function(_oh){return E(_of);},_oi=function(_oj){return E(E(_oj)[1]);},_ok=function(_ol){return E(E(_ol)[2]);},_om=function(_on,_oo){var _op=new T(function(){return A(_ok,[_on,_oo]);}),_oq=new T(function(){return _oi(_on);}),_or=new T(function(){return _3f(_oq);}),_os=new T(function(){return _2P(_oq);});return function(_ot){return A(_os,[_op,function(_ou){return A(_or,[[0,_ou,_ot]]);}]);};},_ov=function(_ow,_ox){return [0,_ow,function(_oy){return _om(_ox,_oy);}];},_oz=function(_oA,_oB){return A(_3f,[_oA,[0,_oB,_oB]]);},_oC=function(_oD,_oE,_oF){return A(_3f,[_oD,[0,_0,_oE]]);},_oG=function(_oH,_oI){return [0,_oH,function(_oJ){return _oz(_oI,_oJ);},function(_oK,_oL){return _oC(_oI,_oK,_oL);}];},_oM=function(_oN,_oO){return A(_oN,[function(_){return jsFind(toJSStr(E(_oO)));}]);},_oP=function(_oQ){return E(E(_oQ)[3]);},_oR=unCStr("[]"),_oS=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_5K,_oc,_oR],_oT=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_oS,_9],_oU=function(_oV){return E(_oT);},_oW=unCStr("Char"),_oX=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_5K,_oc,_oW],_oY=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_oX,_9],_oZ=function(_p0){return E(_oY);},_p1=new T(function(){return _6E(_oU,_oZ);}),_p2=new T(function(){return A(_p1,[_6D]);}),_p3=new T(function(){return E(_6D);}),_p4=function(_p5){return E(E(_p5)[6]);},_p6=function(_p7){return E(E(_p7)[1]);},_p8=[0,0],_p9=[0,32],_pa=[0,10],_pb=function(_pc){var _pd=E(_pc);if(!_pd[0]){return E(_n);}else{var _pe=_pd[1],_pf=E(_pd[2]);if(!_pf[0]){return _pg(_pa,_pe);}else{var _ph=new T(function(){return _pb(_pf);}),_pi=new T(function(){return _pg(_pa,_pe);});return function(_pj){return A(_pi,[[1,_p9,new T(function(){return A(_ph,[_pj]);})]]);};}}},_pk=unCStr("->"),_pl=[1,_pk,_9],_pm=[1,_oc,_pl],_pn=[1,_5K,_pm],_po=[0,32],_pp=function(_pq){var _pr=E(_pq);if(!_pr[0]){return [0];}else{var _ps=_pr[1],_pt=E(_pr[2]);return _pt[0]==0?E(_ps):_1v(_ps,[1,_po,new T(function(){return _pp(_pt);})]);}},_pu=new T(function(){return _pp(_pn);}),_pv=new T(function(){var _pw=_68(_pu);return [0,_pw[1],_pw[2],_5K,_oc,_pk];}),_px=function(_py,_pz){var _pA=E(_py);return _pA[0]==0?E(_pz):A(_pA[1],[new T(function(){return _px(_pA[2],_pz);})]);},_pB=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_pC=[1,_5M,_9],_pD=function(_pE){var _pF=E(_pE);if(!_pF[0]){return [0];}else{var _pG=E(_pF[1]);return [1,[0,_pG[1],_pG[2]],new T(function(){return _pD(_pF[2]);})];}},_pH=new T(function(){var _pI=_1v(_9,_pC);if(!_pI[0]){return E(_oS);}else{var _pJ=_68(new T(function(){return _5W(_6k(_6v,[1,_pB,new T(function(){return _pD(_pI);})]));}));return E(_oS);}}),_pK=[0,40],_pL=function(_pM){return _pg(_pa,_pM);},_pN=[0,8],_pO=unCStr(" -> "),_pP=[0,9],_pQ=[0,93],_pR=[0,91],_pS=[0,41],_pT=[0,44],_pU=function(_pM){return [1,_pT,_pM];},_pV=function(_pW,_pX){var _pY=E(_pX);return _pY[0]==0?[0]:[1,_pW,[1,_pY[1],new T(function(){return _pV(_pW,_pY[2]);})]];},_pg=function(_pZ,_q0){var _q1=E(_q0),_q2=_q1[3],_q3=E(_q1[4]);if(!_q3[0]){return function(_q4){return _1v(E(_q2)[5],_q4);};}else{var _q5=_q3[1],_q6=new T(function(){var _q7=E(_q2)[5],_q8=new T(function(){return _pb(_q3);}),_q9=new T(function(){return E(_pZ)[1]<=9?function(_qa){return _1v(_q7,[1,_p9,new T(function(){return A(_q8,[_qa]);})]);}:function(_qb){return [1,_3t,new T(function(){return _1v(_q7,[1,_p9,new T(function(){return A(_q8,[[1,_3s,_qb]]);})]);})];};}),_qc=E(_q7);if(!_qc[0]){return E(_q9);}else{if(E(E(_qc[1])[1])==40){var _qd=E(_qc[2]);return _qd[0]==0?E(_q9):E(E(_qd[1])[1])==44?function(_qe){return [1,_pK,new T(function(){return A(new T(function(){var _qf=_6k(_pL,_q3);if(!_qf[0]){return E(_n);}else{var _qg=new T(function(){return _pV(_pU,_qf[2]);});return function(_ce){return _px([1,_qf[1],_qg],_ce);};}}),[[1,_pS,_qe]]);})];}:E(_q9);}else{return E(_q9);}}}),_qh=E(_q3[2]);if(!_qh[0]){var _qi=E(_q2),_qj=E(_pH),_qk=hs_eqWord64(_qi[1],_qj[1]);if(!E(_qk)){return E(_q6);}else{var _ql=hs_eqWord64(_qi[2],_qj[2]);if(!E(_ql)){return E(_q6);}else{var _qm=new T(function(){return _pg(_p8,_q5);});return function(_qn){return [1,_pR,new T(function(){return A(_qm,[[1,_pQ,_qn]]);})];};}}}else{if(!E(_qh[2])[0]){var _qo=E(_q2),_qp=E(_pv),_qq=hs_eqWord64(_qo[1],_qp[1]);if(!E(_qq)){return E(_q6);}else{var _qr=hs_eqWord64(_qo[2],_qp[2]);if(!E(_qr)){return E(_q6);}else{var _qs=new T(function(){return _pg(_pN,_qh[1]);}),_qt=new T(function(){return _pg(_pP,_q5);});return E(_pZ)[1]<=8?function(_qu){return A(_qt,[new T(function(){return _1v(_pO,new T(function(){return A(_qs,[_qu]);}));})]);}:function(_qv){return [1,_3t,new T(function(){return A(_qt,[new T(function(){return _1v(_pO,new T(function(){return A(_qs,[[1,_3s,_qv]]);}));})]);})];};}}}else{return E(_q6);}}}},_qw=function(_qx,_qy,_qz,_qA){var _qB=new T(function(){return _3f(_qx);}),_qC=new T(function(){return _oP(_qA);}),_qD=new T(function(){return _p4(_qA);}),_qE=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_pg,[_p8,A(_qy,[_p3]),_9]);}));}),_qF=new T(function(){return A(_p6,[_qz,_f]);});return function(_qG){if(!E(new T(function(){var _qH=A(_qy,[_p3]),_qI=E(_p2),_qJ=hs_eqWord64(_qH[1],_qI[1]);if(!E(_qJ)){return false;}else{var _qK=hs_eqWord64(_qH[2],_qI[2]);return E(_qK)==0?false:true;}}))){var _qL=new T(function(){return A(_qB,[[1,_qG,new T(function(){return A(_qD,[new T(function(){return A(_qC,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _1v(_qG,_qE);}));})]);})]);})]]);}),_qM=A(_qF,[_qG]);if(!_qM[0]){return E(_qL);}else{var _qN=E(_qM[1]);return E(_qN[2])[0]==0?E(_qM[2])[0]==0?A(_qB,[[2,_qN[1]]]):E(_qL):E(_qL);}}else{return A(_qB,[[2,_qG]]);}};},_qO=[0],_qP=new T(function(){return [0,"value"];}),_qQ=function(_qR,_qS,_qT,_qU,_qV,_qW){var _qX=E(_qR),_qY=_qX[1],_qZ=new T(function(){return A(_qX[3],[_qO]);}),_r0=new T(function(){return _qw(_qX,_qT,_qU,_qV);});return A(_qY,[new T(function(){return _oM(_qS,_qW);}),function(_r1){var _r2=E(_r1);return _r2[0]==0?E(_qZ):A(_qY,[new T(function(){return A(_qS,[function(_){var _r3=jsGet(E(_r2[1])[1],E(_qP)[1]);return [1,new T(function(){return fromJSStr(_r3);})];}]);}),function(_r4){var _r5=E(_r4);return _r5[0]==0?E(_qZ):A(_r0,[_r5[1]]);}]);}]);},_r6=1,_r7=function(_r8){return E(E(_r8)[9]);},_r9=function(_ra){return E(E(_ra)[2]);},_rb=function(_rc){return E(E(_rc)[3]);},_rd=function(_re){return E(E(_re)[2]);},_rf=function(_rg,_rh,_ri,_rj,_rk,_rl,_rm,_rn,_ro,_rp,_rq,_rr){var _rs=_oi(_rl),_rt=_rs[1],_ru=_rs[3],_rv=new T(function(){return _8Y(_rn);}),_rw=new T(function(){return _90(_rv);}),_rx=new T(function(){return _rb(_rm);}),_ry=new T(function(){return _rd(_rh);}),_rz=new T(function(){return _r7(_rn);});return A(_rt,[new T(function(){var _rA=E(_rp);if(!_rA[0]){var _rB=E(_rm);return _3z(_ro,_rB[1],_rB[2],_rB[3]);}else{return A(_ru,[_rA[1]]);}}),function(_rC){return A(_rt,[new T(function(){var _rD=E(_ro);return _r9(_rm);}),function(_rE){return A(_rs[2],[new T(function(){return A(_rx,[new T(function(){var _rF=E(new T(function(){var _rG=E(_ro);return [0,coercionToken];})),_rH=E(_rE);return [0,_rH[1],_rH[2],_r6,_rH[4],_rH[5]];})]);}),new T(function(){var _rI=new T(function(){return A(_ru,[[0,new T(function(){return A(_rz,[_rC,_rq,new T(function(){var _rJ=E(_rr);if(!_rJ[0]){return [0];}else{var _rK=_rJ[1],_rL=_1f(_rk,_p1,_rK);return _rL[0]==0?A(_rd,[_ri,_rK]):E(_rL[1]);}}),_e8,_a]);}),_a]]);});return A(_rt,[new T(function(){var _rM=E(_rl);return _qQ(_rM[1],_rM[2],_rj,_rg,_rn,_rC);}),function(_rN){var _rO=E(_rN);switch(_rO[0]){case 0:return E(_rI);case 1:return A(_ru,[[0,new T(function(){return A(_rw,[new T(function(){return A(_rz,[_rC,_rq,_rO[1],_e8,_a]);}),_rO[2]]);}),_a]]);default:var _rP=_rO[1];return A(_ru,[[0,new T(function(){return A(_rz,[_rC,_rq,new T(function(){var _rQ=_1f(_rj,_p1,_rP);return _rQ[0]==0?A(_ry,[_rP]):E(_rQ[1]);}),_e8,_a]);}),[1,_rP]]]);}}]);})]);}]);}]);},_rR=function(_rS,_rT,_rU,_rV,_rW){var _rX=new T(function(){return _oi(_rT);}),_rY=new T(function(){return _3h(_rX);}),_rZ=new T(function(){return _oG(_rY,_rX);}),_s0=new T(function(){return _ov(_rY,_rT);});return function(_ce,_s1,_s2){return _rf(_rW,_rV,_rV,_rU,_rU,_s0,_rZ,_rS,[0,coercionToken],_ce,_s1,_s2);};},_s3=new T(function(){return _rR(_8B,_9H,_og,_ob,_o0);}),_s4=new T(function(){return A(_s3,[_a,_9G,_a]);}),_s5=unCStr("keydown"),_s6=unCStr("mousemove"),_s7=unCStr("blur"),_s8=unCStr("focus"),_s9=unCStr("change"),_sa=unCStr("unload"),_sb=unCStr("load"),_sc=unCStr("keyup"),_sd=unCStr("keypress"),_se=unCStr("mouseup"),_sf=unCStr("mousedown"),_sg=unCStr("dblclick"),_sh=unCStr("click"),_si=unCStr("mouseout"),_sj=unCStr("mouseover"),_sk=function(_sl){switch(E(_sl)[0]){case 0:return E(_sb);case 1:return E(_sa);case 2:return E(_s9);case 3:return E(_s8);case 4:return E(_s7);case 5:return E(_s6);case 6:return E(_sj);case 7:return E(_si);case 8:return E(_sh);case 9:return E(_sg);case 10:return E(_sf);case 11:return E(_se);case 12:return E(_sd);case 13:return E(_sc);default:return E(_s5);}},_sm=[0],_sn=unCStr("OnLoad"),_so=[0,_sn,_sm],_sp=function(_){var _=0,_sq=newMVar(),_=putMVar(_sq,_so);return [0,_sq];},_sr=new T(function(){return _j(_sp);}),_ss=function(_st,_su,_){var _sv=A(_st,[_]);return die(_su);},_sw=function(_sx,_sy,_sz,_){return _ss(function(_){var _=putMVar(_sy,_sx);return _0;},_sz,_);},_sA=function(_sB,_){var _sC=0;if(!E(_sC)){return (function(_){var _sD=E(_sr)[1],_sE=takeMVar(_sD),_sF=jsCatch(function(_){return (function(_){return _sB;})();},function(_F,_){return _sw(_sE,_sD,_F,_);}),_=putMVar(_sD,_sF);return _0;})();}else{var _sG=E(_sr)[1],_sH=takeMVar(_sG),_sI=jsCatch(function(_){return _sB;},function(_F,_){return _sw(_sH,_sG,_F,_);}),_=putMVar(_sG,_sI);return _0;}},_sJ=unCStr("true"),_sK=function(_sL,_sM){while(1){var _sN=E(_sL);if(!_sN[0]){return E(_sM)[0]==0?true:false;}else{var _sO=E(_sM);if(!_sO[0]){return false;}else{if(E(_sN[1])[1]!=E(_sO[1])[1]){return false;}else{_sL=_sN[2];_sM=_sO[2];continue;}}}}},_sP=new T(function(){return [0,"keydown"];}),_sQ=new T(function(){return [0,"mousemove"];}),_sR=new T(function(){return [0,"blur"];}),_sS=new T(function(){return [0,"focus"];}),_sT=new T(function(){return [0,"change"];}),_sU=new T(function(){return [0,"unload"];}),_sV=new T(function(){return [0,"load"];}),_sW=new T(function(){return [0,"keyup"];}),_sX=new T(function(){return [0,"keypress"];}),_sY=new T(function(){return [0,"mouseup"];}),_sZ=new T(function(){return [0,"mousedown"];}),_t0=new T(function(){return [0,"dblclick"];}),_t1=new T(function(){return [0,"click"];}),_t2=new T(function(){return [0,"mouseout"];}),_t3=new T(function(){return [0,"mouseover"];}),_t4=function(_t5){switch(E(_t5)[0]){case 0:return E(_sV);case 1:return E(_sU);case 2:return E(_sT);case 3:return E(_sS);case 4:return E(_sR);case 5:return E(_sQ);case 6:return E(_t3);case 7:return E(_t2);case 8:return E(_t1);case 9:return E(_t0);case 10:return E(_sZ);case 11:return E(_sY);case 12:return E(_sX);case 13:return E(_sW);default:return E(_sP);}},_t6=function(_t7,_t8,_t9){var _ta=new T(function(){return _sk(_t8);}),_tb=new T(function(){return _t4(_t8);});return function(_tc,_){var _td=A(_t7,[_tc,_]),_te=E(_td),_tf=_te[1],_tg=E(_ta),_th=jsGetAttr(_tf,toJSStr(_tg));if(!_sK(fromJSStr(_th),_sJ)){var _ti=E(_t9),_tj=jsSetCB(_tf,E(_tb)[1],E([0,_t9])[1]),_tk=A(_1,[_n,_te,_tg,_sJ,_]);return _te;}else{return _te;}};},_tl=function(_tm,_tn){var _to=new T(function(){return _sk(_tn);}),_tp=[0,_to,_sm];return function(_tq,_){var _tr=E(_tq),_ts=E(_tr[4]),_tt=_ts[1],_tu=_ts[2],_tv=A(_tm,[_tr,_]),_tw=E(_tv),_tx=E(_tw[1]),_ty=_tx[1];return [0,[0,new T(function(){var _tz=E(_tn);switch(_tz[0]){case 0:return _t6(_ty,_tz,function(_){var _tA=_sA(_tp,_),_tB=A(_tt,[_]),_tC=E(_tB);if(!_tC[0]){return _0;}else{var _tD=A(_tu,[_tC[1],_]);return _0;}});case 1:return _t6(_ty,_tz,function(_){var _tE=_sA(_tp,_),_tF=A(_tt,[_]),_tG=E(_tF);if(!_tG[0]){return _0;}else{var _tH=A(_tu,[_tG[1],_]);return _0;}});case 2:return _t6(_ty,_tz,function(_){var _tI=_sA(_tp,_),_tJ=A(_tt,[_]),_tK=E(_tJ);if(!_tK[0]){return _0;}else{var _tL=A(_tu,[_tK[1],_]);return _0;}});case 3:return _t6(_ty,_tz,function(_){var _tM=_sA(_tp,_),_tN=A(_tt,[_]),_tO=E(_tN);if(!_tO[0]){return _0;}else{var _tP=A(_tu,[_tO[1],_]);return _0;}});case 4:return _t6(_ty,_tz,function(_){var _tQ=_sA(_tp,_),_tR=A(_tt,[_]),_tS=E(_tR);if(!_tS[0]){return _0;}else{var _tT=A(_tu,[_tS[1],_]);return _0;}});case 5:return _t6(_ty,_tz,function(_tU,_){var _tV=_sA([0,_to,[2,E(_tU)]],_),_tW=A(_tt,[_]),_tX=E(_tW);if(!_tX[0]){return _0;}else{var _tY=A(_tu,[_tX[1],_]);return _0;}});case 6:return _t6(_ty,_tz,function(_tZ,_){var _u0=_sA([0,_to,[2,E(_tZ)]],_),_u1=A(_tt,[_]),_u2=E(_u1);if(!_u2[0]){return _0;}else{var _u3=A(_tu,[_u2[1],_]);return _0;}});case 7:return _t6(_ty,_tz,function(_){var _u4=A(_tt,[_]),_u5=E(_u4);if(!_u5[0]){return _0;}else{var _u6=A(_tu,[_u5[1],_]);return _0;}});case 8:return _t6(_ty,_tz,function(_u7,_u8,_){var _u9=_sA([0,_to,[1,_u7,E(_u8)]],_),_ua=A(_tt,[_]),_ub=E(_ua);if(!_ub[0]){return _0;}else{var _uc=A(_tu,[_ub[1],_]);return _0;}});case 9:return _t6(_ty,_tz,function(_ud,_ue,_){var _uf=_sA([0,_to,[1,_ud,E(_ue)]],_),_ug=A(_tt,[_]),_uh=E(_ug);if(!_uh[0]){return _0;}else{var _ui=A(_tu,[_uh[1],_]);return _0;}});case 10:return _t6(_ty,_tz,function(_uj,_uk,_){var _ul=_sA([0,_to,[1,_uj,E(_uk)]],_),_um=A(_tt,[_]),_un=E(_um);if(!_un[0]){return _0;}else{var _uo=A(_tu,[_un[1],_]);return _0;}});case 11:return _t6(_ty,_tz,function(_up,_uq,_){var _ur=_sA([0,_to,[1,_up,E(_uq)]],_),_us=A(_tt,[_]),_ut=E(_us);if(!_ut[0]){return _0;}else{var _uu=A(_tu,[_ut[1],_]);return _0;}});case 12:return _t6(_ty,_tz,function(_uv,_){var _uw=_sA([0,_to,[3,_uv]],_),_ux=A(_tt,[_]),_uy=E(_ux);if(!_uy[0]){return _0;}else{var _uz=A(_tu,[_uy[1],_]);return _0;}});case 13:return _t6(_ty,_tz,function(_uA,_){var _uB=_sA([0,_to,[3,_uA]],_),_uC=A(_tt,[_]),_uD=E(_uC);if(!_uD[0]){return _0;}else{var _uE=A(_tu,[_uD[1],_]);return _0;}});default:return _t6(_ty,_tz,function(_uF,_){var _uG=_sA([0,_to,[3,_uF]],_),_uH=A(_tt,[_]),_uI=E(_uH);if(!_uI[0]){return _0;}else{var _uJ=A(_tu,[_uI[1],_]);return _0;}});}}),_tx[2]],_tw[2]];};},_uK=new T(function(){return _tl(_s4,_9F);}),_uL=function(_uM,_){var _uN=A(_uK,[_uM,_]),_uO=E(_uN),_uP=E(_uO[1]);return [0,[0,function(_uQ,_){var _uR=A(_uP[1],[_uQ,_]),_uS=_5i(_uQ,_);return _uQ;},_uP[2]],_uO[2]];},_uT=new T(function(){return [1,_uL,_uT];}),_uU=function(_uV,_uW){var _uX=E(_uV);if(!_uX){return [0];}else{var _uY=E(_uW);return _uY[0]==0?[0]:[1,_uY[1],new T(function(){return _uU(_uX-1|0,_uY[2]);})];}},_uZ=function(_v0,_v1){return _v0<0?[0]:_uU(_v0,_v1);},_v2=function(_v3,_v4){var _v5=E(_v3)[1];return _v5>0?_uZ(_v5,_v4):[0];},_v6=function(_v7){return E(_v7);},_v8=function(_v9){var _va=new T(function(){return _9y(_5A,_v2(_v9,_uT));}),_vb=new T(function(){return _4V(_4O,new T(function(){return unAppCStr("This widget sum ",new T(function(){return _1v(_3u(0,E(_v9)[1],_9),_5w);}));}));});return function(_vc,_){var _vd=_4j(_va,_5o,_vc,_),_ve=E(_vd),_vf=E(_ve[1]),_vg=new T(function(){return _4V(_v6,_vf[1]);});return [0,[0,function(_vh,_){var _vi=A(_vb,[_vh,_]),_vj=A(_vg,[_vh,_]);return _vh;},_vf[2]],_ve[2]];};},_vk=new T(function(){return _v8(_4N);}),_vl=unCStr("center"),_vm=function(_vn,_vo){var _vp=new T(function(){return A(_vn,[_vo]);});return function(_vq,_){var _vr=jsCreateElem(toJSStr(E(_vl))),_vs=jsAppendChild(_vr,E(_vq)[1]),_vt=[0,_vr],_vu=A(_vp,[_vt,_]);return _vt;};},_vv=function(_vw,_){return _vw;},_vx=unCStr("Two counters. One is pure and recursive, the other is stateful"),_vy=new T(function(){return _4V(_4O,_vx);}),_vz=[8,coercionToken],_vA=function(_vB){return _aK(_mG(function(_vC){var _vD=E(_vC);return _vD[0]==0?A(_vB,[_vD[1]]):[2];}),new T(function(){return _mM(_vE,_vB);}));},_vE=function(_vF,_vG){return _vA(_vG);},_vH=function(_vI){return _aK(_aK(_mG(function(_vJ){var _vK=E(_vJ);return _vK[0]==1?A(_vI,[_vK[1]]):[2];}),new T(function(){return _nu(_vE,_vI);})),new T(function(){return _mM(_vL,_vI);}));},_vL=function(_vM,_vN){return _vH(_vN);},_vO=new T(function(){return _mM(_vL,_bL);}),_vP=new T(function(){return _nu(_vE,_bL);}),_vQ=function(_vR){var _vS=E(_vR);return _vS[0]==1?[3,_vS[1],_bK]:[2];},_vT=new T(function(){return _md(_vQ);}),_vU=function(_vV){return E(_vT);},_vW=function(_vX){return A(_ku,[_vX,_vU]);},_vY=[1,_vW],_vZ=new T(function(){return _aK(_vY,_vP);}),_w0=new T(function(){return _aK(_vZ,_vO);}),_w1=function(_ni){return _aA(_w0,_ni);},_w2=new T(function(){return _vA(_bL);}),_w3=function(_ni){return _aA(_w2,_ni);},_w4=function(_w5){return E(_w3);},_w6=[0,_w4,_w1,_vE,_vL],_w7=function(_w8){return E(E(_w8)[4]);},_w9=function(_wa,_wb,_wc){return _nu(new T(function(){return _w7(_wa);}),_wc);},_wd=function(_we){var _wf=new T(function(){return _nu(new T(function(){return _w7(_we);}),_bL);});return function(_ce){return _aA(_wf,_ce);};},_wg=function(_wh,_wi){var _wj=new T(function(){return A(_w7,[_wh,_wi,_bL]);});return function(_ce){return _aA(_wj,_ce);};},_wk=function(_wl){return [0,function(_ni){return _wg(_wl,_ni);},new T(function(){return _wd(_wl);}),new T(function(){return _w7(_wl);}),function(_nh,_ni){return _w9(_wl,_nh,_ni);}];},_wm=new T(function(){return _wk(_w6);}),_wn=unCStr("Prelude.(!!): negative index\n"),_wo=new T(function(){return err(_wn);}),_wp=unCStr("Prelude.(!!): index too large\n"),_wq=new T(function(){return err(_wp);}),_wr=function(_ws,_wt){while(1){var _wu=E(_ws);if(!_wu[0]){return E(_wq);}else{var _wv=E(_wt);if(!_wv){return E(_wu[1]);}else{_ws=_wu[2];_wt=_wv-1|0;continue;}}}},_ww=unCStr("ACK"),_wx=unCStr("BEL"),_wy=unCStr("BS"),_wz=unCStr("SP"),_wA=[1,_wz,_9],_wB=unCStr("US"),_wC=[1,_wB,_wA],_wD=unCStr("RS"),_wE=[1,_wD,_wC],_wF=unCStr("GS"),_wG=[1,_wF,_wE],_wH=unCStr("FS"),_wI=[1,_wH,_wG],_wJ=unCStr("ESC"),_wK=[1,_wJ,_wI],_wL=unCStr("SUB"),_wM=[1,_wL,_wK],_wN=unCStr("EM"),_wO=[1,_wN,_wM],_wP=unCStr("CAN"),_wQ=[1,_wP,_wO],_wR=unCStr("ETB"),_wS=[1,_wR,_wQ],_wT=unCStr("SYN"),_wU=[1,_wT,_wS],_wV=unCStr("NAK"),_wW=[1,_wV,_wU],_wX=unCStr("DC4"),_wY=[1,_wX,_wW],_wZ=unCStr("DC3"),_x0=[1,_wZ,_wY],_x1=unCStr("DC2"),_x2=[1,_x1,_x0],_x3=unCStr("DC1"),_x4=[1,_x3,_x2],_x5=unCStr("DLE"),_x6=[1,_x5,_x4],_x7=unCStr("SI"),_x8=[1,_x7,_x6],_x9=unCStr("SO"),_xa=[1,_x9,_x8],_xb=unCStr("CR"),_xc=[1,_xb,_xa],_xd=unCStr("FF"),_xe=[1,_xd,_xc],_xf=unCStr("VT"),_xg=[1,_xf,_xe],_xh=unCStr("LF"),_xi=[1,_xh,_xg],_xj=unCStr("HT"),_xk=[1,_xj,_xi],_xl=[1,_wy,_xk],_xm=[1,_wx,_xl],_xn=[1,_ww,_xm],_xo=unCStr("ENQ"),_xp=[1,_xo,_xn],_xq=unCStr("EOT"),_xr=[1,_xq,_xp],_xs=unCStr("ETX"),_xt=[1,_xs,_xr],_xu=unCStr("STX"),_xv=[1,_xu,_xt],_xw=unCStr("SOH"),_xx=[1,_xw,_xv],_xy=unCStr("NUL"),_xz=[1,_xy,_xx],_xA=[0,92],_xB=unCStr("\\DEL"),_xC=unCStr("\\a"),_xD=unCStr("\\\\"),_xE=unCStr("\\SO"),_xF=unCStr("\\r"),_xG=unCStr("\\f"),_xH=unCStr("\\v"),_xI=unCStr("\\n"),_xJ=unCStr("\\t"),_xK=unCStr("\\b"),_xL=function(_xM,_xN){if(_xM<=127){var _xO=E(_xM);switch(_xO){case 92:return _1v(_xD,_xN);case 127:return _1v(_xB,_xN);default:if(_xO<32){var _xP=E(_xO);switch(_xP){case 7:return _1v(_xC,_xN);case 8:return _1v(_xK,_xN);case 9:return _1v(_xJ,_xN);case 10:return _1v(_xI,_xN);case 11:return _1v(_xH,_xN);case 12:return _1v(_xG,_xN);case 13:return _1v(_xF,_xN);case 14:return _1v(_xE,new T(function(){var _xQ=E(_xN);return _xQ[0]==0?[0]:E(E(_xQ[1])[1])==72?unAppCStr("\\&",_xQ):E(_xQ);}));default:return _1v([1,_xA,new T(function(){var _xR=_xP;return _xR>=0?_wr(_xz,_xR):E(_wo);})],_xN);}}else{return [1,[0,_xO],_xN];}}}else{return [1,_xA,new T(function(){var _xS=jsShowI(_xM);return _1v(fromJSStr(_xS),new T(function(){var _xT=E(_xN);if(!_xT[0]){return [0];}else{var _xU=E(_xT[1])[1];return _xU<48?E(_xT):_xU>57?E(_xT):unAppCStr("\\&",_xT);}}));})];}},_xV=[0,39],_xW=[1,_xV,_9],_xX=unCStr("\'\\\'\'"),_xY=function(_xZ){var _y0=E(E(_xZ)[1]);return _y0==39?E(_xX):[1,_xV,new T(function(){return _xL(_y0,_xW);})];},_y1=[0,34],_y2=unCStr("\\\""),_y3=function(_y4,_y5){var _y6=E(_y4);if(!_y6[0]){return E(_y5);}else{var _y7=_y6[2],_y8=E(E(_y6[1])[1]);return _y8==34?_1v(_y2,new T(function(){return _y3(_y7,_y5);})):_xL(_y8,new T(function(){return _y3(_y7,_y5);}));}},_y9=function(_ya,_yb){return [1,_y1,new T(function(){return _y3(_ya,[1,_y1,_yb]);})];},_yc=function(_yd){return _1v(_xX,_yd);},_ye=function(_yf,_yg){var _yh=E(E(_yg)[1]);return _yh==39?E(_yc):function(_yi){return [1,_xV,new T(function(){return _xL(_yh,[1,_xV,_yi]);})];};},_yj=[0,_ye,_xY,_y9],_yk=function(_yl){return E(E(_yl)[3]);},_ym=function(_yn,_yo){return A(_yk,[_yn,_yo,_9]);},_yp=function(_yq,_yr,_ys){return _2l(new T(function(){return _yk(_yq);}),_yr,_ys);},_yt=function(_yu){var _yv=new T(function(){return _yk(_yu);});return [0,function(_yw){return E(_yv);},function(_yd){return _ym(_yu,_yd);},function(_yx,_yd){return _yp(_yu,_yx,_yd);}];},_yy=new T(function(){return _yt(_yj);}),_yz=unCStr("submit"),_yA=new T(function(){return A(_rR,[_8B,_9H,_p1,_yy,_wm,_a,_yz]);}),_yB=[0,43],_yC=[1,_yB,_9],_yD=[1,_yC],_yE=new T(function(){return A(_yA,[_yD]);}),_yF=new T(function(){return _tl(_yE,_vz);}),_yG=function(_F,_){return _H(_G,_F,_);},_yH=function(_yI,_yJ,_yK,_){var _yL=A(_yJ,[_yK,_]),_yM=E(_yL),_yN=E(_yM[1]);return [0,[0,function(_yO,_){var _yP=_H(_G,_yO,_),_yQ=A(_1,[_n,_yP,_q,_yI,_]),_yR=A(_yN[1],[_yP,_]);return _yP;},_yN[2]],_yM[2]];},_yS=new T(function(){return _3z(_S,_3m,_Q,_N);}),_yT=new T(function(){return _3z(_S,_3m,_Q,_N);}),_yU=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_yV=function(_yW){return _j(function(_){var _=0;return eval(E(_yW)[1]);});},_yX=new T(function(){return _yV(_yU);}),_yY=function(_yZ,_z0,_z1,_){var _z2=A(_yS,[_z1,_]),_z3=A(_yT,[new T(function(){return E(E(_z2)[2]);}),_]),_z4=E(_z3),_z5=_z4[1],_z6=E(_z4[2]),_z7=_z6[2],_z8=E(_z6[4]),_z9=new T(function(){return E(E(_z2)[1]);}),_za=function(_zb){var _zc=new T(function(){return A(_z0,[_zb]);});return function(_zd,_){var _ze=A(_zc,[_zd,_]),_zf=E(_ze),_zg=E(_zf[1]);return [0,[0,function(_zh,_){var _zi=A(_zg[1],[_zh,_]),_zj=E(_z9),_zk=jsFind(toJSStr(_zj)),_zl=E(_zk);if(!_zl[0]){return _3N(_zj);}else{var _zm=E(_zl[1]),_zn=A(_yX,[E(_zm[1]),_]),_zo=jsKillChild(E(_zm)[1],_zn);return _zh;}},_zg[2]],_zf[2]];};},_zp=_yH(_z9,_yZ,[0,_z6[1],_z7,_z6[3],[0,function(_){return _3P(function(_zq,_){var _zr=_yH(_z9,_yZ,new T(function(){var _zs=E(_zq);return [0,_zs[1],_z7,_zs[3],_zs[4],_zs[5]];}),_);return [0,[0,_2M,E(E(_zr)[1])[2]],_zq];},_z5,_);},function(_zt,_){var _zu=_3P(new T(function(){return _za(_zt);}),_z5,_),_zv=E(_zu);return _zv[0]==0?_a:A(_z8[2],[_zv[1],_]);}],_z6[5]],_),_zw=E(_zp),_zx=_zw[2],_zy=E(_zw[1]),_zz=_zy[1],_zA=new T(function(){return _y(_yG,[1,[0,_q,_z5],_9]);}),_zB=E(_zy[2]);if(!_zB[0]){return [0,[0,function(_zC,_){var _zD=A(_zz,[_zC,_]),_zE=A(_zA,[_zC,_]);return _zC;},_a],new T(function(){var _zF=E(_zx);return [0,_zF[1],_zF[2],_zF[3],_z8,_zF[5]];})];}else{var _zG=A(_za,[_zB[1],new T(function(){var _zH=E(_zx);return [0,_zH[1],_zH[2],_zH[3],_z8,_zH[5]];}),_]),_zI=E(_zG),_zJ=E(_zI[1]);return [0,[0,function(_zK,_){var _zL=A(_zz,[_zK,_]),_zM=A(_zA,[_zK,_]),_zN=A(_zJ[1],[_zM,_]);return _zK;},_zJ[2]],_zI[2]];}},_zO=function(_zP){var _zQ=new T(function(){return _zO(new T(function(){return [0,E(_zP)[1]+1|0];}));}),_zR=new T(function(){return _58(_4O,new T(function(){return _54(_zP);}));});return function(_ce,_s1){return _yY(function(_zS,_){var _zT=A(_yF,[_zS,_]),_zU=E(_zT),_zV=E(_zU[1]);return [0,[0,function(_zW,_){var _zX=A(_zR,[_zW,_]),_zY=A(_zV[1],[_zW,_]);return _zW;},_zV[2]],_zU[2]];},function(_zZ){return E(_zQ);},_ce,_s1);};},_A0=unCStr("main"),_A1=unCStr("Main"),_A2=unCStr("Counter"),_A3=[0,I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_A0,_A1,_A2],_A4=function(_A5,_A6){var _A7=hs_leWord64(_A5,_A6);return E(_A7)==0?false:true;},_A8=function(_A9,_Aa,_Ab,_Ac){var _Ad=hs_eqWord64(_A9,_Ab);if(!E(_Ad)){var _Ae=hs_leWord64(_A9,_Ab);return E(_Ae)==0?false:true;}else{return _A4(_Aa,_Ac);}},_Af=function(_Ag,_Ah){var _Ai=E(_Ag),_Aj=_Ai[1],_Ak=_Ai[2],_Al=E(_Ah),_Am=_Al[1],_An=_Al[2],_Ao=hs_eqWord64(_Aj,_Am);if(!E(_Ao)){return !_A8(_Aj,_Ak,_Am,_An)?2:0;}else{var _Ap=hs_eqWord64(_Ak,_An);return E(_Ap)==0?!_A8(_Aj,_Ak,_Am,_An)?2:0:1;}},_Aq=unCStr("Failure in Data.Map.balanceL"),_Ar=new T(function(){return err(_Aq);}),_As=function(_At,_Au,_Av,_Aw){var _Ax=E(_Aw);if(!_Ax[0]){var _Ay=_Ax[1],_Az=E(_Av);if(!_Az[0]){var _AA=_Az[1],_AB=_Az[2],_AC=_Az[3];if(_AA<=(imul(3,_Ay)|0)){return [0,(1+_AA|0)+_Ay|0,E(E(_At)),_Au,E(_Az),E(_Ax)];}else{var _AD=E(_Az[4]);if(!_AD[0]){var _AE=_AD[1],_AF=E(_Az[5]);if(!_AF[0]){var _AG=_AF[1],_AH=_AF[2],_AI=_AF[3],_AJ=_AF[4];if(_AG>=(imul(2,_AE)|0)){var _AK=function(_AL){var _AM=E(_AF[5]);return _AM[0]==0?[0,(1+_AA|0)+_Ay|0,E(_AH),_AI,E([0,(1+_AE|0)+_AL|0,E(_AB),_AC,E(_AD),E(_AJ)]),E([0,(1+_Ay|0)+_AM[1]|0,E(E(_At)),_Au,E(_AM),E(_Ax)])]:[0,(1+_AA|0)+_Ay|0,E(_AH),_AI,E([0,(1+_AE|0)+_AL|0,E(_AB),_AC,E(_AD),E(_AJ)]),E([0,1+_Ay|0,E(E(_At)),_Au,E(_8),E(_Ax)])];},_AN=E(_AJ);return _AN[0]==0?_AK(_AN[1]):_AK(0);}else{return [0,(1+_AA|0)+_Ay|0,E(_AB),_AC,E(_AD),E([0,(1+_Ay|0)+_AG|0,E(E(_At)),_Au,E(_AF),E(_Ax)])];}}else{return E(_Ar);}}else{return E(_Ar);}}}else{return [0,1+_Ay|0,E(E(_At)),_Au,E(_8),E(_Ax)];}}else{var _AO=E(_Av);if(!_AO[0]){var _AP=_AO[1],_AQ=_AO[2],_AR=_AO[3],_AS=_AO[5],_AT=E(_AO[4]);if(!_AT[0]){var _AU=_AT[1],_AV=E(_AS);if(!_AV[0]){var _AW=_AV[1],_AX=_AV[2],_AY=_AV[3],_AZ=_AV[4];if(_AW>=(imul(2,_AU)|0)){var _B0=function(_B1){var _B2=E(_AV[5]);return _B2[0]==0?[0,1+_AP|0,E(_AX),_AY,E([0,(1+_AU|0)+_B1|0,E(_AQ),_AR,E(_AT),E(_AZ)]),E([0,1+_B2[1]|0,E(E(_At)),_Au,E(_B2),E(_8)])]:[0,1+_AP|0,E(_AX),_AY,E([0,(1+_AU|0)+_B1|0,E(_AQ),_AR,E(_AT),E(_AZ)]),E([0,1,E(E(_At)),_Au,E(_8),E(_8)])];},_B3=E(_AZ);return _B3[0]==0?_B0(_B3[1]):_B0(0);}else{return [0,1+_AP|0,E(_AQ),_AR,E(_AT),E([0,1+_AW|0,E(E(_At)),_Au,E(_AV),E(_8)])];}}else{return [0,3,E(_AQ),_AR,E(_AT),E([0,1,E(E(_At)),_Au,E(_8),E(_8)])];}}else{var _B4=E(_AS);return _B4[0]==0?[0,3,E(_B4[2]),_B4[3],E([0,1,E(_AQ),_AR,E(_8),E(_8)]),E([0,1,E(E(_At)),_Au,E(_8),E(_8)])]:[0,2,E(E(_At)),_Au,E(_AO),E(_8)];}}else{return [0,1,E(E(_At)),_Au,E(_8),E(_8)];}}},_B5=unCStr("Failure in Data.Map.balanceR"),_B6=new T(function(){return err(_B5);}),_B7=function(_B8,_B9,_Ba,_Bb){var _Bc=E(_Ba);if(!_Bc[0]){var _Bd=_Bc[1],_Be=E(_Bb);if(!_Be[0]){var _Bf=_Be[1],_Bg=_Be[2],_Bh=_Be[3];if(_Bf<=(imul(3,_Bd)|0)){return [0,(1+_Bd|0)+_Bf|0,E(E(_B8)),_B9,E(_Bc),E(_Be)];}else{var _Bi=E(_Be[4]);if(!_Bi[0]){var _Bj=_Bi[1],_Bk=_Bi[2],_Bl=_Bi[3],_Bm=_Bi[4],_Bn=E(_Be[5]);if(!_Bn[0]){var _Bo=_Bn[1];if(_Bj>=(imul(2,_Bo)|0)){var _Bp=function(_Bq){var _Br=E(_B8),_Bs=E(_Bi[5]);return _Bs[0]==0?[0,(1+_Bd|0)+_Bf|0,E(_Bk),_Bl,E([0,(1+_Bd|0)+_Bq|0,E(_Br),_B9,E(_Bc),E(_Bm)]),E([0,(1+_Bo|0)+_Bs[1]|0,E(_Bg),_Bh,E(_Bs),E(_Bn)])]:[0,(1+_Bd|0)+_Bf|0,E(_Bk),_Bl,E([0,(1+_Bd|0)+_Bq|0,E(_Br),_B9,E(_Bc),E(_Bm)]),E([0,1+_Bo|0,E(_Bg),_Bh,E(_8),E(_Bn)])];},_Bt=E(_Bm);return _Bt[0]==0?_Bp(_Bt[1]):_Bp(0);}else{return [0,(1+_Bd|0)+_Bf|0,E(_Bg),_Bh,E([0,(1+_Bd|0)+_Bj|0,E(E(_B8)),_B9,E(_Bc),E(_Bi)]),E(_Bn)];}}else{return E(_B6);}}else{return E(_B6);}}}else{return [0,1+_Bd|0,E(E(_B8)),_B9,E(_Bc),E(_8)];}}else{var _Bu=E(_Bb);if(!_Bu[0]){var _Bv=_Bu[1],_Bw=_Bu[2],_Bx=_Bu[3],_By=_Bu[5],_Bz=E(_Bu[4]);if(!_Bz[0]){var _BA=_Bz[1],_BB=_Bz[2],_BC=_Bz[3],_BD=_Bz[4],_BE=E(_By);if(!_BE[0]){var _BF=_BE[1];if(_BA>=(imul(2,_BF)|0)){var _BG=function(_BH){var _BI=E(_B8),_BJ=E(_Bz[5]);return _BJ[0]==0?[0,1+_Bv|0,E(_BB),_BC,E([0,1+_BH|0,E(_BI),_B9,E(_8),E(_BD)]),E([0,(1+_BF|0)+_BJ[1]|0,E(_Bw),_Bx,E(_BJ),E(_BE)])]:[0,1+_Bv|0,E(_BB),_BC,E([0,1+_BH|0,E(_BI),_B9,E(_8),E(_BD)]),E([0,1+_BF|0,E(_Bw),_Bx,E(_8),E(_BE)])];},_BK=E(_BD);return _BK[0]==0?_BG(_BK[1]):_BG(0);}else{return [0,1+_Bv|0,E(_Bw),_Bx,E([0,1+_BA|0,E(E(_B8)),_B9,E(_8),E(_Bz)]),E(_BE)];}}else{return [0,3,E(_BB),_BC,E([0,1,E(E(_B8)),_B9,E(_8),E(_8)]),E([0,1,E(_Bw),_Bx,E(_8),E(_8)])];}}else{var _BL=E(_By);return _BL[0]==0?[0,3,E(_Bw),_Bx,E([0,1,E(E(_B8)),_B9,E(_8),E(_8)]),E(_BL)]:[0,2,E(E(_B8)),_B9,E(_8),E(_Bu)];}}else{return [0,1,E(E(_B8)),_B9,E(_8),E(_8)];}}},_BM=function(_BN,_BO,_BP,_BQ,_BR,_BS){var _BT=E(_BS);if(!_BT[0]){var _BU=_BT[2],_BV=_BT[3],_BW=_BT[4],_BX=_BT[5];switch(_Af([0,_BN,_BO,_BP,_BQ],_BU)){case 0:return _As(_BU,_BV,_BM(_BN,_BO,_BP,_BQ,_BR,_BW),_BX);case 1:return [0,_BT[1],E([0,_BN,_BO,_BP,_BQ]),_BR,E(_BW),E(_BX)];default:return _B7(_BU,_BV,_BW,_BM(_BN,_BO,_BP,_BQ,_BR,_BX));}}else{return [0,1,E([0,_BN,_BO,_BP,_BQ]),_BR,E(_8),E(_8)];}},_BY=[0,_2M,_5m],_BZ=function(_C0,_){return [0,[0,_2M,[1,_C0]],_C0];},_C1=[1,_0],_C2=function(_C3){var _C4=new T(function(){return [0,E(_C3)[1]+1|0];}),_C5=new T(function(){return _58(_4O,new T(function(){return _54(_C3);}));});return function(_ce,_s1){return _4j(function(_C6,_){return [0,[0,_C5,_C1],_C6];},function(_C7,_C8,_){return (function(_C8,_){return _4j(_BZ,function(_C9){return function(_Ca,_){return [0,_BY,new T(function(){var _Cb=E(_C9);return [0,_Cb[1],_Cb[2],_Cb[3],_Cb[4],new T(function(){return _BM(I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_A3,_9,_C4,_Cb[5]);})];})];};},_C8,_);})(_C8,_);},_ce,_s1);};},_Cc=[0,I_fromBits([4029179641,2406453796]),I_fromBits([547056354,2957229436]),_A3,_9],_Cd=function(_Ce){return E(_Cc);},_Cf=function(_Cg,_Ch,_Ci,_Cj,_Ck){while(1){var _Cl=E(_Ck);if(!_Cl[0]){switch(_Af([0,_Cg,_Ch,_Ci,_Cj],_Cl[2])){case 0:_Ck=_Cl[4];continue;case 1:return [1,_Cl[3]];default:_Ck=_Cl[5];continue;}}else{return [0];}}},_Cm=function(_Cn,_Co){var _Cp=E(_Cn),_Cq=_Cp[1],_Cr=_Cp[2],_Cs=_Cp[3],_Ct=_Cp[4],_Cu=E(_Co);if(!_Cu[0]){switch(_Af(_Cp,_Cu[2])){case 0:return _Cf(_Cq,_Cr,_Cs,_Ct,_Cu[4]);case 1:return [1,_Cu[3]];default:return _Cf(_Cq,_Cr,_Cs,_Ct,_Cu[5]);}}else{return [0];}},_Cv=function(_Cw,_Cx,_Cy,_Cz){var _CA=E(_Cx),_CB=_CA[1],_CC=_CA[3],_CD=new T(function(){return A(_Cz,[_p3]);}),_CE=new T(function(){return A(_CC,[_a]);});return A(_CB,[new T(function(){return A(_CB,[_Cy,function(_CF){return A(_CC,[new T(function(){var _CG=E(_Cw);return E(E(_CF)[5]);})]);}]);}),function(_CH){var _CI=_Cm(_CD,_CH);return _CI[0]==0?E(_CE):A(_CC,[[1,_CI[1]]]);}]);},_CJ=new T(function(){return _Cv(_S,_3m,_Q,_Cd);}),_CK=function(_CL){var _CM=new T(function(){return _zO(_CL);});return function(_CN,_){var _CO=A(_CM,[_CN,_]),_CP=E(_CO),_CQ=E(_CP[1]),_CR=_4j(_yF,function(_CS){return function(_C8,_){return _4j(function(_CT,_){var _CU=A(_CJ,[_CT,_]);return [0,[0,_vv,new T(function(){var _CV=E(E(_CU)[1]);return _CV[0]==0?E([1,_CL]):E(_CV);})],new T(function(){return E(E(_CU)[2]);})];},_C2,_C8,_);};},_CP[2],_),_CW=E(_CR),_CX=E(_CW[1]),_CY=new T(function(){return _vm(_v6,function(_CZ,_){var _D0=A(_CQ[1],[_CZ,_]),_D1=A(_CX[1],[_CZ,_]);return _CZ;});});return [0,[0,function(_D2,_){var _D3=A(_vy,[_D2,_]),_D4=_5i(_D2,_),_D5=A(_CY,[_D2,_]);return _D2;},new T(function(){var _D6=E(_CQ[2]);return _D6[0]==0?E(_CX[2]):E(_D6);})],_CW[2]];};},_D7=new T(function(){return _CK(_4N);}),_D8=[0,4],_D9=function(_Da,_Db){return [1,_Db,new T(function(){return _D9(_Da,new T(function(){return A(_Da,[_Db]);}));})];},_Dc=[0,1],_Dd=[1,_Dc,_9],_De=[1,_5x,_9],_Df=function(_Dg,_Dh,_Di){var _Dj=E(_Dh);if(!_Dj[0]){return [0];}else{var _Dk=E(_Di);return _Dk[0]==0?[0]:[1,new T(function(){return A(_Dg,[_Dj[1],_Dk[1]]);}),new T(function(){return _Df(_Dg,_Dj[2],_Dk[2]);})];}},_Dl=function(_Dm){return _Df(_8Q,[1,_5x,_Dm],new T(function(){return _1v(_Dm,_De);}));},_Dn=new T(function(){return _D9(_Dl,_Dd);}),_Do=unCStr(" rows of the Pascal triangle "),_Dp=function(_Dq){var _Dr=new T(function(){return _2l(_o1,_Dq,_9);});return function(_ce,_s1){return _4O(_Dr,_ce,_s1);};},_Ds=unCStr("style"),_Dt=function(_Du,_Dv){var _Dw=new T(function(){return _4V(_Dp,_Du);});return [1,function(_Dx,_){var _Dy=A(_Dw,[_Dx,_]),_Dz=A(_1,[_n,_Dy,_Ds,_p,_]);return _Dy;},_Dv];},_DA=function(_DB,_DC){var _DD=E(_DB);if(!_DD[0]){return [0];}else{var _DE=_DD[1];return _DC>1?_Dt(_DE,new T(function(){return _DA(_DD[2],_DC-1|0);})):_Dt(_DE,_9);}},_DF=function(_DG){var _DH=new T(function(){return _4V(_4O,new T(function(){return unAppCStr("Show ",new T(function(){return _1v(_3u(0,E(_DG)[1],_9),_Do);}));}));});return function(_DI,_){return [0,[0,function(_DJ,_){var _DK=A(_DH,[_DJ,_]),_DL=_8o(new T(function(){var _DM=E(_DG)[1];return _DM>0?_DA(_Dn,_DM):[0];}),_DJ,_);return _DJ;},_a],_DI];};},_DN=new T(function(){return _DF(_D8);}),_DO=unCStr("This example draw a function of x between 10 and -10. You can define the function using javascript expressions"),_DP=new T(function(){return _4V(_4O,_DO);}),_DQ=function(_DR){var _DS=jsShow(E(_DR)[1]);return fromJSStr(_DS);},_DT=function(_DU){var _DV=new T(function(){return _DQ(_DU);});return function(_ce){return _1v(_DV,_ce);};},_DW=function(_DX,_DY,_DZ){var _E0=E(_DZ);if(!_E0[0]){return [0];}else{var _E1=_E0[2],_E2=E(_E0[1]);return _DX!=_E2[1]?[1,_E2,new T(function(){return _DW(_DX,_DY,_E1);})]:_1v(_DY,new T(function(){return _DW(_DX,_DY,_E1);}));}},_E3=[0,45],_E4=function(_E5,_E6,_E7){var _E8=new T(function(){return A(_E5,[[0, -_E7]]);}),_E9=new T(function(){return E(_E6)[1]<=6?function(_Ea){return [1,_E3,new T(function(){return A(_E8,[_Ea]);})];}:function(_Eb){return [1,_3t,[1,_E3,new T(function(){return A(_E8,[[1,_3s,_Eb]]);})]];};});if(_E7>=0){var _Ec=isDoubleNegativeZero(_E7);return E(_Ec)==0?A(_E5,[[0,_E7]]):E(_E9);}else{return E(_E9);}},_Ed=unCStr("canvas"),_Ee=unCStr("id"),_Ef=unCStr("canvas"),_Eg=function(_Eh,_Ei){var _Ej=new T(function(){return A(_Eh,[_Ei]);});return function(_Ek,_){var _El=jsCreateElem(toJSStr(E(_Ef))),_Em=jsAppendChild(_El,E(_Ek)[1]),_En=[0,_El],_Eo=A(_Ej,[_En,_]);return _En;};},_Ep=new T(function(){return _Eg(_v6,_2M);}),_Eq=function(_Er,_){var _Es=A(_Ep,[_Er,_]),_Et=A(_1,[_n,_Es,_Ee,_Ed,_]);return _Es;},_Eu=[0,_Eq,_C1],_Ev=function(_Ew,_){return [0,_Eu,_Ew];},_Ex=unCStr("Pattern match failure in do expression at main.hs:179:5-12"),_Ey=new T(function(){return [0,"(function(exp){ return eval(exp);})"];}),_Ez=new T(function(){return _yV(_Ey);}),_EA=function(_EB,_){var _EC=jsHasCtx2D(_EB);if(!E(_EC)){return _a;}else{var _ED=jsGetCtx2D(_EB);return [1,[0,[0,_ED],[0,_EB]]];}},_EE=function(_EF,_){return _EA(E(_EF)[1],_);},_EG=function(_EH,_EI){return A(_EH,[function(_){var _EJ=jsFind(toJSStr(E(_EI))),_EK=E(_EJ);return _EK[0]==0?_a:_EE(_EK[1],_);}]);},_EL=new T(function(){return _EG(_n,_Ed);}),_EM=[0,-10],_EN=[0,0],_EO=[0,_EM,_EN],_EP=[0,10],_EQ=[0,_EP,_EN],_ER=[1,_EQ,_9],_ES=[1,_EO,_ER],_ET=function(_EU,_){return _0;},_EV=function(_EW){var _EX=E(_EW);if(!_EX[0]){return E(_ET);}else{var _EY=E(_EX[1]);return function(_EZ,_){var _F0=E(_EZ)[1],_F1=jsMoveTo(_F0,E(_EY[1])[1],E(_EY[2])[1]);return (function(_F2,_){while(1){var _F3=E(_F2);if(!_F3[0]){return _0;}else{var _F4=E(_F3[1]),_F5=jsLineTo(_F0,E(_F4[1])[1],E(_F4[2])[1]);_F2=_F3[2];continue;}}})(_EX[2],_);};}},_F6=new T(function(){return _EV(_ES);}),_F7=[0,30],_F8=[0,_EN,_F7],_F9=[0,-30],_Fa=[0,_EN,_F9],_Fb=[1,_Fa,_9],_Fc=[1,_F8,_Fb],_Fd=new T(function(){return _EV(_Fc);}),_Fe=new T(function(){return [0,0/0];}),_Ff=new T(function(){return [0,-1/0];}),_Fg=new T(function(){return [0,1/0];}),_Fh=[0,0],_Fi=function(_Fj,_Fk){while(1){var _Fl=E(_Fj);if(!_Fl[0]){_Fj=[1,I_fromInt(_Fl[1])];continue;}else{var _Fm=E(_Fk);if(!_Fm[0]){_Fj=_Fl;_Fk=[1,I_fromInt(_Fm[1])];continue;}else{return I_fromRat(_Fl[1],_Fm[1]);}}}},_Fn=function(_Fo,_Fp){var _Fq=E(_Fo);if(!_Fq[0]){var _Fr=_Fq[1],_Fs=E(_Fp);return _Fs[0]==0?_Fr==_Fs[1]:I_compareInt(_Fs[1],_Fr)==0?true:false;}else{var _Ft=_Fq[1],_Fu=E(_Fp);return _Fu[0]==0?I_compareInt(_Ft,_Fu[1])==0?true:false:I_compare(_Ft,_Fu[1])==0?true:false;}},_Fv=function(_Fw,_Fx){var _Fy=E(_Fw);if(!_Fy[0]){var _Fz=_Fy[1],_FA=E(_Fx);return _FA[0]==0?_Fz<_FA[1]:I_compareInt(_FA[1],_Fz)>0;}else{var _FB=_Fy[1],_FC=E(_Fx);return _FC[0]==0?I_compareInt(_FB,_FC[1])<0:I_compare(_FB,_FC[1])<0;}},_FD=function(_FE,_FF){return !_Fn(_FF,_Fh)?[0,_Fi(_FE,_FF)]:!_Fn(_FE,_Fh)?!_Fv(_FE,_Fh)?E(_Fg):E(_Ff):E(_Fe);},_FG=function(_FH){var _FI=E(_FH);return _FD(_FI[1],_FI[2]);},_FJ=function(_FK){return [0,1/E(_FK)[1]];},_FL=function(_FM){var _FN=E(_FM),_FO=_FN[1];return _FO<0?[0, -_FO]:E(_FN);},_FP=function(_FQ){var _FR=E(_FQ);return _FR[0]==0?_FR[1]:I_toNumber(_FR[1]);},_FS=function(_FT){return [0,_FP(_FT)];},_FU=[0,0],_FV=[0,1],_FW=[0,-1],_FX=function(_FY){var _FZ=E(_FY)[1];return _FZ!=0?_FZ<=0?E(_FW):E(_FV):E(_FU);},_G0=function(_G1,_G2){return [0,E(_G1)[1]-E(_G2)[1]];},_G3=function(_G4){return [0, -E(_G4)[1]];},_G5=function(_G6,_G7){return [0,E(_G6)[1]+E(_G7)[1]];},_G8=function(_G9,_Ga){return [0,E(_G9)[1]*E(_Ga)[1]];},_Gb=[0,_G5,_G8,_G0,_G3,_FL,_FX,_FS],_Gc=function(_Gd,_Ge){return [0,E(_Gd)[1]/E(_Ge)[1]];},_Gf=[0,_Gb,_Gc,_FJ,_FG],_Gg=function(_Gh,_Gi){return E(_Gh)[1]!=E(_Gi)[1]?true:false;},_Gj=function(_Gk,_Gl){return E(_Gk)[1]==E(_Gl)[1];},_Gm=[0,_Gj,_Gg],_Gn=function(_Go,_Gp){return E(_Go)[1]<E(_Gp)[1];},_Gq=function(_Gr,_Gs){return E(_Gr)[1]<=E(_Gs)[1];},_Gt=function(_Gu,_Gv){return E(_Gu)[1]>E(_Gv)[1];},_Gw=function(_Gx,_Gy){return E(_Gx)[1]>=E(_Gy)[1];},_Gz=function(_GA,_GB){var _GC=E(_GA)[1],_GD=E(_GB)[1];return _GC>=_GD?_GC!=_GD?2:1:0;},_GE=function(_GF,_GG){var _GH=E(_GF),_GI=E(_GG);return _GH[1]>_GI[1]?E(_GH):E(_GI);},_GJ=function(_GK,_GL){var _GM=E(_GK),_GN=E(_GL);return _GM[1]>_GN[1]?E(_GN):E(_GM);},_GO=[0,_Gm,_Gz,_Gn,_Gw,_Gt,_Gq,_GE,_GJ],_GP=[0,1],_GQ=function(_GR){return E(E(_GR)[1]);},_GS=function(_GT){return E(E(_GT)[2]);},_GU=function(_GV){return E(E(_GV)[6]);},_GW=[0,2],_GX=function(_GY,_GZ){var _H0=E(_GZ);return [1,_H0,new T(function(){var _H1=_GQ(_GY);return _GX(_GY,A(_H1[1],[_H0,new T(function(){return A(_H1[7],[_GP]);})]));})];},_H2=function(_H3,_H4){var _H5=E(_H4);if(!_H5[0]){return [0];}else{var _H6=_H5[1];return !A(_H3,[_H6])?[0]:[1,_H6,new T(function(){return _H2(_H3,_H5[2]);})];}},_H7=function(_H8,_H9,_Ha,_Hb){var _Hc=new T(function(){return _GU(_H8);});return _H2(function(_Hd){return A(_Hc,[_Hd,new T(function(){var _He=_GQ(_H9),_Hf=_He[7];return A(_He[1],[_Hb,new T(function(){return A(_GS,[_H9,new T(function(){return A(_Hf,[_GP]);}),new T(function(){return A(_Hf,[_GW]);})]);})]);})]);},_GX(_H9,_Ha));},_Hg=new T(function(){return _H7(_GO,_Gf,_EM,_EP);}),_Hh=function(_Hi,_Hj){var _Hk=E(_Hi);if(!_Hk[0]){return [0];}else{var _Hl=E(_Hj);return _Hl[0]==0?[0]:[1,[0,_Hk[1],_Hl[1]],new T(function(){return _Hh(_Hk[2],_Hl[2]);})];}},_Hm=function(_Hn,_Ho,_){var _Hp=function(_Hq,_){var _Hr=E(_Hq);if(!_Hr[0]){return _9;}else{var _Hs=A(_Ez,[E(toJSStr(_DW(120,new T(function(){return A(_E4,[_DT,_p8,E(_Hr[1])[1],_9]);}),_Hn))),_]),_Ht=_Hp(_Hr[2],_);return [1,[0,_Hs],_Ht];}};return _4j(_Ev,function(_Hu,_C8,_){return (function(_Hv,_){return [0,[0,function(_Hw,_){var _Hx=A(_EL,[_]),_Hy=E(_Hx);if(!_Hy[0]){var _Hz=_2K(_Ex,_);return _Hw;}else{var _HA=_Hp(_Hg,_),_HB=E(_Hy[1]),_HC=jsResetCanvas(E(_HB[2])[1]),_HD=E(_HB[1]),_HE=_HD[1],_HF=jsPushState(_HE),_HG=jsScale(_HE,3,1),_HH=jsPushState(_HE),_HI=jsTranslate(_HE,50,130),_HJ=jsPushState(_HE),_HK=jsRotate(_HE,3.141592653589793),_HL=jsBeginPath(_HE),_HM=A(_F6,[_HD,_]),_HN=A(_Fd,[_HD,_]),_HO=A(_EV,[_Hh(_Hg,_HA),_HD,_]),_HP=jsStroke(_HE),_HQ=jsPopState(_HE),_HR=jsPopState(_HE),_HS=jsPopState(_HE);return _Hw;}},_C1],_Hv];})(_C8,_);},_Ho,_);},_HT=unCStr("Math.pow(x,2)+x+10;"),_HU=[1,_HT],_HV=new T(function(){return _rR(_8B,_9H,_p1,_yy,_wm);}),_HW=new T(function(){return A(_HV,[_a,_9G,_HU]);}),_HX=new T(function(){return _tl(_HW,_9F);}),_HY=function(_HZ,_){var _I0=A(_HX,[_HZ,_]),_I1=E(_I0),_I2=E(_I1[1]);return [0,[0,function(_I3,_){var _I4=A(_I2[1],[_I3,_]),_I5=_5i(_I3,_);return _I3;},new T(function(){var _I6=E(_I2[2]);return _I6[0]==0?E(_HU):E(_I6);})],_I1[2]];},_I7=function(_I8,_){var _I9=_4j(_HY,_Hm,_I8,_),_Ia=E(_I9),_Ib=E(_Ia[1]),_Ic=new T(function(){return _vm(_v6,_Ib[1]);});return [0,[0,function(_Id,_){var _Ie=A(_DP,[_Id,_]),_If=A(_Ic,[_Id,_]);return _Id;},_Ib[2]],_Ia[2]];},_Ig=unCStr("work?"),_Ih=function(_Ii,_Ij,_Ik){var _Il=E(_Ij);return A(_Il[1],[new T(function(){var _Im=E(_Ii);return E(_Ik);}),function(_In){return A(_Il[3],[[1,_3n,new T(function(){var _Io=E(_In);return _1v(_3u(0,E(_Io[2])[1],_9),_Io[1]);})]]);}]);},_Ip=function(_Iq){return E(E(_Iq)[15]);},_Ir=function(_Is){return E(E(_Is)[5]);},_It=unCStr("for"),_Iu=unCStr("label"),_Iv=function(_Iw,_Ix){var _Iy=new T(function(){return _8Y(_Ix);}),_Iz=new T(function(){return _90(_Iy);}),_IA=new T(function(){return _Ih([0,coercionToken],_3h(_Iw),function(_IB){return _oz(_Iw,_IB);});}),_IC=new T(function(){return _3f(_Iw);}),_ID=new T(function(){return _2P(_Iw);}),_IE=new T(function(){return _Ip(_Ix);}),_IF=new T(function(){return _2P(_Iw);}),_IG=new T(function(){return _Ir(_Ix);});return function(_IH,_II){var _IJ=new T(function(){return A(_IG,[_Iu,_IH]);});return function(_IK){return A(_IF,[new T(function(){return A(_IA,[_IK]);}),function(_IL){var _IM=new T(function(){return A(_IE,[_IJ,[1,[0,_It,new T(function(){return E(E(_IL)[1]);})],_9]]);});return A(_ID,[new T(function(){return A(_II,[new T(function(){return E(E(_IL)[2]);})]);}),function(_IN){var _IO=E(_IN),_IP=E(_IO[1]);return A(_IC,[[0,[0,new T(function(){return A(_Iz,[_IM,_IP[1]]);}),_IP[2]],_IO[2]]]);}]);}]);};};},_IQ=new T(function(){return _Iv(_2O,_8B);}),_IR=function(_IS,_IT,_){var _IU=jsGet(_IS,toJSStr(E(_IT)));return new T(function(){return fromJSStr(_IU);});},_IV=function(_IW,_IX,_){return _IR(E(_IW)[1],_IX,_);},_IY=unCStr("name"),_IZ=unCStr("true"),_J0=unCStr("radio"),_J1=new T(function(){return A(_p1,[_6D]);}),_J2=function(_J3,_J4,_J5,_J6){var _J7=new T(function(){return _oi(_J4);}),_J8=new T(function(){return _3z([0,coercionToken],_3h(_J7),function(_J9){return _oz(_J7,_J9);},function(_Ja,_Jb){return _oC(_J7,_Ja,_Jb);});}),_Jc=new T(function(){return _3f(_J7);}),_Jd=new T(function(){return _3f(_J7);}),_Je=new T(function(){return _2P(_J7);}),_Jf=new T(function(){return _2P(_J7);}),_Jg=new T(function(){return _3f(_J7);}),_Jh=new T(function(){return _2P(_J7);}),_Ji=new T(function(){return _3f(_J7);}),_Jj=new T(function(){return _2P(_J7);}),_Jk=new T(function(){return _r7(_J3);}),_Jl=new T(function(){return _Ip(_J3);}),_Jm=new T(function(){return _rd(_J6);});return function(_Jn,_Jo){return function(_Jp){return A(_Je,[new T(function(){return A(_J8,[_Jp]);}),function(_Jq){var _Jr=new T(function(){return E(E(_Jq)[1]);}),_Js=new T(function(){return _om(_J4,function(_){return jsFind(toJSStr(E(_Jr)));});});return A(_Jj,[new T(function(){var _Jt=new T(function(){return E(E(_Jq)[2]);});return A(_Ji,[[0,_Jt,_Jt]]);}),function(_Ju){return A(_Jh,[new T(function(){return A(_Jg,[[0,_0,new T(function(){var _Jv=E(E(_Ju)[1]);return [0,_Jv[1],_Jv[2],_r6,_Jv[4],_Jv[5]];})]]);}),function(_Jw){return A(_Jf,[new T(function(){return A(_Js,[new T(function(){return E(E(_Jw)[2]);})]);}),function(_Jx){return A(_Je,[new T(function(){var _Jy=E(_Jx),_Jz=_Jy[2],_JA=E(_Jy[1]);return _JA[0]==0?A(_Jd,[[0,_9,_Jz]]):A(_om,[_J4,function(_){return _IV(_JA[1],_6R,_);},_Jz]);}),function(_JB){var _JC=new T(function(){return !_sK(E(_JB)[1],_IZ)?[0]:E([1,_Jn]);});return A(_Jc,[[0,[0,new T(function(){return A(_Jl,[new T(function(){return A(_Jk,[_Jr,_J0,new T(function(){var _JD=A(_J5,[_Jn]),_JE=E(_J1),_JF=hs_eqWord64(_JD[1],_JE[1]);if(!E(_JF)){return A(_Jm,[_Jn]);}else{var _JG=hs_eqWord64(_JD[2],_JE[2]);return E(_JG)==0?A(_Jm,[_Jn]):E(_Jn);}}),new T(function(){return E(_JC)[0]==0?false:true;}),_a]);}),[1,[0,_IY,_Jo],_9]]);}),new T(function(){var _JH=E(_JC);return _JH[0]==0?[0]:[1,_JH[1]];})],new T(function(){return E(E(_JB)[2]);})]]);}]);}]);}]);}]);}]);};};},_JI=new T(function(){return _6E(_oU,_oZ);}),_JJ=new T(function(){return _yt(_yj);}),_JK=new T(function(){return _J2(_8B,_9H,_JI,_JJ);}),_JL=function(_JM,_JN){return A(_IQ,[function(_C8,_){return _4O(_JM,_C8,_);},new T(function(){return _tl(new T(function(){return A(_JK,[_JM,_JN]);}),_vz);})]);},_JO=function(_JP){return _JL(_Ig,_JP);},_JQ=unCStr("study?"),_JR=function(_JP){return _JL(_JQ,_JP);},_JS=[1,_JR,_9],_JT=[1,_JO,_JS],_JU=function(_JV){return E(E(_JV)[1]);},_JW=function(_JX,_JY){var _JZ=new T(function(){return _8Y(_JY);}),_K0=new T(function(){return _JU(_JZ);}),_K1=new T(function(){return _90(_JZ);}),_K2=function(_K3){var _K4=E(_K3);if(!_K4[0]){return [0,_K0,_a];}else{var _K5=E(_K4[1]),_K6=_K2(_K4[2]);return [0,new T(function(){return A(_K1,[_K5[1],_K6[1]]);}),new T(function(){var _K7=E(_K5[2]);return _K7[0]==0?E(_K6[2]):E(_K7);})];}},_K8=new T(function(){return _3f(_JX);}),_K9=new T(function(){return _3z([0,coercionToken],_3h(_JX),function(_Ka){return _oz(_JX,_Ka);},function(_Kb,_Kc){return _oC(_JX,_Kb,_Kc);});}),_Kd=new T(function(){return _3f(_JX);}),_Ke=new T(function(){return _2P(_JX);}),_Kf=new T(function(){return _2P(_JX);}),_Kg=new T(function(){return _2P(_JX);}),_Kh=new T(function(){return _2P(_JX);});return function(_Ki,_Kj){return A(_Kh,[new T(function(){return A(_K9,[_Kj]);}),function(_Kk){return A(_Kg,[new T(function(){var _Kl=new T(function(){return E(E(_Kk)[1]);}),_Km=function(_Kn){var _Ko=E(_Kn);if(!_Ko[0]){return function(_Kp){return A(_Kd,[[0,_9,_Kp]]);};}else{var _Kq=new T(function(){return _Km(_Ko[2]);}),_Kr=new T(function(){return A(_Ko[1],[_Kl]);});return function(_Ks){return A(_Kf,[new T(function(){return A(_Kr,[_Ks]);}),function(_Kt){var _Ku=new T(function(){return E(E(_Kt)[1]);});return A(_Ke,[new T(function(){return A(_Kq,[new T(function(){return E(E(_Kt)[2]);})]);}),function(_Kv){return A(_Kd,[[0,[1,_Ku,new T(function(){return E(E(_Kv)[1]);})],new T(function(){return E(E(_Kv)[2]);})]]);}]);}]);};}};return A(_Km,[_Ki,new T(function(){return E(E(_Kk)[2]);})]);}),function(_Kw){var _Kx=new T(function(){var _Ky=_K2(E(_Kw)[1]);return [0,_Ky[1],_Ky[2]];});return A(_K8,[[0,[0,new T(function(){return E(E(_Kx)[1]);}),new T(function(){var _Kz=E(E(_Kx)[2]);return _Kz[0]==0?[0]:[1,_Kz[1]];})],new T(function(){return E(E(_Kw)[2]);})]]);}]);}]);};},_KA=new T(function(){return _JW(_2O,_8B);}),_KB=new T(function(){return A(_KA,[_JT]);}),_KC=unCStr("Do you "),_KD=new T(function(){return _58(_4O,_KC);}),_KE=function(_KF,_){var _KG=A(_KB,[_KF,_]),_KH=E(_KG),_KI=E(_KH[1]);return [0,[0,function(_KJ,_){var _KK=A(_KD,[_KJ,_]),_KL=A(_KI[1],[_KJ,_]),_KM=_5i(_KJ,_);return _KJ;},_KI[2]],_KH[2]];},_KN=unCStr("ok"),_KO=[1,_KN],_KP=new T(function(){return A(_yA,[_KO]);}),_KQ=new T(function(){return _tl(_KP,_vz);}),_KR=unCStr("do you enjoy your work? "),_KS=new T(function(){return _58(_4O,_KR);}),_KT=function(_KU,_KV,_){return [0,[0,_2M,[1,_KU]],_KV];},_KW=function(_KX,_KY,_KZ,_){return _4j(_KX,function(_L0){return E(_KY);},_KZ,_);},_L1=function(_L2,_L3,_F,_){return _KW(_L2,_L3,_F,_);},_L4=function(_L5){return err(_L5);},_L6=[0,_4j,_L1,_KT,_L4],_L7=new T(function(){return _bu(_bh);}),_L8=function(_L9,_La,_Lb,_Lc,_Ld,_Le){var _Lf=new T(function(){return _90(_L9);});return A(_La,[new T(function(){return A(_Lc,[_Le]);}),function(_Lg){var _Lh=E(_Lg),_Li=E(_Lh[1]);return A(_La,[new T(function(){return A(_Ld,[_Lh[2]]);}),function(_Lj){var _Lk=E(_Lj),_Ll=E(_Lk[1]);return A(_Lb,[[0,[0,new T(function(){return A(_Lf,[_Li[1],_Ll[1]]);}),new T(function(){var _Lm=E(_Li[2]);return _Lm[0]==0?E(_Ll[2]):E(_Lm);})],_Lk[2]]]);}]);}]);},_Ln=function(_Lo,_Lp,_Lq,_Lr,_Ls,_Lt){var _Lu=new T(function(){return _Ip(_Lq);});return A(_Lo,[new T(function(){return A(_Lr,[_Lt]);}),function(_Lv){var _Lw=E(_Lv),_Lx=E(_Lw[1]);return A(_Lp,[[0,[0,new T(function(){return A(_Lu,[_Lx[1],_Ls]);}),_Lx[2]],_Lw[2]]]);}]);},_Ly=unCStr("main"),_Lz=unCStr("View"),_LA=unCStr("MFOption"),_LB=[0,I_fromBits([723427420,113737447]),I_fromBits([2464138301,1645412966]),_Ly,_Lz,_LA],_LC=[0,I_fromBits([723427420,113737447]),I_fromBits([2464138301,1645412966]),_LB,_9],_LD=function(_LE){return E(_LC);},_LF=unCStr("base"),_LG=unCStr("Maybe"),_LH=unCStr("Data.Maybe"),_LI=[0,I_fromBits([1880429361,33218723]),I_fromBits([3239333902,3165593319]),_LF,_LH,_LG],_LJ=[0,I_fromBits([1880429361,33218723]),I_fromBits([3239333902,3165593319]),_LI,_9],_LK=function(_LL){return E(_LJ);},_LM=function(_LN){return E(E(_LN)[12]);},_LO=new T(function(){return A(_p1,[_6D]);}),_LP=function(_LQ,_LR,_LS,_LT,_LU,_LV,_LW){var _LX=new T(function(){return A(_LM,[_LQ,new T(function(){var _LY=A(_LS,[_LU]),_LZ=E(_LO),_M0=hs_eqWord64(_LY[1],_LZ[1]);if(!E(_M0)){return A(_rd,[_LT,_LU]);}else{var _M1=hs_eqWord64(_LY[2],_LZ[2]);return E(_M1)==0?A(_rd,[_LT,_LU]):E(_LU);}}),_LV,_LW]);}),_M2=new T(function(){return _3f(_LR);});return function(_M3){return A(_M2,[[0,[0,_LX,[1,_LU]],_M3]]);};},_M4=function(_M5,_M6,_M7,_M8,_M9){var _Ma=new T(function(){return _Cv([0,coercionToken],_3h(_M5),function(_Mb){return _oz(_M5,_Mb);},new T(function(){return _6E(_LK,new T(function(){return _6E(_LD,_M8);}));}));}),_Mc=new T(function(){return _bi(_M7);}),_Md=new T(function(){return _2P(_M5);});return function(_Me,_Mf){var _Mg=new T(function(){return _LP(_M9,_M5,_M8,_M6,_Me,_Mf,_e8);});return function(_Mh){return A(_Md,[new T(function(){return A(_Ma,[_Mh]);}),function(_Mi){var _Mj=E(_Mi),_Mk=_Mj[2],_Ml=E(_Mj[1]);if(!_Ml[0]){return A(_Mg,[_Mk]);}else{var _Mm=E(_Ml[1]);return _Mm[0]==0?A(_Mg,[_Mk]):A(_LP,[_M9,_M5,_M8,_M6,_Me,_Mf,new T(function(){return A(_Mc,[_Me,_Mm[1]]);}),_Mk]);}}]);};};},_Mn=function(_Mo,_Mp,_Mq){var _Mr=E(_Mo),_Ms=_Mr[1],_Mt=_Mr[2],_Mu=_Mr[3],_Mv=_Mr[4],_Mw=E(_Mq);if(!_Mw[0]){var _Mx=_Mw[2],_My=_Mw[3],_Mz=_Mw[4],_MA=_Mw[5];switch(_Af(_Mr,_Mx)){case 0:return _As(_Mx,_My,_BM(_Ms,_Mt,_Mu,_Mv,_Mp,_Mz),_MA);case 1:return [0,_Mw[1],E(_Mr),_Mp,E(_Mz),E(_MA)];default:return _B7(_Mx,_My,_Mz,_BM(_Ms,_Mt,_Mu,_Mv,_Mp,_MA));}}else{return [0,1,E(_Mr),_Mp,E(_8),E(_8)];}},_MB=function(_MC){return E(E(_MC)[11]);},_MD=function(_ME,_MF,_MG,_MH){var _MI=new T(function(){return _oi(_MF);}),_MJ=new T(function(){return _3h(_MI);}),_MK=new T(function(){return _3z([0,coercionToken],_MJ,function(_ML){return _oz(_MI,_ML);},function(_MM,_MN){return _oC(_MI,_MM,_MN);});}),_MO=new T(function(){return _6E(_LD,_MG);}),_MP=new T(function(){return _6E(_LK,_MO);}),_MQ=new T(function(){return _3f(_MI);}),_MR=new T(function(){return _2P(_MI);}),_MS=new T(function(){return _3f(_MI);}),_MT=new T(function(){return _3f(_MI);}),_MU=new T(function(){return _2P(_MI);}),_MV=new T(function(){return _2P(_MI);}),_MW=new T(function(){return _2P(_MI);}),_MX=new T(function(){return _2P(_MI);}),_MY=new T(function(){return _2P(_MI);}),_MZ=new T(function(){return _MB(_ME);});return function(_N0,_N1){return A(_MY,[new T(function(){return A(_MK,[_N1]);}),function(_N2){var _N3=new T(function(){return E(E(_N2)[1]);}),_N4=new T(function(){return _qQ(_MJ,function(_N5){return _om(_MF,_N5);},_MG,_MH,_ME,_N3);});return A(_MX,[new T(function(){var _N6=new T(function(){return E(E(_N2)[2]);});return A(_MT,[[0,_N6,_N6]]);}),function(_N7){return A(_MV,[new T(function(){return A(_MS,[[0,_0,new T(function(){var _N8=E(E(_N7)[1]);return [0,_N8[1],_N8[2],_r6,_N8[4],_N8[5]];})]]);}),function(_N9){return A(_MW,[new T(function(){return A(_N4,[new T(function(){return E(E(_N9)[2]);})]);}),function(_Na){var _Nb=new T(function(){return E(E(_Na)[1]);});return A(_MV,[new T(function(){var _Nc=new T(function(){var _Nd=E(_Nb);return _Nd[0]==2?[1,_Nd[1]]:[0];}),_Ne=new T(function(){return A(_MP,[_Nc]);});return A(_MU,[new T(function(){var _Nf=new T(function(){return E(E(_Na)[2]);});return A(_MT,[[0,_Nf,_Nf]]);}),function(_Ng){return A(_MS,[[0,_0,new T(function(){var _Nh=E(E(_Ng)[1]);return [0,_Nh[1],_Nh[2],_Nh[3],_Nh[4],new T(function(){return _Mn(_Ne,_Nc,_Nh[5]);})];})]]);}]);}),function(_Ni){return A(_MR,[new T(function(){return A(_N0,[new T(function(){return E(E(_Ni)[2]);})]);}),function(_Nj){var _Nk=E(_Nj);return A(_MQ,[[0,[0,new T(function(){return A(_MZ,[_N3,E(_Nk[1])[1]]);}),new T(function(){var _Nl=E(_Nb);return _Nl[0]==2?[1,_Nl[1]]:[0];})],_Nk[2]]]);}]);}]);}]);}]);}]);}]);};},_Nm=[0,_7o,_IZ],_Nn=[1,_Nm,_9],_No=[0,_7o,_IZ],_Np=[1,_No,_9],_Nq=function(_Nr,_Ns,_Nt,_Nu){var _Nv=new T(function(){return _MD(_Nu,_Nt,_p1,_wm);}),_Nw=new T(function(){return A(_3f,[_Nr,_e8]);}),_Nx=new T(function(){return A(_3f,[_Nr,_e9]);}),_Ny=new T(function(){return _8Y(_Nu);}),_Nz=new T(function(){return _oi(_Nt);}),_NA=new T(function(){return _M4(_Nz,_yy,_L7,_p1,_Nu);}),_NB=new T(function(){return _oP(_Nu);}),_NC=new T(function(){return _2P(_Nr);});return function(_ND,_NE,_NF){return A(_NC,[new T(function(){var _NG=new T(function(){return !E(_ND)?E(_Np):[0];}),_NH=new T(function(){return A(_NA,[_NF,new T(function(){return A(_NB,[_NF]);})]);}),_NI=new T(function(){return !E(_ND)?[0]:E(_Nn);}),_NJ=new T(function(){return A(_NA,[_NE,new T(function(){return A(_NB,[_NE]);})]);});return A(_Nv,[function(_NK){var _NL=E(_Nz);return _L8(_Ny,_NL[1],_NL[3],function(_NM){var _NN=E(_Nz);return _Ln(_NN[1],_NN[3],_Nu,_NJ,_NI,_NM);},function(_NO){var _NP=E(_Nz);return _Ln(_NP[1],_NP[3],_Nu,_NH,_NG,_NO);},_NK);}]);}),function(_NQ){return !_sK(_NQ,_NE)?E(_Nw):E(_Nx);}]);};},_NR=new T(function(){return _Nq(_L6,_8K,_9H,_8B);}),_NS=unCStr("yes"),_NT=unCStr("no"),_NU=new T(function(){return A(_NR,[_e9,_NS,_NT]);}),_NV=function(_NW,_){var _NX=A(_NU,[_NW,_]),_NY=E(_NX),_NZ=E(_NY[1]),_O0=A(_KQ,[_NY[2],_]),_O1=E(_O0);return [0,[0,function(_O2,_){var _O3=A(_KS,[_O2,_]),_O4=A(_NZ[1],[_O2,_]),_O5=A(E(_O1[1])[1],[_O2,_]),_O6=_5i(_O2,_);return _O2;},new T(function(){var _O7=E(_NZ[2]);return _O7[0]==0?[0]:[1,[0,_O7[1]]];})],_O1[2]];},_O8=unCStr("do you study in "),_O9=new T(function(){return _58(_4O,_O8);}),_Oa=unCStr("University"),_Ob=function(_JP){return _JL(_Oa,_JP);},_Oc=unCStr("High School"),_Od=function(_JP){return _JL(_Oc,_JP);},_Oe=[1,_Od,_9],_Of=[1,_Ob,_Oe],_Og=new T(function(){return A(_KA,[_Of]);}),_Oh=function(_Oi,_){var _Oj=A(_Og,[_Oi,_]),_Ok=E(_Oj),_Ol=E(_Ok[1]);return [0,[0,function(_Om,_){var _On=A(_O9,[_Om,_]),_Oo=A(_Ol[1],[_Om,_]);return _Om;},new T(function(){var _Op=E(_Ol[2]);return _Op[0]==0?[0]:[1,[1,_Op[1]]];})],_Ok[2]];},_Oq=new T(function(){return _ax("main.hs:(264,11)-(271,64)|case");}),_Or=unCStr(" that you enjoy your work"),_Os=unCStr("False"),_Ot=new T(function(){return _1v(_Os,_Or);}),_Ou=unCStr("True"),_Ov=new T(function(){return _1v(_Ou,_Or);}),_Ow=[0,32],_Ox=function(_Oy,_Oz){var _OA=new T(function(){return _4V(_4O,new T(function(){return unAppCStr("You are ",new T(function(){return _1v(_Oy,[1,_Ow,_Oz]);}));}));});return function(_ce,_s1){return _4j(_KE,function(_OB){var _OC=new T(function(){return !_sK(_OB,_JQ)?!_sK(_OB,_Ig)?E(_Oq):E(_NV):E(_Oh);});return function(_ce,_s1){return _4j(_OC,function(_OD){return function(_OE,_){var _OF=A(new T(function(){var _OG=E(_OD);if(!_OG[0]){var _OH=new T(function(){return _4V(_4O,new T(function(){return unAppCStr("You work and it is ",new T(function(){return !E(_OG[1])?E(_Ot):E(_Ov);}));}));});return function(_OI,_){return [0,[0,function(_OJ,_){var _OK=A(_OH,[_OJ,_]);return _OJ;},_a],_OI];};}else{var _OL=new T(function(){return _4V(_4O,new T(function(){return unAppCStr("You study at the ",_OG[1]);}));});return function(_OM,_){return [0,[0,function(_ON,_){var _OO=A(_OL,[_ON,_]);return _ON;},_a],_OM];};}}),[_OE,_]),_OP=E(_OF),_OQ=E(_OP[1]);return [0,[0,function(_OR,_){var _OS=A(_OA,[_OR,_]),_OT=A(_OQ[1],[_OR,_]);return _OR;},_OQ[2]],_OP[2]];};},_ce,_s1);};},_ce,_s1);};},_OU=function(_OV){var _OW=E(_OV);return _Ox(_OW[1],_OW[2]);},_OX=unCStr("Who are you? "),_OY=new T(function(){return _4V(_4O,_OX);}),_OZ=unCStr("name"),_P0=unCStr("placeholder"),_P1=[0,_P0,_OZ],_P2=[1,_P1,_9],_P3=unCStr("surname"),_P4=[0,_P0,_P3],_P5=[1,_P4,_9],_P6=[1,_KN],_P7=new T(function(){return A(_yA,[_P6]);}),_P8=new T(function(){return _tl(_P7,_vz);}),_P9=new T(function(){return A(_HV,[_a,_9G,_a]);}),_Pa=new T(function(){return A(_HV,[_a,_9G,_a]);}),_Pb=function(_Pc,_){var _Pd=A(_Pa,[_Pc,_]),_Pe=E(_Pd),_Pf=E(_Pe[1]),_Pg=A(_P9,[_Pe[2],_]),_Ph=E(_Pg),_Pi=E(_Ph[1]),_Pj=A(_P8,[_Ph[2],_]),_Pk=E(_Pj),_Pl=new T(function(){return _y(_Pi[1],_P5);}),_Pm=new T(function(){return _y(_Pf[1],_P2);});return [0,[0,function(_Pn,_){var _Po=A(_OY,[_Pn,_]),_Pp=A(_Pm,[_Pn,_]),_Pq=_5i(_Pn,_),_Pr=A(_Pl,[_Pn,_]),_Ps=_5i(_Pn,_),_Pt=A(E(_Pk[1])[1],[_Pn,_]),_Pu=_5i(_Pn,_);return _Pn;},new T(function(){var _Pv=E(_Pf[2]);if(!_Pv[0]){return [0];}else{var _Pw=E(_Pi[2]);return _Pw[0]==0?[0]:[1,[0,_Pv[1],_Pw[1]]];}})],_Pk[2]];},_Px=unCStr("http://mflowdemo.herokuapp.com/noscript/monadicwidgets/combination"),_Py=unCStr("This formulary is the same than the one "),_Pz=[0,97],_PA=[1,_Pz,_9],_PB=function(_PC,_PD){var _PE=new T(function(){return A(_PC,[_PD]);});return function(_PF,_){var _PG=jsCreateElem(toJSStr(_PA)),_PH=jsAppendChild(_PG,E(_PF)[1]),_PI=[0,_PG],_PJ=A(_PE,[_PI,_]);return _PI;};},_PK=unCStr("run in the server by MFlow"),_PL=new T(function(){return _PB(_4O,_PK);}),_PM=unCStr("href"),_PN=function(_PO,_){var _PP=_4O(_Py,_PO,_),_PQ=A(_PL,[_PO,_]),_PR=A(_1,[_n,_PQ,_PM,_Px,_]);return _PO;},_PS=new T(function(){return _4V(_v6,_PN);}),_PT=unCStr("Fields of a formulary appear in sequence. Some trigger events instantly some others use a button to trigger them"),_PU=new T(function(){return _4V(_4O,_PT);}),_PV=function(_PW,_){var _PX=_4j(_Pb,_OU,_PW,_),_PY=E(_PX),_PZ=E(_PY[1]);return [0,[0,function(_Q0,_){var _Q1=A(_PU,[_Q0,_]),_Q2=A(_PS,[_Q0,_]),_Q3=A(_PZ[1],[_Q0,_]);return _Q0;},_PZ[2]],_PY[2]];},_Q4=[1,_5x],_Q5=unCStr("GalleryIndex"),_Q6=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_A0,_A1,_Q5],_Q7=[0,I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_Q6,_9],_Q8=function(_Q9){return E(_Q7);},_Qa=new T(function(){return _Cv(_S,_3m,_Q,_Q8);}),_Qb=function(_Qc,_){var _Qd=A(_Qa,[_Qc,_]);return [0,[0,_vv,new T(function(){var _Qe=E(E(_Qd)[1]);return _Qe[0]==0?E(_Q4):E(_Qe);})],new T(function(){return E(E(_Qd)[2]);})];},_Qf=unCStr("100%"),_Qg=[0,62],_Qh=[1,_Qg,_9],_Qi=[1,_Qh],_Qj=new T(function(){return A(_yA,[_Qi]);}),_Qk=new T(function(){return _tl(_Qj,_vz);}),_Ql=function(_Qm){return E(_Qk);},_Qn=unCStr("https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRAgKkpDyzk8kdIqk5ECsZ14XgbpBzyWFvrCrHombkSBAUn6jFo"),_Qo=[1,_Qn,_9],_Qp=unCStr("https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSfP70npv4FOrkBjScP0tVu2t3veSNoFQ6MMxX6LDO8kldNeu-DxQ"),_Qq=[1,_Qp,_Qo],_Qr=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcS53axpzkDyzEUAdaIP3YsaHuR-_YqN9qFK3W4bp_D2OBZfW5BU_Q"),_Qs=[1,_Qr,_Qq],_Qt=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQ_ywj-zxDq3h_B4l48XHsjTywrdbK5egxvhxkYJ1HOkDFXd_-H"),_Qu=[1,_Qt,_Qs],_Qv=unCStr("https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQmmC4kV3NPFIpGL_x4H_iHG_p-c93DGjWfkxVtjxEFVng7A8o-nw"),_Qw=[1,_Qv,_Qu],_Qx=unCStr("http://almaer.com/blog/uploads/interview-haskell.png"),_Qy=[1,_Qx,_Qw],_Qz=unCStr("height"),_QA=unCStr("img"),_QB=function(_QC,_){var _QD=jsCreateElem(toJSStr(E(_QA))),_QE=jsAppendChild(_QD,E(_QC)[1]);return [0,_QD];},_QF=function(_QG,_QH){while(1){var _QI=E(_QG);if(!_QI[0]){return E(_QH);}else{_QG=_QI[2];var _QJ=_QH+1|0;_QH=_QJ;continue;}}},_QK=new T(function(){return [0,_QF(_Qy,0)-1|0];}),_QL=[0,_2M,_5m],_QM=unCStr("src"),_QN=unCStr("width"),_QO=function(_QP){return function(_ce,_s1){return _4j(function(_C8,_){return _4j(_BZ,function(_QQ){return function(_QR,_){return [0,_QL,new T(function(){var _QS=E(_QQ);return [0,_QS[1],_QS[2],_QS[3],_QS[4],new T(function(){return _BM(I_fromBits([203033753,3200738202]),I_fromBits([3394053259,1065442867]),_Q6,_9,new T(function(){var _QT=E(_QP)[1];return _QT!=E(_QK)[1]?[0,_QT+1|0]:E(_5x);}),_QS[5]);})];})];};},_C8,_);},function(_QU,_C8,_){return (function(_C8,_){return _4j(function(_QV,_){return [0,[0,function(_QW,_){var _QX=_QB(_QW,_),_QY=A(_1,[_n,_QX,_QM,new T(function(){var _QZ=E(_QP)[1];return _QZ>=0?_wr(_Qy,_QZ):E(_wo);}),_]),_R0=A(_1,[_n,_QX,_QN,_Qf,_]),_R1=A(_1,[_n,_QX,_Qz,_Qf,_]),_R2=_5i(_QW,_);return _QW;},_C1],_QV];},_Ql,_C8,_);})(_C8,_);},_ce,_s1);};},_R3=function(_C8,_){return _4j(_Qb,_QO,_C8,_);},_R4=function(_R5,_R6,_){return _R7(_R6,_);},_R8=function(_C8,_){return _yY(_R3,_R4,_C8,_);},_R9=unCStr("this example show a image gallery. It advances each 20 seconds and by pressing the button"),_Ra=new T(function(){return _4V(_4O,_R9);}),_Rb=[0,20000],_Rc=new T(function(){return _3z(_S,_3m,_Q,_N);}),_Rd=function(_Re,_Rf,_Rg,_){var _Rh=A(_Rc,[_Rg,_]),_Ri=new T(function(){return E(E(_Rh)[1]);}),_Rj=new T(function(){return [0,_Rk];}),_Rk=function(_){var _Rl=jsFind(toJSStr(E(_Ri))),_Rm=E(_Rl);if(!_Rm[0]){return _0;}else{var _Rn=E(_Rm[1]),_Ro=jsClearChildren(_Rn[1]),_Rp=E(_m)[1],_Rq=takeMVar(_Rp),_Rr=A(_Rf,[_Rq,_]),_Rs=E(_Rr),_Rt=E(_Rs[1]),_=putMVar(_Rp,_Rs[2]),_Ru=A(_Rt[1],[_Rn,_]),_Rv=E(_Rt[2]);if(!_Rv[0]){var _Rw=jsSetTimeout(E(_Re)[1],E(_Rj)[1]);return _0;}else{var _Rx=E(_Rv[1]);return _0;}}},_Ry=jsSetTimeout(E(_Re)[1],E(_Rj)[1]);return _yH(_Ri,_Rf,new T(function(){return E(E(_Rh)[2]);}),_);},_R7=function(_Rz,_){var _RA=_Rd(_Rb,_R8,_Rz,_),_RB=E(_RA),_RC=E(_RB[1]);return [0,[0,function(_RD,_){var _RE=A(_Ra,[_RD,_]),_RF=A(_RC[1],[_RD,_]);return _RD;},_RC[2]],_RB[2]];},_RG=function(_RH,_RI,_RJ){return A(_RH,[[1,_2i,new T(function(){return A(_RI,[_RJ]);})]]);},_RK=unCStr("Key "),_RL=unCStr("Mouse "),_RM=unCStr("Click "),_RN=unCStr("NoData"),_RO=function(_RP){return _1v(_RN,_RP);},_RQ=unCStr(": empty list"),_RR=unCStr("Prelude."),_RS=function(_RT){return err(_1v(_RR,new T(function(){return _1v(_RT,_RQ);})));},_RU=unCStr("foldr1"),_RV=new T(function(){return _RS(_RU);}),_RW=function(_RX,_RY){var _RZ=E(_RY);if(!_RZ[0]){return E(_RV);}else{var _S0=_RZ[1],_S1=E(_RZ[2]);return _S1[0]==0?E(_S0):A(_RX,[_S0,new T(function(){return _RW(_RX,_S1);})]);}},_S2=[0,32],_S3=function(_S4,_S5){var _S6=E(_S5);switch(_S6[0]){case 0:return E(_RO);case 1:var _S7=function(_S8){return _3u(11,E(_S6[1])[1],[1,_S2,new T(function(){var _S9=E(_S6[2]);return [1,_3t,new T(function(){return A(_RW,[_RG,[1,function(_Sa){return _3u(0,E(_S9[1])[1],_Sa);},[1,function(_Sb){return _3u(0,E(_S9[2])[1],_Sb);},_9]],[1,_3s,_S8]]);})];})]);};return E(_S4)[1]<11?function(_Sc){return _1v(_RM,new T(function(){return _S7(_Sc);}));}:function(_Sd){return [1,_3t,new T(function(){return _1v(_RM,new T(function(){return _S7([1,_3s,_Sd]);}));})];};case 2:var _Se=function(_Sf){return _1v(_RL,new T(function(){var _Sg=E(_S6[1]);return [1,_3t,new T(function(){return A(_RW,[_RG,[1,function(_Sh){return _3u(0,E(_Sg[1])[1],_Sh);},[1,function(_Si){return _3u(0,E(_Sg[2])[1],_Si);},_9]],[1,_3s,_Sf]]);})];}));};return E(_S4)[1]<11?E(_Se):function(_Sj){return [1,_3t,new T(function(){return _Se([1,_3s,_Sj]);})];};default:var _Sk=_S6[1];return E(_S4)[1]<11?function(_Sl){return _1v(_RK,new T(function(){return _3u(11,E(_Sk)[1],_Sl);}));}:function(_Sm){return [1,_3t,new T(function(){return _1v(_RK,new T(function(){return _3u(11,E(_Sk)[1],[1,_3s,_Sm]);}));})];};}},_Sn=function(_So){var _Sp=new T(function(){return _4V(_4O,new T(function(){var _Sq=E(_So);return _1v(_Sq[1],[1,_Ow,new T(function(){return A(_S3,[_p8,_Sq[2],_9]);})]);}));});return function(_Sr,_){return [0,[0,_Sp,_C1],_Sr];};},_Ss=function(_){var _St=E(_sr)[1],_Su=takeMVar(_St),_=putMVar(_St,_Su);return _Su;},_Sv=function(_Sw,_){var _Sx=0;if(!E(_Sx)){var _Sy=_Ss();return [0,[0,_2M,[1,_Sy]],_Sw];}else{var _Sz=E(_sr)[1],_SA=takeMVar(_Sz),_=putMVar(_Sz,_SA);return [0,[0,_2M,[1,_SA]],_Sw];}},_SB=function(_C8,_){return _4j(_Sv,_Sn,_C8,_);},_SC=function(_SD){return E(_SB);},_SE=[12,coercionToken],_SF=[9,coercionToken],_SG=[11,coercionToken],_SH=[5,coercionToken],_SI=[10,coercionToken],_SJ=[6,coercionToken],_SK=[7,coercionToken],_SL=unCStr("height:100px;background-color:lightgreen;position:relative"),_SM=unCStr("div"),_SN=function(_SO,_SP){var _SQ=new T(function(){return A(_SO,[_SP]);});return function(_SR,_){var _SS=jsCreateElem(toJSStr(E(_SM))),_ST=jsAppendChild(_SS,E(_SR)[1]),_SU=[0,_SS],_SV=A(_SQ,[_SU,_]);return _SU;};},_SW=unCStr("h1"),_SX=function(_SY,_SZ){var _T0=new T(function(){return A(_SY,[_SZ]);});return function(_T1,_){var _T2=jsCreateElem(toJSStr(E(_SW))),_T3=jsAppendChild(_T2,E(_T1)[1]),_T4=[0,_T2],_T5=A(_T0,[_T4,_]);return _T4;};},_T6=unCStr("Mouse events here"),_T7=new T(function(){return _SX(_4O,_T6);}),_T8=new T(function(){return _SN(_v6,_T7);}),_T9=function(_Ta,_){var _Tb=A(_T8,[_Ta,_]),_Tc=A(_1,[_n,_Tb,_Ds,_SL,_]);return _Tb;},_Td=[0,_T9,_C1],_Te=function(_Tf,_){return [0,_Td,_Tf];},_Tg=new T(function(){return _tl(_Te,_SK);}),_Th=new T(function(){return _tl(_Tg,_SJ);}),_Ti=new T(function(){return _tl(_Th,_SI);}),_Tj=new T(function(){return _tl(_Ti,_SH);}),_Tk=new T(function(){return _tl(_Tj,_SG);}),_Tl=new T(function(){return _tl(_Tk,_vz);}),_Tm=new T(function(){return _tl(_Tl,_SF);}),_Tn=new T(function(){return _tl(_Tm,_SE);}),_To=unCStr("http://todomvc.com"),_Tp=unCStr("Work in progress for a todo application to be added to "),_Tq=unCStr("todomvc.com"),_Tr=new T(function(){return _PB(_4O,_Tq);}),_Ts=function(_Tt,_){var _Tu=_4O(_Tp,_Tt,_),_Tv=A(_Tr,[_Tt,_]),_Tw=A(_1,[_n,_Tv,_PM,_To,_]);return _Tt;},_Tx=new T(function(){return _4V(_v6,_Ts);}),_Ty=unCStr("Tasks"),_Tz=[0,I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_A0,_A1,_Ty],_TA=function(_TB,_TC,_TD,_){var _TE=A(_TC,[_TD,_]),_TF=E(_TE),_TG=E(_TF[1]),_TH=E(_TB),_TI=jsFind(toJSStr(_TH)),_TJ=E(_TI);if(!_TJ[0]){return _3N(_TH);}else{var _TK=E(_TJ[1]),_TL=jsClearChildren(_TK[1]),_TM=A(_TG[1],[_TK,_]);return [0,[0,_2M,_TG[2]],_TF[2]];}},_TN=[0,_2M,_5m],_TO=function(_TP,_){return [0,_TN,_TP];},_TQ=unCStr("Pattern match failure in do expression at main.hs:318:7-25"),_TR=new T(function(){return _L4(_TQ);}),_TS=function(_TT,_){var _TU=0;if(!E(_TU)){var _TV=_Ss();return [0,[0,_2M,[1,_TV]],_TT];}else{var _TW=E(_sr)[1],_TX=takeMVar(_TW),_=putMVar(_TW,_TX);return [0,[0,_2M,[1,_TX]],_TT];}},_TY=function(_TZ,_U0,_U1,_U2){return A(_TZ,[new T(function(){return function(_){var _U3=jsSet(E(_U0)[1],toJSStr(E(_U1)),toJSStr(E(_U2)));return _0;};})]);},_U4=unCStr("text"),_U5=unCStr("value"),_U6=new T(function(){return _6E(_oU,_oZ);}),_U7=new T(function(){return A(_U6,[_6D]);}),_U8=new T(function(){return A(_U6,[_6D]);}),_U9=unCStr("Prelude.read: ambiguous parse"),_Ua=unCStr("Prelude.read: no parse"),_Ub=function(_Uc){return [1,function(_Ud){return A(_ku,[_Ud,function(_Ue){return E([3,_Uc,_bK]);}]);}];},_Uf=function(_Ug){while(1){var _Uh=(function(_Ui){var _Uj=E(_Ui);if(!_Uj[0]){return [0];}else{var _Uk=_Uj[2],_Ul=E(_Uj[1]);if(!E(_Ul[2])[0]){return [1,_Ul[1],new T(function(){return _Uf(_Uk);})];}else{_Ug=_Uk;return null;}}})(_Ug);if(_Uh!=null){return _Uh;}}},_Um=function(_Un,_Uo){var _Up=_Uf(_aA(A(E(_Un)[3],[_mL,_Ub]),_Uo));return _Up[0]==0?err(_Ua):E(_Up[2])[0]==0?E(_Up[1]):err(_U9);},_Uq=function(_Ur,_Us,_Ut,_Uu){var _Uv=new T(function(){return _rd(_Us);}),_Uw=new T(function(){return _rR(_8B,_9H,_Ut,_Us,_Ur);});return [0,function(_Ux){return A(_Uw,[[1,_Uu],_U4,_Ux]);},function(_Uy,_){var _Uz=E(_Uu),_UA=jsFind(toJSStr(_Uz)),_UB=E(_UA);return _UB[0]==0?_3N(_Uz):A(_TY,[_n,_UB[1],_U5,new T(function(){var _UC=A(_Ut,[_Uy]),_UD=E(_U7),_UE=hs_eqWord64(_UC[1],_UD[1]);if(!E(_UE)){return A(_Uv,[_Uy]);}else{var _UF=hs_eqWord64(_UC[2],_UD[2]);return E(_UF)==0?A(_Uv,[_Uy]):E(_Uy);}}),_]);},function(_){var _UG=E(_Uu),_UH=jsFind(toJSStr(_UG)),_UI=E(_UH);if(!_UI[0]){return _3N(_UG);}else{var _UJ=_IR(E(_UI[1])[1],_U5,_);return new T(function(){var _UK=A(_U6,[_UJ]),_UL=E(_U8),_UM=hs_eqWord64(_UK[1],_UL[1]);if(!E(_UM)){return _Um(_Ur,_UJ);}else{var _UN=hs_eqWord64(_UK[2],_UL[2]);return E(_UN)==0?_Um(_Ur,_UJ):E(_UJ);}});}}];},_UO=unCStr("todo"),_UP=new T(function(){return _wk(_w6);}),_UQ=new T(function(){var _UR=_Uq(_UP,_JJ,_JI,_UO);return [0,_UR[1],_UR[2],_UR[3]];}),_US=new T(function(){var _UT=A(E(_UQ)[2],[_9]);return function(_UU,_){var _UV=A(_UT,[_]);return [0,[0,_2M,[1,_UV]],_UU];};}),_UW=[1,_9],_UX=[0,I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_Tz,_9],_UY=function(_UZ){return E(_UX);},_V0=new T(function(){return _Cv(_S,_3m,_Q,_UY);}),_V1=function(_V2,_){var _V3=A(_V0,[_V2,_]);return [0,[0,_vv,new T(function(){var _V4=E(E(_V3)[1]);return _V4[0]==0?E(_UW):E(_V4);})],new T(function(){return E(E(_V3)[2]);})];},_V5=[0,_2M,_5m],_V6=[0,_2M,_5m],_V7=function(_V8,_V9,_){return [0,_V6,_V9];},_Va=[0,_2M,_5m],_Vb=function(_Vc,_){return [0,_Va,_Vc];},_Vd=unCStr("list"),_Ve=unCStr("nocheck"),_Vf=[1,_Ve,_9],_Vg=[0,_Vf],_Vh=[1,_Vg],_Vi=unCStr("checkbox"),_Vj=function(_Vk,_Vl){var _Vm=new T(function(){return _oi(_Vl);}),_Vn=new T(function(){return _3z([0,coercionToken],_3h(_Vm),function(_Vo){return _oz(_Vm,_Vo);},function(_Vp,_Vq){return _oC(_Vm,_Vp,_Vq);});}),_Vr=new T(function(){return _3f(_Vm);}),_Vs=new T(function(){return _3f(_Vm);}),_Vt=new T(function(){return _2P(_Vm);}),_Vu=new T(function(){return _2P(_Vm);}),_Vv=new T(function(){return _3f(_Vm);}),_Vw=new T(function(){return _2P(_Vm);}),_Vx=new T(function(){return _3f(_Vm);}),_Vy=new T(function(){return _2P(_Vm);}),_Vz=new T(function(){return _r7(_Vk);});return function(_VA,_VB){return function(_VC){return A(_Vt,[new T(function(){return A(_Vn,[_VC]);}),function(_VD){var _VE=new T(function(){return E(E(_VD)[1]);}),_VF=new T(function(){return _om(_Vl,function(_){return jsFind(toJSStr(E(_VE)));});});return A(_Vy,[new T(function(){var _VG=new T(function(){return E(E(_VD)[2]);});return A(_Vx,[[0,_VG,_VG]]);}),function(_VH){return A(_Vw,[new T(function(){return A(_Vv,[[0,_0,new T(function(){var _VI=E(E(_VH)[1]);return [0,_VI[1],_VI[2],_r6,_VI[4],_VI[5]];})]]);}),function(_VJ){return A(_Vu,[new T(function(){return A(_VF,[new T(function(){return E(E(_VJ)[2]);})]);}),function(_VK){return A(_Vt,[new T(function(){var _VL=E(_VK),_VM=_VL[2],_VN=E(_VL[1]);return _VN[0]==0?A(_Vs,[[0,_9,_VM]]):A(_om,[_Vl,function(_){return _IV(_VN[1],_6R,_);},_VM]);}),function(_VO){var _VP=new T(function(){return !_sK(E(_VO)[1],_IZ)?[0]:E([1,_VB,_9]);});return A(_Vr,[[0,[0,new T(function(){return A(_Vz,[_VE,_Vi,_VB,new T(function(){return E(_VP)[0]==0?false:true;}),_a]);}),[1,[0,_VP]]],new T(function(){return E(E(_VO)[2]);})]]);}]);}]);}]);}]);}]);};};},_VQ=new T(function(){return _Vj(_8B,_9H);}),_VR=unCStr("check"),_VS=new T(function(){return A(_VQ,[_e8,_VR]);}),_VT=new T(function(){return _tl(_VS,_vz);}),_VU=function(_VV,_){var _VW=A(_VT,[_VV,_]),_VX=E(_VW),_VY=E(_VX[1]);return [0,[0,function(_VZ,_){var _W0=A(_VY[1],[_VZ,_]);return _VZ;},new T(function(){var _W1=E(_VY[2]);return _W1[0]==0?E(_Vh):E(_W1);})],_VX[2]];},_W2=unCStr("text-decoration:line-through;"),_W3=unCStr("li"),_W4=function(_W5,_W6){var _W7=new T(function(){return A(_W5,[_W6]);});return function(_W8,_){var _W9=jsCreateElem(toJSStr(E(_W3))),_Wa=jsAppendChild(_W9,E(_W8)[1]),_Wb=[0,_W9],_Wc=A(_W7,[_Wb,_]);return _Wb;};},_Wd=function(_We){var _Wf=new T(function(){return _58(_4O,_We);});return function(_Wg,_){var _Wh=_4j(_VU,function(_Wi){return (function(_Wj){var _Wk=E(_Wj);return _Wk[0]==0?function(_Wl,_){return [0,[0,_Wf,_C1],_Wl];}:!_sK(_Wk[1],_VR)?function(_Wm,_){return [0,[0,_Wf,_C1],_Wm];}:E(_Wk[2])[0]==0?function(_Wn,_){return [0,[0,function(_Wo,_){var _Wp=A(_Wf,[_Wo,_]),_Wq=A(_1,[_n,_Wp,_Ds,_W2,_]);return _Wp;},_C1],_Wn];}:function(_Wr,_){return [0,[0,_Wf,_C1],_Wr];};})(E(_Wi)[1]);},_Wg,_),_Ws=E(_Wh),_Wt=E(_Ws[1]);return [0,[0,new T(function(){return _W4(_v6,_Wt[1]);}),_Wt[2]],_Ws[2]];};},_Wu=function(_Wv){var _Ww=E(_Wv);return _Ww[0]==0?[0]:[1,new T(function(){return _Wd(_Ww[1]);}),new T(function(){return _Wu(_Ww[2]);})];},_Wx=function(_Wy,_Wz){while(1){var _WA=(function(_WB,_WC){var _WD=E(_WC);if(!_WD[0]){return E(_WB);}else{_Wy=function(_WE,_){var _WF=A(_WB,[_WE,_]),_WG=E(_WF),_WH=E(_WG[1]),_WI=A(_WD[1],[_WG[2],_]),_WJ=E(_WI),_WK=E(_WJ[1]);return [0,[0,function(_WL,_){var _WM=A(_WH[1],[_WL,_]),_WN=A(_WK[1],[_WL,_]);return _WL;},new T(function(){var _WO=E(_WH[2]);return _WO[0]==0?E(_WK[2]):E(_WO);})],_WJ[2]];};_Wz=_WD[2];return null;}})(_Wy,_Wz);if(_WA!=null){return _WA;}}},_WP=function(_WQ,_WR,_){return _4j(_TS,function(_WS){var _WT=E(E(_WS)[2]);return _WT[0]==3?E(E(_WT[1])[1])==13?function(_C8,_){return _4j(_US,function(_WU){return function(_C8,_){return _4j(_V1,function(_WV){var _WW=new T(function(){return _Wx(_Vb,_Wu([1,_WQ,_WV]));});return function(_ce,_s1){return _4j(function(_C8,_){return _4j(_BZ,function(_WX){return function(_WY,_){return [0,_V5,new T(function(){var _WZ=E(_WX);return [0,_WZ[1],_WZ[2],_WZ[3],_WZ[4],new T(function(){return _BM(I_fromBits([3561938990,657451105]),I_fromBits([3021302870,108592267]),_Tz,_9,[1,_WQ,_WV],_WZ[5]);})];})];};},_C8,_);},function(_X0,_C8,_){return (function(_C8,_){return _4j(function(_C8,_){return _TA(_Vd,_WW,_C8,_);},_V7,_C8,_);})(_C8,_);},_ce,_s1);};},_C8,_);};},_C8,_);}:E(_TO):E(_TR);},_WR,_);},_X1=new T(function(){return A(E(_UQ)[1],[_a]);}),_X2=new T(function(){return _tl(_X1,_9F);}),_X3=unCStr("todos"),_X4=new T(function(){return _SX(_4O,_X3);}),_X5=new T(function(){return _SN(_v6,_2M);}),_X6=function(_X7,_){var _X8=_4j(_X2,_WP,_X7,_),_X9=E(_X8),_Xa=E(_X9[1]),_Xb=new T(function(){return _vm(_v6,function(_Xc,_){var _Xd=A(_X4,[_Xc,_]),_Xe=A(_Xa[1],[_Xc,_]);return _Xc;});});return [0,[0,function(_Xf,_){var _Xg=A(_Xb,[_Xf,_]),_Xh=A(_X5,[_Xf,_]),_Xi=A(_1,[_n,_Xh,_Ee,_Vd,_]);return _Xf;},new T(function(){var _Xj=E(_Xa[2]);return _Xj[0]==0?E(_C1):E(_Xj);})],_X9[2]];},_Xk=function(_Xl,_Xm,_){return [0,[0,_2M,[1,[1,_Xl]]],_Xm];},_Xn=unCStr("revEntry"),_Xo=new T(function(){var _Xp=_Uq(_UP,_JJ,_JI,_Xn);return [0,_Xp[1],_Xp[2],_Xp[3]];}),_Xq=new T(function(){return A(E(_Xo)[1],[_a]);}),_Xr=new T(function(){return _tl(_Xq,_9F);}),_Xs=function(_Xt,_Xu,_){return [0,[0,_2M,[1,[0,_Xt]]],_Xu];},_Xv=unCStr("entry"),_Xw=new T(function(){var _Xx=_Uq(_UP,_JJ,_JI,_Xv);return [0,_Xx[1],_Xx[2],_Xx[3]];}),_Xy=new T(function(){return A(E(_Xw)[1],[_a]);}),_Xz=new T(function(){return _tl(_Xy,_9F);}),_XA=function(_XB,_){var _XC=_4j(_Xz,_Xs,_XB,_),_XD=E(_XC),_XE=E(_XD[1]),_XF=_4j(_Xr,_Xk,_XD[2],_),_XG=E(_XF),_XH=E(_XG[1]);return [0,[0,new T(function(){return _vm(_v6,function(_XI,_){var _XJ=A(_XE[1],[_XI,_]),_XK=_5i(_XI,_),_XL=A(_XH[1],[_XI,_]);return _XI;});}),new T(function(){var _XM=E(_XE[2]);return _XM[0]==0?E(_XH[2]):E(_XM);})],_XG[2]];},_XN=unCStr("To search palindromes: one box present the other\'s reversed. It is also an example of cell usage"),_XO=new T(function(){return _4V(_4O,_XN);}),_XP=function(_XQ){var _XR=A(E(_Xo)[2],[_XQ]);return function(_XS,_){var _XT=A(_XR,[_]);return [0,[0,_2M,[1,_XT]],_XS];};},_XU=function(_XV,_XW){while(1){var _XX=E(_XV);if(!_XX[0]){return E(_XW);}else{_XV=_XX[2];var _XY=[1,_XX[1],_XW];_XW=_XY;continue;}}},_XZ=function(_Y0){var _Y1=new T(function(){return _XU(_Y0,_9);});return function(_Y2,_){return [0,[0,_2M,[1,_Y1]],_Y2];};},_Y3=new T(function(){var _Y4=E(E(_Xw)[3]);return function(_Y5,_){var _Y6=A(_Y4,[_]);return [0,[0,_2M,[1,_Y6]],_Y5];};}),_Y7=function(_C8,_){return _4j(_Y3,_XZ,_C8,_);},_Y8=function(_C8,_){return _4j(_Y7,_XP,_C8,_);},_Y9=function(_Ya){var _Yb=A(E(_Xw)[2],[_Ya]);return function(_Yc,_){var _Yd=A(_Yb,[_]);return [0,[0,_2M,[1,_Yd]],_Yc];};},_Ye=new T(function(){var _Yf=E(E(_Xo)[3]);return function(_Yg,_){var _Yh=A(_Yf,[_]);return [0,[0,_2M,[1,_Yh]],_Yg];};}),_Yi=function(_Yj){var _Yk=new T(function(){return _XU(_Yj,_9);});return function(_Yl,_){return [0,[0,_2M,[1,_Yk]],_Yl];};},_Ym=function(_C8,_){return _4j(_Ye,_Yi,_C8,_);},_Yn=function(_C8,_){return _4j(_Ym,_Y9,_C8,_);},_Yo=function(_Yp){return E(_Yp)[0]==0?E(_Y8):E(_Yn);},_Yq=function(_Yr,_){var _Ys=_4j(_XA,_Yo,_Yr,_),_Yt=E(_Ys),_Yu=E(_Yt[1]);return [0,[0,function(_Yv,_){var _Yw=A(_XO,[_Yv,_]),_Yx=A(_Yu[1],[_Yv,_]);return _Yv;},_Yu[2]],_Yt[2]];},_Yy=unCStr("This widget sum recursively n numbers, but remember the previos entries when one entry is edited"),_Yz=new T(function(){return _4V(_4O,_Yy);}),_YA=function(_YB,_YC,_YD){var _YE=E(_YD);if(!_YE[0]){var _YF=_YE[3],_YG=_YE[4],_YH=_YE[5],_YI=E(_YE[2]),_YJ=_YI[1];return _YB>=_YJ?_YB!=_YJ?_B7(_YI,_YF,_YG,_YA(_YB,_YC,_YH)):[0,_YE[1],E([0,_YB]),_YC,E(_YG),E(_YH)]:_As(_YI,_YF,_YA(_YB,_YC,_YG),_YH);}else{return [0,1,E([0,_YB]),_YC,E(_8),E(_8)];}},_YK=function(_YL,_YM,_YN){var _YO=E(_YL),_YP=_YO[1],_YQ=E(_YN);if(!_YQ[0]){var _YR=_YQ[3],_YS=_YQ[4],_YT=_YQ[5],_YU=E(_YQ[2]),_YV=_YU[1];return _YP>=_YV?_YP!=_YV?_B7(_YU,_YR,_YS,_YA(_YP,_YM,_YT)):[0,_YQ[1],E(_YO),_YM,E(_YS),E(_YT)]:_As(_YU,_YR,_YA(_YP,_YM,_YS),_YT);}else{return [0,1,E(_YO),_YM,E(_8),E(_8)];}},_YW=function(_YX,_YY){while(1){var _YZ=E(_YY);if(!_YZ[0]){var _Z0=E(_YZ[2])[1];if(_YX>=_Z0){if(_YX!=_Z0){_YY=_YZ[5];continue;}else{return [1,_YZ[3]];}}else{_YY=_YZ[4];continue;}}else{return [0];}}},_Z1=[0,_2M,_a],_Z2=function(_Z3,_){return [0,_Z1,_Z3];},_Z4=unCStr("containers-0.5.5.1"),_Z5=unCStr("Data.Map.Base"),_Z6=unCStr("Map"),_Z7=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_Z4,_Z5,_Z6],_Z8=[0,I_fromBits([2800860092,98171937]),I_fromBits([2262449324,1391410843]),_Z7,_9],_Z9=function(_Za){return E(_Z8);},_Zb=function(_Zc){var _Zd=E(_Zc);if(!_Zd[0]){return [0];}else{var _Ze=E(_Zd[1]);return [1,[0,_Ze[1],_Ze[2]],new T(function(){return _Zb(_Zd[2]);})];}},_Zf=function(_Zg,_Zh){return function(_Zi){return E(new T(function(){var _Zj=A(_Zg,[_6D]),_Zk=E(_Zj[3]),_Zl=_Zk[1],_Zm=_Zk[2],_Zn=_1v(_Zj[4],[1,new T(function(){return A(_Zh,[_6D]);}),_9]);if(!_Zn[0]){return [0,_Zl,_Zm,_Zk,_9];}else{var _Zo=_68(new T(function(){return _5W(_6k(_6v,[1,[0,_Zl,_Zm],new T(function(){return _Zb(_Zn);})]));}));return [0,_Zo[1],_Zo[2],_Zk,_Zn];}}));};},_Zp=new T(function(){return _Zf(_Z9,_og);}),_Zq=new T(function(){return _6E(_Zp,_og);}),_Zr=new T(function(){return _Cv(_S,_3m,_Q,_Zq);}),_Zs=function(_Zt,_){var _Zu=A(_Zr,[_Zt,_]);return [0,[0,_2M,new T(function(){return E(E(_Zu)[1]);})],new T(function(){return E(E(_Zu)[2]);})];},_Zv=new T(function(){return _6E(_Zp,_og);}),_Zw=[1,_8],_Zx=new T(function(){return _Cv(_S,_3m,_Q,_Zv);}),_Zy=function(_Zz,_){var _ZA=A(_Zx,[_Zz,_]);return [0,[0,_vv,new T(function(){var _ZB=E(E(_ZA)[1]);return _ZB[0]==0?E(_Zw):E(_ZB);})],new T(function(){return E(E(_ZA)[2]);})];},_ZC=[0,_2M,_5m],_ZD=[1,_a],_ZE=function(_ZF,_ZG){var _ZH=new T(function(){return [0,E(_ZF)[1]+1|0];});return function(_ce,_s1){return _4j(function(_C8,_){return _4j(function(_ZI,_){var _ZJ=_4j(_Zs,function(_ZK){var _ZL=_YW(E(_ZF)[1],_ZK);return _ZL[0]==0?E(_Z2):function(_ZM,_){return [0,[0,_2M,_ZL],_ZM];};},_ZI,_),_ZN=E(_ZJ),_ZO=E(_ZN[1]);return [0,[0,function(_ZP,_){var _ZQ=A(_ZO[1],[_ZP,_]);return _ZP;},new T(function(){var _ZR=E(_ZO[2]);return _ZR[0]==0?E(_ZD):[1,_ZR];})],_ZN[2]];},function(_ZS){var _ZT=new T(function(){return _tl(new T(function(){return A(_s3,[_a,_9G,_ZS]);}),_9F);});return function(_ce,_s1){return _4j(function(_ZU,_){var _ZV=A(_ZT,[_ZU,_]),_ZW=E(_ZV),_ZX=_ZW[2],_ZY=E(_ZW[1]),_ZZ=_ZY[1],_100=_ZY[2],_101=E(_ZS);return _101[0]==0?[0,[0,function(_102,_){var _103=A(_ZZ,[_102,_]);return _102;},_100],_ZX]:[0,[0,function(_104,_){var _105=A(_ZZ,[_104,_]);return _104;},new T(function(){var _106=E(_100);return _106[0]==0?E(_101):E(_106);})],_ZX];},function(_107,_108,_){return _4j(function(_C8,_){return _4j(_Zy,function(_109){var _10a=new T(function(){return _YK(_ZF,_107,_109);}),_10b=new T(function(){return A(_Zv,[_10a]);});return function(_ce,_s1){return _4j(_BZ,function(_10c){return function(_10d,_){return [0,_ZC,new T(function(){var _10e=E(_10c);return [0,_10e[1],_10e[2],_10e[3],_10e[4],new T(function(){return _Mn(_10b,_10a,_10e[5]);})];})];};},_ce,_s1);};},_C8,_);},function(_10f,_C8,_){return (function(_10g,_){return [0,[0,_2M,[1,_107]],_10g];})(_C8,_);},_108,_);},_ce,_s1);};},_C8,_);},function(_10h){var _10i=new T(function(){return _ZE(_ZH,new T(function(){return _8Q(_ZG,_10h);}));}),_10j=new T(function(){return _58(_4O,new T(function(){return _3u(0,E(_ZG)[1]+E(_10h)[1]|0,_9);}));});return function(_ce,_s1){return _4j(function(_10k,_){return [0,[0,function(_10l,_){var _10m=A(_10j,[_10l,_]),_10n=_5i(_10l,_);return _10l;},_5m],_10k];},function(_10o){return E(_10i);},_ce,_s1);};},_ce,_s1);};},_10p=new T(function(){return _ZE(_5x,_5x);}),_10q=unCStr("This widget sum recursively n numbers. When enters 0, present the result"),_10r=new T(function(){return _4V(_4O,_10q);}),_10s=new T(function(){return A(_s3,[_a,_9G,_a]);}),_10t=new T(function(){return _tl(_10s,_9F);}),_10u=function(_10v){var _10w=new T(function(){return _58(_4O,new T(function(){return _54(_10v);}));});return function(_ce,_s1){return _4j(_10t,function(_10x){var _10y=E(E(_10x)[1]);if(!_10y){return function(_10z,_){return [0,[0,function(_10A,_){var _10B=_5i(_10A,_),_10C=_4O(_5n,_10A,_),_10D=A(_10w,[_10A,_]);return _10A;},_a],_10z];};}else{var _10E=new T(function(){return _10u(new T(function(){return [0,E(_10v)[1]+_10y|0];}));}),_10F=new T(function(){return _58(_4O,new T(function(){return _3u(0,E(_10v)[1]+_10y|0,_9);}));});return function(_ce,_s1){return _4j(function(_10G,_){return [0,[0,function(_10H,_){var _10I=A(_10F,[_10H,_]),_10J=_5i(_10H,_);return _10H;},_5m],_10G];},function(_10K){return E(_10E);},_ce,_s1);};}},_ce,_s1);};},_10L=new T(function(){return _10u(_5x);}),_10M=unCStr("This widget sum two numbers and append the result. Using applicative and monadic expressions"),_10N=new T(function(){return _4V(_4O,_10M);}),_10O=function(_10P){return function(_10Q,_){return [0,[0,new T(function(){var _10R=new T(function(){return _58(_4O,new T(function(){return _54(_10P);}));});return _4V(_v6,function(_10S,_){var _10T=_4O(_5n,_10S,_),_10U=A(_10R,[_10S,_]);return _10S;});}),_5m],_10Q];};},_10V=new T(function(){return A(_s3,[_a,_9G,_a]);}),_10W=new T(function(){return _tl(_10V,_9F);}),_10X=unCStr("second number "),_10Y=unCStr("first number"),_10Z=new T(function(){return A(_s3,[_a,_9G,_a]);}),_110=new T(function(){return _tl(_10Z,_9F);}),_111=function(_112,_){var _113=A(_10W,[_112,_]),_114=E(_113),_115=E(_114[1]),_116=A(_110,[_114[2],_]),_117=E(_116),_118=E(_117[1]);return [0,[0,function(_119,_){var _11a=_4O(_10Y,_119,_),_11b=_5i(_119,_),_11c=A(_115[1],[_119,_]),_11d=_5i(_119,_),_11e=_4O(_10X,_119,_),_11f=_5i(_119,_),_11g=A(_118[1],[_119,_]),_11h=_5i(_119,_);return _119;},new T(function(){var _11i=E(_115[2]);if(!_11i[0]){return [0];}else{var _11j=E(_118[2]);return _11j[0]==0?[0]:[1,new T(function(){return _8Q(_11i[1],_11j[1]);})];}})],_117[2]];},_11k=function(_11l,_){var _11m=_4j(_111,_10O,_11l,_),_11n=E(_11m),_11o=E(_11n[1]),_11p=new T(function(){return _4V(_v6,_11o[1]);});return [0,[0,function(_11q,_){var _11r=A(_10N,[_11q,_]),_11s=A(_11p,[_11q,_]);return _11q;},_11o[2]],_11n[2]];},_11t=unCStr("td"),_11u=function(_11v,_11w){var _11x=new T(function(){return A(_11v,[_11w]);});return function(_11y,_){var _11z=jsCreateElem(toJSStr(E(_11t))),_11A=jsAppendChild(_11z,E(_11y)[1]),_11B=[0,_11z],_11C=A(_11x,[_11B,_]);return _11B;};},_11D=unCStr("tr"),_11E=function(_11F,_11G){var _11H=new T(function(){return A(_11F,[_11G]);});return function(_11I,_){var _11J=jsCreateElem(toJSStr(E(_11D))),_11K=jsAppendChild(_11J,E(_11I)[1]),_11L=[0,_11J],_11M=A(_11H,[_11L,_]);return _11L;};},_11N=function(_11O,_){var _11P=_11k(_11O,_),_11Q=E(_11P),_11R=E(_11Q[1]),_11S=A(_vk,[_11Q[2],_]),_11T=E(_11S),_11U=E(_11T[1]),_11V=A(_10L,[_11T[2],_]),_11W=E(_11V),_11X=E(_11W[1]),_11Y=A(_D7,[_11W[2],_]),_11Z=E(_11Y),_120=E(_11Z[1]),_121=A(_10p,[_11Z[2],_]),_122=E(_121),_123=E(_122[1]),_124=A(_DN,[_122[2],_]),_125=E(_124),_126=E(_125[1]),_127=_PV(_125[2],_),_128=E(_127),_129=E(_128[1]),_12a=_Yq(_128[2],_),_12b=E(_12a),_12c=E(_12b[1]),_12d=_X6(_12b[2],_),_12e=E(_12d),_12f=E(_12e[1]),_12g=_I7(_12e[2],_),_12h=E(_12g),_12i=E(_12h[1]),_12j=_R7(_12h[2],_),_12k=E(_12j),_12l=E(_12k[1]),_12m=_4j(_Tn,_SC,_12k[2],_),_12n=E(_12m),_12o=E(_12n[1]);return [0,[0,function(_12p,_){var _12q=A(new T(function(){var _12r=new T(function(){return _11u(_v6,function(_12s,_){var _12t=A(_10r,[_12s,_]),_12u=A(_11X[1],[_12s,_]);return _12s;});}),_12v=new T(function(){return _11u(_v6,_11U[1]);}),_12w=new T(function(){return _11u(_v6,_11R[1]);});return _11E(_v6,function(_12x,_){var _12y=A(_12w,[_12x,_]),_12z=A(_1,[_n,_12y,_Ds,_4L,_]),_12A=A(_12v,[_12x,_]),_12B=A(_1,[_n,_12A,_Ds,_4L,_]),_12C=A(_12r,[_12x,_]),_12D=A(_1,[_n,_12C,_Ds,_4L,_]);return _12x;});}),[_12p,_]),_12E=A(_1,[_n,_12q,_Ds,_4M,_]),_12F=A(new T(function(){var _12G=new T(function(){return _11u(_v6,_126[1]);}),_12H=new T(function(){return _11u(_v6,function(_12I,_){var _12J=A(_Yz,[_12I,_]),_12K=A(_123[1],[_12I,_]);return _12I;});}),_12L=new T(function(){return _11u(_v6,_120[1]);});return _11E(_v6,function(_12M,_){var _12N=A(_12L,[_12M,_]),_12O=A(_1,[_n,_12N,_Ds,_4L,_]),_12P=A(_12H,[_12M,_]),_12Q=A(_1,[_n,_12P,_Ds,_4L,_]),_12R=A(_12G,[_12M,_]),_12S=A(_1,[_n,_12R,_Ds,_4L,_]);return _12M;});}),[_12p,_]),_12T=A(_1,[_n,_12F,_Ds,_4M,_]),_12U=A(new T(function(){var _12V=new T(function(){return _11u(_v6,function(_12W,_){var _12X=A(_Tx,[_12W,_]),_12Y=A(_12f[1],[_12W,_]);return _12W;});}),_12Z=new T(function(){return _11u(_v6,_12c[1]);}),_130=new T(function(){return _11u(_v6,new T(function(){return _vm(_v6,_129[1]);}));});return _11E(_v6,function(_131,_){var _132=A(_130,[_131,_]),_133=A(_1,[_n,_132,_Ds,_4L,_]),_134=A(_12Z,[_131,_]),_135=A(_1,[_n,_134,_Ds,_4L,_]),_136=A(_12V,[_131,_]),_137=A(_1,[_n,_136,_Ds,_4L,_]);return _131;});}),[_12p,_]),_138=A(_1,[_n,_12U,_Ds,_4M,_]),_139=A(new T(function(){var _13a=new T(function(){return _11u(_v6,_12o[1]);}),_13b=new T(function(){return _11u(_v6,_12l[1]);}),_13c=new T(function(){return _11u(_v6,_12i[1]);});return _11E(_v6,function(_13d,_){var _13e=A(_13c,[_13d,_]),_13f=A(_1,[_n,_13e,_Ds,_4L,_]),_13g=A(_13b,[_13d,_]),_13h=A(_1,[_n,_13g,_Ds,_4L,_]),_13i=A(_13a,[_13d,_]),_13j=A(_1,[_n,_13i,_Ds,_4L,_]);return _13d;});}),[_12p,_]),_13k=A(_1,[_n,_139,_Ds,_4M,_]);return _12p;},new T(function(){var _13l=E(_11R[2]);if(!_13l[0]){var _13m=E(_11U[2]);if(!_13m[0]){var _13n=E(_11X[2]);if(!_13n[0]){var _13o=E(_120[2]);if(!_13o[0]){var _13p=E(_123[2]);if(!_13p[0]){var _13q=E(_126[2]);if(!_13q[0]){var _13r=E(_129[2]);if(!_13r[0]){var _13s=E(_12c[2]);if(!_13s[0]){var _13t=E(_12f[2]);if(!_13t[0]){var _13u=E(_12i[2]);if(!_13u[0]){var _13v=E(_12l[2]);return _13v[0]==0?E(_12o[2]):E(_13v);}else{return E(_13u);}}else{return E(_13t);}}else{return E(_13s);}}else{return E(_13r);}}else{return E(_13q);}}else{return E(_13p);}}else{return E(_13o);}}else{return E(_13n);}}else{return E(_13m);}}else{return E(_13l);}})],_12n[2]];},_13w=unCStr("bottom of the page"),_13x=new T(function(){return _58(_4O,_13w);}),_13y=unCStr("border-collapse:collapse"),_13z=unCStr("hplayground examples"),_13A=new T(function(){return _SX(_4O,_13z);}),_13B=unCStr("idelem"),_13C=unCStr("h3"),_13D=function(_13E,_13F){var _13G=new T(function(){return A(_13E,[_13F]);});return function(_13H,_){var _13I=jsCreateElem(toJSStr(E(_13C))),_13J=jsAppendChild(_13I,E(_13H)[1]),_13K=[0,_13I],_13L=A(_13G,[_13K,_]);return _13K;};},_13M=unCStr("   "),_13N=unCStr("https://github.com/agocorona/hplayground"),_13O=unCStr("haskell-web.blogspot.com.es/2014/07/hplayground-translate-your-console.html"),_13P=unCStr("https://github.com/agocorona/hplayground/blob/master/src/Main.hs"),_13Q=unCStr("Article"),_13R=new T(function(){return _PB(_4O,_13Q);}),_13S=unCStr("Examples source code"),_13T=new T(function(){return _PB(_4O,_13S);}),_13U=unCStr("Git repository"),_13V=new T(function(){return _PB(_4O,_13U);}),_13W=function(_13X,_){var _13Y=A(_13V,[_13X,_]),_13Z=A(_1,[_n,_13Y,_PM,_13N,_]),_140=_4O(_13M,_13X,_),_141=A(_13T,[_13X,_]),_142=A(_1,[_n,_141,_PM,_13P,_]),_143=_4O(_13M,_13X,_),_144=A(_13R,[_13X,_]),_145=A(_1,[_n,_144,_PM,_13O,_]);return _13X;},_146=new T(function(){return _vm(_v6,_13W);}),_147=new T(function(){return _13D(_v6,_146);}),_148=unCStr("table"),_149=function(_14a,_14b){var _14c=new T(function(){return A(_14a,[_14b]);});return function(_14d,_){var _14e=jsCreateElem(toJSStr(E(_148))),_14f=jsAppendChild(_14e,E(_14d)[1]),_14g=[0,_14e],_14h=A(_14c,[_14g,_]);return _14g;};},_14i=function(_){var _14j=E(_13B),_14k=jsFind(toJSStr(_14j)),_14l=E(_14k);if(!_14l[0]){return _3N(_14j);}else{var _14m=_14l[1],_14n=E(_m)[1],_14o=takeMVar(_14n),_14p=_11N(_14o,_),_14q=E(_14p),_14r=E(_14q[1]),_=putMVar(_14n,_14q[2]),_14s=A(_13A,[_14m,_]),_14t=A(_1,[_n,_14s,_Ds,_p,_]),_14u=A(_147,[_14m,_]),_14v=A(_149,[_v6,_14r[1],_14m,_]),_14w=A(_1,[_n,_14v,_Ds,_13y,_]),_14x=A(_13x,[_14m,_]);return _14r[2];}},_14y=function(_){return _14i(_);};
var hasteMain = function() {A(_14y, [0]);};window.onload = hasteMain;