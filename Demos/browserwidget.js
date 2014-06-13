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

var _0=unCStr("id"),_1=0,_2=function(_3,_4,_5,_6){return A(_3,[new T(function(){return function(_){var _7=jsSetAttr(E(_4)[1],toJSStr(E(_5)),toJSStr(E(_6)));return _1;};})]);},_8=[0],_9=function(_a){return E(_a);},_b=unCStr("span"),_c=function(_d,_e,_){var _f=A(_d,[_]);return A(_e,[_]);},_g=function(_h,_i,_){return _c(_h,_i,_);},_j=function(_k,_l,_){var _m=A(_k,[_]);return A(_l,[_m,_]);},_n=unCStr("base"),_o=unCStr("GHC.IO.Exception"),_p=unCStr("IOException"),_q=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_n,_o,_p],_r=[0],_s=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_q,_r],_t=function(_u){return E(_s);},_v=function(_w){return E(E(_w)[1]);},_x=unCStr("Maybe.fromJust: Nothing"),_y=new T(function(){return err(_x);}),_z=function(_A,_B,_C){var _D=new T(function(){var _E=A(_A,[_C]),_F=A(_B,[new T(function(){var _G=E(_D);return _G[0]==0?E(_y):E(_G[1]);})]),_H=hs_eqWord64(_E[1],_F[1]);if(!E(_H)){return [0];}else{var _I=hs_eqWord64(_E[2],_F[2]);return E(_I)==0?[0]:[1,_C];}});return E(_D);},_J=function(_K){var _L=E(_K);return _z(_v(_L[1]),_t,_L[2]);},_M=unCStr(": "),_N=[0,41],_O=unCStr(" ("),_P=function(_Q,_R){var _S=E(_Q);return _S[0]==0?E(_R):[1,_S[1],new T(function(){return _P(_S[2],_R);})];},_T=unCStr("already exists"),_U=unCStr("does not exist"),_V=unCStr("protocol error"),_W=unCStr("failed"),_X=unCStr("invalid argument"),_Y=unCStr("inappropriate type"),_Z=unCStr("hardware fault"),_10=unCStr("unsupported operation"),_11=unCStr("timeout"),_12=unCStr("resource vanished"),_13=unCStr("interrupted"),_14=unCStr("resource busy"),_15=unCStr("resource exhausted"),_16=unCStr("end of file"),_17=unCStr("illegal operation"),_18=unCStr("permission denied"),_19=unCStr("user error"),_1a=unCStr("unsatisified constraints"),_1b=unCStr("system error"),_1c=function(_1d,_1e){switch(E(_1d)){case 0:return _P(_T,_1e);case 1:return _P(_U,_1e);case 2:return _P(_14,_1e);case 3:return _P(_15,_1e);case 4:return _P(_16,_1e);case 5:return _P(_17,_1e);case 6:return _P(_18,_1e);case 7:return _P(_19,_1e);case 8:return _P(_1a,_1e);case 9:return _P(_1b,_1e);case 10:return _P(_V,_1e);case 11:return _P(_W,_1e);case 12:return _P(_X,_1e);case 13:return _P(_Y,_1e);case 14:return _P(_Z,_1e);case 15:return _P(_10,_1e);case 16:return _P(_11,_1e);case 17:return _P(_12,_1e);default:return _P(_13,_1e);}},_1f=[0,125],_1g=unCStr("{handle: "),_1h=function(_1i,_1j,_1k,_1l,_1m,_1n){var _1o=new T(function(){var _1p=new T(function(){return _1c(_1j,new T(function(){var _1q=E(_1l);return _1q[0]==0?E(_1n):_P(_O,new T(function(){return _P(_1q,[1,_N,_1n]);}));}));}),_1r=E(_1k);return _1r[0]==0?E(_1p):_P(_1r,new T(function(){return _P(_M,_1p);}));}),_1s=E(_1m);if(!_1s[0]){var _1t=E(_1i);if(!_1t[0]){return E(_1o);}else{var _1u=E(_1t[1]);return _1u[0]==0?_P(_1g,new T(function(){return _P(_1u[1],[1,_1f,new T(function(){return _P(_M,_1o);})]);})):_P(_1g,new T(function(){return _P(_1u[1],[1,_1f,new T(function(){return _P(_M,_1o);})]);}));}}else{return _P(_1s[1],new T(function(){return _P(_M,_1o);}));}},_1v=function(_1w){var _1x=E(_1w);return _1h(_1x[1],_1x[2],_1x[3],_1x[4],_1x[6],_r);},_1y=function(_1z,_1A){var _1B=E(_1z);return _1h(_1B[1],_1B[2],_1B[3],_1B[4],_1B[6],_1A);},_1C=[0,44],_1D=[0,93],_1E=[0,91],_1F=function(_1G,_1H,_1I){var _1J=E(_1H);return _1J[0]==0?unAppCStr("[]",_1I):[1,_1E,new T(function(){return A(_1G,[_1J[1],new T(function(){var _1K=function(_1L){var _1M=E(_1L);return _1M[0]==0?E([1,_1D,_1I]):[1,_1C,new T(function(){return A(_1G,[_1M[1],new T(function(){return _1K(_1M[2]);})]);})];};return _1K(_1J[2]);})]);})];},_1N=function(_1O,_1P){return _1F(_1y,_1O,_1P);},_1Q=function(_1R,_1S,_1T){var _1U=E(_1S);return _1h(_1U[1],_1U[2],_1U[3],_1U[4],_1U[6],_1T);},_1V=[0,_1Q,_1v,_1N],_1W=new T(function(){return [0,_t,_1V,_1X,_J];}),_1X=function(_1Y){return [0,_1W,_1Y];},_1Z=7,_20=function(_21){return [0,_8,_1Z,_r,_21,_8,_8];},_22=function(_23,_){return die(new T(function(){return _1X(new T(function(){return _20(_23);}));}));},_24=function(_25,_){return _22(_25,_);},_26=function(_27,_){return _27;},_28=[0,_j,_g,_26,_24],_29=function(_2a){return E(E(_2a)[1]);},_2b=function(_2c,_2d,_2e,_2f){return A(_29,[_2c,new T(function(){return A(_2d,[_2f]);}),function(_2g){return A(_2e,[new T(function(){return E(E(_2g)[1]);}),new T(function(){return E(E(_2g)[2]);})]);}]);},_2h=function(_2i,_2j,_2k,_2l){return A(_29,[_2i,new T(function(){return A(_2j,[_2l]);}),function(_2m){return A(_2k,[new T(function(){return E(E(_2m)[2]);})]);}]);},_2n=function(_2o,_2p,_2q,_2r){return _2h(_2o,_2p,_2q,_2r);},_2s=function(_2t){return E(E(_2t)[4]);},_2u=function(_2v,_2w){var _2x=new T(function(){return A(_2s,[_2v,_2w]);});return function(_2y){return E(_2x);};},_2z=function(_2A){return E(E(_2A)[3]);},_2B=function(_2C){var _2D=new T(function(){return _2z(_2C);});return [0,function(_2p,_2q,_2r){return _2b(_2C,_2p,_2q,_2r);},function(_2p,_2q,_2r){return _2n(_2C,_2p,_2q,_2r);},function(_2E,_2F){return A(_2D,[[0,_2E,_2F]]);},function(_2r){return _2u(_2C,_2r);}];},_2G=new T(function(){return _2B(_28);}),_2H=function(_2I,_2J){var _2K=jsShowI(_2I);return _P(fromJSStr(_2K),_2J);},_2L=[0,41],_2M=[0,40],_2N=function(_2O,_2P,_2Q){return _2P>=0?_2H(_2P,_2Q):_2O<=6?_2H(_2P,_2Q):[1,_2M,new T(function(){var _2R=jsShowI(_2P);return _P(fromJSStr(_2R),[1,_2L,_2Q]);})];},_2S=[0,112],_2T=function(_2U,_2V,_2W,_2X){var _2Y=E(_2V);return A(_2Y[1],[new T(function(){var _2Z=E(_2U);return E(_2W);}),function(_30){var _31=new T(function(){return E(E(_30)[2]);});return A(_2Y[2],[new T(function(){return A(_2X,[new T(function(){var _32=E(new T(function(){var _33=E(_2U);return [0,coercionToken];})),_34=E(_30);return [0,_34[1],new T(function(){return [0,E(_31)[1]+1|0];}),_34[3],_34[4],_34[5]];})]);}),new T(function(){return A(_2Y[3],[[1,_2S,new T(function(){return _P(_2N(0,E(_31)[1],_r),new T(function(){return E(E(_30)[1]);}));})]]);})]);}]);},_35=[0,coercionToken],_36=function(_37,_){return [0,_37,_37];},_38=function(_39,_3a,_){return [0,_1,_39];},_3b=new T(function(){return _2T(_35,_2G,_36,_38);}),_3c=[0,0],_3d=2,_3e=function(_3f,_3g,_){var _3h=A(_3f,[[0,_r,_3c,_3d,function(_){return _3e(_3f,_3g,_);},_r],_]),_3i=A(E(E(_3h)[1])[1],[_3g,_]);return _1;},_3j=unCStr(" could be found!"),_3k=function(_3l){return err(unAppCStr("No element with ID ",new T(function(){return _P(_3l,_3j);})));},_3m=function(_3n,_3o,_){var _3p=E(_3o),_3q=jsFind(toJSStr(_3p)),_3r=E(_3q);if(!_3r[0]){return _3k(_3p);}else{var _3s=E(_3r[1]),_3t=jsClearChildren(_3s[1]);return _3e(_3n,_3s,_);}},_3u=function(_3v,_3w,_3x,_){var _3y=A(_3b,[_3x,_]),_3z=new T(function(){return E(E(_3y)[1]);}),_3A=A(_3v,[new T(function(){var _3B=E(E(_3y)[2]);return [0,_3B[1],_3B[2],_3B[3],function(_){return _3m(function(_3C,_){return _3u(function(_3D,_){var _3E=A(_3v,[_3B,_]),_3F=E(_3E);return [0,[0,_26,E(_3F[1])[2]],_3F[2]];},_3w,_3C,_);},_3z,_);},_3B[5]];}),_]),_3G=E(_3A),_3H=_3G[2],_3I=E(_3G[1]),_3J=_3I[1],_3K=E(_3I[2]);if(!_3K[0]){return [0,[0,function(_3L,_){var _3M=A(_3J,[_3L,_]),_3N=E(_3z),_3O=jsFind(toJSStr(_3N));if(!E(_3O)[0]){var _3P=jsCreateElem(toJSStr(E(_b))),_3Q=A(_2,[_9,[0,_3P],_0,_3N,_]),_3R=E(_3L),_3S=jsAppendChild(_3P,_3R[1]);return _3R;}else{return _3L;}},_8],_3H];}else{var _3T=A(_3w,[_3K[1],_3H,_]),_3U=E(_3T),_3V=E(_3U[1]),_3W=_3V[1];return [0,[0,function(_3X,_){var _3Y=A(_3J,[_3X,_]),_3Z=E(_3z),_40=jsFind(toJSStr(_3Z));if(!E(_40)[0]){var _41=jsCreateElem(toJSStr(E(_b))),_42=A(_2,[_9,[0,_41],_0,_3Z,_]),_43=E(_3X),_44=jsAppendChild(_41,_43[1]),_45=A(_3W,[_43,_]);return _43;}else{var _46=A(_3W,[_3X,_]);return _3X;}},_3V[2]],_3U[2]];}},_47=function(_48,_49,_){var _4a=jsCreateTextNode(toJSStr(E(_48))),_4b=jsAppendChild(_4a,E(_49)[1]);return [0,_4a];},_4c=unCStr("This widget sums three numbers and append the result"),_4d=[0,112],_4e=[1,_4d,_r],_4f=function(_4g,_4h){var _4i=new T(function(){return A(_4g,[_4h]);});return function(_4j,_){var _4k=jsCreateElem(toJSStr(_4e)),_4l=jsAppendChild(_4k,E(_4j)[1]),_4m=[0,_4k],_4n=A(_4i,[_4m,_]);return _4m;};},_4o=new T(function(){return _4f(_47,_4c);}),_4p=function(_4q){return _2N(0,E(_4q)[1],_r);},_4r=[0,98],_4s=[1,_4r,_r],_4t=function(_4u,_4v){var _4w=new T(function(){return A(_4u,[_4v]);});return function(_4x,_){var _4y=jsCreateElem(toJSStr(_4s)),_4z=jsAppendChild(_4y,E(_4x)[1]),_4A=[0,_4y],_4B=A(_4w,[_4A,_]);return _4A;};},_4C=unCStr("result: "),_4D=function(_4E){return E(_4E);},_4F=function(_4G){return function(_4H,_){return [0,[0,new T(function(){var _4I=new T(function(){return _4t(_47,new T(function(){return _4p(_4G);}));});return _4f(_4D,function(_4J,_){var _4K=_47(_4C,_4J,_),_4L=A(_4I,[_4J,_]);return _4J;});}),_8],_4H];};},_4M=[13,coercionToken],_4N=function(_4O,_4P){return [0,E(_4O)[1]+E(_4P)[1]|0];},_4Q=unCStr("true"),_4R=unCStr("hasevent"),_4S=function(_4T,_4U){while(1){var _4V=E(_4T);if(!_4V[0]){return E(_4U)[0]==0?true:false;}else{var _4W=E(_4U);if(!_4W[0]){return false;}else{if(E(_4V[1])[1]!=E(_4W[1])[1]){return false;}else{_4T=_4V[2];_4U=_4W[2];continue;}}}}},_4X=new T(function(){return [0,"keydown"];}),_4Y=new T(function(){return [0,"mousemove"];}),_4Z=new T(function(){return [0,"blur"];}),_50=new T(function(){return [0,"focus"];}),_51=new T(function(){return [0,"change"];}),_52=new T(function(){return [0,"unload"];}),_53=new T(function(){return [0,"load"];}),_54=new T(function(){return [0,"keyup"];}),_55=new T(function(){return [0,"keypress"];}),_56=new T(function(){return [0,"mouseup"];}),_57=new T(function(){return [0,"mousedown"];}),_58=new T(function(){return [0,"dblclick"];}),_59=new T(function(){return [0,"click"];}),_5a=new T(function(){return [0,"mouseout"];}),_5b=new T(function(){return [0,"mouseover"];}),_5c=function(_5d){switch(E(_5d)[0]){case 0:return E(_53);case 1:return E(_52);case 2:return E(_51);case 3:return E(_50);case 4:return E(_4Z);case 5:return E(_4Y);case 6:return E(_5b);case 7:return E(_5a);case 8:return E(_59);case 9:return E(_58);case 10:return E(_57);case 11:return E(_56);case 12:return E(_55);case 13:return E(_54);default:return E(_4X);}},_5e=function(_5f,_5g,_5h,_5i,_){var _5j=A(_5f,[_5i,_]),_5k=E(_5j),_5l=_5k[1],_5m=E(_4R),_5n=jsGetAttr(_5l,toJSStr(_5m));if(!_4S(fromJSStr(_5n),_4Q)){var _5o=E(_5h),_5p=jsSetCB(_5l,_5c(_5g)[1],_5h),_5q=A(_2,[_9,_5k,_5m,_4Q,_]);return _5k;}else{return _5k;}},_5r=unCStr("br"),_5s=function(_5t,_){var _5u=jsCreateElem(toJSStr(E(_5r))),_5v=jsAppendChild(_5u,E(_5t)[1]);return [0,_5u];},_5w=unCStr("second number "),_5x=unCStr("first number  "),_5y=unCStr("text"),_5z=function(_5A,_5B,_){var _5C=jsCreateElem(toJSStr(E(_5A))),_5D=jsAppendChild(_5C,E(_5B)[1]);return [0,_5C];},_5E=function(_5F,_5G,_5H,_){var _5I=_5z(_5F,_5H,_),_5J=A(_5G,[_5I,_]);return _5I;},_5K=unCStr("()"),_5L=unCStr("GHC.Tuple"),_5M=unCStr("ghc-prim"),_5N=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5M,_5L,_5K],_5O=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5N,_r],_5P=function(_5Q){return E(_5O);},_5R=unCStr("main"),_5S=unCStr("Builder"),_5T=unCStr("JSBuilderM"),_5U=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_5R,_5S,_5T],_5V=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_5U,_r],_5W=function(_5X){return E(_5V);},_5Y=function(_5Z){var _60=E(_5Z);return _60[0]==0?[0]:_P(_60[1],new T(function(){return _5Y(_60[2]);}));},_61=function(_62,_63){var _64=E(_62);if(!_64){return [0,_r,_63];}else{var _65=E(_63);if(!_65[0]){return [0,_r,_r];}else{var _66=new T(function(){var _67=_61(_64-1|0,_65[2]);return [0,_67[1],_67[2]];});return [0,[1,_65[1],new T(function(){return E(E(_66)[1]);})],new T(function(){return E(E(_66)[2]);})];}}},_68=[0,120],_69=[0,48],_6a=function(_6b){var _6c=new T(function(){var _6d=_61(8,new T(function(){var _6e=md5(toJSStr(E(_6b)));return fromJSStr(_6e);}));return [0,_6d[1],_6d[2]];}),_6f=parseInt([0,toJSStr([1,_69,[1,_68,new T(function(){return E(E(_6c)[1]);})]])]),_6g=new T(function(){var _6h=_61(8,new T(function(){return E(E(_6c)[2]);}));return [0,_6h[1],_6h[2]];}),_6i=parseInt([0,toJSStr([1,_69,[1,_68,new T(function(){return E(E(_6g)[1]);})]])]),_6j=hs_mkWord64(_6f,_6i),_6k=parseInt([0,toJSStr([1,_69,[1,_68,new T(function(){return E(_61(8,new T(function(){return E(E(_6g)[2]);}))[1]);})]])]),_6l=hs_mkWord64(_6k,_6k);return [0,_6j,_6l];},_6m=function(_6n,_6o){var _6p=E(_6o);return _6p[0]==0?[0]:[1,new T(function(){return A(_6n,[_6p[1]]);}),new T(function(){return _6m(_6n,_6p[2]);})];},_6q=function(_6r,_6s){var _6t=jsShowI(_6r),_6u=md5(_6t);return _P(fromJSStr(_6u),new T(function(){var _6v=jsShowI(_6s),_6w=md5(_6v);return fromJSStr(_6w);}));},_6x=function(_6y){var _6z=E(_6y);return _6q(_6z[1],_6z[2]);},_6A=function(_6B){var _6C=E(_6B);if(!_6C[0]){return [0];}else{var _6D=E(_6C[1]);return [1,[0,_6D[1],_6D[2]],new T(function(){return _6A(_6C[2]);})];}},_6E=unCStr("Prelude.undefined"),_6F=new T(function(){return err(_6E);}),_6G=function(_6H,_6I){return function(_6J){return E(new T(function(){var _6K=A(_6H,[_6F]),_6L=E(_6K[3]),_6M=_6L[1],_6N=_6L[2],_6O=_P(_6K[4],[1,new T(function(){return A(_6I,[_6F]);}),_r]);if(!_6O[0]){return [0,_6M,_6N,_6L,_r];}else{var _6P=_6a(new T(function(){return _5Y(_6m(_6x,[1,[0,_6M,_6N],new T(function(){return _6A(_6O);})]));}));return [0,_6P[1],_6P[2],_6L,_6O];}}));};},_6Q=new T(function(){return _6G(_5W,_5P);}),_6R=function(_6S,_6T,_6U,_){var _6V=E(_6T),_6W=A(_6S,[_6U,_]),_6X=A(_2,[_9,_6W,_6V[1],_6V[2],_]);return _6W;},_6Y=function(_6Z,_70){while(1){var _71=(function(_72,_73){var _74=E(_73);if(!_74[0]){return E(_72);}else{_6Z=function(_3C,_){return _6R(_72,_74[1],_3C,_);};_70=_74[2];return null;}})(_6Z,_70);if(_71!=null){return _71;}}},_75=unCStr("value"),_76=unCStr("onclick"),_77=unCStr("checked"),_78=[0,_77,_r],_79=[1,_78,_r],_7a=unCStr("type"),_7b=unCStr("input"),_7c=function(_7d,_){return _5z(_7b,_7d,_);},_7e=function(_7f,_7g,_7h,_7i,_7j){var _7k=new T(function(){var _7l=new T(function(){return _6Y(_7c,[1,[0,_7a,_7g],[1,[0,_0,_7f],[1,[0,_75,_7h],_r]]]);});return !E(_7i)?E(_7l):_6Y(_7l,_79);}),_7m=E(_7j);return _7m[0]==0?E(_7k):_6Y(_7k,[1,[0,_76,_7m[1]],_r]);},_7n=unCStr("href"),_7o=[0,97],_7p=[1,_7o,_r],_7q=function(_7r,_){return _5z(_7p,_7r,_);},_7s=function(_7t,_7u){var _7v=new T(function(){return _6Y(_7q,[1,[0,_7n,_7t],_r]);});return function(_7w,_){var _7x=A(_7v,[_7w,_]),_7y=A(_7u,[_7x,_]);return _7x;};},_7z=function(_7A){return _7s(_7A,function(_3C,_){return _47(_7A,_3C,_);});},_7B=unCStr("option"),_7C=function(_7D,_){return _5z(_7B,_7D,_);},_7E=unCStr("selected"),_7F=[0,_7E,_r],_7G=[1,_7F,_r],_7H=function(_7I,_7J,_7K){var _7L=new T(function(){return _6Y(_7C,[1,[0,_75,_7I],_r]);}),_7M=function(_7N,_){var _7O=A(_7L,[_7N,_]),_7P=A(_7J,[_7O,_]);return _7O;};return !E(_7K)?E(_7M):_6Y(_7M,_7G);},_7Q=function(_7R,_7S){return _7H(_7R,function(_3C,_){return _47(_7R,_3C,_);},_7S);},_7T=unCStr("method"),_7U=unCStr("action"),_7V=unCStr("UTF-8"),_7W=unCStr("acceptCharset"),_7X=[0,_7W,_7V],_7Y=unCStr("form"),_7Z=function(_80,_){return _5z(_7Y,_80,_);},_81=function(_82,_83,_84){var _85=new T(function(){return _6Y(_7Z,[1,_7X,[1,[0,_7U,_82],[1,[0,_7T,_83],_r]]]);});return function(_86,_){var _87=A(_85,[_86,_]),_88=A(_84,[_87,_]);return _87;};},_89=unCStr("select"),_8a=function(_8b,_){return _5z(_89,_8b,_);},_8c=function(_8d,_8e){var _8f=new T(function(){return _6Y(_8a,[1,[0,_0,_8d],_r]);});return function(_8g,_){var _8h=A(_8f,[_8g,_]),_8i=A(_8e,[_8h,_]);return _8h;};},_8j=unCStr("textarea"),_8k=function(_8l,_){return _5z(_8j,_8l,_);},_8m=function(_8n,_8o){var _8p=new T(function(){return _6Y(_8k,[1,[0,_0,_8n],_r]);});return function(_8q,_){var _8r=A(_8p,[_8q,_]),_8s=_47(_8o,_8r,_);return _8r;};},_8t=unCStr("color:red"),_8u=unCStr("style"),_8v=[0,_8u,_8t],_8w=[1,_8v,_r],_8x=[0,98],_8y=[1,_8x,_r],_8z=function(_8A){return _6Y(function(_8B,_){var _8C=_5z(_8y,_8B,_),_8D=A(_8A,[_8C,_]);return _8C;},_8w);},_8E=unCStr("toByteString not defined"),_8F=new T(function(){return err(_8E);}),_8G=function(_8H,_8I,_){var _8J=E(_8H);if(!_8J[0]){return _8I;}else{var _8K=A(_8J[1],[_8I,_]),_8L=_8G(_8J[2],_8I,_);return _8I;}},_8M=function(_8N,_8O,_8P,_){var _8Q=A(_8N,[_8P,_]),_8R=A(_8O,[_8P,_]);return _8P;},_8S=[0,_26,_8M,_8G],_8T=[0,_8S,_6Q,_8F,_47,_47,_5E,_8z,_7s,_7z,_7e,_8m,_8c,_7H,_7Q,_81,_6Y],_8U=[0,_28,_9],_8V=unCStr("base"),_8W=unCStr("Control.Exception.Base"),_8X=unCStr("PatternMatchFail"),_8Y=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_8V,_8W,_8X],_8Z=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_8Y,_r],_90=function(_91){return E(_8Z);},_92=function(_93){var _94=E(_93);return _z(_v(_94[1]),_90,_94[2]);},_95=function(_96){return E(E(_96)[1]);},_97=function(_98,_99){return _P(E(_98)[1],_99);},_9a=function(_9b,_9c){return _1F(_97,_9b,_9c);},_9d=function(_9e,_9f,_9g){return _P(E(_9f)[1],_9g);},_9h=[0,_9d,_95,_9a],_9i=new T(function(){return [0,_90,_9h,_9j,_92];}),_9j=function(_9k){return [0,_9i,_9k];},_9l=unCStr("Non-exhaustive patterns in"),_9m=function(_9n,_9o){return die(new T(function(){return A(_9o,[_9n]);}));},_9p=function(_9q,_9r){var _9s=E(_9r);if(!_9s[0]){return [0,_r,_r];}else{var _9t=_9s[1];if(!A(_9q,[_9t])){return [0,_r,_9s];}else{var _9u=new T(function(){var _9v=_9p(_9q,_9s[2]);return [0,_9v[1],_9v[2]];});return [0,[1,_9t,new T(function(){return E(E(_9u)[1]);})],new T(function(){return E(E(_9u)[2]);})];}}},_9w=[0,32],_9x=[0,10],_9y=[1,_9x,_r],_9z=function(_9A){return E(E(_9A)[1])==124?false:true;},_9B=function(_9C,_9D){var _9E=_9p(_9z,unCStr(_9C)),_9F=_9E[1],_9G=function(_9H,_9I){return _P(_9H,new T(function(){return unAppCStr(": ",new T(function(){return _P(_9D,new T(function(){return _P(_9I,_9y);}));}));}));},_9J=E(_9E[2]);return _9J[0]==0?_9G(_9F,_r):E(E(_9J[1])[1])==124?_9G(_9F,[1,_9w,_9J[2]]):_9G(_9F,_r);},_9K=function(_9L){return _9m([0,new T(function(){return _9B(_9L,_9l);})],_9j);},_9M=new T(function(){return _9K("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_9N=function(_9O,_9P){while(1){var _9Q=(function(_9R,_9S){var _9T=E(_9R);switch(_9T[0]){case 0:var _9U=E(_9S);if(!_9U[0]){return [0];}else{_9O=A(_9T[1],[_9U[1]]);_9P=_9U[2];return null;}break;case 1:var _9V=A(_9T[1],[_9S]),_9W=_9S;_9O=_9V;_9P=_9W;return null;case 2:return [0];case 3:return [1,[0,_9T[1],_9S],new T(function(){return _9N(_9T[2],_9S);})];default:return E(_9T[1]);}})(_9O,_9P);if(_9Q!=null){return _9Q;}}},_9X=function(_9Y,_9Z){var _a0=new T(function(){var _a1=E(_9Z);if(_a1[0]==3){return [3,_a1[1],new T(function(){return _9X(_9Y,_a1[2]);})];}else{var _a2=E(_9Y);if(_a2[0]==2){return E(_a1);}else{var _a3=E(_a1);if(_a3[0]==2){return E(_a2);}else{var _a4=new T(function(){var _a5=E(_a3);if(_a5[0]==4){return [1,function(_a6){return [4,new T(function(){return _P(_9N(_a2,_a6),_a5[1]);})];}];}else{var _a7=E(_a2);if(_a7[0]==1){var _a8=_a7[1],_a9=E(_a5);return _a9[0]==0?[1,function(_aa){return _9X(A(_a8,[_aa]),_a9);}]:[1,function(_ab){return _9X(A(_a8,[_ab]),new T(function(){return A(_a9[1],[_ab]);}));}];}else{var _ac=E(_a5);return _ac[0]==0?E(_9M):[1,function(_ad){return _9X(_a7,new T(function(){return A(_ac[1],[_ad]);}));}];}}}),_ae=E(_a2);switch(_ae[0]){case 1:var _af=E(_a3);return _af[0]==4?[1,function(_ag){return [4,new T(function(){return _P(_9N(A(_ae[1],[_ag]),_ag),_af[1]);})];}]:E(_a4);case 4:var _ah=_ae[1],_ai=E(_a3);switch(_ai[0]){case 0:return [1,function(_aj){return [4,new T(function(){return _P(_ah,new T(function(){return _9N(_ai,_aj);}));})];}];case 1:return [1,function(_ak){return [4,new T(function(){return _P(_ah,new T(function(){return _9N(A(_ai[1],[_ak]),_ak);}));})];}];default:return [4,new T(function(){return _P(_ah,_ai[1]);})];}break;default:return E(_a4);}}}}}),_al=E(_9Y);switch(_al[0]){case 0:var _am=E(_9Z);return _am[0]==0?[0,function(_an){return _9X(A(_al[1],[_an]),new T(function(){return A(_am[1],[_an]);}));}]:E(_a0);case 3:return [3,_al[1],new T(function(){return _9X(_al[2],_9Z);})];default:return E(_a0);}},_ao=function(_ap,_aq){return E(_ap)[1]!=E(_aq)[1];},_ar=function(_as,_at){return E(_as)[1]==E(_at)[1];},_au=[0,_ar,_ao],_av=function(_aw){return E(E(_aw)[1]);},_ax=function(_ay,_az,_aA){while(1){var _aB=E(_az);if(!_aB[0]){return E(_aA)[0]==0?true:false;}else{var _aC=E(_aA);if(!_aC[0]){return false;}else{if(!A(_av,[_ay,_aB[1],_aC[1]])){return false;}else{_az=_aB[2];_aA=_aC[2];continue;}}}}},_aD=function(_aE,_aF,_aG){return !_ax(_aE,_aF,_aG)?true:false;},_aH=function(_aI){return [0,function(_aJ,_aK){return _ax(_aI,_aJ,_aK);},function(_aJ,_aK){return _aD(_aI,_aJ,_aK);}];},_aL=new T(function(){return _aH(_au);}),_aM=function(_aN,_aO){var _aP=E(_aN);switch(_aP[0]){case 0:return [0,function(_aQ){return _aM(A(_aP[1],[_aQ]),_aO);}];case 1:return [1,function(_aR){return _aM(A(_aP[1],[_aR]),_aO);}];case 2:return [2];case 3:return _9X(A(_aO,[_aP[1]]),new T(function(){return _aM(_aP[2],_aO);}));default:var _aS=function(_aT){var _aU=E(_aT);if(!_aU[0]){return [0];}else{var _aV=E(_aU[1]);return _P(_9N(A(_aO,[_aV[1]]),_aV[2]),new T(function(){return _aS(_aU[2]);}));}},_aW=_aS(_aP[1]);return _aW[0]==0?[2]:[4,_aW];}},_aX=[2],_aY=function(_aZ){return [3,_aZ,_aX];},_b0=function(_b1,_b2){var _b3=E(_b1);if(!_b3){return A(_b2,[_1]);}else{var _b4=new T(function(){return _b0(_b3-1|0,_b2);});return [0,function(_b5){return E(_b4);}];}},_b6=function(_b7,_b8,_b9){var _ba=new T(function(){return A(_b7,[_aY]);});return [1,function(_bb){return A(function(_bc,_bd,_be){while(1){var _bf=(function(_bg,_bh,_bi){var _bj=E(_bg);switch(_bj[0]){case 0:var _bk=E(_bh);if(!_bk[0]){return E(_b8);}else{_bc=A(_bj[1],[_bk[1]]);_bd=_bk[2];var _bl=_bi+1|0;_be=_bl;return null;}break;case 1:var _bm=A(_bj[1],[_bh]),_bn=_bh,_bl=_bi;_bc=_bm;_bd=_bn;_be=_bl;return null;case 2:return E(_b8);case 3:return function(_bo){var _bp=new T(function(){return _aM(_bj,_bo);});return _b0(_bi,function(_bq){return E(_bp);});};default:return function(_br){return _aM(_bj,_br);};}})(_bc,_bd,_be);if(_bf!=null){return _bf;}}},[_ba,_bb,0,_b9]);}];},_bs=[6],_bt=unCStr("valDig: Bad base"),_bu=new T(function(){return err(_bt);}),_bv=function(_bw,_bx){var _by=function(_bz,_bA){var _bB=E(_bz);if(!_bB[0]){var _bC=new T(function(){return A(_bA,[_r]);});return function(_bD){return A(_bD,[_bC]);};}else{var _bE=E(_bB[1])[1],_bF=function(_bG){var _bH=new T(function(){return _by(_bB[2],function(_bI){return A(_bA,[[1,_bG,_bI]]);});});return function(_bJ){var _bK=new T(function(){return A(_bH,[_bJ]);});return [0,function(_bL){return E(_bK);}];};};switch(E(E(_bw)[1])){case 8:if(48>_bE){var _bM=new T(function(){return A(_bA,[_r]);});return function(_bN){return A(_bN,[_bM]);};}else{if(_bE>55){var _bO=new T(function(){return A(_bA,[_r]);});return function(_bP){return A(_bP,[_bO]);};}else{return _bF([0,_bE-48|0]);}}break;case 10:if(48>_bE){var _bQ=new T(function(){return A(_bA,[_r]);});return function(_bR){return A(_bR,[_bQ]);};}else{if(_bE>57){var _bS=new T(function(){return A(_bA,[_r]);});return function(_bT){return A(_bT,[_bS]);};}else{return _bF([0,_bE-48|0]);}}break;case 16:var _bU=new T(function(){return 97>_bE?65>_bE?[0]:_bE>70?[0]:[1,[0,(_bE-65|0)+10|0]]:_bE>102?65>_bE?[0]:_bE>70?[0]:[1,[0,(_bE-65|0)+10|0]]:[1,[0,(_bE-97|0)+10|0]];});if(48>_bE){var _bV=E(_bU);if(!_bV[0]){var _bW=new T(function(){return A(_bA,[_r]);});return function(_bX){return A(_bX,[_bW]);};}else{return _bF(_bV[1]);}}else{if(_bE>57){var _bY=E(_bU);if(!_bY[0]){var _bZ=new T(function(){return A(_bA,[_r]);});return function(_c0){return A(_c0,[_bZ]);};}else{return _bF(_bY[1]);}}else{return _bF([0,_bE-48|0]);}}break;default:return E(_bu);}}};return [1,function(_c1){return A(_by,[_c1,_9,function(_c2){var _c3=E(_c2);return _c3[0]==0?[2]:A(_bx,[_c3]);}]);}];},_c4=[0,10],_c5=[0,1],_c6=[0,2147483647],_c7=function(_c8,_c9){while(1){var _ca=E(_c8);if(!_ca[0]){var _cb=_ca[1],_cc=E(_c9);if(!_cc[0]){var _cd=_cc[1],_ce=addC(_cb,_cd);if(!E(_ce[2])){return [0,_ce[1]];}else{_c8=[1,I_fromInt(_cb)];_c9=[1,I_fromInt(_cd)];continue;}}else{_c8=[1,I_fromInt(_cb)];_c9=_cc;continue;}}else{var _cf=E(_c9);if(!_cf[0]){_c8=_ca;_c9=[1,I_fromInt(_cf[1])];continue;}else{return [1,I_add(_ca[1],_cf[1])];}}}},_cg=new T(function(){return _c7(_c6,_c5);}),_ch=function(_ci){var _cj=E(_ci);if(!_cj[0]){var _ck=E(_cj[1]);return _ck==(-2147483648)?E(_cg):[0, -_ck];}else{return [1,I_negate(_cj[1])];}},_cl=[0,10],_cm=[0,0],_cn=function(_co,_cp){while(1){var _cq=E(_co);if(!_cq[0]){var _cr=_cq[1],_cs=E(_cp);if(!_cs[0]){var _ct=_cs[1];if(!(imul(_cr,_ct)|0)){return [0,imul(_cr,_ct)|0];}else{_co=[1,I_fromInt(_cr)];_cp=[1,I_fromInt(_ct)];continue;}}else{_co=[1,I_fromInt(_cr)];_cp=_cs;continue;}}else{var _cu=E(_cp);if(!_cu[0]){_co=_cq;_cp=[1,I_fromInt(_cu[1])];continue;}else{return [1,I_mul(_cq[1],_cu[1])];}}}},_cv=function(_cw,_cx,_cy){while(1){var _cz=E(_cy);if(!_cz[0]){return E(_cx);}else{var _cA=_c7(_cn(_cx,_cw),_cz[1]);_cy=_cz[2];_cx=_cA;continue;}}},_cB=function(_cC){var _cD=new T(function(){return _9X(_9X([0,function(_cE){return E(E(_cE)[1])==45?_bv(_c4,function(_cF){return A(_cC,[[1,new T(function(){return _ch(_cv(_cl,_cm,_cF));})]]);}):[2];}],[0,function(_cG){return E(E(_cG)[1])==43?_bv(_c4,function(_cH){return A(_cC,[[1,new T(function(){return _cv(_cl,_cm,_cH);})]]);}):[2];}]),new T(function(){return _bv(_c4,function(_cI){return A(_cC,[[1,new T(function(){return _cv(_cl,_cm,_cI);})]]);});}));});return _9X([0,function(_cJ){return E(E(_cJ)[1])==101?E(_cD):[2];}],[0,function(_cK){return E(E(_cK)[1])==69?E(_cD):[2];}]);},_cL=function(_cM){return A(_cM,[_8]);},_cN=function(_cO){return A(_cO,[_8]);},_cP=function(_cQ){var _cR=new T(function(){return _bv(_c4,function(_cS){return A(_cQ,[[1,_cS]]);});});return [0,function(_cT){return E(E(_cT)[1])==46?E(_cR):[2];}];},_cU=function(_cV){return _bv(_c4,function(_cW){return _b6(_cP,_cL,function(_cX){return _b6(_cB,_cN,function(_cY){return A(_cV,[[5,[1,_cW,_cX,_cY]]]);});});});},_cZ=function(_d0,_d1,_d2){while(1){var _d3=E(_d2);if(!_d3[0]){return false;}else{if(!A(_av,[_d0,_d1,_d3[1]])){_d2=_d3[2];continue;}else{return true;}}}},_d4=unCStr("!@#$%&*+./<=>?\\^|:-~"),_d5=function(_d6){return _cZ(_au,_d6,_d4);},_d7=[0,8],_d8=[0,16],_d9=function(_da){var _db=new T(function(){return _bv(_d8,function(_dc){return A(_da,[[5,[0,_d8,_dc]]]);});}),_dd=new T(function(){return _bv(_d7,function(_de){return A(_da,[[5,[0,_d7,_de]]]);});}),_df=new T(function(){return _bv(_d8,function(_dg){return A(_da,[[5,[0,_d8,_dg]]]);});}),_dh=new T(function(){return _bv(_d7,function(_di){return A(_da,[[5,[0,_d7,_di]]]);});});return [0,function(_dj){return E(E(_dj)[1])==48?E([0,function(_dk){switch(E(E(_dk)[1])){case 79:return E(_dh);case 88:return E(_df);case 111:return E(_dd);case 120:return E(_db);default:return [2];}}]):[2];}];},_dl=false,_dm=true,_dn=function(_do){var _dp=new T(function(){return A(_do,[_d8]);}),_dq=new T(function(){return A(_do,[_d7]);}),_dr=new T(function(){return A(_do,[_d8]);}),_ds=new T(function(){return A(_do,[_d7]);});return [0,function(_dt){switch(E(E(_dt)[1])){case 79:return E(_ds);case 88:return E(_dr);case 111:return E(_dq);case 120:return E(_dp);default:return [2];}}];},_du=function(_dv){return A(_dv,[_c4]);},_dw=function(_dx){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _2N(9,_dx,_r);})));},_dy=function(_dz){var _dA=E(_dz);return _dA[0]==0?E(_dA[1]):I_toInt(_dA[1]);},_dB=function(_dC,_dD){var _dE=E(_dC);if(!_dE[0]){var _dF=_dE[1],_dG=E(_dD);return _dG[0]==0?_dF<=_dG[1]:I_compareInt(_dG[1],_dF)>=0;}else{var _dH=_dE[1],_dI=E(_dD);return _dI[0]==0?I_compareInt(_dH,_dI[1])<=0:I_compare(_dH,_dI[1])<=0;}},_dJ=function(_dK){return [2];},_dL=function(_dM){var _dN=E(_dM);if(!_dN[0]){return E(_dJ);}else{var _dO=_dN[1],_dP=E(_dN[2]);if(!_dP[0]){return E(_dO);}else{var _dQ=new T(function(){return _dL(_dP);});return function(_dR){return _9X(A(_dO,[_dR]),new T(function(){return A(_dQ,[_dR]);}));};}}},_dS=unCStr("NUL"),_dT=function(_dU){return [2];},_dV=function(_dW){return _dT(_dW);},_dX=function(_dY,_dZ){var _e0=function(_e1,_e2){var _e3=E(_e1);if(!_e3[0]){return function(_e4){return A(_e4,[_dY]);};}else{var _e5=E(_e2);if(!_e5[0]){return E(_dT);}else{if(E(_e3[1])[1]!=E(_e5[1])[1]){return E(_dV);}else{var _e6=new T(function(){return _e0(_e3[2],_e5[2]);});return function(_e7){var _e8=new T(function(){return A(_e6,[_e7]);});return [0,function(_e9){return E(_e8);}];};}}}};return [1,function(_ea){return A(_e0,[_dY,_ea,_dZ]);}];},_eb=[0,0],_ec=function(_ed){var _ee=new T(function(){return A(_ed,[_eb]);});return _dX(_dS,function(_ef){return E(_ee);});},_eg=unCStr("STX"),_eh=[0,2],_ei=function(_ej){var _ek=new T(function(){return A(_ej,[_eh]);});return _dX(_eg,function(_el){return E(_ek);});},_em=unCStr("ETX"),_en=[0,3],_eo=function(_ep){var _eq=new T(function(){return A(_ep,[_en]);});return _dX(_em,function(_er){return E(_eq);});},_es=unCStr("EOT"),_et=[0,4],_eu=function(_ev){var _ew=new T(function(){return A(_ev,[_et]);});return _dX(_es,function(_ex){return E(_ew);});},_ey=unCStr("ENQ"),_ez=[0,5],_eA=function(_eB){var _eC=new T(function(){return A(_eB,[_ez]);});return _dX(_ey,function(_eD){return E(_eC);});},_eE=unCStr("ACK"),_eF=[0,6],_eG=function(_eH){var _eI=new T(function(){return A(_eH,[_eF]);});return _dX(_eE,function(_eJ){return E(_eI);});},_eK=unCStr("BEL"),_eL=[0,7],_eM=function(_eN){var _eO=new T(function(){return A(_eN,[_eL]);});return _dX(_eK,function(_eP){return E(_eO);});},_eQ=unCStr("BS"),_eR=[0,8],_eS=function(_eT){var _eU=new T(function(){return A(_eT,[_eR]);});return _dX(_eQ,function(_eV){return E(_eU);});},_eW=unCStr("HT"),_eX=[0,9],_eY=function(_eZ){var _f0=new T(function(){return A(_eZ,[_eX]);});return _dX(_eW,function(_f1){return E(_f0);});},_f2=unCStr("LF"),_f3=[0,10],_f4=function(_f5){var _f6=new T(function(){return A(_f5,[_f3]);});return _dX(_f2,function(_f7){return E(_f6);});},_f8=unCStr("VT"),_f9=[0,11],_fa=function(_fb){var _fc=new T(function(){return A(_fb,[_f9]);});return _dX(_f8,function(_fd){return E(_fc);});},_fe=unCStr("FF"),_ff=[0,12],_fg=function(_fh){var _fi=new T(function(){return A(_fh,[_ff]);});return _dX(_fe,function(_fj){return E(_fi);});},_fk=unCStr("CR"),_fl=[0,13],_fm=function(_fn){var _fo=new T(function(){return A(_fn,[_fl]);});return _dX(_fk,function(_fp){return E(_fo);});},_fq=unCStr("SI"),_fr=[0,15],_fs=function(_ft){var _fu=new T(function(){return A(_ft,[_fr]);});return _dX(_fq,function(_fv){return E(_fu);});},_fw=unCStr("DLE"),_fx=[0,16],_fy=function(_fz){var _fA=new T(function(){return A(_fz,[_fx]);});return _dX(_fw,function(_fB){return E(_fA);});},_fC=unCStr("DC1"),_fD=[0,17],_fE=function(_fF){var _fG=new T(function(){return A(_fF,[_fD]);});return _dX(_fC,function(_fH){return E(_fG);});},_fI=unCStr("DC2"),_fJ=[0,18],_fK=function(_fL){var _fM=new T(function(){return A(_fL,[_fJ]);});return _dX(_fI,function(_fN){return E(_fM);});},_fO=unCStr("DC3"),_fP=[0,19],_fQ=function(_fR){var _fS=new T(function(){return A(_fR,[_fP]);});return _dX(_fO,function(_fT){return E(_fS);});},_fU=unCStr("DC4"),_fV=[0,20],_fW=function(_fX){var _fY=new T(function(){return A(_fX,[_fV]);});return _dX(_fU,function(_fZ){return E(_fY);});},_g0=unCStr("NAK"),_g1=[0,21],_g2=function(_g3){var _g4=new T(function(){return A(_g3,[_g1]);});return _dX(_g0,function(_g5){return E(_g4);});},_g6=unCStr("SYN"),_g7=[0,22],_g8=function(_g9){var _ga=new T(function(){return A(_g9,[_g7]);});return _dX(_g6,function(_gb){return E(_ga);});},_gc=unCStr("ETB"),_gd=[0,23],_ge=function(_gf){var _gg=new T(function(){return A(_gf,[_gd]);});return _dX(_gc,function(_gh){return E(_gg);});},_gi=unCStr("CAN"),_gj=[0,24],_gk=function(_gl){var _gm=new T(function(){return A(_gl,[_gj]);});return _dX(_gi,function(_gn){return E(_gm);});},_go=unCStr("EM"),_gp=[0,25],_gq=function(_gr){var _gs=new T(function(){return A(_gr,[_gp]);});return _dX(_go,function(_gt){return E(_gs);});},_gu=unCStr("SUB"),_gv=[0,26],_gw=function(_gx){var _gy=new T(function(){return A(_gx,[_gv]);});return _dX(_gu,function(_gz){return E(_gy);});},_gA=unCStr("ESC"),_gB=[0,27],_gC=function(_gD){var _gE=new T(function(){return A(_gD,[_gB]);});return _dX(_gA,function(_gF){return E(_gE);});},_gG=unCStr("FS"),_gH=[0,28],_gI=function(_gJ){var _gK=new T(function(){return A(_gJ,[_gH]);});return _dX(_gG,function(_gL){return E(_gK);});},_gM=unCStr("GS"),_gN=[0,29],_gO=function(_gP){var _gQ=new T(function(){return A(_gP,[_gN]);});return _dX(_gM,function(_gR){return E(_gQ);});},_gS=unCStr("RS"),_gT=[0,30],_gU=function(_gV){var _gW=new T(function(){return A(_gV,[_gT]);});return _dX(_gS,function(_gX){return E(_gW);});},_gY=unCStr("US"),_gZ=[0,31],_h0=function(_h1){var _h2=new T(function(){return A(_h1,[_gZ]);});return _dX(_gY,function(_h3){return E(_h2);});},_h4=unCStr("SP"),_h5=[0,32],_h6=function(_h7){var _h8=new T(function(){return A(_h7,[_h5]);});return _dX(_h4,function(_h9){return E(_h8);});},_ha=unCStr("DEL"),_hb=[0,127],_hc=function(_hd){var _he=new T(function(){return A(_hd,[_hb]);});return _dX(_ha,function(_hf){return E(_he);});},_hg=[1,_hc,_r],_hh=[1,_h6,_hg],_hi=[1,_h0,_hh],_hj=[1,_gU,_hi],_hk=[1,_gO,_hj],_hl=[1,_gI,_hk],_hm=[1,_gC,_hl],_hn=[1,_gw,_hm],_ho=[1,_gq,_hn],_hp=[1,_gk,_ho],_hq=[1,_ge,_hp],_hr=[1,_g8,_hq],_hs=[1,_g2,_hr],_ht=[1,_fW,_hs],_hu=[1,_fQ,_ht],_hv=[1,_fK,_hu],_hw=[1,_fE,_hv],_hx=[1,_fy,_hw],_hy=[1,_fs,_hx],_hz=[1,_fm,_hy],_hA=[1,_fg,_hz],_hB=[1,_fa,_hA],_hC=[1,_f4,_hB],_hD=[1,_eY,_hC],_hE=[1,_eS,_hD],_hF=[1,_eM,_hE],_hG=[1,_eG,_hF],_hH=[1,_eA,_hG],_hI=[1,_eu,_hH],_hJ=[1,_eo,_hI],_hK=[1,_ei,_hJ],_hL=[1,_ec,_hK],_hM=unCStr("SOH"),_hN=[0,1],_hO=function(_hP){var _hQ=new T(function(){return A(_hP,[_hN]);});return _dX(_hM,function(_hR){return E(_hQ);});},_hS=unCStr("SO"),_hT=[0,14],_hU=function(_hV){var _hW=new T(function(){return A(_hV,[_hT]);});return _dX(_hS,function(_hX){return E(_hW);});},_hY=function(_hZ){return _b6(_hO,_hU,_hZ);},_i0=[1,_hY,_hL],_i1=new T(function(){return _dL(_i0);}),_i2=[0,1114111],_i3=[0,34],_i4=[0,_i3,_dm],_i5=[0,39],_i6=[0,_i5,_dm],_i7=[0,92],_i8=[0,_i7,_dm],_i9=[0,_eL,_dm],_ia=[0,_eR,_dm],_ib=[0,_ff,_dm],_ic=[0,_f3,_dm],_id=[0,_fl,_dm],_ie=[0,_eX,_dm],_if=[0,_f9,_dm],_ig=[0,_eb,_dm],_ih=[0,_hN,_dm],_ii=[0,_eh,_dm],_ij=[0,_en,_dm],_ik=[0,_et,_dm],_il=[0,_ez,_dm],_im=[0,_eF,_dm],_in=[0,_eL,_dm],_io=[0,_eR,_dm],_ip=[0,_eX,_dm],_iq=[0,_f3,_dm],_ir=[0,_f9,_dm],_is=[0,_ff,_dm],_it=[0,_fl,_dm],_iu=[0,_hT,_dm],_iv=[0,_fr,_dm],_iw=[0,_fx,_dm],_ix=[0,_fD,_dm],_iy=[0,_fJ,_dm],_iz=[0,_fP,_dm],_iA=[0,_fV,_dm],_iB=[0,_g1,_dm],_iC=[0,_g7,_dm],_iD=[0,_gd,_dm],_iE=[0,_gj,_dm],_iF=[0,_gp,_dm],_iG=[0,_gv,_dm],_iH=[0,_gB,_dm],_iI=[0,_gH,_dm],_iJ=[0,_gN,_dm],_iK=[0,_gT,_dm],_iL=[0,_gZ,_dm],_iM=function(_iN){return [0,_iN];},_iO=function(_iP){var _iQ=new T(function(){return A(_iP,[_if]);}),_iR=new T(function(){return A(_iP,[_ie]);}),_iS=new T(function(){return A(_iP,[_id]);}),_iT=new T(function(){return A(_iP,[_ic]);}),_iU=new T(function(){return A(_iP,[_ib]);}),_iV=new T(function(){return A(_iP,[_ia]);}),_iW=new T(function(){return A(_iP,[_i9]);}),_iX=new T(function(){return A(_iP,[_i8]);}),_iY=new T(function(){return A(_iP,[_i6]);}),_iZ=new T(function(){return A(_iP,[_i4]);});return _9X([0,function(_j0){switch(E(E(_j0)[1])){case 34:return E(_iZ);case 39:return E(_iY);case 92:return E(_iX);case 97:return E(_iW);case 98:return E(_iV);case 102:return E(_iU);case 110:return E(_iT);case 114:return E(_iS);case 116:return E(_iR);case 118:return E(_iQ);default:return [2];}}],new T(function(){return _9X(_b6(_dn,_du,function(_j1){var _j2=new T(function(){return _iM(E(_j1)[1]);});return _bv(_j1,function(_j3){var _j4=_cv(_j2,_cm,_j3);return !_dB(_j4,_i2)?[2]:A(_iP,[[0,new T(function(){var _j5=_dy(_j4);return _j5>>>0>1114111?_dw(_j5):[0,_j5];}),_dm]]);});}),new T(function(){var _j6=new T(function(){return A(_iP,[_iL]);}),_j7=new T(function(){return A(_iP,[_iK]);}),_j8=new T(function(){return A(_iP,[_iJ]);}),_j9=new T(function(){return A(_iP,[_iI]);}),_ja=new T(function(){return A(_iP,[_iH]);}),_jb=new T(function(){return A(_iP,[_iG]);}),_jc=new T(function(){return A(_iP,[_iF]);}),_jd=new T(function(){return A(_iP,[_iE]);}),_je=new T(function(){return A(_iP,[_iD]);}),_jf=new T(function(){return A(_iP,[_iC]);}),_jg=new T(function(){return A(_iP,[_iB]);}),_jh=new T(function(){return A(_iP,[_iA]);}),_ji=new T(function(){return A(_iP,[_iz]);}),_jj=new T(function(){return A(_iP,[_iy]);}),_jk=new T(function(){return A(_iP,[_ix]);}),_jl=new T(function(){return A(_iP,[_iw]);}),_jm=new T(function(){return A(_iP,[_iv]);}),_jn=new T(function(){return A(_iP,[_iu]);}),_jo=new T(function(){return A(_iP,[_it]);}),_jp=new T(function(){return A(_iP,[_is]);}),_jq=new T(function(){return A(_iP,[_ir]);}),_jr=new T(function(){return A(_iP,[_iq]);}),_js=new T(function(){return A(_iP,[_ip]);}),_jt=new T(function(){return A(_iP,[_io]);}),_ju=new T(function(){return A(_iP,[_in]);}),_jv=new T(function(){return A(_iP,[_im]);}),_jw=new T(function(){return A(_iP,[_il]);}),_jx=new T(function(){return A(_iP,[_ik]);}),_jy=new T(function(){return A(_iP,[_ij]);}),_jz=new T(function(){return A(_iP,[_ii]);}),_jA=new T(function(){return A(_iP,[_ih]);}),_jB=new T(function(){return A(_iP,[_ig]);});return _9X([0,function(_jC){return E(E(_jC)[1])==94?E([0,function(_jD){switch(E(E(_jD)[1])){case 64:return E(_jB);case 65:return E(_jA);case 66:return E(_jz);case 67:return E(_jy);case 68:return E(_jx);case 69:return E(_jw);case 70:return E(_jv);case 71:return E(_ju);case 72:return E(_jt);case 73:return E(_js);case 74:return E(_jr);case 75:return E(_jq);case 76:return E(_jp);case 77:return E(_jo);case 78:return E(_jn);case 79:return E(_jm);case 80:return E(_jl);case 81:return E(_jk);case 82:return E(_jj);case 83:return E(_ji);case 84:return E(_jh);case 85:return E(_jg);case 86:return E(_jf);case 87:return E(_je);case 88:return E(_jd);case 89:return E(_jc);case 90:return E(_jb);case 91:return E(_ja);case 92:return E(_j9);case 93:return E(_j8);case 94:return E(_j7);case 95:return E(_j6);default:return [2];}}]):[2];}],new T(function(){return A(_i1,[function(_jE){return A(_iP,[[0,_jE,_dm]]);}]);}));}));}));},_jF=function(_jG){return A(_jG,[_1]);},_jH=function(_jI){var _jJ=E(_jI);if(!_jJ[0]){return E(_jF);}else{var _jK=_jJ[2],_jL=E(E(_jJ[1])[1]);switch(_jL){case 9:var _jM=new T(function(){return _jH(_jK);});return function(_jN){var _jO=new T(function(){return A(_jM,[_jN]);});return [0,function(_jP){return E(_jO);}];};case 10:var _jQ=new T(function(){return _jH(_jK);});return function(_jR){var _jS=new T(function(){return A(_jQ,[_jR]);});return [0,function(_jT){return E(_jS);}];};case 11:var _jU=new T(function(){return _jH(_jK);});return function(_jV){var _jW=new T(function(){return A(_jU,[_jV]);});return [0,function(_jX){return E(_jW);}];};case 12:var _jY=new T(function(){return _jH(_jK);});return function(_jZ){var _k0=new T(function(){return A(_jY,[_jZ]);});return [0,function(_k1){return E(_k0);}];};case 13:var _k2=new T(function(){return _jH(_jK);});return function(_k3){var _k4=new T(function(){return A(_k2,[_k3]);});return [0,function(_k5){return E(_k4);}];};case 32:var _k6=new T(function(){return _jH(_jK);});return function(_k7){var _k8=new T(function(){return A(_k6,[_k7]);});return [0,function(_k9){return E(_k8);}];};case 160:var _ka=new T(function(){return _jH(_jK);});return function(_kb){var _kc=new T(function(){return A(_ka,[_kb]);});return [0,function(_kd){return E(_kc);}];};default:var _ke=u_iswspace(_jL);if(!E(_ke)){return E(_jF);}else{var _kf=new T(function(){return _jH(_jK);});return function(_kg){var _kh=new T(function(){return A(_kf,[_kg]);});return [0,function(_ki){return E(_kh);}];};}}}},_kj=function(_kk){var _kl=new T(function(){return _iO(_kk);}),_km=new T(function(){return _kj(_kk);}),_kn=[1,function(_ko){return A(_jH,[_ko,function(_kp){return E([0,function(_kq){return E(E(_kq)[1])==92?E(_km):[2];}]);}]);}];return _9X([0,function(_kr){return E(E(_kr)[1])==92?E([0,function(_ks){var _kt=E(E(_ks)[1]);switch(_kt){case 9:return E(_kn);case 10:return E(_kn);case 11:return E(_kn);case 12:return E(_kn);case 13:return E(_kn);case 32:return E(_kn);case 38:return E(_km);case 160:return E(_kn);default:var _ku=u_iswspace(_kt);return E(_ku)==0?[2]:E(_kn);}}]):[2];}],[0,function(_kv){var _kw=E(_kv);return E(_kw[1])==92?E(_kl):A(_kk,[[0,_kw,_dl]]);}]);},_kx=function(_ky,_kz){var _kA=new T(function(){return A(_kz,[[1,new T(function(){return A(_ky,[_r]);})]]);});return _kj(function(_kB){var _kC=E(_kB),_kD=E(_kC[1]);return E(_kD[1])==34?!E(_kC[2])?E(_kA):_kx(function(_kE){return A(_ky,[[1,_kD,_kE]]);},_kz):_kx(function(_kF){return A(_ky,[[1,_kD,_kF]]);},_kz);});},_kG=unCStr("_\'"),_kH=function(_kI){var _kJ=u_iswalnum(_kI);return E(_kJ)==0?_cZ(_au,[0,_kI],_kG):true;},_kK=function(_kL){return _kH(E(_kL)[1]);},_kM=unCStr(",;()[]{}`"),_kN=function(_kO){return A(_kO,[_r]);},_kP=function(_kQ,_kR){var _kS=function(_kT){var _kU=E(_kT);if(!_kU[0]){return E(_kN);}else{var _kV=_kU[1];if(!A(_kQ,[_kV])){return E(_kN);}else{var _kW=new T(function(){return _kS(_kU[2]);});return function(_kX){var _kY=new T(function(){return A(_kW,[function(_kZ){return A(_kX,[[1,_kV,_kZ]]);}]);});return [0,function(_l0){return E(_kY);}];};}}};return [1,function(_l1){return A(_kS,[_l1,_kR]);}];},_l2=unCStr(".."),_l3=unCStr("::"),_l4=unCStr("->"),_l5=[0,64],_l6=[1,_l5,_r],_l7=[0,126],_l8=[1,_l7,_r],_l9=unCStr("=>"),_la=[1,_l9,_r],_lb=[1,_l8,_la],_lc=[1,_l6,_lb],_ld=[1,_l4,_lc],_le=unCStr("<-"),_lf=[1,_le,_ld],_lg=[0,124],_lh=[1,_lg,_r],_li=[1,_lh,_lf],_lj=[1,_i7,_r],_lk=[1,_lj,_li],_ll=[0,61],_lm=[1,_ll,_r],_ln=[1,_lm,_lk],_lo=[1,_l3,_ln],_lp=[1,_l2,_lo],_lq=function(_lr){var _ls=new T(function(){return A(_lr,[_bs]);});return _9X([1,function(_lt){return E(_lt)[0]==0?E(_ls):[2];}],new T(function(){var _lu=new T(function(){return _iO(function(_lv){var _lw=E(_lv);return (function(_lx,_ly){var _lz=new T(function(){return A(_lr,[[0,_lx]]);});return !E(_ly)?E(E(_lx)[1])==39?[2]:[0,function(_lA){return E(E(_lA)[1])==39?E(_lz):[2];}]:[0,function(_lB){return E(E(_lB)[1])==39?E(_lz):[2];}];})(_lw[1],_lw[2]);});});return _9X([0,function(_lC){return E(E(_lC)[1])==39?E([0,function(_lD){var _lE=E(_lD);switch(E(_lE[1])){case 39:return [2];case 92:return E(_lu);default:var _lF=new T(function(){return A(_lr,[[0,_lE]]);});return [0,function(_lG){return E(E(_lG)[1])==39?E(_lF):[2];}];}}]):[2];}],new T(function(){var _lH=new T(function(){return _kx(_9,_lr);});return _9X([0,function(_lI){return E(E(_lI)[1])==34?E(_lH):[2];}],new T(function(){return _9X([0,function(_lJ){return !_cZ(_au,_lJ,_kM)?[2]:A(_lr,[[2,[1,_lJ,_r]]]);}],new T(function(){return _9X([0,function(_lK){return !_cZ(_au,_lK,_d4)?[2]:_kP(_d5,function(_lL){var _lM=[1,_lK,_lL];return !_cZ(_aL,_lM,_lp)?A(_lr,[[4,_lM]]):A(_lr,[[2,_lM]]);});}],new T(function(){return _9X([0,function(_lN){var _lO=E(_lN),_lP=_lO[1],_lQ=u_iswalpha(_lP);return E(_lQ)==0?E(_lP)==95?_kP(_kK,function(_lR){return A(_lr,[[3,[1,_lO,_lR]]]);}):[2]:_kP(_kK,function(_lS){return A(_lr,[[3,[1,_lO,_lS]]]);});}],new T(function(){return _b6(_d9,_cU,_lr);}));}));}));}));}));}));},_lT=function(_lU){var _lV=new T(function(){return _lq(_lU);});return [1,function(_lW){return A(_jH,[_lW,function(_lX){return E(_lV);}]);}];},_lY=[0,0],_lZ=function(_m0,_m1){var _m2=new T(function(){return A(_m0,[_lY,function(_m3){var _m4=new T(function(){return A(_m1,[_m3]);});return _lT(function(_m5){var _m6=E(_m5);if(_m6[0]==2){var _m7=E(_m6[1]);return _m7[0]==0?[2]:E(E(_m7[1])[1])==41?E(_m7[2])[0]==0?E(_m4):[2]:[2];}else{return [2];}});}]);});return _lT(function(_m8){var _m9=E(_m8);if(_m9[0]==2){var _ma=E(_m9[1]);return _ma[0]==0?[2]:E(E(_ma[1])[1])==40?E(_ma[2])[0]==0?E(_m2):[2]:[2];}else{return [2];}});},_mb=function(_mc,_md,_me){var _mf=function(_mg,_mh){var _mi=new T(function(){return _lq(function(_mj){return A(_mc,[_mj,_mg,function(_mk){return A(_mh,[new T(function(){return [0, -E(_mk)[1]];})]);}]);});});return _9X(_lT(function(_ml){var _mm=E(_ml);if(_mm[0]==4){var _mn=E(_mm[1]);return _mn[0]==0?A(_mc,[_mm,_mg,_mh]):E(E(_mn[1])[1])==45?E(_mn[2])[0]==0?E([1,function(_mo){return A(_jH,[_mo,function(_mp){return E(_mi);}]);}]):A(_mc,[_mm,_mg,_mh]):A(_mc,[_mm,_mg,_mh]);}else{return A(_mc,[_mm,_mg,_mh]);}}),new T(function(){return _lZ(_mf,_mh);}));};return _mf(_md,_me);},_mq=function(_mr,_ms){return [2];},_mt=function(_mu,_mv){return _mq(_mu,_mv);},_mw=function(_mx){var _my=E(_mx);return _my[0]==0?[1,new T(function(){return _cv(new T(function(){return _iM(E(_my[1])[1]);}),_cm,_my[2]);})]:E(_my[2])[0]==0?E(_my[3])[0]==0?[1,new T(function(){return _cv(_cl,_cm,_my[1]);})]:[0]:[0];},_mz=function(_mA){var _mB=E(_mA);if(_mB[0]==5){var _mC=_mw(_mB[1]);if(!_mC[0]){return E(_mq);}else{var _mD=new T(function(){return [0,_dy(_mC[1])];});return function(_mE,_mF){return A(_mF,[_mD]);};}}else{return E(_mt);}},_mG=function(_mu,_mv){return _mb(_mz,_mu,_mv);},_mH=function(_mI,_mJ){var _mK=function(_mL,_mM){var _mN=new T(function(){return A(_mM,[_r]);}),_mO=new T(function(){return A(_mI,[_lY,function(_mP){return _mK(_dm,function(_mQ){return A(_mM,[[1,_mP,_mQ]]);});}]);});return _lT(function(_mR){var _mS=E(_mR);if(_mS[0]==2){var _mT=E(_mS[1]);if(!_mT[0]){return [2];}else{var _mU=_mT[2];switch(E(E(_mT[1])[1])){case 44:return E(_mU)[0]==0?!E(_mL)?[2]:E(_mO):[2];case 93:return E(_mU)[0]==0?E(_mN):[2];default:return [2];}}}else{return [2];}});},_mV=function(_mW){var _mX=new T(function(){return _9X(_mK(_dl,_mW),new T(function(){return A(_mI,[_lY,function(_mY){return _mK(_dm,function(_mZ){return A(_mW,[[1,_mY,_mZ]]);});}]);}));});return _9X(_lT(function(_n0){var _n1=E(_n0);if(_n1[0]==2){var _n2=E(_n1[1]);return _n2[0]==0?[2]:E(E(_n2[1])[1])==91?E(_n2[2])[0]==0?E(_mX):[2]:[2];}else{return [2];}}),new T(function(){return _lZ(function(_n3,_n4){return _mV(_n4);},_mW);}));};return _mV(_mJ);},_n5=function(_n6,_n7){return _mH(_mG,_n7);},_n8=new T(function(){return _mH(_mG,_aY);}),_n9=function(_mv){return _9N(_n8,_mv);},_na=function(_nb){var _nc=new T(function(){return _mb(_mz,_nb,_aY);});return function(_br){return _9N(_nc,_br);};},_nd=[0,_na,_n9,_mG,_n5],_ne=function(_nf,_ng){return _2N(0,E(_nf)[1],_ng);},_nh=function(_ni,_nj){return _1F(_ne,_ni,_nj);},_nk=function(_nl,_nm,_nn){return _2N(E(_nl)[1],E(_nm)[1],_nn);},_no=[0,_nk,_4p,_nh],_np=unCStr("GHC.Types"),_nq=unCStr("Int"),_nr=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_5M,_np,_nq],_ns=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_nr,_r],_nt=function(_nu){return E(_ns);},_nv=function(_nw){return E(E(_nw)[1]);},_nx=function(_ny){return E(E(_ny)[1]);},_nz=function(_nA){return E(E(_nA)[2]);},_nB=function(_nC,_nD){var _nE=new T(function(){return A(_nz,[_nC,_nD]);}),_nF=new T(function(){return _nx(_nC);}),_nG=new T(function(){return _2z(_nF);}),_nH=new T(function(){return _29(_nF);});return function(_nI){return A(_nH,[_nE,function(_nJ){return A(_nG,[[0,_nJ,_nI]]);}]);};},_nK=function(_nL,_nM){return A(_nL,[function(_){return jsFind(toJSStr(E(_nM)));}]);},_nN=function(_nO){return E(E(_nO)[4]);},_nP=unCStr("[]"),_nQ=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_5M,_np,_nP],_nR=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_nQ,_r],_nS=function(_nT){return E(_nR);},_nU=unCStr("Char"),_nV=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_5M,_np,_nU],_nW=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_nV,_r],_nX=function(_nY){return E(_nW);},_nZ=new T(function(){return _6G(_nS,_nX);}),_o0=new T(function(){return A(_nZ,[_6F]);}),_o1=new T(function(){return E(_6F);}),_o2=function(_o3){return E(E(_o3)[7]);},_o4=function(_o5){return E(E(_o5)[1]);},_o6=[0,0],_o7=[0,32],_o8=[0,10],_o9=function(_oa){var _ob=E(_oa);if(!_ob[0]){return E(_9);}else{var _oc=_ob[1],_od=E(_ob[2]);if(!_od[0]){return _oe(_o8,_oc);}else{var _of=new T(function(){return _o9(_od);}),_og=new T(function(){return _oe(_o8,_oc);});return function(_oh){return A(_og,[[1,_o7,new T(function(){return A(_of,[_oh]);})]]);};}}},_oi=unCStr("->"),_oj=[1,_oi,_r],_ok=[1,_np,_oj],_ol=[1,_5M,_ok],_om=[0,32],_on=function(_oo){var _op=E(_oo);if(!_op[0]){return [0];}else{var _oq=_op[1],_or=E(_op[2]);return _or[0]==0?E(_oq):_P(_oq,[1,_om,new T(function(){return _on(_or);})]);}},_os=new T(function(){return _on(_ol);}),_ot=new T(function(){var _ou=_6a(_os);return [0,_ou[1],_ou[2],_5M,_np,_oi];}),_ov=function(_ow,_ox){var _oy=E(_ow);return _oy[0]==0?E(_ox):A(_oy[1],[new T(function(){return _ov(_oy[2],_ox);})]);},_oz=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_oA=[1,_5O,_r],_oB=function(_oC){var _oD=E(_oC);if(!_oD[0]){return [0];}else{var _oE=E(_oD[1]);return [1,[0,_oE[1],_oE[2]],new T(function(){return _oB(_oD[2]);})];}},_oF=new T(function(){var _oG=_P(_r,_oA);if(!_oG[0]){return E(_nQ);}else{var _oH=_6a(new T(function(){return _5Y(_6m(_6x,[1,_oz,new T(function(){return _oB(_oG);})]));}));return E(_nQ);}}),_oI=[0,40],_oJ=function(_oK){return _oe(_o8,_oK);},_oL=[0,8],_oM=unCStr(" -> "),_oN=[0,9],_oO=[0,93],_oP=[0,91],_oQ=[0,41],_oR=[0,44],_oS=function(_oK){return [1,_oR,_oK];},_oT=function(_oU,_oV){var _oW=E(_oV);return _oW[0]==0?[0]:[1,_oU,[1,_oW[1],new T(function(){return _oT(_oU,_oW[2]);})]];},_oe=function(_oX,_oY){var _oZ=E(_oY),_p0=_oZ[3],_p1=E(_oZ[4]);if(!_p1[0]){return function(_p2){return _P(E(_p0)[5],_p2);};}else{var _p3=_p1[1],_p4=new T(function(){var _p5=E(_p0)[5],_p6=new T(function(){return _o9(_p1);}),_p7=new T(function(){return E(_oX)[1]<=9?function(_p8){return _P(_p5,[1,_o7,new T(function(){return A(_p6,[_p8]);})]);}:function(_p9){return [1,_2M,new T(function(){return _P(_p5,[1,_o7,new T(function(){return A(_p6,[[1,_2L,_p9]]);})]);})];};}),_pa=E(_p5);if(!_pa[0]){return E(_p7);}else{if(E(E(_pa[1])[1])==40){var _pb=E(_pa[2]);return _pb[0]==0?E(_p7):E(E(_pb[1])[1])==44?function(_pc){return [1,_oI,new T(function(){return A(new T(function(){var _pd=_6m(_oJ,_p1);if(!_pd[0]){return E(_9);}else{var _pe=new T(function(){return _oT(_oS,_pd[2]);});return function(_br){return _ov([1,_pd[1],_pe],_br);};}}),[[1,_oQ,_pc]]);})];}:E(_p7);}else{return E(_p7);}}}),_pf=E(_p1[2]);if(!_pf[0]){var _pg=E(_p0),_ph=E(_oF),_pi=hs_eqWord64(_pg[1],_ph[1]);if(!E(_pi)){return E(_p4);}else{var _pj=hs_eqWord64(_pg[2],_ph[2]);if(!E(_pj)){return E(_p4);}else{var _pk=new T(function(){return _oe(_o6,_p3);});return function(_pl){return [1,_oP,new T(function(){return A(_pk,[[1,_oO,_pl]]);})];};}}}else{if(!E(_pf[2])[0]){var _pm=E(_p0),_pn=E(_ot),_po=hs_eqWord64(_pm[1],_pn[1]);if(!E(_po)){return E(_p4);}else{var _pp=hs_eqWord64(_pm[2],_pn[2]);if(!E(_pp)){return E(_p4);}else{var _pq=new T(function(){return _oe(_oL,_pf[1]);}),_pr=new T(function(){return _oe(_oN,_p3);});return E(_oX)[1]<=8?function(_ps){return A(_pr,[new T(function(){return _P(_oM,new T(function(){return A(_pq,[_ps]);}));})]);}:function(_pt){return [1,_2M,new T(function(){return A(_pr,[new T(function(){return _P(_oM,new T(function(){return A(_pq,[[1,_2L,_pt]]);}));})]);})];};}}}else{return E(_p4);}}}},_pu=function(_pv,_pw,_px,_py){var _pz=new T(function(){return _2z(_pv);}),_pA=new T(function(){return _nN(_py);}),_pB=new T(function(){return _o2(_py);}),_pC=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_oe,[_o6,A(_pw,[_o1]),_r]);}));}),_pD=new T(function(){return A(_o4,[_px,_3c]);});return function(_pE){if(!E(new T(function(){var _pF=A(_pw,[_o1]),_pG=E(_o0),_pH=hs_eqWord64(_pF[1],_pG[1]);if(!E(_pH)){return false;}else{var _pI=hs_eqWord64(_pF[2],_pG[2]);return E(_pI)==0?false:true;}}))){var _pJ=new T(function(){return A(_pz,[[1,_pE,new T(function(){return A(_pB,[new T(function(){return A(_pA,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _P(_pE,_pC);}));})]);})]);})]]);}),_pK=A(_pD,[_pE]);if(!_pK[0]){return E(_pJ);}else{var _pL=E(_pK[1]);return E(_pL[2])[0]==0?E(_pK[2])[0]==0?A(_pz,[[2,_pL[1]]]):E(_pJ):E(_pJ);}}else{return A(_pz,[[2,_pE]]);}};},_pM=[0],_pN=new T(function(){return [0,"value"];}),_pO=function(_pP,_pQ,_pR,_pS,_pT,_pU){var _pV=E(_pP),_pW=_pV[1],_pX=new T(function(){return A(_pV[3],[_pM]);}),_pY=new T(function(){return _pu(_pV,_pR,_pS,_pT);});return A(_pW,[new T(function(){return _nK(_pQ,_pU);}),function(_pZ){var _q0=E(_pZ);return _q0[0]==0?E(_pX):A(_pW,[new T(function(){return A(_pQ,[function(_){var _q1=jsGet(E(_q0[1])[1],E(_pN)[1]);return [1,new T(function(){return fromJSStr(_q1);})];}]);}),function(_q2){var _q3=E(_q2);return _q3[0]==0?E(_pX):A(_pY,[_q3[1]]);}]);}]);},_q4=1,_q5=function(_q6){return E(E(_q6)[10]);},_q7=function(_q8,_q9){return A(_2z,[_q8,[0,_q9,_q9]]);},_qa=function(_qb){return E(E(_qb)[2]);},_qc=function(_qd,_qe,_qf){return A(_2z,[_qd,[0,_1,_qe]]);},_qg=function(_qh){return E(E(_qh)[2]);},_qi=function(_qj,_qk,_ql,_qm,_qn){var _qo=new T(function(){return _nv(_qj);}),_qp=new T(function(){return _qa(_qo);}),_qq=new T(function(){return _nx(_qk);}),_qr=new T(function(){return _2B(_qq);}),_qs=new T(function(){return _2T([0,coercionToken],_qr,function(_qt){return _q7(_qq,_qt);},function(_qu,_qv){return _qc(_qq,_qu,_qv);});}),_qw=new T(function(){return _2z(_qq);}),_qx=new T(function(){return _29(_qq);}),_qy=new T(function(){return _2z(_qq);}),_qz=new T(function(){return _29(_qq);}),_qA=new T(function(){return _2z(_qq);}),_qB=new T(function(){return _29(_qq);}),_qC=new T(function(){return _2z(_qq);}),_qD=new T(function(){return _29(_qq);}),_qE=new T(function(){return _qg(_qm);}),_qF=new T(function(){return _q5(_qj);});return function(_qG,_qH,_qI){return function(_qJ){return A(_qD,[new T(function(){var _qK=E(_qG);return _qK[0]==0?A(_qs,[_qJ]):A(_qC,[[0,_qK[1],_qJ]]);}),function(_qL){var _qM=new T(function(){return E(E(_qL)[1]);}),_qN=new T(function(){return _pO(_qr,function(_qO){return _nB(_qk,_qO);},_ql,_qn,_qj,_qM);}),_qP=new T(function(){return A(_qF,[_qM,_qH,new T(function(){var _qQ=E(_qI);if(!_qQ[0]){return [0];}else{var _qR=_qQ[1],_qS=_z(_ql,_nZ,_qR);return _qS[0]==0?A(_qE,[_qR]):E(_qS[1]);}}),_dl,_8]);});return A(_qB,[new T(function(){var _qT=new T(function(){return E(E(_qL)[2]);});return A(_qA,[[0,_qT,_qT]]);}),function(_qU){return A(_qz,[new T(function(){return A(_qy,[[0,_1,new T(function(){var _qV=E(E(_qU)[1]);return [0,_qV[1],_qV[2],_q4,_qV[4],_qV[5]];})]]);}),function(_qW){return A(_qx,[new T(function(){return A(_qN,[new T(function(){return E(E(_qW)[2]);})]);}),function(_qX){var _qY=E(_qX),_qZ=_qY[2],_r0=E(_qY[1]);switch(_r0[0]){case 0:return A(_qw,[[0,[0,_qP,_8],_qZ]]);case 1:return A(_qw,[[0,[0,new T(function(){return A(_qp,[new T(function(){return A(_qF,[_qM,_qH,_r0[1],_dl,_8]);}),_r0[2]]);}),_8],_qZ]]);default:var _r1=_r0[1];return A(_qw,[[0,[0,new T(function(){return A(_qF,[_qM,_qH,new T(function(){var _r2=_z(_ql,_nZ,_r1);return _r2[0]==0?A(_qE,[_r1]):E(_r2[1]);}),_dl,_8]);}),[1,_r1]],_qZ]]);}}]);}]);}]);}]);};};},_r3=new T(function(){return _qi(_8T,_8U,_nt,_no,_nd);}),_r4=new T(function(){return A(_r3,[_8,_5y,_8]);}),_r5=new T(function(){return A(_r3,[_8,_5y,_8]);}),_r6=function(_r7,_){var _r8=A(_r5,[_r7,_]),_r9=E(_r8),_ra=_r9[2],_rb=E(_r9[1]),_rc=A(_r4,[_ra,_]),_rd=E(_rc),_re=E(_rd[1]),_rf=new T(function(){return E(E(_ra)[4]);}),_rg=new T(function(){return E(E(_r7)[4]);});return [0,[0,function(_rh,_){var _ri=_47(_5x,_rh,_),_rj=_5e(_rb[1],_4M,_rg,_rh,_),_rk=_5s(_rh,_),_rl=_47(_5w,_rh,_),_rm=_5e(_re[1],_4M,_rf,_rh,_),_rn=_5s(_rh,_);return _rh;},new T(function(){var _ro=E(_rb[2]);if(!_ro[0]){return [0];}else{var _rp=E(_re[2]);return _rp[0]==0?[0]:[1,new T(function(){return _4N(_ro[1],_rp[1]);})];}})],_rd[2]];},_rq=function(_rr,_){var _rs=_3u(_r6,_4F,_rr,_),_rt=E(_rs),_ru=E(_rt[1]),_rv=new T(function(){return _4f(_4D,_ru[1]);});return [0,[0,function(_rw,_){var _rx=A(_4o,[_rw,_]),_ry=A(_rv,[_rw,_]);return _rw;},_ru[2]],_rt[2]];},_rz=unCStr("This widget sums recursively n numbers. When enters 0, present the result"),_rA=new T(function(){return _4f(_47,_rz);}),_rB=[0,0],_rC=new T(function(){return A(_r3,[_8,_5y,_8]);}),_rD=function(_rE,_){var _rF=A(_rC,[_rE,_]),_rG=E(_rF),_rH=E(_rG[1]),_rI=new T(function(){return E(E(_rE)[4]);});return [0,[0,function(_rJ,_){return _5e(_rH[1],_4M,_rI,_rJ,_);},_rH[2]],_rG[2]];},_rK=[1,_1],_rL=function(_rM){var _rN=new T(function(){return _4t(_47,new T(function(){return _4p(_rM);}));});return function(_br,_rO){return _3u(_rD,function(_rP){var _rQ=E(E(_rP)[1]);if(!_rQ){return function(_rR,_){return [0,[0,function(_rS,_){var _rT=_5s(_rS,_),_rU=_47(_4C,_rS,_),_rV=A(_rN,[_rS,_]);return _rS;},_8],_rR];};}else{var _rW=new T(function(){return _rL(new T(function(){return [0,E(_rM)[1]+_rQ|0];}));}),_rX=new T(function(){return _4t(_47,new T(function(){return _2N(0,E(_rM)[1]+_rQ|0,_r);}));});return function(_br,_rO){return _3u(function(_rY,_){return [0,[0,function(_rZ,_){var _s0=A(_rX,[_rZ,_]),_s1=_5s(_rZ,_);return _rZ;},_rK],_rY];},function(_s2){return E(_rW);},_br,_rO);};}},_br,_rO);};},_s3=new T(function(){return _rL(_rB);}),_s4=new T(function(){return A(_r3,[_8,_5y,_8]);}),_s5=function(_s6,_){var _s7=A(_s4,[_s6,_]),_s8=E(_s7),_s9=E(_s8[1]),_sa=new T(function(){return E(E(_s6)[4]);});return [0,[0,function(_sb,_){var _sc=_5e(_s9[1],_4M,_sa,_sb,_),_sd=_5s(_sb,_);return _sb;},_s9[2]],_s8[2]];},_se=[1,_s5,_r],_sf=function(_sg){return _sg>1?[1,_s5,new T(function(){return _sf(_sg-1|0);})]:E(_se);},_sh=new T(function(){return _sf(3);}),_si=[1,_rB],_sj=[0,_26,_si],_sk=function(_sl,_){return [0,_sj,_sl];},_sm=function(_sn,_so,_){var _sp=A(_so,[_]);return _sn;},_sq=function(_sr,_ss,_){var _st=A(_ss,[_]);return new T(function(){return A(_sr,[_st]);});},_su=[0,_sq,_sm],_sv=function(_sw){var _sx=E(_sw);return _sx[0]==0?0:E(_sx[1])[1]+_sv(_sx[2])|0;},_sy=function(_sz){return [0,_sv(_sz)];},_sA=[0,_rB,_4N,_sy],_sB=function(_sC,_sD){var _sE=E(_sD);return _sE[0]==0?[0]:[1,new T(function(){return A(_sC,[_sE[1]]);})];},_sF=function(_sG,_sH,_sI,_sJ,_sK,_sL){var _sM=new T(function(){return _qa(_sG);});return A(_sH,[new T(function(){return A(_sJ,[_sL]);}),function(_sN){var _sO=E(_sN),_sP=E(_sO[1]);return A(_sH,[new T(function(){return A(_sK,[_sO[2]]);}),function(_sQ){var _sR=E(_sQ),_sS=E(_sR[1]);return A(_sI,[[0,[0,new T(function(){return A(_sM,[_sP[1],_sS[1]]);}),new T(function(){var _sT=E(_sP[2]);if(!_sT[0]){return [0];}else{var _sU=E(_sS[2]);return _sU[0]==0?[0]:[1,new T(function(){return A(_sT[1],[_sU[1]]);})];}})],_sR[2]]]);}]);}]);},_sV=function(_sW){return E(E(_sW)[1]);},_sX=function(_sY,_sZ,_t0,_t1,_t2,_t3){var _t4=new T(function(){return _nv(_sY);});return function(_t5){var _t6=E(_sZ);return _sF(_t4,_t6[1],_t6[3],function(_t7){return A(new T(function(){var _t8=new T(function(){return _qa(_t1);});return A(_sV,[_t0,function(_t9){return [0,new T(function(){var _ta=E(E(_t9)[1]);return [0,_ta[1],new T(function(){return _sB(_t8,_ta[2]);})];}),new T(function(){return E(E(_t9)[2]);})];}]);}),[new T(function(){return A(_t2,[_t7]);})]);},_t3,_t5);};},_tb=function(_tc,_td){while(1){var _te=(function(_tf,_tg){var _th=E(_tg);if(!_th[0]){return E(_tf);}else{_tc=new T(function(){return _sX(_8T,_28,_su,_sA,_tf,_th[1]);});_td=_th[2];return null;}})(_tc,_td);if(_te!=null){return _te;}}},_ti=new T(function(){return _tb(_sk,_sh);}),_tj=new T(function(){return _4f(_47,_4c);}),_tk=function(_tl){var _tm=new T(function(){return _4t(_47,new T(function(){return _4p(_tl);}));});return function(_tn,_){return [0,[0,function(_to,_){var _tp=_5s(_to,_),_tq=_47(_4C,_to,_),_tr=A(_tm,[_to,_]);return _to;},_rK],_tn];};},_ts=function(_tt,_){var _tu=_3u(_ti,_tk,_tt,_),_tv=E(_tu),_tw=E(_tv[1]),_tx=new T(function(){return _4f(_4D,_tw[1]);});return [0,[0,function(_ty,_){var _tz=A(_tj,[_ty,_]),_tA=A(_tx,[_ty,_]);return _ty;},_tw[2]],_tv[2]];},_tB=function(_tC,_){var _tD=_rq(_tC,_),_tE=E(_tD),_tF=_ts(_tE[2],_),_tG=E(_tF),_tH=A(_s3,[_tG[2],_]),_tI=E(_tH),_tJ=E(_tI[1]);return [0,[0,function(_tK,_){var _tL=A(E(_tE[1])[1],[_tK,_]),_tM=A(E(_tG[1])[1],[_tK,_]),_tN=A(_rA,[_tK,_]),_tO=A(_tJ[1],[_tK,_]);return _tK;},_tJ[2]],_tI[2]];},_tP=unCStr("idelem"),_tQ=function(_){var _tR=E(_tP),_tS=jsFind(toJSStr(_tR)),_tT=E(_tS);return _tT[0]==0?_3k(_tR):_3e(_tB,_tT[1],_);},_tU=function(_){return _tQ(_);};
var hasteMain = function() {A(_tU, [0]);};window.onload = hasteMain;