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

var _0=0,_1=unCStr("idelem"),_2=function(_3,_4,_){var _5=jsCreateTextNode(toJSStr(E(_3))),_6=jsAppendChild(_5,E(_4)[1]);return [0,_5];},_7=function(_8,_9){var _a=E(_8);return _a[0]==0?E(_9):[1,_a[1],new T(function(){return _7(_a[2],_9);})];},_b=function(_c,_d){var _e=jsShowI(_c);return _7(fromJSStr(_e),_d);},_f=[0,41],_g=[0,40],_h=function(_i,_j,_k){return _j>=0?_b(_j,_k):_i<=6?_b(_j,_k):[1,_g,new T(function(){var _l=jsShowI(_j);return _7(fromJSStr(_l),[1,_f,_k]);})];},_m=[0],_n=[0],_o=[0,98],_p=[1,_o,_n],_q=unCStr("result: "),_r=function(_s,_t,_){var _u=A(_s,[_]);return A(_t,[_]);},_v=function(_w,_x,_){return _r(_w,_x,_);},_y=function(_z,_A,_){var _B=A(_z,[_]);return A(_A,[_B,_]);},_C=unCStr("base"),_D=unCStr("GHC.IO.Exception"),_E=unCStr("IOException"),_F=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_C,_D,_E],_G=[0,I_fromBits([4053623282,1685460941]),I_fromBits([3693590983,2507416641]),_F,_n],_H=function(_I){return E(_G);},_J=function(_K){return E(E(_K)[1]);},_L=unCStr("Maybe.fromJust: Nothing"),_M=new T(function(){return err(_L);}),_N=function(_O,_P,_Q){var _R=new T(function(){var _S=A(_O,[_Q]),_T=A(_P,[new T(function(){var _U=E(_R);return _U[0]==0?E(_M):E(_U[1]);})]),_V=hs_eqWord64(_S[1],_T[1]);if(!E(_V)){return [0];}else{var _W=hs_eqWord64(_S[2],_T[2]);return E(_W)==0?[0]:[1,_Q];}});return E(_R);},_X=function(_Y){var _Z=E(_Y);return _N(_J(_Z[1]),_H,_Z[2]);},_10=unCStr(": "),_11=[0,41],_12=unCStr(" ("),_13=unCStr("already exists"),_14=unCStr("does not exist"),_15=unCStr("protocol error"),_16=unCStr("failed"),_17=unCStr("invalid argument"),_18=unCStr("inappropriate type"),_19=unCStr("hardware fault"),_1a=unCStr("unsupported operation"),_1b=unCStr("timeout"),_1c=unCStr("resource vanished"),_1d=unCStr("interrupted"),_1e=unCStr("resource busy"),_1f=unCStr("resource exhausted"),_1g=unCStr("end of file"),_1h=unCStr("illegal operation"),_1i=unCStr("permission denied"),_1j=unCStr("user error"),_1k=unCStr("unsatisified constraints"),_1l=unCStr("system error"),_1m=function(_1n,_1o){switch(E(_1n)){case 0:return _7(_13,_1o);case 1:return _7(_14,_1o);case 2:return _7(_1e,_1o);case 3:return _7(_1f,_1o);case 4:return _7(_1g,_1o);case 5:return _7(_1h,_1o);case 6:return _7(_1i,_1o);case 7:return _7(_1j,_1o);case 8:return _7(_1k,_1o);case 9:return _7(_1l,_1o);case 10:return _7(_15,_1o);case 11:return _7(_16,_1o);case 12:return _7(_17,_1o);case 13:return _7(_18,_1o);case 14:return _7(_19,_1o);case 15:return _7(_1a,_1o);case 16:return _7(_1b,_1o);case 17:return _7(_1c,_1o);default:return _7(_1d,_1o);}},_1p=[0,125],_1q=unCStr("{handle: "),_1r=function(_1s,_1t,_1u,_1v,_1w,_1x){var _1y=new T(function(){var _1z=new T(function(){return _1m(_1t,new T(function(){var _1A=E(_1v);return _1A[0]==0?E(_1x):_7(_12,new T(function(){return _7(_1A,[1,_11,_1x]);}));}));}),_1B=E(_1u);return _1B[0]==0?E(_1z):_7(_1B,new T(function(){return _7(_10,_1z);}));}),_1C=E(_1w);if(!_1C[0]){var _1D=E(_1s);if(!_1D[0]){return E(_1y);}else{var _1E=E(_1D[1]);return _1E[0]==0?_7(_1q,new T(function(){return _7(_1E[1],[1,_1p,new T(function(){return _7(_10,_1y);})]);})):_7(_1q,new T(function(){return _7(_1E[1],[1,_1p,new T(function(){return _7(_10,_1y);})]);}));}}else{return _7(_1C[1],new T(function(){return _7(_10,_1y);}));}},_1F=function(_1G){var _1H=E(_1G);return _1r(_1H[1],_1H[2],_1H[3],_1H[4],_1H[6],_n);},_1I=function(_1J,_1K){var _1L=E(_1J);return _1r(_1L[1],_1L[2],_1L[3],_1L[4],_1L[6],_1K);},_1M=[0,44],_1N=[0,93],_1O=[0,91],_1P=function(_1Q,_1R,_1S){var _1T=E(_1R);return _1T[0]==0?unAppCStr("[]",_1S):[1,_1O,new T(function(){return A(_1Q,[_1T[1],new T(function(){var _1U=function(_1V){var _1W=E(_1V);return _1W[0]==0?E([1,_1N,_1S]):[1,_1M,new T(function(){return A(_1Q,[_1W[1],new T(function(){return _1U(_1W[2]);})]);})];};return _1U(_1T[2]);})]);})];},_1X=function(_1Y,_1Z){return _1P(_1I,_1Y,_1Z);},_20=function(_21,_22,_23){var _24=E(_22);return _1r(_24[1],_24[2],_24[3],_24[4],_24[6],_23);},_25=[0,_20,_1F,_1X],_26=new T(function(){return [0,_H,_25,_27,_X];}),_27=function(_28){return [0,_26,_28];},_29=7,_2a=function(_2b){return [0,_m,_29,_n,_2b,_m,_m];},_2c=function(_2d,_){return die(new T(function(){return _27(new T(function(){return _2a(_2d);}));}));},_2e=function(_2f,_){return _2c(_2f,_);},_2g=function(_2h,_){return _2h;},_2i=[0,_y,_v,_2g,_2e],_2j=function(_2k){return E(_2k);},_2l=[0,_2i,_2j],_2m=unCStr("id"),_2n=function(_2o){return E(E(_2o)[1]);},_2p=function(_2q,_2r,_2s,_2t){return A(_2n,[_2q,new T(function(){return A(_2r,[_2t]);}),function(_2u){return A(_2s,[new T(function(){return E(E(_2u)[1]);}),new T(function(){return E(E(_2u)[2]);})]);}]);},_2v=function(_2w,_2x,_2y,_2z){return A(_2n,[_2w,new T(function(){return A(_2x,[_2z]);}),function(_2A){return A(_2y,[new T(function(){return E(E(_2A)[2]);})]);}]);},_2B=function(_2C,_2D,_2E,_2F){return _2v(_2C,_2D,_2E,_2F);},_2G=function(_2H){return E(E(_2H)[4]);},_2I=function(_2J,_2K){var _2L=new T(function(){return A(_2G,[_2J,_2K]);});return function(_2M){return E(_2L);};},_2N=function(_2O){return E(E(_2O)[3]);},_2P=function(_2Q){var _2R=new T(function(){return _2N(_2Q);});return [0,function(_2D,_2E,_2F){return _2p(_2Q,_2D,_2E,_2F);},function(_2D,_2E,_2F){return _2B(_2Q,_2D,_2E,_2F);},function(_2S,_2T){return A(_2R,[[0,_2S,_2T]]);},function(_2F){return _2I(_2Q,_2F);}];},_2U=function(_2V){return E(E(_2V)[1]);},_2W=function(_2X){return E(E(_2X)[2]);},_2Y=function(_2Z,_30){var _31=new T(function(){return A(_2W,[_2Z,_30]);}),_32=new T(function(){return _2U(_2Z);}),_33=new T(function(){return _2N(_32);}),_34=new T(function(){return _2n(_32);});return function(_35){return A(_34,[_31,function(_36){return A(_33,[[0,_36,_35]]);}]);};},_37=[0,112],_38=function(_39,_3a,_3b,_3c){var _3d=E(_3a);return A(_3d[1],[new T(function(){var _3e=E(_39);return E(_3b);}),function(_3f){var _3g=new T(function(){return E(E(_3f)[2]);});return A(_3d[2],[new T(function(){return A(_3c,[new T(function(){var _3h=E(new T(function(){var _3i=E(_39);return [0,coercionToken];})),_3j=E(_3f);return [0,_3j[1],new T(function(){return [0,E(_3g)[1]+1|0];}),_3j[3],_3j[4]];})]);}),new T(function(){return A(_3d[3],[[1,_37,new T(function(){return _7(_h(0,E(_3g)[1],_n),new T(function(){return E(E(_3f)[1]);}));})]]);})]);}]);},_3k=function(_3l,_3m,_3n,_3o){return A(_3l,[new T(function(){return function(_){var _3p=jsSetAttr(E(_3m)[1],toJSStr(E(_3n)),toJSStr(E(_3o)));return _0;};})]);},_3q=function(_3r,_3s){return A(_2N,[_3r,[0,_3s,_3s]]);},_3t=unCStr("div"),_3u=unCStr("old"),_3v=function(_3w,_3x,_){var _3y=jsCreateElem(toJSStr(E(_3w))),_3z=jsAppendChild(_3y,E(_3x)[1]);return [0,_3y];},_3A=function(_3B,_3C,_3D){return A(_2N,[_3B,[0,_0,_3C]]);},_3E=function(_3F){var _3G=new T(function(){return _2U(_3F);}),_3H=new T(function(){return _38([0,coercionToken],_2P(_3G),function(_3I){return _3q(_3G,_3I);},function(_3J,_3K){return _3A(_3G,_3J,_3K);});}),_3L=new T(function(){return _2N(_3G);}),_3M=new T(function(){return _2n(_3G);}),_3N=new T(function(){return _2n(_3G);}),_3O=new T(function(){return _2n(_3G);}),_3P=new T(function(){return _2n(_3G);});return function(_3Q,_3R){return A(_3P,[new T(function(){return A(_3H,[_3R]);}),function(_3S){var _3T=new T(function(){return E(E(_3S)[1]);}),_3U=new T(function(){return _7(_3T,_3u);});return A(_3O,[new T(function(){return A(_2Y,[_3F,function(_){return jsFind(toJSStr(E(_3T)));},new T(function(){return E(E(_3S)[2]);})]);}),function(_3V){return A(_3N,[new T(function(){return A(_3Q,[new T(function(){return E(E(_3V)[2]);})]);}),function(_3W){var _3X=E(_3W),_3Y=_3X[2],_3Z=E(_3X[1]),_40=_3Z[1],_41=_3Z[2],_42=E(E(_3V)[1]);if(!_42[0]){return A(_3L,[[0,[0,function(_43,_){var _44=_3v(_3t,_43,_),_45=A(_3k,[_2j,_44,_2m,_3T,_]),_46=A(_40,[_44,_]);return _44;},_41],_3Y]]);}else{var _47=_42[1];return A(_3M,[new T(function(){return A(_3k,[function(_48){return _2Y(_3F,_48);},_47,_2m,_3U,_3Y]);}),function(_49){return A(_3L,[[0,[0,function(_4a,_){var _4b=_3v(_3t,_4a,_),_4c=A(_3k,[_2j,_4b,_2m,_3T,_]),_4d=A(_40,[_4b,_]),_4e=jsClearChildren(E(_47)[1]);return _4b;},_41],new T(function(){return E(E(_49)[2]);})]]);}]);}}]);}]);}]);};},_4f=new T(function(){return _3E(_2l);}),_4g=[13,coercionToken],_4h=function(_4i,_4j){return [0,E(_4i)[1]+E(_4j)[1]|0];},_4k=new T(function(){return [0,"keydown"];}),_4l=new T(function(){return [0,"mousemove"];}),_4m=new T(function(){return [0,"blur"];}),_4n=new T(function(){return [0,"focus"];}),_4o=new T(function(){return [0,"change"];}),_4p=new T(function(){return [0,"unload"];}),_4q=new T(function(){return [0,"load"];}),_4r=new T(function(){return [0,"keyup"];}),_4s=new T(function(){return [0,"keypress"];}),_4t=new T(function(){return [0,"mouseup"];}),_4u=new T(function(){return [0,"mousedown"];}),_4v=new T(function(){return [0,"dblclick"];}),_4w=new T(function(){return [0,"click"];}),_4x=new T(function(){return [0,"mouseout"];}),_4y=new T(function(){return [0,"mouseover"];}),_4z=function(_4A){switch(E(_4A)[0]){case 0:return E(_4q);case 1:return E(_4p);case 2:return E(_4o);case 3:return E(_4n);case 4:return E(_4m);case 5:return E(_4l);case 6:return E(_4y);case 7:return E(_4x);case 8:return E(_4w);case 9:return E(_4v);case 10:return E(_4u);case 11:return E(_4t);case 12:return E(_4s);case 13:return E(_4r);default:return E(_4k);}},_4B=new T(function(){return [0,"(function(e) {e.focus();})"];}),_4C=function(_4D){var _4E=A(_4D,[_]);return E(_4E);},_4F=function(_4G){return _4C(function(_){var _=0;return eval(E(_4G)[1]);});},_4H=new T(function(){return _4F(_4B);}),_4I=function(_4J,_){var _4K=A(_4H,[E(E(_4J)[1]),_]);return _0;},_4L=function(_4M,_){return _4I(_4M,_);},_4N=function(_4O,_4P,_4Q,_4R,_){var _4S=A(_4O,[_4R,_]),_4T=jsSetCB(E(_4R)[1],_4z(_4P)[1],function(_){var _4U=A(_4Q,[_]);return _4L(_4R,_);});return _4S;},_4V=unCStr("br"),_4W=function(_4X,_){var _4Y=jsCreateElem(toJSStr(E(_4V))),_4Z=jsAppendChild(_4Y,E(_4X)[1]);return [0,_4Y];},_50=unCStr("second number "),_51=unCStr("first number  "),_52=unCStr("text"),_53=function(_54,_55,_56,_){var _57=_3v(_54,_56,_),_58=A(_55,[_57,_]);return _57;},_59=unCStr("()"),_5a=unCStr("GHC.Tuple"),_5b=unCStr("ghc-prim"),_5c=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5b,_5a,_59],_5d=[0,I_fromBits([2170319554,3688774321]),I_fromBits([26914641,3196943984]),_5c,_n],_5e=function(_5f){return E(_5d);},_5g=unCStr("main"),_5h=unCStr("Builder"),_5i=unCStr("JSBuilderM"),_5j=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_5g,_5h,_5i],_5k=[0,I_fromBits([3437130497,2826858540]),I_fromBits([793301065,3695859575]),_5j,_n],_5l=function(_5m){return E(_5k);},_5n=function(_5o){var _5p=E(_5o);return _5p[0]==0?[0]:_7(_5p[1],new T(function(){return _5n(_5p[2]);}));},_5q=function(_5r,_5s){var _5t=E(_5r);if(!_5t){return [0,_n,_5s];}else{var _5u=E(_5s);if(!_5u[0]){return [0,_n,_n];}else{var _5v=new T(function(){var _5w=_5q(_5t-1|0,_5u[2]);return [0,_5w[1],_5w[2]];});return [0,[1,_5u[1],new T(function(){return E(E(_5v)[1]);})],new T(function(){return E(E(_5v)[2]);})];}}},_5x=[0,120],_5y=[0,48],_5z=function(_5A){var _5B=new T(function(){var _5C=_5q(8,new T(function(){var _5D=md5(toJSStr(E(_5A)));return fromJSStr(_5D);}));return [0,_5C[1],_5C[2]];}),_5E=parseInt([0,toJSStr([1,_5y,[1,_5x,new T(function(){return E(E(_5B)[1]);})]])]),_5F=new T(function(){var _5G=_5q(8,new T(function(){return E(E(_5B)[2]);}));return [0,_5G[1],_5G[2]];}),_5H=parseInt([0,toJSStr([1,_5y,[1,_5x,new T(function(){return E(E(_5F)[1]);})]])]),_5I=hs_mkWord64(_5E,_5H),_5J=parseInt([0,toJSStr([1,_5y,[1,_5x,new T(function(){return E(_5q(8,new T(function(){return E(E(_5F)[2]);}))[1]);})]])]),_5K=hs_mkWord64(_5J,_5J);return [0,_5I,_5K];},_5L=function(_5M,_5N){var _5O=E(_5N);return _5O[0]==0?[0]:[1,new T(function(){return A(_5M,[_5O[1]]);}),new T(function(){return _5L(_5M,_5O[2]);})];},_5P=function(_5Q,_5R){var _5S=jsShowI(_5Q),_5T=md5(_5S);return _7(fromJSStr(_5T),new T(function(){var _5U=jsShowI(_5R),_5V=md5(_5U);return fromJSStr(_5V);}));},_5W=function(_5X){var _5Y=E(_5X);return _5P(_5Y[1],_5Y[2]);},_5Z=function(_60){var _61=E(_60);if(!_61[0]){return [0];}else{var _62=E(_61[1]);return [1,[0,_62[1],_62[2]],new T(function(){return _5Z(_61[2]);})];}},_63=unCStr("Prelude.undefined"),_64=new T(function(){return err(_63);}),_65=function(_66,_67){return function(_68){return E(new T(function(){var _69=A(_66,[_64]),_6a=E(_69[3]),_6b=_6a[1],_6c=_6a[2],_6d=_7(_69[4],[1,new T(function(){return A(_67,[_64]);}),_n]);if(!_6d[0]){return [0,_6b,_6c,_6a,_n];}else{var _6e=_5z(new T(function(){return _5n(_5L(_5W,[1,[0,_6b,_6c],new T(function(){return _5Z(_6d);})]));}));return [0,_6e[1],_6e[2],_6a,_6d];}}));};},_6f=new T(function(){return _65(_5l,_5e);}),_6g=function(_6h,_6i,_6j,_){var _6k=E(_6i),_6l=A(_6h,[_6j,_]),_6m=A(_3k,[_2j,_6l,_6k[1],_6k[2],_]);return _6l;},_6n=function(_6o,_6p){while(1){var _6q=(function(_6r,_6s){var _6t=E(_6s);if(!_6t[0]){return E(_6r);}else{_6o=function(_6u,_){return _6g(_6r,_6t[1],_6u,_);};_6p=_6t[2];return null;}})(_6o,_6p);if(_6q!=null){return _6q;}}},_6v=unCStr("value"),_6w=unCStr("onclick"),_6x=unCStr("checked"),_6y=[0,_6x,_n],_6z=[1,_6y,_n],_6A=unCStr("type"),_6B=unCStr("input"),_6C=function(_6D,_){return _3v(_6B,_6D,_);},_6E=function(_6F,_6G,_6H,_6I,_6J){var _6K=new T(function(){var _6L=new T(function(){return _6n(_6C,[1,[0,_6A,_6G],[1,[0,_2m,_6F],[1,[0,_6v,_6H],_n]]]);});return !E(_6I)?E(_6L):_6n(_6L,_6z);}),_6M=E(_6J);return _6M[0]==0?E(_6K):_6n(_6K,[1,[0,_6w,_6M[1]],_n]);},_6N=unCStr("href"),_6O=[0,97],_6P=[1,_6O,_n],_6Q=function(_6R,_){return _3v(_6P,_6R,_);},_6S=function(_6T,_6U){var _6V=new T(function(){return _6n(_6Q,[1,[0,_6N,_6T],_n]);});return function(_6W,_){var _6X=A(_6V,[_6W,_]),_6Y=A(_6U,[_6X,_]);return _6X;};},_6Z=function(_70){return _6S(_70,function(_6u,_){return _2(_70,_6u,_);});},_71=unCStr("option"),_72=function(_73,_){return _3v(_71,_73,_);},_74=unCStr("selected"),_75=[0,_74,_n],_76=[1,_75,_n],_77=function(_78,_79,_7a){var _7b=new T(function(){return _6n(_72,[1,[0,_6v,_78],_n]);}),_7c=function(_7d,_){var _7e=A(_7b,[_7d,_]),_7f=A(_79,[_7e,_]);return _7e;};return !E(_7a)?E(_7c):_6n(_7c,_76);},_7g=function(_7h,_7i){return _77(_7h,function(_6u,_){return _2(_7h,_6u,_);},_7i);},_7j=unCStr("method"),_7k=unCStr("action"),_7l=unCStr("UTF-8"),_7m=unCStr("acceptCharset"),_7n=[0,_7m,_7l],_7o=unCStr("form"),_7p=function(_7q,_){return _3v(_7o,_7q,_);},_7r=function(_7s,_7t,_7u){var _7v=new T(function(){return _6n(_7p,[1,_7n,[1,[0,_7k,_7s],[1,[0,_7j,_7t],_n]]]);});return function(_7w,_){var _7x=A(_7v,[_7w,_]),_7y=A(_7u,[_7x,_]);return _7x;};},_7z=unCStr("select"),_7A=function(_7B,_){return _3v(_7z,_7B,_);},_7C=function(_7D,_7E){var _7F=new T(function(){return _6n(_7A,[1,[0,_2m,_7D],_n]);});return function(_7G,_){var _7H=A(_7F,[_7G,_]),_7I=A(_7E,[_7H,_]);return _7H;};},_7J=unCStr("textarea"),_7K=function(_7L,_){return _3v(_7J,_7L,_);},_7M=function(_7N,_7O){var _7P=new T(function(){return _6n(_7K,[1,[0,_2m,_7N],_n]);});return function(_7Q,_){var _7R=A(_7P,[_7Q,_]),_7S=_2(_7O,_7R,_);return _7R;};},_7T=unCStr("color:red"),_7U=unCStr("style"),_7V=[0,_7U,_7T],_7W=[1,_7V,_n],_7X=[0,98],_7Y=[1,_7X,_n],_7Z=function(_80){return _6n(function(_81,_){var _82=_3v(_7Y,_81,_),_83=A(_80,[_82,_]);return _82;},_7W);},_84=unCStr("toByteString not defined"),_85=new T(function(){return err(_84);}),_86=function(_87,_88,_){var _89=E(_87);if(!_89[0]){return _88;}else{var _8a=A(_89[1],[_88,_]),_8b=_86(_89[2],_88,_);return _88;}},_8c=function(_8d,_8e,_8f,_){var _8g=A(_8d,[_8f,_]),_8h=A(_8e,[_8f,_]);return _8f;},_8i=[0,_2g,_8c,_86],_8j=[0,_8i,_6f,_85,_2,_2,_53,_7Z,_6S,_6Z,_6E,_7M,_7C,_77,_7g,_7r,_6n],_8k=unCStr("base"),_8l=unCStr("Control.Exception.Base"),_8m=unCStr("PatternMatchFail"),_8n=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_8k,_8l,_8m],_8o=[0,I_fromBits([18445595,3739165398]),I_fromBits([52003073,3246954884]),_8n,_n],_8p=function(_8q){return E(_8o);},_8r=function(_8s){var _8t=E(_8s);return _N(_J(_8t[1]),_8p,_8t[2]);},_8u=function(_8v){return E(E(_8v)[1]);},_8w=function(_8x,_8y){return _7(E(_8x)[1],_8y);},_8z=function(_8A,_8B){return _1P(_8w,_8A,_8B);},_8C=function(_8D,_8E,_8F){return _7(E(_8E)[1],_8F);},_8G=[0,_8C,_8u,_8z],_8H=new T(function(){return [0,_8p,_8G,_8I,_8r];}),_8I=function(_8J){return [0,_8H,_8J];},_8K=unCStr("Non-exhaustive patterns in"),_8L=function(_8M,_8N){return die(new T(function(){return A(_8N,[_8M]);}));},_8O=function(_8P,_8Q){var _8R=E(_8Q);if(!_8R[0]){return [0,_n,_n];}else{var _8S=_8R[1];if(!A(_8P,[_8S])){return [0,_n,_8R];}else{var _8T=new T(function(){var _8U=_8O(_8P,_8R[2]);return [0,_8U[1],_8U[2]];});return [0,[1,_8S,new T(function(){return E(E(_8T)[1]);})],new T(function(){return E(E(_8T)[2]);})];}}},_8V=[0,32],_8W=[0,10],_8X=[1,_8W,_n],_8Y=function(_8Z){return E(E(_8Z)[1])==124?false:true;},_90=function(_91,_92){var _93=_8O(_8Y,unCStr(_91)),_94=_93[1],_95=function(_96,_97){return _7(_96,new T(function(){return unAppCStr(": ",new T(function(){return _7(_92,new T(function(){return _7(_97,_8X);}));}));}));},_98=E(_93[2]);return _98[0]==0?_95(_94,_n):E(E(_98[1])[1])==124?_95(_94,[1,_8V,_98[2]]):_95(_94,_n);},_99=function(_9a){return _8L([0,new T(function(){return _90(_9a,_8K);})],_8I);},_9b=new T(function(){return _99("Text\\ParserCombinators\\ReadP.hs:(134,3)-(157,60)|function mplus");}),_9c=function(_9d,_9e){while(1){var _9f=(function(_9g,_9h){var _9i=E(_9g);switch(_9i[0]){case 0:var _9j=E(_9h);if(!_9j[0]){return [0];}else{_9d=A(_9i[1],[_9j[1]]);_9e=_9j[2];return null;}break;case 1:var _9k=A(_9i[1],[_9h]),_9l=_9h;_9d=_9k;_9e=_9l;return null;case 2:return [0];case 3:return [1,[0,_9i[1],_9h],new T(function(){return _9c(_9i[2],_9h);})];default:return E(_9i[1]);}})(_9d,_9e);if(_9f!=null){return _9f;}}},_9m=function(_9n,_9o){var _9p=new T(function(){var _9q=E(_9o);if(_9q[0]==3){return [3,_9q[1],new T(function(){return _9m(_9n,_9q[2]);})];}else{var _9r=E(_9n);if(_9r[0]==2){return E(_9q);}else{var _9s=E(_9q);if(_9s[0]==2){return E(_9r);}else{var _9t=new T(function(){var _9u=E(_9s);if(_9u[0]==4){return [1,function(_9v){return [4,new T(function(){return _7(_9c(_9r,_9v),_9u[1]);})];}];}else{var _9w=E(_9r);if(_9w[0]==1){var _9x=_9w[1],_9y=E(_9u);return _9y[0]==0?[1,function(_9z){return _9m(A(_9x,[_9z]),_9y);}]:[1,function(_9A){return _9m(A(_9x,[_9A]),new T(function(){return A(_9y[1],[_9A]);}));}];}else{var _9B=E(_9u);return _9B[0]==0?E(_9b):[1,function(_9C){return _9m(_9w,new T(function(){return A(_9B[1],[_9C]);}));}];}}}),_9D=E(_9r);switch(_9D[0]){case 1:var _9E=E(_9s);return _9E[0]==4?[1,function(_9F){return [4,new T(function(){return _7(_9c(A(_9D[1],[_9F]),_9F),_9E[1]);})];}]:E(_9t);case 4:var _9G=_9D[1],_9H=E(_9s);switch(_9H[0]){case 0:return [1,function(_9I){return [4,new T(function(){return _7(_9G,new T(function(){return _9c(_9H,_9I);}));})];}];case 1:return [1,function(_9J){return [4,new T(function(){return _7(_9G,new T(function(){return _9c(A(_9H[1],[_9J]),_9J);}));})];}];default:return [4,new T(function(){return _7(_9G,_9H[1]);})];}break;default:return E(_9t);}}}}}),_9K=E(_9n);switch(_9K[0]){case 0:var _9L=E(_9o);return _9L[0]==0?[0,function(_9M){return _9m(A(_9K[1],[_9M]),new T(function(){return A(_9L[1],[_9M]);}));}]:E(_9p);case 3:return [3,_9K[1],new T(function(){return _9m(_9K[2],_9o);})];default:return E(_9p);}},_9N=function(_9O,_9P){return E(_9O)[1]!=E(_9P)[1];},_9Q=function(_9R,_9S){return E(_9R)[1]==E(_9S)[1];},_9T=[0,_9Q,_9N],_9U=function(_9V){return E(E(_9V)[1]);},_9W=function(_9X,_9Y,_9Z){while(1){var _a0=E(_9Y);if(!_a0[0]){return E(_9Z)[0]==0?true:false;}else{var _a1=E(_9Z);if(!_a1[0]){return false;}else{if(!A(_9U,[_9X,_a0[1],_a1[1]])){return false;}else{_9Y=_a0[2];_9Z=_a1[2];continue;}}}}},_a2=function(_a3,_a4,_a5){return !_9W(_a3,_a4,_a5)?true:false;},_a6=function(_a7){return [0,function(_a8,_a9){return _9W(_a7,_a8,_a9);},function(_a8,_a9){return _a2(_a7,_a8,_a9);}];},_aa=new T(function(){return _a6(_9T);}),_ab=function(_ac,_ad){var _ae=E(_ac);switch(_ae[0]){case 0:return [0,function(_af){return _ab(A(_ae[1],[_af]),_ad);}];case 1:return [1,function(_ag){return _ab(A(_ae[1],[_ag]),_ad);}];case 2:return [2];case 3:return _9m(A(_ad,[_ae[1]]),new T(function(){return _ab(_ae[2],_ad);}));default:var _ah=function(_ai){var _aj=E(_ai);if(!_aj[0]){return [0];}else{var _ak=E(_aj[1]);return _7(_9c(A(_ad,[_ak[1]]),_ak[2]),new T(function(){return _ah(_aj[2]);}));}},_al=_ah(_ae[1]);return _al[0]==0?[2]:[4,_al];}},_am=[2],_an=function(_ao){return [3,_ao,_am];},_ap=function(_aq,_ar){var _as=E(_aq);if(!_as){return A(_ar,[_0]);}else{var _at=new T(function(){return _ap(_as-1|0,_ar);});return [0,function(_au){return E(_at);}];}},_av=function(_aw,_ax,_ay){var _az=new T(function(){return A(_aw,[_an]);});return [1,function(_aA){return A(function(_aB,_aC,_aD){while(1){var _aE=(function(_aF,_aG,_aH){var _aI=E(_aF);switch(_aI[0]){case 0:var _aJ=E(_aG);if(!_aJ[0]){return E(_ax);}else{_aB=A(_aI[1],[_aJ[1]]);_aC=_aJ[2];var _aK=_aH+1|0;_aD=_aK;return null;}break;case 1:var _aL=A(_aI[1],[_aG]),_aM=_aG,_aK=_aH;_aB=_aL;_aC=_aM;_aD=_aK;return null;case 2:return E(_ax);case 3:return function(_aN){var _aO=new T(function(){return _ab(_aI,_aN);});return _ap(_aH,function(_aP){return E(_aO);});};default:return function(_aQ){return _ab(_aI,_aQ);};}})(_aB,_aC,_aD);if(_aE!=null){return _aE;}}},[_az,_aA,0,_ay]);}];},_aR=[6],_aS=unCStr("valDig: Bad base"),_aT=new T(function(){return err(_aS);}),_aU=function(_aV,_aW){var _aX=function(_aY,_aZ){var _b0=E(_aY);if(!_b0[0]){var _b1=new T(function(){return A(_aZ,[_n]);});return function(_b2){return A(_b2,[_b1]);};}else{var _b3=E(_b0[1])[1],_b4=function(_b5){var _b6=new T(function(){return _aX(_b0[2],function(_b7){return A(_aZ,[[1,_b5,_b7]]);});});return function(_b8){var _b9=new T(function(){return A(_b6,[_b8]);});return [0,function(_ba){return E(_b9);}];};};switch(E(E(_aV)[1])){case 8:if(48>_b3){var _bb=new T(function(){return A(_aZ,[_n]);});return function(_bc){return A(_bc,[_bb]);};}else{if(_b3>55){var _bd=new T(function(){return A(_aZ,[_n]);});return function(_be){return A(_be,[_bd]);};}else{return _b4([0,_b3-48|0]);}}break;case 10:if(48>_b3){var _bf=new T(function(){return A(_aZ,[_n]);});return function(_bg){return A(_bg,[_bf]);};}else{if(_b3>57){var _bh=new T(function(){return A(_aZ,[_n]);});return function(_bi){return A(_bi,[_bh]);};}else{return _b4([0,_b3-48|0]);}}break;case 16:var _bj=new T(function(){return 97>_b3?65>_b3?[0]:_b3>70?[0]:[1,[0,(_b3-65|0)+10|0]]:_b3>102?65>_b3?[0]:_b3>70?[0]:[1,[0,(_b3-65|0)+10|0]]:[1,[0,(_b3-97|0)+10|0]];});if(48>_b3){var _bk=E(_bj);if(!_bk[0]){var _bl=new T(function(){return A(_aZ,[_n]);});return function(_bm){return A(_bm,[_bl]);};}else{return _b4(_bk[1]);}}else{if(_b3>57){var _bn=E(_bj);if(!_bn[0]){var _bo=new T(function(){return A(_aZ,[_n]);});return function(_bp){return A(_bp,[_bo]);};}else{return _b4(_bn[1]);}}else{return _b4([0,_b3-48|0]);}}break;default:return E(_aT);}}};return [1,function(_bq){return A(_aX,[_bq,_2j,function(_br){var _bs=E(_br);return _bs[0]==0?[2]:A(_aW,[_bs]);}]);}];},_bt=[0,10],_bu=[0,1],_bv=[0,2147483647],_bw=function(_bx,_by){while(1){var _bz=E(_bx);if(!_bz[0]){var _bA=_bz[1],_bB=E(_by);if(!_bB[0]){var _bC=_bB[1],_bD=addC(_bA,_bC);if(!E(_bD[2])){return [0,_bD[1]];}else{_bx=[1,I_fromInt(_bA)];_by=[1,I_fromInt(_bC)];continue;}}else{_bx=[1,I_fromInt(_bA)];_by=_bB;continue;}}else{var _bE=E(_by);if(!_bE[0]){_bx=_bz;_by=[1,I_fromInt(_bE[1])];continue;}else{return [1,I_add(_bz[1],_bE[1])];}}}},_bF=new T(function(){return _bw(_bv,_bu);}),_bG=function(_bH){var _bI=E(_bH);if(!_bI[0]){var _bJ=E(_bI[1]);return _bJ==(-2147483648)?E(_bF):[0, -_bJ];}else{return [1,I_negate(_bI[1])];}},_bK=[0,10],_bL=[0,0],_bM=function(_bN,_bO){while(1){var _bP=E(_bN);if(!_bP[0]){var _bQ=_bP[1],_bR=E(_bO);if(!_bR[0]){var _bS=_bR[1];if(!(imul(_bQ,_bS)|0)){return [0,imul(_bQ,_bS)|0];}else{_bN=[1,I_fromInt(_bQ)];_bO=[1,I_fromInt(_bS)];continue;}}else{_bN=[1,I_fromInt(_bQ)];_bO=_bR;continue;}}else{var _bT=E(_bO);if(!_bT[0]){_bN=_bP;_bO=[1,I_fromInt(_bT[1])];continue;}else{return [1,I_mul(_bP[1],_bT[1])];}}}},_bU=function(_bV,_bW,_bX){while(1){var _bY=E(_bX);if(!_bY[0]){return E(_bW);}else{var _bZ=_bw(_bM(_bW,_bV),_bY[1]);_bX=_bY[2];_bW=_bZ;continue;}}},_c0=function(_c1){var _c2=new T(function(){return _9m(_9m([0,function(_c3){return E(E(_c3)[1])==45?_aU(_bt,function(_c4){return A(_c1,[[1,new T(function(){return _bG(_bU(_bK,_bL,_c4));})]]);}):[2];}],[0,function(_c5){return E(E(_c5)[1])==43?_aU(_bt,function(_c6){return A(_c1,[[1,new T(function(){return _bU(_bK,_bL,_c6);})]]);}):[2];}]),new T(function(){return _aU(_bt,function(_c7){return A(_c1,[[1,new T(function(){return _bU(_bK,_bL,_c7);})]]);});}));});return _9m([0,function(_c8){return E(E(_c8)[1])==101?E(_c2):[2];}],[0,function(_c9){return E(E(_c9)[1])==69?E(_c2):[2];}]);},_ca=function(_cb){return A(_cb,[_m]);},_cc=function(_cd){return A(_cd,[_m]);},_ce=function(_cf){var _cg=new T(function(){return _aU(_bt,function(_ch){return A(_cf,[[1,_ch]]);});});return [0,function(_ci){return E(E(_ci)[1])==46?E(_cg):[2];}];},_cj=function(_ck){return _aU(_bt,function(_cl){return _av(_ce,_ca,function(_cm){return _av(_c0,_cc,function(_cn){return A(_ck,[[5,[1,_cl,_cm,_cn]]]);});});});},_co=function(_cp,_cq,_cr){while(1){var _cs=E(_cr);if(!_cs[0]){return false;}else{if(!A(_9U,[_cp,_cq,_cs[1]])){_cr=_cs[2];continue;}else{return true;}}}},_ct=unCStr("!@#$%&*+./<=>?\\^|:-~"),_cu=function(_cv){return _co(_9T,_cv,_ct);},_cw=[0,8],_cx=[0,16],_cy=function(_cz){var _cA=new T(function(){return _aU(_cx,function(_cB){return A(_cz,[[5,[0,_cx,_cB]]]);});}),_cC=new T(function(){return _aU(_cw,function(_cD){return A(_cz,[[5,[0,_cw,_cD]]]);});}),_cE=new T(function(){return _aU(_cx,function(_cF){return A(_cz,[[5,[0,_cx,_cF]]]);});}),_cG=new T(function(){return _aU(_cw,function(_cH){return A(_cz,[[5,[0,_cw,_cH]]]);});});return [0,function(_cI){return E(E(_cI)[1])==48?E([0,function(_cJ){switch(E(E(_cJ)[1])){case 79:return E(_cG);case 88:return E(_cE);case 111:return E(_cC);case 120:return E(_cA);default:return [2];}}]):[2];}];},_cK=false,_cL=true,_cM=function(_cN){var _cO=new T(function(){return A(_cN,[_cx]);}),_cP=new T(function(){return A(_cN,[_cw]);}),_cQ=new T(function(){return A(_cN,[_cx]);}),_cR=new T(function(){return A(_cN,[_cw]);});return [0,function(_cS){switch(E(E(_cS)[1])){case 79:return E(_cR);case 88:return E(_cQ);case 111:return E(_cP);case 120:return E(_cO);default:return [2];}}];},_cT=function(_cU){return A(_cU,[_bt]);},_cV=function(_cW){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _h(9,_cW,_n);})));},_cX=function(_cY){var _cZ=E(_cY);return _cZ[0]==0?E(_cZ[1]):I_toInt(_cZ[1]);},_d0=function(_d1,_d2){var _d3=E(_d1);if(!_d3[0]){var _d4=_d3[1],_d5=E(_d2);return _d5[0]==0?_d4<=_d5[1]:I_compareInt(_d5[1],_d4)>=0;}else{var _d6=_d3[1],_d7=E(_d2);return _d7[0]==0?I_compareInt(_d6,_d7[1])<=0:I_compare(_d6,_d7[1])<=0;}},_d8=function(_d9){return [2];},_da=function(_db){var _dc=E(_db);if(!_dc[0]){return E(_d8);}else{var _dd=_dc[1],_de=E(_dc[2]);if(!_de[0]){return E(_dd);}else{var _df=new T(function(){return _da(_de);});return function(_dg){return _9m(A(_dd,[_dg]),new T(function(){return A(_df,[_dg]);}));};}}},_dh=unCStr("NUL"),_di=function(_dj){return [2];},_dk=function(_dl){return _di(_dl);},_dm=function(_dn,_do){var _dp=function(_dq,_dr){var _ds=E(_dq);if(!_ds[0]){return function(_dt){return A(_dt,[_dn]);};}else{var _du=E(_dr);if(!_du[0]){return E(_di);}else{if(E(_ds[1])[1]!=E(_du[1])[1]){return E(_dk);}else{var _dv=new T(function(){return _dp(_ds[2],_du[2]);});return function(_dw){var _dx=new T(function(){return A(_dv,[_dw]);});return [0,function(_dy){return E(_dx);}];};}}}};return [1,function(_dz){return A(_dp,[_dn,_dz,_do]);}];},_dA=[0,0],_dB=function(_dC){var _dD=new T(function(){return A(_dC,[_dA]);});return _dm(_dh,function(_dE){return E(_dD);});},_dF=unCStr("STX"),_dG=[0,2],_dH=function(_dI){var _dJ=new T(function(){return A(_dI,[_dG]);});return _dm(_dF,function(_dK){return E(_dJ);});},_dL=unCStr("ETX"),_dM=[0,3],_dN=function(_dO){var _dP=new T(function(){return A(_dO,[_dM]);});return _dm(_dL,function(_dQ){return E(_dP);});},_dR=unCStr("EOT"),_dS=[0,4],_dT=function(_dU){var _dV=new T(function(){return A(_dU,[_dS]);});return _dm(_dR,function(_dW){return E(_dV);});},_dX=unCStr("ENQ"),_dY=[0,5],_dZ=function(_e0){var _e1=new T(function(){return A(_e0,[_dY]);});return _dm(_dX,function(_e2){return E(_e1);});},_e3=unCStr("ACK"),_e4=[0,6],_e5=function(_e6){var _e7=new T(function(){return A(_e6,[_e4]);});return _dm(_e3,function(_e8){return E(_e7);});},_e9=unCStr("BEL"),_ea=[0,7],_eb=function(_ec){var _ed=new T(function(){return A(_ec,[_ea]);});return _dm(_e9,function(_ee){return E(_ed);});},_ef=unCStr("BS"),_eg=[0,8],_eh=function(_ei){var _ej=new T(function(){return A(_ei,[_eg]);});return _dm(_ef,function(_ek){return E(_ej);});},_el=unCStr("HT"),_em=[0,9],_en=function(_eo){var _ep=new T(function(){return A(_eo,[_em]);});return _dm(_el,function(_eq){return E(_ep);});},_er=unCStr("LF"),_es=[0,10],_et=function(_eu){var _ev=new T(function(){return A(_eu,[_es]);});return _dm(_er,function(_ew){return E(_ev);});},_ex=unCStr("VT"),_ey=[0,11],_ez=function(_eA){var _eB=new T(function(){return A(_eA,[_ey]);});return _dm(_ex,function(_eC){return E(_eB);});},_eD=unCStr("FF"),_eE=[0,12],_eF=function(_eG){var _eH=new T(function(){return A(_eG,[_eE]);});return _dm(_eD,function(_eI){return E(_eH);});},_eJ=unCStr("CR"),_eK=[0,13],_eL=function(_eM){var _eN=new T(function(){return A(_eM,[_eK]);});return _dm(_eJ,function(_eO){return E(_eN);});},_eP=unCStr("SI"),_eQ=[0,15],_eR=function(_eS){var _eT=new T(function(){return A(_eS,[_eQ]);});return _dm(_eP,function(_eU){return E(_eT);});},_eV=unCStr("DLE"),_eW=[0,16],_eX=function(_eY){var _eZ=new T(function(){return A(_eY,[_eW]);});return _dm(_eV,function(_f0){return E(_eZ);});},_f1=unCStr("DC1"),_f2=[0,17],_f3=function(_f4){var _f5=new T(function(){return A(_f4,[_f2]);});return _dm(_f1,function(_f6){return E(_f5);});},_f7=unCStr("DC2"),_f8=[0,18],_f9=function(_fa){var _fb=new T(function(){return A(_fa,[_f8]);});return _dm(_f7,function(_fc){return E(_fb);});},_fd=unCStr("DC3"),_fe=[0,19],_ff=function(_fg){var _fh=new T(function(){return A(_fg,[_fe]);});return _dm(_fd,function(_fi){return E(_fh);});},_fj=unCStr("DC4"),_fk=[0,20],_fl=function(_fm){var _fn=new T(function(){return A(_fm,[_fk]);});return _dm(_fj,function(_fo){return E(_fn);});},_fp=unCStr("NAK"),_fq=[0,21],_fr=function(_fs){var _ft=new T(function(){return A(_fs,[_fq]);});return _dm(_fp,function(_fu){return E(_ft);});},_fv=unCStr("SYN"),_fw=[0,22],_fx=function(_fy){var _fz=new T(function(){return A(_fy,[_fw]);});return _dm(_fv,function(_fA){return E(_fz);});},_fB=unCStr("ETB"),_fC=[0,23],_fD=function(_fE){var _fF=new T(function(){return A(_fE,[_fC]);});return _dm(_fB,function(_fG){return E(_fF);});},_fH=unCStr("CAN"),_fI=[0,24],_fJ=function(_fK){var _fL=new T(function(){return A(_fK,[_fI]);});return _dm(_fH,function(_fM){return E(_fL);});},_fN=unCStr("EM"),_fO=[0,25],_fP=function(_fQ){var _fR=new T(function(){return A(_fQ,[_fO]);});return _dm(_fN,function(_fS){return E(_fR);});},_fT=unCStr("SUB"),_fU=[0,26],_fV=function(_fW){var _fX=new T(function(){return A(_fW,[_fU]);});return _dm(_fT,function(_fY){return E(_fX);});},_fZ=unCStr("ESC"),_g0=[0,27],_g1=function(_g2){var _g3=new T(function(){return A(_g2,[_g0]);});return _dm(_fZ,function(_g4){return E(_g3);});},_g5=unCStr("FS"),_g6=[0,28],_g7=function(_g8){var _g9=new T(function(){return A(_g8,[_g6]);});return _dm(_g5,function(_ga){return E(_g9);});},_gb=unCStr("GS"),_gc=[0,29],_gd=function(_ge){var _gf=new T(function(){return A(_ge,[_gc]);});return _dm(_gb,function(_gg){return E(_gf);});},_gh=unCStr("RS"),_gi=[0,30],_gj=function(_gk){var _gl=new T(function(){return A(_gk,[_gi]);});return _dm(_gh,function(_gm){return E(_gl);});},_gn=unCStr("US"),_go=[0,31],_gp=function(_gq){var _gr=new T(function(){return A(_gq,[_go]);});return _dm(_gn,function(_gs){return E(_gr);});},_gt=unCStr("SP"),_gu=[0,32],_gv=function(_gw){var _gx=new T(function(){return A(_gw,[_gu]);});return _dm(_gt,function(_gy){return E(_gx);});},_gz=unCStr("DEL"),_gA=[0,127],_gB=function(_gC){var _gD=new T(function(){return A(_gC,[_gA]);});return _dm(_gz,function(_gE){return E(_gD);});},_gF=[1,_gB,_n],_gG=[1,_gv,_gF],_gH=[1,_gp,_gG],_gI=[1,_gj,_gH],_gJ=[1,_gd,_gI],_gK=[1,_g7,_gJ],_gL=[1,_g1,_gK],_gM=[1,_fV,_gL],_gN=[1,_fP,_gM],_gO=[1,_fJ,_gN],_gP=[1,_fD,_gO],_gQ=[1,_fx,_gP],_gR=[1,_fr,_gQ],_gS=[1,_fl,_gR],_gT=[1,_ff,_gS],_gU=[1,_f9,_gT],_gV=[1,_f3,_gU],_gW=[1,_eX,_gV],_gX=[1,_eR,_gW],_gY=[1,_eL,_gX],_gZ=[1,_eF,_gY],_h0=[1,_ez,_gZ],_h1=[1,_et,_h0],_h2=[1,_en,_h1],_h3=[1,_eh,_h2],_h4=[1,_eb,_h3],_h5=[1,_e5,_h4],_h6=[1,_dZ,_h5],_h7=[1,_dT,_h6],_h8=[1,_dN,_h7],_h9=[1,_dH,_h8],_ha=[1,_dB,_h9],_hb=unCStr("SOH"),_hc=[0,1],_hd=function(_he){var _hf=new T(function(){return A(_he,[_hc]);});return _dm(_hb,function(_hg){return E(_hf);});},_hh=unCStr("SO"),_hi=[0,14],_hj=function(_hk){var _hl=new T(function(){return A(_hk,[_hi]);});return _dm(_hh,function(_hm){return E(_hl);});},_hn=function(_ho){return _av(_hd,_hj,_ho);},_hp=[1,_hn,_ha],_hq=new T(function(){return _da(_hp);}),_hr=[0,1114111],_hs=[0,34],_ht=[0,_hs,_cL],_hu=[0,39],_hv=[0,_hu,_cL],_hw=[0,92],_hx=[0,_hw,_cL],_hy=[0,_ea,_cL],_hz=[0,_eg,_cL],_hA=[0,_eE,_cL],_hB=[0,_es,_cL],_hC=[0,_eK,_cL],_hD=[0,_em,_cL],_hE=[0,_ey,_cL],_hF=[0,_dA,_cL],_hG=[0,_hc,_cL],_hH=[0,_dG,_cL],_hI=[0,_dM,_cL],_hJ=[0,_dS,_cL],_hK=[0,_dY,_cL],_hL=[0,_e4,_cL],_hM=[0,_ea,_cL],_hN=[0,_eg,_cL],_hO=[0,_em,_cL],_hP=[0,_es,_cL],_hQ=[0,_ey,_cL],_hR=[0,_eE,_cL],_hS=[0,_eK,_cL],_hT=[0,_hi,_cL],_hU=[0,_eQ,_cL],_hV=[0,_eW,_cL],_hW=[0,_f2,_cL],_hX=[0,_f8,_cL],_hY=[0,_fe,_cL],_hZ=[0,_fk,_cL],_i0=[0,_fq,_cL],_i1=[0,_fw,_cL],_i2=[0,_fC,_cL],_i3=[0,_fI,_cL],_i4=[0,_fO,_cL],_i5=[0,_fU,_cL],_i6=[0,_g0,_cL],_i7=[0,_g6,_cL],_i8=[0,_gc,_cL],_i9=[0,_gi,_cL],_ia=[0,_go,_cL],_ib=function(_ic){return [0,_ic];},_id=function(_ie){var _if=new T(function(){return A(_ie,[_hE]);}),_ig=new T(function(){return A(_ie,[_hD]);}),_ih=new T(function(){return A(_ie,[_hC]);}),_ii=new T(function(){return A(_ie,[_hB]);}),_ij=new T(function(){return A(_ie,[_hA]);}),_ik=new T(function(){return A(_ie,[_hz]);}),_il=new T(function(){return A(_ie,[_hy]);}),_im=new T(function(){return A(_ie,[_hx]);}),_in=new T(function(){return A(_ie,[_hv]);}),_io=new T(function(){return A(_ie,[_ht]);});return _9m([0,function(_ip){switch(E(E(_ip)[1])){case 34:return E(_io);case 39:return E(_in);case 92:return E(_im);case 97:return E(_il);case 98:return E(_ik);case 102:return E(_ij);case 110:return E(_ii);case 114:return E(_ih);case 116:return E(_ig);case 118:return E(_if);default:return [2];}}],new T(function(){return _9m(_av(_cM,_cT,function(_iq){var _ir=new T(function(){return _ib(E(_iq)[1]);});return _aU(_iq,function(_is){var _it=_bU(_ir,_bL,_is);return !_d0(_it,_hr)?[2]:A(_ie,[[0,new T(function(){var _iu=_cX(_it);return _iu>>>0>1114111?_cV(_iu):[0,_iu];}),_cL]]);});}),new T(function(){var _iv=new T(function(){return A(_ie,[_ia]);}),_iw=new T(function(){return A(_ie,[_i9]);}),_ix=new T(function(){return A(_ie,[_i8]);}),_iy=new T(function(){return A(_ie,[_i7]);}),_iz=new T(function(){return A(_ie,[_i6]);}),_iA=new T(function(){return A(_ie,[_i5]);}),_iB=new T(function(){return A(_ie,[_i4]);}),_iC=new T(function(){return A(_ie,[_i3]);}),_iD=new T(function(){return A(_ie,[_i2]);}),_iE=new T(function(){return A(_ie,[_i1]);}),_iF=new T(function(){return A(_ie,[_i0]);}),_iG=new T(function(){return A(_ie,[_hZ]);}),_iH=new T(function(){return A(_ie,[_hY]);}),_iI=new T(function(){return A(_ie,[_hX]);}),_iJ=new T(function(){return A(_ie,[_hW]);}),_iK=new T(function(){return A(_ie,[_hV]);}),_iL=new T(function(){return A(_ie,[_hU]);}),_iM=new T(function(){return A(_ie,[_hT]);}),_iN=new T(function(){return A(_ie,[_hS]);}),_iO=new T(function(){return A(_ie,[_hR]);}),_iP=new T(function(){return A(_ie,[_hQ]);}),_iQ=new T(function(){return A(_ie,[_hP]);}),_iR=new T(function(){return A(_ie,[_hO]);}),_iS=new T(function(){return A(_ie,[_hN]);}),_iT=new T(function(){return A(_ie,[_hM]);}),_iU=new T(function(){return A(_ie,[_hL]);}),_iV=new T(function(){return A(_ie,[_hK]);}),_iW=new T(function(){return A(_ie,[_hJ]);}),_iX=new T(function(){return A(_ie,[_hI]);}),_iY=new T(function(){return A(_ie,[_hH]);}),_iZ=new T(function(){return A(_ie,[_hG]);}),_j0=new T(function(){return A(_ie,[_hF]);});return _9m([0,function(_j1){return E(E(_j1)[1])==94?E([0,function(_j2){switch(E(E(_j2)[1])){case 64:return E(_j0);case 65:return E(_iZ);case 66:return E(_iY);case 67:return E(_iX);case 68:return E(_iW);case 69:return E(_iV);case 70:return E(_iU);case 71:return E(_iT);case 72:return E(_iS);case 73:return E(_iR);case 74:return E(_iQ);case 75:return E(_iP);case 76:return E(_iO);case 77:return E(_iN);case 78:return E(_iM);case 79:return E(_iL);case 80:return E(_iK);case 81:return E(_iJ);case 82:return E(_iI);case 83:return E(_iH);case 84:return E(_iG);case 85:return E(_iF);case 86:return E(_iE);case 87:return E(_iD);case 88:return E(_iC);case 89:return E(_iB);case 90:return E(_iA);case 91:return E(_iz);case 92:return E(_iy);case 93:return E(_ix);case 94:return E(_iw);case 95:return E(_iv);default:return [2];}}]):[2];}],new T(function(){return A(_hq,[function(_j3){return A(_ie,[[0,_j3,_cL]]);}]);}));}));}));},_j4=function(_j5){return A(_j5,[_0]);},_j6=function(_j7){var _j8=E(_j7);if(!_j8[0]){return E(_j4);}else{var _j9=_j8[2],_ja=E(E(_j8[1])[1]);switch(_ja){case 9:var _jb=new T(function(){return _j6(_j9);});return function(_jc){var _jd=new T(function(){return A(_jb,[_jc]);});return [0,function(_je){return E(_jd);}];};case 10:var _jf=new T(function(){return _j6(_j9);});return function(_jg){var _jh=new T(function(){return A(_jf,[_jg]);});return [0,function(_ji){return E(_jh);}];};case 11:var _jj=new T(function(){return _j6(_j9);});return function(_jk){var _jl=new T(function(){return A(_jj,[_jk]);});return [0,function(_jm){return E(_jl);}];};case 12:var _jn=new T(function(){return _j6(_j9);});return function(_jo){var _jp=new T(function(){return A(_jn,[_jo]);});return [0,function(_jq){return E(_jp);}];};case 13:var _jr=new T(function(){return _j6(_j9);});return function(_js){var _jt=new T(function(){return A(_jr,[_js]);});return [0,function(_ju){return E(_jt);}];};case 32:var _jv=new T(function(){return _j6(_j9);});return function(_jw){var _jx=new T(function(){return A(_jv,[_jw]);});return [0,function(_jy){return E(_jx);}];};case 160:var _jz=new T(function(){return _j6(_j9);});return function(_jA){var _jB=new T(function(){return A(_jz,[_jA]);});return [0,function(_jC){return E(_jB);}];};default:var _jD=u_iswspace(_ja);if(!E(_jD)){return E(_j4);}else{var _jE=new T(function(){return _j6(_j9);});return function(_jF){var _jG=new T(function(){return A(_jE,[_jF]);});return [0,function(_jH){return E(_jG);}];};}}}},_jI=function(_jJ){var _jK=new T(function(){return _id(_jJ);}),_jL=new T(function(){return _jI(_jJ);}),_jM=[1,function(_jN){return A(_j6,[_jN,function(_jO){return E([0,function(_jP){return E(E(_jP)[1])==92?E(_jL):[2];}]);}]);}];return _9m([0,function(_jQ){return E(E(_jQ)[1])==92?E([0,function(_jR){var _jS=E(E(_jR)[1]);switch(_jS){case 9:return E(_jM);case 10:return E(_jM);case 11:return E(_jM);case 12:return E(_jM);case 13:return E(_jM);case 32:return E(_jM);case 38:return E(_jL);case 160:return E(_jM);default:var _jT=u_iswspace(_jS);return E(_jT)==0?[2]:E(_jM);}}]):[2];}],[0,function(_jU){var _jV=E(_jU);return E(_jV[1])==92?E(_jK):A(_jJ,[[0,_jV,_cK]]);}]);},_jW=function(_jX,_jY){var _jZ=new T(function(){return A(_jY,[[1,new T(function(){return A(_jX,[_n]);})]]);});return _jI(function(_k0){var _k1=E(_k0),_k2=E(_k1[1]);return E(_k2[1])==34?!E(_k1[2])?E(_jZ):_jW(function(_k3){return A(_jX,[[1,_k2,_k3]]);},_jY):_jW(function(_k4){return A(_jX,[[1,_k2,_k4]]);},_jY);});},_k5=unCStr("_\'"),_k6=function(_k7){var _k8=u_iswalnum(_k7);return E(_k8)==0?_co(_9T,[0,_k7],_k5):true;},_k9=function(_ka){return _k6(E(_ka)[1]);},_kb=unCStr(",;()[]{}`"),_kc=function(_kd){return A(_kd,[_n]);},_ke=function(_kf,_kg){var _kh=function(_ki){var _kj=E(_ki);if(!_kj[0]){return E(_kc);}else{var _kk=_kj[1];if(!A(_kf,[_kk])){return E(_kc);}else{var _kl=new T(function(){return _kh(_kj[2]);});return function(_km){var _kn=new T(function(){return A(_kl,[function(_ko){return A(_km,[[1,_kk,_ko]]);}]);});return [0,function(_kp){return E(_kn);}];};}}};return [1,function(_kq){return A(_kh,[_kq,_kg]);}];},_kr=unCStr(".."),_ks=unCStr("::"),_kt=unCStr("->"),_ku=[0,64],_kv=[1,_ku,_n],_kw=[0,126],_kx=[1,_kw,_n],_ky=unCStr("=>"),_kz=[1,_ky,_n],_kA=[1,_kx,_kz],_kB=[1,_kv,_kA],_kC=[1,_kt,_kB],_kD=unCStr("<-"),_kE=[1,_kD,_kC],_kF=[0,124],_kG=[1,_kF,_n],_kH=[1,_kG,_kE],_kI=[1,_hw,_n],_kJ=[1,_kI,_kH],_kK=[0,61],_kL=[1,_kK,_n],_kM=[1,_kL,_kJ],_kN=[1,_ks,_kM],_kO=[1,_kr,_kN],_kP=function(_kQ){var _kR=new T(function(){return A(_kQ,[_aR]);});return _9m([1,function(_kS){return E(_kS)[0]==0?E(_kR):[2];}],new T(function(){var _kT=new T(function(){return _id(function(_kU){var _kV=E(_kU);return (function(_kW,_kX){var _kY=new T(function(){return A(_kQ,[[0,_kW]]);});return !E(_kX)?E(E(_kW)[1])==39?[2]:[0,function(_kZ){return E(E(_kZ)[1])==39?E(_kY):[2];}]:[0,function(_l0){return E(E(_l0)[1])==39?E(_kY):[2];}];})(_kV[1],_kV[2]);});});return _9m([0,function(_l1){return E(E(_l1)[1])==39?E([0,function(_l2){var _l3=E(_l2);switch(E(_l3[1])){case 39:return [2];case 92:return E(_kT);default:var _l4=new T(function(){return A(_kQ,[[0,_l3]]);});return [0,function(_l5){return E(E(_l5)[1])==39?E(_l4):[2];}];}}]):[2];}],new T(function(){var _l6=new T(function(){return _jW(_2j,_kQ);});return _9m([0,function(_l7){return E(E(_l7)[1])==34?E(_l6):[2];}],new T(function(){return _9m([0,function(_l8){return !_co(_9T,_l8,_kb)?[2]:A(_kQ,[[2,[1,_l8,_n]]]);}],new T(function(){return _9m([0,function(_l9){return !_co(_9T,_l9,_ct)?[2]:_ke(_cu,function(_la){var _lb=[1,_l9,_la];return !_co(_aa,_lb,_kO)?A(_kQ,[[4,_lb]]):A(_kQ,[[2,_lb]]);});}],new T(function(){return _9m([0,function(_lc){var _ld=E(_lc),_le=_ld[1],_lf=u_iswalpha(_le);return E(_lf)==0?E(_le)==95?_ke(_k9,function(_lg){return A(_kQ,[[3,[1,_ld,_lg]]]);}):[2]:_ke(_k9,function(_lh){return A(_kQ,[[3,[1,_ld,_lh]]]);});}],new T(function(){return _av(_cy,_cj,_kQ);}));}));}));}));}));}));},_li=function(_lj){var _lk=new T(function(){return _kP(_lj);});return [1,function(_ll){return A(_j6,[_ll,function(_lm){return E(_lk);}]);}];},_ln=[0,0],_lo=function(_lp,_lq){var _lr=new T(function(){return A(_lp,[_ln,function(_ls){var _lt=new T(function(){return A(_lq,[_ls]);});return _li(function(_lu){var _lv=E(_lu);if(_lv[0]==2){var _lw=E(_lv[1]);return _lw[0]==0?[2]:E(E(_lw[1])[1])==41?E(_lw[2])[0]==0?E(_lt):[2]:[2];}else{return [2];}});}]);});return _li(function(_lx){var _ly=E(_lx);if(_ly[0]==2){var _lz=E(_ly[1]);return _lz[0]==0?[2]:E(E(_lz[1])[1])==40?E(_lz[2])[0]==0?E(_lr):[2]:[2];}else{return [2];}});},_lA=function(_lB,_lC,_lD){var _lE=function(_lF,_lG){var _lH=new T(function(){return _kP(function(_lI){return A(_lB,[_lI,_lF,function(_lJ){return A(_lG,[new T(function(){return [0, -E(_lJ)[1]];})]);}]);});});return _9m(_li(function(_lK){var _lL=E(_lK);if(_lL[0]==4){var _lM=E(_lL[1]);return _lM[0]==0?A(_lB,[_lL,_lF,_lG]):E(E(_lM[1])[1])==45?E(_lM[2])[0]==0?E([1,function(_lN){return A(_j6,[_lN,function(_lO){return E(_lH);}]);}]):A(_lB,[_lL,_lF,_lG]):A(_lB,[_lL,_lF,_lG]);}else{return A(_lB,[_lL,_lF,_lG]);}}),new T(function(){return _lo(_lE,_lG);}));};return _lE(_lC,_lD);},_lP=function(_lQ,_lR){return [2];},_lS=function(_lT,_lU){return _lP(_lT,_lU);},_lV=function(_lW){var _lX=E(_lW);return _lX[0]==0?[1,new T(function(){return _bU(new T(function(){return _ib(E(_lX[1])[1]);}),_bL,_lX[2]);})]:E(_lX[2])[0]==0?E(_lX[3])[0]==0?[1,new T(function(){return _bU(_bK,_bL,_lX[1]);})]:[0]:[0];},_lY=function(_lZ){var _m0=E(_lZ);if(_m0[0]==5){var _m1=_lV(_m0[1]);if(!_m1[0]){return E(_lP);}else{var _m2=new T(function(){return [0,_cX(_m1[1])];});return function(_m3,_m4){return A(_m4,[_m2]);};}}else{return E(_lS);}},_m5=function(_lT,_lU){return _lA(_lY,_lT,_lU);},_m6=function(_m7,_m8){var _m9=function(_ma,_mb){var _mc=new T(function(){return A(_mb,[_n]);}),_md=new T(function(){return A(_m7,[_ln,function(_me){return _m9(_cL,function(_mf){return A(_mb,[[1,_me,_mf]]);});}]);});return _li(function(_mg){var _mh=E(_mg);if(_mh[0]==2){var _mi=E(_mh[1]);if(!_mi[0]){return [2];}else{var _mj=_mi[2];switch(E(E(_mi[1])[1])){case 44:return E(_mj)[0]==0?!E(_ma)?[2]:E(_md):[2];case 93:return E(_mj)[0]==0?E(_mc):[2];default:return [2];}}}else{return [2];}});},_mk=function(_ml){var _mm=new T(function(){return _9m(_m9(_cK,_ml),new T(function(){return A(_m7,[_ln,function(_mn){return _m9(_cL,function(_mo){return A(_ml,[[1,_mn,_mo]]);});}]);}));});return _9m(_li(function(_mp){var _mq=E(_mp);if(_mq[0]==2){var _mr=E(_mq[1]);return _mr[0]==0?[2]:E(E(_mr[1])[1])==91?E(_mr[2])[0]==0?E(_mm):[2]:[2];}else{return [2];}}),new T(function(){return _lo(function(_ms,_mt){return _mk(_mt);},_ml);}));};return _mk(_m8);},_mu=function(_mv,_mw){return _m6(_m5,_mw);},_mx=new T(function(){return _m6(_m5,_an);}),_my=function(_lU){return _9c(_mx,_lU);},_mz=function(_mA){var _mB=new T(function(){return _lA(_lY,_mA,_an);});return function(_aQ){return _9c(_mB,_aQ);};},_mC=[0,_mz,_my,_m5,_mu],_mD=function(_mE){return _h(0,E(_mE)[1],_n);},_mF=function(_mG,_mH){return _h(0,E(_mG)[1],_mH);},_mI=function(_mJ,_mK){return _1P(_mF,_mJ,_mK);},_mL=function(_mM,_mN,_mO){return _h(E(_mM)[1],E(_mN)[1],_mO);},_mP=[0,_mL,_mD,_mI],_mQ=unCStr("GHC.Types"),_mR=unCStr("Int"),_mS=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_5b,_mQ,_mR],_mT=[0,I_fromBits([1521842780,3792221899]),I_fromBits([1346191152,3861967380]),_mS,_n],_mU=function(_mV){return E(_mT);},_mW=function(_mX){return E(E(_mX)[1]);},_mY=function(_mZ,_n0){return A(_mZ,[function(_){return jsFind(toJSStr(E(_n0)));}]);},_n1=[0,0],_n2=function(_n3){return E(E(_n3)[4]);},_n4=unCStr("[]"),_n5=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_5b,_mQ,_n4],_n6=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520]),_n5,_n],_n7=function(_n8){return E(_n6);},_n9=unCStr("Char"),_na=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_5b,_mQ,_n9],_nb=[0,I_fromBits([3763641161,3907222913]),I_fromBits([1343745632,586881778]),_na,_n],_nc=function(_nd){return E(_nb);},_ne=new T(function(){return _65(_n7,_nc);}),_nf=new T(function(){return A(_ne,[_64]);}),_ng=new T(function(){return E(_64);}),_nh=function(_ni){return E(E(_ni)[7]);},_nj=function(_nk){return E(E(_nk)[1]);},_nl=[0,0],_nm=[0,32],_nn=[0,10],_no=function(_np){var _nq=E(_np);if(!_nq[0]){return E(_2j);}else{var _nr=_nq[1],_ns=E(_nq[2]);if(!_ns[0]){return _nt(_nn,_nr);}else{var _nu=new T(function(){return _no(_ns);}),_nv=new T(function(){return _nt(_nn,_nr);});return function(_nw){return A(_nv,[[1,_nm,new T(function(){return A(_nu,[_nw]);})]]);};}}},_nx=unCStr("->"),_ny=[1,_nx,_n],_nz=[1,_mQ,_ny],_nA=[1,_5b,_nz],_nB=[0,32],_nC=function(_nD){var _nE=E(_nD);if(!_nE[0]){return [0];}else{var _nF=_nE[1],_nG=E(_nE[2]);return _nG[0]==0?E(_nF):_7(_nF,[1,_nB,new T(function(){return _nC(_nG);})]);}},_nH=new T(function(){return _nC(_nA);}),_nI=new T(function(){var _nJ=_5z(_nH);return [0,_nJ[1],_nJ[2],_5b,_mQ,_nx];}),_nK=function(_nL,_nM){var _nN=E(_nL);return _nN[0]==0?E(_nM):A(_nN[1],[new T(function(){return _nK(_nN[2],_nM);})]);},_nO=[0,I_fromBits([4033920485,4128112366]),I_fromBits([786266835,2297333520])],_nP=[1,_5d,_n],_nQ=function(_nR){var _nS=E(_nR);if(!_nS[0]){return [0];}else{var _nT=E(_nS[1]);return [1,[0,_nT[1],_nT[2]],new T(function(){return _nQ(_nS[2]);})];}},_nU=new T(function(){var _nV=_7(_n,_nP);if(!_nV[0]){return E(_n5);}else{var _nW=_5z(new T(function(){return _5n(_5L(_5W,[1,_nO,new T(function(){return _nQ(_nV);})]));}));return E(_n5);}}),_nX=[0,40],_nY=function(_nZ){return _nt(_nn,_nZ);},_o0=[0,8],_o1=unCStr(" -> "),_o2=[0,9],_o3=[0,93],_o4=[0,91],_o5=[0,41],_o6=[0,44],_o7=function(_nZ){return [1,_o6,_nZ];},_o8=function(_o9,_oa){var _ob=E(_oa);return _ob[0]==0?[0]:[1,_o9,[1,_ob[1],new T(function(){return _o8(_o9,_ob[2]);})]];},_nt=function(_oc,_od){var _oe=E(_od),_of=_oe[3],_og=E(_oe[4]);if(!_og[0]){return function(_oh){return _7(E(_of)[5],_oh);};}else{var _oi=_og[1],_oj=new T(function(){var _ok=E(_of)[5],_ol=new T(function(){return _no(_og);}),_om=new T(function(){return E(_oc)[1]<=9?function(_on){return _7(_ok,[1,_nm,new T(function(){return A(_ol,[_on]);})]);}:function(_oo){return [1,_g,new T(function(){return _7(_ok,[1,_nm,new T(function(){return A(_ol,[[1,_f,_oo]]);})]);})];};}),_op=E(_ok);if(!_op[0]){return E(_om);}else{if(E(E(_op[1])[1])==40){var _oq=E(_op[2]);return _oq[0]==0?E(_om):E(E(_oq[1])[1])==44?function(_or){return [1,_nX,new T(function(){return A(new T(function(){var _os=_5L(_nY,_og);if(!_os[0]){return E(_2j);}else{var _ot=new T(function(){return _o8(_o7,_os[2]);});return function(_aQ){return _nK([1,_os[1],_ot],_aQ);};}}),[[1,_o5,_or]]);})];}:E(_om);}else{return E(_om);}}}),_ou=E(_og[2]);if(!_ou[0]){var _ov=E(_of),_ow=E(_nU),_ox=hs_eqWord64(_ov[1],_ow[1]);if(!E(_ox)){return E(_oj);}else{var _oy=hs_eqWord64(_ov[2],_ow[2]);if(!E(_oy)){return E(_oj);}else{var _oz=new T(function(){return _nt(_nl,_oi);});return function(_oA){return [1,_o4,new T(function(){return A(_oz,[[1,_o3,_oA]]);})];};}}}else{if(!E(_ou[2])[0]){var _oB=E(_of),_oC=E(_nI),_oD=hs_eqWord64(_oB[1],_oC[1]);if(!E(_oD)){return E(_oj);}else{var _oE=hs_eqWord64(_oB[2],_oC[2]);if(!E(_oE)){return E(_oj);}else{var _oF=new T(function(){return _nt(_o0,_ou[1]);}),_oG=new T(function(){return _nt(_o2,_oi);});return E(_oc)[1]<=8?function(_oH){return A(_oG,[new T(function(){return _7(_o1,new T(function(){return A(_oF,[_oH]);}));})]);}:function(_oI){return [1,_g,new T(function(){return A(_oG,[new T(function(){return _7(_o1,new T(function(){return A(_oF,[[1,_f,_oI]]);}));})]);})];};}}}else{return E(_oj);}}}},_oJ=function(_oK,_oL,_oM,_oN){var _oO=new T(function(){return _2N(_oK);}),_oP=new T(function(){return _n2(_oN);}),_oQ=new T(function(){return _nh(_oN);}),_oR=new T(function(){return unAppCStr("\" as type ",new T(function(){return A(_nt,[_nl,A(_oL,[_ng]),_n]);}));}),_oS=new T(function(){return A(_nj,[_oM,_n1]);});return function(_oT){if(!E(new T(function(){var _oU=A(_oL,[_ng]),_oV=E(_nf),_oW=hs_eqWord64(_oU[1],_oV[1]);if(!E(_oW)){return false;}else{var _oX=hs_eqWord64(_oU[2],_oV[2]);return E(_oX)==0?false:true;}}))){var _oY=new T(function(){return A(_oO,[[1,_oT,new T(function(){return A(_oQ,[new T(function(){return A(_oP,[new T(function(){return unAppCStr("can\'t read \"",new T(function(){return _7(_oT,_oR);}));})]);})]);})]]);}),_oZ=A(_oS,[_oT]);if(!_oZ[0]){return E(_oY);}else{var _p0=E(_oZ[1]);return E(_p0[2])[0]==0?E(_oZ[2])[0]==0?A(_oO,[[2,_p0[1]]]):E(_oY):E(_oY);}}else{return A(_oO,[[2,_oT]]);}};},_p1=[0],_p2=new T(function(){return [0,"value"];}),_p3=function(_p4,_p5,_p6,_p7,_p8,_p9){var _pa=E(_p4),_pb=_pa[1],_pc=new T(function(){return A(_pa[3],[_p1]);}),_pd=new T(function(){return _oJ(_pa,_p6,_p7,_p8);});return A(_pb,[new T(function(){return _mY(_p5,_p9);}),function(_pe){var _pf=E(_pe);return _pf[0]==0?E(_pc):A(_pb,[new T(function(){return A(_p5,[function(_){var _pg=jsGet(E(_pf[1])[1],E(_p2)[1]);return [1,new T(function(){return fromJSStr(_pg);})];}]);}),function(_ph){var _pi=E(_ph);return _pi[0]==0?E(_pc):A(_pd,[_pi[1]]);}]);}]);},_pj=1,_pk=function(_pl){return E(E(_pl)[10]);},_pm=function(_pn){return E(E(_pn)[2]);},_po=function(_pp){return E(E(_pp)[2]);},_pq=function(_pr,_ps,_pt,_pu,_pv){var _pw=new T(function(){return _mW(_pr);}),_px=new T(function(){return _pm(_pw);}),_py=new T(function(){return _2U(_ps);}),_pz=new T(function(){return _2P(_py);}),_pA=new T(function(){return _38([0,coercionToken],_pz,function(_pB){return _3q(_py,_pB);},function(_pC,_pD){return _3A(_py,_pC,_pD);});}),_pE=new T(function(){return _2N(_py);}),_pF=new T(function(){return _2n(_py);}),_pG=new T(function(){return _2N(_py);}),_pH=new T(function(){return _2n(_py);}),_pI=new T(function(){return _2N(_py);}),_pJ=new T(function(){return _2n(_py);}),_pK=new T(function(){return _2N(_py);}),_pL=new T(function(){return _2n(_py);}),_pM=new T(function(){return _po(_pu);}),_pN=new T(function(){return _pk(_pr);});return function(_pO,_pP,_pQ){return function(_pR){return A(_pL,[new T(function(){var _pS=E(_pO);return _pS[0]==0?A(_pA,[_pR]):A(_pK,[[0,_pS[1],_pR]]);}),function(_pT){var _pU=new T(function(){return E(E(_pT)[1]);}),_pV=new T(function(){return _p3(_pz,function(_pW){return _2Y(_ps,_pW);},_pt,_pv,_pr,_pU);}),_pX=new T(function(){return A(_pN,[_pU,_pP,new T(function(){var _pY=E(_pQ);if(!_pY[0]){return [0];}else{var _pZ=_pY[1],_q0=_N(_pt,_ne,_pZ);return _q0[0]==0?A(_pM,[_pZ]):E(_q0[1]);}}),_cK,_m]);});return A(_pJ,[new T(function(){var _q1=new T(function(){return E(E(_pT)[2]);});return A(_pI,[[0,_q1,_q1]]);}),function(_q2){return A(_pH,[new T(function(){return A(_pG,[[0,_0,new T(function(){var _q3=E(E(_q2)[1]);return [0,_q3[1],_q3[2],_pj,_q3[4]];})]]);}),function(_q4){return A(_pF,[new T(function(){return A(_pV,[new T(function(){return E(E(_q4)[2]);})]);}),function(_q5){var _q6=E(_q5),_q7=_q6[2],_q8=E(_q6[1]);switch(_q8[0]){case 0:return A(_pE,[[0,[0,_pX,_m],_q7]]);case 1:return A(_pE,[[0,[0,new T(function(){return A(_px,[new T(function(){return A(_pN,[_pU,_pP,_q8[1],_cK,_m]);}),_q8[2]]);}),_m],_q7]]);default:var _q9=_q8[1];return A(_pE,[[0,[0,new T(function(){return A(_pN,[_pU,_pP,new T(function(){var _qa=_N(_pt,_ne,_q9);return _qa[0]==0?A(_pM,[_q9]):E(_qa[1]);}),_cK,_m]);}),[1,_q9]],_q7]]);}}]);}]);}]);}]);};};},_qb=new T(function(){return _pq(_8j,_2l,_mU,_mP,_mC);}),_qc=new T(function(){return A(_qb,[_m,_52,_m]);}),_qd=new T(function(){return A(_qb,[_m,_52,_m]);}),_qe=function(_qf,_){var _qg=A(_qd,[_qf,_]),_qh=E(_qg),_qi=_qh[2],_qj=E(_qh[1]),_qk=A(_qc,[_qi,_]),_ql=E(_qk),_qm=E(_ql[1]),_qn=new T(function(){return E(E(_qi)[4]);}),_qo=new T(function(){return E(E(_qf)[4]);});return [0,[0,function(_qp,_){var _qq=_2(_51,_qp,_),_qr=_4N(_qj[1],_4g,_qo,_qp,_),_qs=_4W(_qp,_),_qt=_2(_50,_qp,_),_qu=_4N(_qm[1],_4g,_qn,_qp,_),_qv=_4W(_qp,_);return _qp;},new T(function(){var _qw=E(_qj[2]);if(!_qw[0]){return [0];}else{var _qx=E(_qm[2]);return _qx[0]==0?[0]:[1,new T(function(){return _4h(_qw[1],_qx[1]);})];}})],_ql[2]];},_qy=new T(function(){return [0,"(function(e){return e.parentNode;})"];}),_qz=function(_qA,_qB){var _qC=new T(function(){return _2U(_qB);}),_qD=new T(function(){return _38([0,coercionToken],_2P(_qC),function(_qE){return _3q(_qC,_qE);},function(_qF,_qG){return _3A(_qC,_qF,_qG);});}),_qH=new T(function(){return _2N(_qC);}),_qI=new T(function(){return _2n(_qC);}),_qJ=new T(function(){return _2n(_qC);}),_qK=new T(function(){return _2n(_qC);});return function(_qL,_qM){return A(_qK,[new T(function(){return A(_qD,[_qM]);}),function(_qN){var _qO=new T(function(){return E(E(_qN)[1]);}),_qP=new T(function(){return _2Y(_qB,function(_){return jsFind(toJSStr(E(_qO)));});}),_qQ=new T(function(){return _2Y(_qB,function(_){return jsFind(toJSStr(_7(_qO,_3u)));});});return A(_qJ,[new T(function(){return A(_qL,[new T(function(){return E(E(_qN)[2]);})]);}),function(_qR){var _qS=E(_qR),_qT=E(_qS[1]),_qU=_qT[2],_qV=new T(function(){return A(_qA,[_qT[1]]);});return A(_qI,[new T(function(){return A(_qP,[_qS[2]]);}),function(_qW){var _qX=E(_qW),_qY=_qX[2];return E(_qX[1])[0]==0?A(_qI,[new T(function(){return A(_qQ,[_qY]);}),function(_qZ){var _r0=E(_qZ),_r1=_r0[2],_r2=E(_r0[1]);if(!_r2[0]){return A(_qH,[[0,[0,function(_r3,_){var _r4=_3v(_3t,_r3,_),_r5=A(_3k,[_2j,_r4,_2m,_qO,_]),_r6=A(_qV,[_r4,_]);return _r4;},_qU],_r1]]);}else{var _r7=_r2[1],_r8=new T(function(){return _3k(_2j,_r7,_2m,_qO);});return A(_qH,[[0,[0,function(_r9,_){var _ra=A(_4F,[_qy,_]),_rb=E(_r7)[1],_rc=jsKillChild(_rb,_ra),_rd=A(_r8,[_]),_re=E(_r9),_rf=jsAppendChild(_rb,_re[1]);return _re;},_qU],_r1]]);}}]):A(_qH,[[0,[0,_2g,_qU],_qY]]);}]);}]);}]);};},_rg=function(_rh){return E(_rh);},_ri=new T(function(){return _qz(_rg,_2l);}),_rj=new T(function(){return A(_ri,[_qe]);}),_rk=function(_rl,_){var _rm=A(_rj,[_rl,_]),_rn=E(_rm),_ro=_rn[2],_rp=E(_rn[1]),_rq=_rp[1],_rr=E(_rp[2]);if(!_rr[0]){return [0,[0,_rq,_m],_ro];}else{var _rs=new T(function(){return _h(0,E(_rr[1])[1],_n);}),_rt=A(_4f,[function(_ru,_){return [0,[0,function(_rv,_){var _rw=_2(_q,_rv,_),_rx=_3v(_p,_rv,_),_ry=_2(_rs,_rx,_);return _rv;},_m],_ru];},_ro,_]),_rz=E(_rt),_rA=E(_rz[1]);return [0,[0,function(_rB,_){var _rC=A(_rq,[_rB,_]),_rD=A(_rA[1],[_rB,_]);return _rB;},_rA[2]],_rz[2]];}},_rE=2,_rF=function(_rG,_rH,_){var _rI=A(_rG,[[0,_n,_n1,_rE,function(_){return _rF(_rG,_rH,_);}],_]),_rJ=A(E(E(_rI)[1])[1],[_rH,_]);return _0;},_rK=unCStr(" could be found!"),_rL=function(_rM){return err(unAppCStr("No element with ID ",new T(function(){return _7(_rM,_rK);})));},_rN=function(_){var _rO=E(_1),_rP=jsFind(toJSStr(_rO)),_rQ=E(_rP);if(!_rQ[0]){return _rL(_rO);}else{var _rR=_rF(_rk,_rQ[1],_);return _0;}},_rS=function(_){return _rN(_);};
var hasteMain = function() {A(_rS, [0]);};window.onload = hasteMain;