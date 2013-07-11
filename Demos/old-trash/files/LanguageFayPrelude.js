/** @constructor
*/

var True = true;
var False = false;

/*******************************************************************************
 * Thunks.
 */

// Force a thunk (if it is a thunk) until WHNF.
function _(thunkish,nocache){
    while (thunkish instanceof $) {
        thunkish = thunkish.force(nocache);
    }
    return thunkish;
}

// Apply a function to arguments (see method2 in Fay.hs).
function __(){
    var f = arguments[0];
    for (var i = 1, len = arguments.length; i < len; i++) {
        f = (f instanceof $? _(f) : f)(arguments[i]);
    }
    return f;
}

// Thunk object.
function $(value){
    this.forced = false;
    this.value = value;
}

// Force the thunk.
$.prototype.force = function(nocache) {
  return nocache ?
    this.value() :
    (this.forced ?
     this.value :
     (this.value = this.value(), this.forced = true, this.value));
};

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
    this.value = value;
}

// >>
// encode_fay_to_js(">>=") ? $62$$62$
// This is used directly from Fay, but can be rebound or shadowed.
function $62$$62$(a){
    return function(b){
        return new $(function(){
            _(a,true);
            return b;
        });
    };
}

// >>=
// encode_fay_to_js(">>=") ? $62$$62$$61$
// This is used directly from Fay, but can be rebound or shadowed.
function $62$$62$$61$(m){
    return function(f){
        return new $(function(){
            var monad = _(m,true);
            return f(monad.value);
        });
    };
}

// This is used directly from Fay, but can be rebound or shadowed.
function $_return(a){
    return new Fay$$Monad(a);
}

var Fay$$unit = null;

/*******************************************************************************
 * Serialization.
 * Fay <-> JS. Should be bijective.
 */

// Serialize a Fay object to JS.
function Fay$$fayToJs(type,fayObj){
    var base = type[0];
    var args = type[1];
    var jsObj;
    switch(base){
    case "action": {
        // A nullary monadic action. Should become a nullary JS function.
        // Fay () -> function(){ return ... }
        jsObj = function(){
            return Fay$$fayToJs(args[0],_(fayObj,true).value);
        };
        break;
    }
    case "function": {
        // A proper function.
        jsObj = function(){
            var fayFunc = fayObj;
            var return_type = args[args.length-1];
            var len = args.length;
            // If some arguments.
            if (len > 1) {
                // Apply to all the arguments.
                fayFunc = _(fayFunc,true);
                // TODO: Perhaps we should throw an error when JS
                // passes more arguments than Haskell accepts.
                for (var i = 0, len = len; i < len - 1 && fayFunc instanceof Function; i++) {
                    // Unserialize the JS values to Fay for the Fay callback.
                    fayFunc = _(fayFunc(Fay$$jsToFay(args[i],arguments[i])),true);
                }
                // Finally, serialize the Fay return value back to JS.
                var return_base = return_type[0];
                var return_args = return_type[1];
                // If it's a monadic return value, get the value instead.
                if(return_base == "action") {
                    return Fay$$fayToJs(return_args[0],fayFunc.value);
                }
                // Otherwise just serialize the value direct.
                else {
                    return Fay$$fayToJs(return_type,fayFunc);
                }
            } else {
                throw new Error("Nullary function?");
            }
        };
        break;
    }
    case "string": {
        // Serialize Fay string to JavaScript string.
        var str = "";
        fayObj = _(fayObj);
        while(fayObj instanceof Fay$$Cons) {
            str += fayObj.car;
            fayObj = _(fayObj.cdr);
        }
        jsObj = str;
        break;
    }
    case "list": {
        // Serialize Fay list to JavaScript array.
        var arr = [];
        fayObj = _(fayObj);
        while(fayObj instanceof Fay$$Cons) {
            arr.push(Fay$$fayToJs(args[0],fayObj.car));
            fayObj = _(fayObj.cdr);
        }
        jsObj = arr;
        break;
    }
    case "double": {
        // Serialize double, just force the argument. Doubles are unboxed.
        jsObj = _(fayObj);
        break;
    }
    case "int": {
        // Serialize int, just force the argument. Ints are unboxed.
        jsObj = _(fayObj);
        break;
    }
    case "bool": {
        // Bools are unboxed.
        jsObj = _(fayObj);
        break;
    }
    case "unknown":
    case "user": {
        if(fayObj instanceof $)
            fayObj = _(fayObj);
        jsObj = Fay$$fayToJsUserDefined(type,fayObj);
        break;
    }
    default: throw new Error("Unhandled Fay->JS translation type: " + base);
    }
    return jsObj;
}

// Unserialize an object from JS to Fay.
function Fay$$jsToFay(type,jsObj){
    var base = type[0];
    var args = type[1];
    var fayObj;
    switch(base){
    case "action": {
        // Unserialize a "monadic" JavaScript return value into a monadic value.
        fayObj = new Fay$$Monad(Fay$$jsToFay(args[0],jsObj));
        break;
    }
    case "string": {
        // Unserialize a JS string into Fay list (String).
        fayObj = Fay$$list(jsObj);
        break;
    }
    case "list": {
        // Unserialize a JS array into a Fay list ([a]).
        var serializedList = [];
        for (var i = 0, len = jsObj.length; i < len; i++) {
            // Unserialize each JS value into a Fay value, too.
            serializedList.push(Fay$$jsToFay(args[0],jsObj[i]));
        }
        // Pop it all in a Fay list.
        fayObj = Fay$$list(serializedList);
        break;
    }
    case "double": {
        // Doubles are unboxed, so there's nothing to do.
        fayObj = jsObj;
        break;
    }
    case "int": {
        // Int are unboxed, so there's no forcing to do.
        // But we can do validation that the int has no decimal places.
        // E.g. Math.round(x)!=x? throw "NOT AN INTEGER, GET OUT!"
        fayObj = Math.round(jsObj);
        if(fayObj!==jsObj) throw "Argument " + jsObj + " is not an integer!"
        break;
    }
    case "bool": {
        // Bools are unboxed.
        fayObj = jsObj;
        break;
    }
    case "unknown":
    case "user": {
        if (jsObj && jsObj['instance']) {
            fayObj = Fay$$jsToFayUserDefined(type,jsObj);
        }
        else
            fayObj = jsObj;
        break;
    }
    default: throw new Error("Unhandled JS->Fay translation type: " + base);
    }
    return fayObj;
}

/*******************************************************************************
 * Lists.
 */

// Cons object.
function Fay$$Cons(car,cdr){
    this.car = car;
    this.cdr = cdr;
}

// Make a list.
function Fay$$list(xs){
    var out = null;
    for(var i=xs.length-1; i>=0;i--)
        out = new Fay$$Cons(xs[i],out);
    return out;
}

// Built-in list cons.
function Fay$$cons(x){
    return function(y){
        return new Fay$$Cons(x,y);
    };
}

// List index.
function Fay$$index(index){
    return function(list){
        for(var i = 0; i < index; i++) {
            list = _(list).cdr;
        }
        return list.car;
    };
}

/*******************************************************************************
 * Numbers.
 */

// Built-in *.
function Fay$$mult(x){
    return function(y){
        return _(x) * _(y);
    };
}
var $42$ = Fay$$mult;

// Built-in +.
function Fay$$add(x){
    return function(y){
        return _(x) + _(y);
    };
}
var $43$ = Fay$$add;

// Built-in -.
function Fay$$sub(x){
    return function(y){
        return _(x) - _(y);
    };
}
var $45$ = Fay$$sub;

// Built-in /.
function Fay$$div(x){
    return function(y){
        return _(x) / _(y);
    };
}
var $47$ = Fay$$div;

/*******************************************************************************
 * Booleans.
 */

// Are two values equal?
function Fay$$equal(lit1, lit2) {
    // Simple case
    lit1 = _(lit1);
    lit2 = _(lit2);
    if (lit1 === lit2) {
        return true;
    }
    // General case
    if (lit1 instanceof Array) {
        if (lit1.length != lit2.length) return false;
        for (var len = lit1.length, i = 0; i < len; i++) {
            if (!Fay$$equal(lit1[i], lit2[i])) return false;
        }
        return true;
    } else if (lit1 instanceof Fay$$Cons && lit2 instanceof Fay$$Cons) {
        do {
            if (!Fay$$equal(lit1.car,lit2.car))
                return false;
            lit1 = _(lit1.cdr), lit2 = _(lit2.cdr);
            if (lit1 === null || lit2 === null)
                return lit1 === lit2;
        } while (true);
    } else if (typeof lit1 == 'object' && typeof lit2 == 'object' && lit1 && lit2 &&
              lit1.constructor === lit2.constructor) {
      for(var x in lit1) {
        if(!(lit1.hasOwnProperty(x) && lit2.hasOwnProperty(x) &&
            Fay$$equal(lit1[x],lit2[x])))
          return false;
      }
      return true;
    } else {
      return false;
    }
}

// Built-in ==.
function Fay$$eq(x){
    return function(y){
        return Fay$$equal(x,y);
    };
}
var $61$$61$ = Fay$$eq;

// Built-in /=.
function Fay$$neq(x){
    return function(y){
        return !(Fay$$equal(x,y));
    };
}
var $47$$61$ = Fay$$neq;

// Built-in >.
function Fay$$gt(x){
    return function(y){
        return _(x) > _(y);
    };
}
var $62$ = Fay$$gt;

// Built-in <.
function Fay$$lt(x){
    return function(y){
        return _(x) < _(y);
    };
}
var $60$ = Fay$$lt;

// Built-in >=.
function Fay$$gte(x){
    return function(y){
        return _(x) >= _(y);
    };
}
var $62$$61$ = Fay$$gte;

// Built-in <=.
function Fay$$lte(x){
    return function(y){
        return _(x) <= _(y);
    };
}
var $60$$61$ = Fay$$lte;

// Built-in &&.
function Fay$$and(x){
    return function(y){
        return _(x) && _(y);
    };
}
var $38$$38$ = Fay$$and;

// Built-in ||.
function Fay$$or(x){
    return function(y){
        return _(x) || _(y);
    };
}
var $124$$124$ = Fay$$or;

/*******************************************************************************
 * Mutable references.
 */

// Make a new mutable reference.
function Fay$$Ref(x){
    this.value = x;
}

// Write to the ref.
function Fay$$writeRef(ref,x){
    ref.value = x;
}

// Get the value from the ref.
function Fay$$readRef(ref,x){
    return ref.value;
}

/*******************************************************************************
 * Dates.
 */
function Fay$$date(str){
    return window.Date.parse(str);
}

/*******************************************************************************
 * Application code.
 */


var getBody = new $(function(){return Fay$$jsToFay(["action",[["user","Element",[]]]],document['body']);
});
var setTimeout = function($36$_a){return function($36$_b){return new $(function(){return Fay$$jsToFay(["action",[["user","Timer",[]]]],window['setTimeout'](Fay$$fayToJs(["action",[["unknown"]]],$36$_b),Fay$$fayToJs(["double"],$36$_a)));
});
};
};
var setInterval = function($36$_a){return function($36$_b){return new $(function(){return Fay$$jsToFay(["action",[["user","Timer",[]]]],window['setInterval'](Fay$$fayToJs(["action",[["unknown"]]],$36$_b),Fay$$fayToJs(["double"],$36$_a)));
});
};
};
var clearTimeout = function($36$_a){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],window['clearTimeout'](Fay$$fayToJs(["user","Timer",[]],$36$_a)));
});
};
var alert = function($36$_a){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],window.alert(Fay$$fayToJs(["string"],$36$_a)));
});
};
var fib = function($36$_a){return new $(function(){if (_($36$_a) === 0) {return 1;
}if (_($36$_a) === 1) {return 1;
}var n = $36$_a;
return _(_(fib)(_(n) - 1)) + _(_(fib)(_(n) - 2));
});
};
var fib3 = function($36$_a){return new $(function(){if (_($36$_a) === 0) {return 1;
}var n = $36$_a;
return _(_(_(foldr)($43$))(0))(enumFromTo(1)(n));
});
};
var main = new $(function(){return _(alert)(_(show)(_(fib)(10)));
});
var $36$_Just = function(slot1){this.slot1 = slot1;
};
var Just = function(slot1){return new $(function(){return new $36$_Just(slot1);
});
};
var $36$_Nothing = function(){};
var Nothing = new $(function(){return new $36$_Nothing();
});
var show = function($36$_a){return new $(function(){return Fay$$jsToFay(["string"],JSON.stringify(Fay$$fayToJs(["unknown"],$36$_a)));
});
};
var fromInteger = function($36$_a){return new $(function(){var x = $36$_a;
return x;
});
};
var fromRational = function($36$_a){return new $(function(){var x = $36$_a;
return x;
});
};
var snd = function($36$_a){return new $(function(){var x = Fay$$index(1)(_($36$_a));
return x;
throw ["unhandled case in Ident \"snd\"",[$36$_a]];
});
};
var fst = function($36$_a){return new $(function(){var x = Fay$$index(0)(_($36$_a));
return x;
throw ["unhandled case in Ident \"fst\"",[$36$_a]];
});
};
var find = function($36$_a){return function($36$_b){return new $(function(){var $36$_$36$_b = _($36$_b);
if ($36$_$36$_b instanceof Fay$$Cons) {var x = $36$_$36$_b.car;
var xs = $36$_$36$_b.cdr;
var p = $36$_a;
return _(_(p)(x)) ? _(Just)(x) : _(_(find)(p))(xs);
}if (_($36$_b) === null) {return Nothing;
}throw ["unhandled case in Ident \"find\"",[$36$_a,$36$_b]];
});
};
};
var any = function($36$_a){return function($36$_b){return new $(function(){var $36$_$36$_b = _($36$_b);
if ($36$_$36$_b instanceof Fay$$Cons) {var x = $36$_$36$_b.car;
var xs = $36$_$36$_b.cdr;
var p = $36$_a;
return _(_(p)(x)) ? true : _(_(any)(p))(xs);
}if (_($36$_b) === null) {return false;
}throw ["unhandled case in Ident \"any\"",[$36$_a,$36$_b]];
});
};
};
var filter = function($36$_a){return function($36$_b){return new $(function(){var $36$_$36$_b = _($36$_b);
if ($36$_$36$_b instanceof Fay$$Cons) {var x = $36$_$36$_b.car;
var xs = $36$_$36$_b.cdr;
var p = $36$_a;
return _(_(p)(x)) ? _(_(Fay$$cons)(x))(_(_(filter)(p))(xs)) : _(_(filter)(p))(xs);
}if (_($36$_b) === null) {return null;
}throw ["unhandled case in Ident \"filter\"",[$36$_a,$36$_b]];
});
};
};
var not = function($36$_a){return new $(function(){var p = $36$_a;
return _(p) ? false : true;
});
};
var $_null = function($36$_a){return new $(function(){if (_($36$_a) === null) {return true;
}return false;
});
};
var map = function($36$_a){return function($36$_b){return new $(function(){if (_($36$_b) === null) {return null;
}var $36$_$36$_b = _($36$_b);
if ($36$_$36$_b instanceof Fay$$Cons) {var x = $36$_$36$_b.car;
var xs = $36$_$36$_b.cdr;
var f = $36$_a;
return _(_(Fay$$cons)(_(f)(x)))(_(_(map)(f))(xs));
}throw ["unhandled case in Ident \"map\"",[$36$_a,$36$_b]];
});
};
};
var nub = function($36$_a){return new $(function(){var ls = $36$_a;
return _(_(nub$39$)(ls))(null);
});
};
var nub$39$ = function($36$_a){return function($36$_b){return new $(function(){if (_($36$_a) === null) {return null;
}var ls = $36$_b;
var $36$_$36$_a = _($36$_a);
if ($36$_$36$_a instanceof Fay$$Cons) {var x = $36$_$36$_a.car;
var xs = $36$_$36$_a.cdr;
return _(_(_(elem)(x))(ls)) ? _(_(nub$39$)(xs))(ls) : _(_(Fay$$cons)(x))(_(_(nub$39$)(xs))(_(_(Fay$$cons)(x))(ls)));
}throw ["unhandled case in Ident \"nub'\"",[$36$_a,$36$_b]];
});
};
};
var elem = function($36$_a){return function($36$_b){return new $(function(){var $36$_$36$_b = _($36$_b);
if ($36$_$36$_b instanceof Fay$$Cons) {var y = $36$_$36$_b.car;
var ys = $36$_$36$_b.cdr;
var x = $36$_a;
return _(_(_(Fay$$eq)(x))(y)) || _(_(_(elem)(x))(ys));
}if (_($36$_b) === null) {return false;
}throw ["unhandled case in Ident \"elem\"",[$36$_a,$36$_b]];
});
};
};
var $36$_GT = function(){};
var GT = new $(function(){return new $36$_GT();
});
var $36$_LT = function(){};
var LT = new $(function(){return new $36$_LT();
});
var $36$_EQ = function(){};
var EQ = new $(function(){return new $36$_EQ();
});
var sort = new $(function(){return _(sortBy)(compare);
});
var compare = function($36$_a){return function($36$_b){return new $(function(){var y = $36$_b;
var x = $36$_a;
return _(_(x) > _(y)) ? GT : _(_(x) < _(y)) ? LT : EQ;
});
};
};
var sortBy = function($36$_a){return new $(function(){var cmp = $36$_a;
return _(_(foldr)(_(insertBy)(cmp)))(null);
});
};
var insertBy = function($36$_a){return function($36$_b){return function($36$_c){return new $(function(){if (_($36$_c) === null) {var x = $36$_b;
return Fay$$list([x]);
}var ys = $36$_c;
var x = $36$_b;
var cmp = $36$_a;
return (function($36$_ys){if (_($36$_ys) === null) {return Fay$$list([x]);
}var $36$_$36$_ys = _($36$_ys);
if ($36$_$36$_ys instanceof Fay$$Cons) {var y = $36$_$36$_ys.car;
var ys$39$ = $36$_$36$_ys.cdr;
return (function($tmp){if (_($tmp) instanceof $36$_GT) {return _(_(Fay$$cons)(y))(_(_(_(insertBy)(cmp))(x))(ys$39$));
}return _(_(Fay$$cons)(x))(ys);
})(_(_(cmp)(x))(y));
}return (function(){ throw (["unhandled case",$36$_ys]);
 })();
})(ys);
});
};
};
};
var when = function($36$_a){return function($36$_b){return new $(function(){var m = $36$_b;
var p = $36$_a;
return _(p) ? _(_($62$$62$)(m))(_($_return)(Fay$$unit)) : _($_return)(Fay$$unit);
});
};
};
var enumFrom = function($36$_a){return new $(function(){var i = $36$_a;
return _(_(Fay$$cons)(i))(_(enumFrom)(_(i) + 1));
});
};
var enumFromTo = function($36$_a){return function($36$_b){return new $(function(){var n = $36$_b;
var i = $36$_a;
return _(_(_(Fay$$eq)(i))(n)) ? Fay$$list([i]) : _(_(Fay$$cons)(i))(_(_(enumFromTo)(_(i) + 1))(n));
});
};
};
var zipWith = function($36$_a){return function($36$_b){return function($36$_c){return new $(function(){var $36$_$36$_c = _($36$_c);
if ($36$_$36$_c instanceof Fay$$Cons) {var b = $36$_$36$_c.car;
var bs = $36$_$36$_c.cdr;
var $36$_$36$_b = _($36$_b);
if ($36$_$36$_b instanceof Fay$$Cons) {var a = $36$_$36$_b.car;
var as = $36$_$36$_b.cdr;
var f = $36$_a;
return _(_(Fay$$cons)(_(_(f)(a))(b)))(_(_(_(zipWith)(f))(as))(bs));
}}return null;
});
};
};
};
var zip = function($36$_a){return function($36$_b){return new $(function(){var $36$_$36$_b = _($36$_b);
if ($36$_$36$_b instanceof Fay$$Cons) {var b = $36$_$36$_b.car;
var bs = $36$_$36$_b.cdr;
var $36$_$36$_a = _($36$_a);
if ($36$_$36$_a instanceof Fay$$Cons) {var a = $36$_$36$_a.car;
var as = $36$_$36$_a.cdr;
return _(_(Fay$$cons)(Fay$$list([a,b])))(_(_(zip)(as))(bs));
}}return null;
});
};
};
var flip = function($36$_a){return function($36$_b){return function($36$_c){return new $(function(){var y = $36$_c;
var x = $36$_b;
var f = $36$_a;
return _(_(f)(y))(x);
});
};
};
};
var maybe = function($36$_a){return function($36$_b){return function($36$_c){return new $(function(){if (_($36$_c) instanceof $36$_Nothing) {var m = $36$_a;
return m;
}if (_($36$_c) instanceof $36$_Just) {var x = _($36$_c).slot1;
var f = $36$_b;
return _(f)(x);
}throw ["unhandled case in Ident \"maybe\"",[$36$_a,$36$_b,$36$_c]];
});
};
};
};
var $46$ = function($36$_a){return function($36$_b){return function($36$_c){return new $(function(){var x = $36$_c;
var g = $36$_b;
var f = $36$_a;
return _(f)(_(g)(x));
});
};
};
};
var $43$$43$ = function($36$_a){return function($36$_b){return new $(function(){var y = $36$_b;
var x = $36$_a;
return _(_(conc)(x))(y);
});
};
};
var $36$ = function($36$_a){return function($36$_b){return new $(function(){var x = $36$_b;
var f = $36$_a;
return _(f)(x);
});
};
};
var conc = function($36$_a){return function($36$_b){return new $(function(){var ys = $36$_b;
var $36$_$36$_a = _($36$_a);
if ($36$_$36$_a instanceof Fay$$Cons) {var x = $36$_$36$_a.car;
var xs = $36$_$36$_a.cdr;
return _(_(Fay$$cons)(x))(_(_(conc)(xs))(ys));
}var ys = $36$_b;
if (_($36$_a) === null) {return ys;
}throw ["unhandled case in Ident \"conc\"",[$36$_a,$36$_b]];
});
};
};
var concat = new $(function(){return _(_(foldr)(conc))(null);
});
var foldr = function($36$_a){return function($36$_b){return function($36$_c){return new $(function(){if (_($36$_c) === null) {var z = $36$_b;
return z;
}var $36$_$36$_c = _($36$_c);
if ($36$_$36$_c instanceof Fay$$Cons) {var x = $36$_$36$_c.car;
var xs = $36$_$36$_c.cdr;
var z = $36$_b;
var f = $36$_a;
return _(_(f)(x))(_(_(_(foldr)(f))(z))(xs));
}throw ["unhandled case in Ident \"foldr\"",[$36$_a,$36$_b,$36$_c]];
});
};
};
};
var foldl = function($36$_a){return function($36$_b){return function($36$_c){return new $(function(){if (_($36$_c) === null) {var z = $36$_b;
return z;
}var $36$_$36$_c = _($36$_c);
if ($36$_$36$_c instanceof Fay$$Cons) {var x = $36$_$36$_c.car;
var xs = $36$_$36$_c.cdr;
var z = $36$_b;
var f = $36$_a;
return _(_(_(foldl)(f))(_(_(f)(z))(x)))(xs);
}throw ["unhandled case in Ident \"foldl\"",[$36$_a,$36$_b,$36$_c]];
});
};
};
};
var lookup = function($36$_a){return function($36$_b){return new $(function(){if (_($36$_b) === null) {var _key = $36$_a;
return Nothing;
}var $36$_$36$_b = _($36$_b);
if ($36$_$36$_b instanceof Fay$$Cons) {var x = Fay$$index(0)(_($36$_$36$_b.car));
var y = Fay$$index(1)(_($36$_$36$_b.car));
var xys = $36$_$36$_b.cdr;
var key = $36$_a;
return _(_(_(Fay$$eq)(key))(x)) ? _(Just)(y) : _(_(lookup)(key))(xys);
}throw ["unhandled case in Ident \"lookup\"",[$36$_a,$36$_b]];
});
};
};
var intersperse = function($36$_a){return function($36$_b){return new $(function(){if (_($36$_b) === null) {return null;
}var $36$_$36$_b = _($36$_b);
if ($36$_$36$_b instanceof Fay$$Cons) {var x = $36$_$36$_b.car;
var xs = $36$_$36$_b.cdr;
var sep = $36$_a;
return _(_(Fay$$cons)(x))(_(_(prependToAll)(sep))(xs));
}throw ["unhandled case in Ident \"intersperse\"",[$36$_a,$36$_b]];
});
};
};
var prependToAll = function($36$_a){return function($36$_b){return new $(function(){if (_($36$_b) === null) {return null;
}var $36$_$36$_b = _($36$_b);
if ($36$_$36$_b instanceof Fay$$Cons) {var x = $36$_$36$_b.car;
var xs = $36$_$36$_b.cdr;
var sep = $36$_a;
return _(_(Fay$$cons)(sep))(_(_(Fay$$cons)(x))(_(_(prependToAll)(sep))(xs)));
}throw ["unhandled case in Ident \"prependToAll\"",[$36$_a,$36$_b]];
});
};
};
var intercalate = function($36$_a){return function($36$_b){return new $(function(){var xss = $36$_b;
var xs = $36$_a;
return _(concat)(_(_(intersperse)(xs))(xss));
});
};
};
var forM_ = function($36$_a){return function($36$_b){return new $(function(){var m = $36$_b;
var $36$_$36$_a = _($36$_a);
if ($36$_$36$_a instanceof Fay$$Cons) {var x = $36$_$36$_a.car;
var xs = $36$_$36$_a.cdr;
return _(_($62$$62$)(_(m)(x)))(_(_(forM_)(xs))(m));
}if (_($36$_a) === null) {return _($_return)(Fay$$unit);
}throw ["unhandled case in Ident \"forM_\"",[$36$_a,$36$_b]];
});
};
};
var mapM_ = function($36$_a){return function($36$_b){return new $(function(){var $36$_$36$_b = _($36$_b);
if ($36$_$36$_b instanceof Fay$$Cons) {var x = $36$_$36$_b.car;
var xs = $36$_$36$_b.cdr;
var m = $36$_a;
return _(_($62$$62$)(_(m)(x)))(_(_(mapM_)(m))(xs));
}if (_($36$_b) === null) {return _($_return)(Fay$$unit);
}throw ["unhandled case in Ident \"mapM_\"",[$36$_a,$36$_b]];
});
};
};
var $_const = function($36$_a){return function($36$_b){return new $(function(){var a = $36$_a;
return a;
});
};
};
var length = function($36$_a){return new $(function(){var $36$_$36$_a = _($36$_a);
if ($36$_$36$_a instanceof Fay$$Cons) {var xs = $36$_$36$_a.cdr;
return 1 + _(_(length)(xs));
}if (_($36$_a) === null) {return 0;
}throw ["unhandled case in Ident \"length\"",[$36$_a]];
});
};
var mod = function($36$_a){return function($36$_b){return new $(function(){return Fay$$jsToFay(["double"],Fay$$fayToJs(["double"],$36$_a) % Fay$$fayToJs(["double"],$36$_b));
});
};
};
var min = function($36$_a){return function($36$_b){return new $(function(){return Fay$$jsToFay(["double"],Math.min(Fay$$fayToJs(["double"],$36$_a),Fay$$fayToJs(["double"],$36$_b)));
});
};
};
var max = function($36$_a){return function($36$_b){return new $(function(){return Fay$$jsToFay(["double"],Math.max(Fay$$fayToJs(["double"],$36$_a),Fay$$fayToJs(["double"],$36$_b)));
});
};
};
var fromIntegral = function($36$_a){return new $(function(){return Fay$$jsToFay(["double"],Fay$$fayToJs(["int"],$36$_a));
});
};
var otherwise = true;
var reverse = function($36$_a){return new $(function(){var $36$_$36$_a = _($36$_a);
if ($36$_$36$_a instanceof Fay$$Cons) {var x = $36$_$36$_a.car;
var xs = $36$_$36$_a.cdr;
return _(_($43$$43$)(_(reverse)(xs)))(Fay$$list([x]));
}if (_($36$_a) === null) {return null;
}throw ["unhandled case in Ident \"reverse\"",[$36$_a]];
});
};
var Fay$$fayToJsUserDefined = function(type,obj){var _obj = _(obj);
var argTypes = type[2];
if (_obj instanceof $36$_EQ) {return {"instance": "EQ"};
}if (_obj instanceof $36$_LT) {return {"instance": "LT"};
}if (_obj instanceof $36$_GT) {return {"instance": "GT"};
}if (_obj instanceof $36$_Nothing) {return {"instance": "Nothing"};
}if (_obj instanceof $36$_Just) {return {"instance": "Just","slot1": Fay$$fayToJs(["unknown"],_(_obj.slot1))};
}return obj;
};
var Fay$$jsToFayUserDefined = function(type,obj){if (obj["instance"] === "EQ") {return new $36$_EQ();
}if (obj["instance"] === "LT") {return new $36$_LT();
}if (obj["instance"] === "GT") {return new $36$_GT();
}if (obj["instance"] === "Nothing") {return new $36$_Nothing();
}if (obj["instance"] === "Just") {return new $36$_Just(Fay$$jsToFay(["unknown"],obj["slot1"]));
}return obj;
};




