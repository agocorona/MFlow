/** @constructor
*/
var Main = function(){
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
$.prototype.force = function(nocache){
    return nocache
        ? this.value()
        : this.forced
        ? this.value
        : (this.forced = true, this.value = this.value());
};


// Eval in the context of the Haskell bindings.
function Fay$$eval(str){
    return eval(str);
}

/*******************************************************************************
 * Monad.
 */

function Fay$$Monad(value){
    this.value = value;
}

// >>
// encode_fay_to_js(">>=") → $62$$62$
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
// encode_fay_to_js(">>=") → $62$$62$$61$
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

// Built-in +.
function Fay$$add(x){
    return function(y){
        return _(x) + _(y);
    };
}

// Built-in -.
function Fay$$sub(x){
    return function(y){
        return _(x) - _(y);
    };
}

// Built-in /.
function Fay$$div(x){
    return function(y){
        return _(x) / _(y);
    };
}

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

// Built-in /=.
function Fay$$neq(x){
    return function(y){
        return !(Fay$$equal(x,y));
    };
}

// Built-in >.
function Fay$$gt(x){
    return function(y){
        return _(x) > _(y);
    };
}

// Built-in <.
function Fay$$lt(x){
    return function(y){
        return _(x) < _(y);
    };
}

// Built-in >=.
function Fay$$gte(x){
    return function(y){
        return _(x) >= _(y);
    };
}

// Built-in <=.
function Fay$$lte(x){
    return function(y){
        return _(x) <= _(y);
    };
}

// Built-in &&.
function Fay$$and(x){
    return function(y){
        return _(x) && _(y);
    };
}

// Built-in ||.
function Fay$$or(x){
    return function(y){
        return _(x) || _(y);
    };
}

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

var getBody = new $(function(){return Fay$$jsToFay(["action",[["user","Element",[]]]],document['body']);});var setTimeout = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","Timer",[]]]],window['setTimeout'](Fay$$fayToJs(["action",[["unknown"]]],$_b),Fay$$fayToJs(["double"],$_a)));});};};var setInterval = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","Timer",[]]]],window['setInterval'](Fay$$fayToJs(["action",[["unknown"]]],$_b),Fay$$fayToJs(["double"],$_a)));});};};var clearTimeout = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],window['clearTimeout'](Fay$$fayToJs(["user","Timer",[]],$_a)));});};var alert = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],window.alert(Fay$$fayToJs(["string"],$_a)));});};var documentGetElements = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["list",[["user","Element",[]]]]]],document.getElementsByTagName(Fay$$fayToJs(["string"],$_a)));});};var documentGetElementById = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","Element",[]]]],document.getElementById(Fay$$fayToJs(["string"],$_a)));});};var addEventListener = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],window['addEventListener'](Fay$$fayToJs(["string"],$_a),Fay$$fayToJs(["action",[["unknown"]]],$_b),Fay$$fayToJs(["bool"],$_c)));});};};};var getElementBy = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],$('Fay$$fayToJs(["string"],$_a)'));});};var getThis = new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],this);});var addClass = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['addClass'](Fay$$fayToJs(["string"],$_a)));});};};var addClassWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['addClass'](Fay$$fayToJs(["function",[["double"],["string"],["action",[["string"]]]]],$_a)));});};};var getAttr = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['attr'](Fay$$fayToJs(["string"],$_a)));});};};var setAttr = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['attr'](Fay$$fayToJs(["string"],$_a), Fay$$fayToJs(["string"],$_b)));});};};};var setAttrWith = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['attr'](Fay$$fayToJs(["string"],$_a), Fay$$fayToJs(["function",[["double"],["string"],["action",[["string"]]]]],$_b)));});};};};var hasClass = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["bool"]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['hasClass'](Fay$$fayToJs(["string"],$_a)));});};};var getHtml = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['html']());});};var setHtml = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['html'](Fay$$fayToJs(["string"],$_a)));});};};var setHtmlWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['html'](Fay$$fayToJs(["function",[["double"],["string"],["action",[["user","JQuery",[]]]]]],$_a)));});};};var getProp = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['prop'](Fay$$fayToJs(["string"],$_a)));});};};var setProp = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['prop'](Fay$$fayToJs(["string"],$_a), Fay$$fayToJs(["string"],$_b)));});};};};var setPropWith = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['prop'](Fay$$fayToJs(["string"],$_a), Fay$$fayToJs(["function",[["double"],["string"],["action",[["string"]]]]],$_b)));});};};};var removeAttr = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['removeAttr'](Fay$$fayToJs(["string"],$_a)));});};};var removeClass = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['removeClass'](Fay$$fayToJs(["string"],$_a)));});};};var removeClassWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['removeClass'](Fay$$fayToJs(["function",[["double"],["string"],["action",[["user","JQuery",[]]]]]],$_a)));});};};var removeProp = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['removeProp'](Fay$$fayToJs(["string"],$_a)));});};};var toggleClass = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['toggleClass'](Fay$$fayToJs(["string"],$_a)));});};};var toggleClassBool = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['toggleClass'](Fay$$fayToJs(["string"],$_a), Fay$$fayToJs(["bool"],$_b)));});};};};var toggleAllClasses = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['toggleClass'](Fay$$fayToJs(["bool"],$_a)));});};};var toggleClassWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['toggleClass'](Fay$$fayToJs(["function",[["double"],["string"],["bool"],["action",[["user","JQuery",[]]]]]],$_a)));});};};var toggleClassBoolWith = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['toggleClass'](Fay$$fayToJs(["function",[["double"],["string"],["bool"],["action",[["user","JQuery",[]]]]]],$_a), Fay$$fayToJs(["bool"],$_b)));});};};};var getVal = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['val']());});};var setVal = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['val'](Fay$$fayToJs(["string"],$_a)));});};};var setValWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['val'](Fay$$fayToJs(["function",[["double"],["string"],["action",[["user","JQuery",[]]]]]],$_a)));});};};var setText = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['text'](Fay$$fayToJs(["string"],$_a)));});};};var setTextWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['text'](Fay$$fayToJs(["function",[["double"],["string"],["action",[["user","JQuery",[]]]]]],$_a)));});};};var getText = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['text']());});};var holdReady = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],window['jQuery']['holdReady'](Fay$$fayToJs(["bool"],$_a)));});};var selectElement = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],window['jQuery'](Fay$$fayToJs(["user","Element",[]],$_a)));});};var select = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],window['jQuery'](Fay$$fayToJs(["string"],$_a)));});};var selectEmpty = new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],window['jQuery']());});var selectQuery = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],window['jQuery'](Fay$$fayToJs(["string"],$_a), Fay$$fayToJs(["user","JQuery",[]],$_b)));});};};var ready = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["unknown"]]],window['jQuery'](Fay$$fayToJs(["action",[["unknown"]]],$_a)));});};var getCss = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["string"]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['css'](Fay$$fayToJs(["string"],$_a)));});};};var setCss = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['css'](Fay$$fayToJs(["string"],$_a), Fay$$fayToJs(["string"],$_b)));});};};};var setCssWith = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['css'](Fay$$fayToJs(["string"],$_a), Fay$$fayToJs(["function",[["double"],["string"],["action",[["string"]]]]],$_b)));});};};};var getHeight = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['height']());});};var setHeight = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['height'](Fay$$fayToJs(["double"],$_a)));});};};var setHeightWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['height'](Fay$$fayToJs(["function",[["double"],["double"],["action",[["double"]]]]],$_a)));});};};var getInnerHeight = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['innerHeight']());});};var getInnerWidth = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['innerWidth']());});};var getOuterHeight = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['outerHeight']());});};var getOuterHeightBool = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['outerHeight'](Fay$$fayToJs(["bool"],$_a)));});};};var getOuterWidth = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['outerWidth']());});};var getOuterWidthBool = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['outerWidth'](Fay$$fayToJs(["bool"],$_a)));});};};var getScrollLeft = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['scrollLeft']());});};var setScrollLeft = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['scrollLeft'](Fay$$fayToJs(["double"],$_a)));});};};var getScrollTop = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['scrollTop']());});};var setScrollTop = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['scrollTop'](Fay$$fayToJs(["double"],$_a)));});};};var getWidth = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['width']());});};var setWidth = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['width'](Fay$$fayToJs(["double"],$_a)));});};};var setWidthWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['width'](Fay$$fayToJs(["function",[["double"],["double"],["action",[["double"]]]]],$_a)));});};};var after = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['after'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var afterWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['after'](Fay$$fayToJs(["function",[["double"],["action",[["user","JQuery",[]]]]]],$_a)));});};};var append = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['append'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var appendWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['append'](Fay$$fayToJs(["function",[["double"],["action",[["user","JQuery",[]]]]]],$_a)));});};};var appendTo = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['appendTo'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var appendToJQuery = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['appendTo'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var before = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['before'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var beforeWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['before'](Fay$$fayToJs(["function",[["double"],["action",[["user","JQuery",[]]]]]],$_a)));});};};var detach = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['detach']());});};var detachSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['detach'](Fay$$fayToJs(["string"],$_a)));});};};var empty = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['empty']());});};var insertAfter = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['insertAfter'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var insertBefore = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['insertBefore'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var prepend = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['prepend'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var prependWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['prepend'](Fay$$fayToJs(["function",[["double"],["action",[["user","JQuery",[]]]]]],$_a)));});};};var prependTo = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['prependTo'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var remove = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['remove']());});};var removeSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['remove'](Fay$$fayToJs(["string"],$_a)));});};};var replaceAll = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['replaceAll'](Fay$$fayToJs(["string"],$_a)));});};};var replaceWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['replaceWith'](Fay$$fayToJs(["string"],$_a)));});};};var replaceWithJQuery = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['replaceWith'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var replaceWithWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['replaceWith'](Fay$$fayToJs(["action",[["user","JQuery",[]]]],$_a)));});};};var unwrap = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['unwrap']());});};var wrap = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['wrap'](Fay$$fayToJs(["string"],$_a)));});};};var wrapWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['wrap'](Fay$$fayToJs(["function",[["double"],["action",[["user","JQuery",[]]]]]],$_a)));});};};var wrapAllHtml = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['wrapAll'](Fay$$fayToJs(["string"],$_a)));});};};var wrapAllSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['wrapAll'](Fay$$fayToJs(["string"],$_a)));});};};var wrapAllElement = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['wrapAll'](Fay$$fayToJs(["user","Element",[]],$_a)));});};};var wrapInnerHtml = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['wrapInner'](Fay$$fayToJs(["string"],$_a)));});};};var wrapInnerSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['wrapInner'](Fay$$fayToJs(["string"],$_a)));});};};var wrapInnerElement = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['wrapInner'](Fay$$fayToJs(["user","Element",[]],$_a)));});};};var addSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],$2['add'](Fay$$fayToJs(["string"],$_a)));});};};var addElement = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['add'](Fay$$fayToJs(["user","Element",[]],$_a)));});};};var addHtml = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['add'](Fay$$fayToJs(["string"],$_a)));});};};var add = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['add'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var addSelectorWithContext = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['add'](Fay$$fayToJs(["string"],$_a), Fay$$fayToJs(["user","JQuery",[]],$_b)));});};};};var andSelf = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['andSelf']());});};var children = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['children']());});};var childrenMatching = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['children'](Fay$$fayToJs(["string"],$_a)));});};};var closestSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['closest'](Fay$$fayToJs(["string"],$_a)));});};};var closestWithContext = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['closest'](Fay$$fayToJs(["string"],$_a), Fay$$fayToJs(["string"],$_b)));});};};};var closest = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['closest'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var closestElement = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['closest'](Fay$$fayToJs(["user","Element",[]],$_a)));});};};var contents = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['contents']());});};var each = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['each'](Fay$$fayToJs(["function",[["double"],["user","Element",[]],["bool"]]],$_a)));});};};var end = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['end']());});};var eq = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['eq'](Fay$$fayToJs(["double"],$_a)));});};};var filterWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['filter'](Fay$$fayToJs(["function",[["double"],["action",[["bool"]]]]],$_a)));});};};var filterElement = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['filter'](Fay$$fayToJs(["user","Element",[]],$_a)));});};};var filterJQuery = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['filter'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var findSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['find'](Fay$$fayToJs(["string"],$_a)));});};};var findJQuery = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['find'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var findElement = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['find'](Fay$$fayToJs(["user","Element",[]],$_a)));});};};var first = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['first']());});};var getElement = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","Element",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['get'](0));});};var has = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['has'](Fay$$fayToJs(["string"],$_a)));});};};var hasElement = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['has'](Fay$$fayToJs(["user","Element",[]],$_a)));});};};var is = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['is'](Fay$$fayToJs(["string"],$_a)));});};};var isWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['is'](Fay$$fayToJs(["function",[["double"],["bool"]]],$_a)));});};};var isJQuery = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['is'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var isElement = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['is'](Fay$$fayToJs(["user","Element",[]],$_a)));});};};var last = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['last']());});};var jQueryMap = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['map'](Fay$$fayToJs(["function",[["double"],["user","Element",[]],["action",[["user","JQuery",[]]]]]],$_a)));});};};var next = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['next']());});};var nextSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['next'](Fay$$fayToJs(["string"],$_a)));});};};var nextAll = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['nextAll']());});};var nextAllSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['nextAll'](Fay$$fayToJs(["string"],$_a)));});};};var nextUntil = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['nextUntil'](Fay$$fayToJs(["string"],$_a)));});};};var nextUntilFiltered = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['nextUntil'](Fay$$fayToJs(["string"],$_a), Fay$$fayToJs(["string"],$_b)));});};};};var nextUntilElement = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['nextUntil'](Fay$$fayToJs(["user","Element",[]],$_a)));});};};var nextUntilElementFiltered = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['nextUntil'](Fay$$fayToJs(["user","Element",[]],$_a), Fay$$fayToJs(["string"],$_b)));});};};};var isNot = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['not'](Fay$$fayToJs(["string"],$_a)));});};};var notElement = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['not'](Fay$$fayToJs(["user","Element",[]],$_a)));});};};var notElements = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['not'](Fay$$fayToJs(["list",[["user","Element",[]]]],$_a)));});};};var notWith = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['not'](Fay$$fayToJs(["function",[["double"],["bool"]]],$_a)));});};};var notJQuery = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['not'](Fay$$fayToJs(["user","JQuery",[]],$_a)));});};};var offsetParent = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['offsetParent']());});};var parent = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['parent']());});};var parentSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['parent'](Fay$$fayToJs(["string"],$_a)));});};};var parents = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['parents']());});};var parentsSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['parents'](Fay$$fayToJs(["string"],$_a)));});};};var parentsUntil = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['parentsUntil'](Fay$$fayToJs(["string"],$_a)));});};};var parentsUntilFiltered = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['parentsUntil'](Fay$$fayToJs(["string"],$_a), Fay$$fayToJs(["string"],$_b)));});};};};var parentsUntilElement = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['parentsUntil'](Fay$$fayToJs(["user","Element",[]],$_a)));});};};var parentsUntilElementFiltered = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['parentsUntil'](Fay$$fayToJs(["user","Element",[]],$_a), Fay$$fayToJs(["string"],$_b)));});};};};var prev = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['prev']());});};var hide = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['hide']());});};var unhide = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['show']());});};var fadeOut = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['fadeOut']());});};var fadeIn = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['fadeIn']());});};var focus = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['focus']());});};var prevSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['prev'](Fay$$fayToJs(["string"],$_a)));});};};var prevAll = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['prevAll']());});};var prevAllSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['prevAll'](Fay$$fayToJs(["string"],$_a)));});};};var prevUntil = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['prevUntil'](Fay$$fayToJs(["string"],$_a)));});};};var prevUntilFiltered = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['prevUntil'](Fay$$fayToJs(["string"],$_a), Fay$$fayToJs(["string"],$_b)));});};};};var prevUntilElement = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['prevUntil'](Fay$$fayToJs(["user","Element",[]],$_a)));});};};var prevUntilElementFiltered = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['prevUntil'](Fay$$fayToJs(["user","Element",[]],$_a), Fay$$fayToJs(["string"],$_b)));});};};};var siblings = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['siblings']());});};var siblingsSelector = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['siblings'](Fay$$fayToJs(["string"],$_a)));});};};var slice = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['slice'](Fay$$fayToJs(["double"],$_a)));});};};var sliceFromTo = function($_a){return function($_b){return function($_c){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_c)['slice'](Fay$$fayToJs(["double"],$_a), Fay$$fayToJs(["double"],$_b)));});};};};var fin = function($_a){return new $(function(){return _($_return)(Fay$$unit);});};var size = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["double"]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['length']);});};var $_void = function($_a){return new $(function(){var m = $_a;return _(_($62$$62$)(m))(_($_return)(Fay$$unit));});};var make = function($_a){return new $(function(){var n = $_a;return _(_($36$)(select))(_(_($43$$43$)(Fay$$list("<")))(_(_($43$$43$)(n))(_(_($43$$43$)(Fay$$list("></")))(_(_($43$$43$)(n))(Fay$$list(">"))))));});};var _onKeycode = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['keycode'](Fay$$fayToJs(["function",[["double"],["action",[["bool"]]]]],$_a)));});};};var $_KeyUp = function(){};var KeyUp = new $(function(){return new $_KeyUp();});var $_KeyDown = function(){};var KeyDown = new $(function(){return new $_KeyDown();});var $_KeyRet = function(){};var KeyRet = new $(function(){return new $_KeyRet();});var $_SomeKey = function(slot1){this.slot1 = slot1;};var SomeKey = function(slot1){return new $(function(){return new $_SomeKey(slot1);});};var onKeycode = function($_a){return function($_b){return new $(function(){var el = $_b;var callback = $_a;return _(_(_onKeycode)(function($_a){var code = $_a;return _(callback)((function($_code){if (_($_code) === 38) {return KeyUp;}if (_($_code) === 40) {return KeyDown;}if (_($_code) === 13) {return KeyRet;}return _(SomeKey)(code);})(code));}))(el);});};};var onClick = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_b)['click'](Fay$$fayToJs(["action",[["bool"]]],$_a)));});};};var click = function($_a){return new $(function(){return Fay$$jsToFay(["action",[["user","JQuery",[]]]],Fay$$fayToJs(["user","JQuery",[]],$_a)['click']());});};var main = new $(function(){return _($_return)(Fay$$unit);});var foo = new $(function(){return _(_($62$$62$$61$)(getThis))(function($_a){var $_this = $_a;return _(_(setText)($_this))(Fay$$list("hello"));});});var $_Just = function(slot1){this.slot1 = slot1;};var Just = function(slot1){return new $(function(){return new $_Just(slot1);});};var $_Nothing = function(){};var Nothing = new $(function(){return new $_Nothing();});var show = function($_a){return new $(function(){return Fay$$jsToFay(["string"],JSON.stringify(Fay$$fayToJs(["unknown"],$_a)));});};var fromInteger = function($_a){return new $(function(){var x = $_a;return x;});};var fromRational = function($_a){return new $(function(){var x = $_a;return x;});};var snd = function($_a){return new $(function(){var x = Fay$$index(1)(_($_a));return x;throw ["unhandled case in Ident \"snd\"",[$_a]];});};var fst = function($_a){return new $(function(){var x = Fay$$index(0)(_($_a));return x;throw ["unhandled case in Ident \"fst\"",[$_a]];});};var find = function($_a){return function($_b){return new $(function(){var $_$_b = _($_b);if ($_$_b instanceof Fay$$Cons) {var x = $_$_b.car;var xs = $_$_b.cdr;var p = $_a;return _(_(p)(x)) ? _(Just)(x) : _(_(find)(p))(xs);}if (_($_b) === null) {return Nothing;}throw ["unhandled case in Ident \"find\"",[$_a,$_b]];});};};var any = function($_a){return function($_b){return new $(function(){var $_$_b = _($_b);if ($_$_b instanceof Fay$$Cons) {var x = $_$_b.car;var xs = $_$_b.cdr;var p = $_a;return _(_(p)(x)) ? true : _(_(any)(p))(xs);}if (_($_b) === null) {return false;}throw ["unhandled case in Ident \"any\"",[$_a,$_b]];});};};var filter = function($_a){return function($_b){return new $(function(){var $_$_b = _($_b);if ($_$_b instanceof Fay$$Cons) {var x = $_$_b.car;var xs = $_$_b.cdr;var p = $_a;return _(_(p)(x)) ? _(_(Fay$$cons)(x))(_(_(filter)(p))(xs)) : _(_(filter)(p))(xs);}if (_($_b) === null) {return null;}throw ["unhandled case in Ident \"filter\"",[$_a,$_b]];});};};var not = function($_a){return new $(function(){var p = $_a;return _(p) ? false : true;});};var $_null = function($_a){return new $(function(){if (_($_a) === null) {return true;}return false;});};var map = function($_a){return function($_b){return new $(function(){if (_($_b) === null) {return null;}var $_$_b = _($_b);if ($_$_b instanceof Fay$$Cons) {var x = $_$_b.car;var xs = $_$_b.cdr;var f = $_a;return _(_(Fay$$cons)(_(f)(x)))(_(_(map)(f))(xs));}throw ["unhandled case in Ident \"map\"",[$_a,$_b]];});};};var nub = function($_a){return new $(function(){var ls = $_a;return _(_(nub$39$)(ls))(null);});};var nub$39$ = function($_a){return function($_b){return new $(function(){if (_($_a) === null) {return null;}var ls = $_b;var $_$_a = _($_a);if ($_$_a instanceof Fay$$Cons) {var x = $_$_a.car;var xs = $_$_a.cdr;return _(_(_(elem)(x))(ls)) ? _(_(nub$39$)(xs))(ls) : _(_(Fay$$cons)(x))(_(_(nub$39$)(xs))(_(_(Fay$$cons)(x))(ls)));}throw ["unhandled case in Ident \"nub'\"",[$_a,$_b]];});};};var elem = function($_a){return function($_b){return new $(function(){var $_$_b = _($_b);if ($_$_b instanceof Fay$$Cons) {var y = $_$_b.car;var ys = $_$_b.cdr;var x = $_a;return _(_(_(Fay$$eq)(x))(y)) || _(_(_(elem)(x))(ys));}if (_($_b) === null) {return false;}throw ["unhandled case in Ident \"elem\"",[$_a,$_b]];});};};var $_GT = function(){};var GT = new $(function(){return new $_GT();});var $_LT = function(){};var LT = new $(function(){return new $_LT();});var $_EQ = function(){};var EQ = new $(function(){return new $_EQ();});var sort = new $(function(){return _(sortBy)(compare);});var compare = function($_a){return function($_b){return new $(function(){var y = $_b;var x = $_a;return _(_(x) > _(y)) ? GT : _(_(x) < _(y)) ? LT : EQ;});};};var sortBy = function($_a){return new $(function(){var cmp = $_a;return _(_(foldr)(_(insertBy)(cmp)))(null);});};var insertBy = function($_a){return function($_b){return function($_c){return new $(function(){if (_($_c) === null) {var x = $_b;return Fay$$list([x]);}var ys = $_c;var x = $_b;var cmp = $_a;return (function($_ys){if (_($_ys) === null) {return Fay$$list([x]);}var $_$_ys = _($_ys);if ($_$_ys instanceof Fay$$Cons) {var y = $_$_ys.car;var ys$39$ = $_$_ys.cdr;return (function($tmp){if (_($tmp) instanceof $_GT) {return _(_(Fay$$cons)(y))(_(_(_(insertBy)(cmp))(x))(ys$39$));}return _(_(Fay$$cons)(x))(ys);})(_(_(cmp)(x))(y));}return (function(){ throw (["unhandled case",$_ys]); })();})(ys);});};};};var when = function($_a){return function($_b){return new $(function(){var m = $_b;var p = $_a;return _(p) ? _(_($62$$62$)(m))(_($_return)(Fay$$unit)) : _($_return)(Fay$$unit);});};};var enumFrom = function($_a){return new $(function(){var i = $_a;return _(_(Fay$$cons)(i))(_(enumFrom)(_(i) + 1));});};var enumFromTo = function($_a){return function($_b){return new $(function(){var n = $_b;var i = $_a;return _(_(_(Fay$$eq)(i))(n)) ? Fay$$list([i]) : _(_(Fay$$cons)(i))(_(_(enumFromTo)(_(i) + 1))(n));});};};var zipWith = function($_a){return function($_b){return function($_c){return new $(function(){var $_$_c = _($_c);if ($_$_c instanceof Fay$$Cons) {var b = $_$_c.car;var bs = $_$_c.cdr;var $_$_b = _($_b);if ($_$_b instanceof Fay$$Cons) {var a = $_$_b.car;var as = $_$_b.cdr;var f = $_a;return _(_(Fay$$cons)(_(_(f)(a))(b)))(_(_(_(zipWith)(f))(as))(bs));}}return null;});};};};var zip = function($_a){return function($_b){return new $(function(){var $_$_b = _($_b);if ($_$_b instanceof Fay$$Cons) {var b = $_$_b.car;var bs = $_$_b.cdr;var $_$_a = _($_a);if ($_$_a instanceof Fay$$Cons) {var a = $_$_a.car;var as = $_$_a.cdr;return _(_(Fay$$cons)(Fay$$list([a,b])))(_(_(zip)(as))(bs));}}return null;});};};var flip = function($_a){return function($_b){return function($_c){return new $(function(){var y = $_c;var x = $_b;var f = $_a;return _(_(f)(y))(x);});};};};var maybe = function($_a){return function($_b){return function($_c){return new $(function(){if (_($_c) instanceof $_Nothing) {var m = $_a;return m;}if (_($_c) instanceof $_Just) {var x = _($_c).slot1;var f = $_b;return _(f)(x);}throw ["unhandled case in Ident \"maybe\"",[$_a,$_b,$_c]];});};};};var $46$ = function($_a){return function($_b){return function($_c){return new $(function(){var x = $_c;var g = $_b;var f = $_a;return _(f)(_(g)(x));});};};};var $43$$43$ = function($_a){return function($_b){return new $(function(){var y = $_b;var x = $_a;return _(_(conc)(x))(y);});};};var $36$ = function($_a){return function($_b){return new $(function(){var x = $_b;var f = $_a;return _(f)(x);});};};var conc = function($_a){return function($_b){return new $(function(){var ys = $_b;var $_$_a = _($_a);if ($_$_a instanceof Fay$$Cons) {var x = $_$_a.car;var xs = $_$_a.cdr;return _(_(Fay$$cons)(x))(_(_(conc)(xs))(ys));}var ys = $_b;if (_($_a) === null) {return ys;}throw ["unhandled case in Ident \"conc\"",[$_a,$_b]];});};};var concat = new $(function(){return _(_(foldr)(conc))(null);});var foldr = function($_a){return function($_b){return function($_c){return new $(function(){if (_($_c) === null) {var z = $_b;return z;}var $_$_c = _($_c);if ($_$_c instanceof Fay$$Cons) {var x = $_$_c.car;var xs = $_$_c.cdr;var z = $_b;var f = $_a;return _(_(f)(x))(_(_(_(foldr)(f))(z))(xs));}throw ["unhandled case in Ident \"foldr\"",[$_a,$_b,$_c]];});};};};var foldl = function($_a){return function($_b){return function($_c){return new $(function(){if (_($_c) === null) {var z = $_b;return z;}var $_$_c = _($_c);if ($_$_c instanceof Fay$$Cons) {var x = $_$_c.car;var xs = $_$_c.cdr;var z = $_b;var f = $_a;return _(_(_(foldl)(f))(_(_(f)(z))(x)))(xs);}throw ["unhandled case in Ident \"foldl\"",[$_a,$_b,$_c]];});};};};var lookup = function($_a){return function($_b){return new $(function(){if (_($_b) === null) {var _key = $_a;return Nothing;}var $_$_b = _($_b);if ($_$_b instanceof Fay$$Cons) {var x = Fay$$index(0)(_($_$_b.car));var y = Fay$$index(1)(_($_$_b.car));var xys = $_$_b.cdr;var key = $_a;return _(_(_(Fay$$eq)(key))(x)) ? _(Just)(y) : _(_(lookup)(key))(xys);}throw ["unhandled case in Ident \"lookup\"",[$_a,$_b]];});};};var intersperse = function($_a){return function($_b){return new $(function(){if (_($_b) === null) {return null;}var $_$_b = _($_b);if ($_$_b instanceof Fay$$Cons) {var x = $_$_b.car;var xs = $_$_b.cdr;var sep = $_a;return _(_(Fay$$cons)(x))(_(_(prependToAll)(sep))(xs));}throw ["unhandled case in Ident \"intersperse\"",[$_a,$_b]];});};};var prependToAll = function($_a){return function($_b){return new $(function(){if (_($_b) === null) {return null;}var $_$_b = _($_b);if ($_$_b instanceof Fay$$Cons) {var x = $_$_b.car;var xs = $_$_b.cdr;var sep = $_a;return _(_(Fay$$cons)(sep))(_(_(Fay$$cons)(x))(_(_(prependToAll)(sep))(xs)));}throw ["unhandled case in Ident \"prependToAll\"",[$_a,$_b]];});};};var intercalate = function($_a){return function($_b){return new $(function(){var xss = $_b;var xs = $_a;return _(concat)(_(_(intersperse)(xs))(xss));});};};var forM_ = function($_a){return function($_b){return new $(function(){var m = $_b;var $_$_a = _($_a);if ($_$_a instanceof Fay$$Cons) {var x = $_$_a.car;var xs = $_$_a.cdr;return _(_($62$$62$)(_(m)(x)))(_(_(forM_)(xs))(m));}if (_($_a) === null) {return _($_return)(Fay$$unit);}throw ["unhandled case in Ident \"forM_\"",[$_a,$_b]];});};};var mapM_ = function($_a){return function($_b){return new $(function(){var $_$_b = _($_b);if ($_$_b instanceof Fay$$Cons) {var x = $_$_b.car;var xs = $_$_b.cdr;var m = $_a;return _(_($62$$62$)(_(m)(x)))(_(_(mapM_)(m))(xs));}if (_($_b) === null) {return _($_return)(Fay$$unit);}throw ["unhandled case in Ident \"mapM_\"",[$_a,$_b]];});};};var $_const = function($_a){return function($_b){return new $(function(){var a = $_a;return a;});};};var length = function($_a){return new $(function(){var $_$_a = _($_a);if ($_$_a instanceof Fay$$Cons) {var x = $_$_a.car;var xs = $_$_a.cdr;return 1 + _(_(length)(xs));}if (_($_a) === null) {return 0;}throw ["unhandled case in Ident \"length\"",[$_a]];});};var mod = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["double"],Fay$$fayToJs(["double"],$_a) % Fay$$fayToJs(["double"],$_b));});};};var min = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["double"],Math.min(Fay$$fayToJs(["double"],$_a),Fay$$fayToJs(["double"],$_b)));});};};var max = function($_a){return function($_b){return new $(function(){return Fay$$jsToFay(["double"],Math.max(Fay$$fayToJs(["double"],$_a),Fay$$fayToJs(["double"],$_b)));});};};var Fay$$fayToJsUserDefined = function(type,obj){var _obj = _(obj);var argTypes = type[2];if (_obj instanceof $_EQ) {return {"instance": "EQ"};}if (_obj instanceof $_LT) {return {"instance": "LT"};}if (_obj instanceof $_GT) {return {"instance": "GT"};}if (_obj instanceof $_Nothing) {return {"instance": "Nothing"};}if (_obj instanceof $_Just) {return {"instance": "Just","slot1": Fay$$fayToJs(["unknown"],_(_obj.slot1))};}if (_obj instanceof $_SomeKey) {return {"instance": "SomeKey","slot1": Fay$$fayToJs(["double"],_(_obj.slot1))};}if (_obj instanceof $_KeyRet) {return {"instance": "KeyRet"};}if (_obj instanceof $_KeyDown) {return {"instance": "KeyDown"};}if (_obj instanceof $_KeyUp) {return {"instance": "KeyUp"};}return obj;};var Fay$$jsToFayUserDefined = function(type,obj){if (obj["instance"] === "EQ") {return new $_EQ();}if (obj["instance"] === "LT") {return new $_LT();}if (obj["instance"] === "GT") {return new $_GT();}if (obj["instance"] === "Nothing") {return new $_Nothing();}if (obj["instance"] === "Just") {return new $_Just(Fay$$jsToFay(["unknown"],obj["slot1"]));}if (obj["instance"] === "SomeKey") {return new $_SomeKey(Fay$$jsToFay(["double"],obj["slot1"]));}if (obj["instance"] === "KeyRet") {return new $_KeyRet();}if (obj["instance"] === "KeyDown") {return new $_KeyDown();}if (obj["instance"] === "KeyUp") {return new $_KeyUp();}return obj;};
// Exports
this.max = max;
this.min = min;
this.mod = mod;
this.length = length;
this.$_const = $_const;
this.mapM_ = mapM_;
this.forM_ = forM_;
this.intercalate = intercalate;
this.prependToAll = prependToAll;
this.intersperse = intersperse;
this.lookup = lookup;
this.foldl = foldl;
this.foldr = foldr;
this.concat = concat;
this.conc = conc;
this.$36$ = $36$;
this.$43$$43$ = $43$$43$;
this.$46$ = $46$;
this.maybe = maybe;
this.flip = flip;
this.zip = zip;
this.zipWith = zipWith;
this.enumFromTo = enumFromTo;
this.enumFrom = enumFrom;
this.when = when;
this.insertBy = insertBy;
this.sortBy = sortBy;
this.compare = compare;
this.sort = sort;
this.elem = elem;
this.nub$39$ = nub$39$;
this.nub = nub;
this.map = map;
this.$_null = $_null;
this.not = not;
this.filter = filter;
this.any = any;
this.find = find;
this.fst = fst;
this.snd = snd;
this.fromRational = fromRational;
this.fromInteger = fromInteger;
this.show = show;
this.foo = foo;
this.main = main;

// Built-ins
this._ = _;
this.$           = $;
this.$fayToJs    = Fay$$fayToJs;
this.$jsToFay    = Fay$$jsToFay;

};
;
var main = new Main();
main._(main.main);


