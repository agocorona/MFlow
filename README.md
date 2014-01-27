[![Build Status](https://travis-ci.org/agocorona/MFlow.png?branch=master)](https://travis-ci.org/agocorona/MFlow)

See the [MFlow demo and documentation site](http://mflowdemo.herokuapp.com)

# MFlow

[![Build Status](https://travis-ci.org/sheganinans/MFlow.png)](https://travis-ci.org/sheganinans/MFlow)


MFlow is simply the most high level, most advanced and fun to program web framework.  MFlow uses Haskell magic to counteract the wicked Web programming magic, that terrifies the programmer with all their explosion of events, configurations, plumbing  lookups and identifiers, in order to restore programming to be intuitive and natural again. Everything in MFlow that seems sophisticated is to solve a real problem at hand, not a fancy way to hide with new names the inherent flawed nature of the MVC model when used in real web applications.

MFlow Web applications are much like console applications. You just write an ordinary sequential program with inputs, outputs and control statements where the inputs and outputs are web pages. MFlow will run the sequence forward and backward depending on your input to find the appropriate location in the sequence to respond your query. 

MFlow works just in the same way people would read a cooking recipe: Each person look for instructions forward or backward until they find the correct point in the sequence appropriate for his state in the cooking process. To know his state, the people remember the name of the steps already done, but not the details of each step. That is exactly what MFlow does. All is pure tracking, backtracking and event logging. 

Other frameworks try to do it with heavy page state or execution state snapshots That is too bad for scalability and this has limited the acceptance of this model for large scale web applications. 

Additionally MFlow has nice unique features for the creation of rich and dynamic applications: widgets can exhibit dinamic behaviours and refresh independently on their own without using explicit javaScript. The code for these dynamic widgets are, again much like console applications. With a few changes, a multi-page application can be converted into a dynamic auto-refreshed widget that can be inserted in a page along with others.

MFlow is the only Web Framework that uses matching of requests and true backtracking as the mechanism for browser-server synchronization. Each link or form in the page return type safe responses. The navigation is also type safe since it is encoded within a monadic procedure. The server process ever synchronize with the browser request. There are no sequence errors. The page is composed of reusable type-safe components called widgets that may change their rendering depending o the user responses by auto refreshing themselves  without using JavaScript.

Since the navigation is coded as a normal procedure under the navigation monad, any navigation sequence can be reusable. Deployment and configuration is reduced to zero. The elements can work together if they type-check. In the examples you will see different ways to combine components: either widgets inside widgets, different widgets in a page or complete application flows called as normal procedures in a program.

## Goals:

-To invert back the inversion of control of web applications and turn web programming into ordinary, intuitive, imperative-like, programming, as seen by the programmer. 

-At the same time, to maintain for the user all the freedom that he has in web applications. Back buttons, bookmarked URLs must work.

-For scalability-sensitive applications, to avoid the fat state snapshots that continuation based frameworks need to cope with these two previous requirements. State replication and horizontal scalability must be possible.

-For REST advocates, to maintain the elegant notation of REST URLs and the statelessness of GET requests. 

-For expert haskell programmers, to reuse the already existent web libraries and techniques. 

-For beginner programmers and for Software Engineers, to provide with a high level DSL of reusable, self contained widgets for the user interface, and multipage procedures that can work together provided that they statically typecheck, with zero configuration. 

-For highly interactive applications, to give dynamic widgets that have their own dynamic behaviors in the page, and communicate themselves without the need of explicit  JavaScript programming. 

## How navigation works:

MFlow solves the first requirements using an innovative approach. The routes are expressed as normal, monadic haskell code in the FlowM monad. Local links point to alternative routes within this monadic computation just like a textual menu in a console application. Any GET page is directly reachable by means of a RESTful URL.

At any moment the flow can respond to the back button or to any RESTful path that the user may paste in the navigation bar. If the procedure is waiting for another different page, the FlowM monad backtrack until the path partially match. From this position on, the execution goes forward until the rest of the path match.  Thus, no matter the previous state of the server process, it recover the state of execution appropriate for the request. This way the server process is virtually stateless for any GET request. However, it is possible to store a session state, which may backtrack or not when the navigation goes back and forth. It is up to the programmer. Synchronization between server state and web browser state is supported out-of-the-box.

When the state matters, and user interactions can last for long, such are shopping carts, it uses a log for thread state persistence. The server process shut itself down after a programmer defined timeout. Once a new request of the same user arrive, the log is used to recover the process state. There is no need to store a snapshot of every continuation, just the result of each step.

## Data tier:

State consistence and transactions are given by the TCache package. 

http://hackage.haskell.org/package/TCache

It is data cache within the STM monad (Software Transactional Memory).  Serialization and deserialization of data is programmer defined, so it can adapt it to any database, although any other database interface can be used. Default persistence in files comes out of the box for rapid development purposes.

##  Widgets:

The processes interact trough widgets, that are an extension of formlets with additional applicative combinators , formatting, link management, callbacks, modifiers, caching and AJAX. All is coded in pure haskell. Each widget return statically typed data. They can dynamically modify themselves using AJAX internally (Just prefix it with autorefresh). They are auto-contained: they may include their own JavaScript code, server code and client code in a single pure Haskell procedure that can be combined with other widgets with no other configuration.

To combine widgets, applicative combinators are used. Widgets with dynamic behaviours can use the monadic syntax and callbacks.

## Modularity:

The interfaces and communications are abstract, but there are bindings for blaze-html, HSP, Text.XHtml and byteString, Hack and WAI but it can be extended to non Web based architectures.

Bindings for hack, and hsp >= 0.8,  are not compiled by Hackage, and do not appear, but are included in the package files. To use them, add then to the exported modules and execute cabal install

It is designed for applications that can be run with no deployment with runghc in order to speed up the development process. see <http://haskell-web.blogspot.com.es/2013/05/a-web-application-in-tweet.html>

## Features:

* Push widgets: http://haskell-web.blogspot.com.es/2013/07/maxwell-smart-push-counter.html 
* Complete execution traces for errors: http://haskell-web.blogspot.com.es/2013/07/automatic-error-trace-generation-in.html 
* RESTful URLs: http://haskell-web.blogspot.com.es/2013/07/the-web-navigation-monad.html 
* Automatic, independent refreshing of widgets via Ajax. (see http://haskell-web.blogspot.com.es/2013/06/and-finally-widget-auto-refreshing.html) 
* Besides aplicative syntax (declarative-like) each widget can use the monadic syntax (imperative-like) so widgets can express their own behaviour and can run its own independent page flow. (see http://haskell-web.blogspot.com.es/2013/06/the-promising-land-of-monadic-formlets.html) 
* Per-widget callbacks, used in page flows, that change the rendering of the widget (see http://haskell-web.blogspot.com.es/2013/06/callbacks-in-mflow.html) 
* Widgets in modal and non modal dialogs (using jQuery dialog) 
* Other jQuery widgets as MFlow widgets 
* WAI integration
* Basic content management and multilanguage
* blaze-html support
* Ajax
* User-defined data in sessions 
* Widget requirements for automatic installation of scripts, CSS and server flows. 
* Transparent back button management
* Cached widgets
* Callbacks 


