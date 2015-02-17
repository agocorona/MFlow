[![Build Status](https://travis-ci.org/agocorona/MFlow.png?branch=master)](https://travis-ci.org/agocorona/MFlow)

See the [MFlow demo and documentation site](http://mflowdemo.herokuapp.com)

# MFlow

MFlow is a web framework that turns the mess of web programming from handlers and configuration files back into sane and normal programming. Your code is the website.

MFlow Web applications are much like console applications. You just write an ordinary sequential program with inputs, outputs and control statements where the inputs and outputs are web pages. MFlow will run the sequence forward and backward depending on your input to find the appropriate location in the sequence to respond your query.

MFlow uses Haskell magic to counteract the wicked magic of web programming. It terrifies the programmer with all their explosion of events, configurations, plumbing, lookups, and identifiers. MFlow restores Web programming to the intuitive and natural way, without inversion of control.

## The goals of MFlow are:

-Inverting back the inversion of control of web applications and turn web programming into ordinary, intuitive, imperative-like programming.

-At the same time, maintaining for the programmer all the freedom that they have in web applications. MFlow keeps out of your way.

-For scalability-sensitive applications, no fat state snapshots that other continuation-based frameworks need to cope with these two previous requirements. State replication and horizontal scalability are central to MFlow's philosophy.

-For REST advocates, MFlow maintains the elegant notation of REST URLs and the statelessness of GET requests.

-For expert Haskell programmers, reuse of already existing web libraries and techniques is trivial.

-For beginner programmers and for software engineers, MFlow provides a high level DSL of reusable and self contained widgets for the user interface, and multipage procedures that work together provided that they statically typecheck with zero configuration.

-For highly interactive applications, MFlow gives dynamic widgets that have their own behaviors in the page and they communicate without the need of explicit JavaScript programming.

-No deployment, speed up the development process. see <http://haskell-web.blogspot.com.es/2013/05/a-web-application-in-tweet.html>

## How navigation works:

MFlow solves the problem of re-inversion of control by using a different approach, routes are expressed as normal monadic Haskell code in the FlowM monad, local links point to alternative routes within this monadic computation, this means any GET page is directly reachable by means of a RESTful URL.

At any moment the flow can respond to the back button or to any RESTful path that the user may paste in the navigation bar. If the procedure is waiting for another different page, the FlowM monad backtrack until the paths partially match. From this position on the execution goes forward until the rest of the paths match. Thus, no matter the previous state of the server process it will recover the state of execution appropriate for the request. This way the server process is virtually stateless for any GET request. It is also possible to store a session state, which may backtrack or not, when the navigation goes back and forth. MFlow keeps it all in sync, synchronization between server state and web browser state is supported out-of-the-box.

When the state matters, and user interactions can last for long period of time, such are shopping carts etc., MFlow uses a log for thread state persistence. The server process shuts itself down after a programmer defined timeout, once a new request of the same user arrives, the log is used to recover the process state. There is no need to store a snapshot of every continuation, just the result of each step. This solves the problem of fat state snapshots and give a very lightweight way to handle state.

## Data tier:

State consistency and transactions are handled by the TCache package.

http://hackage.haskell.org/package/TCache

It is data cache within the STM monad (Software Transactional Memory), serialization and deserialization of data is programmer defined. TCache can adapt to any database, default persistence in files comes out of the box for rapid development purposes, but you can switch to a variety of backends when needed, see the database examples for more details.

## Widgets:

The processes interact through widgets, which are an extension of formlets with additional applicative combinators, formatting, link management, callbacks, modifiers, caching, and AJAX. All of it is coded in pure Haskell and you use pure Haskell to make your own widgets. Each widget return statically typed data, they can dynamically modify themselves using AJAX internally (just prefix it with auto refresh), are auto-contained: they may include their own JavaScript code, server code and client code in a single pure Haskell procedure that can be combined with other widgets with no other configuration.

To combine widgets, applicative combinators are used. Widgets with dynamic behaviors that use the monadic syntax and callbacks. When you combine widgets, everything is type checked and making large websites by gluing together small reusable building blocks is the entire aim of the MFlow project.

## Modularity:

The interfaces and communications are abstract and there are bindings for blaze-HTML, HSP, Text.XHtml, ByteString, Hack, and WAI. So it can be extended to non web based architectures.

Bindings for hack, and HSP >= 0.8,  are not installed by default, but are included in the package files. To use them, add them to the exported modules and execute cabal install

It is designed for applications that can be run with no deployment with runghc in order to speed up the development process. see <http://haskell-web.blogspot.com.es/2013/05/a-web-application-in-tweet.html>

## Features:

* Push widgets: http://haskell-web.blogspot.com.es/2013/07/maxwell-smart-push-counter.html
* Complete execution traces for errors: http://haskell-web.blogspot.com.es/2013/07/automatic-error-trace-generation-in.html
* RESTful URLs: http://haskell-web.blogspot.com.es/2013/07/the-web-navigation-monad.html
* Automatic, independent refreshing of widgets via Ajax. (see http://haskell-web.blogspot.com.es/2013/06/and-finally-widget-auto-refreshing.html) 
* Besides applicative syntax (declarative-like) each widget can use the monadic syntax (imperative-like) so widgets can express their own behavior and can run its own independent page flow. (see http://haskell-web.blogspot.com.es/2013/06/the-promising-land-of-monadic-formlets.html)
* Per-widget callbacks, used in page flows, that change the rendering of the widget (see http://haskell-web.blogspot.com.es/2013/06/callbacks-in-mflow.html)
* Widgets in modal and non modal dialogues (using jQuery dialog)
* Other jQuery widgets as MFlow widgets
* WAI integration
* Content management and multilanguage
* blaze-html support
* Ajax
* User-defined data in sessions
* Widget requirements for automatic installation of scripts, CSS and server flows.
* Transparent back button management
* Cached widgets
* Callbacks
* Lazy load of widgets
* Web Services
