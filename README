MFlow is am application server and Web Framework for stateful and persistent Web applications that run in a navigation monad with backtracking that provides services of routing and state persistence. 

Each link or form in the page return type safe responses. The navigation is also type safe since it is a monadic procedure with type-safe components. The page is composed of reusable type-safe components called widgets that may change their rendering depending o the user responses by autorefreshing, without using JavaScript.

Since the navigation is monadic, any navigation sequence can be reusable. Deployment and configuation is reduced to zero. The elements can work together if they typecheck.

# Goals:

-To invert back the inversion of control of web applications and turn web programming into ordinary, intuitive, imperative-like, programming as seen by the programmer. 

-At the same time, to maintain for the user all the freedom that he has in web applications. 

-For scalability-sensitive applications, to avoid the fat state snapshots that continuation based frameworks need to cope with these two previous requirements. State replication and Horizontal scalability must be possible.

-For REST advocates, to maintain the elegant notation of REST urls and the statelessness of GET requests. 

-For expert haskell programmers, to reuse the already existent web libraries and techniques. 

-For beginner programmers and for Software Enginners, to provide with a high level DSL of reusable, self contained widgets for the user interface, and multipage procedures that can work together provided that they statically typecheck, with zero configuration. 

-For highly interactive applications, to give dynamic widgets that have their own dynamic behaviors in the page, and communicate themselves without the need of explicit  JavaScript programming. 

# How it works:
MFlow try to solve the first requirements using an innovative approach. The routes are expressed as normal, monadic haskell code in the FlowM monad. Local links point to alternative routes within this monadic computation just like a textual menu in a console application. Any GET page is directly reachable by means of a RESTful URL.

At any moment the flow can respond to the back button or to any RESTful path that the user may paste in the navigation bar. If the procedure is waiting for another different page, the FlowM monad backtrack until the path partially match. From this position on, the execution goes forward until the rest of the path match.  Thus, no matter the previous state of the server process, it recover the state of execution appropriate for the request. This way the server process is virtually stateless for any GET request. However, it is possible to store a session state, which may backtrack or not when the navigation goes back and forth. It is up to the programmer. Synchronization between server state and web browser state is supported out-of-the-box.

When the state matters, and user interactions can last for long, such are shopping carts, it uses a log for thread state persistence. The server process shut itself down after a programmer defined timeout. Once a new request of the same user arrive, the log is used to recover the process state. There is no need to store a snapshot of every continuation, just the result of each step.

# Data tier:

State consistence and transactions are given by the TCache package. 

*   http://hackage.haskell.org/package/TCache

It is data cache within the STM monad (Software Transactional Memory).  Serialization and deserialization of data is programmer defined, so it can adapt it to any database, although any other database interface can be used. Default persistence in files comes out of the box for rapid development purposes.

#  Widgets:

The processes interact trough widgets, that are an extension of formlets with additional applicative combinators , formatting, link management, callbacks, modifiers, caching and AJAX. All is coded in pure haskell. Each widget return statically typed data. They can dynamically modify themselves using AJAX internally (ust prefix it with autorefresh). They are auto-contained: they may include their own JavaScript code, server code and client code in a single pure Haskell procedure that can be combined with other widgets with no other configuration.

To combine widgets, applicative combinators are used. Widgets with dynamic behaviours can use the monadic syntax and callbacks.

# Modularity:
The interfaces and communications are abstract, but there are bindings for blaze-html, HSP, Text.XHtml and byteString, Hack and WAI but it can be extended to non Web based architectures.

Bindings for hack, and hsp >= 0.8,  are not compiled by Hackage, and do not appear, but are included in the package files. To use them, add then to the exported modules and execute cabal install

It is designed for applications that can be run with no deployment with runghc in order to speed up the development process. see <http://haskell-web.blogspot.com.es/2013/05/a-web-application-in-tweet.html>

# Features:

• Push widgets: http://haskell-web.blogspot.com.es/2013/07/maxwell-smart-push-counter.html 
• Complete execution traces for errors: http://haskell-web.blogspot.com.es/2013/07/automatic-error-trace-generation-in.html 
•RESTful URLs: http://haskell-web.blogspot.com.es/2013/07/the-web-navigation-monad.html 
• Automatic, independent refreshing of widgets via Ajax. (see http://haskell-web.blogspot.com.es/2013/06/and-finally-widget-auto-refreshing.html) 
• Besides aplicative syntax each widget can use the monadic idiom so it can express his own behaviour and can run its own independent page flow. (see http://haskell-web.blogspot.com.es/2013/06/the-promising-land-of-monadic-formlets.html) 
• Per-widget callbacks, used in page flows, that change the rendering of the widget (see http://haskell-web.blogspot.com.es/2013/06/callbacks-in-mflow.html) 
• Widgets in modal and non modal dialogs (using jQuery dialog) 
• Other jQuery widgets as MFlow widgets 
• WAI integration
• Basic content management and multilanguage
• blaze-html support
• Ajax
• User-defined data in sessions 
• Widget requirements for automatic installation of scripts, CSS and server flows. 
• Transparent back button management
• Cached widgets
• Callbacks 

See MFlow.Forms for details 

To do: 
•Clustering 
