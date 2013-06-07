{-# OPTIONS -XDeriveDataTypeable
            -XMultiParamTypeClasses -XRecordWildCards

            #-}
module Main where
import MFlow.Hack.XHtml.All

import Data.Typeable
import Control.Monad.Trans
import qualified Data.Vector as V





main= do
   userRegister  "pepe" "pepe"
   putStrLn $ options messageFlows
   run 80 $ hackMessageFlow messageFlows
   where
   messageFlows=  [("main",  runFlow mainProds)
                  ,("hello", stateless hello)]

   options msgs= "in the browser choose\n\n" ++
     concat [ "http://server/"++ i ++ "\n" | (i,_) <- msgs]



-- an stateless procedure, as an example
hello :: Env -> IO String
hello env =  return  "hello, this is a stateless response"


data Prod= Prod{pname :: String, pprice :: Int} deriving (Typeable,Read,Show)

-- formLets can have Html formatting
instance FormLet Prod IO Html where
   digest mp= table <<< (
      Prod <$> tr <<< (td << "enter the name"  <++ td <<< getString (pname <$> mp))
           <*> tr <<< (td << "enter the price" <++ td <<< getInt ( pprice <$> mp)))


-- Here an example of predefined widget (`Selection`) that return an Int, combined in the same
-- page with the fromLet for the introduction of a new product.
-- The result of the interaction with the user is either one or the other result

shopProds :: V.Vector Int -> [Prod]
          -> View Html IO  (Either Int Prod)
shopProds cart products=
  br
  <++                    -- add Html to the first widget
  p << "-----Shopping List-----"
  <++
  widget(Selection{
       stitle = bold << "choose an item",
       sheader= [ bold << "item"   , bold << "price", bold << "times chosen"],
       sbody= [([toHtml pname, toHtml $ show pprice, toHtml $ show $ cart V.! i],i )
              | (Prod{..},i ) <- zip products [1..]]})

  <+>                    -- operator to mix two wdigets

  br
  <++                    -- add Html to the second widget
  p << "---Add a new product---"
  <++
  table <<<              -- <<< encloses a widget in HTML tags
            (tr <<< td ! [valign "top"]
                          <<< widget (Form (Nothing :: Maybe Prod) )

             ++>         -- append Html after the widget

             tr << td ! [align "center"]
                          << hotlink  "hello"
                                      (bold << "Hello World"))

-- the header

appheader user forms= thehtml
         << body << dlist << (concatHtml
            [dterm <<("Hi "++ user)
            ,dterm << "This example contains two forms enclosed within user defined HTML formatting"
            ,dterm << "The first one is defined as a Widget, the second is a formlet formatted within a table"
            ,dterm << "both are defined using an extension of the FormLets concept"
            ,dterm << "the form results are statically typed"
            ,dterm << "The state is implicitly logged. No explicit handling of state"
            ,dterm << "The program logic is written as a procedure. Not    in request-response form. But request response is possible"
            ,dterm << "lifespan of the serving process and the execution state defined by the programmer"
            ,dterm << "user state is  automatically recovered after cold re-start"
            ,dterm << "transient, non persistent states possible."
            ])
            +++ forms

-- Here the procedure. It ask for either entering a new product
-- or to "buy" one of the entered products.
-- There is a timeout of ten minutes before the process is stopped
-- There is a timeout of one day for the whole session so after this, the
-- user will see the list  or prudicts erased.
-- In a real application the product list should be stored out of the session
-- using TCache's writeDBRef for example
-- The state is user specific.

mainProds ::  FlowM Html (Workflow IO) ()
mainProds   = do
   setTimeouts (10*60) (24*60*60)
   setHeader $ \w -> bold << "Please enter user/password (pepe/pepe)" +++ br +++ w
   us <-  getUser

   setHeader  $ appheader  us
   mainProds1 [] $ V.fromList [0]
   where
   mainProds1  prods cart=  do
     mr <- step . ask  $ shopProds  cart prods
     case mr of
      Right prod -> mainProds1  (prod:prods) (V.snoc cart 0)
      Left i   -> do
         let newCart= cart V.// [(i, cart V.! i + 1 )]
         mainProds1 prods newCart


