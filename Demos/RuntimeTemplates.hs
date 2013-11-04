-- | example of storage and query by using tcache
{-# OPTIONS -XDeriveDataTypeable -XNoMonomorphismRestriction  #-}
module RuntimeTemplates where
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery
import Control.Workflow.Configuration
import MFlow.Wai.Blaze.Html.All hiding(name, select, base)
import Data.Typeable
import Data.Monoid
import Data.ByteString.Lazy.Char8 as BS hiding (index)
import Control.Exception(SomeException)
import Control.Monad (when)

import Menu
import Debug.Trace
(!>) = flip trace

--pagem= page
--main= do
----    index name    -- index the name field, so I can query  for it later
--    syncWrite  $ Asyncronous 10 defaultCheck  1000
--    runNavigation "" $  runtimeTemplates


data  MyData= MyData{name :: String} deriving (Typeable,Read, Show)  

instance Indexable MyData where  key=  name     -- just to notify what is the key of the register

instance Serializable MyData where    -- default persistence
   serialize=  pack . show
   deserialize=   read . unpack
   setPersist =   \_ -> Just filePersist      --  in files

data Options= NewName | ListNames  | Exit deriving (Show, Typeable,Eq)

newtype NextReg = NextReg Int deriving Typeable

runtimeTemplates= do
     -- runConfiguration is usedd to create some example registers
     liftIO $ runConfiguration "createDataforTemplates" $ do
             ever $ index name  -- index the name field, so I can query  for it later
                         -- better, should put this sentence in main

             -- create ['a'..'z'] entries. Only the first time, since step
             -- runConfiguration prevent the execution of this again
             once $ atomically $ mapM_ (newDBRef . MyData . return) ['a'..'z']
     process

process= do                     
     r <- pagem $ edTemplate "edituser" "menuallnames"
                $ wlink NewName   << p << "enter a new name"
              <|> wlink ListNames << p << "List names"
              <|> wlink Exit << p << "Exit to the home page"

     case r of
         NewName -> do
              pagem  $ edTemplate "edituser" "enterallnames"
                     $ pageFlow "enter" $ witerate (
                            do  name <- dField (getString Nothing `validate` jsval)
                                      <** submitButton "ok" <++ br
                                -- store the register in the cache
                                -- Later will be written by the default persistence automatically
                                liftIO . atomically . newDBRef $ MyData name   
                        **> do
                             n <- countRegisters
                             dField (wraw $ b << show n) <++ fromStr " registers added ")
                   **> wlink () << b << "exit"

         ListNames -> do
              -- query for all the names stored in all the registers
              allnames <- getAllNames
              let len= Prelude.length allnames
              
              pagem $  iterateResults allnames len
                  **> wlink "templ" << p << "click here to show the result within a runtime template"

              setSessionData $ NextReg 0
              pagem $  edTemplate "edituser" "listallnames"
                    $  iterateResults allnames len
                  **> wlink "scroll" << p << "click here to present the results as a on-demand-filled scroll list"

              setSessionData $ NextReg 0
              pagem $ appendUpdate (do
                          NextReg ind <- getSessionData `onNothing` return (NextReg 0)
                          getData ind     len allnames <++ br
                          getData (ind+1) len allnames <++ br
                          getData (ind+2) len allnames <++ br
                          getData (ind+3) len allnames <++ br
                          setSessionData . NextReg $ next ind len
                          wlink "more" << b << "more" <++ br)

                  **> wlink () << p << "click here to go to the menu"

     if r == Exit then return()  else process


     where
     getAllNames= liftIO . atomically $ select name $ name .>.  ""

     countRegisters= getAllNames >>= return . Prelude.length

     iterateResults allnames len = pageFlow "iter" $ witerate $ do
              NextReg ind <- getSessionData `onNothing` return (NextReg 0)
              dField (getData ind     len allnames) <++ br
              dField (getData (ind+1) len allnames) <++ br
              dField (getData (ind+2) len allnames) <++ br
              dField (getData (ind+3) len allnames) <++ br
              setSessionData . NextReg $ next ind len
              r <- wlink True  << b << "next" <++ fromStr " "
                   <|>
                   wlink False << b << "prev"
              when (r== False) $ setSessionData . NextReg $  prev ind len

     getData i len all= wraw $ fromStr $  if i>= len || i < 0 then "" else all !! i
     next i len = case i > len  of  True -> 0 ; _ -> i + 4
     prev i len = case i < 0 of True -> len; _ -> i - 4

     jsval s=
         if Prelude.length s > 10 then do
           return $ Just  $ b << "length must be less than 10 chars"
         else return Nothing
