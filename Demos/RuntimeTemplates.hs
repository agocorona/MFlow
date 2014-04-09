-- | example of runtime templates, storage and query by using tcache
{-# OPTIONS -XDeriveDataTypeable -XNoMonomorphismRestriction  -XCPP #-}
module RuntimeTemplates where
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery
import Control.Workflow.Configuration
import Data.Typeable
import Data.Monoid
import Data.ByteString.Lazy.Char8 as BS hiding (index)
import Control.Exception(SomeException)
import Control.Monad (when)

-- #define ALONE -- to execute it alone, uncomment this

-- Note:  there are static links in templates in the main menu. unless they
-- are updated using  witerate/dField, the main menu of this example will not
-- work when the app run alone since the expected paths are different.
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All  hiding(name,select,base)
import Debug.Trace
(!>) = flip trace
main= runNavigation "" $ transientNav runtimeTemplates
#else
import MFlow.Wai.Blaze.Html.All hiding(name,select,base, page)
import Menu
#endif



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

             -- create ['a'..'z'] entries. Only the first time
             -- runConfiguration prevent the execution of this again
             once $ atomically $ mapM_ (newDBRef . MyData . return) ['a'..'z']
     process

process= do
     setSessionData $ NextReg 0  
     r <- page  $  edTemplate "edituser" "menuallnames" 
                $ wlink NewName   << p << "enter a new name"
              <|> wlink ListNames << p << "List names"
              <|> wlink Exit      << p << "Exit to the home page"

     case r of
         Exit -> return()
         NewName -> do
              page  $ edTemplate "edituser" "enterallnames"
                    $ pageFlow "enter"
                    $ witerate (
                            do  noCache
                                name <- dField (getString Nothing `validate` jsval)
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

              page $ pageFlow "iter" $ iterateResults allnames len
                 **> wlink "templ" << p << "click here to show the result within a runtime template"

              setSessionData $ NextReg 0
              page $ edTemplate "edituser" "listallnames"
                   $ pageFlow "iter" $ iterateResults allnames len
                 **> wlink "scroll" << p << "click here to present the results as a on-demand-filled scroll list"

              setSessionData $ NextReg 0
              page $ pageFlow "upd" $ appendUpdate ( do
                          NextReg ind <- getSessionData `onNothing` return (NextReg 0)
                          getData ind     len allnames <++ br
                          getData (ind+1) len allnames <++ br
                          getData (ind+2) len allnames <++ br
                          getData (ind+3) len allnames <++ br
                          setSessionData . NextReg $ next ind len
                          wlink "more" << b << "more" <++ br)

                  **> wlink () << p << "click here to go to the menu"

     if r == Exit then return() else process

     where
     getAllNames= liftIO . atomically $ select name $ name .>.  ""

     countRegisters= (getAllNames >>= return . Prelude.length)


     iterateResults allnames len = witerate $ do
          maxAge 300
          NextReg ind <- getSessionData `onNothing` return (NextReg 0)
          dField(getData  ind    len allnames) <++ br
          dField(getData (ind+1) len allnames) <++ br
          dField(getData (ind+2) len allnames) <++ br
          dField(getData (ind+3) len allnames) <++ br
          r <-    dField (wlink (next ind len) << b << "next" ) <++ fromStr " "
              <|> dField (wlink (prev ind len) << b << "prev")
              <|> restp
          setSessionData $ NextReg r


     getData i len all=  wraw . fromStr $ if i >= len || i < 0 then "" else all !! i
     next i len = case i > len  of  True -> 0 ; _ -> i + 4
     prev i len = case i < 0 of True -> len; _ -> i - 4

     jsval s=
         if Prelude.length s > 10 then do
           return $ Just  $ b << "length must be less than 10 chars"
         else return Nothing
