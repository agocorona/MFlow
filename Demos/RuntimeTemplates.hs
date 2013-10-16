-- | example of storage and query by using tcache
{-# OPTIONS -XDeriveDataTypeable  #-}
module RuntimeTemplates where
import Data.TCache.DefaultPersistence
import Data.TCache.IndexQuery
import MFlow.Wai.Blaze.Html.All hiding(name, select, base)
import Data.Typeable
import Data.Monoid
import Data.ByteString.Lazy.Char8 as BS hiding (index)
import Control.Exception(SomeException)

import Menu

--pagem= page
--main= do
----    index name    -- index the name field, so I can query  for it later
--    syncWrite  $ Asyncronous 10 defaultCheck  1000
--    runNavigation "" $ step  runtimeTemplates


data  MyData= MyData{name :: String} deriving (Typeable, Read, Show)  -- that is enough for file persistence

instance Indexable MyData where  key=  name     -- just to notify what is the key of the register

data Options= NewName | ListNames  | Exit deriving (Show, Typeable,Eq)


runtimeTemplates= do
     liftIO $ index name  -- index the name field, so I can query  for it later
                          -- better, should put this sentence in main
     r <- pagem $ edTemplate "edituser" "menuallnames"
                $ wcached "templatesMenu" 0 
                $ wlink NewName   << p << "enter a new name"
              <|> wlink ListNames << p <<  "List names"

     case r of
         NewName -> do
              name <- pagem  $ edTemplate "edituser" "enterallnames"
                          $ getString Nothing `validate` jsval
                        <** submitButton "ok"

              liftIO . atomically . newDBRef $ MyData name   -- store the name in the cache (later will be written to disk automatically)
              return()

         ListNames -> do
              -- query for all the names stored in all the registers
              allnames <- liftIO . atomically $ select name $ name .>.  ""
              pagem $   tFieldEd "edituser" "headerListNames" (p << "header")
                    **> edTemplateList "edituser"  "listNames"
                        [wraw . p $ fromStr name | name <- allnames]
                    **> wlink () << p << "click here to go to the menu"

     if r== Exit then return ()
                 else runtimeTemplates

     where
     -- simple alert valiation message.
     -- validation messages in presence of templates must be via jscript
     jsval s= if Prelude.length s > 10 then do
         requires[JScript "alert('length must be less than 10 chars');"]
         return $ Just mempty
         else return Nothing
