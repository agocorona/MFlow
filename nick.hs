{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Main
where
import MFlow.Wai.Blaze.Html.All hiding(main)
import qualified MFlow.Forms as F
import Data.Typeable
import System.IO.Unsafe
import Control.Workflow
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger
import Control.Monad (liftM)
import Data.Monoid

--
queryBusinessClient :: FlowM Html IO ()
queryBusinessClient = do
  -- HTML
  lastName <- page  
    $   getString Nothing <! hint "Last Name" 
    <++ br
    <** submitButton "Submit"

  page $ wlink () << b  "Here2"

  page 
    $   b << ("hey oh"::String)
    ++> br 
    ++> wlink () << b  "click here"


  where
  hint x = [("placeholder", x)]


site :: FlowM Html IO ()
site = do
  -- login

  queryBusinessClient
  
  -- logout

  where
  login = do 
    r <- page 
      $   h3 << ("Login"::String)
      ++> userWidget Nothing userLogin
    return r
  logout = do 
    page
      $   wlink () << b  "Logout"
    F.logout



main :: IO ()
main = do 
  setAdminUser ("nickgeoca"::String) (""::String)
  runNavigation "" . transientNav $ do     -- TODO: Add TLS here with runSecureNavigation
      site 