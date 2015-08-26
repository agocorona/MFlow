{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Main where

import           MFlow.Wai.Blaze.Html.All hiding (main)
import MFlow.Forms.Internals

data SiteSection
  = Products
  | About
  | Contacts
  | Cart
  | User
  | Admin
  deriving (Show)


main :: IO ()
main = runNavigation "store" (step storeWebApp)

storeWebApp :: FlowM Html IO ()
storeWebApp =
  do userRegister "heraldhoi@gmail.com" "admin"
     siteSections

siteSections :: FlowM Html IO ()
siteSections =
  do mainSection
     aboutSection
     contactsSection
     productsSection
     return ()

mainSection, productsSection, aboutSection, contactsSection
  :: forall a. FlowM Html IO a

mainSection     = mainLayout $ h1 "


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Main where

import           MFlow.Wai.Blaze.Html.All hiding (main)
import MFlow.Forms.Internals

data SiteSection
  = Products
  | About
  | Contacts
  | Cart
  | User
  | Admin
  deriving (Show)


main :: IO ()
main = runNavigation "store" (step storeWebApp)

storeWebApp :: FlowM Html IO ()
storeWebApp =
  do userRegister "heraldhoi@gmail.com" "admin"
     siteSections

siteSections :: FlowM Html IO ()
siteSections =
  do mainSection
     aboutSection
     contactsSection
     productsSection
     return ()

mainSection, productsSection, aboutSection, contactsSection
  :: forall a. FlowM Html IO a

mainSection     = mainLayout $ h1 "