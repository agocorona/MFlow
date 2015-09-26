{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Main where


import           Data.TCache.DefaultPersistence
import qualified Data.TCache.IndexQuery         as Q

import           MFlow.Wai.Blaze.Html.All       hiding (Tag, User, contents,
                                                 div, id)
import qualified MFlow.Wai.Blaze.Html.All       as H

import qualified Data.ByteString.Lazy.Char8     as L8
import           Data.Monoid                    ((<>))
import           Data.Typeable                  (Typeable)


type F a = FlowM Html IO a -- ^ Flow of some data.
type W a = View Html IO a  -- ^ View of some data, that is the Widget.

data AppFlow
  = Navigate SiteSection
  deriving (Show, Typeable)


data SiteSection
  = Home
  | Products
  | About
  | Contacts
  | Cart
  | User
  | Management
  deriving (Show, Typeable)


data AdminAction
  = CreateCategory ProductCategory
  deriving (Show, Typeable)


data ProductCategory = ProductCategory
  { productCategoryName :: String
  , productCategoryDesc :: String }
  deriving (Show, Read, Ord, Eq, Typeable)


instance Indexable ProductCategory where
  key x = "Cat:" <> productCategoryName x

instance Serializable ProductCategory where
  serialize = L8.pack . show
  deserialize = read . L8.unpack


main :: IO ()
main = runNavigation "store" . step $ storeWebApp

storeWebApp :: FlowM Html IO ()
storeWebApp =
  do setTimeouts 120 (30 * 24 * 60 * 60)
     setupTemplate
     userRegister "admin" "admin"
     liftIO . Q.index $ productCategoryName
     r <- layout Home
     case r of
       Navigate c -> layout c
     return ()

layout :: SiteSection -> F AppFlow
-- | The page layout.  It includes:
--
-- * top navigational menu
-- * left side bar
-- * right side bar
-- * main contents of the page
-- * a footer
--
-- Takes a 'SiteSection' which contents will be presented.
layout = page . layoutWidget

layoutWidget :: SiteSection -> W AppFlow
layoutWidget s =
  topMenu
  <|> leftAside
  <|> mainBlock s
  <|> rightAside s
  <|> layoutFooter

-- | Site's top menu.
-- [ Link | Link | .. | Link ]
topMenu :: W AppFlow
topMenu = do
  usr <- getCurrentUser
  nav' (Navigate <$> (siteMenu usr))
  -- returns a list of links wrapped by <nav> HTML tag with "Top-Nav" class.
  where nav' x = (H.nav <<< x) `tagClass` "Top-Nav"

-- | Left Aside.
-- Nothing special here for now.
-- Just a dummy "LEFT" message.
leftAside :: W a
leftAside = p "LEFT" ++> noWidget

-- | Right Aside.
-- Presents a user widget (login/user form) with some dummy static content.
-- Takes a 'SiteSection', that is page to be shown after login/logout routine.
-- Currently, the idea is to simply refresh current page, and user form is shown on all site's pages.
rightAside :: SiteSection -> W AppFlow
rightAside s = authenticateWidget s <++ rightAsideContent

-- | Main Block.
-- A widget which presents contents of given page.
mainBlock :: SiteSection -> W AppFlow
mainBlock = contentsOf

-- | Dummy footer widget.
layoutFooter :: W a
layoutFooter = p "FOOTER" ++> noWidget

-- Returns a widget presenting contents of given page.
contentsOf :: SiteSection -> W AppFlow
contentsOf s =
  do -- some static data
     r <- htmlContents s
          ++> case s of
                -- some pages could allow some interaction with user via forms
                -- only manager's page is implemented now
                Management -> adminFlow
                _          -> noWidget
     return (Navigate r)
  where adminFlow =
          -- inner manager's page flow
          do r <- pageFlow "management" adminControls
             case r of
               CreateCategory nc -> createNewCategory nc
             noWidget
             -- return Management
        createNewCategory = liftIO . atomically . newDBRef


htmlContents :: SiteSection -> Html
htmlContents Home       = h1 "???????"
htmlContents Products   = h1 "?????????"
htmlContents About      = h1 "? ????????"
htmlContents Contacts   = h1 "????????"
htmlContents User       = h1 "??? ???????"
htmlContents Management = h1 "??????????"

rightAsideContent :: Html
rightAsideContent = p "RIGHT"

-- | Shows manager forms.  Currently there is only one form which allows to
-- create new categories.  Also shows a list of existing categories.
adminControls :: W AdminAction
adminControls =
  do -- read all stored categories
     lst <- liftIO . atomically . Q.indexOf $ productCategoryName
     -- present a form for category creation with rendered list of existing categories
     r <- wform $
            do pc <- ProductCategory <$> getString Nothing `hint` "????????"
                                     <*> getString Nothing `hint` "????????"
                                     <** submitButton "???????"
               return pc
          <++ categories lst
     return (CreateCategory r)
  where -- render a list of categories, only category name rendered
        categories cs = do p "????????? ???????"
                           mapM_ (\(x, _) -> p (fromStr x)) cs


-- | Returns menu depending on user.  Admins have special menu entries.
siteMenu :: String -> W SiteSection
siteMenu u =
  if userIsAdmin u
     then commonMenu <|> adminMenu
     else commonMenu
  where userIsAdmin "admin" = True
        userIsAdmin _       = False

-- | This widget presents a sequence of links avalible for all users.
commonMenu :: W SiteSection
commonMenu = absLink Home "???????"
             <|> absLink Products "?????????"
             <|> absLink About "? ????????"
             <|> absLink Contacts "????????"
             <|> absLink User "??? ???????"

-- | This widget presents site managers specific links.
adminMenu :: W SiteSection
adminMenu = absLink Management "??????????"

-- | Authentication widget.  Presents either login or logout form.
-- This widget returns valid result when actual login/logout action performed
-- to force page rerfesh, that is to guarantee update of all user depended
-- pieces of page.
authenticateWidget :: SiteSection -> W AppFlow
authenticateWidget s = wform $
  do uname <- getCurrentUser
     if uname == anonymous
        then loginForm s
        else logoutForm s uname

loginForm :: SiteSection -> W AppFlow
loginForm s =
  do (nm, pwd) <- (,) <$> getString    Nothing `hint` "?????"
                      <*> getPassword          `hint` "??????"
                      <** submitButton "?????"
     mr <- userValidate (nm, pwd)
     case mr of
       Nothing  -> invokeLogin s nm
       Just msg -> notValid msg

logoutForm :: SiteSection -> String -> W AppFlow
logoutForm s n =
  b << n ++> br ++> logoutLink
  where logoutLink = do submitButton "?????"
                        invokeLogout s

invokeLogin :: SiteSection -> String -> W AppFlow
invokeLogin s n =
  do login n
     clearEnv
     return (Navigate s)

invokeLogout :: SiteSection -> W AppFlow
invokeLogout s =
  do logout
     clearEnv
     return (Navigate s)

-- Add CSS styleheet link to head and setup outer wrap for all pages.
setupTemplate :: F ()
setupTemplate =
  do setFilesPath $
       "/Users/arthurfayzrakhmanov/"
       <> "Haskell/sites/mflow-fixture-store/src/Static/"
     setHeader $
       docTypeHtml . (html ! lang "ru") . body
     requires [CSSFile "style.css"]


tagClass :: W a -> String ->  W a
tagClass v c = v <! [("class", c)]

hint :: W a -> String -> W a
hint v h = v <! [("placeholder", h)]