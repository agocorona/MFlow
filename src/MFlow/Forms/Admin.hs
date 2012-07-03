{-# OPTIONS
            -XScopedTypeVariables

            #-}
module MFlow.Forms.Admin where
import MFlow.Hack
import MFlow.Forms
import MFlow.Forms.XHtml
import MFlow.Hack.XHtml
import MFlow
import Text.XHtml.Strict hiding (widget)
import Control.Applicative
import Control.Workflow
import Control.Monad.Trans
import Data.TCache
import Data.TCache.IndexQuery
import System.Exit
import System.IO
import System.IO.Unsafe
import Data.ByteString.Lazy.Char8(unpack)

adminMFlow ::  FlowM Html  IO ()
adminMFlow= do
   op <- ask  $  wlink "sync"  (bold << "sync")
             <|> wlink "flush" (bold << "flush")
             <|> wlink "errors"(bold << "errors")
             <|> wlink "users" (bold << "users")
             <|> wlink "end"   (bold << "end")
             <|> wlink "abort" (bold << "abort")

   case op of
    "sync" ->  liftIO $ syncCache >> print "syncronized cache"
    "flush" -> liftIO $ atomically flushAll >> print "flushed cache"
    "users" -> users
    "errors" -> errors
    "end"  -> liftIO $ syncCache >> print "bye" >> exitWith(ExitSuccess)
    "abrt" -> liftIO $ exitWith(ExitSuccess)
    _ -> return()
   adminMFlow

hlog= unsafePerformIO $ openFile logFileName ReadMode
errors= do
  log <- liftIO $ hGetContents hlog
  let ls = read log
  us <- showFormList ls 0 10

  optionsUser us ""



users= do
  users <- liftIO $ atomically $ return . map  fst =<< indexOf userName

  showFormList   [[u] | u<-users] 0 10


showFormList (ls :: [[String]]) n l= do

  ops <- ask  $ updown n l |+| list
  case ops of
    (Just nav, Nothing) -> showFormList ls nav l
    (_,Just us) -> optionsUser us ""
  where
  list= widget $ span1 n l [let u= head e in wlink u (p << e)<++ (thespan << show e)| e <- ls ]
  span1 n l = take l . drop n
  updown n l= wlink ( n +l) (p << "up") <|> wlink ( n -l) (p << "down")

optionsUser us err= do
-- add put in debug mode
    wfs <- liftIO $ return . map fst =<<  getMessageFlows
    wf <- ask  $ thespan << err ++> widget [ wlink wf (thespan << wf)| wf <- wfs]
    mstat <- liftIO $  getWFHistory  wf Token{twfname= wf,tuser=us}
    case mstat of
      Nothing -> optionsUser us "not found"
      Just stat -> do
         ask $ (thespan << unpack (showHistory stat)) ++> wlink "" (thespan << "press a key to menu")
         optionsUser us ""
