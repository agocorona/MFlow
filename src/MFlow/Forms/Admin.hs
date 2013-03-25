{-# OPTIONS
            -XScopedTypeVariables

            #-}
module MFlow.Forms.Admin(adminLoop, wait, addAdminWF) where
import MFlow.Forms
import MFlow.Forms.XHtml
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
import Data.ByteString.Lazy.Char8 as B (unpack,tail,hGetNonBlocking,append, pack)
import System.IO
import Data.RefSerialize hiding ((<|>))
import Data.Typeable
import Data.Monoid
import Data.Maybe
import Data.Map as M (keys)
import System.Exit
import Control.Exception as E
import Control.Concurrent
import Control.Concurrent.MVar



ssyncCache= putStr "sync..." >> syncCache >> putStrLn "done"

-- | A small console interpreter with some commands:
--
-- [@sync@] Synchronize the cache with persistent storage (see `syncCache`)
--
-- [@flush@] Flush the cache
--
-- [@end@] Synchronize and exit
--
-- [@abort@] Exit. Do not synchronize
--
-- on exception, for example Control-c, it sync and exits.
-- It must be used as the last statement of the main procedure.
adminLoop :: IO ()
adminLoop= do
  msgs <- getMessageFlows
  putStrLn ""
  putStrLn $  "Served:"
  mapM putStrLn  [ "     http://server:port/"++ i  | i <- M.keys msgs]
  putStrLn ""
  putStrLn "Commands: sync, flush, end, abort"
  adminLoop1
  `E.catch` (\(e:: E.SomeException) ->do
                      ssyncCache
                      error $ "\nException: "++ show e)

adminLoop1= do
       putStr ">"; hFlush stdout
       op <- getLine
       case op of
        "sync"  -> ssyncCache
        "flush" -> atomically flushAll >> putStrLn "flushed cache"
        "end"   -> ssyncCache >> putStrLn "bye" >> exitWith ExitSuccess
        "abort" -> exitWith ExitSuccess
        _       -> return()
       adminLoop1

-- | execute the process and wait for its finalization.
--  then it synchronizes the cache
wait f= do
    mv <- newEmptyMVar
    forkIO (f1 >> putMVar mv True)
    putStrLn "wait: ready"
    takeMVar mv
   `E.catch` (\(e:: E.SomeException) ->do
                  ssyncCache
                  error $ "Signal: "++ show e)
    where
    f1= f
--     do
--        n <- getNumProcessors
--        setNumCapabilities n
--        f

-- | Install the admin flow in the list of flows handled by `HackMessageFlow`
-- this gives access to an administrator page. It is necessary to
-- create an admin user with `setAdminUser`.
--
-- The administration page is reached with the path \"adminserv\"
addAdminWF= addMessageFlows[("adminserv",transient $ runFlow adminMFlow)]


adminMFlow ::  FlowM   Html IO ()
adminMFlow= do
   admin <- getAdminName
   u <- getUser (Just admin) $ p << bold << "Please login as Administrator" ++> userLogin
   op <- ask  $  p <<< wlink "sync"  (bold << "sync")
             <|> p <<< wlink "flush" (bold << "flush")
             <|> p <<< wlink "errors"(bold << "errors")
             <|> p <<< wlink "users" (bold << "users")
             <|> p <<< wlink "end"   (bold << "end")
             <|> wlink "abort" (bold << "abort")

   case op of
    "users" -> users
    "sync" ->  liftIO $ syncCache >> print "syncronized cache"
    "flush" -> liftIO $ atomically flushAll >> print "flushed cache"

    "errors" -> errors
    "end"  -> liftIO $ syncCache >> print "bye" >> exitWith(ExitSuccess)
    "abort" -> liftIO $ exitWith(ExitSuccess)
    _ -> return()
   adminMFlow


errors= do
  size <- liftIO $ hFileSize hlog
  if size == 0
   then ask $ wlink () (bold << "no error log")
   else do
       liftIO $ hSeek hlog AbsoluteSeek 0
       log   <- liftIO $ hGetNonBlocking hlog  (fromIntegral size)

       let ls :: [[String ]]= runR  readp $ pack "[" `append` (B.tail log) `append` pack "]"
       let rows= [wlink (head e) (bold << head e) `waction` optionsUser  : map (\x ->noWidget <++ fromStr x) (Prelude.tail e) | e <- ls]
       showFormList rows 0 10
  breturn()







users= do
  users <- liftIO $ atomically $ return . map  fst =<< indexOf userName

  showFormList   [[wlink u (bold << u) `waction` optionsUser   ] | u<-users] 0 10

showFormList
  :: (Functor m, MonadIO m) =>
     [[View Html m ()]]
     -> Int -> Int -> FlowM Html m b
showFormList ls n l= do
  nav <- ask  $  updown n l <|> (list **> updown n l)
  showFormList ls nav l

  where
  list= table <<< firstOf (span1 n l [tr <<< cols  e | e <- ls ])

  cols e= firstOf[td <<< c | c <- e]
  span1 n l = take l . drop n
  updown n l= wlink ( n +l) (bold << "up ") <|> wlink ( n -l) (bold << "down ") <++ br

optionsUser  us = do
    wfs <- liftIO $ return . M.keys =<< getMessageFlows
    stats <-  let u= undefined
              in  liftIO $ mapM  (\wf -> getWFHistory wf (Token wf us u u u u)) wfs
    let wfss= filter (isJust . snd) $ zip wfs stats
    if null wfss
     then ask $ bold << " not logs for this user" ++> wlink () (bold << "Press here")
     else do
      wf <-  ask $ firstOf [ wlink wf (p << wf) | (wf,_) <-  wfss]
      ask $ p << unpack (showHistory . fromJust . fromJust $ lookup wf  wfss) ++>  wlink () (p << "press to menu")

