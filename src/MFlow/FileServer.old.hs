{- | A file server for frequently accessed files, such are static web pages and image decorations, icons etc
that are cached (memoized) according with the "Data.TCache" policies in the program space. This avoid the blocking of
the efficient GHC threads by frequent IO calls.So it enhances the performance
in the context of heavy concurrence.
It uses 'Data.TCache.Memoization'.
The caching-uncaching follows the `setPersist` criteria.
-}
-----------------------------------------------------------------------------
--
-- Module      :  FileServer
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
--
--
-----------------------------------------------------------------------------
{-# OPTIONS -XScopedTypeVariables  #-}
module MFlow.FileServer (setFilesPath, serveFile

) where

import MFlow
import MFlow.Cookies(contentHtml)
import Control.Monad.State
import Data.TCache.Memoization
import MFlow.Forms.XHtml
import System.Directory
import Data.ByteString.Lazy.Char8 as B(readFile,concat,append,pack,empty)

import Control.Exception as CE
import Data.List
import System.IO.Unsafe
import Data.IORef
import Data.Monoid


rfilesPath= unsafePerformIO $ newIORef "files/"

-- | Set the path of the files in the web server. The links to the files are relative to it
setFilesPath :: String -> IO ()
setFilesPath path= writeIORef rfilesPath path

pathPrm=  "path"
fileServe :: Flow
fileServe  = stateless $ \env  -> do
  case lookup pathPrm   env of
    Nothing -> error " no file specified"
    Just path' ->do
     when(let hpath= head path' in hpath == '/' || hpath =='\\') $ error noperm
     when(not(".." `isSuffixOf` path') && ".." `isInfixOf` path') $ error noperm
     filesPath <- readIORef rfilesPath
     let path= filesPath ++ path'
     serveFile path
    -- isDirectory <- doesDirectoryExist  path -- !> path

--     case isDirectory of
--       True  -> do
--          dir <-  directory1 $ path ++ "/"
--          return $ HttpData  (setMime "text/html") []  dir
--       False -> servefile path


 where


 dropBack ".."= ".."
 dropBack path
     | "../" `isPrefixOf` revpath =reverse . maybetail $ dropWhile (/= '/') $ drop 3 revpath
     | otherwise= path
   where
   revpath= reverse path
--   maybetail ""= "."
   maybetail xs= tail xs
 noperm= "no permissions"
 
stringServer mime str= stateless
-- | Is the flow to be added to the list in order to stream any file from the filesystem
-- for example, images
--
-- This app includes the fileServe  flow:
--
-- @
-- main= do
--   addFileServerWF
--   addMessageFlows messageFlows
--   run 80  hackMessageFlow
--   adminLoop
--   where
--   messageFlows=  [(\"noscript\" , transient $ runFlow showStories)
--                  ,("\admin\"    , transient $ runFlow admin)
--                  ,("\mail\"     , transient $ runFlow mail)]@
--
-- | Add the fileServer to the list of server flows
-- it is mandatory if you want to use the file service
addFileServerWF= addMessageFlows [("file", fileServe)]



-- | Creates the url of file path. To be used in ordinary links to files.
-- in Text.XHtml, a image would be embeded as
--
-- > image ![src $ linkFile imagepath]
--
-- in HSP:
--
-- > <img src=(linkFile imagepath)\>

-- | Given the relative path of a file, it return the content of the @href@ element in a html link
linkFile :: String -> String
linkFile path=  "file?path=" <>  path

directory :: Flow
directory = stateless $ \_ -> do
   path <- readIORef rfilesPath
   directory1 path

directory1 path = do
   fs <- getDirectoryContents path
   return $  HttpData [contentHtml][] $ B.concat [btag "a" [("href",linkFile ( path ++  file))] (B.pack file) `append` btag "br" [] B.empty | file <- fs]


