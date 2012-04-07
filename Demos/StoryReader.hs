{-# OPTIONS -XDeriveDataTypeable
            -XMultiParamTypeClasses
            -XTypeSynonymInstances
            #-}
module Main where
import MFlow.Hack.XHtml.All

import Data.Typeable
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Maybe
import System.IO
import System.IO.Unsafe

import Data.Monoid

import Data.TCache
import Data.RefSerialize hiding((<|>))
import Data.TCache.DefaultPersistence
import Data.ByteString.Lazy.Char8(pack,unpack)
import System.Directory

adminUser= "admin"
main= do
   userRegister adminUser adminUser
   putStrLn $ options messageFlows
   run 80 $ hackMessageFlow messageFlows
   where
   messageFlows=  [("main",  runFlow showStories)
                  ,("admin", transient $ runFlow admin)]

   options msgs= "in the browser choose\n\n" ++
     concat [ "http://server/"++ i ++ "\n" | (i,_) <- msgs]



type FileName= String
type Seek= Integer



type Stories= M.Map FileName Handle

instance Serialize Stories where
 showp  map = showp keys  where keys = M.keys map
 readp  = do
    stories <- readp
    return $ storiesList stories
    where

    storiesList stories = unsafePerformIO $ do
       hs <- mapM ( \f -> openFile f ReadMode) stories
       return . M.fromList $ zip stories hs

instance Indexable Stories where
  key= const keyStories

keyStories= "Stories"

rstories :: DBRef Stories
rstories= unsafePerformIO . atomically . newDBRef $ M.fromList []

showHistoriesPage :: [FileName] -> View Html IO  String
showHistoriesPage  stories=
  br
  +>                    -- add Html to the widget
  p << "-----Stories-----"
  +>
  widget(Selection{
       stitle = bold << "choose an Story",
       sheader= [ bold << "title"   ],
       sbody= [([toHtml title],title )
              | title <-  stories]})




chunkSize= 1000 :: Integer
newtype  Chunk= Chunk [String]
data Navigation= Forward | Backward | Home deriving (Read, Show, Typeable)


--instance Launchable Chunk Navigation IO Html

instance Widget Chunk Navigation IO Html where
   widget (Chunk buf)=
     widget $  (wlink Forward  $ bold << "forward")
           <|> (wlink Backward $ bold <<"backward")
           <|> (wlink Home $ bold <<"Other stories")
           <|> (concatHtml [p << l | l <- buf]
                 +>
                  (wlink Forward  $ bold << "forward"))
           <|> (wlink Backward $ bold <<"backward")
           <|> (wlink Home $ bold <<"Other stories")



appheader c = thehtml << body << c



--admin :: FlowM Html  IO ()
admin= do
 setHeader $ \c-> appheader (concatHtml [(p << bold << "Administration"),c])
 setTimeouts (4* 60) 0
 u <- getUser
 if u /= adminUser
    then do
      ask $ bold   << "You must sign as administrator"
            +> (wlink "l" (bold << "Login again")
            <+ p << hotlink "main" (bold <<"main page"))
      admin

    else do
     setTimeouts (4* 60) 0
     op <- ask   $  p <<< wlink "a" (p << "Añadir contenido a relato")
                <|> p <<< wlink "n" (p << "Añadir un relato nuevo")
                <|> p <<< wlink "d" (p << "Borrar un relato" )

     case op of
      "a" -> do
           Just stories <- lift $ atomically $ readDBRef rstories
           let nstories = M.keys stories
           hist <- ask $ showHistoriesPage  nstories
           let Just h = M.lookup hist stories
           content <- lift $ hGetContents h
           ncontent <- ask . getMultilineText . Just $ content
           lift $ hPutStr h ncontent
      "n" -> do
           hist <- ask . getString $ Just "Enter the name of the story"
           (hist,cont) <- ask $
                           (,) <$> getString (Just "Name of the Story")
                               <*> getMultilineText (Just "Enter the content")
           liftIO $ do
             h <- openFile hist WriteMode
             mstories <- atomically $ readDBRef rstories
             atomically $ writeDBRef rstories $ add (hist,h) mstories
             hPutStr h cont
      "d" -> do
           Just stories <- lift $ atomically $ readDBRef rstories

           hist <- ask . showHistoriesPage $  M.keys stories
           liftIO $ do

             atomically $ writeDBRef rstories $ del hist stories
             removeFile hist



  where
  del k   stories= M.delete  k stories

  add (k,v) (Just stories)= M.insert k v stories
  add (k,v) Nothing =  M.singleton k v



type UserContext= M.Map FileName (Seek,Handle)

--showStories ::  FlowM Html (Workflow IO) ()
showStories   = do
   setTimeouts (10*60) 0
   Just stories <- liftIO . atomically $ readDBRef rstories
   let usercontext = initUserContext stories
   showStories1 usercontext
   where

   initUserContext stories =  M.fromList [  (n,(0,h)) | (n,h) <- M.toList stories]

   showStories1  stories=  do
     story   <- step . ask  $ getUserWidget *> showHistoriesPage (M.keys stories)
     showStory stories story

     where
     showStory stories story = do

           todo <- step $ do
                    chunk <-  lift $ getChunk  story
                    ask $  getUserWidget *>  widget chunk

           case todo of
                Forward -> do
                  let newstories= forward story
                  showStory newstories story

                Backward -> do
                  let newstories= backward story
                  showStory newstories story

                Home -> showStories1 stories
           where
           forward  story = do
              case M.lookup story stories of
                 Just(seekit, h) ->
                  M.insert story (seekit + chunkSize,h) stories

           backward  story= do
              case M.lookup story stories of
                 Just(seekit, h) ->
                  M.insert story (seekit - chunkSize,h) stories

           getChunk  story = do
              case M.lookup (story :: String) stories of
                Just(seekit, h) -> do
                  hSeek h  AbsoluteSeek seekit
                  buf <- readLines h
                  return $ Chunk buf


readLines h = readLines1 [] 0
  where
  readLines1 buf len =do
    l <- hGetLine h
    iseof <- hIsEOF h
    let len'= len + length l
        buf'= buf++ [l]
    if fromIntegral len' >= chunkSize || iseof
       then return buf'
       else readLines1 buf' len'

