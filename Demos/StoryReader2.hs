{-# OPTIONS -XDeriveDataTypeable
            -XMultiParamTypeClasses
            -XTypeSynonymInstances
            -XScopedTypeVariables
            -XNoMonomorphismRestriction
            #-}
module Main where
import MFlow.Hack.XHtml.All

import Data.Typeable
import Control.Monad(when)
import Control.Monad.Trans
import qualified Data.Map as M
import Data.Maybe
import System.IO hiding(openFile,openBinaryFile, hGetLine, hPutStr)
import System.IO.UTF8(openBinaryFile,hGetLine, hPutStr)
import System.IO.Unsafe
import System.IO.Error(isEOFError,catch)
import Control.Exception hiding (catch)

import qualified Codec.Binary.UTF8.String as UTF8


import Data.Monoid

import Data.TCache
import Data.RefSerialize hiding((<|>))
import Data.TCache.DefaultPersistence
import System.Directory
import Data.List((\\))
import Data.Char(isSpace)




import Debug.Trace
import Control.Monad.State

(!>)= flip trace


adminUser= "admin"
main= do
   maybeColdRebuildStories
   userRegister adminUser adminUser
   putStrLn $ "in the browser go to: http://localhost"
   run 80 $ hackMessageFlow  messageFlows
   where
   messageFlows=  [("noscript",  runFlow showStories)
                  ,("admin", transient $ runFlow admin)
                  ,("info", stateless opts)]

   opts :: Env -> IO Html
   opts _=  return $ p <<  concatHtml (p <<"options: " : options)

   options=   [ p << hotlink i (bold << i) | (i,_) <- tail $ reverse messageFlows]

maybeColdRebuildStories=   do
   strs <- atomically $ readDBRef rstories
   case strs of
     Just _ -> return()
     Nothing -> do
       l<- getDirectoryContents storiesPath   -- UTF8.decodeString
       let l'= l \\ [keyObjDBRef rstories,"..","."]
       hs <- mapM ( \f -> openBinaryFile f ReadWriteMode) $ map (storiesPath++) l'
       atomically $  writeDBRef rstories . M.fromList . zip l' $ zip l' hs

type Name= String
type FileName= String
type Seek= Integer



type Stories= M.Map Name (FileName, Handle)


instance Serialize Stories where
 showp  stories = showp . map (\(n,(f,_)) ->  (n,f) )$ M.assocs stories
 readp  = do
    stories :: [(String,String)] <- readp
    return $ storiesList $ map fst stories
    where

    storiesList stories = unsafePerformIO $ do
       hs <- mapM ( \f -> openBinaryFile f ReadWriteMode) $ map (storiesPath++) stories
       return . M.fromList $ zip stories $ zip stories hs

instance Indexable Stories where
  key= const keyStories
  defPath _= storiesPath

storiesPath= "Stories/"

keyStories= "Stories"

rstories :: DBRef Stories
rstories= getDBRef keyStories



chunkSize= 2000 :: Integer

--data Navigation= Seek Integer | Home deriving (Read, Show, Typeable)



appheader c =
   thehtml <<  body <<  c


formUser=
       (User <$> getString (Just "Enter user") <! [("size","5")]
             <*> getPassword                   <! [("size","5")]
             <+> submitButton "login")
             <+> fromString "  Password again" ++> getPassword <! [("size","5")]
             <* submitButton "to register"

userwidget=  userWidget Nothing formUser

userFormOrName= View $ do
  felem@(FormElm f mu) <- runView userwidget
  case mu of
    Just u -> return $ FormElm [fromString u] mu
    Nothing -> return felem

--admin :: FlowM Html  IO ()
admin= do
     setHeader $ \c -> appheader $ concatHtml [ h1 ![align "center"] << "Administration",c]
     setTimeouts (4* 60) 0
     clearEnv
     u <- getUser (Just "admin") formUserLine
     let adcontstr= "Añadir contenido a relato"
         addrelstr= "Añadir un relato nuevo"
         delrelstr= "Borrar un relato"

     setTimeouts (4* 60) 0
     op <- ask   $  p <<< wlink "a" (p << adcontstr)
                <|> p <<< wlink "n" (p << addrelstr)
                <|> p <<< wlink "d" (p << delrelstr)


     case op of
      "a" -> do
         stories <- liftIO (atomically ( readDBRef rstories))
                      >>= return . (fromMaybe M.empty)
         let nstories = M.keys stories
         r <- ask $ homelink |+| h3 ! [align "center"] << adcontstr ++>  showHistoriesPage  nstories
         case r of
          (Just _,_)  -> admin
          (_,Just hist)-> do
           let Just (_,h) = M.lookup hist stories
           content  <- liftIO $ hGetContents1 h
           mr <- ask $  homelink
                    |+| wform ( h3 ! [align "center"] << adcontstr
                               ++> getMultilineText (Just content) ![rows "30",thestyle "width:80%"]
                               <* br ++> submitButton "submit")
                      -- <+> br +> homelink
           case mr of
            (_,Just ncontent) ->  liftIO $ hPutStr h ncontent
            (Just _,_) -> admin

      "n" -> do
         r <- ask .wform $ h3 << addrelstr ++> br ++>
                          ((,) <$> p <<< getString (Just "Name of the Story")
                               <*> p <<< getMultilineText (Just "Enter the content") ![rows "30",  thestyle "width:80%"]
                               <*  p <<< submitButton "submit")
                           <+>  homelink
         case r of
          (_,Just _) -> admin
          (Just(hist,cont),_) -> do
           liftIO $ do
             h <- openBinaryFile hist ReadWriteMode
--             withResource (M.empty :: Stories)   $ \ ms -> add hist h ms

             atomically $ do
                 mstories <- readDBRef rstories
                 writeDBRef rstories $ add  hist h mstories
             hPutStr h cont

      "d" -> do
           Just stories <- liftIO $ atomically $ readDBRef rstories

           hist <- ask . showHistoriesPage $  M.keys stories
           liftIO $ do
             atomically $ writeDBRef rstories $ del hist stories
             removeFile hist
     liftIO $ syncCache
     admin
  where
  homelink= br ++> wlink "h" ( bold << "Admin Home")
  del k   stories= M.delete  k stories

  add k v (Just stories)= M.insert k (strip k,v) stories
  add k v Nothing =  M.singleton k (strip k,v)
  strip k= k \\ "\\/:*?\"<>|"
  hGetContents1 h = hGetContents2 []
   where
   hGetContents2 str  = do
      hSeek h  AbsoluteSeek 0
      ml <- hGetLineExc h
      case ml of
       Nothing -> return str
       Just l  -> hGetContents2 $ str++l



showHistoriesPage :: [FileName] -> View Html IO  String
showHistoriesPage  stories=
   h2 << "Choose an story"
   ++>
   widget [p <<< wlink s (bold << s) | s <- stories]

-- do
-- n <- getNewName
-- br
--  ++>
--  wform (
--   table ! [thestyle "align:center;width:80%"] <<< (
--    caption << h2 << "Choose an story"  ++>
--    th ! [align "left"] << bold << "Title"  ++>
--      widget[tr <<< (td << story  ++> td <<< getRadioActive n story) | story <- stories] 
--    ))

other= -1000  --fake seek to signal back to main

--showStories ::  FlowM Html (Workflow IO) ()
showStories   = do
   setTimeouts (60) 0
   Just stor <- liftIO . atomically $ readDBRef rstories

   showStories1 $ M.fromList [  (n,0) | n <- M.keys stor]
   where

   showStories1  usercontext =  do
     story <- step . ask  $  userFormOrName **>  showHistoriesPage (M.keys usercontext)
     showStory usercontext story


     where

     showStory usercontext  story = do

       seek <- step $ do
                (chunk,sizelastChunk,seekit,size)  <- getChunk story
                ask $  topForm story seekit size
                       **>
                       showBuffer seekit sizelastChunk chunk


       case seek of
            -1000 -> showStories1 usercontext
            seek  -> do
              let usercontext'= M.insert story seek usercontext
              showStory usercontext' story

       where

       topForm title seek size=
         table ! [thestyle "width:100%"]
          <<< tr
            <<< (td <<< userFormOrName
                <++ concatHtml
                  [td  ! [align "center"] << (show (seek * 100 `div` size) ++ "%")
                  ,td  ! [align "right"] << title])

       getChunk  story  = liftIO $ do
          Just stories <- atomically $ readDBRef rstories
          let mh =  M.lookup story stories
          case mh of
            Nothing -> return (["This Story not longer exist"],0,0,0)
            Just (_,h) ->
                case M.lookup story  usercontext of
                  Nothing -> do
                      hSeek h  AbsoluteSeek 0
                      readLines h 0

                  Just seek -> do
                      hSeek h  AbsoluteSeek seek
                      readLines h seek



readLines h seek= readLines1 [] 0
  where
  readLines1 buf len =do
    mr <- hGetLineExc h
    size<- hFileSize h
    case mr of
       Nothing -> return (buf, len,seek,size)
       Just line  -> do
        let len'= len + length line
            buf'= buf++ [line]
        if fromIntegral len' >= chunkSize + 80
           then do
             let buf''= if seek >0 then dropWhile(not . isSpace) (head buf'):tail buf'
                                   else buf'
             return (buf'',len',seek, size)
           else readLines1 buf' len'

hGetLineExc h= (do
     x <- hGetLine h
     return $ Just x)
    `catch` (\(e :: IOError) -> do
        when( not $ isEOFError e) $ print e
        return Nothing)


--showBuffer :: (MonadIO m, Functor m)
--           =>  Integer ->  Int
--           -> [String] -> View Html m Integer
showBuffer seekit sizelastChunk buf =
   let
     disableAttrs= [("style","visibility:hidden")]
     bf= bold << ">>>>"
     bb= bold << "<<<<"

     seekbn   = let x= seekit - chunkSize
                in if x  >= 0
                       then seekit - chunkSize
                       else seekit
     seekfw   = if sizelastChunk < fromIntegral chunkSize
                       then seekit + chunkSize
                       else seekit



     fwlink= if seekfw== seekit
               then wlink seekfw bf <! disableAttrs
               else wlink fw bf


     bwlink= if seekbn == seekit
               then wlink seekbn bb <! disableAttrs
               else wlink bw bb


     otherLink = wlink other ( bold <<"Other stories")

     links = table ! [thestyle "width:100%"]
             <<< tr  <<<(td <<<  bwlink
                     <|> td ! [align "center"] <<< otherLink
                     <|> td ! [align "right"]  <<< fwlink)


     lenbuf2= length bufconcat `div` 2
     (buf1,buf2)= splitAt lenbuf2  bufconcat
     bufLink1 = wlink  bw (linesToHtml buf1]) <! [("style","text-decoration: none;color:black")]
     bufLink2 = wlink  fw (linesToHtml buf2]) <! [("style","text-decoration: none;color:black")]
   in
     links <|> bufLink1 <|> bufLink2 <|> links


