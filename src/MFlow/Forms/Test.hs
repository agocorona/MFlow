-----------------------------------------------------------------------------
--
-- Module      :  MFlow.Forms.Test
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# OPTIONS
            -XOverlappingInstances
            -XFlexibleInstances
            -XUndecidableInstances
            -XPatternGuards
            #-}

module MFlow.Forms.Test (Response(..),runTest, ask, waction, wmodify) where
import qualified MFlow.Forms (ask, waction, wmodify)
import MFlow.Forms hiding (ask, waction, wmodify)
import MFlow.Forms.Admin
import Control.Workflow as WF
import Control.Concurrent
import Control.Monad
import MFlow
import qualified Data.Map as M
import Control.Monad.Trans
import System.IO.Unsafe
import System.Random
import Data.Char(chr, ord)
import Data.Typeable
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.MVar
import Data.TCache.Memoization


class Response a where
  response :: IO a

instance Response a => Response (Maybe a) where
   response= do
     b <- randomRIO(0,1 :: Int)
     case b of 0 -> response >>= return . Just ; _ -> return Nothing

instance  Response String where
   response= replicateM 5  $ randomRIO ('a','z')

instance Response Int where
   response= randomRIO(1,1000)

instance Response Integer where
   response= randomRIO(1,1000)


instance (Response a, Response b) => Response (a,b) where
  response= fmap (,) response `ap` response

instance (Bounded a, Enum a) => Response a where
    response= mx
     where
     mx= do
          let x= typeOfIO mx

          n <- randomRIO ( fromEnum $ minBound `asTypeOf` x
                         , fromEnum $ maxBound `asTypeOf` x)
          return $ toEnum n
          where
          typeOfIO :: IO a -> a
          typeOfIO = error $ "typeOfIO not defined"





runTest :: [(Int, String)] -> IO ()
runTest ps=do
  mapM_ (forkIO . run1) ps

  putStrLn $ "started " ++ (show . sum . fst $ unzip ps) ++ " threads"
  adminLoop

run1 (nusers,  proc) = do
  mfs <- getMessageFlows
  case M.lookup proc mfs of
      Just f  -> replicateM_ nusers $ randomFlow f
      Nothing -> error $ "MFlow.Forms.Test: not found. \"" ++ proc ++ "\""
  where
  randomFlow f = do
    name <- response
    x <- response
    y <- response
    z <- response
    r1<- liftIO newEmptyMVar
    r2<- liftIO newEmptyMVar
    let t = Token x y z r1 r2
    forkIO . WF.exec1  name $  f t


ask :: (Response a, MonadIO m, Functor m, ToHttpData v, ToByteString v, FormInput v,Typeable v) => View v m a -> FlowM v m a
ask w = do
     w  `MFlow.Forms.wmodify` (\v x -> consume v >> return (v,x))
     `seq` rest
     where
     consume= liftIO . B.writeFile "NULL" . B.concat . map  toByteString
     rest= do
        bool <- liftIO $ response
        case bool of
              False -> fail ""
              True -> do
                b <- liftIO response
                r <- liftIO $ response
                case  (b,r)  of
                    (True,x)  -> breturn x
                    _         -> ask w


--
--match form=do
--  marches <- readIORef matches
--  return $ head map (m s) matches
--  where
--  m s (ms,ps) = case and  $ map (flip isInfixOf $ s) ms of
--                       True  -> Just ps
--                       False -> Nothing
--
--composeParams (Gen ps) form= zip (getParams form) ps
--  where
--  getParams form=
--    let  search name  form
--          | null form = mempty
--          | isPrefix name form = drop (length name) form
--          | otherwise= search name $ tail form
--
--         par s= takeWhile(/='\"') . dropWhile (/='\"') . tail . dropWhile (/='=') $ s
--         getPar= par $ search "name"
--    in  getPar form
--

waction ::(Functor m, MonadIO m,Response a, FormInput view)
     => View view m a
     -> (a -> FlowM view m b)
     -> View view m b
waction w f= do
  x <- liftIO response
  MFlow.Forms.waction (return x) f


--wmodify
--  :: (Functor m, MonadIO m, FormInput v, Response (Maybe a)) =>
--     View v m a1
--     -> ([v] -> Maybe a -> WState v m ([v], Maybe b))
--     -> View v m b
wmodify formt act = do
    x <-  liftIO response
    formt `MFlow.Forms.wmodify` (\ f _-> return (f,x)) `MFlow.Forms.wmodify` act

