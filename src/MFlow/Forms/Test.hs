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
            -XRecordWildCards
            #-}

module MFlow.Forms.Test (runTest,ask) where
import MFlow.Forms hiding(ask)
import qualified MFlow.Forms (ask)
import MFlow.Forms(FormInput(..))
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
import Data.List
import Data.Typeable
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Concurrent.MVar
import Data.TCache.Memoization
import Control.Monad.State
import Data.Monoid
import Data.Maybe
import Data.IORef
import MFlow.Cookies(cookieuser)


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


instance (Response a, Response b) => Response (Maybe a,Maybe b) where
  response= do
       r <- response
       case r of
        (Nothing,Nothing) -> response
        other -> return other


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


runTest :: [(Int, Token -> Workflow IO ())] -> IO ()
runTest ps=do
  mapM_ (forkIO . run1) ps

  putStrLn $ "started " ++ (show . sum . fst $ unzip ps) ++ " threads"
  adminLoop

run1 (nusers,  proc) =  replicateM_ nusers $ randomFlow proc
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
{-
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
-}

{-
type Var= String
data Test=  Test{tflink:: [(Var,String)]
                ,selectOptions :: [(Var,[String])]
                ,tfinput :: [(Var, String)]
                ,tftextarea :: [(Var, String)]
                }
                deriving(Read,Show)

type TestM =  Test -> Test

instance Monoid  TestM  where
  mempty=  id
  mappend= (.)

instance  FormInput TestM  where
    ftag = const id
    inred  = const id 
    fromStr = const id 
    flink var _= let(n,v)=break (=='=') var in  \t ->t{tflink= (n,tail v):tflink t}
    finput n _ v _ _ = \t -> t{tfinput = (n,v):tfinput t}
    ftextarea n v= \t -> t{tftextarea = (n,v):tftextarea t}
    fselect n _= \t -> t{selectOptions=(n,[]):selectOptions t}
    foption o _ _= \t ->
         let (n,opts)= head $ selectOptions t
         in t{selectOptions=(n,o:opts):tail (selectOptions t)}
    formAction  _ _= id
    addAttributes _ _= id

generateResponse Test{..}= do
  b <- response
  case b of
     True -> genLink
     False -> genForm

  where
  genForm= do
         -- one on every response is incomplete
         n <- randomRIO(0,10) :: IO Int
         case n of
           0 -> do
             genInput

           _ -> do
             r1 <- genInput
             r2 <- genSelect
             r3 <- genTextArea
             return $ r1++r2++r3
  genLink= do
         let n = length tflink
         if n == 0 then genForm
          else do
            r <- randomRIO(0,n )
            return [tflink !! r]

  genSelect=do
   let n = length selectOptions
   if n== 0
    then return []
    else mapM gen selectOptions
    where
    gen(s,os)= do
     let m = length os
     j <- randomRIO(0,m)
     return (s, os !! j)

  genInput= do
     let n = length tftextarea
     if n==0
      then return []
      else mapM gen tfinput
      where gen(n,_)= do
             str <- response
             return $  (n,str)

  genTextArea= do
     let n = length tfinput
     if n==0
       then return []
       else mapM gen tftextarea
       where
       gen(n,_)= do
             str <- response
             return $  (n,str)

pwf= "pwf"
ind= "ind"
instance  Processable Params where
     pwfname = fromMaybe noScript  . lookup pwf
     puser= fromMaybe anonymous  . lookup cookieuser
     pind = fromMaybe "0"  . lookup ind
     getParams = id



runTest nusers = do
   wfs <- getMessageFlows
   replicateM nusers $ gen wfs
   where
   gen wfs = do
     u <- response
     mapM (genTraffic u) $ M.toList wfs

   genTraffic u (n,_)= forkIO $ iterateresponses  [(pwf,n),(cookieuser,u)] []

   iterateresponses ident msg= iterate [] msg
     where
     iterate cs msg= do
       (HttpData ps cooks test,_) <- msgScheduler $  ident ++ cs++ msg
       let cs'= cs++ map (\(a,b,c,d)-> (a,b)) cooks
       resp <- generateResponse . read $ B.unpack test
       iterate cs' resp

      -}
