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

module MFlow.Forms.Test (Generate(..),runTest,runTest1,inject, ask, askt, userWidget, getUser, getUserSimple, verify) where
import MFlow.Forms hiding(ask,askt,getUser,userWidget,getUserSimple)
import qualified MFlow.Forms (ask)
import MFlow.Forms.Internals
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

import Data.Dynamic
import Data.TCache.Memoization



class Generate a where
  generate :: IO a

instance Generate a => Generate (Maybe a) where
   generate= do
     b <- randomRIO(0,1 :: Int)
     case b of 0 -> generate >>= return . Just ; _ -> return Nothing

instance  Generate String where
   generate= replicateM 5  $ randomRIO ('a','z')

instance Generate Int where
   generate= randomRIO(1,1000)

instance Generate Integer where
   generate= randomRIO(1,1000)


instance (Generate a, Generate b) => Generate (a,b) where
  generate= fmap (,) generate `ap` generate


instance (Generate a, Generate b) => Generate (Maybe a,Maybe b) where
  generate= do
       r <- generate
       case r of
        (Nothing,Nothing) -> generate
        other -> return other


instance (Bounded a, Enum a) => Generate a where
    generate= mx
     where
     mx= do
          let x= typeOfIO mx
          n <- randomRIO ( fromEnum $ minBound `asTypeOf` x
                         , fromEnum $ maxBound `asTypeOf` x)
          return $ toEnum n
          where
          typeOfIO :: IO a -> a
          typeOfIO = undefined

-- | run a list of flows with a number of simultaneous threads



runTest :: [(Int, Flow)] -> IO () 
runTest ps= do
  mapM_ (forkIO . run1) ps
  putStrLn $ "started " ++ (show . sum . fst $ unzip ps) ++ " threads"
   
  where
  run1 (nusers,  proc) =  replicateM_ nusers $ runTest1 proc
  
runTest1 f = do
    atomicModifyIORef testNumber (\n -> (n+1,n+1))
    name <- generate
    x <- generate
    y <- generate
    z <- generate 
    r1<- liftIO newEmptyMVar
    r2<- liftIO newEmptyMVar 
    let t = Token x y z [] [] r1 r2
    WF.start  name   f t

testNumber= unsafePerformIO $ newIORef 0

getTestNumber :: MonadIO m => m Int
getTestNumber= liftIO $ readIORef testNumber

-- | inject substitutes an expression by other. It may be used to override
-- ask interaction with the user. It should bee used infix for greater readability:
--
-- > ask something    `inject` const someother
--
-- The parameter passed is the test number
-- if the flow has not been executed by runTest, inject return the original
inject :: MonadIO m => m b -> (Int -> b) -> m b
inject exp v= do
   n <- getTestNumber
   if n== 0 then exp else exp `seq` return $ v n

-- | a simulated ask that generate  simulated user input of the type expected.
--
--  It forces the web page rendering, since it is monadic and can contain
-- side effects and load effects to be tested.
--
--  it is a substitute of 'ask' from "MFlow.Forms" for testing purposes.

-- execute 'runText' to initiate threads under different load conditions.
ask :: (Generate a, MonadIO m,  Functor m, FormInput v,Typeable v) => View v m a -> FlowM v m a
ask w = do
    FormElm forms mx <- FlowM . lift $ runView  w
    r <- liftIO generate
    let n= B.length . toByteString $ mconcat forms
    breturn $ n `seq` mx `seq` r
--    let u= undefined
--    liftIO $ runStateT (runView mf) s
--    bool <- liftIO generate 
--    case bool of
--          False -> fail ""
--          True -> do
--            b <- liftIO generate
--            r <- liftIO generate
--            case  (b,r)  of
--                (True,x)  -> breturn x
--                _         -> ask w


-- | instead of generating a result like `ask`, the result is given as the first parameter
-- so it does not need a Generate instance.
--
-- It forces the web page rendering, since it is monadic so it can contain
-- side effects and load effects to be tested.
askt :: (MonadIO m, FormInput v) => (Int -> a) -> View v m a -> FlowM v m a
askt v w =  do
    FormElm forms mx <- FlowM . lift $ runView  w
    n <- getTestNumber
    let l= B.length . toByteString $ mconcat forms
    breturn $ l `seq` mx `seq` v n

--mvtestopts :: MVar (M.Map String (Int,Dynamic))
--mvtestopts = unsafePerformIO $ newMVar M.empty

--asktn :: (Typeable a,MonadIO m) => [a] -> View v m a -> FlowM v m a
--asktn xs w= do
--    v <- liftIO $ do
--         let k = addrStr xs
--         opts <- takeMVar mvtestopts
--         let r = M.lookup k opts
--         case r of
--              Nothing -> do
--                putMVar mvtestopts $ M.singleton k (0,toDyn xs)
--                return $ head  xs
--              Just (i,d) -> do
--                putMVar mvtestopts $ M.insert k (i+1,d) opts
--                return $ (fromMaybe (error err1) $ fromDynamic d) !! i
--
--    askt v w
--
--    where
--    err1= "MFlow.Forms.Test: asktn: fromDynamic error"


-- | verify a property. if not true, throw the error message.
--
-- It is intended to be used in a infix notation, on the right of the code,
-- in order to separate the code assertions from the application code and make clearly
-- visible them as a form of documentation.
-- separated from it:
--
-- > liftIO $ print (x :: Int)          `verify` (return $ x > 10, "x < = 10")
--
-- the expression is monadic to allow for complex verifications that may involve IO actions
verifyM :: Monad m => m b -> (m Bool, String) -> m b
verifyM f (mprop, msg)= do
    prop <- mprop
    case prop of
     True ->  f
     False -> error  msg

-- | a pure version of verifyM
verify :: a -> (Bool, String) -> a
verify f (prop, msg)= do
    case prop of
     True ->  f
     False -> error  msg


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

waction :: (Functor m, MonadIO m,Generate a, FormInput view)
     => View view m a
     -> (a -> FlowM view m b)
     -> View view m b
waction w f= do
  x <- liftIO generate
  MFlow.Forms.waction (return x) f

userWidget :: ( MonadIO m, Functor m
          , FormInput view) 
         => Maybe String
         -> View view m (Maybe (String,String), Maybe String)
         -> View view m String
userWidget muser formuser= do
   user <- getCurrentUser
   if muser== Just user then return user
    else if isJust muser then do
          let user= fromJust muser
          login user >> return user
    else liftIO generate >>= \u -> login u >> return u

   where
   login uname= do
         st <- get
         let t = mfToken st
             t'= t{tuser= uname}
         put st{mfToken= t'}
         return ()
   
getUserSimple :: ( MonadIO m, FormInput view, Typeable view
                 ,  Functor m)
              => FlowM view m String
getUserSimple= getUser Nothing userFormLine


getUser :: ( FormInput view, Typeable view
           ,  Functor m,MonadIO m)
          => Maybe String
          -> View view m (Maybe (String,String), Maybe String)
          -> FlowM view m String
getUser mu form= ask $ userWidget mu form

--wmodify
--  :: (Functor m, MonadIO m, FormInput v, Generate (Maybe a)) =>
--     View v m a1
--     -> ([v] -> Maybe a -> WState v m ([v], Maybe b))
--     -> View v m b
--wmodify formt act = do
--    x <-  liftIO generate
--    formt `MFlow.Forms.wmodify` (\ f _-> return (f,x)) `MFlow.Forms.wmodify` act

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

generateGenerate Test{..}= do
  b <- generate
  case b of
     True -> genLink
     False -> genForm

  where
  genForm= do
         -- one on every generate is incomplete
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
             str <- generate
             return $  (n,str)

  genTextArea= do
     let n = length tfinput
     if n==0
       then return []
       else mapM gen tftextarea
       where
       gen(n,_)= do
             str <- generate
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
     u <- generate
     mapM (genTraffic u) $ M.toList wfs

   genTraffic u (n,_)= forkIO $ iterategenerates  [(pwf,n),(cookieuser,u)] []

   iterategenerates ident msg= iterate [] msg
     where
     iterate cs msg= do
       (HttpData ps cooks test,_) <- msgScheduler $  ident ++ cs++ msg
       let cs'= cs++ map (\(a,b,c,d)-> (a,b)) cooks
       resp <- generateGenerate . read $ B.unpack test
       iterate cs' resp

      -}
