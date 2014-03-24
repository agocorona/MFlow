-----------------------------------------------------------------------------
--
-- A haskell formlet is the combination of a parameter parser to read input plus a writer to generate HTTP
-- output
--
-- I use this similarity to create parsec-like combinators that use the formlet monad in MFlow
--(the View monad) to parse the web service parameters and to generate the output.
--
-- This service below implements a service that sum or multiply two Int-egers.
--
-- > parserService :: View Html IO ()
-- > parserService=
-- >        do rest "sum"  ; disp $ (+) <$> wint "t1" <*> wint "t2"
-- >    <|> do rest "prod" ; disp $ (*) <$> wint "t1" <*> wint "t2"
-- >    <?> do -- blaze Html
-- >           h1 << "ERROR. API usage:"
-- >           h3 << "http://server/api/sum?t1=[Int]&t2=[Int]"
-- >           h3 << "http://server/api/prod?t1=[Int]&t2=[Int]"
-- >   where
-- >   asks w= ask $ w >> stop
-- >   wint p= wparam p :: View Html IO Int
--
-- Can be called with:
--
-- > mainParser  = runNavigation "apiparser" . step . asks (parserService >> stop)
--
-- or
--
-- > mainParser =do
-- >   addMessageFlows[("apiparser",wstateless  parserService)]
-- >   wait $ run 80 waiMessageFlow
-----------------------------------------------------------------------------

module MFlow.Forms.WebApi (
restp, rest, wparam, disp,(<?>)
) where
import MFlow.Forms.Internals
import MFlow.Forms(stop,(++>))
import Control.Monad.State
import Data.Typeable
import Data.Monoid
-- | Get the next segment of the REST path. if there is no one or if the data does not match
-- with the type expected, then ir return invalid.
--  Therefore a monadic sequence in the View monad will not continue
restp :: (Monad m,Functor m, FormInput v,Typeable a,Read a) => View v m a
restp =  View $ do
   mr <- getRestParam
   return $ FormElm mempty mr



-- | check that the next rest segment has a certain value. It return invalid otherwise.
-- therefore a monadic sequence in the View monad will not continue
rest v= do
   r <- restp
   if r==v then return v
    else
     modify (\s -> s{mfPagePath= reverse . tail . reverse $ mfPagePath s}) >> stop

-- | get a parameter from a GET or POST key-value input.
wparam par= View $ do
   mr <- getKeyValueParam par
   return $ FormElm mempty mr

-- | append to the output the result of an expression
dispv :: (Monad m, FormInput v) => View v m v -> View  v m ()
dispv w= View $ do
   FormElm f mx <- runView w
   case mx of
     Nothing -> return $ FormElm f Nothing
     justx@(Just x) -> return $ FormElm (f <> x) $ return ()


-- | append to the output the result of an expression
disp :: (Monad m, FormInput v, Show a) => View v m a -> View  v m ()
disp w= View $ do
   FormElm f mx <- runView w
   case mx of
     Nothing -> return $ FormElm f Nothing
     justx@(Just x) -> return $ FormElm (f <>fromStr (show x)) $ return ()

-- | error message when a applicative expression do not match
infixl 3 <?>
(<?>) w v= View $ do
  r@(FormElm f mx) <- runView w
  case mx of
    Nothing -> runView $ v ++> stop
    Just _ -> return r



