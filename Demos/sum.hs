{-# LANGUAGE  DeriveDataTypeable  #-}
module Main where
import MFlow.Wai.Blaze.Html.All
import Data.Typeable
import qualified Control.Workflow as WF -- delete

runServerAt port proc= do
  addMessageFlows  [("example",  transient . runFlow $ proc )]
  wait $ run port waiMessageFlow

main= runServerAt 80 mainFlow

data Opts= Sum | Navigation deriving (Typeable, Show)


mainFlow=  do
  r <- ask $   wlink  Sum << b << "sum three numbers" <++ br
           <|> wlink Navigation << b << "example of navigation"

  case r of
     Sum -> sumIt
     Navigation -> navigation
  return ()

sumIt= do
  setHeader $ html . body
  ask $ p << "Sum of three numbers" ++> wlink "" << p << "press here to go"
  n1 <- ask $  p << "give me the first number"  ++>  getInt Nothing
  n2 <- ask $  p << "give me the second number" ++>  getInt Nothing
  n3 <- ask $  p << "give a third number" ++>  getInt Nothing
  ask $ p << ("the result is " ++ show (n1 + n2 + n3)) ++> wlink () << p << "click here"


data Nav= Departments | Colors deriving (Typeable,Show)
data Color= Red | Green | Blue deriving (Typeable, Show)
data Department= Department1 | Department2 deriving(Typeable,Show)

navigation= do
    r <- ask $   wlink  Departments << b << "go to departments" <++ br
             <|> wlink Colors << b << "go to colors"
    case r of
       Departments -> do
         r <- ask $   wlink  Department1 << b << "department1" <++ br
                  <|> wlink  Department2 << b << "department2"
         ask $ p << ("this is the page of "++ show r)++> wlink () << b << "press here"
       Colors -> do
         r <- ask $   wlink  Red << b << "department1" <++ br
                  <|> wlink  Green << b << "department2" <++ br
                  <|> wlink  Blue << b << "department2"
         ask $ p << ("this is the page of "++ show r)++> wlink () << b << "press here"

