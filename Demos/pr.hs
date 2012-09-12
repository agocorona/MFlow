{-
:l demos/pr.hs
-}
import Control.Applicative

data ReadShow v a= ReadShow v Maybe a

instance Monoid v => Applicative( ReadShow v) where
  pure a  = ReadShow mempty a
  ReadShow v x <*> ReadShow v' y= 
                   ReadShow (mconcat v v') (x <*> y)

class ReadShow v a where
  doRead v -> Maybe a
  doWrite a -> v

doRead :: v -> ReadShow v a -> Maybe a

doWrite a  -> ReadShow v a -> v
