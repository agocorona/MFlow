-----------------------------------------------------------------------------
--
-- Module      :  MFlow.Load
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

module MFlow.Load (

) where
import MFlow.Cookies
import MFlow.Hack
import MFlow.Forms.Load
import System.Random

instance Processable  Params   where
    pwfname (Req x)= fromMaybe "noscript" $ "wfname" x
    puser (Req x)= fromMaybe anonymous $ lockup "cookieuser" x
    pind (Req x)= pind x   
    getParams =id

load wfname user ind wfs= load1 basemsg
  where
  basemsg= [("wfname", wfname),(cookieuser, user),(flow,ind)]

  load1 msg= do
      loads <- msgScheduler msg wfs
      let (links, formFields)= partition isLink loads
      rl <- retLink links
      ri <- mapM retInputs loads
      load1 $ basemsg ++ catMaybes (rl ++ ri)

  isLink (ShowString _ "Link")= True
  isLink _ = False

  retLink=
  retInput None _= return Nothing
  retInput Numeric par _ _= randomIO >>=\ri return $ Just (par, ri)
  retInput ShrtString par _ _= randstring >>=\rs -> return $ Just(par,rs)
  retInput LongString par _= randlstring >>= \rs -> return $ Just(par,rs)

  randint   = randomIO
  rand short= let Gen s= listOf1 (arbitrary :: Char)

partitio
