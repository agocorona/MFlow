module TLSLogin where

import MFlow.Wai.Blaze.Html.All

main = runSecureNavigation "" . step $ do
    r <- page $ h3 << "Login"
        ++> userWidget Nothing userLogin
    return ()
