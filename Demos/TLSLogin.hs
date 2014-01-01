module TLSLogin where

import MFlow.Wai.Blaze.Html.All

--Expects certificate.pem and key.pem in project directory.

main = runSecureNavigation "" . step $ do
    r <- page $ h3 << "Login"
        ++> userWidget Nothing userLogin
    return ()
