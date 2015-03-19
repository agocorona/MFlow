{-# OPTIONS  -XCPP #-}
module SumView (sumInView) where

-- #define ALONE -- to execute it alone, uncomment this
#ifdef ALONE
import MFlow.Wai.Blaze.Html.All
main= runNavigation "" $ transientNav grid
#else
import MFlow.Wai.Blaze.Html.All hiding(retry, page)
import Menu
#endif

sumInView= page $ p << "ask for three numbers in the same page and display the result."
              ++> p << "It is possible to modify the inputs and the sum will reflect it"
              ++> sumWidget
               
sumWidget=   pageFlow "sum" $ do
      n <- do
           n1 <- p << "Enter first number"  ++> getInt Nothing <++ br
           n2 <- p << "Enter second number" ++> getInt Nothing <++ br
           n3 <- p << "Enter third number"  ++> getInt Nothing <++ br
           return (n1+ n2 + n3)

          -- factoring out the button
          <**  br ++> pageFlow "button" (submitButton "submit")
           
      p <<  ("The result is: "++show n)  ++>  wlink () << b << " menu"
      <++ p << "you can change the numbers in the boxes to see how the result changes"

-- to run it alone, replace page by ask and uncomment the following line
--main= runNavigation "" $ transientNav sumWidget
