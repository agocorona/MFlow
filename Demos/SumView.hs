
module SumView (sumInView) where

import MFlow.Wai.Blaze.Html.All
import Menu

sumInView= askm $ p << "ask for three numbers in the same page and display the result.\
                      \It is possible to modify the inputs and the sum will reflect it"
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

-- to run it alone, replace askm by ask and uncomment the following line
--main= runNavigation "" $ transientNav sumWidget
