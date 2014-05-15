



module Main where

import MFlow.Wai.Blaze.Html.All
import Data.Monoid


main = runNavigation "" . transientNav $ do
    r <- page  $  h3 << "Basic example with inputs and links"
              ++> getString (Just "text input")
              <* submitButton "OK"

    page $   b << ("you typed: "++ r)
         ++> p << "wlink's return typed values to the flow"
         ++> p << "in MFlow a page is a widget made of widgets"
         ++> wlink () << p << "next"


    r <- page $   h3 << "Other basic elements"
              ++> p << " the operator (++>) prepend HTML to a widget"
              ++> p << "in this case, a field that expect an Int"
              ++> getInt Nothing
                     <! [("placeholder","This is an attribute for the getInt form")]
              <* submitButton "OK"
              <++ p << "the operator (<<) add text to a blaze-html tag in this case"
              <>  p << "If the OverloadedStrings extension is used, this is not necessary"
              <>  p << " Note that form tags are added automatically"

    page $ b << ("you entered: "++ show r) ++> wlink () << p << "next"


    r <- page  $   h3 << "Here the alternative operator (<|>) is used to choose between two options"
               ++> wlink True  << b << "True" <++ br
               <|> wlink False << b << "False"

    page $ b << ("you entered: "++ show r)  ++> wlink () << p << "next"

    -- |+|    <+>

    r <- page $ ul << h3 << "Operators"
            ++> li << " (<<<) embed a widget in a tag"
            ++> li << " (|*>) intersperse a widget within a list of others"
            ++> ul << li << "in this case, a link is interspersed within a list of input fields"
            ++> ((h1 <<< wlink "next" << b << "next")
            |*> [getString Nothing <![("placeholder","enter field "++ show i)] | i <- [1..3]])
            <** submitButton "OK"

    case r of
      (Just _, Nothing) -> return ()
      (Nothing, Just s) -> do
                page $ p << ("you entered " ++ s ++ " in some box")
                   ++> wlink " next" << b << "next"
                return()

    page $ pageFlow "1" $ do
        r <- h3 << "Page flows run a monad within a page"
             ++> p << "note in the code that this sequence is within 'page'"
             ++> p << "The call \"pageFlow\" creates a set of unique identifiers and stores"
             ++> p << "the data entered during the steps of the pageflow"
             ++> wlink True  << b << "True" <++ br
             <|> wlink False << b << "False"

        p << "Until the first line is not validated, the second does not execute"
          ++> p << ("you entered: "++ show r)  ++> wlink () << p << "next"

    page $ pageFlow "2" $ do
        h2 << "Field validators" ++> wlink () << p << "next"

        r <- p << "this field expect a string less than 5 characters. Otherwise it present a alert message"
             ++> getString Nothing
                `validate`
                (\r -> if length r > 5 then return . Just $ script << "alert ('length must be less than five chars ')"
                                       else return Nothing)
             <* submitButton "OK"

        p << ("you entered: "++ show r) ++> wlink () << p << "next"


    page $ pageFlow "3" $ do
        h3 << "A select/option box" ++> wlink () << b << "click" <++ br
        r <- getSelect
                        (setSelectedOption "" (p << "select a option")   <|>
                         setOption "red"      (b << "red")               <|>
                         setOption "blue"     (b << "blue")              <|>
                         setOption "Green"    (b << "Green"))
                         <! [("onchange","this.form.submit()")]

        p << (r ++ " selected") ++> wlink () << p <<  "next"

    let colors= getCheckBoxes(  (setCheckBox False "Red"   <++ b <<  "red")
                             <> (setCheckBox False "Green" <++ b <<  "green")
                             <> (setCheckBox False "blue"  <++ b <<  "blue"))

    page $ pageFlow "4" $ do
        h2 << "checkboxes" ++> wlink () << b << "click" <++ br

        r <- colors <** submitButton "submit"

        p << (show r ++ " selected") ++> wlink () << p <<  " next"


    page $ pageFlow "5" ( do
       h3 << "A mix of applicative and monadic operators can be used to create multiple pageflows within a page"
        ++> b << "here two pageflows in a form are composed with an alternative operator"
        ++>  wlink "hi" << p << "click here"


       wform(pageFlow "colors" (do
                 r <- colors <++ br
                 p << (show r ++ "selected")  ++> br ++> noWidget)
           <|>
             pageFlow "input" (do
                 r <- getString Nothing  <++ br
                 p << (show r ++ "selected")  ++> br ++> noWidget)
        <** submitButton "OK" ))

     <|> br ++> wlink () << b << "back to the beginning"



