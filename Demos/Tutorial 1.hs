module Tutorial where

import MFlow.Wai.Blaze.Html.All
import Data.Monoid

-- widget signature : View rendering monad returnValue
-- flow aignature: FlowM rendering monad returnValue
-- page : View v m a -> FlowM v m a
main = runNavigation "" . step $ do
    r <- page  $  h3 << "Basic example with inputs and links"
              ++> getString (Just "text input")
              <* submitButton "OK"

    page $   b << ("you typed: "++ r)
         ++> p << "wlink's return typed values to the flow"
         ++> p << "in MFlow a page is a widget made of widgets"
         ++> wlink () << p << "next"


    r <- page  $  h3 << "Operators"
              ++> p << " The operator (++>) prepend HTML to a widget"
              ++> p << "in this case, a field that expect an Int"
              ++> getInt Nothing
              <! [("placeholder","This is an attribute for the getInt form")]
              <* submitButton "OK"
              <++ p << "the operator (<<) add text to a blaze-html tag in this case"
              <>  p << "If the OverloadedStrings extension is used, this is not necessary"
              <>  p << " Note that form tags are added automatically"

    page $ b << ("you entered: "++ show r) ++> wlink () << p << "next"


    r <- page  $   h3 << "Here the alternative operator is used to choose between two options"
               ++> wlink True  << b << "True" <++ br
               <|> wlink False << b << "False"

    page $ b << ("you entered: "++ show r)  ++> wlink () << p << "next"

    -- |+|    <+>

    r <- page $ ul << h3 << "More operators"
         ++> li << " (<<<) embed a widget in a tag"
         ++> li << " (|*>) intersperse a widget within a list of others"
         ++> ul << li << "in this case, a link is interspersed within a list of input fields"
         ++> ((h3 <<< wlink "next" << b << "next")
         |*> [getString Nothing <![("placeholder","enter field "++ show i)] | i <- [1..3]])
         <** submitButton "OK"

    case r of
      (Just _, Nothing) -> return ()
      (Nothing, Just s) -> do
                page $ p << ("you entered " ++ s ++ " in some box")
                   ++> wlink " next" << b << "next"
                return()

    page $ pageFlow "first" $ do
        r <- h3 << "Page flows: run within the View monad, within a page"
             ++> p << "The call \"pageFlow\" creates a set of unique identifiers and stores\
                      \the data entered during the steps of the pageflow to replay the computation\
                      \each time the page is refreshed."
             ++> p << "until the last monadic statement does not validate, the page Flow will execute again and again"
             ++> wlink True  << b << "True" <++ br
             <|> wlink False << b << "False"

        p << "Until the first line is not validated, the second does not execute"
          ++> p << ("you entered: "++ show r)  ++> wlink () << p << "click"

    page $ pageFlow "second" $ do
        h2 << "Field validators" ++> wlink () << p << "next"

        r <- p << "this field expect a string less than 5 characters. Otherwise it present a alert message"
             ++> getString Nothing
                `validate`
                (\r -> if length r > 5 then return . Just $ script << "alert ('length must be lsst than five chars ')"
                                       else return Nothing)
             <* submitButton "OK"

        p << ("you entered: "++ show r) ++> wlink () << p << "click"


    page $ pageFlow "third" $ do
        h2 << "A select/option box" ++> wlink () << b << "click" <++ br
        r <- getSelect
                        (setSelectedOption "" << p << "select a option"   <|>
                         setOption "red"      << b << "red"               <|>
                         setOption "blue"     << b << "blue"              <|>
                         setOption "Green"    << b << "Green")
                         <! [("onchange","this.form.submit()")]

        p << (r ++ " selected") ++> wlink () << p <<  "next"

    let colors= getCheckBoxes(  (setCheckBox False "Red"   <++ b <<  "red")
                             <> (setCheckBox False "Green" <++ b <<  "green")
                             <> (setCheckBox False "blue"  <++ b <<  "blue"))

    page $ pageFlow "four" $ do
        h2 << "checkboxes" ++> wlink () << b << "click" <++ br

        r <- colors <** submitButton "submit"

        p << (show r ++ " selected") ++> wlink () << p <<  " menu"

    page $ (do
       h3 << "A mix of applicative and monadic operators can be used to create multiple pageflows within a page"
        ++> p << "here two pageflows are composed with an alternative operator"
        ++> p << "the button triggers the presentation of the changes in both elements."
        ++> p << "they are part of the same HTML form, but they change their display depending on the input"
        ++> wlink "" << p << "click here"
       (pageFlow "colors" (do
         r <- colors <++ br
         p << (show r ++ "selected") ++> noWidget <++ br)
        <|>
        pageFlow "input" (do
         r <- getString Nothing  <++ br
         p << (show r ++"entered") ++> noWidget <++ br)

        <* submitButton "OK" ))
     <|> br ++> wlink "next" << b << "skip to the next"
            <++ p << "this link is alternative (<|>) to the rest of the page, so it can be pressed\
                     \to skip it at any moment"

    return ()

main= runNavigation "" .step . page (pageFlow "s" ( do
       n  <- getInt Nothing   <++ "first"
       n' <- getInt (Just n)  <++ "second"
       p << (n+n') ++> wlink () "click to repeat")
     <** submitButton "Send")
