{-# LANGUAGE  DeriveDataTypeable #-}
import Haste.HPlay.View
import Haste.HPlay.Cell
import Haste.Perch
import Haste
import Haste.LocalStorage
import Control.Applicative
import Prelude hiding (div,span,id,all)
import Data.Typeable
import Control.Monad(when)
import Control.Monad.IO.Class
import qualified Data.IntMap as M
import Data.Monoid
import Haste.Serialize
import Haste.JSON (JSON(..))



data Status= Completed | Active deriving (Show,Eq,Read)

type Tasks = M.IntMap  (String,Status)

instance Serialize Status where
  toJSON= Str . toJSString . show
  parseJSON (Str jss)=  return . read  $ fromJSStr jss

data PresentationMode= Mode String deriving Typeable

all= ""
active= "active"
completed= "completed"

main= do
  addHeader $ link ! atr "rel" "stylesheet" ! href "base.css"
  runBody todo

todo ::  Widget ()
todo = do
      section ! id "todoapp" $ do
        nelem "header" ! id "header"
        section ! id "main" $ do
          ul ! id "todo-list"  $ noHtml

        footer ! id "footer"  $ do
          span ! id "todo-count" $ noHtml
          ul ! id "filters" $ noHtml
          span ! id "clear-holder" $ noHtml

      footer ! id "info" $ do
        p "Double-click to edit a todo"
        p $ do
           toElem "Created by "
           a ! href "http://twitter.com/agocorona" $ "Alberto G. Corona"
           p $ do
              toElem "Part of "
              a ! href "http://todomvc.com" $ "TodoMVC"

     ++> header                       --             ++> add HTML to a widget
     **> toggleAll
     **> filters all
     **> itemsLeft
     **> showClearCompleted           --             **> is the *> applicative operator

 where

 itemsLeft= at "todo-count" Insert $ do
    n <- getTasks >>= return . M.size . M.filter ((==) Active . snd) . fst
    wraw $ case n of
      1 -> do
                strong "1"
                toElem " item left"
      _ -> do
                strong (show n)
                toElem " items left"

 showClearCompleted= at "clear-holder" Insert $ do
    (tasks,_) <- getTasks
    let n =  M.size . M.filter ((==) Completed . snd)   $ tasks
    when (n >0) $ do
        (button ! id "clear-completed" $ "Clear completed") `pass` OnClick
        setTasks $ M.filter ((==) Active . snd) tasks
        displayFiltered

 filters op =at "filters" Insert $ filters' op

    where
    filters' op= (links op `wake` OnClick)
      `wcallback` (\op' -> do
        setSData $ Mode op'
        displayFiltered **> return ()
        filters' op')

    links op=
        li ! clas op all       <<< wlink all        (toElem "All")     <|>
        li ! clas op active    <<< wlink active     (toElem "Active")  <|>
        li ! clas op completed <<< wlink completed  (toElem "Completed")

    clas current op= atr "class" $ if current== op then "selected" else "unsel"

 header = at "header" Insert $ h1 "todos" ++> newTodo

 toggleAll = at "main" Prepend $ do
    t <- getCheckBoxes $ setCheckBox False "toggle" `wake` OnClick ! atr "class" "toggle-all"
    let newst=  case t of
            [] ->  Active
            _  ->  Completed
    (tasks,_) <- getTasks
    filtered  <- getFiltered tasks
    let filtered' = M.map (\(t,_) -> (t,newst)) filtered
        tasks'    = M.union filtered' tasks
    setTasks tasks'
    displayFiltered
    itemsLeft

 displayFiltered = do
    (tasks,_) <- getTasks
    filtered <- getFiltered tasks
    at "todo-list" Insert $ foldl (<|>) mempty $ reverse
      [display  i | i <- M.keys filtered]
   **> return ()

 getFiltered tasks= do
   Mode todisplay <- getSData <|> return (Mode all)
   return $ M.filter (fil todisplay)  tasks
   where
   fil  f (t,st)
     | f==all=   True
     | f==active = case st of
                         Active -> True
                         _      -> False
     | otherwise = case st of
                         Completed -> True
                         _         -> False


 newTodo= do
      let entry= boxCell "new-todo"
      task <- mk entry Nothing `wake` OnKeyUp
                ! atr "placeholder" "What needs to be done?"
                ! atr "autofocus" ""
      EventData evname evdata <- getEventData
      when( evdata == Key 13) $ do
         entry .= ""
         idtask <- addTask task Active
         Mode m <- getSData <|> return (Mode all)
         when (m /= completed) $ at "todo-list" Prepend $ display idtask
         itemsLeft

 display idtask =
   (li <<< ( toggleActive  **> destroy)) `wcallback` const (delTask idtask)

   where
   toggleActive = do
        Just (task,st) <- getTask idtask
        let checked= case st of Completed -> True; Active -> False
        ch <- getCheckBoxes $ setCheckBox checked "check" `wake` OnClick ! atr "class" "toggle"
        case ch of
          ["check"] -> changeState  idtask Completed task
          _         -> changeState  idtask Active task

   destroy= (button ! atr "class" "destroy" $ noHtml) `pass` OnClick

   changeState  i stat task=
          insertTask i task stat
      **> itemsLeft
      **> showClearCompleted
      **> viewEdit i stat task

   viewEdit idtask st task = do
        let lab= case st of
                 Completed  -> label ! atr "class" "completed"
                 Active     -> label

        lab  task `pass` OnDblClick

     `wcallback` const (do
            ntask <-  inputString (Just task) `wake` OnKeyUp ! atr "class" "edit"
            EventData _ (Key k) <- getEventData
            continueIf (k== 13) ntask

       `wcallback` (\ntask -> do
            insertTask idtask ntask st
            viewEdit idtask st ntask))

-- Model, using LocalStorage

getTasks :: MonadIO m  => m (Tasks, Int)
getTasks= liftIO $ do
    mt <- getItem "tasks"
    case mt of
      Left _ ->   setItem "tasks" (M.toList (M.empty :: Tasks),0 ::Int) >> getTasks
      Right (ts,n) -> return (M.fromList ts, n)

getTask i= liftIO $ do
     (tasks,id) <- getTasks
     return $ M.lookup i tasks

setTasks :: MonadIO m => Tasks -> m ()
setTasks tasks=  liftIO $ do
      (_,n) <- getTasks
      setItem "tasks" (M.toList tasks,n)

addTask :: String -> Status -> Widget Int
addTask task state= liftIO $ do
     (tasks,id) <- getTasks
     let tasks'= M.insert id  (task, state) tasks
     setItem "tasks" (M.toList tasks', id+1)
     return id

insertTask id task state= liftIO $ do
     (tasks,i) <- getTasks
     let tasks'= M.insert  id (task,state) tasks
     setItem "tasks" (M.toList tasks', i)
     return tasks'

delTask i= liftIO $ do
   (tasks,_) <- getTasks
   setTasks $ M.delete i tasks
