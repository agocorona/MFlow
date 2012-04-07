mezclar views en la misma pantalla

instance (Monoid v,ConvertTo view1 v, ConvertTo view2 v) => View wiew1 m a -> View view2 m b



after login other user blocked:
 DeleteFile "TCacheData/Workflow/Stat#admin#anon#@admin": does not exist (El sistema no puede encontrar el archivo especificado.)


meter MFlow en github
factorizar ask

instances de FormView para test load


add nsquence to serialization

refSerialize que evite variables
  redefinir showp[]
    con showp todos los elementos
  modos adicionales de serializar lineas listnorefs

wform debe chequear si needForm? no hace falta

caching
if th user is admin or it run in localhost, show errors in pages. if not, log them
no valida si se hace enter en un campo
Serialize stat evitar salvar cuando esta em modo recovery
no pone acentos. usar Text en StoryReader
nombres unicode y getDirectoryContents
manejo de errores en worflows
  si se borra on error, desaparece todo el estado de usuario
  si se cambia el flujo en recovery, errror de serializacion
  si no se cambia nada, reproduce el errror.
loopStep que guarde el log, pero que sea un paso en el log.


> newtype BackT m a = BackT { runBackT :: m (FailBack a ) }
> type  WState view m = StateT (MFlowState view) m
> type FlowM view m=  BackT (WState view m)
> data FormElm view a = FormElm [view] (Maybe a)
> newtype FormT view m a = FormT { runFormT :: StateT (MFlowState view) m (FormElm view a) }
> type View view m a= FormT view (WState view m)  a


getUser por que pregunta usuario cuando abro nuevo navegador


probar backT con step


chequear iteraciones con valores introducidos cuando falla la validación
  leer de Env con getParam1



fileserve path _= do
  case getParam1 "path" of
    Nothing -> error "path not found"
    Just pth -> cached readFile pth


reduce tuples by type:

instance ConvertTo (Maybe (Maybe a, Maybe b), Maybe c) (Maybe a, Maybe b, Maybe c)
  converTo ((ma,mb),mc)= (ma,mb,mc)

instance ConvertTo (Maybe(Maybe (Maybe a, Maybe b), Maybe c),Maybe d) (Maybe a, Maybe b, Maybe c, Maybe d)
  converTo ((Maybe(ma,mb),mc),md)= (ma,mb,mc,md)

instance ConvertTo a b, Append b c=> ConvertTo (a,c) (b,c) where
  convertTo(x,y)= append(convertTo x,y) z

class Append b c d where
   app :: b -> c -> d

instance Append (a,b,d) c (a,b,d,c) where
   app (x,y,t) z = (x,y,t,z)

instance Append (a,b) c (a,b,c) where
   app (x,y) z = (x,y,z)

instance convertTo (a

data Intersperse w x= Intersperse w [x]

instance Widget (Intersperse w w1) (w,w1) m view where
 widget(Intersperse w xs)= do
  Form form _ <- widget(Intersperse w xs)= fold (x1 <+> w <+> x2)   w  xs
  Form _ rxs <- widget xs
  Form _ rw  <- widget w


Como evitar doble llamada a functor
  para crear form
  para validar input


newType MonadBack m a= MBack (runMBack :: m a}
class Monad m => Monad (MonadBack m) where
  f >>= (MBack g)= loop
     where
     loop= do
       x <- f
       y <- g
       if backCond y then loop else y
  return f= f

type FlowM view = StateT (MFlowState view)

convert wf log de uno a otro proc

class GetParaM req param where
 getParam :: req -> param

Getparam

usuario password (login)  repeat password (register)

User <?> getString Nothing)<*> getPassword <+> (submitButton "login")
     <+> span << "repeat password" <++ getPassword (button "register")

widget
versioning TCache


data Migrable a= Migrable String a



class migrate (Ver a


versioning Workflow

añadir un paso mas como modificar la funcion

exception IDynamic Preguntar usuario que valor insertar/borrar

version de workflow

widget(getUserWidget <+> getUserWidget <+> getUserWidget) :: View Html IO ()

data users= Map User Password

instance Indexable users where
  key= const "Users"

instance Serializable User where
  setPersist= const $ Persist read write  delete
  where
  read k= do
     users= atomically readDBRef rusers
     return $ lookup k users
  write u= do update

lectura de ficheros:
problema datos y texto en ficheros potencialmente muy extensos
 presentación por trozos


persistence in file
leer un fichero muy largo

data Pointer= Pointer File Seek


read :: Pointer -> String

q


-- |
data Persist = Persist{
       readByKey   ::  (String -> IO(Maybe B.ByteString)) -- ^  read
     , write       ::  (String -> B.ByteString -> IO())   -- ^  write
     , delete      ::  (String -> IO())}       -- ^  delete


MFlow.hack.XHtml.All

class Typeable a, Typeable b, Iterable a b c, Map Maybe c mc
      => TCast c mc where
  tcast ::  Dynamic -> mc


instance Iterable a b c,Typeble a, Typeable b)
          => TCast (Map (Maybe c))


como elegir los tipos de documentos para crear en cada grupo:
WFName, prototipo con reify

como saber los tipos de documnetos que puede editar un usuario?
cuando se crea un subject, se añaden permisos

reifyType  :: [(Perms, DBRef group)] -> IO a

reifyType [Create, gr] :: IO Document

lista de recibidos por grupo

cuando crea un documento, a que grupo lo manda? que workflow
aplica?.

definir documento -> workflow tipos de workflows
tipos de propuestas

workflow
grupo
usuario

un workflow puede usar varios grupos

antes: tipos  WR asociados al grupo: los WF que se podian
iniciar en el grupo.
nuevo:
  hay varios tipos de documento
     asociar WF al tipo de documento o al documento?
     al segundo. problema: dejar al usuario elegir?
        otro usuario puede enmendar.
     mejor lo primero

  hay varios grupos
    el WF se da de alta en el grupo que lo inicia.

    hay que asociar al grupo tipos de documento y WFs
    varios tipos por WF

   grupo lista de WFs
     un WF-> lista de tipos de docs


definir niveles de propuestas subject <-> nivel <-> workflow
tipo: sale un desplegable con los workflows
un grupo puede tener varios workflows

data BodyElems view = Link view
                    | forall a b.Typeable b,Digest2 a b => Form a


type Body= [BodyElems]

instance Digest2 Body Dynamic where
  digest2 xs val=FormM $ \env -> do
      rs <- mapM (\x ->runFormT  $  digest2 x val env)
      case  filter (\ Form _ mj -> isJust mj) rs
         [r] -> return r
          []  -> return  $ Form (map (\Form f _ ->f)  rs) Nothing
         _ -> error "Digest2 Body problem"

instance Digest2

return  (1+) <$> return  (2) <*> return 3  <*> return 4

Control.Workflow.UserDefs.User <$> digest [] Nothing
                       <*> digest [] Nothing
                       <*> (return (Form [] []))
                       <*> digest [] Nothing

askList:: (GetLine a, Digest a) =>
         => Token -> Params -> [a] -> IO  [a]

askList xs= do
   send t form .comumn . map getLine xs
   receiveReq t


Form view a = Form view a

newtype FormT view a = FormT { runFormT :: m (From view a) }


instance (Functor m, Monad m) => Applicative (FormT view) where
  pure a  = FormT $ return (Form [] a)
  FormT f <*> FormT v = FormT $ f >>= Form form1 k ->
    v >>= Form form2 x->  return (Right (k x))

class  Digest  a  view where
   digest ::   Params -> IO (Form [view] a)

instance (Digest a  , Digest b  ) => Digest (a,b)   where
  digest prms= do
      Form f1 a <- digest prms
      Form f2 b <- digest prms
      return Form (mappend f1 f2) (a,b)

ask t req page= do
         Form form x <-  digest req
         case form of
           [] -> return x
           _ -> do

             send t $ mappend (column $ form) page
             req <- return . getParamms =<< receiveReq t
             ask t req page




instance (Monad m) => Monad (MEitherT m) where
   fail _ = MEitherT (return Nothing)
   return = lift . return
   x >>= f = MEitherT $ do
       v <- runMEitherT x
       case v of
           Nothing -> return Nothing
           Just y  -> runMEitherT (f y)



instance Monoid e => Monad (Form e) where
    return x =  Form [] x
    Form f1 x >> Form f2 y ->  Form  $ mappend errs1 errs
        (MLeft errs, MRight _) -> MLeft errs
        (MRight _, r) -> r


    x >>= f = case x of
        MRight r -> f r
        MLeft errs ->  MLeft errs


hay que decorar el form (con page?)
ask t req page= do
         mx <-  digest req
         case mx of
           MRight x -> return x
           MLeft  msgs -> do
             send t $ mappend (column $ msgs) page
             r <- receiveReq t >>= digest  . getParams

             case   r  of
               MRight x  -> return x
               MLeft msgs -> ask t  [] $ mappend (column $ map fromString msgs) page




otros problemas como componer:

data X a b= X a b

instance Digest a view where
 digest env=
           x <- digest env
           y <- digest env
           return $ do
               x' <- x
               y' <- y
               return $ X  x' y'


result <- runMaybeT (MaybeT foo >>= MaybeT bar >>= MaybeT baz)

newtype MEitherT m a = MaybeT { runMEitherT :: m (MEither a) }

instance (Monad m) => Monad (MEitherT m) where
   fail _ = MEitherT (return Nothing)
   return = lift . return
   x >>= f = MEitherT $ do
       v <- runMEitherT x
       case v of
           Nothing -> return Nothing
           Just y  -> runMEitherT (f y)


Form a= MEither view a

form :: Form a
form= X <$> digest a <$> digest b


usar un segunda key como clave.
tiene asocuado un Map segCamp pKey

join or reference:

> data Person= Person{ name :: String, cars :: [DBRef Car]}
> data Car{owner :: DBRef Person ,name:: String}

> registerModifyTrigger (\car@(Car powner _ ) ->
>  withDBRef powner $ \m case m of
>      Just owner -> writeDBRef powner owner{cars= nub $ cars owner ++ car]


> main= do
>    bruce <- newDBRef $ Person "Bruce" []
>    withResources [] $ const [Car bruce "Bat Mobile",
>                             ,Car bruce "Porsche"]
>    print $ cars bruce


pathom types

data Expr a = Expr PrimExpr

constant :: Show a => a -> Expr a
(.+.)  :: Expr Int -> Expr Int -> Expr Int
(.==.) :: Eq a=> Expr a-> Expr a-> Expr Bool
(.&&.) :: Expr Bool -> Expr Bool-> Expr Bool

data PrimExpr
  = BinExpr   BinOp PrimExpr PrimExpr
  | UnExpr    UnOp PrimExpr
  | ConstExpr String

data BinOp
  = OpEq | OpAnd | OpPlus | ...

------------
selectors

type  Collection v = Collection  Vector (DBRef v)

data Selector v= LT v `In` Collection v | EQ v | And Sel v Sel v....

expand :: a (Selector v) -> [a v]




readResource puede no depender de la key
por tanto un prototipo con un valor incompleto puede servir
para recuperar una colección
readResources :: a -> [a]

readResource :: a -> a

readResourceByKey :: String -> a

Para que puede servir readResources?
para

Select a= All | Only a | LEqual a | GThan a


instance Functor Tree a =>


data Emp name company= Emp{name :: key , company :: company ....}

data Emp (Select Nombre)(Select Company)

instance Functor (Emp n c ) where
   fmap f emp= emp {name= f $ name emp, company= f $ company emp...


-- elimina todos los All
class CacheExpland a selector where
  expand :: a (selector s) -> IO [a s]

class CacheExpland2 a  selector selector' where
  expand :: a (selector s) (selector' t) -> IO [a s t]


  expand Emp{name = GT "B", company="jljljl"..}=

instance Expansor (selector x) where
  expansor All   =  Index [key]
  expansor Only x=

initSelector x=

instance IResource (a s)=> IResource a (selector s)


-------------------
DBRef  RRef

data DBList a= DBList  a

readDBList (DBList a)= getListResources a

getListResources :: [a] -> [Maybe[a]]


..............

strong deserialization sin necesidad de registerType


array of types

strongSerialize x=
     registerType x -- add to a serializrable vector
     hasString typeOf x ++ serialise x

strongDeserialize  str=
   let n= read
   deserial=   vector ! n

vector= Vector (typeRef,deserialize, readp)
show  vector= typeReps

deserialize vector=


mewtype DBRef a= DBRef TVar (Either Key (Elem a))

data Elem a= Elem{ key :: String, inDBRef :: Bool, value :: aNY_PORT
                 , modifyTime, accessTime :: Integer}

inDBRef sirve para saber si eliminar el TVar del cache o no
si es parte de una DBRef instanciada con newDBRef, entonces se mantiene
en el cache.

como saber si una DBRef ya no se se utiliza?
un DBRef con TVar Nothing, como se elimina del cache si no se usa?
problema: si esta linkado en el cache no se ejecuta el onDelete
si no esta en el cache, no se le puede recargar

Definir data  DBref1 a= DBRef1 (DBRef a)y solo meter en cache DBRef
----------------------
triggers



data TriggerType= OnCreateModify | OnDelete

data Trigger= forall a. (IResource a, Typeable a) => Trigger TriggerType TypeRep (a -> IO()

triggers :: IORef [Trigger]


registerTrigger :: (IResource a, Typeable a) => TriggerType -> (a -> IO()) -> IO()
registerTrigger t=  atomicModifyIORef (ts -> t:ts)

applyTriggers:: (IResource a, Typeable a) => TriggerType -> a -> IO()
applyTriggers applytype a = do
   ts <- readIORef triggers
   mapM_ (f a) ts
   where
   f a (Trigger ttype type t)=
     if  applytype==ttype&& typOf a == type
        then  t a
        else  return()

Web monad context
  Params
  lang
  userName


mixer de monads:

class SwitchMonads m n where
 switch :: m a -> (a -> n b) -> n b

>>=>

instance SwitchMonads (Either msg) Maybe where
  (Left _) `switch` f= Nothing
  (Right x) `switch` f= f x

instance SwitchMonads (Either msg) (IO Maybe) where
  (Left _) `switch` f= return Nothing
  (Right x) `switch` f= f x

una RefVar es o una referencia auna tupla o la clave para accederla
data Ref x= TVar cacheElem | RKey x

data Elem a= TVar (Elem a AccessTime ModifTime) | EKey String a

type TPVar a=   IORef   (Elem a)  deriving Typeable

una estructura con References:

data Struc X= Struc{ a: Ref A, b:: Ref B}

x= Struct (newRef A a) (newRef B b)

a'= takeRef (a x)


como identificar el index de un usuario

cada mensaje tiene que tener asociado un grupo
 para que? para que el usuario sepa de que grupo viene
 el grupo está asociado a unaq cola
 la cola depende de un rol
 rol = grupo = cola
 rol de usuario debe ser una lista

como se indexan los usuarios?

ccomo se asignan los verbos?

actions= [(String, Params -> view)]

como elegir los verbos
view edit
view vote delegate
dejarlo como datos en la segunda estructura
enm la segunda estructura, se pone

data List a dat view= ()
  IResource a, Typeable a, Digest a, Editable a view)
  => List a dat todo  deriving Typeable

actions=[("view/vote", mappend (view a) (vote dat)), ("delegate", delegate dat

----
diferencia entre ask y editElem?
 ask es sobre un objeto
 editElem es sobre una lista de objetos
 se necesitan varios

hace falta una clase que
 permita editar
 permita votar
 datos a editar:
   (obj, data)

   verbos sobre obj:
       vew
       edit
       viewLine
   sobre data:
       user defined




 instance Editable a -> editable List a

 instance Digest a => Digest (List a)

instance Editable obj, Editable data => Editable List(obj,data)
  showLine p (List (o,d))= row $ showLine p o hsep showLine p d
  render p (List (o,d)= column $ render p o hsep render p d
  getForm  prms (List x)= do
       Verb v <- digest prms
       case v

se puede usar siempre modify?
  process permite que editElem retorne siempre un valor
  Digest de la cola entera retorna la cola entera

  Digest de un elemento retorna un elemento
  necestiamos Digest de la cola y que retorne un elemento editado


  como se hace un flujo si no hay process?
  procesando una propuesta
    propuestas, colas con Keys de propuestas

  editEleme= do
      guardar objeto
      meter key en cola
      forkIO $ ask queue
      wait timeout
      leer objeto

instance (Digest x , ShowLine x, Render x) => Digest Queue x
   digest params= editElem

composición (objeto, load)

instance Editable


generador de forms. no actualiza la pagina de form.

ask:: (Digest model view) => Token  -> IO model
ask t = r where r=do
         page <- gerForm r
         send t  page
         r' <- receiveReq t >>= digest  . getParams

         case   r'  of
           MRight x  -> return x
           MLeft msgs -> ask t $ mappend (column $ map fromString msgs) page


instance Monad (MEither e) where
    return =  MRight
    x >> f = case x of
        MLeft errs -> case f  of
                      MLeft errs1 ->  MLeft  $ errs1 ++ errs
                      _ -> MLeft errs
        MRight r -> f r

---
x >> f=

data Options = Approbal | ChooseOptions String Int [Option] deriving (Read,Show,Eq)
data Option= Option String Status deriving (Read,Show,Eq)

newtype Priority= Priority Int  deriving (Eq, Ord)
type PriorIVote = (Priority, IndexVote)

type Percent= Float
data Status = Draft | Processing | Approbed Percent |
              Rejected Why |Closed Status | Voted Status deriving (Read,Show,Eq)




options         :: Options                  -- options to vote
votes           :: DiffArray  Int PriorIVote-- (representant priority, option voted) array
sumVotes        :: DiffUArray Int Int       -- total votes per option




create >>= sendGroup >>= vote_edit >>= applylaw >>=

vote_edit que incluye?
  autenticacion= convert $ texto HTML
      userPasswordRequest :: req

      autenthicate :: Digest User req ->  User
      autenthicate = ask userPasswordRequest

  grabar objeto
  manejo de Key
  manejo de listas
     lista usuario, grupos
        opciones por objeto , no por usuario
     lista de cosas a votar a editar
  manejo de opciones de acciones
      responder en cola de respuesta
      modificar en cola de recepcion
      modificar el objeto original

  editar objeto
  editar los votos

pasar todo a Users.hs?
  class lookup String para obtener parametros
       class Digest

  instance convertTo String a

Proposal Type object votes

type es el tipo de propuesta para que aplique las representaciones y los
criterios de evaluación de votos.


type tiene que ser uno de los tipos editados en la propuesta de constitución

grupo: nombre, constitución




writeResource Key _ obj= writeResource obj??


data Key a= Key String a

instance (Show a, Read a) => IResource (Key a) where
  keyResource (Key k _)= k
  serialize (Key _ x)= show x
  deserialize str= Key undefined . read
  readResource (Key k _) = readResource >>= return  . Key k
  writeResource (Key _ x)= writeResource x

instance IResource a => IResource Key a)
  keyResource (Key k _)= k
  serialize (Key _ x)= serilize x
  deserialize str= Key undefined . deserialize
  readResource str = readResource str >>= \x -> return  $ Key (keyResource x) x
  writeResource (Key _ x)= writeResource x

newType Hash= Hash Integer
data Proto a= Proto (IORef Hash) a
proto a = Proto (writeIORef..)

hashString (Data hash _)= unsafeCoerce hash

.....
mantener colas por usuario e ind, no por workflow
meterlas en una estructura temporal. un tchan en runnongworkflows?
-----
como usar tipos para evitar errores


newType WFName= WFName String
newType ObjKey= ObjKey String

Token= Token{wfname,user, ind :: String, q, qr :: Queue}



------------------
que hacer con las colas en webScheduler?
 - crear un Map tokenName (TChan,TChan)
-- meterlas en el Stat , convertir Token en Stat
   pasar ese stat en los transient workflows
   lo natural seria tener un send
---
que pasa cuando un send envia a otro workflow y el receive se queda bloqueado en
espera?

getState no matar el thread cuando cambia de primitiva
un token puede ejecutar mas de un workflow. eso para permitir workflows
de larga vida, como cestas de la compra.
inconvenientes: detectar
    Es un Map token workflow. hay que convertirlo en una map tokenworkflow thread


admintir TCache para que permita indeººxar por mas de un campo
usar otherKeysResource
readresource que retorne el primero
crear un elemento nuevo sin TVar que sirva solo para consulta y tenga una
lista de claves principales de elementos que tienen  la misma clave secundaria
o bien utilizar el mismo TVar, y definiendo un objeto lista que agrupe los que tienen la misma
clave secundaria.

evitar usar registerType usando types en lugar de hexadecimal
definir una tabla de equivalencias hexa-> string en la parte where de
refSerialize:
where  Vab4567= "Control.Workflow.Queue"


programar TPVars sobre TCache

usuarios  roles

el usuario puede ver una lista o puede procesar elementos como en un workflow
en el segundo caso, los elementos deben ser eliminados
en el primero no.
como se procesan enmiendas?
 parece mas logico presentarlo como ediciones de elementos comunes
 mas que procesos de colas.




data User= User{name, password , role:: String,  in, out :: Queue} | Workflow String

como indexar un field con operaciones:



--como indexar un field con operaciones:

{-------------------
class (IResource a) =>  ToIndex a where
   toIndex :: Ord b =>  a -> [(String, b)]



data Index b = Index{nameIndex :: String, index :: M.Map b [String]}


addtoIndex x= do
   let indexes= toIndex x
   map add indexes
   where
   add (index,v)=
      withResources [Index index undefined] $
      \[midx] -> case midx of
         Just(Index index map) -> Index index $ M.insert v (keyResource x) map
         Nothing -> Index index M.singleton v (keyResource x)
-}




-------------------
class (IResource a) =>  toIndex a where
   toIndex :: Ord b => [ (a -> b)]



data Index b = Index String  [(Mapb [Key])]
toIndex :: a -> (a-> b) -> String
addtoIndex user role nameindex

getElems nameIndex roleValue=...lookup
getAllElems nameIndex = .... concat $ elems map

and index value

---------------------

autenticacion

atenticate  añadir a messageFlow

register user password role

autenticate user password
----------------
manipulaciones Objetos

data Object a= Object a


messageflow para aprobación

el workflow utiliza varios usuarios que entran, no un solo usuario como en un workflow.
cada usuario necesita un dialogo de una o varias pantallas, por tanto no se puede conectar a un messageflow tal como
esta ahora.
verbos:

asociar a cada usuario autenticado dos colas in out

waitFor user msg
waitFor workflow msg
  user puede tener asociado siempre un workflow y en ese caso se puede obviar la primera forma?
  en teoria si
  puede ser un workflow parametrizado por un nombre de usuario o rol? por ejemplo

  waitFor aprobación boss documento= add this document to the workflow queue.
    luego el workflow no puede usar el documento como parametro porque en su cola puede haber mas de un documento
    no sirve startWF wf ... doc . hay que usar colas.

    ese workflow en el que entran documentos y entran usuarios puede conectar directamente con web?
    como abstraerlo del interface?

    el proceso tiene que tener dos colas, una para documentos (para hacer cosas con ellos, como aprobarlos etc) y otra para usuarios. como se modela?
    se trata de un proceso que presente la lista de objetos al usuario y el conjunto de verbos que puede ejecutar con cada uno
    para ello cada documento tiene que tener una lista de acciones asociado
    tiene que modelizarse como un mail. la presentacion depende del Interfaz, no de messageFlow.
    cada item tiene que tener una o varias actions.
 data Item a= Item a [Action]

 data Action= forall a.IAction a => Action Name a

 conjunto de acciones fijado (editar, aprobar..) o libre ambos dependen del interfaz.
 el usuario escoje una accion
 class IAction  a b where
     exec :: a -> IO b

 pero edit no se puede codificar abstrayendose del interface

 data Approbal = Approbal

 iTask: editTask  obj pide entrada de datos al usuario.

 patterns: sequence, recursion, exclusive choice, multiple choice, split/merge (parallel or,
 parallel and, discriminator), ...

interfaces para web services


