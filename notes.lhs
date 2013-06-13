si no matching completo de path, que de error

parameter numbers in monadic view
   afecta a todos los operadores
   ahora se exige que el recorrido de paramentros sea el mismo
   eso no se puede forzar, porque el recorrido monadico puede ser infinito
     dar un indice a cada paso
     resetear en contador actual en cada paso
   el problema es equivalente a numerar un arbol de la msma forma
   siempre aunque crezca.
      hay que usar un nuevo contador a cada nivel

                 1
            11         12
         11 112 113  121 122

                1           2
             13    14     15  16
           27 28  29
   otra opción asignarles un numero y almacenarlo en el widget

       FormElm (Maybe(IORef Int))  [v] (Maybe x)
         no vale cuando se juntan dos o mas formelems
       FormElm Int [v] Maybe
       los formelem son generados y destruidos

   generar un arbol de asignaciones:
           1   2         1      3
                        12


   guardando valores de parametros en cada interacción
      como se puede detectar un cambio en el arbol?
         se detenctan cuales son los parametros nuevos
         se desplazan los viejos:

         1 2 3 4 5
         llegan
         1 2 3 4 5 6 7 8

         hay tres parametros nuevos, pero donde estan insertos?

         pueden ser todos del mismo tipo (ejm: contadores)
         no se puede detectar de donde son.



restarting flag

dynamic envelope:
  detect changes in the internal widget

  send it by javascript by means of a ajax proc watching for requests


  send to ask only the ajax code to respond requests


links back

  link function

  return in the mflow monad
     que quiten
  que cuando se matchee el ultimo token del path en un recorrido

x backtracking no funciona en form in the view
   cuando cambia el path, borrar los parametros.
   en receiveReqTimeout
      if pathchanged borrar old params.

links de retorno a veces no funcionan

tail path  do not allow backtracking
  but tail path in callbacks do not work

shop da error

content management textos vacios

necesitamos un sistema de matching que haga asocie una entrada del path con un link concreto

hacer el map de links que no se resetee
o que cuando hay iteracuión, que se conserve el map de links

seaside multicounter
http://seaside.st/about/examples/multicounter

counter n= do
   r <- ask $ p << show n ++> wlink "+" "+" <|> wlink "-" "-"
   case r of
     "+" -> counter $ n + 1
     "-" -> counter $ n - 1

counterWidget n=
   p << show n
   ++> wlink "+" "+" >>= counterWidget (n + 1)
   <|> wlink "-" "-" >>= counterWidget (n - 1)


modal:: (html -> html) -> FlowM Html IO a -> (a ->FlowM Html IO b) -> FLowM Html IO b

modal holder w action = do

   holder <<< w `waction` action'
   where
   action'= do
      FormElm f mx <-

como se genera una respuesta partiendo de un tipo

instance Generate (Id a) where
    generate= return . id

Como se almacenan las votacones:
 Vector(prioridad,opcion)

 map user (prioridad , option)    delegations
  como se recuentan los votos cada vez que vota alguien?
   cuando vota un usuario
        mirar sus delegantes
        ir a cada uno de sus delegantes
          mirar la prioridad de su voto
            si es mayor, cambiar el voto en el sumario

como se serializan las votaciones
  para serializar, es mejor un vector que un map si votan todos
  si votan unos cuantos, es mejor un map
  o mejor una tabla per group usuario - numero
    y luego un map int vote
  opcion no almacenar y ejecutar la secuencia de eventos de votacion
    guardar cada voto como un evento
    ejecutar los eventos para generar la votacion en memoria, no serializarla
    los eventos se almacenarian en un wf con process

Si uso google docs,por que copiar el texto en el subject
  si puedo poner un link?
  servicio añadido votación.
  add WFlows to google docs.

documento: texto, votation{pregunta, opctiones}

como introducri en el wF un bucle hasta que se cumple una condicion?
  rechazar un documento hasta que se apruebe.
    se puede modificar y votar continuamente
    como determinar que se votó si se modifica continuamente?
      cuando se aprueba se bloquea?
       como se notifica al votante que se ha modificado?
         mensajes? discusión?
    si se usa una unica DBRef, se pierde el historial de modificaciones
     cuando esta siendo sometido a votación, bloquear edicion
     cuando es rechazado,
        desbloquear edicion
        permitir reiniciar wf
           creando un nuevo WFRef
           prosiguiendo el antiguo WF para tener toda la historia del proceso.
              codigo de enganche?
           usar un exec que continue el log
           permisos de ejecutar?
             solo el autor y cuando esta en estado editable.

 como reiniciar el wf?
   ejecutando getConfigureWF otra vez? si
     pero getConfigureWF añade mas votation y coje el ultimo subject

 como se presentan los resultados
   no se borran de las colas
       permanecen en las de cada grupo
       no hay cola de destino
       hay que notificar el estado finalizado en el Subject

como se evaluan los resultados
   por la ultima votacion?


processData transact= do
  let k= key transact
  th <- start k process
  r  <-tellToWF k x
  process val= do
    step $ do
      r <- rec
      val'= r val
      send $ val'
      return r
    process val'


rollback con rebuild-1
freeze:
  se salva el log a historia
  se guarda el resultado final

processData t= do
   let var =
   res <- do
           mr <-  readMVar var
           case mr of
               Just res -> return res
               Nothing  -> rebuild mempty
   log t
   let res'= mappend res t
   ever $ writeMVar var res
   return res'
 where
 log x= exec1nc (key x) (step $ return x)
 rebuild res= do
   is <- isInRecover
   if not is
     then return res
     else do
        t <- step $  undefined
        res' <- res <> t
        rebuild1 res'

--  fforward f = WF $\s@Stat{..} ->(s{versions=[],recover=false},())

no hace falta fforward
 el WF permanece en TCache
  independientemente de si el proceso se ha acabado o no.
  varios procesos pueden escribir en el mismo log si usan en mismo wfname.
  basta rebuild y fforward

to do:
  mensaje fin de sesion
  ajax: popup con link
  no ajax:
     meter addressable

  meter addressable
     se desactiva con POST
     re-activable manualmente
     como saber si es post o get?
     de momento no desactivarlo
     como funciona
      if addressable st
          st{params= params st ++ getParams mfEnv st}

  datos dentro de un proc que lo maneja



  transacciones:
   quiero reservar 1 entrada
     como lo hago transaccional?
      como hago events atomico?
        timestamp todos los req

  por qué no va al link cuando hay timeout y se refresca?
  condicion de bloqueo?

  debug con Supervisor
    trace back

  general state para stateless flows?
   y para datosz


problema de dirigirse a root de un flow
  hasParams controla si hay parametros
  ahora si no hasParams, refresca
      para manejar stateful  - shopCart
      pero entoces root no es addressable
      soluciones
        parametro especial  /verbo?root
            detectarlo
             if  (addresable && rootParam)
                  not (inSync st')
              ||
               && not (onInit st')
               && hasParams (mfSequence st') (mfSeqCache st') ( mfEnv st')
        quitar hasParams
            se iria siempre a root si no hay parametros
                malo para shopCart

      no hacer nada

problema : hacer un buen log cuando hay un error con Supervisor
   que guarde una traza de ejecucion

   (!>) proc text= add tesxt al stack,
    cuando falla

data Action  a= forall msg.Backward msg  | Forward a

data Supreme m a= Sup { runSup :: StateT m Handlers (Action  a)}

class Monad m => Supreme m where
    fail   msg = Sup . return $ Backward msg
    return x = Sup . return $ Forward x
    x >>= f  = Sup $ loop
     where
     loop msg= do
        v <- runSup x                          -- !> "loop"
        case v of

            Forward y  -> do
                 z <- runSup (f y)           -- !> "BACK"
                 case z of
                  Backward msg   -> loop msg            -- !> "GoBack"
                  other -> return other
            Backward msg  -> return  $ Backward msg

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

transactional loopback
  con reserva de datos en una request
    en vez de reservar manualmente, en espera de respuesta
    se puede hacer automaticamente?
    por ejemplo:

      r <- reserve data
      c <- ask confirm r
      case c of
        True ->adquire r
        False ->
          cancel r

    simplificado:

         mr <- ask takeOne data
         case mr of
          Just..
          Nothing..

takeOne widget data= reserve data >>= widget `action` adquireReject `modify` ...

class Reservable a where
  reserve ::  x -> a x -> STM ( Reserve x a)
  adquire ::  Reserve x a -> STM x
  cancel  ::  Reserve x a -> STM ()

test x= do
 r <- reserve x
 cancel r
 == x

reserve may be a weaker (or more general) locking .
locking may be a particular case when the locked resource is the whole structure.
hay algo parecido equivalente en STM?
  Es mas optimista acceder al objeto despues de elegido si no existe, reintentar si posible.



ajax:
 otra opction para que un widget interactue con el servidor con codigo dentro de ese widget:%
   onClick doserver "verb?parm=ajaxstring

   el widget lee

 invoque action in the server:
 onAction=doServer act

 doServer str =
   xmlhttp.open("GET",str,true);
   xmlhttp.send();

xmlhttp.onreadystatechange=function()
  {
  if (xmlhttp.readyState==4 && xmlhttp.status==200)
    {
    eval(xmlhttp.responseText);
    }
  }

 ajaxHead=



 invocaciones con parametro ajax:

<html>
<head>
<script type="text/javascript">
function loadXMLObj()
{
var xmlhttp;
if (window.XMLHttpRequest)
  {// code for IE7+, Firefox, Chrome, Opera, Safari
  xmlhttp=new XMLHttpRequest();
  }
else
  {// code for IE6, IE5
  xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
  }
return xmlhttp
}

xmlhttp= loadXMLObj()

function loadXMLDoc(){
xmlhttp.open("GET","ajax_info.txt",true);
xmlhttp.send();

xmlhttp.onreadystatechange=function()
  {
  if (xmlhttp.readyState==4 && xmlhttp.status==200)
    {
    document.getElementById("myDiv").innerHTML=xmlhttp.responseText;
    }
  }

}
</script>
</head>
<body>

<div id="myDiv"><h2>Let AJAX change this text</h2></div>
<button type="button" onclick="loadXMLDoc()">Change Content</button>
</body>
</html>


(<+>),wintersperse,(|*>),(|+|), (**>),(<**),wconcat,(<|>),(<*),(<$>),(<*>),
-- * formatting combinators
(<<<),(<++),(++>),(<!)

(.<+>.),(.|*>.),(.|+|.), (.**>.),(.<**.),(.<|>.), .<<.
-- * formatting combinators
(.<<<.),(.<++.),(.++>.),(.<!.)

bhtml bbody

como hacer que cada elemento tenga su propio numero una vez creado.
 la segunda  vez que pase el proceso.
 porque algunos widgets monadicos puede que no recorran todo el espacio de nombres
   el el state identificar nombres de widgets
    w1 <*> w2=> (n1,n2)
    y el codigo lo utiliza
    prevSeq sería una array de esas estructuras
 prohibir monadas en widgets
   como seria login?

que pasa con los form cacheados?
   si pincho back, la secuencia en el servidor se tiene que resetear al valor previo
   si no, nunca encajarán
   cuando se mete un widget cacheado, no hay que recordar la secuencia actual, sino la del widget
   pero una pagina puede contener widgets cacheados o no
        por lo que los numeros de secuencia no valen para hacer matching
        porque la secuencialidad en la página se pierde
        habria que guardar numero secuencia de cada widget de la página

   opción: un numero de secuencia de página en cookie
      con secuencia de numeros de campos.



añadir gestion usuarios storyreader
relato tendria un grafico


FlowM view IO b

atomic  :: (Serialize a,
      Typeable view,
      FormInput view,
      Monoid view,
      MonadIO m,
      Typeable a) =>
      FlowM view m a
      -> FlowM view  m a
atomic f= atomically $ do
   s <- get
   BackT $ do
    (r,s') <-  lift . unsafeIOToSTM $ runStateT (runBackT f) s
    -- when recovery of a workflow, the MFlow state is not considered
    return r

meter transacciones en bloques atomicos


unsafeMFLowToSTM :: MFlowState view -> FlowM view IO a -> IO a
unsafeMFlowToSTM s f=
     (r,s') <-  lift . unsafeIOToSTM $ runStateT (runBackT f) s

atomic :: FlowM view
atomic f= do
  s <- get
  (r,s') <- atomically $ runStateT (runBackT f) s
  put s'
  return r

como notificar a workflow que ha sido salvado el estado?

modo sincrono siempre -> facil

evitarlo

crear una copia y salvarla
problema general
 transactional stream
   se lee progresivamente
   se escribe progresivamente

 puede haber solapamiento entre lo que hay en disco y memoria

 onsave (limpia buffer)
 writeDBRef tiene que leerlo enterio

 guardar por un lado header
 y por otro versions

data Buffer a= Buffer [a] Int (DBRef(Buffer a))

writeResource (Buffer a)=

manejo de versiones en Serializable


instance Serialize a => Serialize (Version a) where
  showp (Version x)= showp x
  readp = choice[ readp >>= return . Version
                , readp >>= return . Version . conv]

instance Serializable a => Serializable (Version a) where
  serialize (Version x)= serialize x
  deserialize str=
      Version $ deserialize  str
      `catch` $ \_ -> Version . conv $ deserialize str


class Serialize a => Versioned a where
  data Prev a
  conv :: Prev a -> a


como liberar memoria del log de estado de workflow

leer del fichero: formato (cabecera, sequencer)

sequencer= offset

read sequencer= fseek file


ghc -prof -auto-all -rtsopts demos\StoryReader2.hs

Demos\StoryReader +RTS -p

abre mal los ficheros con acentos. corregido en ghc 7.4


chekear que presentq bien los formularios en validación
pagina web de administracion sync flush debug user errores etc
app para compilar y ejecutar repositorios GIT
   git pull
   cabal install

flag writedebuginfo recoverInDebugMode, unificar step y stepDebug

link powered by haskell



factorizar ask


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


