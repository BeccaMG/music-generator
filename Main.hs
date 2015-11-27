{- 
* @file
* @author  Machado Rebeca 10-10
*         Jimenez Oswaldo 10-10368
*
* Implementacion del Programa Main, siendo sus funciones principales
* Componer' y 'buscar <Int>'
* -}

module Main where

import Prelude hiding (init)
import Input
import Euterpea hiding (Event)
import Data.List
import Data.Function
import System.Random
import qualified Data.Map as MyDataMap
import Data.Time


-- Directorio predeterminado
directorio :: String
directorio = "./xml/"

-- Longitud de las secuencias musicales generadas
longitud :: Int
longitud = 50   

getTimeSeed = getCurrentTime >>= return . truncate . utctDayTime

randomLista :: Int -> [Float]
randomLista seed = map (/1000) $ take longitud $ randomRs (0, 999) (mkStdGen seed)
   
   
-- ************************************************************************** --
-- ******************** CONSTRUCCION MODELO CONTEXTO ************************ --    
-- ************************************************************************** --

type Probabilidad = Float
type Contexto = MiEvento
type Segmento = (Float,Float)

data MiEvento e = Evento e | ParEventos (e,e) | EventoVacio ()
    deriving (Show,Eq,Ord)
    
-- El modelo es un mapa asociativo entre eventos y naturales (frecuencia del evento)
data Modelo a = Modelo (MyDataMap.Map (MiEvento a) Int)
    deriving (Show,Eq,Ord)
    
type TablaProbabilidad      a = MyDataMap.Map (MiEvento a) Probabilidad

type ProbabilidadContextual a = MyDataMap.Map (Contexto a) (RectaProbabilidad a)

type RectaProbabilidad      a = MyDataMap.Map Segmento (MiEvento a)

    
toList:: Modelo k -> [(MiEvento k, Int)]
toList (Modelo m) = MyDataMap.toList m    

-- ************************************************************************** --
-- *************************** MAIN PRINCIPAL ******************************* --    
-- ************************************************************************** --  
  
{- El archivo ejecutable del programa,evalua la función componer. 
   Para evaluar la función buscar, se debe correr el programa con 
   el interpretador (ghci).
   -}
main :: IO ()
main = componer

-- ************************************************************************** --
-- *************************** FUNCIÓN COMPONER ***************************** --    
-- ************************************************************************** -- 
   
    
{- Induce un modelo de contexto a partir de la colección musical 
   en el directorio por defecto, genera una secuencia musical 
   nueva a partir de este modelo, la imprime por pantalla y la 
   reproduce.
   -}
componer :: IO ()
componer = componer' directorio

componer' :: String -> IO ()
componer' dir = do
  (seqs, filenames) <- loadMusicXmls dir
    
  {- Se verifica que el directorio no este vacio -}
  if (null seqs) && (null filenames) then
     putStrLn $ "El directorio esta vacio"
  else do
      seed <- getTimeSeed
      let modelo = construirModelo (concat seqs)
      let composicion = obtenerComposicion modelo seed
      putStrLn $ show composicion
      play $ sequenceToMusic composicion

  
  
{---------------------------------------------- Funciones auxiliares/componer -}

{-  isEvento

    @param (MiEvento k, Int): Un evento asociado con su frecuencia.
    ======================================================================
    
    Dado un evento y su frecuencia, determina si este evento es un evento simple
    (de la forma 'A') y no un evento compuesto (de la forma "AB")
-}
isEvento:: (MiEvento k, Int) -> Bool
isEvento (Evento _, _) = True
isEvento x = False


{-  listaEventos

    @param Modelo a: Es el modelo con todas las frecuencias.
    ======================================================================
    
    Dado un modelo de contexto, obtiene todos los eventos distintos de la 
    secuencia original.
-}
listaEventos :: (Modelo a) -> [(MiEvento a)]
listaEventos m = map fst $ takeWhile isEvento $ toList m


{-  probabilidadCondicional

    @param Modelo a: Es el modelo con todas las frecuencias.
    @param MiEvento a: Es el evento al cual se le calcula la probabilidad 
                       condicional.
    @param Contexto a: Es el contexto dado.
    ======================================================================
    
    Calcula la probabilidad condicional de un evento dado un contexto de la 
    forma:
                        probabilidad = 0.3pe + 0.7pc
                        
    Donde 
    * "pe" es la frecuencia del evento entre la frecuencia del evento vacío 
    (longitud de la secuencia) y 
    * "pc" es la frecuencia del par (contexto,evento) entre la frecuencia del 
    contexto.
    
    Es decir, en una secuencia "ABC" la probabilidad de B dado A sería:
                        probabilidad = 0.3pe + 0.7pc = 0.8
                        
    Donde
    pe = 1/3 (pues B tiene frecuencia 1 y la longitud es 3)
    pc = 1/1 (pues AB tiene frecuencia 1 y B también)
-}
probabilidadCondicional :: (Ord a) => (Modelo a) -> (MiEvento a) -> (Contexto a) -> Probabilidad
probabilidadCondicional (Modelo m) (Evento e) (EventoVacio ()) = p
    where 
        p = (fromIntegral (valor m (Evento e))) / (fromIntegral (valor m (EventoVacio ())))
probabilidadCondicional (Modelo m) (Evento e) (Evento c) = 0.3*pe + 0.7*pc
    where
        pe = (fromIntegral (valor m (Evento e))) / (fromIntegral (valor m (EventoVacio ())))
        pc = (fromIntegral (valor m (ParEventos (c,e)))) / (fromIntegral (valor m (Evento c)))


{-  normalizarProbs

    @param TablaProbabilidad a: Una tabla de probabilidad condicional
    ======================================================================
    
    Dada una tabla de probabilidades (una tabla con todos los eventos y sus 
    probabilidades condicionales según un contexto), normaliza todas las
    probabilidades de la tabla; es decir, las divide entre su norma.
-}
normalizarProbs :: (TablaProbabilidad a) -> (TablaProbabilidad a)
normalizarProbs t = MyDataMap.map (/suma) t
    where
        suma = MyDataMap.fold (+) 0 t
        
        
{-  distribucionProbabilidad

    @param Modelo a: Es el modelo con todas las frecuencias.
    @param Contexto a: Es el contexto dado.
    ======================================================================
    
    Calcula la probabilidad de todos los eventos del modelo en base a un 
    contexto dado con la función "probabilidadCondicional" y las asocia en un 
    mapa. Estas probabilidades están normalizadas para que la distribución (la 
    suma de todas las probabilidades) sea siempre igual a 1.
    
    Es decir, en una secuencia "ABC" la distribución de probabilidad dado A 
    sería:
                        [('A',0.1),('B',0.8),('C',0.1)]
-}              
distribucionProbabilidad :: (Ord a) => (Modelo a) -> (Contexto a) -> (TablaProbabilidad a)
distribucionProbabilidad modelo contexto = normalizarProbs $ foldl f (MyDataMap.empty) (listaEventos modelo)
    where
        f tabla evento = MyDataMap.insert evento (probabilidadCondicional modelo evento contexto) $ tabla

        
{-  calcularRecta

    @param TablaProbabilidad a: Una tabla de probabilidad condicional
    ======================================================================
    
    Dada una tabla de probabilidades (una tabla con todos los eventos y sus 
    probabilidades condicionales según un contexto), construye una recta con 
    estas probabilidades distribuidas y el evento asociado a cada segmento de 
    recta.
    
    Es decir, en una secuencia "ABC" la distribución de probabilidad dado A 
    sería:
                         [('A',0.1),('B',0.8),('C',0.1)]
                                
    La recta obtenida en base a esta distribución sería:
             [ ( (0.0,0.1), 'A'), ( (0.1,0.9), 'B'), ( (0.9,1.0), 'C')]
                                
    O lo que es lo mismo:
    
                0.0 _______0.1______________________0.9________1.0
                        A                B                 C
-}
calcularRecta :: (Ord a) => (TablaProbabilidad a) -> (RectaProbabilidad a)
calcularRecta tablaContexto = MyDataMap.fromList $ zip probs (map fst (MyDataMap.toList tablaContexto))
    where 
        probs = zip suma (tail suma)
        suma = scanl (+) 0 (map snd (MyDataMap.toList tablaContexto))

        
{-  calcularProbabilidades

    @param Modelo a: Modelo con todas las frecuencias de eventos.
    ======================================================================
    
    Dado un modelo de contexto, calcula la distribución de probabilidades de
    todos los eventos de ese modelo y las guarda en un mapa asociativo donde
    cada evento tiene su tabla de distribución convertida en recta.
    
    Es decir, en una secuencia "ABC" la función retornaría:
    
 [ ('A', ((0.0,0.1), 'A'), ((0.1,0.9), 'B'), ((0.9,1.0), 'C')), ('B', ...) ... ]
                                
    O lo que es lo mismo:
    
                0.0 _______0.1______________________0.9________1.0
                        A                B                 C          ...
                __________________________________________________
                                        A
-}        
calcularProbabilidades :: (Ord a) => (Modelo a) -> (ProbabilidadContextual a)
calcularProbabilidades modelo = foldl f (MyDataMap.empty) ((EventoVacio()):listaEventos modelo)
    where
        f rprob evento = MyDataMap.insert evento (calcularRecta $ distribucionProbabilidad modelo evento) $ rprob
        
        
{-  obtenerEventoDadoContexto

    @param ProbabilidadContextual a: Todas las probabilidades condicionales
    asociadas a todos los eventos de una secuencia.
    @param Contexto a: Contexto dado.
    ======================================================================
    
    Esta función toma un mapa asociativo contexto - recta de probabilidad y
    obtiene la recta asociada al contexto dado como parámetro. Con esa recta
    obtiene un evento cualquiera por medio de un número aleatorio perteneciente 
    al intervalo [0,1). Este número caerá en un segmento de recta cualquiera y
    se obtiene el evento asociado a ese segmento de recta, que, a su vez,
    representa una distribución de probabilidad. Esto asegura que cada evento
    es escogido con probabilidad P.
    
    Es decir, en una secuencia "ABC", dado el contexto 'A' la recta es:
    
                0.0 _______0.1______________________0.9________1.0
                        A                B                 C        
                        
    Suponiendo que el número aleatorio es 0.78, el evento escogido es B.
-}
obtenerEventoDadoContexto :: (Ord a) => (ProbabilidadContextual a) -> (Contexto a) -> Float -> (MiEvento a)
obtenerEventoDadoContexto probs contexto random = head $ MyDataMap.elems $ MyDataMap.filterWithKey (\(x,y) _ -> x<=random && random<y) recta
    where
        recta = (MyDataMap.findWithDefault (MyDataMap.empty) contexto probs)
        

{-  obtenerComposicion

    @param Modelo Evento: Modelo de contexto con todas las frecuencias de los
    eventos de una secuencia musical.
    ======================================================================
    
    Dado un modelo de contexto que asocia eventos con su frecuencia en una 
    secuencia, se calcula progresivamente un evento dado como contexto el evento 
    anterior (comenzando por el vacío) con la función 
    "obtenerEventoDadoContexto". Esto devuelve una lista de eventos.
-}
obtenerComposicion :: (Ord a) => (Modelo a) -> Int -> [a]
obtenerComposicion modelo seed = map (\((Evento a), _) -> a) lista
    where 
        lista = take longitud $ tail $ iterate f ((EventoVacio ()), 0)
        f (evento,n) = ((obtenerEventoDadoContexto (calcularProbabilidades modelo) evento ((randomLista seed) !! n)), n+1)
        

    
--Combinacion de ambas producciones
--Genera el modelo completo en una sola corrida a la lista de tuplas
-- Modo de uso: construirModelo listaTuplas 
-- Result: modelo orden 0, modelo orden 1
    
fst' :: (MyDataMap.Map (MiEvento e) Int, MyDataMap.Map (MiEvento e) Int, Int) -> MyDataMap.Map (MiEvento e) Int
fst' (mapaOrden2,_,_) = mapaOrden2

snd' :: (MyDataMap.Map (MiEvento e) Int, MyDataMap.Map (MiEvento e) Int, Int) -> MyDataMap.Map (MiEvento e) Int
snd' (_,mapaOrden1,_) = mapaOrden1

thrd' :: (MyDataMap.Map (MiEvento e) Int, MyDataMap.Map (MiEvento e) Int, Int) -> Int
thrd' (_,_,cantidadBlancos) = cantidadBlancos


{-  construirModelo

    @param [k]: Lista de tuplas Evento a partir del cual se construira
    el modelo de contexto de Eventos (para k Evento)
    ======================================================================
    
    Dado una lista de tuplas de tipo Evento se construye el modelo de contexto
    asociado, en base a la frecuencia de aparicion de cada uno de estos k-Eventos.
    Emplea a la funcion producirModelo en caso de que la lista de entrada
    posea dos o mas elementos
-}
construirModelo:: (Ord k) => [k] -> (Modelo k)
construirModelo  []  = Modelo MyDataMap.empty
construirModelo  [x] = Modelo $ MyDataMap.insert (Evento x) 1 $ MyDataMap.singleton (EventoVacio ()) 1
construirModelo listaTuplas = Modelo $ MyDataMap.union segundoOrden 
                             $ MyDataMap.union primerOrden 
                             $ MyDataMap.singleton (EventoVacio ()) cantidadVacios              
          where segundoOrden   = fst'  misModelos
                primerOrden    = snd'  misModelos 
                cantidadVacios = thrd' misModelos
                misModelos     = producirModelo listaTuplas

{-  producirModelo

    @param [k]: Lista de tuplas Evento a partir del cual se construira
    el modelo de contexto de Eventos (para k Evento)
    @param (MyDataMap.Map (MiEvento k) Int, MyDataMap.Map (MiEvento k) Int,Int)
    tripleta que corresponde al mapa asociativo Evento-Frecuencia de segundoOrden,
    al mapa asociativo Evento-Frecuencia de primerOrden, y la longitud de la lista
    de eventos; de forma respectiva. 
    ======================================================================
    
    Dado una lista de tuplas de tipo Evento se construye el modelo de contexto
    asociado, en base a la frecuencia de aparicion de cada uno de estos k-Eventos.
    Se construye de forma recursiva (empleando recursion de cola a traves de
    la funcion calcularFrecuencia), permitiendo ensamblar simultaneamente
    ambos modelos en una sola corrida.
    
-}
producirModelo:: (Ord k) => [k] -> (MyDataMap.Map (MiEvento k) Int, MyDataMap.Map (MiEvento k) Int,Int)
producirModelo  (t1:t2:tuplasRestantes) 
    =  calcularFrecuencia (mapaVacio,primerEvento,tamListaEventos) t1 t2 tuplasRestantes
    where mapaVacio    = MyDataMap.empty
          primerEvento = MyDataMap.singleton (Evento t1) 1
          tamListaEventos = 1 
          
calcularFrecuencia:: (Ord k) => (MyDataMap.Map (MiEvento k) Int, MyDataMap.Map (MiEvento k) Int, Int)-> 
                     k ->k ->[k] -> (MyDataMap.Map (MiEvento k) Int,MyDataMap.Map (MiEvento k) Int,Int)
calcularFrecuencia (mapaOrden2,mapaOrden1,contador) tuplaAnterior tuplaActual []
             = (MyDataMap.insertWith (+) 
                    (ParEventos (tuplaAnterior,tuplaActual)) 1 mapaOrden2,
                     MyDataMap.insertWith (+) (Evento tuplaActual) 1 mapaOrden1,
                     contador+1)
                     
calcularFrecuencia (mapaOrden2,mapaOrden1,contador) tuplaAnterior tuplaActual (tSiguiente:restoTuplas)
             = calcularFrecuencia ((MyDataMap.insertWith (+) 
                   (ParEventos(tuplaAnterior,tuplaActual)) 1 mapaOrden2),
                   (MyDataMap.insertWith (+) (Evento tuplaActual) 1 mapaOrden1),
                    contador+1) tuplaActual tSiguiente restoTuplas
                    
                    
-- ************************************************************************** --
-- **************************** FUNCIÓN BUSCAR ****************************** --    
-- ************************************************************************** --    

{- Recupera las diez secuencias más similares a la k-ésima secuencia 
   de la colección musical en el directorio por defecto, donde la 
   colección musical ha sido ordenada en orden alfabético por el 
   nombre de archivo. Imprime una lista ordenada de las diez 
   secuencias más similares. En cada fila de la lista se debe indicar 
   el número de la secuencia (relativo al orden alfabético de la 
   colección), el nombre de archivo y la distancia a la consulta.
   -}
buscar :: Int -> IO ()
buscar kEsimo = buscar' directorio kEsimo
  
buscar' :: String -> Int -> IO ()
buscar' dir kEsimo = do
  seqfns <- loadMusicXmls dir
  
  {- seqfns_ordenados contiene las secuencias y los nombres asociados a estas
     secuencias en forma de tupla ([secuencias],[archivos_asociados])
     Estan ordenados de acuerdo al orden alfabetico de la coleccion -}
  let seqfns_ordenados = unzip $ sortBy (compare `on` snd) $ (uncurry zip) seqfns
  
  {- Se verifica que el directorio no este vacio -}
  if (null $fst seqfns_ordenados) && (null $snd seqfns_ordenados) then
     putStrLn $ "El directorio esta vacio"
  else do
  
      let cantidadSecuencias =length $snd seqfns_ordenados
      
      {- Se verifica que el indice insertado pertenezca al rango valido 
         de busqueda -}
      if (kEsimo > 0) && (kEsimo <= cantidadSecuencias) then do
            
          let modelos = map construirModelo $ fst seqfns_ordenados
          
          let modeloKesimo = modelos!!(kEsimo-1)
          
          {- distancias contiene una lista de tuplas de la forma (posicion,distancia)
             La segunda componente, distancia, corresponde a la distancia entre 
             el modelo y la consulta
             La primera componente, posicion, corresponde a la posicion relativa del 
             archivo en la coleccion. Se genera 1 tupla por cada secuencia, exceptuando
             aquella asociada a la busqueda en cuestion -}
          let distancias = [(i+1,distancia modeloKesimo (modelos!!i)) | 
                            i<-[0..((length modelos)-1)], i/=(kEsimo-1)]
                                
          {- Se ordenan las tuplas (posicion,distancia) de acuerdo a la distancia
             de menor a mayor -}                      
          let ordenados_porDistancia = sortBy (compare `on` snd) distancias                       
          
          {- Impresion de la consulta solicitada por el usuario al invocar la funcion
             buscar. -}
          putStrLn ("Consulta: Localizacion Archivo ["++ (show kEsimo) ++ "]   " 
                               ++ (show $ (!!(kEsimo-1)) $snd seqfns_ordenados) ++ "\n") 
          
          let mensaje = "Desplegando las diez secuencias de la colección con menor distancia a la consulta... \n"                
          putStrLn (mensaje)
          
          let mensaje = "Posicion:     Nombre Archivo:      Distancia a la Consulta: \n"

          {- Impresion las diez secuencias de la colección con menor distancia a 
             la consulta -}
          putStrLn mensaje
          
          {- Para cada tupla (posicion,distancia) se arma el String de representacion
             que corresponde a la salida solicitada. 
             Formato: <Posicion relativa> <Nombre Archivo> <Distancia> -}
          putStr $concat $take 10 [(show $fst z)++"         "++(show $ (!!((fst z)-1)) 
                                   $snd seqfns_ordenados)++"    "++(show $ snd z)++" \n" 
                                   | z<- ordenados_porDistancia]

      else
          putStrLn $ "Indice fuera de rango, inserte un numero entre 1 y "++(show cantidadSecuencias)
      
{- Funcion distancia entre dos modelos. Entran dos mapas asociativos evento frecuencia
   y retorna el flotante asociado a la distancia entre ambos mapas.
   1. Primero se obtiene la union, sin repeticiones, de las claves de ambos mapas
   a traves de la instruccion (ki,v) <- (MyDataMap.toList (MyDataMap.union mapaA mapaB))
   2. Se aplica la formula de obtencion del valor suministrada en el proyecto (ver definicion de la funcion valor)
   3. Se obtiene la suma de las restas al cuadrado de de los valores (aplicando foldr a la lista de restas cuadraticas
   4. Se calcula la raiz cuadrada del resultado obtenido

   0.0 -}
distancia:: (Ord a) => Modelo a -> Modelo a -> Float
distancia (Modelo mapaA) (Modelo mapaB) 
            = sqrt.fromIntegral $ foldr (+) 0 $ 
                  [((valor mapaB ki) - (valor mapaA ki))^2 | 
                      (ki,v) <- (MyDataMap.toList(MyDataMap.union mapaA mapaB))]

valor:: (Ord a) => MyDataMap.Map (MiEvento a) Int -> (MiEvento a) -> Int
valor mapa evento = mapaLookup' mapa evento

{- Aplica la funcion auxiliar mapaLookup''. Devuelve el valor (frecuencia)
 asociado a una clave (Evento) de un mapa asociativo, en caso de que esta exista. 
 En caso contrario, retorna 0 -}
mapaLookup':: (Ord a) => MyDataMap.Map (MiEvento a) Int -> (MiEvento a) -> Int
mapaLookup' mapa (EventoVacio ())    = MyDataMap.findWithDefault 0 (EventoVacio ()) mapa
mapaLookup' mapa (Evento evento)     = MyDataMap.findWithDefault 0 (Evento evento) mapa
mapaLookup' mapa (ParEventos evento) = MyDataMap.findWithDefault 0 (ParEventos evento) mapa


frecuencia :: (Ord a) => Modelo a -> (MiEvento a) -> Int
frecuencia (Modelo mapa) x = valor mapa x

tocar :: Int -> IO ()
tocar n = do
  seqfns <- loadMusicXmls directorio
  let (seqs, filenames) = unzip $ sortBy (compare `on` snd) $ (uncurry zip) seqfns
  if (n > 0) && (n <= length seqs) then
    putStrLn (filenames !! (n-1)) >>
    play (sequenceToMusic (seqs !! (n-1)))
    else
      putStrLn "Indice fuera de rango"
          
eventToNote :: Evento -> Music Note1
eventToNote e = note
  where
  d = (fromIntegral $ snd e) / 16
  p = Euterpea.pitch $ fst e
  note = Prim (Note d (p,[]))
  
sequenceToMusic :: [Evento] -> Music Note1
sequenceToMusic es = line $ map eventToNote es
