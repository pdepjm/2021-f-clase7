module Library where
import PdePreludat


{- 
Pdepreludat:
Number

Tipos en Haskell puro:
Int
Integer
Float
Double
Complex

Son un problema para enseñar, para aprender desde el principio.
Lo que importa es orden superior, aplicación parcial, modelar con funciones,
entender de tipos.
-}
factorial :: Number -> Number
-- factorial numero | numero < 0 = error "No existe factorial de negativos"-- BOOM!
factorial 0 = 1
factorial numero = numero * factorial (numero - 1)
-- 3! = 3 * 2! 
-- 2! = 2 * 1!
-- 255! = 254! * 255

fibonacci :: Number -> Number
fibonacci 0 = 1
fibonacci 1 = 1
fibonacci enesimo = fibonacci (enesimo - 1) + fibonacci (enesimo - 2)

-- data Lista a = ListaVacia | UnaLista a Lista

-- Array NO, lastiman a Haskell
-- edad (UnaPersona _ e) = e

miHead :: [a] -> a
miHead (cabeza : _) = cabeza

miTail :: [a] -> [a]
miTail (_ : cola) = cola

-- (No son recursivas)


miLast :: [a] -> a
miLast (cabeza : []) = cabeza
miLast (cabeza : cola) = miLast cola 

-- Esto:
-- miLength [x] = 1
-- Es lo mismo que:
-- miLength (x : []) = 1


-- TIPO
miLength :: [a] -> Number
-- CASO BASE
miLength [] = 0
-- CASO RECURSIVO
miLength (cabeza : cola) = 1 + miLength cola

-- TIPO
miTake :: Number -> [a] -> [a]
-- CASO BASE
miTake _ [] = []
-- CASO BASE
miTake 0 _ = []
-- CASO RECURSIVO
miTake cantidad (x : xs) = x : miTake (cantidad - 1) xs

-- Relacion [1,2,3,4] con [2,3,4,5,6,7,8]
-- take 3 [2,3,4,5,6,7,8] => Cola de [1,2,3,4]

-- take 4 [1,2] 
-- => 1 : take 3 [2]
-- => 1 : 2 : take 2 []
-- => 1 : 2 : [] 


miAny :: (a->Bool) -> [a] -> Bool 
miAny _ [] = False
miAny cumple (x : xs) = cumple x || miAny cumple xs

-- Any se cumple <=> cumple con la cabeza || cumple any en la cola

miAll :: (a->Bool) -> [a] -> Bool 
miAll _ [] = True
miAll cumple (x : xs) = cumple x && miAll cumple xs

miMap :: (a -> b) -> [a] -> [b]
miMap _ [] = []
miMap transformar (x:xs) = transformar x : miMap transformar xs









-- miMap :: ??? -> ???
-- miMap ??? = ???
-- miMap ??? = ???

-- miFilter :: ??? -> ???
-- miFilter ??? = ???
-- miFilter ??? = ???


















data Mascota = UnaMascota {
  nombre :: String,
  especie :: Especie,
  edad :: Number   -- edad en días perrunos
} deriving (Show)

data Especie = Gato | Perro | Tortuga | Araña | Plantita deriving(Show)

crecer :: Mascota -> Number -> Mascota
crecer mascota dias = mascota { edad = edad mascota + dias * 2 - 1 }

cantarHakunaMatata :: Mascota -> [Number] -> Mascota
cantarHakunaMatata mascota [] = mascota
cantarHakunaMatata mascota (dia : dias) = cantarHakunaMatata (crecer mascota dia) dias 

{-
bebeNala

crecer

bebeNala `crecer` 30 `crecer` 25 `crecer` 55 

(esto es otra forma de ver lo que hace Fold)
-}
cantarHakunaMatata2 :: Mascota -> [Number] -> Mascota
cantarHakunaMatata2 mascota dias = foldl crecer mascota dias

--foldl :: (a -> b -> a) -> a -> [b] -> a


miMaximum :: Ord a => [a] -> a
miMaximum cosas = foldl1 max cosas
--miMaximum' cosas = foldl max (head cosas) cosas
---- imaginar entre todos los elementos de la lista

-- Con miMaximum lo que pasa es esto:
-- 23 `max` 784 `max` 7

-- Con miMaximum' lo que pasa es esto:
-- 23 `max` 23 `max` 784 `max` 7

miSum :: [Number] -> Number
miSum numeros = foldl (+) 0 numeros
--miSum'' numeros = foldl (+) (head numeros) (tail numeros)


productoria :: [Number] -> Number
productoria numeros = foldl (*) 1 numeros

infinitaDesde :: Number -> [Number]
infinitaDesde nro = nro : infinitaDesde (nro+1)

subirSueldo _ = 1 --imaginen que esto tiene efecto

-- Haskell Lazy , perezoso
-- muchos otros lenguajes: eager evaluation (ansiosa)

-- Si yo tuviera EFECTO, no podría tener lazy evaluation.
-- C decidió tener efecto, por eso es importante evaluar todo (evaluar ansiosamente).



























-- miMaximum:: ??? -> ???
-- miMaximum ??? = ???












