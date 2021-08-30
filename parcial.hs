import Text.Show.Functions
import Data.List

sinRepetidos [] = []
sinRepetidos (x:xs) = x : filter (/= x) (sinRepetidos xs)

---------------------- PUNTO 1 ----------------------

data Persona = Persona {
    nombre :: String,
    niveles :: (Suerte, Convencimiento, Fuerza)
} deriving (Show, Eq)

type Suerte = Int

type Convencimiento = Int

type Fuerza = Int

harry :: Persona
harry = Persona "Harry Potter" (11, 5, 4)

ron :: Persona
ron = Persona "Ron Weasly" (6, 4, 6)

hermione :: Persona
hermione = Persona "Hermione Granger" (12, 8, 2)

---------------------- PUNTO 2 ----------------------

data Pocion = Pocion {
    nombrePocion :: String,
    receta :: [Ingrediente]
} deriving (Show)

data Ingrediente = Ingrediente {
    nombreIngrediente :: String,
    gramos :: Int,
    efectos :: [Efecto]
} deriving (Show)

type Efecto = (Suerte, Convencimiento, Fuerza) -> (Suerte, Convencimiento, Fuerza)

felixFelices :: Pocion
felixFelices = Pocion "Felix Felices" [escarabajosMachacados 52, ojoDeTigreSucio 2]

escarabajosMachacados :: Int -> Ingrediente
escarabajosMachacados gramos = Ingrediente "Escarabajos Machacados" gramos [f1, f2]

ojoDeTigreSucio :: Int -> Ingrediente
ojoDeTigreSucio gramos = Ingrediente "Ojo De Tigre Sucio" gramos [f3]

multijugos :: Pocion
multijugos = Pocion "Multijugos" [cuernoBicornioPolvo 10, sanguijuelasHormonales 54]

cuernoBicornioPolvo :: Int -> Ingrediente
cuernoBicornioPolvo gramos = Ingrediente "Cuerno Bicornio En Polvo" gramos [invertir3, f1, f2]

sanguijuelasHormonales :: Int -> Ingrediente
sanguijuelasHormonales gramos = Ingrediente "Sanguijuelas Hormonales" gramos [duplicarNiveles, f3]

f1 :: (Int, Int, Int) -> (Int, Int, Int)
f1 (ns, nc, nf) = (ns + 1, nc + 2, nf + 3)

f2 :: (Int, Int, Int) -> (Int, Int, Int)
f2 (ns, nc, nf) = (ns, nc, nf + 5)

f3 :: (Int, Int, Int) -> (Int, Int, Int)
f3 (ns, nc, nf) = (ns, nc, nf - 3)

duplicarNiveles :: (Int, Int, Int) -> (Int, Int, Int)
duplicarNiveles (ns, nc, nf) = (ns * 2, nc * 2, nf * 2)

invertir3 :: (a, b, c) -> (c, b, a)
invertir3 (a, b, c) = (c, b, a)

misPociones :: [Pocion]
misPociones = [felixFelices, multijugos]

---------------------- PUNTO 3 ----------------------

type Niveles = (Suerte, Convencimiento, Fuerza)

sumaNiveles :: Niveles -> Int
sumaNiveles (suerte, convencimiento, fuerza) = suerte + convencimiento + fuerza

diferenciaNiveles :: Niveles -> Int
diferenciaNiveles (suerte, convencimiento, fuerza) = (max fuerza (max suerte convencimiento)) - (min suerte (min convencimiento fuerza))

---------------------- PUNTO 4 ----------------------

sumaNivelesPersona :: Persona -> Int
sumaNivelesPersona = sumaNiveles . niveles

diferenciaNivelesPersona :: Persona -> Int
diferenciaNivelesPersona = diferenciaNiveles . niveles

---------------------- PUNTO 5 ----------------------

efectosDePocion :: Pocion -> [Efecto]
efectosDePocion = concatMap efectos . receta

---------------------- PUNTO 6 ----------------------

pocionesHeavies :: [Pocion] -> [String]
pocionesHeavies = map nombrePocion . filter (losEfectosSonAlMenos 4)

losEfectosSonAlMenos :: Int -> Pocion -> Bool
losEfectosSonAlMenos unaCantidad = (>= unaCantidad) . length . efectosDePocion

---------------------- PUNTO 7 ----------------------

incluyeA :: Eq a => [a] -> [a] -> Bool
incluyeA lista1 lista2 = intersect lista1 lista2 == lista1

---------------------- PUNTO 8 ----------------------

esPocionMagica :: Pocion -> Bool
esPocionMagica pocion = (tienenTodasLasVocales . nombresIngredientes $ pocion) && losGramosSonPares pocion

nombresIngredientes :: Pocion -> [String]
nombresIngredientes = map nombreIngrediente . receta

tienenTodasLasVocales :: [String] -> Bool
tienenTodasLasVocales nombres = any (incluyeA "aeiou") nombres

losGramosSonPares :: Pocion -> Bool
losGramosSonPares = all even . map gramos . receta

---------------------- PUNTO 9 ----------------------

tomarPocion :: Pocion -> Persona -> Persona
tomarPocion pocion = aplicarEfectosANiveles (efectosDePocion pocion)

aplicarEfectosANiveles :: [Efecto] -> Persona -> Persona
aplicarEfectosANiveles efectos persona = foldl aplicarUnEfecto persona efectos

aplicarUnEfecto :: Persona -> Efecto -> Persona
aplicarUnEfecto persona efecto = persona {niveles = efecto . niveles $ persona}

---------------------- PUNTO 10 ----------------------

esAntidoto :: Persona -> Pocion -> Pocion -> Bool
esAntidoto persona pocion1 pocion2 = persona == (tomarPocion pocion2 . tomarPocion pocion1 $ persona)

---------------------- PUNTO 11 ----------------------

personaMasAfectada :: Pocion -> (Niveles -> Int) -> [Persona] -> Persona
personaMasAfectada pocion ponderacionDeNiveles personas = maximoSegun (ponderacionDeNiveles . niveles . tomarPocion pocion) personas

maximoSegun :: (Ord a) => (b -> a) -> [b] -> b
maximoSegun f xs = foldl1 (maxSegun f) xs 

maxSegun :: Ord a => (b -> a) -> b -> b -> b
maxSegun f x y | f x > f y = x
               | otherwise = y

---------------------- PUNTO 12 ----------------------

personas :: [Persona]
personas = [harry, hermione, ron]

fst3 :: (a, b, c) -> a
fst3 (a,_,_) = a

snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b

trd3 :: (a, b, c) -> c
trd3 (_,_,c) = c

-- personaMasAfectada multijugos sumaNiveles personas
-- > Hermione Granger

-- personaMasAfectada multijugos ((`div` 3) . sumaNiveles) personas
-- > Hermione Granger

-- personaMasAfectada multijugos trd3 personas
-- > Hermione Granger

-- personaMasAfectada multijugos diferenciaNiveles personas
-- > Hermione Granger

-- personaMasAfectada felixFelices sumaNiveles personas
-- > Hermione Granger

-- personaMasAfectada felixFelices ((`div` 3) . sumaNiveles) personas
-- > Hermione Granger

-- personaMasAfectada felixFelices trd3 personas
-- > Ron Weasly

-- personaMasAfectada felixFelices diferenciaNiveles personas
-- > Hermione Granger

---------------------- PUNTO 13 ----------------------
--superPocion :: [Ingrediente] -> Pocion
--superPocion ingredientes = Pocion "Super Pocion" (calcularIngredientes 0 ingredientes)

--calcularIngredientes :: Int -> [Ingrediente] -> [Ingrediente]
--calcularIngredientes n ingredientes =  [(head (cycle ingredientes)) (n + 1)] ++ [calcularIngredientes (n + 1) (tail (cycle ingredientes))]
