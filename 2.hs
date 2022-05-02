import Text.Show.Functions
type Ataque = (Heroe -> Heroe)
data Heroe = UnHeroe {
  nombre::String,
  nivel::Int,
  vida::Int,
  ataque::Ataque
} deriving Show

sacarVida heroe = heroe{vida = (vida heroe) - 10}
cortarVida heroe = heroe{vida = div (vida heroe) 2}

heroeDeEjemplo = UnHeroe{nombre="Juan Carlos",nivel=10,vida=12,ataque=sacarVida}
otroHeroeDeEjemplo = UnHeroe{nombre="Asdf",nivel=5,vida=9, ataque=cortarVida}
elDeMasVida = UnHeroe{nombre="Asdf",nivel=1,vida=900, ataque=cortarVida}

atacar heroe otroHeroe = (ataque heroe) otroHeroe

mejorHeroe criterio heroe1 heroe2 
  | criterio heroe1 > criterio heroe2 = heroe1
  | otherwise = heroe2

mejorHeroeDeLista criterio [heroe] = heroe
mejorHeroeDeLista criterio (heroe1:heroe2:xs) = mejorHeroeDeLista criterio ((mejorHeroe criterio heroe1 heroe2):xs)

-- Otro bueno para fold
atacarMultiple [] heroeAtacado = heroeAtacado
atacarMultiple (heroeAtacante:heroes) heroeAtacado = atacarMultiple heroes (atacar heroeAtacante heroeAtacado)

atacarAMuchos atacante heroes = map (ataque atacante) heroes


map' _ [] = []
map' funcion (x:xs) = funcion x: map' funcion xs

-- Posibles soluciones para lo que contaba, fíjense que son muy similares
all' _ [] = True
all' funcion (x:xs) = funcion x && all' funcion xs

any' _ [] = False
any' funcion (x:xs) = funcion x || any' funcion xs

filter' _ [] = []
filter' funcion (x:xs) | funcion x = x : filter' funcion xs
                       | otherwise = filter' funcion xs
