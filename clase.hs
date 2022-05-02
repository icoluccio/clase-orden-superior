import Text.Show.Functions
type Vida = Int
type Nivel = Int
type Ataque = (Heroe -> Heroe)

data Heroe = UnHeroe {
  nombre::String,
  nivel::Nivel,
  vida::Vida,
  ataque::Ataque
} deriving (Show)

atacar :: Heroe -> Heroe -> Heroe
atacar heroe otroHeroe = (ataque heroe) otroHeroe
-- Que es lo mismo que
atacar' heroe = ataque heroe

sacarVida intensidad otroHeroe = otroHeroe{vida = vida otroHeroe - intensidad}
cortarVida intensidad otroHeroe = otroHeroe{vida = div (vida otroHeroe) (intensidad + 1)}
bajarNivel intensidad otroHeroe = otroHeroe{nivel = (nivel otroHeroe) - intensidad}

puntaje :: Heroe -> Int
puntaje heroe = vida heroe + nivel heroe + 10
puntajeMultiplicado multiplicador heroe = multiplicador * puntaje heroe

heroeDeEjemplo = UnHeroe{nombre="Juan Carlos",nivel=10,vida=12,ataque=sacarVida 2}
heroeDeEjemplo2 = UnHeroe{nombre="Juan Carlos",nivel=10,vida=12,ataque=sacarVida 5}
otroHeroeDeEjemplo = UnHeroe{nombre="Otro héroe",nivel=5,vida=90, ataque=cortarVida 2}
otroHeroeDeEjemplo2 = UnHeroe{nombre="Otro héroe",nivel=5,vida=9,ataque=bajarNivel 3}

cambiarHeroe :: Heroe -> Ataque -> Heroe
cambiarHeroe heroe funcion = heroe{ataque=funcion}


heroeConMasVida = mejorHeroe vida

mejorHeroe :: Ord b => (a -> b) -> a -> a -> a
mejorHeroe criterio heroe1 heroe2 
  | criterio heroe1 > criterio heroe2 = heroe1
  | otherwise = heroe2

mejorHeroeDeLista :: Ord b => (a -> b) -> [a] -> a
mejorHeroeDeLista criterio [heroe] = heroe
mejorHeroeDeLista criterio (heroe1:heroe2:heroesRestantes) = 
   mejorHeroeDeLista criterio ((mejorHeroe criterio heroe1 heroe2):heroesRestantes)

map' _ [] = []
map' funcion (cabeza:cola) = funcion cabeza:(map' funcion cola)