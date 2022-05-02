import Text.Show.Functions

type Ataque = Heroe -> Heroe

data Heroe = UnHeroe {
  nombre::String,
  nivel::Int,
  vida::Int,
  ataque::Ataque
} deriving Show

sacarVida nivel heroe = heroe{vida = vida heroe - nivel}
cortarVida nivel heroe = heroe{vida = div (vida heroe) nivel}


heroeDeEjemplo = UnHeroe{nombre="Juan Carlos",nivel=10,vida=12,ataque=sacarVida 8}
otroHeroeDeEjemplo = UnHeroe{nombre="Asdf",nivel=5,vida=9, ataque=cortarVida 6}
elDeMasVida = UnHeroe{nombre="Asdf",nivel=1,vida=900, ataque=cortarVida 2}

atacar heroe otroHeroe = (ataque heroe) otroHeroe

mejorHeroe criterio heroe1 heroe2 
  | criterio heroe1 > criterio heroe2 = heroe1
  | otherwise = heroe2

mejorHeroeDeLista criterio [heroe] = heroe
mejorHeroeDeLista criterio (heroe1:heroe2:xs) = mejorHeroeDeLista criterio ((mejorHeroe criterio heroe1 heroe2):xs)

atacarMultiple [] heroeAtacado = heroeAtacado

-- Tarea: hacer usando algún fold!
atacarMultiple (heroeAtacante:heroes) heroeAtacado = atacarMultiple heroes (atacar heroeAtacante heroeAtacado)

atacarAMuchos atacante heroes = map (atacar atacante) heroes

vidaMayorQue numero heroe = vida heroe > numero
