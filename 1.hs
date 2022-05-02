data Heroe = UnHeroe {
  nombre::String,
  nivel::Int,
  vida::Int
} deriving Show

heroeDeEjemplo = UnHeroe{nombre="Juan Carlos",nivel=10,vida=12}
otroHeroeDeEjemplo = UnHeroe{nombre="Asdf",nivel=5,vida=9}

atacar heroe otroHeroe = otroHeroe{vida = vida otroHeroe - 1}

