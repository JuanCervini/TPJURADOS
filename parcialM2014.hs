
data Jugador = CJugador {
    nombreJugador :: String,
    edad :: Int,
    promedio :: Float,
    habilidad :: Int,
    cansancio :: Float
} deriving(Show, Eq)

data Equipo = Equipo{
    nombreEquipo :: String,
    grupo :: Char,
    jugadores :: [Jugador]
} deriving(Show, Eq)

martin, juan, maxi :: Jugador
martin = CJugador "Martin" 26 0.0 50 35.0
juan = CJugador "Juancho" 30 0.2 50 40.0
maxi = CJugador "Maxi Lopez" 27 0.4 68 30.0

jonathan, lean, brian :: Jugador
jonathan = CJugador "Chueco" 20 1.5 80 99.0
lean = CJugador "Hacha" 23 0.01 50 35.0
brian = CJugador "Panadero" 21 5 80 15.0

garcia, messi, aguero :: Jugador
garcia = CJugador "Sargento" 30 1 80 13.0
messi = CJugador "Pulga" 26 10 99 43.0
aguero = CJugador "Aguero" 24 5 90 5.0

equipo1, losDeSiempre, restoDelMundo :: (String, Char, [Jugador])
equipo1 = ("Lo Que Vale Es El Intento", 'F', [martin, juan, maxi])
losDeSiempre = ( "Los De Siempre", 'F', [jonathan, lean, brian])
restoDelMundo = ("Resto del Mundo", 'A', [garcia, messi, aguero])

figuraDelEquipo :: Equipo -> [Jugador]
figuraDelEquipo equipo = filter esFigura (jugadores equipo)

esFigura :: Jugador -> Bool
esFigura jugador = habilidad jugador > 75 && promedio jugador > 0

jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

esFarandulero :: Jugador -> Bool
esFarandulero jugador = nombreJugador jugador `elem` jugadoresFaranduleros

tieneFarandulero :: Equipo -> Bool
tieneFarandulero equipo = any esFarandulero (jugadores equipo)

esJoven jugador = edad jugador < 27

nombresFiguritasDificiles equipo jugadores = map nombreJugador $ filter esFiguritaDificil (jugadores equipo)

esFiguritaDificil :: Jugador -> Bool
esFiguritaDificil jugador = esFigura jugador && esJoven jugador && not (esFarandulero jugador)



jugarPartido equipo jugador
                                | esFiguritaDificil jugador                 = jugador {cansancio = 50}
                                | esJoven jugador                           = jugador {cansancio = cansancio jugador * 0.1}
                                | not (esJoven jugador) && esFigura jugador = jugador {cansancio = cansancio jugador + 20}
                                | otherwise                                 = jugador {cansancio = 2 * cansancio jugador}


