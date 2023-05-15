

type Habilidades = String

type Deseo = Chico -> Chico

data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [Habilidades],
    deseos :: [Deseo]
}

instance Show Chico where
  show (Chico n e h d) = "Chico { nombre = " ++ show n ++ ", edad = " ++ show e ++ ", habilidades = " ++ show h ++ ", deseos = <funciones> }"

timmy :: Chico
timmy = Chico "Timmy" 10 ["mirar television", "jugar en la pc"] [serMayor]

aprenderHabilidades :: [Habilidades] -> Deseo
aprenderHabilidades nuevasHabilidades chico = chico { habilidades = habilidades chico ++ nuevasHabilidades}

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed chico = aprenderHabilidades ["jugar need for speed 1", "jugar need for speed 2"] chico

serMayor :: Deseo
serMayor chico = chico { edad = 18}



aplicarFunciones :: Foldable t => b -> t (b -> b) -> b
aplicarFunciones valor funciones = foldr ($) valor funciones

concederDeseoBienNR :: Chico -> Chico
concederDeseoBienNR chico = aplicarFunciones chico (deseos chico)
--
alterarEdad :: Int -> Chico -> Chico
alterarEdad n chico = chico {edad = edad chico + n }

--concederUnicoDeseo :: Chico -> Chico
--concederUnicoDeseo chico = head (mapearFunciones (deseos chico) chico)

wanda deseos chico = (head deseos chico)

cosmo :: Deseo
cosmo chico = chico { edad = edad chico `div` 2}



data Chica = Chica {
    nombrec :: String,
    condicion :: Chico -> Bool
}

listaDePretendientes :: [String]
listaDePretendientes = ["Raul", "Timmy", "Pedrito"]

tieneHabilidad :: Habilidades -> Chico -> Bool
tieneHabilidad habilidad chico = habilidad `elem` habilidades chico

esSuperMaduro :: Chico -> Bool
esSuperMaduro chico = ((>= 18) . edad) chico && tieneHabilidad "manejar" chico

quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA chica lista
                            | any (condicion chica) lista = head (filter (condicion chica) lista)
                            | otherwise = last lista

noEsTimmy :: Chico -> Bool
noEsTimmy chico = ((/="Timmy"). nombre) chico

ricardita :: Chica
ricardita = Chica "Ricardita" (tieneHabilidad "tener abdominales")


habilidadesProhibidas :: [String]
habilidadesProhibidas = ["enamorar","matar","dominar el mundo"]

esInfractor :: Chico -> Bool
esInfractor chico = any (algunaHabProh (take 5 (habilidades chico))) habilidadesProhibidas

algunaHabProh :: (Foldable t, Eq a) => t a -> a -> Bool
algunaHabProh lista a = a `elem` lista

infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules lista = map nombre (filter esInfractor lista)
