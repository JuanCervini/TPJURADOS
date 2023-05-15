{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}


data Persona = Persona {
    habilidades :: [String],
    bondadoso :: Bool
} deriving (Show, Eq)

data PowerR = PowerR{
    color :: String,
    habilidadesPowerR :: [String],
    nivelPelea :: Int
} deriving (Show, Eq)

powerRangerRojo :: PowerR
powerRangerRojo = PowerR "rojo" ["asesinar con cuchillo", "esquivar balas"] 10

ramon :: Persona
ramon = Persona ["hacer buen asado", "limpiar"] True

--convertirEnPowerRanger :: String -> Persona -> PowerR -> PowerR
--convertirEnPowerRanger color  persona =  armarPowerR color (habilidadesPotenciadas (habilidades persona)) (nivelPeleaCantidadLetras (habilidades persona))

nivelPeleaCantidadLetras :: Persona -> Int
nivelPeleaCantidadLetras persona = length ((concat . habilidades) persona)

habilidadesPotenciadas :: Persona -> [String]
habilidadesPotenciadas persona = map("Super" ++) (habilidades persona)

aramarPowerR :: String -> [String] -> Int -> PowerR
aramarPowerR color habilidades nivel = PowerR { color = color, habilidadesPowerR = habilidades, nivelPelea = nivel}