module Wordle (
  Juego,
  objetivo, 
  maxIntentos,
  Estado(..),
  nuevo,
  longitudObjetivo,
  estadoJuego,
  obtenerIntentos,
  intentosDisponibles,
  realizarIntento
)
  where

import Core (Match(..), match)

data Juego = Juego 
  {
    objetivo :: String,
    maxIntentos :: Int,
    intentos :: [String],
    estado :: Estado
  }
  deriving (Show)

data Estado
    = EnProgreso
    | Ganó
    | Perdió
    deriving (Eq, Show)

nuevo :: String -> Int -> Juego
nuevo target intentosTotales = Juego target intentosTotales [] EnProgreso

longitudObjetivo :: String -> Int
longitudObjetivo target = length target

obtenerIntentos :: Juego -> [String]
obtenerIntentos j = intentos j

intentosDisponibles :: Juego -> Int
intentosDisponibles j = maxIntentos j - length (intentos j)

estadoJuego :: Juego -> Estado
estadoJuego = estado

validarIntento :: Juego -> String -> Either String String
validarIntento j intento
    | length intento /= longitudObjetivo (objetivo j) = Left "Longitud inválida"
    | not (all esLetraValida intento) = Left "El intento contiene caracteres inválidos"
    | otherwise = Right intento
    where 
      esLetraValida c = c >= 'A' && c <= 'Z'

realizarIntento :: Juego -> String -> Either String Juego
realizarIntento j intento =
  case validarIntento j intento of
    Left err -> Left err
    Right intentoValido -> 
      let resultado = match (objetivo j) intentoValido 
          nuevoEstado = actualizarEstado j resultado
          j' = j {intentos = intentoValido : intentos j, estado = nuevoEstado}
      in Right j'

actualizarEstado :: Juego -> [(Char, Match)] -> Estado
actualizarEstado j resultadoIntento 
    | all (\x -> snd x == Correcto) resultadoIntento = Ganó
    | intentosDisponibles j == 0 = Perdió
    | otherwise = EnProgreso
