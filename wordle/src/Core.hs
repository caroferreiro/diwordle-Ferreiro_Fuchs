module Core (Match(..), match) where

data Match
  = Correcto
  | LugarIncorrecto
  | NoPertenece
  deriving (Eq, Show)

match :: String -> String -> [(Char, Match)]
match objetivo intento = compararLetras objetivo intento 0

compararLetras :: String -> String -> Int -> [(Char, Match)]
compararLetras _ [] _ = []   
compararLetras objetivo (x : xs) idx = evaluarLetra x objetivo idx : compararLetras objetivo xs (idx + 1)

evaluarLetra :: Char -> String -> Int -> (Char, Match)
evaluarLetra letra objetivo idx
  | idx < length objetivo && letra == objetivo !! idx = (letra, Correcto)   -- caso 1: la letra está en la posición correcta
  | letra `elem` objetivo = (letra, LugarIncorrecto)    -- caso 2: la letra está en otro lugar de la palabra
  | otherwise = (letra, NoPertenece)    -- caso 3: la letra no pertenece a la palabra

-- >>> match "posta" "seria"
-- [('s',LugarIncorrecto),('e',NoPertenece),('r',NoPertenece),('i',NoPertenece),('a',Correcto)]

