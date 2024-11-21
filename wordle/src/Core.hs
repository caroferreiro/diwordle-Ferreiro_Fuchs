module Core (
    Match(..), 
    match,
    aparicionesLetras
    ) where

data Match
  = Correcto
  | LugarIncorrecto
  | NoPertenece
  deriving (Eq, Show)

match :: String -> String -> [(Char, Match)]
match objetivo intento = compararLetras objetivo intento (aparicionesLetras objetivo) 0

compararLetras :: String -> String -> [(Char, Int)] -> Int -> [(Char, Match)]
compararLetras _ [] _ _ = []   
compararLetras objetivo (x : xs) objetivoFreq idx = evaluarLetra x objetivo objetivoFreq idx : compararLetras objetivo xs (actualizarFrecuencias objetivoFreq x) (idx + 1)

evaluarLetra :: Char -> String -> [(Char, Int)] -> Int -> (Char, Match)
evaluarLetra letra objetivo objetivoFreq idx
  | idx < length objetivo && letra == objetivo !! idx = (letra, Correcto)   -- caso 1: la letra está en la posición correcta
  | letra `elem` objetivo && apariciones objetivoFreq letra > 0 = (letra, LugarIncorrecto)    -- caso 2: la letra está en otro lugar de la palabra
  | otherwise = (letra, NoPertenece)    -- caso 3: la letra no pertenece a la palabra

-- >>> match "posta" "seria"
-- [('s',LugarIncorrecto),('e',NoPertenece),('r',NoPertenece),('i',NoPertenece),('a',Correcto)]

aparicionesLetras :: String -> [(Char, Int)]
aparicionesLetras "" = []
aparicionesLetras (x : xs) = (x, cantApariciones (x : xs) x) : aparicionesLetras (eliminarOcurrencias x xs)

cantApariciones :: String -> Char -> Int
cantApariciones "" _ = 0 
cantApariciones (x : xs) c = if x == c then 1 + cantApariciones xs c else cantApariciones xs c

eliminarOcurrencias :: Char -> String -> String
eliminarOcurrencias c xs = filter (not . (== c)) xs

-- >>> aparicionesLetras "playa" 
-- [('p',1),('l',1),('a',2),('y',1)]

actualizarFrecuencias :: [(Char, Int)] -> Char -> [(Char, Int)]
actualizarFrecuencias [] _ = []
actualizarFrecuencias ((c, n) : xs) char
  | c == char && n > 1 = (c, n - 1) : xs 
  | c == char = xs
  | otherwise = (c, n) : actualizarFrecuencias xs char

apariciones :: [(Char, Int)] -> Char -> Int
apariciones [] _ = 0
apariciones ((c, n) : xs) char
  | c == char = n
  | otherwise = apariciones xs char

-- >>> match "palta" "paella"
-- [('p',Correcto),('a',Correcto),('e',NoPertenece),('l',LugarIncorrecto),('l',NoPertenece),('a',LugarIncorrecto)]

