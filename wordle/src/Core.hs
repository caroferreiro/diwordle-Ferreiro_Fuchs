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

match' :: String -> String -> [(Char, Match)]
match' objetivo intento = compararLetras objetivo intento (aparicionesLetras objetivo)

compararLetras' :: String -> String -> [(Char, Int)] -> Int -> [(Char, Match)]
compararLetras' _ [] _ _ = []   
compararLetras' objetivo (x : xs) objetivoFreq idx = evaluarLetra x objetivo objetivoFreq idx : compararLetras' objetivo xs (actualizarFrecuencias objetivoFreq x) (idx + 1)

evaluarLetra :: Char -> String -> [(Char, Int)] -> Int -> (Char, Match)
evaluarLetra letra objetivo objetivoFreq idx
   | idx < length objetivo && letra == objetivo !! idx = (letra, Correcto)   -- caso 1: la letra está en la posición correcta
   | letra `elem` objetivo && apariciones objetivoFreq letra > 0 = (letra, LugarIncorrecto)    -- caso 2: la letra está en otro lugar de la palabra
   | otherwise = (letra, NoPertenece)    -- caso 3: la letra no pertenece a la palabra

-- >>> match' "bomba" "bamba"
-- [('b',Correcto),('a',NoPertenece),('m',Correcto),('b',Correcto),('a',Correcto)]

-- | PROBLEMA: La segunda 'a' debería ser marcada como NoPertenece
-- | Esto pasa porque las palabras se analizan secuencialmente
-- | Deberíamos marcar primero las letras correctas y después analizar si la letra pertenece o no a la palabra en un lugar incorrecto


match :: String -> String -> [(Char, Match)]
match objetivo intento = compararLetras objetivo intento (aparicionesLetras objetivo)

compararLetras :: String -> String -> [(Char, Int)] -> [(Char, Match)]
compararLetras objetivo intento objetivoFreq =
  let resultadoPreliminar = zipWith (\i o -> if i == o then (i, Correcto) else (i, NoPertenece)) intento objetivo
      letrasCorrectas = filter (\(_, info) -> info == Correcto) resultadoPreliminar
      frecuenciasActualizadas = foldl actualizarFrecuencias objetivoFreq (map fst letrasCorrectas)
  in zipWith (evaluarLugarIncorrecto frecuenciasActualizadas) resultadoPreliminar intento

evaluarLugarIncorrecto :: [(Char, Int)] -> (Char, Match) -> Char -> (Char, Match)
evaluarLugarIncorrecto _ (letra, Correcto) _ = (letra, Correcto)
evaluarLugarIncorrecto _ (letra, LugarIncorrecto) _ = (letra, LugarIncorrecto)
evaluarLugarIncorrecto frecuencias (_, NoPertenece) letraIntento
    | apariciones frecuencias letraIntento > 0 = (letraIntento, LugarIncorrecto)
    | otherwise = (letraIntento, NoPertenece)

aparicionesLetras :: String -> [(Char, Int)]
aparicionesLetras "" = []
aparicionesLetras (x : xs) = (x, cantApariciones (x : xs) x) : aparicionesLetras (eliminarOcurrencias x xs)

cantApariciones :: String -> Char -> Int
cantApariciones "" _ = 0 
cantApariciones (x : xs) c = if x == c then 1 + cantApariciones xs c else cantApariciones xs c

eliminarOcurrencias :: Char -> String -> String
eliminarOcurrencias c xs = filter (not . (== c)) xs

-- >>> aparicionesLetras "bomba" 
-- [('b',2),('o',1),('m',1),('a',1)]

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

-- >>> match "bomba" "bamba"
-- [('b',Correcto),('a',NoPertenece),('m',Correcto),('b',Correcto),('a',Correcto)]
