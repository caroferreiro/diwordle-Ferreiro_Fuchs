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
match' objetivo intento = compararLetras' objetivo intento (aparicionesLetras objetivo) 0

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
match _ [] = []
match objetivo intento = -- compararLetras objetivo intento (aparicionesLetras objetivo)
    let (resultadoParcial, frecuenciasRestantes) = marcarCorrectos objetivo intento (aparicionesLetras objetivo)
    in marcarLIoNP resultadoParcial frecuenciasRestantes intento

marcarCorrectos :: String -> String -> [(Char, Int)] -> ([(Char, Match)], [(Char, Int)])
marcarCorrectos [] [] frecuencias = ([], frecuencias)   -- caso base
marcarCorrectos (o : objetivo') (i : intento') frecuencias
  | o == i =
      let nuevasFrecuencias = actualizarFrecuencias frecuencias o
          (resultado, frecuenciasActualizadas) = marcarCorrectos objetivo' intento' nuevasFrecuencias
      in ((i, Correcto) : resultado, frecuenciasActualizadas)
  | otherwise =
      let (resultado, frecuencias') = marcarCorrectos objetivo' intento' frecuencias
      in ((i, NoPertenece) : resultado, frecuencias')

marcarLIoNP :: [(Char, Match)] -> [(Char, Int)] -> String -> [(Char, Match)]
marcarLIoNP [] _ [] = []
marcarLIoNP ((i, Correcto) : parcial') frecuencias (_ : intento') = (i, Correcto) : marcarLIoNP parcial' frecuencias intento'
marcarLIoNP ((i, NoPertenece) : parcial') frecuencias (_ : intento') =
    if apariciones frecuencias i > 0
      then -- si la letra está presente, pero no en esta posición, y su frecuencia en el objetivo es > 0 -> LugarIncorrecto 
          let nuevasFrecuencias = actualizarFrecuencias frecuencias i
          in (i, LugarIncorrecto) : marcarLIoNP parcial' nuevasFrecuencias intento'
      else
        (i, NoPertenece) : marcarLIoNP parcial' frecuencias intento'
marcarLIoNP _ _ _ = []


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

-- >>> match "bijol" "bombo"
-- [('b',Correcto),('o',LugarIncorrecto),('m',NoPertenece),('b',NoPertenece),('o',NoPertenece)]
