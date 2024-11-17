module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Match
  = Correcto
  | LugarIncorrecto
  | NoPertenece
  deriving (Eq, Show)

match :: String -> String -> [(Char, Match)]
match intento palabra = matchAux intento palabra 0


matchAux :: String -> String -> Int -> [(Char, Match)]
matchAux [] _ _ = [] 
matchAux (x:xs) palabra idx = evaluarLetra x idx palabra : matchAux xs palabra (idx + 1)


evaluarLetra :: Char -> Int -> String -> (Char, Match)
evaluarLetra letra idx solucion
  | idx < length solucion && letra == solucion !! idx = (letra, Correcto)
  | letra `elem` solucion = (letra, LugarIncorrecto) 
  | otherwise = (letra, NoPertenece)


-- >>> match "posta" "seria"
-- [('p',NoPertenece),('o',NoPertenece),('s',LugarIncorrecto),('t',NoPertenece),('a',Correcto)]

data Juego = Juego {
    palabra :: String,
    intentos :: Int
    }
    deriving (Show)