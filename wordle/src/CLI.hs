module CLI (main) where

import TinyApp.Interactive (Sandbox(..), Key(..), Event(..), ContinueExit(..), runInteractive)
import System.Environment (getArgs)
import Core (Match(..), match)
import Wordle (Juego, Estado(..), objetivo, longitudObjetivo, realizarIntento, nuevo, estadoJuego, obtenerIntentos, intentosDisponibles)
import Data.Char (toUpper)
import System.Random.Stateful (uniformRM, globalStdGen)

ansiResetColor, ansiBgYellowColor, ansiBgGreenColor, ansiBgRedColor :: String
ansiResetColor = "\ESC[39m\ESC[49m"
ansiBgYellowColor = "\ESC[103m"
ansiBgGreenColor = "\ESC[42m"
ansiBgRedColor = "\ESC[41m"

data State = State
    {
        juego :: Juego,
        intentoActual :: String,
        mensajeError :: String,
        finalizar :: Bool
    }

getDiccionario :: FilePath -> IO [String]
getDiccionario archivo = do
    diccionario <- readFile archivo
    let palabras = lines diccionario
    return palabras

jugar :: String -> Int -> (String -> Bool) -> Sandbox State
jugar target intentosTotales validarPalabra =
    Sandbox
      {
        initialize = 
          State
            {
              juego = nuevo target intentosTotales,
              intentoActual = "",
              mensajeError = "",
              finalizar = False
            },
            
        render = \s -> unlines
          [  
            if null (obtenerIntentos (juego s)) && intentoActual s == "" && mensajeError s == ""
              then "¡Bienvenido a Wordle App!"
              else mensajeError s,
            " ",
            "Palabra secreta: " ++  replicate (longitudObjetivo target) '*',
            "Intento actual: " ++ intentoActual s,
            "Jugar:",
            unlines (map (`renderIntento` s) (obtenerIntentos (juego s))),
            "Intentos restantes: " ++ show (intentosDisponibles (juego s)),
            " ",
            case estadoJuego (juego s) of
                Ganó -> "¡Ganaste!"
                Perdió -> "Perdiste :("
                EnProgreso -> ""
          ],
        
        update = \(Key key _) s ->
          if finalizar s  -- Si el estado indica que el juego terminó, entonces se hace Exit
            then (s, Exit)
          else case key of
            KEsc -> (s, Exit)
            KEnter -> 
                let nuevoEstado = procesarIntento validarPalabra s
                in if estadoJuego (juego nuevoEstado) /= EnProgreso
                    then (nuevoEstado {finalizar = True}, Continue)
                    else (nuevoEstado, Continue)
            KBS -> (s {intentoActual = init (intentoActual s)}, Continue)
            KChar c -> (s { intentoActual = intentoActual s ++ [toUpper c] }, Continue)
            _ -> (s, Continue)
      }

procesarIntento :: (String -> Bool) -> State -> State
procesarIntento validarPalabra s = 
    let palabra = intentoActual s
    in if validarPalabra palabra
        then case realizarIntento (juego s) (intentoActual s) of
            Left err -> s {mensajeError = err, intentoActual = ""} 
            Right juego' -> s {juego = juego', intentoActual = "", mensajeError = ""}
        else s {mensajeError = "Palabra inválida", intentoActual = ""}

renderIntento :: String -> State -> String
renderIntento intento s = 
    let renderizado = map renderLetra (match (objetivo (juego s)) intento) -- map renderLetra: aplica renderLetra a cada (Char, Match) que devuelve match
        letras = concat renderizado
        cantCasillas = longitudObjetivo (objetivo (juego s))
        casillas = concatMap (const "+---") [0..cantCasillas]
    in casillas ++ "+" ++ "\n" ++ letras

renderLetra :: (Char, Match) -> String
renderLetra (c, Correcto) = ansiResetColor ++ "| " ++ ansiBgGreenColor ++ [c] ++ ansiResetColor ++ " |"   -- Letra correcta y en la posición correcta
renderLetra (c, LugarIncorrecto) = ansiResetColor ++ "| " ++ ansiBgYellowColor ++ [c] ++ ansiResetColor ++ " |"   -- Letra correcta en posición incorrecta
renderLetra (c, NoPertenece) = ansiResetColor ++ "| " ++ ansiBgRedColor ++ [c] ++ ansiResetColor ++ " |"  -- Letra incorrectaaa



main :: IO ()
main = do
    args <- getArgs
    diccionario <- getDiccionario "diccionario.txt"
    idx <- uniformRM (0, length diccionario - 1) globalStdGen
    let target = if null args then diccionario !! idx else map toUpper (head args)
    let intentosTotales = 6
    let validarPalabra palabra = elem (map toUpper palabra) diccionario
    runInteractive (jugar target intentosTotales validarPalabra)