module CLI (main) where

import TinyApp.Interactive (Sandbox(..), Key(..), Event(..), ContinueExit(..), runInteractive)
import System.Environment (getArgs)
import Core (Match(..), match)
import Wordle (Juego, Estado(..), objetivo, longitudObjetivo, realizarIntento, nuevo, estadoJuego, obtenerIntentos, intentosDisponibles)
import Data.Char (toUpper)

ansiResetColor, ansiBgYellowColor, ansiBgGreenColor, ansiBgRedColor :: String
ansiResetColor = "\ESC[39m\ESC[49m"
ansiBgYellowColor = "\ESC[103m"
ansiBgGreenColor = "\ESC[42m"
ansiBgRedColor = "\ESC[41m"

data State = State
    {
        juego :: Juego,
        intentoActual :: String,
        mensajeError :: String
    }


main :: IO ()
main = do
    args <- getArgs
    let target = if null args then "HORQUILLA" else head args 
    let intentosTotales = 6
    runInteractive (jugar target intentosTotales)

jugar :: String -> Int -> Sandbox State
jugar target intentosTotales =
    Sandbox
      {
        initialize = 
          State
            {
              juego = nuevo target intentosTotales,
              intentoActual = "",
              mensajeError = ""
            },
            
        render = \s -> unlines
          [  
            if null (obtenerIntentos (juego s)) && intentoActual s == "" && mensajeError s == ""
              then "¡Bienvenido a Wordle App!"
              else mensajeError s,
            " ",
            "Palabra secreta: " ++  replicate (longitudObjetivo target) '*',
            "Intentos realizados:",
            unlines (map (`renderIntento` s) (obtenerIntentos (juego s))),
            "Intento actual: " ++ intentoActual s,
            "Intentos restantes: " ++ show (intentosDisponibles (juego s)),
            " ",
            case estadoJuego (juego s) of
                Ganó -> "¡Ganaste!"
                Perdió -> "Perdiste :("
                EnProgreso -> ""
          ],
        
        update = \(Key key _) s ->
          case key of
            KEsc -> (s, Exit)
            KEnter -> 
                let nuevoEstado = procesarIntento s
                in if estadoJuego (juego nuevoEstado) /= EnProgreso
                    then (nuevoEstado, Exit)
                    else (nuevoEstado, Continue)
            KBS -> (s {intentoActual = init (intentoActual s)}, Continue)
            KChar c -> (s { intentoActual = intentoActual s ++ [toUpper c] }, Continue)
            _ -> (s, Continue)
      }

procesarIntento :: State -> State
procesarIntento s = 
    case realizarIntento (juego s) (intentoActual s) of
        Left err -> s {mensajeError = err, intentoActual = ""} 
        Right juego' -> s {juego = juego', intentoActual = "", mensajeError = ""}

renderIntento :: String -> State -> String
renderIntento intento s = 
    let renderizado = map renderLetra (match (objetivo (juego s)) intento)
        casillas = map (\c -> "| " ++ c ++ " ") renderizado
        fila = concat casillas ++ "|"
    in "+---+---+---+---+---+" ++ "\n" ++ fila ++ "\n" ++ "+---+---+---+---+---+"

renderLetra :: (Char, Match) -> String
renderLetra (c, Correcto) = ansiBgGreenColor ++ [c] ++ ansiResetColor      -- Letra correcta y en la posición correcta
renderLetra (c, LugarIncorrecto) = ansiBgYellowColor ++ [c] ++ ansiResetColor      -- Letra correcta en posición incorrecta
renderLetra (c, NoPertenece) = ansiBgRedColor ++ [c] ++ ansiResetColor     -- Letra incorrecta