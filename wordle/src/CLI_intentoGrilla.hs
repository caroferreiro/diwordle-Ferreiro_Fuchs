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
        mensajeError :: String,
        finalizar :: Bool
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
            "Juego:",
            unlines (generarGrilla (longitudObjetivo (objetivo (juego s))) (intentosDisponibles (juego s)) (obtenerIntentos (juego s))),
            "Intento actual: " ++ intentoActual s,
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
                let nuevoEstado = procesarIntento s
                in if estadoJuego (juego nuevoEstado) /= EnProgreso
                    then (nuevoEstado {finalizar = True}, Continue)
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

-- renderIntento :: String -> State -> String
-- renderIntento intento s = 
--     let renderizado = map renderLetra (match (objetivo (juego s)) intento) -- map renderLetra: aplica renderLetra a cada (Char, Match) que devuelve match
--         casillas = concat renderizado
--     in casillas

-- renderLetra :: (Char, Match) -> String
-- renderLetra (c, Correcto) = ansiBgGreenColor ++ [c] ++ ansiResetColor   -- Letra correcta y en la posición correcta
-- renderLetra (c, LugarIncorrecto) = ansiBgYellowColor ++ [c] ++ ansiResetColor   -- Letra correcta en posición incorrecta
-- renderLetra (c, NoPertenece) = ansiBgRedColor ++ [c] ++ ansiResetColor  -- Letra incorrectaaa

generarGrilla :: Int -> Int -> [String] -> [String]
generarGrilla ancho altura intentos =
    let grillaVacia = replicate (altura - length intentos) (generarGrillaVacia ancho)
        grillaConIntentos = map (`renderIntento` ancho) intentos
    in grillaVacia ++ grillaConIntentos

generarGrillaVacia :: Int -> String
generarGrillaVacia ancho = 
    let casillas = replicate ancho "| "
        fila = concat casillas ++ " |"
        separador = concat (replicate ancho "+--") ++ "+"
    in separador ++ "\n" ++ fila ++ "\n" ++ separador

renderIntento :: String -> Int -> String
renderIntento intento ancho = 
    let casillas = map renderLetra intento
        fila = concat casillas ++ replicate (ancho - length intento) '|'
        separador = concat (replicate ancho "+--") ++ "+"
    in separador ++ "\n" ++ fila ++ "\n" ++ separador

renderLetra :: Char -> String
renderLetra c = "| " ++ [c] ++ " "