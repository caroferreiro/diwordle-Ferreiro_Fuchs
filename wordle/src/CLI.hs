module CLI (main) where

import TinyApp.Interactive (Sandbox(..), Key(..), Event(..), ContinueExit(..), runInteractive)
import System.Environment (getArgs)
import Core (Match(..), match, aparicionesLetras)
import Wordle (Juego (objetivo), Estado(..), objetivo, longitudObjetivo, realizarIntento, nuevo, estadoJuego, obtenerIntentos, intentosDisponibles)
import Data.Char (toUpper)
import System.Random.Stateful (uniformRM, globalStdGen)
import Data.List (intercalate)
import JuegoDaily (EstadoJuego(..))
import Data.Time (getCurrentTime, utctDay)
import Data.Time.Format.ISO8601 (iso8601Show)

ansiResetColor, ansiBgYellowColor, ansiBgGreenColor, ansiBgRedColor :: String
ansiResetColor = "\ESC[39m\ESC[49m"
ansiBgYellowColor = "\ESC[103m"
ansiBgGreenColor = "\ESC[42m"
ansiBgRedColor = "\ESC[41m"

data State = State
    {
        juego :: Juego,
        intentoActual :: String,
        letrasDescartadas :: [Char],
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
              letrasDescartadas = [],
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
            concatMap (const "+---") [1..longitudObjetivo (objetivo (juego s))] ++ "+",
            unlines (map (`renderIntento` s) (obtenerIntentos (juego s))),
            "Intentos restantes: " ++ show (intentosDisponibles (juego s)),
            " ",
            case estadoJuego (juego s) of
                Ganó -> "¡Ganaste!"
                Perdió -> "Perdiste :( La palabra era: " ++ objetivo (juego s)
                EnProgreso -> ""
          ],
        
        update = \(Key key _) s ->
          if finalizar s  -- si el estado indica que el juego terminó, entonces se hace Exit
            then (s, Exit)
          else case key of
            KEsc -> (s, Exit)
            KEnter -> 
                let nuevoEstado = procesarIntento validarPalabra s
                in if estadoJuego (juego nuevoEstado) /= EnProgreso
                    then (nuevoEstado {finalizar = True}, Continue)
                    else (nuevoEstado, Continue)
            KBS -> (chequearLetras s {intentoActual = init (intentoActual s)}, Continue)
            KChar c -> ( chequearLetras s { intentoActual = intentoActual s ++ [toUpper c] }, Continue)
            _ -> (s, Continue)
      }

chequearLetras :: State -> State
chequearLetras s =
  let descartadas = filter (\c -> c `elem` letrasDescartadas s) (intentoActual s)
  in if length descartadas == 1
    then s {mensajeError = "La letra " ++ descartadas ++ " ya fue descartada"}
    else 
      if length descartadas > 1
        then s {mensajeError = "Las letras " ++ intercalate ", " (map (: []) descartadas) ++ " ya fueron descartadas"}
        else s {mensajeError = ""}

procesarIntento :: (String -> Bool) -> State -> State
procesarIntento validarPalabra s = 
    let palabra = intentoActual s
    in if validarPalabra palabra
        then case realizarIntento (juego s) (intentoActual s) of
            Left err -> s {mensajeError = err, intentoActual = ""} 
            Right juego' -> s {juego = juego', intentoActual = "", letrasDescartadas = agregarDescartada s, mensajeError = ""}
        else s {mensajeError = "Palabra inválida", intentoActual = ""}

agregarDescartada :: State -> [Char]
agregarDescartada s = 
  let letrasObjetivo = aparicionesLetras (objetivo (juego s))
      descartadas = filter (\letra -> not (letra `elem` map fst letrasObjetivo)) (intentoActual s)
  in letrasDescartadas s ++ descartadas

renderIntento :: String -> State -> String
renderIntento intento s = 
    let renderizado = map renderLetra (match (objetivo (juego s)) intento) -- map renderLetra: aplica renderLetra a cada (Char, Match) que devuelve match
        letras = concat renderizado
        cantCasillas = longitudObjetivo (objetivo (juego s))
        casillas = concatMap (const "+---") [1..cantCasillas]
    in "| " ++ letras ++ "\n" ++ casillas ++ "+"

renderLetra :: (Char, Match) -> String
renderLetra (c, Correcto) = ansiBgGreenColor ++ [c] ++ ansiResetColor ++ " | "   -- Letra correcta y en la posición correcta
renderLetra (c, LugarIncorrecto) = ansiBgYellowColor ++ [c] ++ ansiResetColor ++ " | "   -- Letra correcta en posición incorrecta
renderLetra (c, NoPertenece) = ansiBgRedColor ++ [c] ++ ansiResetColor ++ " | "  -- Letra incorrectaaa



main :: IO ()
main = do
    args <- getArgs
    diccionario <- getDiccionario "diccionario.txt"
    estado <- leerEstado "estado.json"
    let fechaActual = fmap iso8601Show getCurrentDay
    let (modo, palabra, fecha) = parseArgs args estado diccionario fechaActual
    let estadoDiario = case estado of
                          Just st -> st
                          Nothing -> EstadoJuego [] fechaActual []
    let intentosTotales = 6
    let validarPalabra palabra = if null args then elem (map toUpper palabra) diccionario else True
    let nuevoEstado = actualizarHistorial estado fecha palabra
    runInteractive (jugar palabra intentosTotales validarPalabra)
    guardarIntentos estadoDiario

getCurrentDay :: IO Day
getCurrentDay = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  pure $ localDay (utcToLocalTime tz t)

parseArgs :: [String] -> Maybe EstadoJuego -> [String] -> Day -> (String, String, Day)
parseArgs args estado diccionario fechaActual =
    case args of
      ("--random":_) -> ( "random", obtenerPalabraRandom diccionario, fechaActual)
      ("--daily":[]) -> obtenerEstadoDaily estado fechaActual diccionario
      ("--daily":fechaStr:_) -> 
            case parseDay fechaStr of
                Just fecha -> ("daily", obtenerPalabraPorFecha estado fecha diccionario, fecha)
                Nothing -> error "Formato de fecha inválido. Use YYYY-MM-DD."
      ("--palabra":palabra:_) -> ("palabra", palabra, fechaActual)
      _ -> obtenerEstadoDaily estado fechaActual diccionario


actualizarHistorial :: Maybe EstadoJuego -> Day -> String -> EstadoJuego
actualizarHistorial (Just estado) fecha palabra =
    estado { historial = (fecha, palabra) : filter ((/= fecha) . fst) (historial estado) }
actualizarHistorial Nothing fecha palabra =
    EstadoJuego { historial = [(fecha, palabra)], intentosDiaActual = [] }      

parseDay :: String -> Maybe Day
parseDay s = iso8601ParseM s

obtenerPalabraRandom :: [String] -> String
obtenerPalabraRandom diccionario = do
    idx <- uniformRM (0, length diccionario - 1) globalStdGen
    return $ diccionario !! idx


obtenerPalabraPorFecha :: Maybe EstadoJuego -> Day -> [String] -> String
obtenerPalabraPorFecha (Just estado) fecha diccionario =
    case lookup fecha (historial estado) of
        Just palabra -> palabra
        Nothing -> obtenerPalabraRandom diccionario
obtenerPalabraPorFecha Nothing _ diccionario = obtenerPalabraRandom diccionario
 
insertarPalabra :: Maybe EstadoJuego -> String -> [String] -> EstadoJuego
insertarPalabra (Just estado) fecha diccionario= 
  let palabraNueva = obtenerPalabraRandom diccionario
        in EstadoJuego
            { historial = (fecha, palabraNueva) : historial estado
            }
insertarPalabra Nothing fecha diccionario =
    let palabraNueva = obtenerPalabraRandom diccionario
    in EstadoJuego
        { historial = [(fecha, palabraNueva)]
        }


obtenerEstadoDaily :: Maybe EstadoJuego -> Day -> [String] -> (String, String, Day)
obtenerEstadoDaily (Just estado) fechaActual diccionario =
    case lookup fechaActual (historial estado) of
        Just palabra -> ("daily", palabra, fechaActual)
        Nothing ->
            let palabraNueva = obtenerPalabraRandom diccionario
            in ("daily", palabraNueva, fechaActual)
obtenerEstadoDaily Nothing fechaActual diccionario =
    let palabraNueva = obtenerPalabraRandom diccionario
    in ("daily", palabraNueva, fechaActual)

actualizarIntentos :: EstadoJuego -> String -> EstadoJuego
actualizarIntentos estado intento = 
    estado { intentosDiaActual = intento : intentosDiaActual estado }

guardarIntentos :: EstadoJuego -> IO ()
guardarIntentos estado = do
    guardarEstado "estado.json" estado
