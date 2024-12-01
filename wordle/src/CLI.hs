module CLI (main) where

import TinyApp.Interactive (Sandbox(..), Key(..), Event(..), ContinueExit(..), runInteractive')
import System.Environment (getArgs)
import Core (Match(..), match, aparicionesLetras)
import Wordle (Juego, Estado(..), objetivo, longitudObjetivo, realizarIntento, nuevo, estadoJuego, obtenerIntentos, intentosDisponibles)
import Data.Char (toUpper)
import System.Random.Stateful (uniformRM, globalStdGen)
import Data.List (intercalate)
import ArchivoEstado (EstadoJuego(..), leerEstado, guardarEstado)
import ArchivoEstadisticas (Estadisticas(..), leerEstadisticas, guardarEstadisticas, inicializarEstadisticas, promedioIntentos)
import Data.Time (Day, getCurrentTime, getCurrentTimeZone, localDay, utcToLocalTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)

ansiResetColor, ansiBgYellowColor, ansiBgGreenColor, ansiBgRedColor :: String
ansiResetColor = "\ESC[39m\ESC[49m"
ansiBgYellowColor = "\ESC[103m"
ansiBgGreenColor = "\ESC[42m"
ansiBgRedColor = "\ESC[41m"

ansiUnderline, ansiUnderlineOff :: String
ansiUnderline = "\ESC[4m"
ansiUnderlineOff = "\ESC[24m"

data State = State
    {
        juego :: Juego,
        intentoActual :: String,
        posicionActual :: Int,
        letrasDescartadas :: [Char],
        mensajeError :: String,
        finalizar :: Bool,
        estadisticas :: Estadisticas
    }

jugar :: State -> Sandbox State
jugar estadoInicial =
    Sandbox
      {
        initialize = estadoInicial,
        render = \s -> unlines
          [  
            if null (obtenerIntentos (juego s)) && intentoActual s == "" && mensajeError s == ""
              then "¡Bienvenido a Wordle App!"
              else mensajeError s,
            " ",
            "Palabra secreta: " ++  replicate (longitudObjetivo (objetivo (juego s))) '*',
            "Intento actual: " ++ renderIntentoActual (intentoActual s) (posicionActual s),
            "Jugar:",
            concatMap (const "+---") [1..longitudObjetivo (objetivo (juego s))] ++ "+",
            unlines (map (`renderJugada` s) (obtenerIntentos (juego s))),
            "Intentos restantes: " ++ show (intentosDisponibles (juego s)),
            " ",
            case estadoJuego (juego s) of
                Ganó -> let estadoActualizado = actualizarEstadisticas (estadisticas s) s
                            estadisticasActualizadas = estadisticas estadoActualizado
                        in "¡Ganaste!" <> "\n" <> renderEstadisticas estadisticasActualizadas
                Perdió -> let estadoActualizado = actualizarEstadisticas (estadisticas s) s
                              estadisticasActualizadas = estadisticas estadoActualizado
                          in "Perdiste :(" <> "\n" <> "La palabra era: " ++ objetivo (juego s) <> "\n" <> renderEstadisticas estadisticasActualizadas
                EnProgreso -> ""
          ],
        update = \(Key key _) s ->
          if finalizar s  -- si el estado indica que el juego terminó, entonces se hace Exit
            then (s, Exit)
          else case key of
            KEsc -> (s, Exit)
            KEnter -> (procesarIntento s (intentoActual s), Continue)
            KBS -> 
              if null (intentoActual s)
                then (s, Continue)
                else if posicionActual s == length (intentoActual s)
                  then (moverIzq (chequearLetras s {intentoActual = init (intentoActual s)}), Continue)
                  else (chequearLetras s {intentoActual = init (intentoActual s)}, Continue)
            KChar ' ' -> (hint s, Continue)
            KChar c -> (chequearLetras (actualizarIntento s (toUpper c)), Continue)
            KLeft -> (moverIzq s, Continue)
            KRight -> (moverDer s, Continue)
            _ -> (s, Continue)
      }

actualizarIntento :: State -> Char -> State
actualizarIntento s c = 
    let intento = intentoActual s
        posActual = posicionActual s
    in if posActual == length intento
      then moverDer (s {intentoActual = intentoActual s ++ [c]})
      else s {intentoActual = take posActual intento ++ [c] ++ drop (posActual + 1) intento}

procesarIntento :: State -> String -> State
procesarIntento s palabra = 
    case realizarIntento (juego s) palabra of
      Left err -> s {mensajeError = err, intentoActual = ""} 
      Right juego' -> 
        let nuevoEstado = s {juego = juego', intentoActual = "", letrasDescartadas = agregarDescartada s, mensajeError = ""}
        in if estadoJuego juego' /= EnProgreso
            then nuevoEstado {finalizar = True}
            else nuevoEstado

agregarDescartada :: State -> [Char]
agregarDescartada s = 
  let letrasObjetivo = aparicionesLetras (objetivo (juego s))
      descartadas = filter (\letra -> not (letra `elem` map fst letrasObjetivo)) (intentoActual s)
  in letrasDescartadas s ++ descartadas

chequearLetras :: State -> State
chequearLetras s =
  let descartadas = filter (\c -> c `elem` letrasDescartadas s) (intentoActual s)
  in if length descartadas == 1
    then s {mensajeError = "La letra " ++ descartadas ++ " ya fue descartada"}
    else 
      if length descartadas > 1
        then s {mensajeError = "Las letras " ++ intercalate ", " (map (: []) descartadas) ++ " ya fueron descartadas"}
        else s {mensajeError = ""}

hint :: State -> State
hint s =
  let 
      target = objetivo (juego s)
      intentosLetras = concat (obtenerIntentos (juego s))
      noAdivinadas = filter (`notElem` intentosLetras) target
  in 
    if not (null noAdivinadas)
      then s {mensajeError = "Sugerencia: " ++ [head noAdivinadas]}
      else s {mensajeError = "No hay más letras para sugerir."}

moverDer :: State -> State
moverDer s = 
    if posicionActual s < length (intentoActual s)
        then s {posicionActual = (posicionActual s) + 1}
        else s

moverIzq :: State -> State
moverIzq s = 
    if posicionActual s > 0
        then s {posicionActual = (posicionActual s) - 1}
        else s

actualizarEstadisticas :: Estadisticas -> State -> State
actualizarEstadisticas est s =
    s { estadisticas =
      Estadisticas 
        {
          partidasGanadas = partidasGanadas est + if gano then 1 else 0,
          partidasPerdidas = partidasPerdidas est + if estadoJuego (juego s) == Perdió then 1 else 0,
          intentosHastaGanar = if gano then intentosHastaGanar est ++ [intentos] else intentosHastaGanar est,
          racha = if gano then racha est + 1 else 0
        }
    }
    where 
        gano = estadoJuego (juego s) == Ganó
        intentos = length (obtenerIntentos (juego s))

--
renderIntentoActual :: String -> Int -> String
renderIntentoActual intento posActual =
    if posActual < 0 || posActual >= length intento
      then intento 
      else
        let subrayada = ansiUnderline ++ [intento !! posActual] ++ ansiUnderlineOff 
        in take posActual intento ++ subrayada ++ drop (posActual + 1) intento

renderJugada :: String -> State -> String
renderJugada intento s = 
    let renderizado = map renderLetra (match (objetivo (juego s)) intento) -- map renderLetra: aplica renderLetra a cada (Char, Match) que devuelve match
        letras = concat renderizado
        cantCasillas = longitudObjetivo (objetivo (juego s))
        casillas = concatMap (const "+---") [1..cantCasillas]
    in "| " ++ letras ++ "\n" ++ casillas ++ "+"

renderLetra :: (Char, Match) -> String
renderLetra (c, Correcto) = ansiBgGreenColor ++ [c] ++ ansiResetColor ++ " | "   -- Letra correcta y en la posición correcta
renderLetra (c, LugarIncorrecto) = ansiBgYellowColor ++ [c] ++ ansiResetColor ++ " | "   -- Letra correcta en posición incorrecta
renderLetra (c, NoPertenece) = ansiBgRedColor ++ [c] ++ ansiResetColor ++ " | "  -- Letra incorrecta

renderEstadisticas :: Estadisticas -> String
renderEstadisticas est = unlines
    [ "Estadísticas:",
      "Juegos ganados: " ++ show (partidasGanadas est),
      "Promedio de intentos hasta ganar: " ++ show (promedioIntentos (intentosHastaGanar est)),
      "Juegos perdidos: " ++ show (partidasPerdidas est),
      "Racha actual sin perder: " ++ show (racha est)
    ]

--
main :: IO ()
main = do
    args <- getArgs
    diccionario <- getDiccionario "diccionario.txt"
    idx <- uniformRM (0, length diccionario - 1) globalStdGen
    fechaActual <- getCurrentDay
    estadoArchivo <- leerEstado "estado.json"
    estadisticasArchivo <- leerEstadisticas "estadisticas.json"

    let intentosTotales = 6
    let validarPalabra palabra = 
          case args of
            ("--palabra":_) -> True
            _ -> map toUpper palabra `elem` diccionario

    let estado = parseArgs args estadoArchivo diccionario idx fechaActual intentosTotales validarPalabra estadisticasArchivo
    s <- runInteractive' (jugar estado)
    --runInteractive (jugar palabra intentosTotales validarPalabra)

    let estadoFinal = EstadoJuego {fecha = fechaActual, objetivoAr = objetivo (juego s), intentosAr = obtenerIntentos (juego s), letrasDescartadasAr = letrasDescartadas s}
    guardarEstado args "estado.json" estadoFinal

    let estadisticasActualizadas = estadisticas (actualizarEstadisticas (estadisticas s) s)
    guardarEstadisticas args "estadisticas.json" estadisticasActualizadas

getDiccionario :: FilePath -> IO [String]
getDiccionario archivo = do
    diccionario <- readFile archivo
    let palabras = lines diccionario
    return palabras

getCurrentDay :: IO Day
getCurrentDay = do
  t <- getCurrentTime
  tz <- getCurrentTimeZone
  pure $ localDay (utcToLocalTime tz t)

parseDay :: String -> Maybe Day
parseDay s = iso8601ParseM s

parseArgs :: [String] -> Maybe EstadoJuego -> [String] -> Int -> Day -> Int -> (String -> Bool) -> Maybe Estadisticas -> State
parseArgs args estadoArchivo diccionario idx fechaActual intentosTotales validarPalabra estadisticasArchivo =
    case args of
      ["--daily"] -> cargarEstado estadoArchivo diccionario idx fechaActual intentosTotales validarPalabra estadisticasArchivo
      ("--daily":fechaStr:_) -> 
          case parseDay fechaStr of
              Just fechaIngresada -> 
                if fechaIngresada == fechaActual
                  then cargarEstado estadoArchivo diccionario idx fechaActual intentosTotales validarPalabra estadisticasArchivo
                  else error "La fecha ingresada no coincide con la fecha actual" 
              Nothing -> error "Fecha inválida: usar formato YYYY-MM-DD."
      ["--random"] -> inicializarRandom diccionario idx intentosTotales validarPalabra estadisticasArchivo
      ("--palabra":palabra:_) -> inicializarFija (map toUpper palabra) intentosTotales validarPalabra estadisticasArchivo
      [] -> cargarEstado estadoArchivo diccionario idx fechaActual intentosTotales validarPalabra estadisticasArchivo
      _ -> error "Argumento invalido. Argumentos validos: --daily, --random, --palabra"

inicializarRandom :: [String] -> Int -> Int -> (String -> Bool) -> Maybe Estadisticas -> State
inicializarRandom diccionario idx intentosTotales validarPalabra estadisticasArchivo = 
  State
    { 
      juego = nuevo (obtenerPalabraRandom diccionario idx) intentosTotales validarPalabra,
      intentoActual = "",
      posicionActual = 0,
      letrasDescartadas = [],
      mensajeError = "",
      finalizar = False,
      estadisticas = inicializarEstadisticas estadisticasArchivo
    }

cargarEstado :: Maybe EstadoJuego -> [String] -> Int -> Day -> Int -> (String -> Bool) -> Maybe Estadisticas -> State
cargarEstado (Just archivo) diccionario idx fechaActual intentosTotales validarPalabra estadisticasArchivo =
  if fechaActual == fecha archivo
    then
      recuperarJuego 
        (
          State
            { 
              juego = nuevo (objetivoAr archivo) intentosTotales validarPalabra,
              intentoActual = "",
              posicionActual = 0,
              letrasDescartadas = letrasDescartadasAr archivo,
              mensajeError = "",
              finalizar = False,
              estadisticas = inicializarEstadisticas estadisticasArchivo
            }
        )
        (intentosAr archivo)
    else
      State 
        { 
          juego = nuevo (obtenerPalabraRandom diccionario idx) intentosTotales validarPalabra,
          intentoActual = "",
          posicionActual = 0,
          letrasDescartadas = [],
          mensajeError = "",
          finalizar = False,
          estadisticas = inicializarEstadisticas estadisticasArchivo
        }
cargarEstado Nothing diccionario idx _ intentosTotales validarPalabra estadisticasArchivo =
  State
    { juego = nuevo (obtenerPalabraRandom diccionario idx) intentosTotales validarPalabra,
      intentoActual = "",
      posicionActual = 0,
      letrasDescartadas = [],
      mensajeError = "",
      finalizar = False,
      estadisticas = inicializarEstadisticas estadisticasArchivo 
    }

recuperarJuego :: State -> [String] -> State
recuperarJuego s intentos' = foldl procesarIntento s intentos'

inicializarFija :: String -> Int -> (String -> Bool) -> Maybe Estadisticas -> State
inicializarFija target intentosTotales validarPalabra estadisticasArchivo = 
  State
    { 
      juego = nuevo target intentosTotales validarPalabra,
      intentoActual = "",
      posicionActual = 0,
      letrasDescartadas = [],
      mensajeError = "",
      finalizar = False,
      estadisticas = inicializarEstadisticas estadisticasArchivo
    }

obtenerPalabraRandom :: [String] -> Int -> String
obtenerPalabraRandom diccionario idx = diccionario !! idx