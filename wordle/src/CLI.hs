module CLI (main) where

import TinyApp.Interactive (Sandbox(..), Key(..), Event(..), ContinueExit(..), runInteractive')
import System.Environment (getArgs)
import Core (Match(..), match, aparicionesLetras)
import Wordle (Juego, Estado(..), objetivo, longitudObjetivo, realizarIntento, nuevo, estadoJuego, obtenerIntentos, intentosDisponibles)
import Data.Char (toUpper)
import System.Random.Stateful (uniformRM, globalStdGen)
import Data.List (intercalate)
import ArchivoEstado (EstadoJuego(..), leerArchivo, guardarEstado)
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
        finalizar :: Bool
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
            "Intento actual: " ++ intentoActual s,
            "Jugar:",
            concatMap (const "+---") [1..longitudObjetivo (objetivo (juego s))] ++ "+",
            unlines (map (`renderIntento` s) (obtenerIntentos (juego s))),
            "Intentos restantes: " ++ show (intentosDisponibles (juego s)),
            " ",
            case estadoJuego (juego s) of
                Ganó -> "¡Ganaste!"
                Perdió -> "Perdiste :(" ++ "\n" ++  "La palabra era: " ++ objetivo (juego s)
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
        then s {posicionActual = (posicionActual s) + 1}
        else s

renderIntento :: String -> State -> String
renderIntento intento s = 
    let posActual = posicionActual s
        subrayada = ansiUnderline ++ [intento !! posActual] ++ ansiUnderlineOff
        preSubrayada = take posActual intento
        postSubrayada = drop (posActual + 1) intento

        intentoSubrayado = preSubrayada ++ subrayada ++ postSubrayada
        renderizado = map renderLetra (match (objetivo (juego s)) intentoSubrayado) -- map renderLetra: aplica renderLetra a cada (Char, Match) que devuelve match
        letras = concat renderizado

        cantCasillas = longitudObjetivo (objetivo (juego s))
        casillas = concatMap (const "+---") [1..cantCasillas]
    in "| " ++ letras ++ "\n" ++ casillas ++ "+"

renderLetra :: (Char, Match) -> String
renderLetra (c, Correcto) = ansiBgGreenColor ++ [c] ++ ansiResetColor ++ " | "   -- Letra correcta y en la posición correcta
renderLetra (c, LugarIncorrecto) = ansiBgYellowColor ++ [c] ++ ansiResetColor ++ " | "   -- Letra correcta en posición incorrecta
renderLetra (c, NoPertenece) = ansiBgRedColor ++ [c] ++ ansiResetColor ++ " | "  -- Letra incorrecta


main :: IO ()
main = do
    args <- getArgs
    diccionario <- getDiccionario "diccionario.txt"
    idx <- uniformRM (0, length diccionario - 1) globalStdGen
    fechaActual <- getCurrentDay
    estadoArchivo <- leerArchivo "estado.json"

    let intentosTotales = 6
    --estado <- cargarEstado (leerArchivo "estado.json") args fechaActual diccionario idx intentosTotales

    let validarPalabra palabra = 
          case args of
            ("--palabra":_) -> True
            _ -> map toUpper palabra `elem` diccionario

    let estado = parseArgs args estadoArchivo diccionario idx fechaActual intentosTotales validarPalabra
    s <- runInteractive' (jugar estado)
    --runInteractive (jugar palabra intentosTotales validarPalabra)

    let estadoFinal = EstadoJuego {fecha = fechaActual, objetivoAr = objetivo (juego s), intentosAr = obtenerIntentos (juego s), letrasDescartadasAr = letrasDescartadas s}
    guardarEstado args "estado.json" estadoFinal

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

parseArgs :: [String] -> Maybe EstadoJuego -> [String] -> Int -> Day -> Int -> (String -> Bool) -> State
parseArgs args estadoArchivo diccionario idx fechaActual intentosTotales validarPalabra =
    case args of
      ["--daily"] -> cargarEstado estadoArchivo diccionario idx fechaActual intentosTotales validarPalabra
      ("--daily":fechaStr:_) -> 
          case parseDay fechaStr of
              Just fechaIngresada -> 
                if fechaIngresada == fechaActual
                  then cargarEstado estadoArchivo diccionario idx fechaActual intentosTotales validarPalabra
                  else error "La fecha ingresada no coincide con la fecha actual"
              Nothing -> error "Fecha inválida: usar formato YYYY-MM-DD."
      ["--random"] -> inicializarRandom diccionario idx intentosTotales validarPalabra
      ("--palabra":palabra:_) -> inicializarFija (map toUpper palabra) intentosTotales validarPalabra
      [] -> cargarEstado estadoArchivo diccionario idx fechaActual intentosTotales validarPalabra
      _ -> error "Argumento invalido. Argumentos validos: --daily, --random, --palabra"

inicializarRandom :: [String] -> Int -> Int -> (String -> Bool) -> State
inicializarRandom diccionario idx intentosTotales validarPalabra = 
  State
    { 
      juego = nuevo (obtenerPalabraRandom diccionario idx) intentosTotales validarPalabra,
      intentoActual = "",
      posicionActual = 0,
      letrasDescartadas = [],
      mensajeError = "",
      finalizar = False
    }

cargarEstado :: Maybe EstadoJuego -> [String] -> Int -> Day -> Int -> (String -> Bool) -> State
cargarEstado (Just archivo) diccionario idx fechaActual intentosTotales validarPalabra =
  if fechaActual == fecha archivo
    then
      recuperarJuego 
        (
          State
            { 
              juego = nuevo (objetivoAr archivo) intentosTotales validarPalabra,
              intentoActual = "",
              posicionActual = 0,
              letrasDescartadas = [],
              mensajeError = "",
              finalizar = False
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
          finalizar = False
        }
cargarEstado Nothing diccionario idx _ intentosTotales validarPalabra =
  State
    { juego = nuevo (obtenerPalabraRandom diccionario idx) intentosTotales validarPalabra,
      intentoActual = "",
      posicionActual = 0,
      letrasDescartadas = [],
      mensajeError = "",
      finalizar = False
    }

recuperarJuego :: State -> [String] -> State
recuperarJuego s [] = s
recuperarJuego s (i : intentos') = recuperarJuego (procesarIntento s i) intentos'

inicializarFija :: String -> Int -> (String -> Bool) -> State
inicializarFija target intentosTotales validarPalabra = 
  State
    { 
      juego = nuevo target intentosTotales validarPalabra,
      intentoActual = "",
      posicionActual = 0,
      letrasDescartadas = [],
      mensajeError = "",
      finalizar = False
    }

obtenerPalabraRandom :: [String] -> Int -> String
obtenerPalabraRandom diccionario idx = diccionario !! idx