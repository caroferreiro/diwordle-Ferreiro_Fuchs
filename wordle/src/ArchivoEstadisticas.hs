{-# LANGUAGE DeriveGeneric #-}

module ArchivoEstadisticas where

import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import System.IO.Error (catchIOError)

data Estadisticas = Estadisticas
  { 
    partidasGanadas :: Int,
    partidasPerdidas :: Int,
    intentosHastaGanar :: [Int],
    racha :: Int
  } 
  deriving (Generic)

instance FromJSON Estadisticas
instance ToJSON Estadisticas

leerEstadisticas :: FilePath -> IO (Maybe Estadisticas)
leerEstadisticas archivo = do
  contenido <- catchIOError (B.readFile archivo) (\_ -> pure B.empty)
  pure (decode contenido)

guardarEstadisticas :: [String] -> FilePath -> Estadisticas -> IO ()
guardarEstadisticas args archivo estadisticas =
  case args of
      [] -> B.writeFile archivo (encode estadisticas)
      ["--daily"] -> B.writeFile archivo (encode estadisticas)
      ("--daily":_) -> B.writeFile archivo (encode estadisticas)
      ["--random"] -> return ()   -- no guardar archivo si el argumento es "--random"
      ("--palabra":_) -> return ()    -- no guardar archivo si el argumento es "--random"
      _ -> B.writeFile archivo (encode estadisticas)

inicializarEstadisticas :: Maybe Estadisticas -> Estadisticas
inicializarEstadisticas (Just est) = est
inicializarEstadisticas Nothing = 
  Estadisticas
    { 
      partidasGanadas = 0,
      partidasPerdidas = 0,
      intentosHastaGanar = [],
      racha = 0
    }

promedioIntentos :: [Int] -> Float
promedioIntentos intentos
  | null intentos = 0
  | otherwise = fromIntegral (sum intentos) / fromIntegral (length intentos)