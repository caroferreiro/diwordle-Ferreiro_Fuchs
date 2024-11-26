{-# LANGUAGE DeriveGeneric #-}

module ArchivoEstado where

import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import System.IO.Error (catchIOError)
import Data.Time (Day)

data EstadoJuego = EstadoJuego
  { 
    fecha :: Day,
    objetivoAr :: String,
    intentosAr :: [String],
    letrasDescartadasAr :: [Char]
  } 
  deriving (Generic)

instance FromJSON EstadoJuego
instance ToJSON EstadoJuego

leerArchivo :: FilePath -> IO (Maybe EstadoJuego)
leerArchivo archivo = do
  contenido <- catchIOError (B.readFile archivo) (\_ -> pure B.empty)
  pure (decode contenido)

guardarEstado :: [String] -> FilePath -> EstadoJuego -> IO ()
guardarEstado args archivo estado =
  case args of
      [] -> B.writeFile archivo (encode estado)
      ["--daily"] -> B.writeFile archivo (encode estado)
      ("--daily":_) -> B.writeFile archivo (encode estado)
      ["--random"] -> return ()   -- no guardar archivo si el argumento es "--random"
      ("--palabra":_) -> return ()    -- no guardar archivo si el argumento es "--random"
      _ -> B.writeFile archivo (encode estado)