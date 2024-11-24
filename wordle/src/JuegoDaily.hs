{-# LANGUAGE DeriveGeneric #-}

module JuegoDaily where

import Data.Aeson (decode, encode, eitherDecode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Data.Time (Day)

data EstadoJuego = EstadoJuego
  { historial :: [(Day, String)],  
    fechaActual :: Day,             
    intentosDiaActual :: [String]      
  } deriving (Show, Generic)

instance FromJSON EstadoJuego
instance ToJSON EstadoJuego

leerEstado :: FilePath -> IO (Maybe EstadoJuego)
leerEstado archivo = do
  contenido <- B.readFile archivo
  return $ decode contenido

guardarEstado :: FilePath -> EstadoJuego -> IO ()
guardarEstado archivo estado = B.writeFile archivo (encode estado)
