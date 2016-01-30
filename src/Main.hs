module Main where

import Servant (Proxy(..), serve, Server)
import Servant.API (Raw)
import Servant.Utils.StaticFiles (serveDirectory)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Options.Applicative

type Api = Raw

mainServer :: Options -> IO ()
mainServer (Options dir port) = do
  putStrLn $ "Serving '" ++ dir ++ "' on port " ++ show port
  Warp.run port $ serve (Proxy :: Proxy Api) (serveDirectory dir)

data Options = Options
  { _dir  :: String
  , _port :: Int
  } deriving (Eq, Show)

main :: IO ()
main = execParser opts >>= mainServer
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Serve the contents of DIRECTORY on HTTP port PORT"
     <> header "serve-here - A minimal server for static content" )

options :: Parser Options
options = Options
  <$> strOption
      ( long "directory"
     <> short 'd'
     <> metavar "DIRECTORY"
     <> value "."
     <> showDefault
     <> help "Serve contents of DIRECTORY" )
  <*> option auto
      ( long "port"
     <> short 'p'
     <> metavar "PORT"
     <> value 8001
     <> showDefault
     <> help "Listen on HTTP port PORT" )
