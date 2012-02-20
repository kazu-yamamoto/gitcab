{-# LANGUAGE PackageImports #-}
import "gitcab" Application (getApplicationDev)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort, settingsHost, HostPreference(..))
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import Prelude

main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    forkIO $ runSettings defaultSettings
        { settingsPort = port
        , settingsHost = Host "127.0.0.1"
        } app
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
