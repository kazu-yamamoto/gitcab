{-# LANGUAGE OverloadedStrings #-}

module Handler.File where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Git as G
import qualified Data.Text as T
import Import
import System.Git as G

type Path = Text
type PathInfo = [Text]

----------------------------------------------------------------

getFileR :: PathInfo -> Handler RepHtml
getFileR pinfo = do
    mobj <-liftIO $ G.gitPathToGitObject ('/' : T.unpack path)
    case mobj of
        Left _              -> layout $ noSuchFile path
        Right (GoTree _ xs) -> layout $ showDir    path pinfo xs
        Right (GoBlob _ bs) -> layout $ showFile   path bs
        Right _             -> layout $ noSuchFile path
  where
    path = pathInfoToPath pinfo
    layout wham = defaultLayout $ defStyle >> wham

pathInfoToPath :: PathInfo -> Path
pathInfoToPath pinfo = T.intercalate "/" pinfo

defStyle :: Widget
defStyle = toWidget [cassius|
h1
    font-size: medium
    padding-left: 10px
ul
    padding-left: 20px
    padding-bottom: 20px
    list-style-type: none
pre
    padding-left: 20px
    padding-bottom: 20px
footer
    padding-left: 10px
}
|]

----------------------------------------------------------------

showDir :: Path -> PathInfo -> [GitTreeEntry] -> Widget
showDir path pinfo xs = [whamlet|
<h1>#{path}
<ul>
    $forall fn <- xs
      <li><a href=@{FileR $ prepend fn}>#{fileNameDir fn}
|]
  where
    prepend fn = pinfo ++ [T.pack (G.fileName fn)]
    fileNameDir obj
      | fileType obj == Directory = T.pack (G.fileName obj) `T.append` "/"
      | otherwise                 = T.pack (G.fileName obj)


showFile :: Path -> ByteString -> Widget
showFile path bs = [whamlet|
<h1>#{path}
<pre>#{BS.unpack bs}
|]

noSuchFile :: Path -> Widget
noSuchFile path = [whamlet|
 #{path}: no such file or directory
|]
