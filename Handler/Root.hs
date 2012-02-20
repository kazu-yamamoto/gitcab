{-# LANGUAGE OverloadedStrings #-}

module Handler.Root where

import Import

getRootR :: Handler RepHtml
getRootR = redirect $ FileR []
