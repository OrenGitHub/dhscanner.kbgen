{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}

import Yesod
import Prelude
import Data.Aeson()

-- project imports
import Asts
import KbGen ( kbGen )

-- general imports
import GHC.Generics

data Healthy = Healthy Bool deriving ( Generic )

instance ToJSON Healthy where toJSON (Healthy status) = object [ "healthy" .= status ]

data App = App

mkYesod "App" [parseRoutes|
/kbgen HomeR POST
/healthcheck HealthcheckR GET
|]

instance Yesod App

getHealthcheckR :: Handler Value
getHealthcheckR = returnJson $ Healthy True

postHomeR :: Handler Value
postHomeR = do
    callables <- requireCheckJsonBody :: Handler Callable
    returnJson $ kbGen callable

main :: IO ()
main = warp 3000 App
