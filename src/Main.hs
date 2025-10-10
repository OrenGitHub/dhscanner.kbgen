{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

import Yesod
import Prelude
import GHC.Generics

-- project imports
import Factify
import Callable

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
    callable <- requireCheckJsonBody :: Handler Callable
    returnJson $ factify callable

main :: IO ()
main = warp 3000 App
