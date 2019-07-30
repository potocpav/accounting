module Server where

import Prelude hiding (apply)

import Effect (Effect)
import Effect.Console (log)
import Node.Express.App (App, get, apply)
import Node.Express.Handler (Handler)
import Node.Express.Response (send)
import Node.Express.Types (Application)


wowHandler :: Handler
wowHandler = send "WOW!"


app :: App
app = do
    get "/api/test" wowHandler


applyMain :: Application -> Effect Unit
applyMain application = apply app application
