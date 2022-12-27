{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Data.Aeson

app :: Application
app request respond =
  case requestMethod request of
    "GET" -> do
      let responseBody = encode (object ["message" .= ("Hello, World!" :: String)])
      respond $ responseLBS status200 [(hContentType, "application/json")] responseBody
    _ -> respond $ responseLBS status405 [(hContentType, "text/plain")] "Method Not Allowed"

main :: IO ()
main = do
  putStrLn "Listening on port 8080..."
  run 8080 app
