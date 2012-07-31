-- Usage: sendCommand "http://localhost:5000" "user" "password" (Cmd (Just 5) (Set True))
-- to set relais 5 to true
-- Cmd Nothing Get to get the state of all relais
-- Will return a Right with a list of Bools inside on success or a Left errormessage on failure

module Relais where


import Network.Curl
import Codec.Binary.Base64.String (encode)


data Action = Set Bool | Get deriving (Show,Eq)
data Cmd = Cmd { relais :: Maybe Int
               , action :: Action
               }
               deriving (Show, Eq)


sendCommand :: String -> String -> String -> Cmd -> IO (Either String [Bool])
sendCommand url user pw (Cmd r a) = do
    curl <- initialize
    let send = do_curl_ curl $ url ++ "/relais" ++ relais ++ "?format=raw"
        resp = send settings :: IO (CurlResponse_ [(String, String)] String)
    fmap (decode . respBody) resp
  where
    relais = maybe "" (('/':) . show) r
    settings = [ CurlCustomRequest method
               , CurlHttpAuth [HttpAuthBasic]
               , CurlHttpHeaders ["Authorization: Basic " ++ encode (user ++ ':':pw)]
               ]
    method = case a of
              Get       -> "GET"
              Set True  -> "POST"
              Set False -> "DELETE"


decode xs = if all (\x -> x == '1' || x == '0') xs
              then Right $ map decode' xs
              else Left $ "decode: Invalid repsonse: " ++ xs
  where
    decode' '0' = False
    decode' '1' = True
