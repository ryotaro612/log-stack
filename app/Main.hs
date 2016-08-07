module Main where

import Lib
import System.IO
import Text.ParserCombinators.Parsec
import Text.Parsec
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Functor.Identity (Identity)

--b :: ParseTime t => Maybe t
b :: Maybe LocalTime
b = do
 c <-  parseTimeM True defaultTimeLocale "%H" "23" 
 return c


logs :: String -> IO [String]
logs filepath = do 
    handle <- openFile filepath ReadMode
    contents <- hGetContents handle
    return $ lines contents

key :: ParsecT String u Identity [Char]
key = many1 $ noneOf ":"

kv :: ParsecT String u Identity ([Char], [Char])
kv = do
    k <- key
    char ':'
    v <- many $ noneOf "\t"
    return (k, v)

ltsv :: ParsecT String u Identity [([Char], [Char])]
ltsv = do
    k <- kv
    ks <- many $ do {(char '\t'); kk <- kv; return kk}
    return (k : ks)

data NginxLog = NginxLog { dateGmt :: String
                         , dateLocal :: String
                         , documentRoot :: String 
                         , documentUri :: String 
                         , geoipAreaCode :: String
                         , geoipCity :: String
                         , geoipCityContinentCode :: String
                         , geoipCityCountryCode :: String
                         , geoipCityCountryCode3 :: String
                         , geoipCityCountryName :: String
                         , geoipCountryCode :: String
                         , geoipCountryCode3 :: String
                         , geoipCountryName :: String
                         , geoipDmaCode :: String
                         , geoipLatitude :: String
                         , geoipLongitude :: String
                         , geoipOrg :: String
                         , geoipPostalCode :: String
                         , geoipRegion :: String
                         , geoipRegionName :: String
                         , realipRemoteAddr :: String
                         , remoteAddr :: String 
                         , remotePort :: String 
                         , remoteUser :: String 
                         , request :: String 
                         , requestBody :: String 
                         , requestBodyFile :: String 
                         , requestCompletion :: String 
                         , requestFilename :: String 
                         , requestMethod :: String 
                         , requestUri :: String 
                         , scheme:: String 
                         , status:: String 
                         , timeIso8601:: String 
                         , timeLocal:: String 
                         , uri:: String 
                         , httpReferer:: String
                         , httpUserAgent :: String
                         } deriving Show

fnd :: String -> [(String, String)] -> String
fnd name lst = case filter (\(k, v) -> (k == name)) lst of
    [(k, v)] -> v
    [] -> ""

cre:: [(String, String)] -> NginxLog
cre lst = NginxLog (fnd "date_gmt" lst)
                   (fnd "date_local" lst)
                   (fnd "document_root" lst)
                   (fnd "document_uri" lst)
                   (fnd "geoip_area_code" lst)
                   (fnd "geoip_city" lst)
                   (fnd "geoip_city_continent_code" lst)
                   (fnd "geoip_city_country_code" lst)
                   (fnd "geoip_city_country_code3" lst)
                   (fnd "geoip_city_country_name" lst)
                   (fnd "geoip_country_code" lst)
                   (fnd "geoip_country_code3" lst)
                   (fnd "geoip_country_name" lst)
                   (fnd "geoip_dma_code" lst)
                   (fnd "geoip_latitude" lst)
                   (fnd "geoip_longitude" lst)
                   (fnd "geoip_org" lst)
                   (fnd "geoip_postal_code" lst)
                   (fnd "geoip_region" lst)
                   (fnd "geoip_region_name" lst)
                   (fnd "realip_remote_addr" lst)
                   (fnd "remote_addr" lst)
                   (fnd "remote_port" lst)
                   (fnd "remote_user" lst)
                   (fnd "request" lst)
                   (fnd "request_body" lst)
                   (fnd "request_body_file" lst)
                   (fnd "request_completion" lst)
                   (fnd "request_filename" lst)
                   (fnd "request_method" lst)
                   (fnd "request_uri" lst)
                   (fnd "scheme" lst)
                   (fnd "status" lst)
                   (fnd "time_iso8601" lst)
                   (fnd "time_local" lst)
                   (fnd "uri" lst)
                   (fnd "http_referer" lst)
                   (fnd "http_user_agent" lst)

main :: IO ()
main = do
    logs <- logs "access.log"
    print $ (\e -> case parse ltsv "" e of Right r -> cre r ) <$> logs
    putStrLn ""
    -- print $ (\e -> case parse ltsv "" e of Right r -> cre r ) <$> a  
    -- putStrLn "This test always fails!"

{-

module Main where

import System.IO
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec
import Text.Parsec
import Database.MySQL.Simple




fnd :: String -> [(String, String)] -> String
fnd name lst = case filter (\(k, v) -> (k == name)) lst of
    [(k, v)] -> v
    [] -> ""

cre:: [(String, String)] -> NginxLog
cre lst = NginxLog (fnd "date_gmt" lst)
                   (fnd "date_local" lst)
                   (fnd "document_root" lst)
                   (fnd "document_uri" lst)
                   (fnd "fastcgi_script_name" lst)
                   (fnd "host" lst)
                   (fnd "hostname" lst)
                   (fnd "msec" lst)
                   (fnd "nginx_version" lst)
                   (fnd "pid" lst)
                   (fnd "pipe" lst)
                   (fnd "proxy_add_x_forwared_for" lst)
                   (fnd "realip_remote_addr" lst)
                   (fnd "realpath_root" lst)
                   (fnd "remote_addr" lst)
                   (fnd "remote_port" lst)
                   (fnd "request" lst)
                   (fnd "request_body" lst)
                   (fnd "request_completion" lst)
                   (fnd "request_filename" lst)
                   (fnd "request_length" lst)
                   (fnd "request_method" lst)
                   (fnd "request_time" lst)
                   (fnd "request_uri" lst)
                   (fnd "scheme" lst)
                   (fnd "server_addr" lst)
                   (fnd "server_name" lst)
                   (fnd "server_port" lst)
                   (fnd "server_protocolj" lst)
                   (fnd "status" lst)
                   (fnd "tcpinfo_rtt" lst)
                   (fnd "tcpinfo_rtt_var" lst)
                   (fnd "tcpinfo_snd_cwnd" lst)
                   (fnd "tcpinfo_rcv_space" lst)
                   (fnd "time_iso8601" lst)
                   (fnd "time_local" lst)
                   (fnd "uri" lst)



hoge :: [(String, String)] -> [String]
hoge a = do
  aa <- a
  return (fst aa)

hello :: IO Int
hello = do
  conn <- connect defaultConnectInfo
  [Only i] <- query_ conn "select 2 + 2"
  return i

main:: IO()
main = do
    a <- f
    conn <- connect defaultConnectInfo
    print $ (\e -> case parse ltsv "" e of Right r -> cre r ) <$> a  
    putStrLn "This test always fails!"
    exitFailure
 -}
