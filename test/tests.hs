{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-} 
{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.IO
import System.Exit (exitFailure)
import Text.ParserCombinators.Parsec
import Text.Parsec
import Database.MySQL.Simple

none :: Stream s m Char => ParsecT s u m Char
none = noneOf ":"

key = many1 $ noneOf ":"

kv = do
    k <- key
    char ':'
    v <- many $ noneOf "\t"
    return (k, v)

ltsv = do
    k <- kv
    ks <- many $ do {(char '\t'); kk <- kv; return kk}
    return (k : ks)

f :: IO [String]
f = do 
    handle <- openFile "access.log" ReadMode
    contents <- hGetContents handle
    return $ lines contents
   

data NginxLog = NginxLog { dateGmt :: String
                         , dateLocal :: String
                         , documentRoot :: String 
                         , documentUri :: String 
                         , fastcgiScriptName :: String 
                         , host :: String 
                         , hostname :: String 
                         , msec :: String 
                         , nginxVersion :: String 
                         , pid :: String 
                         , pipe :: String 
                         , proxyAddXForwardedFor :: String 
                         , realipRemoteAddr :: String 
                         , realpathRoot :: String 
                         , remoteAddr :: String 
                         , remotePort :: String 
                         , request :: String 
                         , requestBody :: String 
                         , requestCompletion :: String 
                         , requestFilename :: String 
                         , requestLength :: String 
                         , requestMethod :: String 
                         , requestTime :: String 
                         , requestUri :: String 
                         , scheme:: String 
                         , serverAddr:: String 
                         , serverName:: String 
                         , serverPort:: String 
                         , serverProtocol:: String 
                         , status:: String 
                         , tcpinfoRtt:: String 
                         , tcpinfoRttVar:: String 
                         , tcpinfoSndCwnd:: String 
                         , tcpinfoRcvSpace:: String 
                         , timeIso8601:: String 
                         , timeLocal:: String 
                         , uri:: String 
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
    i <-  hello
    print i
    --print $ (\e -> case parse ltsv "" e of Right r -> cre r ) <$> a  

--    print $ parse ltsv "" "foo_bar:hoge\thoge:hoge"
    putStrLn "This test always fails!"
    exitFailure

--simple :: Stream s m Char => ParsecT s u m Char
--simple = letter

--run :: Show a => Parser a -> String -> IO ()
{-
run p input= case (parse p "" input) of
    Left err -> 
        do{ putStr "parse error at "
          ; print err}
    Right x  -> print x
-}  
