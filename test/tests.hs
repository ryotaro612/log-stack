module Main where

import System.IO
import System.Exit (exitFailure)
-- import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec

--sample :: Int
-- key = many1 $ letter <|> (char '_') <|> 
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

fnd :: String -> [String] -> String
fnd name lst = case filter (\e -> (e == name)) lst of
    [a] -> a
    [] -> ""

cre:: [String] -> NginxLog
cre lst = NginxLog (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (lst !! 1)
                   (fnd "uri" lst)



hoge :: [(String, String)] -> [String]
hoge a = do
  aa <- a
  return (fst aa)

{-p e = case (parse ltsv "" e)  of
    Right a -> a
-}

main:: IO()
main = do
    a <- f
    print $ (\e -> case parse ltsv "" e of 
        Right r -> cre  ((\ee -> fst ee)<$> r )) <$> a  

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
