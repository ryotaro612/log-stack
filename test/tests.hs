module Main where

import System.IO
import System.Exit (exitFailure)
-- import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.Parsec

--sample :: Int
key = many1 $ letter <|> (char '_')

kv = do
    char '$'
    k <- key
    char ':'
    v <- many1 $ noneOf ":\t"
    return (k, v)

ltsv = do
    k <- kv
    ks <- many $ do {(char '\t'); kk <- kv; return kk}
    return (k : ks)


   
{-
sample = do 
    c <- char '$' 
    l <- letter <$
    return 1 
-}
main:: IO()
main = do
    print $ parse ltsv "" "$foo_bar:hoge\t$hoge:hoge"
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
