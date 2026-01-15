module Main (main) where

import System.IO (isEOF)

import HttpPathChecker (Path, str2path)

newtype IsEof = IsEof Bool

processConverted :: Either String Path -> IO ()
processConverted (Left err) = putStrLn err
processConverted (Right pat) = do
    print pat
    isEof :: Bool <- isEOF
    processStdin (IsEof isEof)

processStdin :: IsEof -> IO ()
processStdin (IsEof True) = return ()
processStdin (IsEof False) = do
    line :: String <- getLine
    let converted :: Either String Path = str2path line
    processConverted converted

main :: IO ()
main = do
    isEof :: Bool <- isEOF
    processStdin (IsEof isEof)
