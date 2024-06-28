module Main where

import System.Environment

import IcfpLang


main :: IO ()
main = do args <- getArgs
          case args of
            ("-e" : rest) -> parseAndEval (unlines rest)
            ["-f", file] -> do program <- readFile file
                               parseAndEval program
            _ -> usage

parseAndEval :: String -> IO ()
parseAndEval s = let expr = parseProgram s in
                 do putStr "EXPR: "
                    print expr 
                    putStr "\n\nVALUE: "
                    print (evaluate expr)
                    putChar '\n'

usage :: IO ()
usage = do putStrLn "usage: icfpc -e token...       evaluate program given on command line"
           putStrLn "       icfpc -f file           read and evaluate program from file"
