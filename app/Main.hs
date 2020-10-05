module Main where

import Control.Monad (liftM)
import Data.Int (Int64)
import Data.List (intercalate)
import System.Clock
import System.Environment
import System.Exit(ExitCode(..))
import System.IO
import System.Process.Typed

type Command = String
type Arguments = [String]


main :: IO ()
main = do
    (command, args) <- readArguments
    putErrStrLn $ command ++ " " ++ unwords args

    startTime <- getTime Monotonic
    exitCode <- runProcess $ proc command args
    diffTime <- getTime Monotonic >>= (return . diffTimeSpec startTime)

    putErrStrLn $ "\n\r" ++
               "Exit code....: " ++ show (exitCodeToInt exitCode) ++ "\n\r" ++
               "Total time...: " ++ timeSpecToHumanString diffTime

readArguments :: IO (Command, Arguments)
readArguments = do
    args <- getArgs
    if null args || null ( head args)
    then fail "Usage: <command> [<arguments>]"
    else return (head args, tail args)

putErrStrLn :: String -> IO ()
putErrStrLn = hPutStrLn stderr

exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n

timeSpecToHumanString :: TimeSpec -> String
timeSpecToHumanString = convert . asMilliseconds
    where convert timeVal
            | timeVal > hour = show (timeVal `div` hour) ++ " hours " ++ convert (timeVal `mod` hour)
            | timeVal > minute = show (timeVal `div` minute) ++ " minutes " ++ convert (timeVal `mod` minute)
            | timeVal > second = show (timeVal `div` second) ++ " seconds " ++ convert (timeVal `mod` second)
            | otherwise = show timeVal ++ " milliseconds"
            where second = 1000
                  minute = 60 * 1000
                  hour = 60 * 60 * 1000

asMilliseconds :: TimeSpec -> Int64
asMilliseconds (TimeSpec sec nsec) = round $ (sec' + (nsec' * nanosecond)) * 1000
    where sec' = fromIntegral sec :: Double
          nsec' = fromIntegral nsec :: Double
          nanosecond = 10 ^^ negate 9 :: Double
