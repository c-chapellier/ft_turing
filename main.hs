{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import qualified Data.Map as Map
import Data.Aeson as Aeson ( eitherDecode )
import qualified Data.ByteString.Lazy as ByteStringLazy
import System.Environment ( getArgs )

import Machine
import Transition

getUsage :: String
getUsage = "usage: ft_turing [-h] jsonfile input\n"
    ++ "\n"
    ++ "positional arguments:\n"
    ++ "    jsonfile        json description of the machine\n"
    ++ "\n"
    ++ "    input           input of the machine\n"
    ++ "\n"
    ++ "optional arguments:\n"
    ++ "    -h, --help      show this help message and exit\n"

getJSON :: String -> IO ByteStringLazy.ByteString
getJSON = ByteStringLazy.readFile

checkTransition :: Machine -> Transition -> Bool
checkTransition m t = Transition.read t `elem` Machine.alphabet m
    && Transition.to_state t `elem` Machine.states m
    && Transition.write t `elem` Machine.alphabet m
    && Transition.action t `elem` ["LEFT", "RIGHT"]

checkTransitions :: Machine -> (String, [Transition]) -> Bool
checkTransitions m (key, t) = all (checkTransition m) t

checkConfig :: Machine -> String -> Either String Machine
checkConfig m input = if all ((== 1) . length) (Machine.alphabet m)
    && notElem (head (Machine.blank m)) input
    && elem (Machine.blank m) (Machine.alphabet m)
    && all (`elem` Machine.states m) (Machine.finals m)
    && all (checkTransitions m) (Map.toAscList (Machine.transitions m))
                    then Right m
                    else Left "Parsing error, check you config file young man"

-- add head
showStep :: String -> String -> Transition -> String
showStep tape k t = "[" ++ tape ++ "........] (" ++ k ++ ", " ++ show t ++ ")\n"

run :: Machine -> String -> String
run m input = showStep input 

main :: IO ()
main = do
    args <- getArgs
    case args of
        [jsonfile, input] -> do
            rc <- (Aeson.eitherDecode <$> getJSON jsonfile) :: IO (Either String Machine)
            case rc of
                Left err -> putStrLn err
                Right m 1-> do
                    print m1
                    let d = checkConfig m1 input :: Either String Machine
                    case d of
                        Left err -> putStrLn err
                        Right m2 -> do
                            print "Parsing ok"
                            run m2 input
        _ -> putStrLn getUsage



