module Turing where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Aeson as Aeson ( eitherDecode )
import qualified Data.ByteString.Lazy as ByteStringLazy

import qualified Color
import qualified Machine ( Machine(transitions, finals, initial, blank), check )
import qualified Transition ( Transition(..) )

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

showTape :: String -> Int -> String
showTape tape head = Color.yellow (take head tape) ++ Color.red [tape !! head] ++ Color.yellow (drop (head + 1) tape)

showStep :: String -> Int -> String -> Transition.Transition -> String
showStep tape head state t = "[" ++ showTape tape head ++ Color.yellow "..." ++ "] (" ++ Color.cyan state ++ ", " ++ show t ++ ")"

isCurrentChar :: String -> Transition.Transition -> Bool
isCurrentChar cell t = cell == Transition.read t

nextStep :: Machine.Machine -> String -> String -> Int -> IO ()
nextStep m state tape head = do
    let checkedTape = if head == length tape then tape ++ Machine.blank m else tape
    let t = Maybe.fromMaybe (Transition.Transition "" "" "" "") (List.find (isCurrentChar [checkedTape !! head]) (Machine.transitions m Map.! state))
    putStrLn (showStep checkedTape head state t)
    let newState = Transition.to_state t
    let newTape = take head checkedTape ++ Transition.write t ++ drop (head + 1) checkedTape
    let newHead = if Transition.action t == "RIGHT" then head + 1 else head - 1
    if newState `elem` Machine.finals m
        then putStrLn ("[" ++ Color.yellow (newTape ++ "...") ++ "]")
        else nextStep m newState newTape newHead

run :: Either String Machine.Machine -> String -> IO ()
run (Left err) tape = putStrLn err
run (Right m) tape = do
        print m
        nextStep m (Machine.initial m) tape 0

checkConfig :: Either String Machine.Machine -> String -> IO ()
checkConfig (Left err) tape = putStrLn err
checkConfig (Right m) tape = run (Machine.check m tape) tape

turing :: [String] -> IO ()
turing [configFile, tape] = do
            m <- Aeson.eitherDecode <$> getJSON configFile
            checkConfig m tape
turing _ = putStrLn getUsage
