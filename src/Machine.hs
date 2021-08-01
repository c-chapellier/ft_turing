{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Machine where

import qualified Debug.Trace as Trace
import qualified Data.Aeson as Aeson ( FromJSON, ToJSON )
import qualified Data.Map as Map
import qualified GHC.Generics as Generics

import qualified Color
import qualified Transition ( Transition(read, to_state, write, action) )

data Machine = Machine
    { name          :: String
    , alphabet      :: [String]
    , blank         :: String
    , states        :: [String]
    , initial       :: String
    , finals        :: [String]
    , transitions   :: Map.Map String [Transition.Transition]
    } deriving (Generics.Generic)

showTransition :: String -> Transition.Transition -> String
showTransition k t = "(" ++ Color.cyan k ++ ", " ++ show t ++ ")\n"

showTransitions :: (String, [Transition.Transition]) -> String
showTransitions (k, t) = concatMap (showTransition k) t

instance Show Machine where
  show a =
    "********************************************************************************"
    ++ Color.yellow "\nName : "     ++  name a
    ++ Color.yellow "\nAlphabet: "  ++  show (alphabet a)
    ++ Color.yellow "\nBlank: "     ++  blank a
    ++ Color.yellow "\nStates : "   ++  show (states a)
    ++ Color.yellow "\nInitial : "  ++  initial a
    ++ Color.yellow "\nFinals : "   ++  show (finals a)
    ++ "\n"                         ++  concatMap showTransitions (Map.toAscList (transitions a))
    ++ "********************************************************************************"

instance Aeson.FromJSON Machine
instance Aeson.ToJSON Machine

check1 :: Machine -> Transition.Transition -> Bool
check1 m t = if Transition.read t `elem` Machine.alphabet m
            then Transition.read t `elem` Machine.alphabet m
            else Trace.trace (Transition.read t ++ " not in " ++ show (Machine.alphabet m)) (Transition.read t `elem` Machine.alphabet m)

check2 :: Machine -> Transition.Transition -> Bool
check2 m t  = if Transition.to_state t `elem` Machine.states m
            then Transition.to_state t `elem` Machine.states m
            else Trace.trace (Transition.to_state t ++ " not in states") (Transition.to_state t `elem` Machine.states m)

check3 :: Machine -> Transition.Transition -> Bool
check3 m t = if Transition.write t `elem` Machine.alphabet m
            then Transition.write t `elem` Machine.alphabet m
            else Trace.trace (Transition.write t ++ " not in " ++ show (Machine.alphabet m)) (Transition.write t `elem` Machine.alphabet m)

check4 :: Machine -> Transition.Transition -> Bool
check4 m t = if Transition.action t `elem` ["LEFT", "RIGHT"]
            then Transition.action t `elem` ["LEFT", "RIGHT"]
            else Trace.trace (Transition.action t ++ " not in " ++ show ["LEFT", "RIGHT"]) (Transition.action t `elem` ["LEFT", "RIGHT"])

checkTransition :: Machine -> Transition.Transition -> Bool
checkTransition m t = and [check1 m t, check2 m t, check3 m t, check4 m t]

checkTransitions :: Machine -> (String, [Transition.Transition]) -> Bool
checkTransitions m (key, t) = all (checkTransition m) t

check :: Machine -> String -> Either String Machine
check m input =
    if all ((== 1) . length) (Machine.alphabet m)
    then if all (`elem` concat (Machine.alphabet m)) input
        then if head (Machine.blank m) `notElem` input
            then if Machine.blank m `elem` Machine.alphabet m
                then if all (`elem` Machine.states m) (Machine.finals m)
                    then if all (checkTransitions m) (Map.toAscList (Machine.transitions m))
                        then Right m
                        else Left "Parsing error, transitions not good"
                    else Left "Parsing error, finals must be states"
                else Left "Parsing error, blank char not in alphabet"
            else Left "Parsing error, the blank char must not be in the input"
        else Left "Parsing error, not all input chars are in alphabet"
    else Left "Parsing error, alphabets not size 1"
    