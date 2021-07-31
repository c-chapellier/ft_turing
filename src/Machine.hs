{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Machine where

import Data.Aeson as Aeson ( FromJSON, ToJSON )
import qualified Data.Map as Map
import qualified GHC.Generics as Generics

import Transition ( Transition(read, to_state, write, action) )

data Machine = Machine
    { name          :: String
    , alphabet      :: [String]
    , blank         :: String
    , states        :: [String]
    , initial       :: String
    , finals        :: [String]
    , transitions   :: Map.Map String [Transition]
    } deriving (Generics.Generic)

showTransition :: String -> Transition -> String
showTransition k t = "(" ++ k ++ ", " ++ show t ++ ")\n"

showTransitions :: (String, [Transition]) -> String
showTransitions (k, t) = concatMap (showTransition k) t

instance Show Machine where
  show a =
    "********************************************************************************"
    ++ "\nName : "     ++  name a
    ++ "\nAlphabet: "  ++  show (alphabet a)
    ++ "\nBlank: "     ++  blank a
    ++ "\nStates : "   ++  show (states a)
    ++ "\nInitial : "  ++  initial a
    ++ "\nFinals : "   ++  show (finals a)
    ++ "\n"            ++  concatMap showTransitions (Map.toAscList (transitions a))
    ++ "********************************************************************************\n"

instance FromJSON Machine
instance ToJSON Machine

checkTransition :: Machine -> Transition -> Bool
checkTransition m t = Transition.read t `elem` Machine.alphabet m
    && Transition.to_state t `elem` Machine.states m
    && Transition.write t `elem` Machine.alphabet m
    && Transition.action t `elem` ["LEFT", "RIGHT"]

checkTransitions :: Machine -> (String, [Transition]) -> Bool
checkTransitions m (key, t) = all (checkTransition m) t

check :: Machine -> String -> Either String Machine
check m input = if all ((== 1) . length) (Machine.alphabet m)
    && all (`elem` concat (Machine.alphabet m)) input
    && notElem (head (Machine.blank m)) input
    && elem (Machine.blank m) (Machine.alphabet m)
    && all (`elem` Machine.states m) (Machine.finals m)
    && all (checkTransitions m) (Map.toAscList (Machine.transitions m))
                    then Right m
                    else Left "Parsing error, check you config file young man"