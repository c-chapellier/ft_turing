{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Machine where

import Data.Aeson as Aeson ( FromJSON, ToJSON )
import qualified Data.Map as Map
import qualified GHC.Generics as Generics

import Transition ( Transition )

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