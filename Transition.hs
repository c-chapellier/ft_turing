{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Transition where

import Data.Aeson as Aeson ( FromJSON, ToJSON )
import qualified GHC.Generics as Generics

data Transition = Transition
    { read          :: String
    , to_state      :: String
    , write         :: String
    , action        :: String
    } deriving (Generics.Generic)

instance Show Transition where
    show a = Transition.read a ++ ") -> (" ++ to_state a ++ ", " ++ write a ++ ", " ++ action a

instance FromJSON Transition
instance ToJSON Transition
