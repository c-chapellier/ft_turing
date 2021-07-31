{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Transition where

import qualified Data.Aeson as Aeson ( FromJSON, ToJSON )
import qualified GHC.Generics as Generics

import qualified Color

data Transition = Transition
    { read          :: String
    , to_state      :: String
    , write         :: String
    , action        :: String
    } deriving (Generics.Generic)

instance Show Transition where
    show a = Color.red (Transition.read a) ++ ") -> (" ++ Color.blue (to_state a) ++ ", " ++ Color.magenta (write a) ++ ", " ++ Color.green (action a)

instance Aeson.FromJSON Transition
instance Aeson.ToJSON Transition
