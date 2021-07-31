module Main where
    
import qualified System.Environment as Env ( getArgs )

import qualified Turing

main :: IO ()
main = do
    args <- Env.getArgs
    Turing.turing args
