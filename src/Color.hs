module Color where

black :: String -> String
black s = "\27[30m" ++ s ++ "\27[0m"

red :: String -> String
red s = "\27[31m" ++ s ++ "\27[0m"

green :: String -> String
green s = "\27[32m" ++ s ++ "\27[0m"

yellow :: String -> String
yellow s = "\27[33m" ++ s ++ "\27[0m"

blue :: String -> String
blue s = "\27[34m" ++ s ++ "\27[0m"

magenta :: String -> String
magenta s = "\27[35m" ++ s ++ "\27[0m"

cyan :: String -> String
cyan s = "\27[36m" ++ s ++ "\27[0m"

white :: String -> String
white s = "\27[37m" ++ s ++ "\27[0m"
