module Main (main) where

import SimpleJSON
import PutJSON

-- jsonObject = JObject [("yuno", JBool True), ("ichigo", JBool True)]

main :: IO()
main = putStrLn (renderJValue (JObject [("yuno", JBool True), ("ichigo", JBool True)]))

