module Main (main) where

import Knuckles (generate)

main :: IO ()
main = do
  generate "A" "ALaws"
  generate "B" "BLaws"
  generate "B" "CLaws"
