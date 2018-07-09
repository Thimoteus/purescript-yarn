module Test.Main where

import Data.String.Yarn (capWords, capitalize, fromChars, leftpad, lines, replicate, reverse, substitute, unlines, unwords, words)
import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Data.String (Pattern(..), Replacement(..))

multiln :: String
multiln = """abc
def
ghi"""

littlemj :: String
littlemj = "abc, it's easy as 123, as simple as do re mi, abc, 123, baby you and me girl"

littlemj' :: String
littlemj' = "123, it's easy as 123, as simple as do re mi, 123, 123, baby you and me girl"

main :: Effect Unit
main = do
  log "fromChars test"
  logShow $ fromChars ['a', 'b', 'c'] == "abc"
  log "lines and unlines test"
  logShow $ unlines (lines multiln) == multiln
  log "words and unwords test"
  logShow $ unwords (words "abc def ghi") == "abc def ghi"
  log "substitute test"
  logShow $ substitute (Pattern "abc") (Replacement "123") littlemj == littlemj'
  log "capitalize test"
  logShow $ capitalize "abc 123" == "Abc 123"
  log "capWords test"
  logShow $ capWords "abc def ghi" == "Abc Def Ghi"
  log "leftpad test"
  logShow $ leftpad "abc" == " abc"
  log "reverse test"
  logShow $ reverse "abcdefg" == "gfedcba"
  log "replicate test"
  logShow $ replicate 5 'a' == "aaaaa"
  log "replicate negative test"
  logShow $ replicate (-1) 'a' == ""
