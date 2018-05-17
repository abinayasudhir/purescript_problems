module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (concat, filter, foldr, length, reverse, (!!))
import Data.Int (floor)
import Data.Int as I
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Number (fromString)
import Data.String (Pattern(Pattern), dropRight, joinWith, split, takeRight)
import Data.String as S
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Problem 01"
  log $ show $ problem_01 [1,2,3,4,5,6,7,8,9,10,14,15,20,21,35,70,100,101,102,350,700]
  log $ show $ ""
  log $ show $ "Problem 02"
  log $ "abcd1234xy%$.g12.8udbfjwlep"
  log $ show $ dollars "abcd1234xy%$.g12.8udbfjwlep"
  log $ ".1"
  log $ show $ dollars ".1"
  log $ "1"
  log $ show $ dollars "1"
  log $ ".12333"
  log $ show $ dollars ".12333"
  log $ "1000000.01"
  log $ show $ dollars "1000000.01"
  log $ ".000001"
  log $ show $ dollars ".000001"
  log $ "12345678901234567890.12345678901234567890"
  log $ show $ dollars "12345678901234567890.12345678901234567890"
  log $ "1030400000.12345678901234567890"
  log $ show $ dollars "1030400000.12345678901234567890"
  log "\n\nproblems in the email\n\n"
  log $ "0"
  log $ show $ dollars "0"
  log $ "1"
  log $ show $ dollars "1"
  log $ ".1"
  log $ show $ dollars ".1"
  log $ "1."
  log $ show $ dollars "1."
  log $ "0."
  log $ show $ dollars "0."
  log $ ".34"
  log $ show $ dollars ".34"
  log $ "0.3456789"
  log $ show $ dollars "0.3456789"
  log $ "1.0"
  log $ show $ dollars "1.0"
  log $ "1.01"
  log $ show $ dollars "1.01"
  log $ "1000456.13"
  log $ show $ dollars "1000456.13"
  log $ "\n\ndollar vs dollars\n\n"
  log $ "1.13"
  log $ show $ dollars "1.13"
  log $ "1000.13"
  log $ show $ dollars "1000.13"
  log $ "1000000.13"
  log $ show $ dollars "1000000.13"
  log $ "1000000000.13"
  log $ show $ dollars "1000000000.13"

  -- 1000456.13

----------------------------------------------------------------------------------------

problem_01 :: Array Int -> Array Int
problem_01 = filter fn
  where
    fn i =
      if (i `mod` 5 == 0 || i `mod` 7 == 0) && (i `mod` 35 /= 0) -- LCM(5,7) is 35
      then false
      else true

------------------------------------------------------------------------------------------

dollars :: String -> String
dollars = step2 <<< step1

step1 :: String -> Tuple (Array(Tuple Int String)) Int
step1 s = ret
  where
    s' = joinWith "" $ filter fn (split (Pattern "") s)

    fn s = if (s >= "0" && s <= "9") || s == "." then true else false

    ary = split (Pattern ".") s'

    limit100 n = (floor (n * 100.0))

    toNum :: Maybe String -> Number
    toNum (Just n) = fromMaybe 0.0 $ fromString n
    toNum Nothing = 0.0

    ret = Tuple
          (reverse $ filter fil $ fns1 0 $ fromMaybe "" (ary !! 0))
          (limit100 $ toNum $ (map ((<>) "0.") ary !! 1))

fil :: Tuple Int String -> Boolean
fil (Tuple 0 _) = false
fil _ = true

fns1 :: Int -> String -> Array(Tuple Int String)
fns1 u s = [Tuple (fromMaybe 0 $ I.fromString $ takeRight 3 s) (fromMaybe "Not Implemented" $ places !! u)]
           <>
           (if S.length s > 3 then fns1 (u+1) (dropRight 3 s) else [])

step2 :: Tuple (Array(Tuple Int String)) Int -> String
step2 (Tuple dollar cent) = d' <> sp <> " and " <> cents cent
  where
    d = joinWith " " (map (\(Tuple a b)-> hundreds a b) dollar)
    d' = if d == "" then "zero" else d

    sp = if length dollar == 1
            && (foldr (\(Tuple no _) -> (+) no) 0 dollar) == 1
            && (foldr (\(Tuple _ p) -> (<>) p) "" dollar) == ""
         then " dollar" else " dollars"

cents :: Int -> String
cents 0 = "zero cents"
cents 1 = "one cent"
cents x = no2String x <> " cents"

no2String :: Int -> String
no2String c | c >= 1 && c <= 99 = unsafePartial $ fromJust $ (oneTo99 !! (c - 1))
            | otherwise = ""

hundreds' :: Int -> String
hundreds' no = ret h n
  where
    h = no2String (no/100)
    n = no2String (no `mod` 100)
    ret "" "" = ""
    ret h "" = h <> " hundred"
    ret "" n = n
    ret h n = h <> " hundred" <> " and " <> n

hundreds :: Int -> String -> String
hundreds h p = fn (hundreds' h) p
  where
    fn "" "" = ""
    fn "" p = ""
    fn h "" = h
    fn h p = h <> " " <> p

places :: Array String
places = ["", "thousand", "million", "billion", "trillion", "quadrillion", "quintillion"]

ones :: Array String
ones = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

teens :: Array String
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]

tens :: Array String
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

oneTo99 :: Array String
oneTo99 = ones <> teens <> (concat $ (map itens tens))
  where
    itens :: String -> Array String
    itens t = [t] <> (map ((<>) t) pOnes)
    pOnes = map ((<>) "-") ones
