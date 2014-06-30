module Main where

import Test.HUnit
import System.Exit

import Data.Summary.Bool
import Data.Summary.Double

main :: IO ()
main = do
    c <- runTestTT allTests
    case failures c + errors c of
        0 -> exitSuccess
        _ -> exitFailure

allTests :: Test
allTests = TestList [boolTests, doubleTests]

t :: String -> Assertion -> Test
t cs a = TestLabel cs $ TestCase a

----------------------------------------------------------------
-- Data.Summary.Bool
----------------

bs :: BoolSumm
bs = boolSumm [True, False, True, False]

boolTests :: Test
boolTests = TestLabel "Data.Summary.Bool" $ TestList [
              t "sampleMean" $ sampleMean bs @?= 0.5
            , t "sampleSE"   $ sampleSE   bs @?= 0.25
            , t "sampleSize" $ sampleSize bs @?= 4
            ]

----------------------------------------------------------------
-- Data.Summary.Double
----------------

ds :: DoubleSumm
ds = doubleSumm [1..5]

doubleTests :: Test
doubleTests = TestLabel "Data.Summary.Double" $ TestList [
                TestCase $ sampleMean ds @?= 3
              , TestCase $ sampleVar  ds @?= 2.5
              , TestCase $ sampleSD   ds @?= sqrt 2.5
              , TestCase $ sampleSE   ds @?= sqrt 2.5 / sqrt 5
              , TestCase $ sampleSize ds @?= 5
              ]