module Main where

import Test.HUnit
    ( assertString, runTestTTAndExit, Test(TestCase), assertBool )

import DalleDownload ( testConnection )

import qualified System.Exit as Exit

main :: IO ()
main = runTestTTAndExit openAIAPITest

openAIAPITest :: Test
openAIAPITest = TestCase $ do
  result <- testConnection
  assertBool "OpenAI Connection does not work" $ result /= ""