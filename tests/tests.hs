{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
    ( assertString, runTestTTAndExit, Test(TestCase), assertBool )

import DalleDownload (fetchDalle3 )

import qualified System.Exit as Exit

main :: IO ()
main = runTestTTAndExit openAIAPITest

openAIAPITest :: Test
openAIAPITest = TestCase $ do
  result <- fetchDalle3
  assertBool "OpenAI Connection does not work" $ result /= ""