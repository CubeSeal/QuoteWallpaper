module Tests where

import Test.HUnit

import DalleDownload

import qualified System.Exit as Exit

main :: IO ()
main = do

openAIAPITest :: Test
openAIAPITest = TestCase $ assertString testConnection