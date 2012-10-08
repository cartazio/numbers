module Main
where

import Test.Framework (
  Test,
  defaultMain,
  )

import Data.Number.BigFloat (bigfloat_properties)

main :: IO ()
main = defaultMain tests

tests :: [Test.Framework.Test]
tests = [ bigfloat_properties ]
