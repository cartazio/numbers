module Test.Data.Number.BigFloat (bigfloat_properties) where

import Data.Number.BigFloat (BigFloat, Prec50)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)


prop_bigfloat_double_agree_equality :: Double -> Bool
prop_bigfloat_double_agree_equality dbl =
  dbl == bf1
  where
    -- Convert dbl to a BigFloat.
    bf1' = realToFrac dbl :: BigFloat Prec50
    -- And convert it back.
    bf1 = realToFrac bf1' :: Double


prop_bigfloat_double_agree_ordering :: Double -> Double -> Bool
prop_bigfloat_double_agree_ordering dbl1 dbl2 =
  compare dbl1 dbl2 == compare bf1 bf2
  where
    -- Convert dbl1,dbl2 to BigFloat.
    bf1 = realToFrac dbl1 :: BigFloat Prec50
    bf2 = realToFrac dbl2 :: BigFloat Prec50


bigfloat_properties :: Test.Framework.Test
bigfloat_properties =
  testGroup "BigFloat Properties" [
    testProperty
      "bigfloat/double agree (equality)"
      prop_bigfloat_double_agree_equality,

    testProperty
      "bigfloat/double agree (ordering)"
      prop_bigfloat_double_agree_ordering
  ]
