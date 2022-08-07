module Data.Number.Abstract
where

-- | An abstract real number.
--
-- This type implements the standard numeric type classes, but has only one element.
-- It thus represents numbers that are held entirely abstract.
data AReal = AReal
  deriving (Eq, Ord, Read, Show)

unary :: AReal -> AReal
unary _ = AReal

binary :: AReal -> AReal -> AReal
binary _ _ = AReal

instance Num AReal where
  (+) = binary
  (-) = binary
  (*) = binary
  negate = unary
  abs = unary
  signum = unary
  fromInteger _ = AReal

instance Fractional AReal where
  (/) = binary
  recip = unary
  fromRational _ = AReal

instance Floating AReal where
  pi = AReal
  exp = unary
  log = unary
  sqrt = unary
  (**) = binary
  logBase = binary
  sin = unary
  cos = unary
  tan = unary
  asin = unary
  acos = unary
  atan = unary
  sinh = unary
  cosh = unary
  tanh = unary
  asinh = unary
  acosh = unary
  atanh = unary
