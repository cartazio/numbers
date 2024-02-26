{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | An incomplete implementation of interval aritrhmetic.
module Data.Number.Interval(Interval, ival, ival', ispan, getIval, IntervalNesting(..),
                            module Data.Number.PartialOrd) where
import Data.Number.PartialOrd

data Interval a = I a a

ival :: (Ord a) => a -> a -> Interval a
ival l h | l <= h = I l h
         | otherwise = error "Interval.ival: low > high"

ival' :: (Ord a) => a -> a -> Interval a
ival' x y | x <= y = I x y
          | otherwise = I y x

getIval :: Interval a -> (a, a)
getIval (I l h) = (l, h)

ispan :: (Num a) => Interval a -> a
ispan (I l h) = h - l

instance (Eq a) => Eq (Interval a) where
    I l h == I l' h'  =  l == l' && h == h'
    I l h /= I l' h'  =  l /= l' || h /= h'
    
-- could probably use some efficienct overrides
instance (Ord a) => PartialOrd (Interval a) where
    cmp (I l h) (I l' h') = case (compare l l', compare h h') of
                                 (EQ          , EQ          ) -> Just EQ
                                 (GT          , EQ          ) -> Just GT
                                 (LT          , EQ          ) -> Just LT
                                 (EQ          , GT          ) -> Just GT
                                 (GT          , GT          ) -> Just GT
                                 (LT          , GT          ) -> Nothing
                                 (EQ          , LT          ) -> Just LT
                                 (GT          , LT          ) -> Nothing
                                 (LT          , LT          ) -> Just LT

instance (Show a) => Show (Interval a) where
    showsPrec p (I l h) = showParen (p >= 11) $ showString "ival "
                                              . showsPrec 11 l
                                              . showsPrec 11 h

instance (Read a) => Read (Interval a) where
    readsPrec p = readParen (p >= 11) (\s0 -> [ (I l h , s3)
                                              | ("ival", s1) <- lex          s0
                                              , (  l   , s2) <- readsPrec 11 s1
                                              , (    h , s3) <- readsPrec 11 s2
                                              ])

instance (Ord a, Num a) => Num (Interval a) where
    I l h + I l' h'  =  I (l + l') (h + h')
    I l h - I l' h'  =  I (l - h') (h - l')
    I l h * I l' h'  =  I (minimum xs) (maximum xs) where xs = [l*l', l*h', h*l', h*h']
    negate (I l h)   =  I (-h) (-l)
    -- leave out abs and signum
    abs _ = error "Interval abs"
    signum _ = error "Interval signum"
    fromInteger i    =  I l l where l = fromInteger i
 
instance (Ord a, Fractional a) => Fractional (Interval a) where
    I l h / I l' h' | signum l' == signum h' && l' /= 0 =  I (minimum xs) (maximum xs)
                    | otherwise = error "Interval: division by 0"
                    where xs = [l/l', l/h', h/l', h/h']
    fromRational r  =  I l l where l = fromRational r

-- this definition ensures that all intervals of a bounded type
-- are "between" minBound and maxBound according to the
-- (default) PartialOrd instance
instance (Bounded a) => Bounded (Interval a) where
    minBound = I minBound minBound
    maxBound = I maxBound maxBound

newtype IntervalNesting a = Nesting { getNesting :: Interval a } deriving (Eq, Show, Read, Num, Fractional)

-- this instance implements a partial order based on nesting,
-- which should agree with the natural order on ispan where
-- applicable
instance (Ord a) => PartialOrd (IntervalNesting a) where
    cmp (Nesting (I l h)) (Nesting (I l' h')) = case (compare l l', compare h h') of
                                                     (EQ          , EQ          ) -> Just EQ
                                                     (GT          , EQ          ) -> Just LT
                                                     (LT          , EQ          ) -> Just GT
                                                     (EQ          , GT          ) -> Just LT
                                                     (GT          , GT          ) -> Nothing
                                                     (LT          , GT          ) -> Just LT
                                                     (EQ          , LT          ) -> Just GT
                                                     (GT          , LT          ) -> Just GT
                                                     (LT          , LT          ) -> Nothing
