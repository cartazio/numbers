-- | An incomplete implementation of interval aritrhmetic.
module Data.Number.Interval(Interval, ival, ival', getIval) where

data Interval a = I !a !a

ival :: (Ord a) => a -> a -> Interval a
ival l h | l <= h = I l h
         | otherwise = error "Interval.ival: low > high"

ival' :: (Ord a) => a -> a -> Interval a
ival' x y | x <= y = I x y
          | otherwise = I x y

getIval :: Interval a -> (a, a)
getIval (I l h) = (l, h)

ispan :: (Num a) => Interval a -> a
ispan (I l h) = h - l

instance (Ord a) => Eq (Interval a) where
    I l h == I l' h'  =  l == h' && h == l'
    I l h /= I l' h'  =  h < l' || h' < l

instance (Ord a) => Ord (Interval a) where
    I l h <  I l' h'  =  h <  l'
    I l h <= I l' h'  =  h <= l'
    I l h >  I l' h'  =  l >  h'
    I l h >= I l' h'  =  l >= h'
    -- These funcions are partial, so we just leave them out.
    compare _ _ = error "Interval compare"
    max _ _ = error "Interval max"
    min _ _ = error "Interval min"

instance (Show a) => Show (Interval a) where
    showsPrec p (I l h) = showParen (p >= 11) $ showString "ival "
                                              . showsPrec 11 l
                                              . showsPrec 11 h

instance (Read a) => Read (Interval a) where
    readsPrec p = readParen (p >= 11) (\s0 -> [ (I l h , s2)
                                              | ("ival", s0) <- lex          s
                                              , (  l   , s1) <- readsPrec 11 s0
                                              , (    h , s2) <- readsPrec 11 s1
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
    fromRational r   =  I l l where l = fromRational r

instance (Bounded a) => Bounded (Interval a) where
    minBound = I minBound minBound
    maxBound = I maxBound maxBound
