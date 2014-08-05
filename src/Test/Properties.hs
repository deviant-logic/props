{-# LANGUAGE NoMonomorphismRestriction #-}

module Test.Properties where

import Data.Function (on)

-- | @f a b == f b a@
--
-- prop> symmetric (==) :: Int -> Int -> Bool
-- prop> symmetric (+)  :: Int -> Int -> Bool
symmetric     = symmetricBy id

-- | @'symmetric' f a b@ implies @a == b@
--
-- prop> antisymmetric (-) :: Int -> Int -> Bool
antisymmetric = antisymmetricBy id

-- | @a == (g . f) a@
--
-- prop> inverts pred succ :: Int -> Bool
-- prop> inverts succ pred :: Char -> Bool
inverts       = invertsBy id

-- | @a == (f . f) a@ (alternatively, @'inverts' f f@)
--
-- prop> involutive negate
-- prop> involutive reverse :: String -> Bool
involutive    = involutiveBy id

-- | @a <= f a@
--
-- prop> nonDecreasing id
-- prop> nonDecreasing (\x -> if even x then x else succ x)
nonDecreasing = nonDecreasingBy id

-- | @a < f a@
--
-- prop> increasing succ :: Int -> Bool
increasing    = increasingBy id

-- | Alias for 'idempotent'
fixes         = fixesBy id

-- | @f a == f (f a)@
--
-- prop> idempotent (const "thingy")
-- prop> idempotent (*0)
-- prop> idempotent (&& False)
idempotent    = fixes

-- | @f e a == a@
--
-- prop> leftId (+) 0
leftId        = leftIdBy id

-- | @f a e == a@
--
-- prop> rightId (+) 0
rightId       = rightIdBy id

-- | @'leftId' f e && 'rightId' f e@
--
-- prop> identity (+) 0
identity      = identityBy id

-- | @f a (f b c) == f (f a b) c@
--
-- prop> associative (&&)
-- prop> associative (||)
-- prop> associative (++)
-- prop> associative (*)
associative   = associativeBy id

-- | @'identity' f e@ and @'associative' f a b c@
--
-- prop> monoidal (&&) True
-- prop> monoidal (||) False
-- prop> monoidal (++) []
-- prop> monoidal (*) 1
monoidal      = monoidalBy id

-- | @a == (f . g) a && b == (g . f) b@
--
-- >>> :set -XTupleSections
--
-- prop> isomorphic succ (pred :: Int -> Int)
-- prop> isomorphic not not
-- prop> isomorphic reverse (reverse :: String -> String)
-- prop> isomorphic snd (((),) :: Int -> ((),Int))
isomorphic = isomorphicBy id id

-- | @Just a == (g . f) a@
--
-- >>> import Text.Read (readMaybe)

-- prop> partiallyIsomorphic id Just
-- prop> partiallyIsomorphic show readMaybe
partiallyIsomorphic = partiallyIsomorphicBy id

-- | @eq (f a) == eq (g a)@
--
-- prop> equalizes even (*2) (*4)
-- prop> equalizes (const [4]) (take 7) (take 12)
equalizes cmp f g a = eqBy cmp (f a) (g a)

eqBy  = on (==)
leqBy = on (<=)
leBy  = on (<)

symmetricBy     cmp f a b     = eqBy cmp (f a b) (f b a)
antisymmetricBy cmp f a b     = eqBy cmp a b || not (symmetricBy cmp f a b)
invertsBy       cmp f g a     = eqBy cmp a (g . f $ a)
involutiveBy    cmp f a       = invertsBy cmp f f a
nonDecreasingBy cmp f a       = leqBy cmp a (f a)
increasingBy    cmp f a       = leBy  cmp a (f a)
fixesBy         cmp f a       = eqBy cmp (f a) (f . f $ a)
idempotentBy                  = fixesBy
associativeBy   cmp f a b c   = eqBy cmp (f a (f b c)) (f (f a b) c)
monoidalBy      cmp f e a b c = all (identityBy cmp f e) [a, b, c] && associativeBy cmp f a b c

leftIdBy        cmp f e a     = eqBy cmp (f e a) a
rightIdBy       cmp f e a     = eqBy cmp (f a e) a
identityBy      cmp f e a     = leftIdBy cmp f e a && rightIdBy cmp f e a

isomorphicBy  cmpa cmpb f g a b = invertsBy cmpa f g a && invertsBy cmpb g f b
partiallyIsomorphicBy cmp f g a = eqBy (fmap cmp) (Just a) (g . f $ a)
