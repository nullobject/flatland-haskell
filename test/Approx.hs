module Approx where

import Test.HUnit

class Approx a where
  (~=) :: a -> a -> Bool

instance Approx Double where
  x ~= y = abs (x - y) < 1.0e-2

instance (Approx a, Approx b) => Approx (a, b) where
  (a0, a1) ~= (b0, b1) = a0 ~= b0 && a1 ~= b1

(@?~=) :: (Show a, Approx a) => a -> a -> Assertion
(@?~=) actual expected = expected ~= actual @? message
  where message = "expected: " ++ show expected ++ "\n but got: " ++ show actual

(@~=?) :: (Show a, Approx a) => a -> a -> Assertion
(@~=?) expected actual = expected ~= actual @? message
  where message = "expected: " ++ show expected ++ "\n but got: " ++ show actual
