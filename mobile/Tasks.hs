module Tasks where

{-
type mobile =
| Obj of int (* weight *)
| Bar of int (* radius *) * mobile * mobile
-}

data Mobile
  = Obj Integer
  | Bar Integer Mobile Mobile
  deriving (Eq, Show)

-- examples by ChatGPT (with corrections)

-- A single object of weight 5
example1 :: Mobile
example1 = Obj 5

-- A balanced bar: left and right objects both weight 3
example2 :: Mobile
example2 = Bar 2 (Obj 3) (Obj 3)

-- An unbalanced bar: left = 2, right = 4
example3 :: Mobile
example3 = Bar 2 (Obj 2) (Obj 4)

-- A nested balanced mobile:
--        Bar
--       /   \
--     Obj  Bar
--          / \
--       Obj  Obj
example4 :: Mobile
example4 =
  Bar 3
    (Obj 4)
    (Bar 2 (Obj 2) (Obj 2))
-- Total weight left = 4
-- Total weight right = 2 + 2 = 4 → balanced

-- Deeply nested unbalanced mobile
example5 :: Mobile
example5 =
  Bar 1
    (Bar 2 (Obj 2) (Obj 1))  -- total = 3
    (Bar 1 (Obj 2) (Obj 2))  -- total = 4 → unbalanced

-- Symmetric structure, but not weight-balanced
example6 :: Mobile
example6 =
  Bar 2
    (Bar 1 (Obj 1) (Obj 2))
    (Bar 1 (Obj 2) (Obj 2))
-- left = 1 + 2 = 3
-- right = 2 + 2 = 4 → unbalanced

sharedSub :: Mobile
sharedSub = Bar 1 (Obj 2) (Obj 2)

example7 :: Mobile
example7 = Bar 3 sharedSub sharedSub
-- left and right are both the same submobile (total = 4)

-- Check whether a mobile is balanced. This means:
-- under each bar, the left and right submobiles have exactly the same weight.

balanced :: Mobile -> Maybe Integer
balanced (Obj weight) = do
  return weight
balanced (Bar radius m1 m2) = do
  w1 <- balanced m1
  w2 <- balanced m2
  if w1 == w2 then Just (w1 + w2) else Nothing

-- Take a mobile that might not be balanced,
-- and return a mobile of the same shape, whose object weights have been adjusted to ensure perfect balance.

make_balanced' :: Integer -> Mobile -> (Integer, Mobile)
make_balanced' adj (Obj weight)       = (0, Obj adj)
make_balanced' adj (Bar radius m1 m2) = (1 + max h1 h2, Bar radius m1' m2')
  where
    (h1, m1') = make_balanced' (adj `div` 2) m1
    (h2, m2') = make_balanced' (adj `div` 2) m2

make_balanced :: Mobile -> Mobile
make_balanced m = m'
  where
    (h, m') = make_balanced' (2 ^ h) m

-- upper Gauss bracket of square root (assumes n >= 0)

isqrt :: Integer -> Integer
isqrt n = approximate 0
  where
    approximate i = if i*i >= n then i else approximate (i+1)

{-
Compute the list of the coordinates of all the leaf
objects, from (logical) left to right.
I'm returning lower left coordinates.
-}

type Coord = (Integer, Integer)
type Scale = (Integer, Integer)
coordinates :: Scale -> Coord -> Mobile -> (Integer, [Coord])
coordinates sc ref (Obj weight) = (isqrt weight, [ ref ])
coordinates sc@(xs, ys) (xr, yr) (Bar radius m1 m2) = (2 * max w1 w2, co1 ++ co2)
  where
    (w1, co1) = coordinates sc (xr, yr + ys) m1
    (w2, co2) = coordinates sc (xr + max w1 w2, yr + ys) m2
