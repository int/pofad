-- assuming f (x,y) is O(1) (though sometimes it can be expensive),
-- we mainly analyze the complexity of counts of the f(x,y) evaluations
--
-- in ghci, you can :set +s to see the running time of differnt versions

import Data.List (sort)
import Test.QuickCheck

-- as always, the first is no-brainer brute. O(z^2)
invert0 f z = [(x,y) | x <- [0..z], y <- [0..z], f (x,y) == z]

-- saddleback O(z)
invert1 f z = find (0,z) f z
find (u,v) f z
    | u > z || v < 0 = []
    | z' < z = find (u+1, v) f z
    | z' > z = find (u, v-1) f z
    | otherwise = (u,v) : find (u+1, v-1) f z
    where z' = f (u,v)


-- tests
nat = choose (0::Integer, 100)

f0 (x,y) = 2^y * (2*x+1) - 1
f1 (x,y) = x*2^x + y*2^y + 2*x + y
f2 (x,y) = 3*x + 37*y + y^2
f3 (x,y) = x^2 + y^2 + x + y
f4 (x,y) = x + 2^y + y - 1

same xs = all (== head xs) xs

invs = [invert0, invert1]
fs = [f0, f1, f2, f3, f4]

results z f = map (r z f) invs
    where r z f i = sort $ i f z

prop f = forAll nat $ \z -> same $ results z f
check = quickCheck . prop
checkall = quickCheck . conjoin $ map prop fs

main = do
    check f0
    check f3
    verboseCheck . prop $ f2
    checkall
