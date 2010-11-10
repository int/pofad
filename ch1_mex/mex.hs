import Data.List
import Data.Array
import Test.QuickCheck


-- simple, nice but O(n^2)
minfree :: [Int] -> Int
minfree xs = head ([0..] \\ xs)

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

-- pure functional
checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) $ zip (filter ( <= n) xs) (repeat True)
    where n = length xs

-- O(n) using array
minfree' = search . checklist

-- just another example for using array
countlist :: [Int] -> Int -> Array Int Int
countlist xs n = accumArray (+) 0 (0, n) $ zip xs (repeat 1)

countsort xs n = concat [ replicate k x | (x, k) <-  assocs . countlist xs $ n ]

-- tests
limit = 2^10
nats = listOf . choose $ (0, limit)
prop = forAll nats $ \x -> minfree x == minfree' x

main = do
    print $ f [0..10]
    print $ f [2,4,6,8,5,0,3,5,1,2]
    quickCheck prop
    quickCheck (forAll nats $ \xs -> sort xs == countsort xs limit)
    where f xs = (minfree xs, minfree' xs)
