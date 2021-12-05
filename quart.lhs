> module Main where

> import Data.List (sort)

> main = interact (show . f)

> type Summary a = (a,[a],a,a,a,[a],a)

> f :: String -> Summary Double
> f = s5n . sort . map ((100 *) . read . last . words) . lines

> s5n :: (Ord a, Fractional a) => [a] -> Summary a
> s5n xs = (q0, so, q1, q2, q3, bo, q4)
>     where q2 = median xs
>           (f,g) = splitAt (length xs `div` 2) xs
>           q1 = median f
>           q3 = median g
>           iqr = q3 - q1
>           q0 = head f
>           q4 = last g
>           so = filter (< (q1 - 1.5 * iqr)) f
>           bo = filter (> (q3 + 1.5 * iqr)) g

> median :: (Eq a, Fractional a) => [a] -> a
> median xs
>     | length f > length g = last f
>     | length f < length g = head g
>     | otherwise = (last f + head g) / 2
>     where (f,g) = splitAt (length xs `div` 2) xs
