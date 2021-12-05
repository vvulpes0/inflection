> module Main where

> import Data.Bifunctor (bimap,first)
> import Data.List (inits,intercalate
>                  ,isInfixOf,isPrefixOf,isSuffixOf
>                  ,partition,tails)
> import Data.Map (Map)
> import System.Environment (getArgs)
> import qualified Data.Map as Map

> main :: IO ()
> main = putStr =<< f =<< getArgs
>     where f (t:[]) = (>>= id) (main' <$> lines' <$> readFile t)
>           f _ = return $ unlines
>                 [ "usage: cm training"
>                 , "\ttest data on stdin"
>                 , "\tresults on stdout"
>                 ]

> lines' :: String -> [String]
> lines' = map (filter (/= '\r')) . lines

> main' :: [String] -> IO String
> main' t
>     = unlines <$> map (inflect (inflector t)) <$> lines' <$> getContents

> type Context = (String,String)

> -- |Construct an inflector from lines of training data.
> inflector :: [String] -> Map String [(Int,(Context,Context),String)]
> inflector = Map.fromList . map (fmap generalize)
>             . clusterMap (concat . drop 2 . splitOn '\t')

> -- |Apply an inflector to a line of test data.
> inflect f line
>     = maybe (g w) g (apply w <$> match w <$> Map.lookup t f)
>     where (w:t:_) = splitOn '\t' line
>           g x = intercalate "\t" [w, x, t]

> -- |Construct a list of lists, grouped by the given function.
> cluster :: Eq b => (a -> b) -> [a] -> [(b, [a])]
> cluster _ [] = []
> cluster g xs@(x:_) = uncurry (:) . bimap h (cluster g)
>                      $ partition f xs
>     where f y = (g y) == (g x)
>           h y = (g $ head y, y)

> -- |The longest substring of the first argument that is in the second.
> gcs :: String -> String -> String
> gcs a b = snd . maximum . map (f . lcp) $ tails a
>     where lcp x = last . filter (`isInfixOf` b) $ inits x
>           f x = (length x, x)

> -- |The longest common suffix of the two arguments.
> lcs :: Eq a => [a] -> [a] -> [a]
> lcs a b = reverse . map fst . takeWhile (uncurry (==))
>           $ zip (reverse a) (reverse b)

> -- |In what context does the first argument appear in the second?
> contextualize :: String -> String -> Context
> contextualize p s
>     | not (p `isInfixOf` s) = mempty -- not really, just, no Maybe
>     | p `isPrefixOf` s = ("", drop (length p) s)
>     | otherwise = first (take 1 s ++) . contextualize p $ drop 1 s

> type Pattern = (Context, String, Context)

> pattern :: [String] -> Pattern
> pattern (x:y:_) = let g = gcs x y
>                   in (contextualize g x, g, contextualize g y)
> pattern _ = mempty

> clusterMap :: Eq b => (String -> b) -> [String] -> [(b, [Pattern])]
> clusterMap g = map (fmap (map (pattern . splitOn '\t')))
>                . cluster g

> generalize :: [Pattern] -> [(Int,(Context,Context),String)]
> generalize ps = generalize' . map s $ cluster (\(a,_,c)->(a,c)) ps
>     where s = fmap (map (\(_,y,_) -> y))

> generalize' :: Eq a => [(b, [[a]])] -> [(Int, b, [a])]
> generalize' [] = []
> generalize' ((_,[]):ps) = generalize' ps
> generalize' ((b,(x:xs)):ps)
>     = (1 + length c, b, foldr1 lcs (x:c)) : generalize' ((b,u):ps)
>     where (c,u) = partition (not . null . lcs x) xs

> match :: String -> [(Int, (Context, Context), String)]
>       -> (Int, (Context, Context), String)
> match w gs
>     | null possible = maximum gs
>     | otherwise = maximum . snd . maximum $ cluster score possible
>     where f (_, (p, _), r)
>               = ((r++snd p) `isSuffixOf` w) && (fst p `isPrefixOf` w)
>           possible = filter f gs
>           score (_, (p, _), r) = length r + length (uncurry (++) p)

> apply :: String -> (Int, (Context, Context), String) -> String
> apply w (_, (p, s), r)
>     = fst s ++ (desuffix $ deprefix w) ++ snd s
>     where deprefix = drop (length $ fst p)
>           desuffix = reverse . drop (length $ snd p) . reverse

> splitOn :: Eq a => a -> [a] -> [[a]]
> splitOn _ [] = []
> splitOn x ys = uncurry (:) . fmap (splitOn x . dropWhile (== x))
>                $ break (== x) ys
