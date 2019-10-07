import Control.Monad
import Data.List
import Data.List.Split

intsToTuple :: [String] -> (Int, Int)
intsToTuple [a, b] = (read a :: Int, read b :: Int)

getArea :: [(Int, Int)] -> Int
getArea [x] = 0
getArea (x:xs) = currentDistance + getArea xs
    where
        currentDistance = fst(x) * snd(head xs) - snd(x) * fst(head xs)

main :: IO()
main = do
    q <- readLn :: IO Int
    coordinates <- forM [1..q] $ (\q_itr -> do
        s <- getLine
        return (intsToTuple (splitOn " " s))
        )
    putStrLn(show (fromIntegral (area coordinates) / 2))
    where 
        area coordinates = getArea coordinates + getArea [last coordinates, head coordinates]

