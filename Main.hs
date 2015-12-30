import Data.List
import Data.List.Split
import System.Environment

type Line = String
type Cell = String

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  let lp = rpadToMaxLen ' ' $ lines file --ensure last col has elements
  let erg = (if "inflate" `elem` args then inflate else compress) lp
  mapM_ (putStrLn . concat) erg

rpad :: a -> Int -> [a] -> [a]
rpad a len as = take len $ as ++ repeat a

rpadToMaxLen :: a -> [[a]] -> [[a]]
rpadToMaxLen a as = map (rpad a len) as
  where len = maximum $ map length as

dissect :: (a -> Bool) -> [a] -> [Int]
dissect f = map sum . chunksOf 2 . map length . split (condense $ whenElt f)

compress :: [Line] -> [[Cell]]
compress ls = add_del $ transpose $ map shrinkcol cols
  where delim     = ';'
        head_lens = dissect (==' ') (head ls) --calc lengths of the chunks(colname + spaces) for splitplaces
        cols      = transpose $ map (splitPlaces head_lens) ls
        shrinkcol = map (reverse . dropWhile (==' ') . reverse . dropWhile (==' '))
        add_del   = map (map (++[delim]))

inflate :: [Line] -> [[Cell]]
inflate ls = transpose $ map padcol table
  where table = transpose $ map (splitOn ";") ls --list of cols
        padcol c = map (rpad ' ' m) c --pad whole col
          where m = (1+) $ maximum $ map length c --determine longest string in col, pad one more for prettiness
