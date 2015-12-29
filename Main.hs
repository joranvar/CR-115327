import Data.List
import Data.List.Split
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  file <- readFile $ head args
  let lp = preprocess (lines file)
  let erg = (if "inflate" `elem` args then inflate else compress) lp
  mapM_ (putStrLn . concat) erg

rpad :: Int -> String -> String
rpad len string = take len $ string ++ repeat ' '

--ensure last col has elements
preprocess :: [String] -> [String]
preprocess ls = map (rpad len) ls
  where len = maximum $ map length ls

--calc lengths of the chunks(colname + spaces) for splitplaces
dissect :: String -> [Int]
dissect line = zipWith (\a b->length (a++b)) names spaces
  where names = wordsBy (==' ') line
        spaces= wordsBy (/=' ') (line++" ") --add " " to ensure space after last col-name

compress :: [String] -> [[String]]
compress ls = add_del $ transpose $ map shrinkcol cols
  where delim     = ';'
        head_lens = dissect (head ls)
        cols      = transpose $ map (splitPlaces head_lens) ls
        shrinkcol = map (reverse . dropWhile (==' ') . reverse . dropWhile (==' '))
        add_del   = map (map (++[delim]))

inflate :: [String] -> [[String]]
inflate ls = transpose $ map padcol table
  where table = transpose $ map (splitOn ";") ls --list of cols
        padcol c = map (rpad m) c --pad whole col
          where m = (1+) $ maximum $ map length c --determine longest string in col, pad one more for prettiness
