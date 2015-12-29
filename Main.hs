import Data.List
import Data.List.Split
import System.Environment
main = do
  args <- getArgs
  file <- readFile (args !! 0)
  let lp = preprocess (lines file)
  let erg = (if elem "inflate" args then (inflate) else (compress)) lp
  mapM_ putStrLn $ map concat erg

rpad len string = take len $ string++(repeat ' ')

--ensure last col has elements
preprocess lines = map (rpad len) lines
             where len = maximum $ map (length) lines

--calc lengths of the chunks(colname + spaces) for splitplaces
dissect line = zipWith (\a b->length (a++b)) names spaces
          where names = wordsBy (==' ') line
                spaces= wordsBy (/=' ') (line++" ") --add " " to ensure space after last col-name

compress lines = add_del $ transpose $ map (shrinkcol) cols
            where delim       = ';'
                  head_lens   = dissect (head lines)
                  cols        = transpose $ map (splitPlaces head_lens) lines
                  shrinkcol c = map (\el -> reverse $ dropWhile (==' ') $ reverse $ dropWhile (==' ') el) c
                  add_del     = map (map (++[delim]))

inflate lines = transpose $ map padcol $ table
          where table = transpose $ map (splitOn ";") lines --list of cols
                padcol c = map (rpad m) c --pad whole col
                   where m = (1+) $ maximum $ (map length) c --determine longest string in col, pad one more for prettiness
