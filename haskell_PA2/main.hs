--install
--sudo apt-get update
--sudo apt-get install haskell-platform -y
--ppm_test_file(http://nifty.stanford.edu/2012/guerin-image-editor/)
--ghc horizontal.hs && ./horizontal && cp result/horizontal_cake.txt /mnt/c/Users/alans/Desktop/horizontal_cake.ppm
import System.IO
import Data.List
import Data.List.Split

--ghci -> interactive console ,,, can leave with :q
--ghc --make main && ./main

-- minInt = minBound :: Int
number = 9 :: Int
list1 = [3,5,7,11]
list2 = 3:5:7:11:[]
len = length list2

indexx = list1 !! 0

evens = filter (<=5) list2
even2 = takeWhile (<=5) list2
