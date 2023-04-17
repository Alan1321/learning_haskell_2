import System.IO
import Data.List
import Data.List.Split

-- RGB : (r, g, b)
type Pixel = (Int, Int, Int)

-- {-
--  - Receive a list of Strings wich are number
--  - Return the list but converted in a list of Ints
-- -}
-- convertToInt :: [String] -> [Int]
-- convertToInt xs = map (\x -> read x :: Int) xs

-- Receive a list fo ints and return a list of Pixels
convertToPixeis :: [Int] -> [Pixel]
convertToPixeis [] = []
convertToPixeis (a:b:c:xs) = (a,b,c) : convertToPixeis xs


-- horizontal_flip :: [()] -> [()]
-- horizontal_flip pixels = map reverse pixels

vertical_flip :: [[a]] -> [[a]]
vertical_flip = reverse


{-
 - Pixel to String with their RGB's values separeted
 - with spaces
 -}
showPixel :: Pixel -> String
showPixel (a,b,c) = show a ++ " " ++ show b ++ " " ++ show c ++ "\n"

-- --converting list of list of tuples to a single list
-- output :: [(Int, Int, Int)] -> 
-- output = concatMap (toList . fmap (\(r,g,b) -> [r,g,b]))

-- toInt: recursively convert a list of Strings into a list of ints
-- The base case is an empty list ([])
-- The recursive step splits the list into head h and tail t. Then, it
-- converts h and stores it in a list, then concatenates that with
-- the result of converting the rest of the list (our recursive call).
toInt :: [String] -> [Int]
toInt [] = []
toInt (h:t) = [read h] ++ toInt t

main = do
    -- read a text file - retuns a string
    let input_file = "test/cake.txt"
    input <- readFile input_file
    --print input

    -- The lines function splits the string by newlines and returns
    --  a list of strings, one string for each line
    let contents = lines input
    --print contents

    let first_4 = take 4 contents
    -- Split the list so each value is a separate item in the list.
    -- unwords will combine all of the strings in our current list into
    -- one string, with a space between each string, and then words 
    -- splits the string back up using whitespace.
    let flattened_contents = words (unwords contents)
    --print flattened_contents

    --store first 4 item in head and rest in tail
    let start = take 4 flattened_contents
    let rest = drop 4 flattened_contents
    --storing head
    let start_2 = unlines(first_4)

    print "converting rest of list to int"
    -- Convert rest into a list of integers (currently a list of strings)
    let rest_ints = toInt rest
    --print rest_ints

    let pixels = convertToPixeis rest_ints
    --print pixels

    -- let h_flip = reverse $ chunksOf 4 pixels
    -- print h_flip

    let chunks = chunksOf 720 pixels
    let h_flip = map reverse chunks
    -- let ff = concat h_flip
    -- print ff
    let cc = concat h_flip
    let ss = map showPixel cc
    let final_string = concat ss
    let ff = start_2 ++ final_string
    writeFile "result/horizontal_cake.ppm" ff

    let v_flip = reverse chunks
    let v_cc = concat v_flip
    let v_ss = map showPixel v_cc
    let v_final_string = concat v_ss
    let v_ff = start_2 ++ v_final_string
    writeFile "result/vertical_cake.ppm" v_ff
    

