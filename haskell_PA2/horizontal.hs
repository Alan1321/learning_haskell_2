import System.IO
import Data.List
import Data.List.Split

horizontal_flip :: [[a]] -> [[a]]
horizontal_flip = map reverse

vertical_flip :: [[a]] -> [[a]]
vertical_flip = reverse

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
    --print start

    print "converting rest of list to int"
    -- Convert rest into a list of integers (currently a list of strings)
    let rest_ints = toInt rest
    --print rest_ints

    print "Splitting list into group of 2 elements"
    -- Split the list up into groups of 6 elements (the first param for divvy).
    -- The first element of each new group is an offset from the previous
    -- first element (the second 6).
    let new_list = divvy 720 720 rest_ints

    let flipping_horizontally = horizontal_flip new_list
    let flipping_vertically = vertical_flip new_list

    let start_2 = unlines(first_4)

    let h_1 = map (map show) flipping_horizontally
    let h_2 = unlines(map unwords h_1) 
    
    let v_1 = map (map show) flipping_vertically
    let v_2 = unlines(map unwords v_1) 

    let horizontal_output = start_2 ++ h_2
    writeFile "result/horizontal_cake.ppm" horizontal_output

    let vertical_output = start_2 ++ v_2
    writeFile "result/vertical_cake.ppm" vertical_output