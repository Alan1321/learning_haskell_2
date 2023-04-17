import System.IO
import Data.List
import Data.List.Split

horizontal_flip :: [[a]] -> [[a]]
horizontal_flip = map reverse

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
    print contents

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
    
    let start2 = ["1", "2", "3", "4", "5", "6"]

    -- print "converting rest of list to int"
    -- -- Convert rest into a list of integers (currently a list of strings)
    -- let rest_ints = toInt start2
    -- print rest_ints

    -- print "Splitting list into group of 2 elements"
    -- -- Split the list up into groups of 6 elements (the first param for divvy).
    -- -- The first element of each new group is an offset from the previous
    -- -- first element (the second 6).
    -- let new_list = divvy 2 2 rest_ints
    -- print new_list

    -- -- let reversed_list = horizontal_reverse new_list
    -- -- print new_list

    -- let start3 = [[1,2],[3,4],[5,6]]
    -- let result = horizontal_flip new_list
    -- print result

    print "converting rest of list to int"
    -- Convert rest into a list of integers (currently a list of strings)
    let rest_ints = toInt rest
    --print rest_ints

    print "Splitting list into group of 2 elements"
    -- Split the list up into groups of 6 elements (the first param for divvy).
    -- The first element of each new group is an offset from the previous
    -- first element (the second 6).
    let new_list = divvy 3 3 rest_ints
    --print new_list

    -- let reversed_list = horizontal_reverse new_list
    -- print new_list

    let result = horizontal_flip new_list
    --print result
    -- let mixed = start : result
    -- let myString = show result
    -- writeFile "horizontal.txt" myString

    -- Let's take new_list, convert back to strings, flatten it, and write it to an output file
    -- let back_to_strings = let strs = map (map show) result
    --                       in unlines (map unwords strs)
    -- writeFile "result/example_new.txt" back_to_strings

    -- let start_1 = map (map show) start
    let start_2 = unlines(first_4)

    let back_1 = map (map show) result
    let back_2 = unlines(map unwords back_1) 
    
    let output = start_2 ++ back_2
    -- print output
    -- let strs = map (map show) result
    --     res_str = unlines (map unwords strs)
    --     output = intercalate " " (start ++ unwords res_str)
    writeFile "result/horizontal_cake.txt" output

    -- let appended = start:back_to_strings
    -- writeFile "horizontal_cake.txt" appended
    -- print start
    -- print back_to_strings