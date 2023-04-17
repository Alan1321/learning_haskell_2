import System.IO
import Data.List
import Data.List.Split

-- This example program is exists to show you how some operations can be done 
-- on lists and how to read and write from a file. There are definitely
-- different and likely better ways to do some of this, but this 
-- aims to show you how things work in an easy to understand way.


-- toInt: recursively convert a list of Strings into a list of ints
-- The base case is an empty list ([])
-- The recursive step splits the list into head h and tail t. Then, it
-- converts h and stores it in a list, then concatenates that with
-- the result of converting the rest of the list (our recursive call).
toInt :: [String] -> [Int]
toInt [] = []
toInt (h:t) = [read h] ++ toInt t

-- addVal: add the given value (parameter val) to every element of
-- a list (parameter list).
-- map applies the operation (val +) to each item of the list.
-- Notice we don't list the 2nd operand of + because each list item
-- will be the second operand.
addVal :: Num b => b -> [b] -> [b]
addVal val list = map (val +) list

-- If the value passed in (item) is less than 0, set it to 0
-- Otherwise, add 50
-- Guards (|) are good when you have to test for multiple conditions
conditionalAdd :: (Ord a, Num a) => a -> a
conditionalAdd item
    | item < 0 = 0
    | item >= 0 = item + 50

-- Add 3 to each 3rd element of the list. Uses recursion.
-- A list can be an emply list (our base case []) or a non-empy list
-- which we split into 4 parts - the first 3 elements (a, b, and c)
-- and the remainder. This function assumes the number of elements
-- in list is a multiple of 3.
addThree :: Num a => [a] -> [a]
addThree list = 
    case list of 
        [] -> []
        a:b:c:rest -> a:b:(c+3) : addThree rest



main = do
    -- read a text file - retuns a string
    let input_file = "example.txt"
    input <- readFile input_file
    print input
    -- The lines function splits the string by newlines and returns
    --  a list of strings, one string for each line
    let contents = lines input
    print contents

    -- Split the list so each value is a separate item in the list.
    -- unwords will combine all of the strings in our current list into
    -- one string, with a space between each string, and then words 
    -- splits the string back up using whitespace.
    let flattened_contents = words (unwords contents)
    print flattened_contents

    -- Store the first 4 items in start and the remainder of the list in rest
    let start = take 4 flattened_contents
    let rest = drop 4 flattened_contents
    print "Splitting start/rest"
    print start
    print rest

    -- -- Get the second value from start (start !! 1) and convert it 
    -- -- into an integer (using read and ::Integer). 
    -- -- Note: haskell lists are 0-indexed
    -- print "converting initial value"
    -- let value1 = read (start !! 1)::Integer
    -- print value1

    print "converting rest of list to int"
    -- Convert rest into a list of integers (currently a list of strings)
    let rest_ints = toInt rest
    print rest_ints


    -- Add 3 to every 3rd item in the list
    let result = addThree rest_ints
    print result

    -- Add the same value to every item in the list
    -- You can safely reassign an expression to result as long as result
    -- does not show up on the right side of the expression
    let result = addVal 13 rest_ints
    print result

    -- Subtract each list item from 100
    let result2 = map (100 - ) result
    print result2

    -- Conditionally change the value of each item in the list
    let result3 = map conditionalAdd result2
    print result3

    print "Splitting list into group of 6 elements"
    -- Split the list up into groups of 6 elements (the first param for divvy).
    -- The first element of each new group is an offset from the previous
    -- first element (the second 6).
    let new_list = divvy 6 6 rest_ints
    print new_list

    -- Now add 3 to each 3rd element of the inner lists
    let new_list2 = map addThree new_list
    print new_list2

    -- Average the values in each sublist using integer division (want int result)
    -- Have to use div function instead of / for integer division.
    let averages = let sums = map sum new_list
                   in map (`div` 8) sums
    print averages

    -- A shorter way to do the above. Always choose the methods that make sense to you.
    let test = map ((`div` 8) . sum) new_list
    print test

    -- Let's take new_list, convert back to strings, flatten it, and write it to an output file
    let back_to_strings = let strs = map (map show) new_list2
                          in unlines (map unwords strs)
    writeFile "example_new.txt" back_to_strings


