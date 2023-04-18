import System.IO
import Data.List
import Data.List.Split
import System.Random

-- RGB : (r, g, b)
type Pixel = (Int, Int, Int)

toInt :: [String] -> [Int]
toInt [] = []
toInt (h:t) = [read h] ++ toInt t

-- Receive a list fo ints and return a list of Pixels
convertToPixeis :: [Int] -> [Pixel]
convertToPixeis [] = []
convertToPixeis (a:b:c:xs) = (a,b,c) : convertToPixeis xs

blurred_list = [1,2,3]

-- blur :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int, Int),(Int, Int, Int),(Int, Int, Int)]
blur (r1, g1, b1) (r2, g2, b2) (r3, g3, b3) = [(avg r1 r2 r3, g1, b1),(avg r1 r2 r3, g2, b2),(avg r1 r2 r3, g3, b3)]

avg :: Int -> Int -> Int -> Int
avg x y z = (x + y + z) `div` 3

switchFirst :: [[(Int, Int, Int)]] -> [[(Int, Int, Int)]]
switchFirst xs = map (\ys -> let avg = sum (map (\(x,_,_) -> x) ys) `div` length ys
                             in map (\(_,y,z) -> (avg,y,z)) ys) xs

showPixel :: Pixel -> String
showPixel (a,b,c) = show a ++ " " ++ show b ++ " " ++ show c ++ "\n"


clampRGB :: (Int, Int, Int) -> (Int, Int, Int)
clampRGB (r,g,b) = (clamp r, clamp g, clamp b)
  where
    clamp x = if x > 255 `div` 2 then 255 else 0

averageFirst :: [(Int, Int, Int)] -> [(Int, Int, Int)]
averageFirst [] = []
averageFirst [(x,y,z)] = [(x,y,z)]
averageFirst ((x1,y1,z1):(x2,y2,z2):(x3,y3,z3):rest) =
  let avg = (x1 + x2 + x3) `div` 3
  in (avg,y1,z1):(avg,y2,z2):(avg,y3,z3):(averageFirst rest)

-- averageFirst :: [(Int, Int, Int)] -> [(Int, Int, Int)]
-- averageFirst [] = []
-- averageFirst [(x,y,z)] = [(x,y,z)]
-- averageFirst ((x1,y1,z1):(x2,y2,z2):(x3,y3,z3):rest) =
--   let avg = (x1 + x2 + x3 + y1 + y2 + y3 + z1 + z2 + z3) `div` 9
--   in (avg,y1,z1):(avg,y2,z2):(avg,y3,z3):(averageFirst rest)

addRandomRGB :: (Int, Int, Int) -> IO (Int, Int, Int)
addRandomRGB (r,g,b) = do
  randomR <- randomRIO (0,255-r)
  randomG <- randomRIO (0,255-g)
  randomB <- randomRIO (0,255-b)
  let finalR = (r + randomR) `mod` 256
  let finalG = (g + randomG) `mod` 256
  let finalB = (b + randomB) `mod` 256
  return (finalR, finalG, finalB)

main = do
    -- read a text file - retuns a string
    let input_file = "test/cake.txt"
    input <- readFile input_file
    --print input

    -- The lines function splits the string by newlines and returns
    --  a list of strings, one string for each line
    let contents = lines input
    --print contents

    let first_4 = take 3 contents
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
    print start_2
    print "converting rest of list to int"
    -- Convert rest into a list of integers (currently a list of strings)
    let rest_ints = toInt rest
    --print rest_ints

    let pixels = convertToPixeis rest_ints
    --print pixels

    -- let h_flip = reverse $ chunksOf 4 pixels
    -- print h_flip

    ------------------------------------
    --trying blur--
    -- let chunks = chunksOf 3 pixels
    -- --print chunks

    -- let output = switchFirst chunks
    -- print output

    -- --horizontal flip
    -- --let h_flip = map reverse chunks
    -- let cc = concat output
    -- let ss = map showPixel cc
    -- let final_string = concat ss
    -- let ff = start_2 ++ final_string
    -- writeFile "result/blur_cake.txt" ff
    -----------------------------------------
    ----contrast working----
    -- let output = map clampRGB pixels
    -- --print output
    -- --let cc = concat output
    -- let ss = map showPixel output
    -- let final_string = concat ss
    -- let ff = start_2 ++ final_string
    -- writeFile "result/contrast_cake.ppm" ff
    ------------------------------------------
    --random  noise--
    -- output <- mapM addRandomRGB pixels
    -- print output
    -- let ss = map showPixel output
    -- let final_string = concat ss
    -- let ff = start_2 ++ final_string
    -- writeFile "result/noise_cake.ppm" ff
    -------------------------------------------
    --blur
    let output = averageFirst pixels
    -- let cc = concat output
    let ss = map showPixel output
    let final_string = concat ss
    let ff = start_2 ++ final_string
    writeFile "result/blurr2_cake.ppm" ff