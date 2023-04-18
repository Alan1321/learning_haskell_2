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

greyScale :: Pixel -> Pixel
greyScale (r, g, b) = ((r+g+b) `div` 3, (r+g+b) `div` 3, (r+g+b) `div` 3)

invert :: Pixel -> Pixel
invert (r,g,b) = (255-r, 255-g, 255-b)

flatRed :: Pixel -> Pixel
flatRed (r, g, b) = (0,g,b)

flatGreen :: Pixel -> Pixel
flatGreen (r, g, b) = (r,0,b)

flatBlue :: Pixel -> Pixel
flatBlue (r, g, b) = (r,g,0)

-- --averageRed :: [[(Int, Int, Int)]] -> [[(Int, Int, Int)]]
-- averageRed = map (map blur)

-- blur :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int, Int),(Int, Int, Int),(Int, Int, Int)]
blur [(r1, g1, b1),(r2, g2, b2),(r3, g3, b3)] = [(avg r1 r2 r3, g1, b1),(avg r1 r2 r3, g2, b2),(avg r1 r2 r3, g3, b3)] ++ blurred_list

avg :: Int -> Int -> Int -> Int
avg x y z = (x + y + z) `div` 3


-- -- Function to calculate the average RGB tuple of three consecutive tuples
-- averageRGB :: [(Int, Int, Int)] -> (Int, Int, Int)
-- averageRGB [(r1, g1, b1), (r2, g2, b2), (r3, g3, b3)] =
--   (div (r1+r2+r3) 3, div (g1+g2+g3) 3, div (b1+b2+b3) 3)


-- -- Function to perform horizontal blur on a list of lists of RGB tuples
-- horizontalBlur :: [[(Int, Int, Int)]] -> [[(Int, Int, Int)]]
-- horizontalBlur = map (map averageRGB) . map (chunksOf 3) . map concat
--   where chunksOf _ [] = []
--         chunksOf n xs = take n xs : chunksOf n (drop n xs)

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

blurred_list = [1,2,3]

main = do
    -- read a text file - retuns a string
    let input_file = "test/test2.txt"
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
    --print pixels

    -- --horizontal flip
    -- let h_flip = map reverse chunks
    -- let cc = concat h_flip
    -- let ss = map showPixel cc
    -- let final_string = concat ss
    -- let ff = start_2 ++ final_string
    -- writeFile "result/horizontal_cake.ppm" ff

    -- --vertical flip
    -- let v_flip = reverse chunks
    -- let v_cc = concat v_flip
    -- let v_ss = map showPixel v_cc
    -- let v_final_string = concat v_ss
    -- let v_ff = start_2 ++ v_final_string
    -- writeFile "result/vertical_cake.ppm" v_ff
    
    -- --greyScale
    -- let grey = map greyScale pixels
    -- let g_ss = map showPixel grey
    -- let g_final_string = concat g_ss
    -- let g_ff = start_2 ++ g_final_string
    -- writeFile "result/grey_cake.ppm" g_ff

    -- --invert colors
    -- let inverted = map invert pixels
    -- let i_ss = map showPixel inverted
    -- let i_final_string = concat i_ss
    -- let i_ff = start_2 ++ i_final_string
    -- writeFile "result/invert_cake.ppm" i_ff

    -- --flatten red
    -- let flat_red = map flatRed pixels
    -- let fr_ss = map showPixel flat_red
    -- let fr_final_string = concat fr_ss
    -- let fr_ff = start_2 ++ fr_final_string
    -- writeFile "result/flat_red_cake.ppm" fr_ff

    -- --flatten green
    -- let flat_green = map flatGreen pixels
    -- let fg_ss = map showPixel flat_green
    -- let fg_final_string = concat fg_ss
    -- let fg_ff = start_2 ++ fg_final_string
    -- writeFile "result/flat_green_cake.ppm" fg_ff

    -- --flatten blue
    -- let flat_blue = map flatBlue pixels
    -- let fb_ss = map showPixel flat_blue
    -- let fb_final_string = concat fb_ss
    -- let fb_ff = start_2 ++ fb_final_string
    -- writeFile "result/flat_blue_cake.ppm" fb_ff

    ----------------------------------------------------------
    -- let chunk3 = chunksOf 3 pixels
    -- print chunk3

    -- --let blurred = map blur chunk3
    -- print blurred_list

    print pixels
