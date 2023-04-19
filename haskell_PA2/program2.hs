{-
    Name: Alan Subedi
    Date: 04/18/2023
    Course: CS424-01
    Description: This program reads in a ppm file and performs the following conversions on it: horizontal_flip, vertical_flip, greyScale, InvertColor, flatRed, flatGreen, flatBlue
-}

import System.IO
import System.Directory
import Data.List
import Data.List.Split

--defining a Pixel
type Pixel = (Int, Int, Int)

--function converts a list of ints to list of Pixels -> [(int, int, int),(int, int, int),(int, int, int)]
intToPixel :: [Int] -> [Pixel]
intToPixel [] = []
intToPixel (x:y:z:rest) = (x,y,z) : intToPixel rest

-- toInt: recursively convert a list of Strings into a list of ints
-- The base case is an empty list ([])
-- The recursive step splits the list into head h and tail t. Then, it
-- converts h and stores it in a list, then concatenates that with
-- the result of converting the rest of the list (our recursive call).
toInt :: [String] -> [Int]
toInt [] = []
toInt (h:t) = [read h] ++ toInt t

--function which receives a tuple of r,g,b values and converts that into a string
pixelToString :: Pixel -> String
pixelToString (r,g,b) = show r ++ " " ++ show g ++ " " ++ show b ++ "\n"

--function receives a pixel data and then averages out the r,g,b values
greyScale :: Pixel -> Pixel
greyScale (r, g, b) = ((r+g+b) `div` 3, (r+g+b) `div` 3, (r+g+b) `div` 3)

--function receives a pixel data and then subtracts each r,g,b value from 255
invertImage :: Pixel -> Pixel
invertImage (r,g,b) = (255-r, 255-g, 255-b)

--function receives a pixel data, assigns R to 0 and returns back pixel
flatRed :: Pixel -> Pixel
flatRed (r, g, b) = (0,g,b)

--function receives a pixel data, assigns G to 0 and returns back pixel
flatGreen :: Pixel -> Pixel
flatGreen (r, g, b) = (r,0,b)

--function receives a pixel data, assigns B to 0 and returns back pixel
flatBlue :: Pixel -> Pixel
flatBlue (r, g, b) = (r,g,0)

--function get a pixel data, header string, and outfile location, performs operations on it and then writes out data to a file
writeOutToFile :: [Pixel] -> String -> String -> IO ()
writeOutToFile pixels header outfile_location = do
    let tuple_to_string = map pixelToString pixels --converting pixel tuple back to string of 3 points
    let string_list_to_string = concat tuple_to_string --converting list of strings of data to a single string separated by \n
    let final_data = header ++ string_list_to_string --attaching the header to pixel data
    writeFile outfile_location final_data --writing out to a file

main = do
    -- read a text file - retuns a string
    let input_filename = "cake.ppm"
    input <- readFile input_filename
    
    -- The lines function splits the string by newlines and returns
    --  a list of strings, one string for each line
    let contents = lines input
    
    -- Split the list so each value is a separate item in the list.
    -- unwords will combine all of the strings in our current list into
    -- one string, with a space between each string, and then words 
    -- splits the string back up using whitespace.
    let flattened_contents = words (unwords contents)
    
    --store first 3 item in head and rest in tail
    let start = take 3 contents
    let rest = drop 4 flattened_contents
    
    --access the column from header and convert it to Integer
    let start_array = take 3 flattened_contents
    let column = start_array !! 1
    let columnInt = read column :: Int

    --converting start data from list of strings to just a single string
    let header = unlines(start)
    
    -- Convert rest into a list of integers (currently a list of strings)
    let rest_ints = toInt rest

    --Converting each 3 integers in the array to a tuple of Pixle in the formant of (Int, Int, Int)
    let pixels = intToPixel rest_ints

    --chunking each three Pixels into a list -> will be used for Horizontal and Vertical Flip 
    let chunks = chunksOf columnInt pixels

    --making a Directory that will store all the results
    let directory_name = "results_subedi_alan_424"
    createDirectory directory_name
    putStrLn ""
    -------------------------------------------------------------------------------------------

    --horizontal flip
    let h_flip = map reverse chunks --performing the horizontal flip
    let h_single_list_pixel = concat h_flip --converting list of list of pixels to just a single list of pixels
    writeOutToFile h_single_list_pixel header (directory_name ++ "/" ++ input_filename ++ "_hflip.ppm") --function call to write out to a file
    print "Horizontal Flip...Complete"

    --vertical flip
    let v_flip = reverse chunks
    let v_single_list_pixel = concat v_flip
    writeOutToFile v_single_list_pixel header (directory_name ++ "/" ++ input_filename ++ "_vflip.ppm")
    print "Vertical Flip...Complete"

    --greyScale
    let greyScaling = map greyScale pixels
    writeOutToFile greyScaling header (directory_name ++ "/" ++ input_filename ++ "_greyscale.ppm")
    print "Gray Scale...Complete"

    --invert colors
    let inverted = map invertImage pixels
    writeOutToFile inverted header (directory_name ++ "/" ++ input_filename ++ "_inverted.ppm")
    print "Inverted Colors...Complete"

    --flatten red
    let flat_red = map flatRed pixels
    writeOutToFile flat_red header (directory_name ++ "/" ++ input_filename ++ "_nored.ppm")
    print "Flatten Red...Complete"

    --flatten green
    let flat_green = map flatGreen pixels
    writeOutToFile flat_green header (directory_name ++ "/" ++ input_filename ++ "_nogreen.ppm")
    print "Flatten Green...Complete"

    --flatten blue
    let flat_blue = map flatBlue pixels
    writeOutToFile flat_blue header (directory_name ++ "/" ++ input_filename ++ "_noblue.ppm")
    print "Flatten Blue...Complete"

    let output = ( "\nAll Operations Complete. Results are Stored in Directory : " ++ (directory_name) ++ "/\n")
    putStrLn output
