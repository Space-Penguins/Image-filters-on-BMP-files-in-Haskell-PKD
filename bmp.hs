import Codec.BMP
import qualified Data.ByteString as BS
import Data.Array
import GHC.Word
import qualified Data.ByteString.Char8
import Data.Tuple
import Test.HUnit

{- Representation Convention: Picture represents the entire photo as a list with Word8 elements.
   Representation Invariant: Length of Picture must be equal to or bigger than 4 and must have an increment of 4.
-}
type Picture = [Word8]

{- Representation Convention: Pixel represents each pixel as a list with Word8 elements.
   Representation Invariant: Length of Pixel must be equal to 4.
-}
type Pixel = [Word8]

{- Representation Convention: PixelBox represents all the pixels surrounding a pixel including the pixel itself as a list with Word8 elements.
   Representation Invariant: Length of PixelBox must be equal to or bigger than 32.
-}
type PixelBox = [Word8]

{- Representation Convention: PixelPosition represents the index of a given pixel in an array as a double where both values are Int.
   Representation Invariant: Both values in PixelPosition must be equal to or bigger than 1.
-}
type PixelPosition = (Int,Int)

{- Representation Convention: Dimensions represents the width and height of an image as a double where both values are Int.
   Representation Invariant: Both values in Dimensions must be equal to or bigger than 1.
-}
type Dimensions = (Int, Int)

{- Representation Convention: Filter represents the name of which filter the user wants to use as a String.
   Representation Invariant: The value of Filter must exist either in filterList or in pixelMoverList.
-}
type Filter = String

{- filterList
   Association lists with key-value pairs.
   All filter that can be applied to a single pixel.
-}
filterList :: [(Filter, PixelPosition -> Dimensions -> Array Dimensions Picture -> Pixel)]
filterList = [ ("1", greyScale)
             , ("2", boxBlur)
             , ("3", brighten) 
             ]

{- pixelMoverList
   Association lists with key-value pairs.
   All filter that needs to work with the whole array.
-}
pixelMoverList :: [(Filter, Dimensions -> Array Dimensions Picture -> Array Dimensions Picture)]
pixelMoverList = [   ("1", flipY)  
                  ,  ("2", flipX)
                  ,  ("3", flipXY)
                  ,  ("4", rotate90R)
                  ,  ("5", rotate90L)
                  ,  ("6", rotate180)
                  ]

{- main
   Promt the user to specify an image and what type of filter to apply.
   RETURNS: Function loadFileFilter or loadFilePixelMover depending on user input.
   SIDE EFFECTS: Print strings to the terminal, input from terminal and loads a BMP file to memmory.
-}
main :: IO ()
main = do
    putStr "Enter a file path to a BMP file to work with: "
    file <- getLine
    putStrLn "Do you want to add a filter or rotate/flip the image?"
    putStrLn "1. Filter\n2. Rotate/Flip"
    choice <- getLine
    if choice == "1"
      then loadFileFilter file
      else if choice == "2"
            then loadFilePixelMover file
            else error "Choose between 1 or 2."

{- loadFileFilter f
   Promt the user to choose from a list of filters.
   RETURNS: saveFile
   SIDE EFFECTS: Print strings to the terminal, input from terminal and convert the BMP file to memmory ByteString and Word8.
-}
loadFileFilter :: FilePath -> IO ()
loadFileFilter file = do
    Right bmp <- readBMP file
    putStrLn "What filter do you want to apply to the image?"
    putStrLn "1. Grey scale\n2. Box blur\n3. Brighten"
    filter <- getLine
    let dimensions = bmpDimensions bmp                            -- Get the width and height of the image.
        rgba       = unpackBMPToRGBA32 bmp                        -- Unpack the BMP to a ByteString.
        unpacked   = BS.unpack rgba                               -- Unpack the ByteString to a list of Word8.
        pArray     = pixelArray dimensions unpacked               -- Convert the list of Word8 into an 2D array.
        newArray'  = newArray filter dimensions pArray            -- Make a new array form the original with an applied filter
        newBMP     = concat $ arrayToPicture dimensions newArray' -- Convert the 2D array to a list of Word8.
        newRGBA    = BS.pack newBMP                               -- Pack the Word8 list to a ByteString.
    saveFile newRGBA  dimensions

{- loadFilePixelMover f
   Promt the user to choose from a list of filters.
   RETURNS: saveFile
   SIDE EFFECTS: Print strings to the terminal, input from terminal and convert the BMP file to memmory ByteString and Word8.
-}
loadFilePixelMover :: FilePath -> IO ()
loadFilePixelMover file = do
    Right bmp <- readBMP file
    putStrLn "What rotate/flip do you want to apply to the image?"
    putStrLn "1. Flip X\n2. Flip Y\n3. Flip XY\n4. Rotate 90 Right\n5. Rotate 90 Left\n6. Rotate 180"
    filter <- getLine
    let (Just filter') = lookup filter pixelMoverList
        dimensions     = bmpDimensions bmp                                     -- Get the width and height of the image.
        rgba           = unpackBMPToRGBA32 bmp                                 -- Unpack the BMP to a ByteString.
        unpacked       = BS.unpack rgba                                        -- Unpack the ByteString to a list of Word8.
        pArray         = pixelArray dimensions unpacked                        -- Transforms the list of Word8 into an 2D array.
        newArray'      = filter' dimensions pArray                             -- Make a new array form the original with an applied filter
        newDimensions  = bounds newArray'                                      -- Get the width and height of the new array.
        newBMP         = concat $ arrayToPicture (snd newDimensions) newArray' -- Transforms the 2D array to a list of Word8.
        newRGBA        = BS.pack newBMP                                        -- Pack the Word8 list to a ByteString.
    saveFile newRGBA (snd newDimensions)

{- pixelArray d pl
   Transforms a list of Word8 into an 2D array.
   RETURNS: pl as an 2D array.
   EXAMPLES: pixelArray (2,2) [1,1,0,255,2,1,0,255,1,2,0,255,2,2,0,255]
                     == array ((1,1),(2,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255])]
             pixelArray (6,1) [1,1,0,255,2,1,0,255,3,1,0,255,4,1,0,255,5,1,0,255,6,1,0,255] 
                     == array ((1,1),(6,1)) [((1,1),[1,1,0,255]),((2,1),[2,1,0,255]),((3,1),[3,1,0,255]),((4,1),[4,1,0,255]),((5,1),[5,1,0,255]),((6,1),[6,1,0,255])]
-}
pixelArray :: Dimensions -> Picture -> Array Dimensions Picture
pixelArray (x,y) pl = array ((1,1),(x,y)) [((i,j), take 4 (drop (4*((j*x-(x-i))-1)) pl)) | j <- [1..y], i <- [1..x]]

{- getPixelBox pp d array
   Finds all the pixels surrounding a pixel including the pixel itself.
   PRE: d must be the same as the highest index value in array.
        Arrays dimensions upper values must be equal to or bigger than 2.
   RETURNS: An array with all the values surrounding pp in pArray including the pp itself.
   EXAMPLES: getPixelBox (1,1) (2,2) (array ((1,1),(2,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255])])
                                   == array ((1,1),(2,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255])]
             getPixelBox (1,1) (2,2) (array ((1,1),(2,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255])])
                                   == array ((1,1),(2,2)) [((1,1),[2,2,0,255]),((1,2),[2,1,0,255]),((2,1),[1,2,0,255]),((2,2),[1,1,0,255])]
-}
getPixelBox :: PixelPosition -> Dimensions -> Array Dimensions Picture -> Array Dimensions Picture
getPixelBox (i,j) (x,y) pArray | i == 1 && j == 1 = array ((1,1),(2,2)) [((p,q), pArray ! (p,q))             | p <- [1..2], q <- [1..2]] -- Gets a 2x2 box from the lower left corner.
                               | i == 1 && j == y = array ((1,1),(2,2)) [((p,q), pArray ! (p,y-(q-1)))       | p <- [1..2], q <- [1..2]] -- Gets a 2x2 box from the upper left corner.
                               | i == x && j == 1 = array ((1,1),(2,2)) [((p,q), pArray ! (x-(p-1),q))       | p <- [1..2], q <- [1..2]] -- Gets a 2x2 box from the lower right corner.
                               | i == x && j == y = array ((1,1),(2,2)) [((p,q), pArray ! (x-(p-1),y-(q-1))) | p <- [1..2], q <- [1..2]] -- Gets a 2x2 box from the upper right corner.
                               | i == 1           = array ((1,1),(2,3)) [((p,q), pArray ! (p,j-(2-q)))       | p <- [1..2], q <- [1..3]] -- Gets a 2x3 box from the left edge surrounding the pixel (1,j).
                               | j == 1           = array ((1,1),(3,2)) [((p,q), pArray ! (i-(2-p),q))       | p <- [1..3], q <- [1..2]] -- Gets a 3x2 box from the bottom row surrounding the pixel (i,1).
                               | i == x           = array ((1,1),(2,3)) [((p,q), pArray ! (x-(2-p),j-(2-q))) | p <- [1..2], q <- [1..3]] -- Gets a 2x3 box from the right edge surrounding the pixel (x,j).
                               | j == y           = array ((1,1),(3,2)) [((p,q), pArray ! (i-(2-p),y-(2-q))) | p <- [1..3], q <- [1..2]] -- Gets a 3x2 box from the top row surrounding the pixel (i,y).
                               | otherwise        = array ((1,1),(3,3)) [((p,q), pArray ! (i-(2-p),j-(2-q))) | p <- [1..3], q <- [1..3]] -- Gets a 3x3 box surrounding the pixel (i,j).

{- newArray filter d array
   Builds a new array with filter applied to the picture.
   PRE: The filter chosen has to be one of the filters inside filterList or pixelMoverList.
        d must be the same as the highest index value in array.
   RETURNS: An array with filter applied to each pixel.
   EXAMPLES: newArray "1" (2,2) (array ((1,1),(2,2)) [((1,1),[255,0,0,255]),((1,2),[0,0,255,255]),((2,1),[0,255,0,255]),((2,2),[255,255,255,255])])
                              == array ((1,1),(2,2)) [((1,1),[85,85,85,255]),((1,2),[25,25,25,255]),((2,1),[127,127,127,255]),((2,2),[237,237,237,255])]
-}
newArray :: Filter -> Dimensions -> Array Dimensions Picture -> Array Dimensions Picture
newArray filter (x,y) orgArray = array ((1,1),(x,y)) [((i,j), filter' (i,j) (x,y) orgArray) | j <- [1..y], i <- [1..x]]
  where (Just filter') = lookup filter filterList

{- arrayToPicture d array
   Creates a picture out of array with d as it's dimensions.
   PRE: d must be the same as the highest index value in array.
   RETURNS: All values from array as elements in a list.
   EXAMPLES: arrayToPicture (2,2) array ((1,1),(2,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255])]
                               == [[1,1,0,255],[2,1,0,255],[1,2,0,255],[2,2,0,255]]
-}
arrayToPicture :: Dimensions -> Array Dimensions Picture -> [Picture]
arrayToPicture (x,y) p = [p ! (i,j) | j <- [1..y], i <- [1..x]]

{- saveFile bs d
   Saves the file into a .bmp picture with d dimensions.
   RETURNS: 
-}
saveFile :: BS.ByteString -> Dimensions -> IO ()
saveFile rgba (x,y) = do
    let bmp = packRGBA32ToBMP x y rgba                                                    -- Pack the ByteString to an image.
    putStrLn "What name do you want for your new image? Don't forget to end it with .bmp"
    name <- getLine
    writeBMP name bmp                                                                     -- Saves the image in a file.

{- flipY d array
   Flips the picture on its x-axis.
   PRE: d must be the same as the highest index value in array.
   RETURNS: All values from array fliped on its x-axis in a new array..
   EXAMPLES: flipY (2,2) (array ((1,1),(2,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255])])
                       == array ((1,1),(2,2)) [((1,1),[1,2,0,255]),((1,2),[1,1,0,255]),((2,1),[2,2,0,255]),((2,2),[2,1,0,255])]
             flipY (2,3) (array ((1,1),(2,3)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((1,3),[1,3,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((2,3),[2,3,0,255])])
                       == array ((1,1),(2,3)) [((1,1),[1,3,0,255]),((1,2),[1,2,0,255]),((1,3),[1,1,0,255]),((2,1),[2,3,0,255]),((2,2),[2,2,0,255]),((2,3),[2,1,0,255])]
             flipY (3,2) (array ((1,1),(3,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((3,1),[3,1,0,255]),((3,2),[3,2,0,255])])
                       == array ((1,1),(3,2)) [((1,1),[1,2,0,255]),((1,2),[1,1,0,255]),((2,1),[2,2,0,255]),((2,2),[2,1,0,255]),((3,1),[3,2,0,255]),((3,2),[3,1,0,255])]
-}
flipY :: Dimensions -> Array Dimensions Picture -> Array Dimensions Picture
flipY (x,y) orgArray = array ((1,1),(x,y)) [((i,y-(j-1)), orgArray ! (i,j)) | j <- [1..y], i <- [1..x]]

{- flipX d array
   Flips the picture on its y-axis.
   PRE: d must be the same as the highest index value in array.
   RETURNS: All values from array fliped on its y-axis in a new array..
   EXAMPLES: flipX (2,2) (array ((1,1),(2,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255])])
                       == array ((1,1),(2,2)) [((1,1),[2,1,0,255]),((1,2),[2,2,0,255]),((2,1),[1,1,0,255]),((2,2),[1,2,0,255])]
             flipX (2,3) (array ((1,1),(2,3)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((1,3),[1,3,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((2,3),[2,3,0,255])])
                       == array ((1,1),(2,3)) [((1,1),[2,1,0,255]),((1,2),[2,2,0,255]),((1,3),[2,3,0,255]),((2,1),[1,1,0,255]),((2,2),[1,2,0,255]),((2,3),[1,3,0,255])]
             flipX (3,2) (array ((1,1),(3,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((3,1),[3,1,0,255]),((3,2),[3,2,0,255])])
                       == array ((1,1),(3,2)) [((1,1),[3,1,0,255]),((1,2),[3,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((3,1),[1,1,0,255]),((3,2),[1,2,0,255])]
-}
flipX :: Dimensions -> Array Dimensions Picture -> Array Dimensions Picture
flipX (x,y) orgArray = array ((1,1),(x,y)) [((x-(i-1),j), orgArray ! (i,j)) | j <- [1..y], i <- [1..x]]

{- flipXY d array
   Flips the picture on its x and y-axis.
   PRE: d must be the same as the highest index value in array.
   RETURNS: All values from array fliped on its x and y-axis in a new array.
   EXAMPLES: flipXY (2,2) (array ((1,1),(2,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255])])
                        == array ((1,1),(2,2)) [((1,1),[2,2,0,255]),((1,2),[2,1,0,255]),((2,1),[1,2,0,255]),((2,2),[1,1,0,255])]
             flipXY (2,3) (array ((1,1),(2,3)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((1,3),[1,3,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((2,3),[2,3,0,255])])
                        == array ((1,1),(2,3)) [((1,1),[2,3,0,255]),((1,2),[2,2,0,255]),((1,3),[2,1,0,255]),((2,1),[1,3,0,255]),((2,2),[1,2,0,255]),((2,3),[1,1,0,255])]
             dlipXY (3,2) (array ((1,1),(3,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((3,1),[3,1,0,255]),((3,2),[3,2,0,255])])
                        == array ((1,1),(3,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((3,1),[3,1,0,255]),((3,2),[3,2,0,255])]
-}
flipXY :: Dimensions -> Array Dimensions Picture -> Array Dimensions Picture
flipXY (x,y) orgArray = array ((1,1),(x,y)) [((x-(i-1),y-(j-1)), orgArray ! (i,j)) | j <- [1..y], i <- [1..x]]

{- rotate90R d array
   Rotates the picture 90 degrees to the right.
   PRE: d must be the same as the highest index value in array.
   RETURNS: A new array with all the values from array rotated 90 degrees to the right.
   EXAMPLES: rotate90R (2,2) (array ((1,1),(2,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255])])
                           == array ((1,1),(2,2)) [((1,1),[2,1,0,255]),((1,2),[1,1,0,255]),((2,1),[2,2,0,255]),((2,2),[1,2,0,255])]
             rotate90R (2,3) (array ((1,1),(2,3)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((1,3),[1,3,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((2,3),[2,3,0,255])])
                           == array ((1,1),(3,2)) [((1,1),[2,1,0,255]),((1,2),[1,1,0,255]),((2,1),[2,2,0,255]),((2,2),[1,2,0,255]),((3,1),[2,3,0,255]),((3,2),[1,3,0,255])]
             rotate90R (2,2) (array ((1,1),(3,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((3,1),[3,1,0,255]),((3,2),[3,2,0,255])])
                           == array ((1,1),(2,3)) [((1,1),[3,1,0,255]),((1,2),[2,1,0,255]),((1,3),[1,1,0,255]),((2,1),[3,2,0,255]),((2,2),[2,2,0,255]),((2,3),[1,2,0,255])]
-}
rotate90R :: Dimensions -> Array Dimensions Picture -> Array Dimensions Picture
rotate90R (x,y) orgArray = array ((1,1),(y,x)) [((j,x-(i-1)), orgArray ! (i,j)) | j <- [1..y], i <- [1..x]]                         

{- rotate90L d array
   Rotates the picture 90 degrees to the left.
   PRE: d must be the same as the highest index value in array.
   RETURNS: A new array with all the values from array rotated 90 degrees to the left.
   EXAMPLES: rotate90L (2,2) (array ((1,1),(2,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255])])
                           == array ((1,1),(2,2)) [((1,1),[1,2,0,255]),((1,2),[2,2,0,255]),((2,1),[1,1,0,255]),((2,2),[2,1,0,255])]
             rotate90L (2,3) (array ((1,1),(2,3)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((1,3),[1,3,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((2,3),[2,3,0,255])])
                           == array ((1,1),(3,2)) [((1,1),[1,3,0,255]),((1,2),[2,3,0,255]),((2,1),[1,2,0,255]),((2,2),[2,2,0,255]),((3,1),[1,1,0,255]),((3,2),[2,1,0,255])]
             rotate90L (3,2) (array ((1,1),(3,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((3,1),[3,1,0,255]),((3,2),[3,2,0,255])])
                           == array ((1,1),(2,3)) [((1,1),[1,2,0,255]),((1,2),[2,2,0,255]),((1,3),[3,2,0,255]),((2,1),[1,1,0,255]),((2,2),[2,1,0,255]),((2,3),[3,1,0,255])]
-}
rotate90L :: Dimensions -> Array Dimensions Picture -> Array Dimensions Picture
rotate90L (x,y) orgArray = array ((1,1),(y,x)) [((y-(j-1),i), orgArray ! (i,j)) | j <- [1..y], i <- [1..x]]

{- rotate180 d array
   Rotates the picture 180 degrees.
   PRE: d must be the same as the highest index value in array.
   RETURNS: A new array with all the values from array rotated 180 degrees.
   EXAMPLES: rotate180 (2,2) (array ((1,1),(2,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255])])
                           == array ((1,1),(2,2)) [((1,1),[2,2,0,255]),((1,2),[2,1,0,255]),((2,1),[1,2,0,255]),((2,2),[1,1,0,255])]
             rotate180 (2,3) (array ((1,1),(2,3)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((1,3),[1,3,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((2,3),[2,3,0,255])])
                           == array ((1,1),(2,3)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((1,3),[1,3,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((2,3),[2,3,0,255])]
             rotate180 (3,2) (array ((1,1),(3,2)) [((1,1),[1,1,0,255]),((1,2),[1,2,0,255]),((2,1),[2,1,0,255]),((2,2),[2,2,0,255]),((3,1),[3,1,0,255]),((3,2),[3,2,0,255])])
                           == array ((1,1),(3,2)) [((1,1),[3,2,0,255]),((1,2),[3,1,0,255]),((2,1),[2,2,0,255]),((2,2),[2,1,0,255]),((3,1),[1,2,0,255]),((3,2),[1,1,0,255])]
-}
rotate180 :: Dimensions -> Array Dimensions Picture -> Array Dimensions Picture
rotate180 d orgArray = rotate90R (swap d) (rotate90R d orgArray) 

{- greyScale pp d array
   Greyscales a pixel.
   PRE: pp must be a index in array.
   RETURNS: A greyscaled Pixel.
   EXAMPLES: greyScale (1,1) (2,2) (array ((1,1),(2,2)) [((1,1),[255,0,0,255]),((1,2),[0,0,255,255]),((2,1),[0,255,0,255]),((2,2),[255,255,255,255])])
                                == [85,85,85,255]
-}
greyScale :: PixelPosition -> Dimensions -> Array Dimensions Picture -> Pixel
greyScale (i,j) _ orgArray = 
  let greyScale' [r, g, b, a] = [avg, avg, avg, a]
        where avg = div r 3 + div g 2 + div b 10 --The average for all colors using the lumonosity method.
  in  greyScale' (orgArray ! (i,j))
  
  
{- brighten pp d array
   Makes a pixel brighter.
   PRE: pp must be a index in array.
   RETURNS: A brighter Pixel.
   EXAMPLES: brighten (1,1) (2,2) (array ((1,1),(2,2)) [((1,1),[220,60,45,30]),((1,2),[0,0,255,255]),((2,1),[20,210,145,0]),((2,2),[0,70,100,100])])
                                == [255,110,95,30]
-}
brighten :: PixelPosition -> Dimensions -> Array Dimensions Picture -> Pixel
brighten (i,j) _ orgArray =
  let brighten' [r, g, b, a] = [newR, newG, newB, a]
              where newR | r < 205 = r+50
                         | otherwise = 255
                    newG | g < 205 = g+50
                         | otherwise = 255
                    newB | b < 205 = b + 50
                         | otherwise = 255
  in brighten' (orgArray ! (i,j))

{- boxBlur pp d array
   Change a pixels R,G,B,A values depending on the average values surrounding the pixel including the pixel itself, creating a blur effect.
   PRE: pp must be a index in array, d must be the same as the highest index value in array.
        Arrays dimensions upper values must be equal to or bigger than 2.
   RETURNS: A Pixel with the average R,G,B and A values from the surrounding the Pixels including the pixel itself.
   EXAMPLES: boxBlur (1,1) (2,2) (array ((1,1),(2,2)) [((1,1),[255,0,0,255]),((1,2),[0,0,255,255]),((2,1),[0,255,0,255]),((2,2),[255,255,255,255])])
                              == [127,127,127,255]
-}
boxBlur :: PixelPosition -> Dimensions -> Array Dimensions Picture -> Pixel
boxBlur (i,j) (x,y) pArray =  getAverageColorValue $ concat $ elems $ getPixelBox (i,j) (x,y) pArray

{- getAverageColorValue pb p
   Get the average color value of list of Pixels.
   PRE: length of pb must be equal to or bigger than 4.
   RETURNS: A Pixel with the average R,G,B and A values from pb.
   EXAMPLES: getAverageColorValue [255,0,0,255,0,0,255,255,0,255,0,255,255,255,255,255] == [127,127,127,255]
-}
getAverageColorValue :: PixelBox -> Pixel
-- VARIANT: length pb
getAverageColorValue pb = 
  let 
    getAverageColorValue' [] r' g' b' a' n             = [toEnum (div r' n), toEnum (div g' n), toEnum (div b' n), toEnum (div a' n)]
    getAverageColorValue' (r:g:b:a:rest) r' g' b' a' n = getAverageColorValue' rest (fromEnum r+r') (fromEnum g+g') (fromEnum b+b') (fromEnum a+a') (n+1)
  in
    getAverageColorValue' pb 0 0 0 0 0 -- Takes 5 '0' as argument so they can be used as accumulators.

--------------------TEST CASES-------------------

--Creating a few variables for testing edge cases of different filter functions using photos of 2x2, 2x3 or 3x2 pixels.
a = flipY (2,2) (array ((1,1),(2,2)) [((1,1),[218,25,33,255]),((1,2),[233,27,35,255]),((2,1),[224,26,34,255]),((2,2),[224,26,34,255])])
b = array ((1,1),(2,2)) [((1,1),[233,27,35,255]),((1,2),[218,25,33,255]),((2,1),[224,26,34,255]),((2,2),[224,26,34,255])]


test1 = TestCase $ assertEqual "a" b a --Testing flipY with a 2x2 photo.

c = flipY (2,3) (array ((1,1),(2,3)) [((1,1),[101,11,15,255]),((1,2),[145,16,22,255]),((1,3),[93,10,14,255]),((2,1),[71,8,10,255]),((2,2),[74,8,11,255]),((2,3),[0,0,0,255])])
d = array ((1,1),(2,3)) [((1,1),[93,10,14,255]),((1,2),[145,16,22,255]),((1,3),[101,11,15,255]),((2,1),[0,0,0,255]),((2,2),[74,8,11,255]),((2,3),[71,8,10,255])]

test2 = TestCase $ assertEqual "c" d c --Testing flipY with a 2x3 photo.

e = flipX (3,2) (array ((1,1),(3,2)) [((1,1),[254,254,254,255]),((1,2),[232,232,232,255]),((2,1),[229,229,229,255]),((2,2),[219,219,219,255]),((3,1),[219,219,219,255]),((3,2),[232,232,232,255])])
f = array ((1,1),(3,2)) [((1,1),[219,219,219,255]),((1,2),[232,232,232,255]),((2,1),[229,229,229,255]),((2,2),[219,219,219,255]),((3,1),[254,254,254,255]),((3,2),[232,232,232,255])]

test3 = TestCase $ assertEqual "e" f e --Testing flipX with a 3x2 photo.

g = rotate90R (2,3) (array ((1,1),(2,3)) [((1,1),[101,11,15,255]),((1,2),[145,16,22,255]),((1,3),[93,10,14,255]),((2,1),[71,8,10,255]),((2,2),[74,8,11,255]),((2,3),[0,0,0,255])])
h = array ((1,1),(3,2)) [((1,1),[71,8,10,255]),((1,2),[101,11,15,255]),((2,1),[74,8,11,255]),((2,2),[145,16,22,255]),((3,1),[0,0,0,255]),((3,2),[93,10,14,255])]

test4 = TestCase $ assertEqual "g" h g --Testing rotate90R with a 2x3 photo.

i = rotate90L (2,3) (array ((1,1),(2,3)) [((1,1),[101,11,15,255]),((1,2),[145,16,22,255]),((1,3),[93,10,14,255]),((2,1),[71,8,10,255]),((2,2),[74,8,11,255]),((2,3),[0,0,0,255])])
j = array ((1,1),(3,2)) [((1,1),[93,10,14,255]),((1,2),[0,0,0,255]),((2,1),[145,16,22,255]),((2,2),[74,8,11,255]),((3,1),[101,11,15,255]),((3,2),[71,8,10,255])]

test5 = TestCase $ assertEqual "i" j i --Testing rotate90L with a 2x3 photo.

k = rotate180 (2,2) (array ((1,1),(2,2)) [((1,1),[218,25,33,255]),((1,2),[233,27,35,255]),((2,1),[224,26,34,255]),((2,2),[224,26,34,255])])
l = array ((1,1),(2,2)) [((1,1),[224,26,34,255]),((1,2),[224,26,34,255]),((2,1),[233,27,35,255]),((2,2),[218,25,33,255])]

test6 = TestCase $ assertEqual "k" l k --Testing rotate180 with a 2x2 photo.
test7 = TestCase $ assertEqual "greyScale (1,1) (1,1) (array ((1,1),(1,1)) [((1,1),[218,25,33,255])])" [87,87,87,255] (greyScale (1,1) (1,1) (array ((1,1),(1,1)) [((1,1),[218,25,33,255])])) --Testing greyScale filter on a pixel with [R, G, B] values as [218, 25, 33]
test8 = TestCase $ assertEqual "boxBlur (1,1) (2,2) (array ((1,1),(2,2)) [((1,1),[14,200,49,255]),((1,2),[56,56,255,255]),((2,1),[43,69,42,255]),((2,2),[255,67,255,255])])" [92,98,150,255] (boxBlur (1,1) (2,2) (array ((1,1),(2,2)) [((1,1),[14,200,49,255]),((1,2),[56,56,255,255]),((2,1),[43,69,42,255]),((2,2),[255,67,255,255])]))
test9 = TestCase $ assertEqual "brighten (1,1) (1,1) (array ((1,1),(1,1)) [((1,1),[218,25,30,127])])" [255,75,80,127] (brighten (1,1) (1,1) (array ((1,1),(1,1)) [((1,1),[218,25,30,127])]))

runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6, test7, test8, test9] --Running all tests by typing runtests in the Terminal.