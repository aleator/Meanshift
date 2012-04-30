{-#LANGUAGE BangPatterns#-}
module Main where
import Data.List hiding (sum)
import qualified Data.Vector.Unboxed as V
import Prelude hiding (sum)
import Control.Arrow
import MeanShift
import CV.Image
import CV.Pixelwise
import CV.Transforms
import System.Environment
import Data.Maybe
import Control.Monad

-- | Extract a pixel from an image.
sample :: (Int,Int) -> Image RGB D32 -> (Int,Int) -> Vector
sample (w,h) image (!x,!y)
   | x>0 && y>0 && x < w && y < h = let (r,g,b) = getPixel (x,y) image
                                    in V.fromList [fi x, fi y, colorScale*realToFrac r, colorScale*realToFrac g, colorScale*realToFrac b]
   | otherwise = V.fromList [fi x, fi y, 0,0,0]

-- | Extract color and coordinate information from the sample (see above.) 
coord v = let (!x):(!y):_ = V.toList v in (round x,round y)
color v = let _:_:r:g:b:_ = V.toList v in (realToFrac (b/colorScale), realToFrac (g/colorScale), realToFrac (r/colorScale))

-- | Weighting term to balance the effect between spatial and color domains.
colorScale = 50

main :: IO ()
main = do
   [fn] <- getArgs
   image <- readFromFile fn
   let -- Reading the test-data. You might want to scale it to smaller size to avoid waiting too much. 
       testData :: Image RGB D32
       testData =  image 
       (w,h) = getSize testData

       -- A windowing function that is used to extract image patches. 
       window :: Window
       window (x,s) = [sample (w,h) testData (u+j,v+i)
                      | let size  = round s
                      , i <- [-size,-size+1..size]
                      , j <- [-size,-size+1..size]
                      , let (u,v) = coord x
                      ]

       -- Application of meanshift to the image data.
       shift   = meanShiftWindow 5 window 3
       process = last . take 12 . fixedPointE 0.1 shift
       
       -- The resulting image. This will be evaluated in parallel if you compile with `-rtsopt -threaded`-options and
       -- execute the resulting program with `-RTS -N` flags
       r :: Image RGB D32
       r = toImagePar 8 $ MkP (getSize testData) (color . process . sample (w,h) testData)
   saveImage "filtered.png" r
   return ()
