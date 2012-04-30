{-#LANGUAGE BangPatterns, ParallelListComp#-}
-- | This module presents a basic version of the meanshift algorithm for
-- feature-space analysis. Mean shifting is an iterative process with
-- fixed points that correspond to 
-- modes of kernel density estimate performed
-- with the same bandwidth (first parameter). This 
-- can be used to, for example, to partition the data by
-- determining which fixed point each of the samples belongs to.
-- 
-- Usage example:
--  > fixedPointE 0.001 (meanShift 0.1 points) (V.fromList [1,1,1])
--
--  More examples can be found in the Examples directory of this package.

module Math.MeanShift 
    (
     -- * Basic Meanshift routines
     ,meanShift,meanShiftWindow
     -- * Auxiliary functions for iterating the meanshift steps.
     ,fixedPoint, fixedPointE,
     -- * Types
     ,Window,Support
     -- * (multidimensional) Kernel Density Estimates
     ,kde
    ) where
import qualified Data.Vector.Unboxed as V
import Data.List hiding (sum)
import qualified Data.List as L
import Prelude hiding (sum)

-- The project cabal file <url:../../MeanShift.cabal>

-- | Euclidian norm
norm² :: Vector -> Double
norm² = V.sum . V.map (**2)


-- | One dimensional normal kernel and its derivative.
normalKernel,normalKernel' :: Double -> Double
normalKernel x = exp(-0.5 * x)
normalKernel' x = -2 *  exp(-0.5 * x)

-- | Kernel density estimate of given points. Uses a normal kernel.
kde :: Double -> [Vector] -> (Vector -> Double)
kde h vs x = (1 / (n*((2*π)**(d/2))*(h**d)))
             * (L.sum $ map (\xi -> normalKernel (norm² ((x ^- xi) ./ h))) vs)
   where
      n = fi . length $ vs
      d = fi . V.length . head $ vs


-- | Calculate the Mean shift for a point in a dataset. This is
-- efficient only when we cannot make an a priori estimate on which
-- points contribute to the mean shift at given location.
--
meanShift :: Double -> [Vector] -> (Vector -> Vector)
meanShift h vs x = sumW d vs dists (1/V.sum dists) x
   where
    d = V.length (head vs)
    dists = V.fromList $ map (\xi -> normalKernel' $ distPerH x xi) vs
    distPerH :: Vector -> Vector  -> Double
    distPerH !a !b = V.sum (V.zipWith (\u v -> ((u-v) / h)^(2::Int)) a b)

type Window = Support -> [Vector]
type Support = (Vector,Double)

-- | Mean shift with a windowing function. Performing mean shift is more
--   efficient if we can index and calculate only those points that are in
--   the support of our kernel.
{-#INLINEABLE meanShiftWindow#-}
meanShiftWindow :: Int -> Window -> Double -> (Vector -> Vector)
meanShiftWindow d window h x
    = sumW d w dists (1/V.sum dists) x
   where
    dists = V.fromList $ map (\xi -> normalKernel' $ distPerH x xi) w
    w = window (x,h*2) -- TODO: Think this through
    distPerH :: Vector -> Vector -> Double
    distPerH !a !b = V.sum  (V.zipWith (\u v -> ((u-v) / h)^(2::Int)) a b)

-- | Find a path to the fixed point of a function.
{-#INLINEABLE fixedPoint#-}
fixedPoint :: Eq a => (a -> a) -> a -> [a]
fixedPoint f x = x:let x' = f x in if x'/=x then fixedPoint f x' else [x']

fixedPointE :: Double -> (Vector -> Vector) -> Vector -> [Vector]
fixedPointE e f x = x:let x' = f x
                    in if V.sum (V.map abs $ x' ^- x) > e then fixedPointE e f x' else [x']


-- * Auxiliary functions, and shorthands

type Vector = V.Vector Double

v :: [Double] -> Vector
v = V.fromList

{-#INLINE (^+)#-}
{-#INLINE (^-)#-}
{-#INLINE (^/)#-}
(^+),(^-),(^/) :: Vector -> Vector -> Vector
(^-) = V.zipWith (-)
(^+) = V.zipWith (+)
(^/) = V.zipWith (/)
a .+ b = V.map (+b) a
(.+),(./),(.*) :: Vector -> Double -> Vector
a ./ b = V.map (/b) a
a .* b = V.map (*b) a
infixl 7 ^/
infixl 6 ^+ , ^-
box :: x -> [x]
box x = [x]
box2 :: x -> x -> [x]
box2 x y = [x,y]

sv :: Double -> Vector
sv = V.singleton
fs :: Vector -> Double
fs = V.head

{-#INLINEABLE sumD#-}
sumD :: Int -> [Vector] -> Vector
sumD d xs = V.generate d (\i -> L.sum (map (`V.unsafeIndex` i) xs) )


{-#INLINEABLE sumW#-}
sumW :: Int -> [Vector] -> Vector -> Double -> Vector
sumW d es ws n = V.generate d (\i -> go i 0 es 0)
   where
      go i j (x:xs) acc = go i (j+1) xs $ acc + n*(x V.! i)*(ws V.! j)
      go _ _ []     acc = acc


π :: Double
π = pi

fi :: Int -> Double
fi = fromIntegral

