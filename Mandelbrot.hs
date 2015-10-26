module Mandelbrot where

import Data.Complex
import Colour

{- basic emplementation of the mandelbrot set. in order to make
the fractals more interesting, I implemented an inversion function
which inverts the complex plane around a specific point. picking points
near the edge of the mandelbrot set will result in some very interesting
pictures -}

maxIter = 128 :: Integer

mandelbrot :: Complex Double -> Integer
mandelbrot c = recMandelbrot c c maxIter where
	recMandelbrot z c n
		| magnitude (z*z) > 4 = 0
		| n < 0 = 0
		| otherwise = 1 + recMandelbrot (z*z+c) c (n-1)

inversion :: Complex Double -> Double -> Complex Double -> Complex Double
inversion c radius x = c + mkPolar (radius/r) theta where
	(r,theta) = polar(x-c)

colorize :: Integer -> Colour
colorize x = if x >= maxIter then black else Colour r g b where
	r = 0.9 * (sin $ fromIntegral x)
	g = 0.5 * (cos $ fromIntegral x)
	b = abs $ 0.25 * (4.0 + (fromIntegral $ x `mod` 8))

--this function is only used to render test images and can safely be ignored	
draw :: Integer -> Integer -> [[Colour]]
draw x y = map (map (colorize . mandelbrot . inv)) grid where
	texCoords = [map (\a->(fromIntegral b :+ fromIntegral a)) [-(y `div` 2) .. y `div` 2 - 1] | b <- [-(x `div` 2) .. x `div` 2 -1]]
	grid = map (map((*(2:+0)).(/(fromIntegral y :+ 0)))) texCoords
	inv = inversion ((-0.75) :+ 0.1) 0.005


