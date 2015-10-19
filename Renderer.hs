module Renderer where

import Geometry
import Lights
import MyVector3
import Colour
import Mandelbrot
import Data.Complex

epsilon = 0.00001 :: Double
infinity = 99999999 :: Double
sceneRadius = 100.0 :: Double

render :: Vector3 -> Vector3 -> Vector3 -> [Shape] -> [Lamp] -> [[Colour]]
render pos dir up shapes lights = map (map raytrace) rays where
--render the scene of shapes and lights
	rays = genRays 1920 1080 dir up
	raytrace = trace shapes lights pos

genRays :: Integer -> Integer -> Vector3 -> Vector3 -> [[Vector3]]
genRays x y dir up = map (map distort) grid where
--generate a grid of unit vectors distorted to focal length f
	grid = [map (\a->(b,a)) [-(y `div` 2) .. y `div` 2 - 1] | b <- [-(x `div` 2) .. x `div` 2 -1]]
	distort (m,n) = normalize (dir + xoff + yoff) where
		xoff = scale (f * fromIntegral m / fromIntegral x) right
		yoff = scale (f * fromIntegral n / fromIntegral x) up
	f = 0.125
	right = Vec3 [0,-1,0]--dir `cross` up
	

trace :: [Shape] -> [Lamp] -> Vector3 -> Vector3 -> Colour
trace shapes lights pos dir
--trace the path of a ray as it bounces over objects, until it leaves the scene
	| dist < epsilon = surfaceColor + 0.5 * (trace shapes lights shiftPos reflection)
	| dist >= sceneRadius = (colorize . mandelbrot . inv . normToTex $ normalize pos)
	| otherwise = trace shapes lights (pos + scale dist dir) dir where
		dist = de shapes pos
		surfaceColor = illuminate (closestSurf shapes pos) lights dir normal
		reflection = dir - scale (2 * dot dir normal) normal
		normal = normalize $ derivative (de shapes) pos
		shiftPos = pos + scale 2.0 reflection
	

--these functions are for mapping the atmosphere to the mandelbrot set
inv :: Complex Double -> Complex Double
inv = inversion ((-0.75) :+ 0.1) 0.005

normToTex :: Vector3 -> Complex Double
--normToTex (Vec3 (x:y:z:[]) ) = (pi / acos y) :+ (pi / acos z)
--normToTex (Vec3 (x:y:z:[]) ) = (1.0/y) :+ (1.0/acos z
normToTex (Vec3 (x:y:z:[]) ) = y :+ z
-- ---^--------^-------


de :: [Shape] -> (Vector3 -> Double)
--construct a universal distance estimator function through composition of
--the various primitive de functions
de shapes = \x -> let distances = map ((\f->f x) . fst) shapes in
	foldl min infinity distances

closestSurf :: [Shape] -> Vector3 -> Surface
--find the surface closest to a given position
closestSurf shapes x = isClosest infinity NotASurface shapes where
	isClosest small srf [] = srf
	isClosest small srf (g:gs)
		| (fst g) x < small = isClosest (fst g x) (snd g) gs
		| otherwise = isClosest small srf gs

derivative :: (Vector3 -> Double) -> Vector3 -> Vector3
--differentiate a function from R3->R using the approximation e=0
derivative f p = Vec3 [f (p+x) - f p, f (p+y) - f p, f (p+z) - f p] where
	x = Vec3 [epsilon, 0, 0]
	y = Vec3 [0, epsilon, 0]
	z = Vec3 [0, 0, epsilon]

illuminate :: Surface -> [Lamp] -> Vector3 -> Vector3 -> Colour
illuminate NotASurface _ _ _ = black
illuminate _ [] _ _ = black
illuminate s (l:ls) dir normal = mix s (diff + spec + amb + illuminate s ls dir normal) where
--illuminate a surface with
	spec = specular s l dir normal
	diff = diffuse s l normal
	amb = 0.5 * white

