module MyVector3 where

{- basic vector utility to be used throughout the raytracer.
although there is nothing explicitly stopping construction of
non-3-element-vectors, do not do it -}

data Vector3 = Vec3 [Double]

instance Num Vector3 where
	negate (Vec3 a) = Vec3 (map negate a)
	(Vec3 a) + (Vec3 b) = Vec3 (zipWith (+) a b)
	(Vec3 a) * (Vec3 b) = Vec3 (zipWith (*) a b)
	abs (Vec3 a) = Vec3 (map abs a)

magnitude :: Vector3 -> Double
magnitude (Vec3 a) = sqrt(sum $ map (\x-> x*x) a)

clamp :: Vector3 -> Vector3
clamp (Vec3 a) = Vec3 (map (\x -> if x > 0 then x else 0) a)

dot :: Vector3 -> Vector3 -> Double
dot (Vec3 a) (Vec3 b) = foldl (+) 0 (zipWith (*) a b)

normalize :: Vector3 -> Vector3
normalize x@(Vec3 a) = Vec3 (map (* (1.0/(magnitude x))) a)

element :: Vector3 -> Int -> Double
element (Vec3 a) i =  a !! i

scale :: Double -> Vector3 -> Vector3
scale x (Vec3 v) = Vec3 (map (*x) v)

cross :: Vector3 -> Vector3 -> Vector3
cross (Vec3 a) (Vec3 b) =
	Vec3 [ (a!!1)*(b!!2)-(a!!2)*(b!!1), (a!!2)*(b!!0)-(a!!0)*(b!!2), (a!!0)*(b!!1)-(a!!1)*(b!!0)]
