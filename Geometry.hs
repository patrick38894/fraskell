module Geometry where

import MyVector3
import Lights
import Colour


{- this file defines shapes, aka geometric primitives.
they are defined using distance functions. these are a functions which
estimate the distance from a point in space
to the surface of the shape. a shape is a tuple of
its distance function and its surface properties -}

type Shape = ((Vector3->Double),Surface)

sphere :: Double -> Vector3 -> (Vector3 -> Double)
sphere r x = \p -> magnitude (p-x) -r

torus:: Double -> Double -> Vector3 -> (Vector3 -> Double)
torus r1 r2 x = \p -> let
	p' = p + x
	a = p' `element` 0
	b = p' `element` 1
	c = p' `element` 2
	iDist = sqrt(a*a+c*c) - r1 in--inner distance
		sqrt(iDist*iDist+b*b) - r2 --total distance

