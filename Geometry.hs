module Geometry where

import MyVector3
import Lights
import Colour

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

