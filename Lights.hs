module Lights where

import Colour
import MyVector3

data Lamp = Light Vector3 Colour Double
data Surface = NotASurface | Surf Colour Double Double

cScale :: Double -> Colour -> Colour
cScale x c = cmap (*x) c

mix :: Surface -> Colour -> Colour
mix (Surf sClr _ _) lClr = cclip $ lClr - cclip (white - sClr)

specular :: Surface -> Lamp -> Vector3 -> Vector3 -> Colour
specular (Surf _ diff spec) (Light pos lclr mag) dir normal
	| dot normal pos < 0 = 0
	| otherwise = cScale ( mag * (dot dir (normalize pos)) ** spec) lclr

diffuse :: Surface -> Lamp -> Vector3 -> Colour
diffuse (Surf _ diff spec) (Light pos lclr mag) normal
	| dot normal pos < 0 = 0
	| otherwise = cScale (mag * (dot normal (normalize pos)) ** diff) lclr

