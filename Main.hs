import Geometry
import Lights
import Renderer
import MyVector3
import Colour
import PPM6

{- a simple example demonstrating some of the capabilities of this framework -}

main = save_ppm "example.ppm" (render viewPos viewDir viewUp shapes lights)

viewPos = Vec3 [-50, 20, 0]

shapes = [(sphere 1.0 (Vec3 [0, 0, 0]), Surf (0.2 * yellow) 3.0 40.0)
	, (torus 2.0 0.25 (Vec3 [1, 0, 0]), Surf (0.4*white) 2.0 35.0)]
lights = [Light (Vec3 [-4, 3, -3]) green 1.5
	, Light (Vec3 [-3, -2, -12]) blue 0.5
	, Light (Vec3 [0, 0, 8]) white 0.5
	, Light (Vec3 [6, -4, 13]) yellow 3.0]

viewDir = negate $ normalize viewPos
viewUp = Vec3 [0,0,1]
