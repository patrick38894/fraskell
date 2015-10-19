import Colour
import PPM6

main = save_ppm "test.ppm" colors
colors = replicate 100 (replicate 100 red)
