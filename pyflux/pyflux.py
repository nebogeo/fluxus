from pyflux import *

desiredfps(1000)
tex = load_texture("testcard.png")
clear_colour((0,0,0))
show_axis(1)

colour((0,1,0))
root = build_cube()
parent(root)

push()
translate((2,0,-10))
colour((1,1,1))
texture(tex)
cube = build_cube()
pop()

push()
translate((0,5,-10))
scale((0.5,0.5,0.5))

build_cube()
pop()

push()
translate((0,0,-10))
rotate((0,0,45))
build_cube()
pop()

def animate():
	grab(root)
	if key_pressed("q"): translate((0,0,1))
	if key_pressed("a"): translate((0,0,-1))
	if key_pressed("o"): rotate((0,1,0))
	if key_pressed("p"): rotate((0,-1,0))
	ungrab()
		
	
engine_callback("animate()")
