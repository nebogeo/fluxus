Target       = "fluxus"
Install   	 = "/usr/local/bin"
LibPaths     = Split("/usr/local/lib /usr/X11R6/lib")
IncludePaths = Split("/usr/local/include libfluxus/src libfluxphysics/src")
	  
Libs = Split("portaudio sndfile guile fftw3f ode png \
			  glut tiff GL GLU z m Xi Xmu Xext Xt SM ICE X11 pthread lo")

Source = Split("src/AudioCollector.cpp \
				src/FluxusMain.cpp \
				src/FluxusBinding.cpp \
				src/PortAudioClient.cpp \
				src/TurtleBuilder.cpp \
				src/GLEditor.cpp \
				src/Utils.cpp \
				src/OSCServer.cpp \
				src/main.cpp \
				libfluxus/src/GraphicsUtils.cpp \
				libfluxus/src/Lifeforms.cpp \
				libfluxus/src/PNGLoader.cpp \
				libfluxus/src/PolyPrimitive.cpp \
				libfluxus/src/TextPrimitive.cpp \
				libfluxus/src/CompiledPrimitive.cpp \
				libfluxus/src/LinePrimitive.cpp \
				libfluxus/src/NURBSPrimitive.cpp \
				libfluxus/src/Primitive.cpp \
				libfluxus/src/Light.cpp \
				libfluxus/src/Renderer.cpp \
				libfluxus/src/SceneGraph.cpp \
				libfluxus/src/State.cpp \
				libfluxus/src/TexturePainter.cpp \
				libfluxus/src/Tree.cpp \
				libfluxus/src/dada.cpp \
				libfluxphysics/src/Physics.cpp")					

env = Environment(CCFLAGS = '-pipe -Wall -O3 -ggdb -pg -ffast-math -Wno-unused -fPIC', LINKFLAGS='-pg')
env.Program(source = Source, target = Target, LIBS=Libs, LIBPATH=LibPaths, CPPPATH=IncludePaths)


Libs = Split("portaudio sndfile guile fftw3 ode png \
			  glut tiff GL GLU z m Xi Xmu Xext X11 pthread lo")

if not GetOption('clean'):
	print '--------------------------------------------------------'		
	print 'Fluxus: Configuring Build Environment'
	print '--------------------------------------------------------'		
	conf = Configure( env )	
	
	if not conf.CheckLibWithHeader('m', 'math.h', 'C'):
		print 'ERROR: libm must be installed!'
		Exit(1)   
		
	if not conf.CheckLibWithHeader('pthread', 'pthread.h', 'C'):
		print 'ERROR: glut must be installed!'
		Exit(1)   

	if not conf.CheckLibWithHeader('sndfile', 'sndfile.h', 'C'):
		print 'ERROR: sndfile must be installed!'
		Exit(1)   
	
	if not conf.CheckLibWithHeader('fftw3', 'fftw3.h', 'C'):
		print 'ERROR: fftw3 must be installed!'
		Exit(1)   
		
	if not conf.CheckLibWithHeader('lo', 'lo/lo.h', 'C'):
		print 'ERROR: liblo must be installed!'
		Exit(1) 
		  
	if not conf.CheckLibWithHeader('ode', 'ode/ode.h', 'C'):
		print 'ERROR: ode must be installed!'
		Exit(1)   

	if not conf.CheckLibWithHeader('guile', 'guile/gh.h', 'C'):
		print 'ERROR: guile must be installed!'
		Exit(1)   

	if not conf.CheckLibWithHeader('portaudio', 'portaudio.h', 'C'):
		print 'ERROR: portaudio must be installed!'
		Exit(1)   

	env = conf.Finish()	


env.Install(Install, Target)
env.Alias('install', Install)
