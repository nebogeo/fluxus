#                                                              -*- python -*-

Target       = "fluxus"
Install   	 = "/usr/local/bin"
LibPaths     = Split("/usr/local/lib /usr/X11R6/lib")
IncludePaths = Split("/usr/local/include libfluxus/src libfluxphysics/src")

#Libs = Split("jack sndfile guile fftw3 ode png glut tiff GL GLU z m Xi Xmu Xext Xt SM ICE X11 pthread lo jpeg")

Source = Split("libfluxus/src/PData.cpp \
                libfluxus/src/PDataOperator.cpp \
		libfluxus/src/PDataContainer.cpp \
		libfluxus/src/PDataArithmetic.cpp \
		libfluxus/src/GraphicsUtils.cpp \
		libfluxus/src/Lifeforms.cpp \
		libfluxus/src/PNGLoader.cpp \
		libfluxus/src/PolyPrimitive.cpp \
		libfluxus/src/TextPrimitive.cpp \
		libfluxus/src/CompiledPrimitive.cpp \
		libfluxus/src/LinePrimitive.cpp \
		libfluxus/src/ParticlePrimitive.cpp \
		libfluxus/src/NURBSPrimitive.cpp \
		libfluxus/src/Primitive.cpp \
		libfluxus/src/Light.cpp \
		libfluxus/src/Renderer.cpp \
		libfluxus/src/SceneGraph.cpp \
		libfluxus/src/State.cpp \
		libfluxus/src/TexturePainter.cpp \
		libfluxus/src/Tree.cpp \
		libfluxus/src/dada.cpp \
		libfluxus/src/SearchPaths.cpp \
		libfluxphysics/src/Physics.cpp \
		src/AudioCollector.cpp \
		src/FluxusMain.cpp \
		src/FluxusBinding.cpp \
		src/JackClient.cpp \
		src/TurtleBuilder.cpp \
		src/GLEditor.cpp \
		src/Repl.cpp \
		src/Utils.cpp \
		src/OSCServer.cpp \
		src/Recorder.cpp \
		src/main.cpp")					

env = Environment(CCFLAGS = '-ggdb -pipe -Wall -O3 -ffast-math -Wno-unused -fPIC')
Libs = Split("jack sndfile guile fftw3 ode png tiff z m X11 pthread lo jpeg")

if env['PLATFORM'] == 'darwin':
	from osxbundle import *
	TOOL_BUNDLE(env)
	Frameworks = Split("GLUT OpenGL")
	env.Program(source = Source, target = Target, LIBS=Libs, LIBPATH=LibPaths, CPPPATH=IncludePaths, FRAMEWORKS=Frameworks)
else:
	Libs.extend(Split("glut GL GLU"))
	env.Program(source = Source, target = Target, LIBS=Libs, LIBPATH=LibPaths, CPPPATH=IncludePaths)

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

	if not conf.CheckLibWithHeader('jack', 'jack/jack.h', 'C'):
		print 'ERROR: jack must be installed!'
		Exit(1)   

	env = conf.Finish()	


if env['PLATFORM'] == 'darwin':
	env.MakeBundle("Fluxus.app", "fluxus", "key", "fluxus-Info.plist", typecode='APPL', icon_file='macos/fluxus.icns')

env.Install(Install, Target)
env.Alias('install', Install)
