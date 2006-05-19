#                                                              -*- python -*-

import os, os.path

Target       = "fluxus"

Prefix = "/usr/local"
Install      = Prefix + "/bin"

GuileVersionMajMin = "1.8"
GuilePrefix        = "/usr/local"
GuileDataPrefix    = GuilePrefix + "/share/guile"
GuileSchemePrefix  = GuileDataPrefix + "/" + GuileVersionMajMin

SchemePrefix = GuileSchemePrefix + "/fluxus"

LibPaths     = ["/usr/local/lib"]
IncludePaths = Split("/usr/local/include libfluxus/src libfluxphysics/src")

# First member of each list is a library, second - a header or headers list
# to be passed to the CheckLibWithHeader(...) at configure time.
# We may add extra libraries later on per platform basis
LibList      = [["m", "math.h"],
		["jack", "jack/jack.h"],
		["sndfile", "sndfile.h"],
		["guile", "guile/gh.h"],
		["fftw3", "fftw3.h"],
		["ode", "ode/ode.h"],
		["lo", "lo/lo.h"],
		["pthread", "pthread.h"],
		["jpeg", ["stdio.h", "stdlib.h", "jpeglib.h"]],
		["tiff", "tiff.h"],
		["z", "zlib.h"],
		["png", "libpng/png.h"]]

Source = Split("libfluxus/src/PData.cpp \
        libfluxus/src/PDataOperator.cpp \
		libfluxus/src/PDataContainer.cpp \
		libfluxus/src/PDataArithmetic.cpp \
		libfluxus/src/GraphicsUtils.cpp \
		libfluxus/src/PNGLoader.cpp \
		libfluxus/src/PolyPrimitive.cpp \
		libfluxus/src/TextPrimitive.cpp \
		libfluxus/src/CompiledPrimitive.cpp \
		libfluxus/src/LinePrimitive.cpp \
		libfluxus/src/ParticlePrimitive.cpp \
		libfluxus/src/PixelPrimitive.cpp \
		libfluxus/src/NURBSPrimitive.cpp \
		libfluxus/src/LocatorPrimitive.cpp \
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
		src/SchemePrim.cpp \
		src/JackClient.cpp \
		src/TurtleBuilder.cpp \
		src/GLEditor.cpp \
		src/Repl.cpp \
		src/Utils.cpp \
		src/OSCServer.cpp \
		src/OSCCore.cpp \
		src/Recorder.cpp \
		src/main.cpp")
FluxusVersion = "HEAD"

param_cast_test_src = """
#include <libguile.h>

SCM test () { return NULL; }

int main(int argc, char ** argv) {
	scm_c_define_gsubr("test", 0,0,0,(SCM (*)())test);
	return 0;
}
"""

def CheckParamCast(context):
	context.Message('Checking if callbacks should be cast to (SCM (*)())...')
	result = context.TryCompile(param_cast_test_src, ".cpp")
	context.Result(result)
	return result

env = Environment(CCFLAGS = '-ggdb -pipe -Wall -O3 -ffast-math -Wno-unused -fPIC',
		  LIBPATH = LibPaths,
		  CPPPATH = IncludePaths,
		  VERSION_NUM = FluxusVersion)

Default(env.Program(source = Source, target = Target))

if env['PLATFORM'] == 'darwin':
	env.Replace(LINK = "macos/libtool --mode=link g++")
	env.Prepend(LINKFLAGS = ["-static"])
else:
	LibList += [["X11", "X11/Xlib.h"],
				# for X11
				#["Xi", None],["Xmu", None], ["Xext", None], ["Xt", None], ["SM", None], ["ICE", None],
                ["glut", "GL/glut.h"],
           	    ["GL", "GL/gl.h"],
		    ["GLU", "GL/glu.h"]]
	env.Append(LIBPATH = ["/usr/X11R6/lib"])
	
if not GetOption('clean'):
	print '--------------------------------------------------------'		
	print 'Fluxus: Configuring Build Environment'
	print '--------------------------------------------------------'		
	conf = Configure( env, custom_tests = {'CheckParamCast' : CheckParamCast} )	
	# all libraries are required, but they can be checked for independently
	# (hence autoadd=0), which allows us to speed up the tests ...
	for (lib,headers) in LibList:
		if not conf.CheckLibWithHeader(lib, headers, 'C', autoadd = 0):
			print "ERROR: '%s' must be installed!" % (lib)
			Exit(1)
	# check if ellipsis is needed in casts
	if not conf.CheckParamCast():
		env.Append(CCFLAGS=' -DNEED_ELLIPSIS_IN_CASTS')
	env = conf.Finish()
	# ... but we shouldn't forget to add them to LIBS manually
	env.Replace(LIBS = [rec[0] for rec in LibList])
	

# packaging / installing
if env['PLATFORM'] == 'darwin':
	from macos.osxbundle import *
	TOOL_BUNDLE(env)
	# We add frameworks after configuration bit so that testing is faster.
	env.Replace(FRAMEWORKS = Split("GLUT OpenGL CoreAudio"))
	env.Alias("app", env.MakeBundle("Fluxus.app",
					"fluxus",
					"key",
					"macos/fluxus-Info.plist",
					typecode='APPL',
					icon_file='macos/fluxus.icns'))
	GuileScripts = "Fluxus.app/Contents/Resources/guile_scripts"
	SchemePrefix = GuileScripts + "/site/fluxus"
	for where, dirs, files in os.walk(GuileSchemePrefix):
		dest = os.path.join(GuileScripts, where[len(GuileSchemePrefix)+1:])
		for f in files:
			env.Install(dest, os.path.join(where,f))
	
	env['BUILDERS']['DiskImage'] = Builder(action = BuildDmg)
	DmgFiles = [File("COPYING"), Dir("Fluxus.app"), Dir("docs"), Dir("examples"), Dir("scm")]
	env.Alias("dmg", env.DiskImage('Fluxus-' + FluxusVersion + '.dmg',
				       DmgFiles))
else:
	env.Install(Install, Target)
	env.Alias('install', Prefix)

env.Install(SchemePrefix,"#/scm/init.scm")
env.Install(SchemePrefix,"#/scm/macros.scm")

