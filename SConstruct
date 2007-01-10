################################################################################
# Top level SConscript for fluxus
#
# Checks all dependancies needed, builds the fluxus canvas 
# application, then calls the sconscripts for libfluxus and
# the fluxus PLT modules 

import os, os.path, shutil

MajorVersion = "0"
MinorVersion = "12"
FluxusVersion = MajorVersion+"."+MinorVersion
Target = "fluxus-"+FluxusVersion
Prefix = "/usr/local"
CollectsLocation = Prefix + "/lib/plt/collects/fluxus-"+FluxusVersion
LibPaths     = ["/usr/local/lib", "/usr/lib", "../../libfluxus"]
IncludePaths = ["/usr/local/include", "/usr/include", "/usr/local/lib/plt", "../../libfluxus/src"]

################################################################################
# Make the "one" environment for building and installing

env = Environment(CCFLAGS = '-ggdb -pipe -Wall -O3 -ffast-math -Wno-unused -fPIC',
		  LIBPATH = LibPaths,
		  CPPPATH = IncludePaths,
		  VERSION_NUM = FluxusVersion)
		  
env.Append(CCFLAGS=' -DFLUXUS_MAJOR_VERSION='+MajorVersion)
env.Append(CCFLAGS=' -DFLUXUS_MINOR_VERSION='+MinorVersion)

# need to do this to get scons to link plt's mzdyn.o
env["STATIC_AND_SHARED_OBJECTS_ARE_THE_SAME"]=1
MZDYN = "/usr/local/lib/plt/mzdyn.o"

################################################################################
# Figure out which libraries we are going to need

# First member of each list is a library, second - a header or headers list
# to be passed to the CheckLibWithHeader(...) at configure time.
# We may add extra libraries later on per platform basis
LibList = [["m", "math.h"],
		["pthread", "pthread.h"],
		["dl", "stdio.h"],
		["mzgc", "plt/scheme.h"],
		["mzscheme", "plt/scheme.h"],
		["jpeg", ["stdio.h", "stdlib.h", "jpeglib.h"]],
		["tiff", "tiff.h"],
		["z", "zlib.h"],
		["png", "libpng/png.h"],
		["ode", "ode/ode.h"],
		["jack", "jack/jack.h"],
		["sndfile", "sndfile.h"],
		["fftw3", "fftw3.h"],
		["lo", "lo/lo.h"]]
		
if env['PLATFORM'] == 'darwin':
	env.Replace(LINK = "macos/libtool --mode=link g++")
	env.Prepend(LINKFLAGS = ["-static"])
else:
	LibList += [["X11", "X11/Xlib.h"],
           	    ["GL", "GL/gl.h"],
           	    ["GLU", "GL/glu.h"],
                ["glut", "GL/glut.h"],
                ["GLEW", "GL/glew.h"]]
	env.Append(LIBPATH = ["/usr/X11R6/lib"])
	
	# add the X11 libs on - needed if we are not building on xorg
	if ARGUMENTS.get("X11",0):
		LibList=[["Xi", "X11/Xlib.h"],
				 ["Xmu", "X11/Xlib.h"], 
				 ["Xext", "X11/Xlib.h"], 
				 ["Xt", "X11/Xlib.h"], 
				 ["SM", "X11/Xlib.h"], 
				 ["ICE", "X11/Xlib.h"]] + LibList;

################################################################################
# Make sure we have these libraries availible
			  
if not GetOption('clean'):
	print '--------------------------------------------------------'		
	print 'Fluxus: Configuring Build Environment'
	print '--------------------------------------------------------'		
	conf = Configure(env)
	
	# all libraries are required, but they can be checked for independently
	# (hence autoadd=0), which allows us to speed up the tests ...
	for (lib,headers) in LibList:
		if not conf.CheckLibWithHeader(lib, headers, 'C', autoadd = 1):
			print "ERROR: '%s' must be installed!" % (lib)
			Exit(1)
			
	# enable users to enable multitexturing manually
	if ARGUMENTS.get("MULTITEXTURE",1)=="1":
		env.Append(CCFLAGS=' -DENABLE_MULTITEXTURE')
		
	env = conf.Finish()
	# ... but we shouldn't forget to add them to LIBS manually
	env.Replace(LIBS = [rec[0] for rec in LibList])
	
################################################################################
# Build the fluxus application
	
Install = Prefix + "/bin"

Source = ["src/GLEditor.cpp", 
		"src/Interpreter.cpp",
		"src/Repl.cpp",
		"src/Utils.cpp",
		"src/Recorder.cpp",
		"src/FluxusMain.cpp", 
		"src/main.cpp"]

env.Program(source = Source, target = Target)

################################################################################
# Build everything else
# call the core library builder and the scheme modules

SConscript(dirs = Split("libfluxus modules"), exports = ["env", "CollectsLocation", "MZDYN"])

################################################################################
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
	
	env['BUILDERS']['DiskImage'] = Builder(action = BuildDmg)
	DmgFiles = [File("COPYING"), Dir("Fluxus.app"), Dir("docs"), Dir("examples"), Dir("scm")]
	env.Alias("dmg", env.DiskImage('Fluxus-' + FluxusVersion + '.dmg',
				       DmgFiles))
else:
	env.Install(Install, Target)
	env.Alias('install', Prefix)

