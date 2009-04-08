# -*- mode: python; -*-
################################################################################
# Top level SConscript for fluxus
#
# Checks all dependencies needed, builds the fluxus canvas
# application, then calls the sconscripts for libfluxus and
# the fluxus PLT modules

import os, sys

MajorVersion = "0"
MinorVersion = "16"
FluxusVersion = MajorVersion+MinorVersion
# remember to change fluxa too...
Target = "fluxus"

# changed prefix and pltprefix so they can be invoked at runtime
# like scons Prefix=/usr PLTPrefix=/usr instead of default /usr/local

DESTDIR = ARGUMENTS.get('DESTDIR', '')
# this makes DESTDIR relative to root of the source tree, no matter
# where we are
if len(DESTDIR)>0 and DESTDIR[0] != "/":
                DESTDIR = "#" + DESTDIR

if sys.platform == 'darwin':
	AddOption('--app', action='store_true', help='Build OSX application')
	file = os.popen('dirname "`which mzscheme`"')
	PLTBin = file.read()
	file.close()
	Prefix = ARGUMENTS.get('Prefix','/opt/local')
	PLTPrefix = ARGUMENTS.get('PLTPrefix', PLTBin[:-5])
	PLTInclude = ARGUMENTS.get('PLTInclude', PLTPrefix + "/include")
	PLTLib = ARGUMENTS.get('PLTLib', PLTPrefix + "/lib")
else:
	Prefix = ARGUMENTS.get('Prefix','/usr/local')
	PLTPrefix = ARGUMENTS.get('PLTPrefix','/usr/local')
	PLTInclude = ARGUMENTS.get('PLTInclude', PLTPrefix + "/include/plt")
	PLTLib = ARGUMENTS.get('PLTLib', PLTPrefix + "/lib/plt")
BinInstall = DESTDIR + Prefix + "/bin"

DataLocation = Prefix + "/share/fluxus-"+FluxusVersion
DataInstall = DESTDIR + DataLocation
FluxusCollectsLocation = Prefix + "/lib"
CollectsInstall = DESTDIR + FluxusCollectsLocation + "/fluxus-" + FluxusVersion

if sys.platform == 'darwin':
        PLTCollectsLocation = PLTPrefix + "/collects/"
else:
        PLTCollectsLocation = PLTLib + "/collects/"

if sys.platform == 'darwin' and GetOption('app'):
        PLTCollectsLocation = '/Applications/Fluxus.app/Contents/Resources/collects/'
        FluxusCollectsLocation = '/Applications/Fluxus.app/Contents/Resources/collects/'
        DataLocation = '/Applications/Fluxus.app/Contents/Resources'

LibPaths     = [
        PLTLib,
        PLTLib+"/..",
        "/usr/lib",
        "../../libfluxus"]

IncludePaths = [
        "/usr/local/include",
        "/usr/include",
        "/usr/local/include/freetype2",  # arg - freetype needs to be
        "/usr/include/freetype2",        # on the include path :(
        PLTInclude,
        "../../libfluxus/src"]

################################################################################
# Make the "one" environment for building and installing

env = Environment(CCFLAGS = '-ggdb -pipe -Wall -O3 -ffast-math -Wno-unused -fPIC',
                  VERSION_NUM = FluxusVersion)

if env['PLATFORM'] == 'darwin':
        IncludePaths += ['/opt/local/include',
                                        '/opt/local/include/freetype2']
        LibPaths += ['/opt/local/lib']

env.Append(CPPPATH = IncludePaths)
env.Append(LIBPATH = LibPaths)
env.Append(CCFLAGS=' -DFLUXUS_MAJOR_VERSION='+MajorVersion)
env.Append(CCFLAGS=' -DFLUXUS_MINOR_VERSION='+MinorVersion)
env.Append(CCFLAGS=" -DPLT_COLLECTS_LOCATION="+"\"\\\""+PLTCollectsLocation+"\"\\\"")
env.Append(CCFLAGS=" -DFLUXUS_COLLECTS_LOCATION="+"\"\\\""+FluxusCollectsLocation+"\"\\\"")
env.Append(CCFLAGS=" -DDATA_LOCATION="+"\"\\\""+DataLocation+"\"\\\"")

# multitexturing causes crashes on some cards, default it to off, and
# enable users to enable manually while I figure out what it is...
if ARGUMENTS.get("MULTITEXTURE","1")=="1":
        env.Append(CCFLAGS=' -DENABLE_MULTITEXTURE')

if ARGUMENTS.get("GLSL","1")=="1":
        env.Append(CCFLAGS=' -DGLSL')

if ARGUMENTS.get("STEREODEFAULT","0")=="1":
        env.Append(CCFLAGS=' -DSTEREODEFAULT')

if ARGUMENTS.get("ACCUM_BUFFER","0")=="1":
        env.Append(CCFLAGS=' -DACCUM_BUFFER')

# need to do this to get scons to link plt's mzdyn.o
env["STATIC_AND_SHARED_OBJECTS_ARE_THE_SAME"]=1
MZDYN = PLTLib + "/mzdyn.o"

if ARGUMENTS.get("3M","1")=="1":
        env.Append(CCFLAGS=' -DMZ_PRECISE_GC')
        MZDYN = PLTLib + "/mzdyn3m.o"

################################################################################
# Figure out which libraries we are going to need

# First member of each list is a library, second - a header or headers list
# to be passed to the CheckLibWithHeader(...) at configure time.
# We may add extra libraries later on per platform basis
LibList = [["m", "math.h"],
                ["pthread", "pthread.h"],
                ["dl", "stdio.h"],
                ["mzscheme3m", PLTInclude + "/scheme.h"],
                ["jpeg", ["stdio.h", "stdlib.h", "jpeglib.h"]],
                ["tiff", "tiff.h"],
                ["freetype", "ft2build.h"],
                ["z", "zlib.h"],
                ["png", "png.h"],
                ["ode", "ode/ode.h"],
                ["sndfile", "sndfile.h"],
                ["fftw3", "fftw3.h"],
                ["lo", "lo/lo.h"],
                ["GLEW", "GL/glew.h"]]

if env['PLATFORM'] == 'posix':
        env.Prepend(LINKFLAGS = ["-rdynamic"])
        LibList += [["X11", "X11/Xlib.h"],
                    ["GL", "GL/gl.h"],
                    ["GLU", "GL/glu.h"],
                    ["glut", "GL/glut.h"],
                    ["asound", "alsa/asoundlib.h"]]

        env.Append(LIBPATH = ["/usr/X11R6/lib"])

        # add the X11 libs on - needed if we are not building on xorg
        if ARGUMENTS.get("X11",0):
                LibList=[["Xi", "X11/Xlib.h"],
                                 ["Xmu", "X11/Xlib.h"],
                                 ["Xext", "X11/Xlib.h"],
                                 ["Xt", "X11/Xlib.h"],
                                 ["SM", "X11/Xlib.h"],
                                 ["ICE", "X11/Xlib.h"]] + LibList;
elif env['PLATFORM'] == 'darwin':
        # add jack as a library if not making an app
        if not GetOption('app'):
            LibList += [["jack", "jack/jack.h"]]

        env.Append(FRAMEWORKS = ['GLUT', 'OpenGL', 'CoreAudio' ,'PLT_MrEd'],
                FRAMEWORKPATH = [PLTLib])

################################################################################
# Make sure we have these libraries availible

if not GetOption('clean'):
        print '--------------------------------------------------------'
        print 'Fluxus: Configuring Build Environment'
        print '--------------------------------------------------------'
        conf = Configure(env)

        # FIXME: check mzscheme3m framework properly on osx
        if env['PLATFORM'] == 'darwin':
                if not conf.CheckHeader('scheme.h'):
                        print "ERROR: 'mzscheme3m' must be installed!"
                        Exit(1)
                LibList = filter(lambda x: x[0] != 'mzscheme3m', LibList)

        # all libraries are required, and some of them require each other,
        # hence the order is important, and autoadd=1
        for (lib,headers) in LibList:
                if not conf.CheckLibWithHeader(lib, headers, 'C', autoadd = 1):
                        print "ERROR: '%s' must be installed!" % (lib)
                        Exit(1)

        if not conf.CheckFunc("dInitODE2"):
            env.Append(CCFLAGS=' -DGOODE_OLDE_ODE')

        # the liblo version 0.25 does not include the declaration of lo_arg_size anymore
        # This will be re-included in future version
        if not conf.CheckFunc("lo_arg_size_check", "#include <lo/lo.h>\n#define lo_arg_size_check() lo_arg_size(LO_INT32, NULL)", "C++"):
            env.Append(CCFLAGS=' -DNO_LO_ARG_SIZE_DECL')

        env = conf.Finish()
        # ... but we shouldn't forget to add them to LIBS manually
        env.Replace(LIBS = [rec[0] for rec in LibList])

# replace libs with static libs if building an osx app
if env['PLATFORM'] == 'darwin' and GetOption('app'):
	for l in ['png', 'tiff', 'GLEW', 'z', 'sndfile', 'fftw3', 'freetype', 'ode', 'jpeg']:
		env['LIBS'].remove(l)
		env['LIBS'].append(File('/opt/local/lib/lib%s.a' % l))

################################################################################
# Build the fluxus application
Install = BinInstall

# need to build the bytecode for the base scheme library
# this is the wrong place to do this
if not GetOption('clean'):
        os.system("mzc --c-mods src/base.c ++lib scheme/base")

Source = ["src/GLEditor.cpp",
                "src/GLFileDialog.cpp",
                "src/Interpreter.cpp",
                "src/Repl.cpp",
                "src/Recorder.cpp",
                "src/FluxusMain.cpp",
                "src/PolyGlyph.cpp",
                "src/main.cpp"]

env.Program(source = Source, target = Target)

################################################################################
# Build everything else
# call the core library builder and the scheme modules

SConscript(dirs = Split("libfluxus modules fluxa"),
           exports = ["env", "CollectsInstall", "DataInstall", "MZDYN", "BinInstall"])

################################################################################
# packaging / installing
if env['PLATFORM'] == 'darwin' and GetOption('app'):
        from macos.osxbundle import *
        TOOL_BUNDLE(env)
        # We add frameworks after configuration bit so that testing is faster.
        # FIXME: check if Jackmp is available if making an app
        env.Replace(FRAMEWORKS = Split("GLUT OpenGL CoreAudio PLT_MrEd Jackmp"))
        # add dynamic libs
        frameworks = [PLTLib + '/PLT_MrEd.framework',
                     '/Library/Frameworks/Jackmp.framework']
        dylibs = [ '/opt/local/lib/liblo.dylib']

        env.Alias('app', env.MakeBundle('Fluxus.app',
                                        Target,
                                        'key',
                                        'macos/fluxus-Info.plist',
                                        dylibs = dylibs,
                                        frameworks = frameworks,
                                        resources=[['modules/material/fonts/', 'material/fonts/'],
                                                   ['modules/material/meshes/', 'material/meshes/'],
                                                   ['modules/material/shaders/', 'material/shaders/'],
                                                   ['modules/material/textures/', 'material/textures/'],
                                                   ['modules/scheme/', 'collects/fluxus-%s/' % FluxusVersion],
                                                   ['modules/fluxus-engine/fluxus-engine_ss.dylib',
                                                       'collects/fluxus-' + FluxusVersion + '/compiled/native/i386-macosx/3m/fluxus-engine_ss.dylib'],
                                                   ['modules/fluxus-audio/fluxus-audio_ss.dylib',
                                                       'collects/fluxus-' + FluxusVersion + '/compiled/native/i386-macosx/3m/fluxus-audio_ss.dylib'],
                                                   ['modules/fluxus-midi/fluxus-midi_ss.dylib',
                                                       'collects/fluxus-' + FluxusVersion + '/compiled/native/i386-macosx/3m/fluxus-midi_ss.dylib'],
                                                   ['modules/fluxus-osc/fluxus-osc_ss.dylib',
                                                       'collects/fluxus-' + FluxusVersion + '/compiled/native/i386-macosx/3m/fluxus-osc_ss.dylib']],
                                        typecode='APPL',
                                        icon_file='macos/fluxus.icns'))
        # build dmg
        '''
        env['BUILDERS']['DiskImage'] = Builder(action = BuildDmg)
        DmgFiles = [File('AUTHORS'), File('CHANGES'), File('LICENCE'), \
                File('README'), File('COPYING'), Dir("Fluxus.app"), \
                Dir("examples")]
        env.Alias("dmg", env.DiskImage('Fluxus-' + FluxusVersion + '.dmg',
                                       DmgFiles))
        '''

        # fluxa
        frameworks = [ '/Library/Frameworks/Jackmp.framework']
        dylibs = [ '/opt/local/lib/liblo.dylib']
        env.Alias('app', env.MakeBundle('Fluxa.app',
                                        'fluxa/fluxa',
                                        'key',
                                        'macos/fluxa-Info.plist',
                                        dylibs = dylibs,
                                        frameworks = frameworks,
                                        typecode='APPL',
                                        icon_file='macos/fluxa.icns'))


env.Install(Install, Target)
env.Alias('install', [DESTDIR + Prefix, CollectsInstall])

