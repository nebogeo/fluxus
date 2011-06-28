# -*- mode: python; -*-
################################################################################
# Top level SConscript for fluxus
#
# Checks all dependencies needed, builds the fluxus canvas
# application, then calls the sconscripts for libfluxus and
# the fluxus Racket modules

import os, os.path, sys, commands, subprocess

MajorVersion = "0"
MinorVersion = "18"
FluxusVersion = MajorVersion+MinorVersion
# remember to change fluxa too...
Target = "fluxus"

DESTDIR = ARGUMENTS.get('DESTDIR', '')
# this makes DESTDIR relative to root of the source tree, no matter
# where we are
if len(DESTDIR)>0 and DESTDIR[0] != "/":
                DESTDIR = "#" + DESTDIR

if sys.platform == 'darwin':
	AddOption('--app', action='store_true', help='Build OSX application')
	file = os.popen('dirname "`which racket`"')
	RacketBin = file.read()
	file.close()
	Prefix = ARGUMENTS.get('Prefix','/opt/local')
	RacketPrefix = ARGUMENTS.get('RacketPrefix', RacketBin[:-5])
	RacketInclude = ARGUMENTS.get('RacketInclude', RacketPrefix + "/include")
	RacketLib = ARGUMENTS.get('RacketLib', RacketPrefix + "/lib")
	RacketCollects = ARGUMENTS.get('RacketCollects', RacketPrefix + "/collects/")

elif sys.platform == 'win32':
	Prefix = ARGUMENTS.get('Prefix','c:/Program Files/Fluxus')
	RacketPrefix = ARGUMENTS.get('RacketPrefix','c:/Program Files/Racket')
	RacketInclude = ARGUMENTS.get('RacketInclude', RacketPrefix + "/include")
	RacketLib = ARGUMENTS.get('RacketLib', RacketPrefix + "/lib")
	RacketCollects = ARGUMENTS.get('RacketCollects', RacketLib + "/racket/collects/")
else:
	Prefix = ARGUMENTS.get('Prefix','/usr/local')
	RacketPrefix = ARGUMENTS.get('RacketPrefix','/usr/local')
	RacketInclude = ARGUMENTS.get('RacketInclude', RacketPrefix + "/include/racket")
	RacketLib = ARGUMENTS.get('RacketLib', RacketPrefix + "/lib/racket")
# dave changed for racket package which puts collects in /usr/share
	RacketCollects = ARGUMENTS.get('RacketCollects', RacketLib + "/collects/")

BinInstall = DESTDIR + Prefix + "/bin"

DataLocation = Prefix + "/share/fluxus-"+FluxusVersion
DataInstall = DESTDIR + DataLocation
FluxusCollectsLocation = Prefix + "/lib"
CollectsInstall = DESTDIR + FluxusCollectsLocation + "/fluxus-" + FluxusVersion
DocsInstall = DESTDIR + Prefix + "/share/doc/fluxus-" + FluxusVersion

if sys.platform == 'darwin' or sys.platform == 'win32':
        RacketCollectsLocation = ARGUMENTS.get('RacketCollects', RacketPrefix + "/collects/")
else:
        RacketCollectsLocation = ARGUMENTS.get('RacketCollects', RacketCollects)


if sys.platform == 'darwin' and GetOption('app'):
	RacketCollectsLocation = 'collects/' # not used relative racket collects path and
	DataLocation = 'Resources' 			 # data location is determined in Interpreter.cpp
	FluxusCollectsLocation = '#Fluxus.app/Contents/Resources/collects/'
	CollectsInstall = FluxusCollectsLocation + '/fluxus-' + FluxusVersion
	DataInstall = '#Fluxus.app/Contents/Resources/'

# run racket to get (path->string (system-library-subpath))
file = os.popen("racket -em \"(begin (display (path->string (system-library-subpath)))(exit))\"")
archpath = file.read()
file.close()

BinaryModulesLocation = CollectsInstall + "/compiled/native/" + archpath

LibPaths     = [
        RacketLib,
        RacketLib+"/..",
        "/usr/lib",
        "../../libfluxus"]

IncludePaths = [
        "/usr/local/include",
        "/usr/include",
        "/usr/local/include/freetype2",  # arg - freetype needs to be
        "/usr/include/freetype2",        # on the include path :(
        RacketInclude,
        "../../libfluxus/src"]

################################################################################
# Make the "one" environment for building and installing

paranoid = ' -W -Wcast-qual -Wwrite-strings -Wcast-align -Wpointer-arith -Wconversion -Wredundant-decls -Wunreachable-code -Winline -Wlarger-than-256'

env = Environment(CCFLAGS = '-ggdb -pipe -Wall -O3 -ffast-math -Wno-unused -fPIC',
                  VERSION_NUM = FluxusVersion)
env.MergeFlags(ARGUMENTS.get('CCFLAGS', '').split())
env.MergeFlags(ARGUMENTS.get('LDFLAGS', '').split())

if env['PLATFORM'] == 'win32':
	IncludePaths += [ "/MinGW/include/freetype2" ]
	LibPaths += [ "/MinGW/lib" ]

if env['PLATFORM'] == 'darwin':
	if os.path.exists('/opt/local/lib'):
		# macports
		IncludePaths += ['/opt/local/include', '/opt/local/include/freetype2']
		LibPaths += ['/opt/local/lib']
	else:
		# homebrew
		IncludePaths += ['/usr/X11/include', '/usr/X11/include/freetype2']
		LibPaths += ['/usr/X11/lib']

env.Append(CPPPATH = IncludePaths)
env.Append(LIBPATH = LibPaths)
env.Append(CCFLAGS=' -DFLUXUS_MAJOR_VERSION='+MajorVersion)
env.Append(CCFLAGS=' -DFLUXUS_MINOR_VERSION='+MinorVersion)
env.Append(CCFLAGS=" -DRACKET_COLLECTS_LOCATION="+"\"\\\""+RacketCollectsLocation+"\"\\\"")
env.Append(CCFLAGS=" -DFLUXUS_COLLECTS_LOCATION="+"\"\\\""+FluxusCollectsLocation+"\"\\\"")
env.Append(CCFLAGS=" -DDATA_LOCATION="+"\"\\\""+DataLocation+"\"\\\"")

if ARGUMENTS.get("GLSL","1")=="1":
        env.Append(CCFLAGS=' -DGLSL')

if ARGUMENTS.get("STEREODEFAULT","0")=="1":
        env.Append(CCFLAGS=' -DSTEREODEFAULT')

if ARGUMENTS.get("ACCUM_BUFFER","0")=="1":
        env.Append(CCFLAGS=' -DACCUM_BUFFER')

if ARGUMENTS.get("MULTISAMPLE","0")=="1":
        env.Append(CCFLAGS=' -DMULTISAMPLE')

if ARGUMENTS.get("MULTITEXTURE","1")=="0":
        env.Append(CCFLAGS=' -DDISABLE_MULTITEXTURE')

if ARGUMENTS.get("RELATIVE_COLLECTS","0")=="1":
	env.Append(CCFLAGS=' -DRELATIVE_COLLECTS')

static_modules=0
if ARGUMENTS.get("STATIC_MODULES","0")=="1":
	static_modules=1
	env.Append(CCFLAGS=' -DSTATIC_LINK')

static_everything=0
if ARGUMENTS.get("STATIC_EVERYTHING","0")=="1":
	static_everything=1
	static_modules=1
	env.Append(CCFLAGS=' -DSTATIC_LINK')

static_ode=int(ARGUMENTS.get("STATIC_ODE","0"))
racket_framework=int(ARGUMENTS.get("RACKET_FRAMEWORK", "1"))
addons=int(ARGUMENTS.get('ADDONS', '1'))

# need to do this to get scons to link plt's mzdyn.o
env["STATIC_AND_SHARED_OBJECTS_ARE_THE_SAME"]=1
MZDYN = RacketLib + "/mzdyn.o"

if env['PLATFORM'] == 'win32':
	# need to do this to get scons to link plt's mzdyn.o
	MZDYN = RacketLib + "/gcc/mzdyn.o"

	if ARGUMENTS.get("3M","1")=="1":
		env.Append(CCFLAGS=' -DMZ_PRECISE_GC')
		MZDYN = RacketLib + "/gcc/mzdyn3m.o"
else:
	# need to do this to get scons to link plt's mzdyn.o
	MZDYN = RacketLib + "/mzdyn.o"
	
	if ARGUMENTS.get("3M","1")=="1":
		env.Append(CCFLAGS=' -DMZ_PRECISE_GC')
		MZDYN = RacketLib + "/mzdyn3m.o"

################################################################################
# Figure out which libraries we are going to need

# First member of each list is a library, second - a header or headers list
# to be passed to the CheckLibWithHeader(...) at configure time.
# We may add extra libraries later on per platform basis
LibList = [["m", "math.h"],
			["pthread", "pthread.h"],
			["dl", "stdio.h"],
			["jpeg", ["stdio.h", "stdlib.h", "jpeglib.h"]],
			["tiff", "tiff.h"],
			["freetype", "ft2build.h"],
			["z", "zlib.h"],
			["png", "png.h"],
			["ode", "ode/ode.h"],
			["sndfile", "sndfile.h"],
			["fftw3", "fftw3.h"],
			["lo", "lo/lo.h"],
			["GLEW", "GL/glew.h"],
			["racket3m", "scheme.h"],
			["jack", "jack/jack.h"]]

if env['PLATFORM'] == 'win32':
	LibList = [["m", "math.h"],
			["freetype", "ft2build.h"],
			["glew32", "GL/glew.h"],
			["glut32", "GL/glut.h"],
			["glu32", "GL/glu.h"],
			["opengl32", "GL/gl.h"],
			["openal32", "AL/al.h"],
			["libmzsch3m_6ncc9s", RacketInclude + "/scheme.h"]]

if env['PLATFORM'] == 'posix':
        env.Prepend(LINKFLAGS = ["-rdynamic"])
        LibList += [["GL", "GL/gl.h"],
                    ["GLU", "GL/glu.h"],
                    ["glut", "GL/glut.h"],
                    ["asound", "alsa/asoundlib.h"],
                    ["openal", "AL/al.h"]]

################################################################################
# Make sure we have these libraries availible

if not GetOption('clean'):
        print '--------------------------------------------------------'
        print 'Fluxus: Configuring Build Environment'
        print '--------------------------------------------------------'
        # detect ode precision
        if not GetOption('clean'):
          try:
            odec = subprocess.Popen(['ode-config', '--cflags', '--libs'], stdout=subprocess.PIPE)
            ode_str = odec.communicate()
            if isinstance(ode_str[0], str):
              env.MergeFlags(ode_str[0])
          except:
            print 'WARNING: unable to run ode-config, cannot detect ODE precision'

        conf = Configure(env)

        # check Racket and OpenAL frameworks on osx
        if env['PLATFORM'] == 'darwin':
                if not conf.CheckHeader('scheme.h'):
                        print "ERROR: 'racket3m' must be installed!"
                        Exit(1)
                if racket_framework:
                  LibList = filter(lambda x: x[0] != 'racket3m', LibList)
                # OpenAL should be installed everywhere
                if not conf.CheckHeader('OpenAL/al.h'):
                        print "ERROR: 'OpenAL' must be installed!"
                        Exit(1)

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

# link ode statically
if static_ode and not GetOption('clean'):
	if 'ode' in env['LIBS']:
		env['LIBS'].remove('ode')
		env['LIBS'].append(File('%s/lib/libode.a' % Prefix))

if env['PLATFORM'] == 'darwin':
	env.Append(FRAMEWORKPATH = [RacketLib])
	env.Append(CCFLAGS = ' -DOS_X') # required by PLT 4.2.5

	if GetOption('app') and not GetOption('clean'):
		# make enough space for install_name_tool
		env.Append(LINKFLAGS='-headerpad_max_install_names')
		# replace libs with static libs if building an osx app
		for l in ['png', 'tiff', 'GLEW', 'z', 'sndfile', 'fftw3', 'freetype', 'ode', 'jpeg']:
			env['LIBS'].remove(l)
			env['LIBS'].append(File('%s/lib/lib%s.a' % (Prefix, l)))

		env.Append(CCFLAGS = ' -D__APPLE_APP__ -DRELATIVE_COLLECTS')
		# add jack as a framework
		env['LIBS'].remove('jack')
		# FIXME: check if Jackmp is available when making an app
		frameworks = Split("GLUT OpenGL CoreAudio CoreFoundation Jackmp")
		if racket_framework: frameworks += ['Racket']
		env.Append(FRAMEWORKS = frameworks)
	else:
		frameworks = Split("GLUT OpenGL CoreAudio")
		if racket_framework: frameworks += ['Racket']
		env.Append(FRAMEWORKS = frameworks)


################################################################################
# Build the fluxus application
Install = BinInstall

# need to build the bytecode for the base scheme library
# this is the wrong place to do this
if not GetOption('clean'):
	if static_modules:
		# in static mode, we want to embed all of plt we need
		# using popen as it's crossplatform 
		raco_status = os.popen("raco ctool --c-mods src/base.c \
			++lib racket/base  \
			++lib racket/base/lang/reader  \
			++lib xml/xml \
			++lib compiler \
			++lib mzscheme \
			++lib mzlib/string \
			++lib setup   \
			++lib config").close()
	else:
		raco_status = subprocess.call(['raco', 'ctool', '--c-mods', 'src/base.c', '++lib', 'racket/base'])

	if raco_status != 0:
		print "ERROR: Failed to run command 'raco'"
		Exit(1)


Source = ["src/GLEditor.cpp",
          "src/GLFileDialog.cpp",
          "src/Interpreter.cpp",
          "src/Repl.cpp",
          "src/Recorder.cpp",
          "src/FluxusMain.cpp",
          "src/PolyGlyph.cpp",
          "src/Unicode.cpp",
          "src/main.cpp"]

app_env = env.Clone()

# statically link all the modules
if not GetOption('clean') and static_modules:
		
	# statically link in all the fluxus modules
	# these pick up the 'libfluxus-engine_ss.a' libs
	# rather than .so due to the 'lib' at the start
	app_env.Append(LIBPATH = ["modules/fluxus-engine/"])
	app_env.Append(LIBPATH = ["modules/fluxus-osc/"])
	app_env.Append(LIBPATH = ["modules/fluxus-audio/"])
	app_env.Append(LIBPATH = ["modules/fluxus-midi/"])
	app_env.Append(LIBPATH = ["libfluxus/"])
	app_env.Append(LIBS = ["fluxus-engine_ss"])
	app_env.Append(LIBS = ["fluxus-osc_ss"])
	app_env.Append(LIBS = ["fluxus-audio_ss"])
	app_env.Append(LIBS = ["fluxus-midi_ss"])
	
	if static_everything:
		# this is all a bit fragile, due to the need to put all the 
		# dependancies of the dynamic libraries here, in the right order
		
		# start off with the first options
		linkcom = " -static-libgcc -Wl,-Bstatic -lstdc++ -ljack -lrt -lasound \
			 -lfluxus -lode -lsndfile -lFLAC -lvorbis -lvorbisenc -logg -lpthread_nonshared "
	
		app_env['LIBS'].remove("pthread")
		app_env['LIBS'].remove("dl")
                app_env['LIBS'].remove("ode")
                app_env['LIBS'].remove("sndfile")
	
		# now go through the rest of the libs, removing them from 
		# the environment at the same time
		for i in " GLEW GLU glut asound m fftw3 mzscheme3m png tiff \
					jpeg freetype lo z ".split():
			app_env['LIBS'].remove(i)
			linkcom+="-l"+i+" "
	
		# add the remaining dependancies
		app_env.Append(LINKCOM = linkcom + ' -Wl,-Bdynamic')
	else:
		# statically link in mzscheme only
		if env['PLATFORM'] == 'win32':
			app_env['LIBS'].remove('libmzsch3m_6ncc9s')
		else:
			app_env['LIBS'].remove('mzscheme3m')

		app_env.Append(LINKCOM = ' -Wl,-Bstatic -lmzscheme3m -Wl,-Bdynamic')
		
		# have to add the libs needed by the fluxus modules here
		app_env.Append(LIBS = ["fluxus"])
		app_env.Append(LIBS = ["jack"])
		app_env.Append(LIBS = ["asound"])
		app_env.Append(LIBS = ["ode"])

app_env.Program(source = Source, target = Target)

################################################################################
# Build everything else
# call the core library builder and the scheme modules

build_dirs = ['libfluxus', 'modules']
if env['PLATFORM'] != 'win32':
  build_dirs += ['fluxa']
  if addons: build_dirs += ['addons']

SConscript(dirs = build_dirs,
         exports = ["env", "CollectsInstall", "DataInstall", "MZDYN", "BinInstall", \
         "BinaryModulesLocation", "static_modules", "static_ode", "racket_framework", "Prefix"])

################################################################################
# documentation

Docs = ["docs/fluxus-documentation-en.pdf",
	"docs/fluxus-documentation-fr.pdf",
	"AUTHORS", "CHANGES", "COPYING", "LICENCE", "README"]

Examples = [ "examples" ]

################################################################################
# packaging / installing
if env['PLATFORM'] == 'darwin' and GetOption('app'):
        sys.path.append(os.path.abspath('packages/macos'))
        from osxbundle import *
        TOOL_BUNDLE(env)
        # add dynamic libs
        frameworks = [] #['/Library/Frameworks/Jackmp.framework']
        if racket_framework:
            frameworks += [RacketLib + '/Racket.framework']
        dylibs = [ '%s/lib/liblo.dylib' % Prefix]

        env.Alias('app', env.MakeBundle('Fluxus.app',
                                        Target,
                                        'key',
                                        'packages/macos/fluxus-Info.plist',
                                        dylibs = dylibs,
                                        frameworks = frameworks,
                                        resources = [],
                                        typecode = 'APPL',
                                        icon_file = 'packages/macos/fluxus.icns'))
        env.Clean('Fluxus.app', 'Fluxus.app')
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
        frameworks = [] #[ '/Library/Frameworks/Jackmp.framework']
        dylibs = [ '%s/lib/liblo.dylib' % Prefix]
        env.Alias('app', env.MakeBundle('Fluxa.app',
                                        'fluxa/fluxa',
                                        'key',
                                        'packages/macos/fluxa-Info.plist',
                                        dylibs = dylibs,
                                        frameworks = frameworks,
                                        typecode='APPL',
                                        icon_file='packages/macos/fluxa.icns'))
        env.Clean('Fluxa.app', 'Fluxa.app')

if env['PLATFORM'] == 'win32':
	Target += '.exe'

env.Install(DocsInstall, Docs)
env.Install(DocsInstall, Examples)
env.Install(Install, Target)
env.Alias('install', [DESTDIR + Prefix, CollectsInstall])

