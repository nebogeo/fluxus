#!/usr/bin/env python
import os

MajorVersion = "0"
MinorVersion = "16"
FluxusVersion = MajorVersion+MinorVersion
Target = "fluxus-0.16"

BundleDir = 'Fluxus.app'

file = os.popen('dirname "`which mzscheme`"')
PLTBin = file.read()
file.close()
PLTPrefix = PLTBin[:-5]

# copy plt collects path manually
print 'copying plt collects path...'
os.system('cp -r "%s"* Fluxus.app/Contents/Resources/collects/' % (PLTPrefix + '/collects/'))

# scheme modules don't get installed for some reason
print 'copying fluxus collects path...'
os.system('cp modules/scheme/*.s* Fluxus.app/Contents/Resources/collects/fluxus-%s/' % FluxusVersion)

# scheme modules don't get installed for some reason
print 'copying help...'
os.system('cp docs/helpmap.scm Fluxus.app/Contents/Resources/collects/fluxus-%s/' % FluxusVersion)

# change dynamic shared library install names
def get_dlibs(target):
	file = os.popen('otool -L %s' % target)
	dlibs = file.readlines()
	file.close()
	return dlibs[1:]

dlibs = get_dlibs('%s/Contents/MacOS/%s' % (BundleDir, Target))

sdylibs = ['liblo', 'libjack']
sframeworks = ['PLT_MzScheme.framework']

for l in dlibs:
    dylib = l.split()[0]
    for slib in sdylibs:
        if ((dylib.find(slib)) > -1) and (dylib.find('@') == -1):
            print 'changing install name %s -> @executable_path/../Frameworks/%s.dylib' % (dylib, slib)
            os.system('install_name_tool -change %s @executable_path/../Frameworks/%s.dylib %s' %
                    (dylib, slib, BundleDir + '/Contents/MacOS/' + Target))

# change the dylib self-references
for l in sdylibs:
    print 'changing self-reference %s.dylib -> @executable_path/../Frameworks/%s.dylib' % \
                    (l, l)
    os.system('install_name_tool -id @executable_path/../Frameworks/%s.dylib %s.dylib' %
                    (l, BundleDir + '/Contents/Frameworks/' + l))

# change frameworks install names
for l in dlibs:
    dylib = l.split()[0]
    for slib in sframeworks:
        if ((dylib.find(slib)) > -1) and (dylib.find('@') == -1):
            print 'changing install name %s -> @executable_path/../Frameworks/%s' % (dylib, dylib)
            os.system('install_name_tool -change %s @executable_path/../Frameworks/%s %s' %
                    (dylib, dylib, BundleDir + '/Contents/MacOS/' + Target))
            print 'changing self-reference %s -> @executable_path/../Frameworks/%s' % (dylib, dylib)
            os.system('install_name_tool -id @executable_path/../Frameworks/%s %s' %
                    (dylib, BundleDir + '/Contents/Frameworks/' + dylib ))

            if slib == 'PLT_MzScheme.framework':
                print 'changing self-reference PLT_MzScheme.framework -> @executable_path/../Frameworks/%s' % dylib
                os.system('install_name_tool -id @executable_path/../Frameworks/%s %s' %
                        ('PLT_MzScheme.framework/PLT_MzScheme',
                         BundleDir + '/Contents/Frameworks/PLT_MzScheme.framework/PLT_MzScheme'))

# change module install names

modules = ['fluxus-engine_ss.dylib', 'fluxus-audio_ss.dylib',
		   'fluxus-midi_ss.dylib', 'fluxus-osc_ss.dylib']

for m in modules:
	print 'changing install names of', m
	modulepath = BundleDir + '/Contents/Resources/collects/fluxus-' + \
					FluxusVersion + \
					'/compiled/native/i386-macosx/3m/' + m
	dlibs = get_dlibs(modulepath)
	for l in dlibs:
		dylib = l.split()[0]
		# change frameworks
		for slib in sframeworks:
			if ((dylib.find(slib)) > -1) and (dylib.find('@') == -1):
				print 'changing install name %s -> @executable_path/../Frameworks/%s' % (dylib, dylib)
				print ('install_name_tool -change %s @executable_path/../Frameworks/%s %s' %
						(dylib, dylib, modulepath))
				os.system('install_name_tool -change %s @executable_path/../Frameworks/%s %s' %
						(dylib, dylib, modulepath))
		# change dylibs
		for slib in sdylibs:
			if ((dylib.find(slib)) > -1) and (dylib.find('@') == -1):
				print 'changing install name %s -> @executable_path/../Frameworks/%s.dylib' % (dylib, slib)
				os.system('install_name_tool -change %s @executable_path/../Frameworks/%s.dylib %s' %
						(dylib, slib, modulepath))


