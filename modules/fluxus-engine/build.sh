mzc --compiler /usr/bin/g++ ++ccf -I../../libfluxus/src --cc src/FluxusEngine.cpp 
mzc --compiler /usr/bin/g++ ++ccf -I../../libfluxus/src --cc src/FluxusGlobalState.cpp 
mzc --compiler /usr/bin/g++ ++ccf -I../../libfluxus/src --cc src/FluxusLocalState.cpp 
mzc --compiler /usr/bin/g++ ++ccf -I../../libfluxus/src --cc src/FluxusMaths.cpp 
mzc --compiler /usr/bin/g++ ++ccf -I../../libfluxus/src --cc src/FluxusPrimitives.cpp 
mzc --compiler /usr/bin/g++ ++ccf -I../../libfluxus/src --cc src/FluxusPData.cpp 
mzc --compiler /usr/bin/g++ ++ccf -I../../libfluxus/src --cc src/FluxusUtils.cpp 
mzc --compiler /usr/bin/g++ ++ccf -I../../libfluxus/src --cc src/FluxusTurtle.cpp 
mzc --compiler /usr/bin/g++ ++ccf -I../../libfluxus/src --cc src/FluxusLights.cpp 
mzc --compiler /usr/bin/g++ ++ccf -I../../libfluxus/src --cc src/FluxusPhysics.cpp 
mzc --compiler /usr/bin/g++ ++ccf -I../../libfluxus/src --cc src/TurtleBuilder.cpp 
mzc --compiler /usr/bin/g++ ++ccf -I../../libfluxus/src --cc src/Common.cpp
		
mzc --linker /usr/bin/g++ --ld fluxus-engine.so \
	FluxusEngine.o \
	FluxusGlobalState.o \
	FluxusLocalState.o \
	FluxusPrimitives.o \
	FluxusMaths.o \
	FluxusPData.o \
	FluxusUtils.o \
	FluxusTurtle.o \
	FluxusLights.o \
	FluxusPhysics.o \
	TurtleBuilder.o \
	Common.o \
	../../libfluxus/libfluxus.a \
	-O3 -lGL -lGLU -lglut -lGLEW -lpng -lode
