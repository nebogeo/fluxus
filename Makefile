OS_NAME = $(shell uname -s)
ifneq ($(OS_NAME),Linux)
ifneq ($(OS_NAME),Darwin)
FIXME
endif
endif

CC	=	gcc
CXX	=	g++
FLUXINC =	-Isrc -Ilibfluxphysics/src -Ilibfluxus/src

ifeq ($(OS_NAME),Linux)
CFLAGS	=	-pipe -Wall -O3 -ffast-math -DNO_DEBUG -Wno-unused  -fPIC 
CXXFLAGS=	-pipe -Wall -O3 -ffast-math -DNO_DEBUG -Wno-unused  -fPIC 
INCPATH	=	$(FLUXINC) -I/usr/local/include -I/usr/X11R6/include
LINK	=	g++
LFLAGS	=

LIBS	=   -L/usr/local/lib -Wl,-rpath,/usr/local/lib -L/usr/X11R6/lib \
	-lglut -lGL -lGLU -lfltk -lfltk_gl -lguile -lrfftw -lode \
	-lfftw -lz -lpng -lm -lXi -lXmu -lXext -lX11 -lportaudio -ltiff -lsndfile
endif

ifeq ($(OS_NAME),Darwin)
CFLAGS	=	-pipe -Wall -O3 -ffast-math -DNO_DEBUG -Wno-unused  -fPIC
CXXFLAGS=	-pipe -Wall -O3 -ffast-math -DNO_DEBUG -Wno-unused  -fPIC
INCPATH	=	$(FLUXINC) -I/usr/local/include -I/sw/include -I../portaudio_v18_1/pa_common -I../fltk-1.1.4 -I../ode/include
LINK	=	g++
LFLAGS	=

LIBS	=   -L/usr/local/lib -L../fltk-1.1.4/lib -L../ode/lib -L/sw/lib \
			-lfltk -lfltk_gl -lguile -lrfftw -lfftw -lz -lpng -lm -lode -lobjc -ltiff -lsndfile\
			-F../portaudio_v18_1/pa_mac_core/build -framework portaudio -framework AGL -framework GLUT -framework OpenGL -framework Carbon
endif

###### Autoconf variables

prefix = /usr/local
exec_prefix = ${prefix}

bindir = ${exec_prefix}/bin
sbindir = ${exec_prefix}/sbin
libexecdir = ${exec_prefix}/libexec
datadir = ${prefix}/share
sysconfdir = ${prefix}/etc
sharedstatedir = ${prefix}/com
localstatedir = ${prefix}/var
libdir = ${exec_prefix}/lib
infodir = ${prefix}/info
mandir = ${prefix}/man

####### Files

HEADERS =	libfluxusphysics/src/Physics.h \
		libfluxus/src/GraphicsUtils.h \
		libfluxus/src/Lifeforms.h \
		libfluxus/src/PNGLoader.h \
		libfluxus/src/PolyPrimitive.h \
		libfluxus/src/Primitive.h \
		libfluxus/src/Renderer.h \
		libfluxus/src/SceneGraph.h \
		libfluxus/src/State.h \
		libfluxus/src/TexturePainter.h \
		libfluxus/src/Tree.h \
		libfluxus/src/Light.h \
		libfluxus/src/LinePrimitive.h \
		libfluxus/src/CompiledPrimitive.h \
		libfluxus/src/TextPrimitive.h \
		libfluxus/src/dada.h \
		src/AudioCollector.h \
		src/GUI.h \
		src/TurtleBuilder.h \
		src/Fl_Code_Editor.h \
		src/Utils.h \
		src/PortAudioClient.h
		
SOURCES =	libfluxphysics/src/Physics.cpp \
		libfluxus/src/GraphicsUtils.cpp \
		libfluxus/src/Lifeforms.cpp \
		libfluxus/src/PNGLoader.cpp \
		libfluxus/src/PolyPrimitive.cpp \
		libfluxus/src/Primitive.cpp \
		libfluxus/src/Renderer.cpp \
		libfluxus/src/SceneGraph.cpp \
		libfluxus/src/State.cpp \
		libfluxus/src/TexturePainter.cpp \
		libfluxus/src/Tree.cpp \
		libfluxus/src/Light.cpp \
		libfluxus/src/LinePrimitive.cpp \
		libfluxus/src/CompiledPrimitive.cpp \
		libfluxus/src/TextPrimitive.cpp \
		libfluxus/src/dada.cpp \
		src/AudioCollector.cpp \
		src/GUI.cpp \
		src/PortAudioClient.cpp \
		src/TurtleBuilder.cpp \
		src/Fl_Code_Editor.cpp \
		src/Utils.cpp \
		src/main.cpp 
		
OBJECTS =       libfluxphysics/src/Physics.o \
		libfluxus/src/GraphicsUtils.o \
		libfluxus/src/Lifeforms.o \
		libfluxus/src/PNGLoader.o \
		libfluxus/src/PolyPrimitive.o \
		libfluxus/src/Primitive.o \
		libfluxus/src/Renderer.o \
		libfluxus/src/SceneGraph.o \
		libfluxus/src/State.o \
		libfluxus/src/TexturePainter.o \
		libfluxus/src/Tree.o \
		libfluxus/src/Light.o \
		libfluxus/src/LinePrimitive.o \
		libfluxus/src/CompiledPrimitive.o \
		libfluxus/src/TextPrimitive.o \
		libfluxus/src/dada.o \
		src/AudioCollector.o \
		src/GUI.o \
		src/PortAudioClient.o \
		src/TurtleBuilder.o \
		src/Utils.o \
		src/Fl_Code_Editor.o \
		src/main.o

TARGET	=	fluxus

####### Implicit rules

.SUFFIXES: .cpp .cxx .cc .C .c

.cpp.o:
	$(CXX) -c $(CXXFLAGS) $(INCPATH) -o $@ $<

.cxx.o:
	$(CXX) -c $(CXXFLAGS) $(INCPATH) -o $@ $<

.cc.o:
	$(CXX) -c $(CXXFLAGS) $(INCPATH) -o $@ $<

.C.o:
	$(CXX) -c $(CXXFLAGS) $(INCPATH) -o $@ $<

.c.o:
	$(CC) -c $(CFLAGS) $(INCPATH) -o $@ $<

####### Build rules

all: $(TARGET)

$(TARGET): $(UICDECLS) $(OBJECTS) $(OBJMOC)
	$(LINK) $(LFLAGS) -o $(TARGET) $(OBJECTS) $(OBJMOC) $(LIBS)

with-jack: $(UICDECLS) $(OBJECTS) $(OBJMOC) 
	$(LINK) $(LFLAGS) -o $(TARGET) $(OBJECTS) $(OBJMOC) $(JACKLIBS)

clean:
	-rm -f $(OBJECTS) $(OBJMOC) $(SRCMOC) $(UICIMPLS) $(UICDECLS) $(TARGET)
	-rm -f *~ core
	
install: all
	cp $(TARGET) $(bindir)
	
uninstall:
	rm -f $(bindir)/fluxus

####### Compile

libfluxphysics/src/Physics.o  : libfluxphysics/src/Physics.cpp libfluxphysics/src/Physics.h

libfluxus/src/GraphicsUtils.o : libfluxus/src/GraphicsUtils.cpp \
	libfluxus/src/GraphicsUtils.h \
	libfluxus/src/PolyPrimitive.h

libfluxus/src/Lifeforms.o : libfluxus/src/Lifeforms.cpp \
	libfluxus/src/Lifeforms.h \
	libfluxus/src/Lifeforms.h \
	libfluxus/src/Renderer.h

libfluxus/src/PNGLoader.o : libfluxus/src/PNGLoader.cpp \
	libfluxus/src/PNGLoader.h

libfluxus/src/PolyPrimitive.o : libfluxus/src/PolyPrimitive.cpp \
	libfluxus/src/PolyPrimitive.h \
	libfluxus/src/Renderer.h \
	libfluxus/src/Primitive.h

libfluxus/src/Primitive.o : libfluxus/src/Primitive.cpp \
	libfluxus/src/Primitive.h \
	libfluxus/src/State.h

libfluxus/src/Renderer.o : libfluxus/src/Renderer.cpp \
	libfluxus/src/Renderer.h \
	libfluxus/src/dada.h \
	libfluxus/src/SceneGraph.h \
	libfluxus/src/State.h \
	libfluxus/src/Primitive.h \
	libfluxus/src/PNGLoader.h

libfluxus/src/SceneGraph.o : libfluxus/src/SceneGraph.cpp \
	libfluxus/src/SceneGraph.h \
	libfluxus/src/Tree.h \
	libfluxus/src/Primitive.h \
	libfluxus/src/State.h \
	libfluxus/src/TexturePainter.h \
	libfluxus/src/PolyPrimitive.h

libfluxus/src/State.o : libfluxus/src/State.cpp \
	libfluxus/src/Renderer.h \
	libfluxus/src/State.h

libfluxus/src/TexturePainter.o : libfluxus/src/TexturePainter.cpp \
	libfluxus/src/TexturePainter.h \
	libfluxus/src/PNGLoader.h

libfluxus/src/Tree.o : libfluxus/src/Tree.cpp \
	libfluxus/src/Tree.h 

libfluxus/src/dada.o : libfluxus/src/dada.cpp \
	libfluxus/src/dada.h

src/AudioCollector.o : src/AudioCollector.cpp \
	src/AudioCollector.h \
	src/PortAudioClient.h

src/GUI.o : src/GUI.cpp \
	src/GUI.h 

src/PortAudioClient.o : src/PortAudioClient.cpp \
	src/PortAudioClient.h

src/TurtleBuilder.o : src/TurtleBuilder.cpp \
	src/TurtleBuilder.h

src/Fl_Code_Editor.o : src/Fl_Code_Editor.cpp \
	src/Fl_Code_Editor.h

src/Utils.o : src/Utils.cpp \
	src/Utils.h 

src/main.o : src/main.cpp \
	src/GUI.h
