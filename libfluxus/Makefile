CC	=	gcc
CXX	=	g++
CFLAGS	=	-pipe -Wall -O3 -ffast-math -DNO_DEBUG -Wno-unused  -fPIC -I/usr/local/include -I/usr/X11R6/include
CXXFLAGS=	-ggdb -pipe -Wall -O3 -ffast-math -DNO_DEBUG -Wno-unused  -fPIC -I/usr/local/include -I/usr/X11R6/include
INCPATH	=	
LINK	=	g++
LFLAGS	=

LIBS	=   -L/usr/local/lib -Wl,-rpath,/usr/local/lib -L/usr/X11R6/lib \
	-shared -lglut -lGL -lGLU -lz -lpng -lm -lXi -lXmu -lXext -lX11

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

HEADERS =	src/GraphicsUtils.h \
		src/Lifeforms.h \
		src/PNGLoader.h \
		src/PolyPrimitive.h \
		src/TextPrimitive.h \
		src/CompiledPrimitive.h \
		src/LinePrimitive.h \
		src/Primitive.h \
		src/Light.h \
		src/Renderer.h \
		src/SceneGraph.h \
		src/State.h \
		src/TexturePainter.h \
		src/Tree.h \
		src/dada.h 
		
SOURCES =	src/GraphicsUtils.cpp \
		src/Lifeforms.cpp \
		src/PNGLoader.cpp \
		src/PolyPrimitive.cpp \
		src/TextPrimitive.cpp \
		src/CompiledPrimitive.cpp \
		src/LinePrimitive.cpp \
		src/Primitive.cpp \
		src/Light.cpp \
		src/Renderer.cpp \
		src/SceneGraph.cpp \
		src/State.cpp \
		src/TexturePainter.cpp \
		src/Tree.cpp \
		src/dada.cpp 
		
OBJECTS =  src/GraphicsUtils.o \
		src/Lifeforms.o \
		src/PNGLoader.o \
		src/PolyPrimitive.o \
		src/TextPrimitive.o \
		src/CompiledPrimitive.o \
		src/LinePrimitive.o \
		src/Primitive.o \
		src/Light.o \
		src/Renderer.o \
		src/SceneGraph.o \
		src/State.o \
		src/TexturePainter.o \
		src/Tree.o \
		src/dada.o 

TARGET	=	libfluxus.so
STATICTARGET=	libfluxus.a

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
	-rm -f $(OBJECTS) $(OBJMOC) $(SRCMOC) $(UICIMPLS) $(UICDECLS) $(TARGET) $(STARGET)
	-rm -f *~ core

static:
	ar -cr $(STATICTARGET) $(OBJECTS)

install: all static
	cp $(TARGET) $(libdir)
	cp $(STATICTARGET) $(libdir)
	cp src/*.h /usr/local/include/fluxus

uninstall:
	rm -f $(libdir)/$(TARGET)

####### Compile

src/GraphicsUtils.o : src/GraphicsUtils.cpp \
	src/GraphicsUtils.h \
	src/PolyPrimitive.h

src/Lifeforms.o : src/Lifeforms.cpp \
	src/Lifeforms.h \
	src/Lifeforms.h \
	src/Renderer.h

src/PNGLoader.o : src/PNGLoader.cpp \
	src/PNGLoader.h

src/PolyPrimitive.o : src/PolyPrimitive.cpp \
	src/PolyPrimitive.h \
	src/Renderer.h \
	src/Primitive.h

src/TextPrimitive.o : src/TextPrimitive.cpp \
	src/TextPrimitive.h \
	src/PolyPrimitive.h \
	src/Renderer.h \
	src/Primitive.h

src/CompiledPrimitive.o : src/CompiledPrimitive.cpp \
	src/CompiledPrimitive.h \
	src/Renderer.h \
	src/Primitive.h

src/LinePrimitive.o : src/LinePrimitive.cpp \
	src/LinePrimitive.h \
	src/Renderer.h \
	src/Primitive.h

src/Primitive.o : src/Primitive.cpp \
	src/Primitive.h \
	src/State.h

src/Light.o : src/Light.cpp \
	src/Light.h 

src/Renderer.o : src/Renderer.cpp \
	src/Renderer.h \
	src/dada.h \
	src/SceneGraph.h \
	src/State.h \
	src/Primitive.h \
	src/PNGLoader.h

src/SceneGraph.o : src/SceneGraph.cpp \
	src/SceneGraph.h \
	src/Tree.h \
	src/Primitive.h \
	src/State.h \
	src/TexturePainter.h \
	src/PolyPrimitive.h

src/State.o : src/State.cpp \
	src/Renderer.h \
	src/State.h

src/TexturePainter.o : src/TexturePainter.cpp \
	src/TexturePainter.h \
	src/PNGLoader.h

src/Tree.o : src/Tree.cpp \
	src/Tree.h 

src/dada.o : src/dada.cpp \
	src/dada.h

