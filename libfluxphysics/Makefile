CC	=	gcc
CXX	=	g++
CFLAGS	=	-pipe -Wall -O3 -ffast-math -DNO_DEBUG -Wno-unused  -fPIC -I/usr/local/include -I/usr/X11R6/include
CXXFLAGS=	-pipe -Wall -O3 -ffast-math -DNO_DEBUG -Wno-unused  -fPIC -I/usr/local/include -I/usr/X11R6/include
INCPATH	=	
LINK	=	g++
LFLAGS	=

LIBS	=   -L/usr/local/lib -Wl,-rpath,/usr/local/lib -L/usr/X11R6/lib \
	    -shared -lfluxus -lode

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

HEADERS =	src/Physics.h		
SOURCES =	src/Physics.cpp 		
OBJECTS =       src/Physics.o 
TARGET	=	libfluxphysics.so

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
	
install: $(TARGET)
	cp $(TARGET) $(libdir)
	cp src/*.h /usr/local/include/fluxus

uninstall:
	rm -f $(libdir)/$(TARGET)
	
	
####### Compile

src/Physics.o  : src/Physics.cpp src/Physics.h
