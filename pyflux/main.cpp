#include <iostream.h>
#include <Python.h>
#include <fluxus/Renderer.h>
#include <fluxus/GraphicsUtils.h>
#include <fluxus/LinePrimitive.h>
#include <GL/glut.h>

using namespace fluxus;

Renderer renderer;
string EngineCallbackStr;

struct FluxState
{
	char Key;
};

FluxState InputState;

static PyObject *pyflux_show_axis(PyObject *self, PyObject* args)
{
	int s=0;
	if (!PyArg_ParseTuple(args, "i", &s)) return NULL;	
    renderer.ShowAxis(s);
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_push(PyObject *self, PyObject* args)
{
    renderer.PushState();
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_pop(PyObject *self, PyObject* args)
{
    renderer.PopState();
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_identity(PyObject *self, PyObject* args)
{
    Primitive *Grabbed=renderer.Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.init();
    else renderer.GetState()->Transform.init();
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_apply(PyObject *self, PyObject* args)
{
	int id=0;
	if (!PyArg_ParseTuple(args, "i", &id)) return NULL;	
	renderer.GetPrimitive(id)->ApplyTransform();
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_clear(PyObject *self, PyObject* args)
{
	renderer.Clear();
	EngineCallbackStr="";
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_grab(PyObject *self, PyObject* args)
{
	int id=0;
	if (!PyArg_ParseTuple(args, "i", &id)) return NULL;	
	renderer.Grab(id);
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_ungrab(PyObject *self, PyObject* args)
{
	renderer.UnGrab();
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_translate(PyObject *self, PyObject* args)
{
	float v[3];
	if (!PyArg_ParseTuple(args, "(fff)", &v[0], &v[1], &v[2])) return NULL;	
    Primitive *Grabbed=renderer.Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.translate(v[0],v[1],v[2]);
    else renderer.GetState()->Transform.translate(v[0],v[1],v[2]);
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_rotate(PyObject *self, PyObject* args)
{
	float v[3];
	if (!PyArg_ParseTuple(args, "(fff)", &v[0], &v[1], &v[2])) return NULL;	
    Primitive *Grabbed=renderer.Grabbed();	
    if (Grabbed) Grabbed->GetState()->Transform.rotxyz(v[0],v[1],v[2]);
    else renderer.GetState()->Transform.rotxyz(v[0],v[1],v[2]);	
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_scale(PyObject *self, PyObject* args)
{
	float v[3];
	if (!PyArg_ParseTuple(args, "(fff)", &v[0], &v[1], &v[2])) return NULL;
    Primitive *Grabbed=renderer.Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.scale(v[0],v[1],v[2]);
    else renderer.GetState()->Transform.scale(v[0],v[1],v[2]);
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_parent(PyObject *self, PyObject* args)
{
	int i;
	if (!PyArg_ParseTuple(args, "i", &i)) return NULL;
    renderer.GetState()->Parent=i;
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_clear_colour(PyObject *self, PyObject* args)
{
	float col[3];
	if (!PyArg_ParseTuple(args, "(fff)", &col[0], &col[1], &col[2])) return NULL;	
    renderer.SetBGColour(dColour(col[0],col[1],col[2]));
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_colour(PyObject *self, PyObject* args)
{
	float col[3];
	if (!PyArg_ParseTuple(args, "(fff)", &col[0], &col[1], &col[2])) return NULL;	
    Primitive *Grabbed=renderer.Grabbed();
    if (Grabbed) Grabbed->GetState()->Colour=dColour(col[0],col[1],col[2]);
    else renderer.GetState()->Colour=dColour(col[0],col[1],col[2]);
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_opacity(PyObject *self, PyObject* args)
{
    float a=0;
	if (!PyArg_ParseTuple(args, "f", &a)) return NULL;
    Primitive *Grabbed=renderer.Grabbed();
    if (Grabbed) Grabbed->GetState()->Opacity=a;
    else renderer.GetState()->Opacity=a;
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_load_texture(PyObject *self, PyObject* args)
{
	char *name=0;
	if (!PyArg_ParseTuple(args, "s", &name)) return NULL;
	int id=renderer.LoadTexture(name);
    return PyInt_FromLong(id);
}

static PyObject *pyflux_texture(PyObject *self, PyObject* args)
{
	int id=0;
	if (!PyArg_ParseTuple(args, "f", &id)) return NULL;
    renderer.GetState()->Texture=id;
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_build_cube(PyObject *self, PyObject* args)
{
	PolyPrimitive *BoxPrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakeCube(BoxPrim);
    BoxPrim->Finalise();
    return PyInt_FromLong(renderer.AddPrimitive(BoxPrim));
}

static PyObject *pyflux_build_plane(PyObject *self, PyObject* args)
{
	PolyPrimitive *PlanePrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakePlane(PlanePrim);
    PlanePrim->Finalise();    	
    return PyInt_FromLong(renderer.AddPrimitive(PlanePrim));
}

static PyObject *pyflux_build_cylinder(PyObject *self, PyObject* args)
{    
	int hsegments,rsegments;
	if (!PyArg_ParseTuple(args, "ii", &hsegments, &rsegments)) return NULL;	
	PolyPrimitive *CylPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeCylinder(CylPrim, 1, 1, hsegments, rsegments);
    CylPrim->Finalise();    	
    return PyInt_FromLong(renderer.AddPrimitive(CylPrim));
}

static PyObject *pyflux_build_line(PyObject *self, PyObject* args)
{
	float s[3],e[3],swidth=0,ewidth=0;
	if (!PyArg_ParseTuple(args, "(fff)f(fff)f", &s[0], &s[1], &s[2], swidth, &e[0], &e[1], &e[2], ewidth)) return NULL;	
	LinePrimitive *LinePrim = new LinePrimitive;
	LinePrim->SetStart(dVertex(dVector(s[0],s[1],s[2]),dVector(0,1,0)),swidth);
	LinePrim->SetEnd(dVertex(dVector(e[0],e[1],e[2]),dVector(0,1,0)),ewidth);
	return PyInt_FromLong(renderer.AddPrimitive(LinePrim));
}

static PyObject *pyflux_build_sphere(PyObject *self, PyObject* args)
{
	int hsegments,rsegments;
	if (!PyArg_ParseTuple(args, "ii", &hsegments, &rsegments)) return NULL;	
	PolyPrimitive *SphPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeSphere(SphPrim, 1, hsegments, rsegments);
    SphPrim->Finalise();    	
    return PyInt_FromLong(renderer.AddPrimitive(SphPrim));
}

static PyObject *pyflux_ortho(PyObject *self, PyObject* args)
{
	renderer.SetOrtho(true);
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_persp(PyObject *self, PyObject* args)
{
	renderer.SetOrtho(false);
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_backfacecull(PyObject *self, PyObject* args)
{
	int s;
	if (!PyArg_ParseTuple(args, "i", &s)) return NULL;	
	renderer.SetBackFaceCull(s);
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_desiredfps(PyObject *self, PyObject* args)
{
	int s;
	if (!PyArg_ParseTuple(args, "i", &s)) return NULL;	
	renderer.SetDesiredFPS(s);
    Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_print_scene_graph(PyObject *self, PyObject* args)
{
	renderer.PrintSceneGraph();
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_engine_callback(PyObject *self, PyObject* args)
{
	char *s=NULL;
	if (!PyArg_ParseTuple(args, "s", &s)) return NULL;	
	EngineCallbackStr=s;
	Py_INCREF(Py_None);
	return Py_None;
}

static PyObject *pyflux_key_pressed(PyObject *self, PyObject* args)
{
	char *s;
	if (!PyArg_ParseTuple(args, "s", &s)) return NULL;	
	if (s[0]==InputState.Key) return PyInt_FromLong(1);
	else return PyInt_FromLong(0);
}

////////////////////////////////////////////////////////////////////////////

static PyMethodDef pyflux_methods[] = {
	{"show_axis",			pyflux_show_axis,			1,""},
	{"push",				pyflux_push,				METH_NOARGS,""},
	{"pop",					pyflux_pop,					METH_NOARGS,""},
	{"identity",			pyflux_identity,			METH_NOARGS,""},
	{"apply",				pyflux_apply,				METH_NOARGS,""},
	{"clear",				pyflux_clear,				METH_NOARGS,""},
	{"grab",				pyflux_grab,				1,""},
	{"ungrab",				pyflux_ungrab,				METH_NOARGS,""},
	{"translate",			pyflux_translate,			1,""},
	{"rotate",				pyflux_rotate,				1,""},
	{"scale",				pyflux_scale,				1,""},
	{"parent",				pyflux_parent,				1,""},
	{"clear_colour",		pyflux_clear_colour,		1,""},
	{"colour",				pyflux_colour,				1,""},
	{"opacity",				pyflux_opacity,				1,""},
	{"load_texture",		pyflux_load_texture,				1,""},
	{"texture",				pyflux_texture,				1,""},
	{"build_cube",		    pyflux_build_cube,			METH_NOARGS,""},
	{"build_plane",		    pyflux_build_plane,			METH_NOARGS,""},
	{"build_cylinder",		pyflux_build_cylinder,		2,""},
	{"build_line",		    pyflux_build_line,			4,""},
	{"build_sphere",		pyflux_build_sphere,		2,""},
	{"ortho",				pyflux_ortho,				METH_NOARGS,""},
	{"persp",				pyflux_persp,				METH_NOARGS,""},
	{"backfacecull",		pyflux_backfacecull,		1,""},
	{"desiredfps",			pyflux_desiredfps,			1,""},
	{"print_scene_graph",	pyflux_print_scene_graph,	METH_NOARGS,""},
	{"engine_callback",		pyflux_engine_callback,		1,""},
	{"key_pressed",			pyflux_key_pressed,			1,""},
	{NULL,		NULL}		/* sentinel */
};

void initpyflux(void)
{
	PyImport_AddModule("pyflux");
	Py_InitModule("pyflux", pyflux_methods);
}

void Display(void)
{
	if (EngineCallbackStr!="") PyRun_SimpleString(EngineCallbackStr.c_str());
	renderer.Render();
	glutSwapBuffers();
	glutPostRedisplay();
	InputState.Key=0;
}

void Keyboard(unsigned char key, int x, int y)
{
	InputState.Key=key;
}

int main(int argc, char **argv)
{
	// Init the maths lib
	InitDada();

	// Initialize the Python interpreter. 
	Py_Initialize();
	initpyflux();
	
	// GLUT Window Initialization:
	glutInit (&argc, argv);
	glutInitWindowSize (640, 480);
	glutInitDisplayMode ( GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
	glutCreateWindow ("pyflux");

	glutDisplayFunc(Display);
	glutKeyboardFunc(Keyboard);
	
	PySys_SetArgv(argc, argv);
	
	FILE *fd=fopen("pyflux.py","r");
	if (fd)
	{
		fseek(fd,0,SEEK_END);
		int size=ftell(fd)+1;
		fseek(fd,0,SEEK_SET);
		char *script = new char[size];
		fread(script,1,size,fd);		
		fclose(fd);
		script[size]='\0';
		PyRun_SimpleString(script);
	}
	
	glutMainLoop ();

	// Exit, cleaning up the interpreter 
	Py_Exit(0);
}



