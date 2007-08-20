#include "PolyGlyph.h"
#include "assert.h"
#include <iostream>

PolyGlyph::PolyGlyph(const string &ttffilename)
{
	FT_Error error;
	error = FT_Init_FreeType(&m_Library);            
	error = FT_New_Face(m_Library, ttffilename.c_str(), 0, &m_Face);
	
	if (error)
	{
		cerr<<"PolyGlyph::PolyGlyph: could not load font: "<<ttffilename<<endl;
		assert(0);
	}
	  
	// use 5pt at 100dpi 
	error = FT_Set_Char_Size(m_Face, 50 * 64, 0, 100, 0); 
	m_Slot = m_Face->glyph;
}

void PolyGlyph::Render(wchar_t ch)
{	
	map<wchar_t,int>::iterator i = m_Cache.find(ch);
	if (i!=m_Cache.end())
	{
		glCallList(i->second);    
	}
	else
	{	
		FT_Error error;
		error = FT_Load_Char(m_Face, ch, FT_LOAD_DEFAULT);
   		if (error) return;
		
		int glList = glGenLists(1);
		GlyphGeometry* geo = new GlyphGeometry;
		BuildGeometry(m_Slot,*geo);
		
        glNewList(glList, GL_COMPILE);
		RenderGeometry(*geo);
		glTranslatef(m_Slot->metrics.horiAdvance,0,0);
        glEndList();
		delete geo;
		
		m_Cache[ch]=glList;
		glCallList(glList);    
	}
}

float PolyGlyph::CharacterWidth(wchar_t ch)
{
	FT_Error error;
	error = FT_Load_Char(m_Face, ch, FT_LOAD_DEFAULT);
    if (error) return 0;
	return m_Slot->metrics.horiAdvance;
}

float PolyGlyph::CharacterHeight(wchar_t ch)
{
	FT_Error error;
	error = FT_Load_Char(m_Face, ch, FT_LOAD_DEFAULT);
    if (error) return 0;
	return m_Slot->metrics.vertAdvance;
}
	
void PolyGlyph::BuildGeometry(const FT_GlyphSlot glyph, GlyphGeometry &geo)
{
	vector<GlyphGeometry::Vec3<double> > points;
	GLUtesselator* t = gluNewTess();

	gluTessCallback(t, GLU_TESS_BEGIN_DATA, (void (*)())PolyGlyph::TessBegin);
	gluTessCallback(t, GLU_TESS_VERTEX_DATA, (void (*)())PolyGlyph::TessVertex);
	gluTessCallback(t, GLU_TESS_COMBINE_DATA, (void (*)())PolyGlyph::TessCombine);
	gluTessCallback(t, GLU_TESS_END_DATA, (void (*)())PolyGlyph::TessEnd);
	gluTessCallback(t, GLU_TESS_ERROR_DATA, (void (*)())PolyGlyph::TessError);

	gluTessProperty(t, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_NONZERO);

	gluTessProperty(t, GLU_TESS_TOLERANCE, 0);
	gluTessNormal(t, 0.0f, 0.0f, 1.0f);
	gluTessBeginPolygon(t, &geo);

	unsigned int start=0;
	for(int c=0; c<glyph->outline.n_contours; c++)
	{
 	    unsigned int end = glyph->outline.contours[c]+1;
		for(unsigned int p = start; p<end; p++)
		{
			points.push_back(GlyphGeometry::Vec3<double>(glyph->outline.points[p].x,glyph->outline.points[p].y,0));
		}
		start=end;
	}
	
	start=0;
	for(int c=0; c<glyph->outline.n_contours; c++)
	{
 	    unsigned int end = glyph->outline.contours[c]+1;
		gluTessBeginContour(t);
		for(unsigned int p = start; p<end; p++)
		{
			gluTessVertex(t, &points[p].x, 
			                 &points[p].x);
		}
		start=end;
		gluTessEndContour(t);
	}

	gluTessEndPolygon(t);
	gluDeleteTess(t);
}

void PolyGlyph::TessError( GLenum errCode, GlyphGeometry* geo)
{
    geo->m_Error=errCode;
}


void PolyGlyph::TessVertex( void* data, GlyphGeometry* geo)
{
	double *ptr = (double*)data;
    geo->m_Meshes[geo->m_Meshes.size()-1].m_Data.push_back(GlyphGeometry::Vec3<float>(ptr[0],ptr[1],ptr[2]));
}


void PolyGlyph::TessCombine( double coords[3], void* vertex_data[4], float weight[4], void** outData, GlyphGeometry* geo)
{
   outData=vertex_data;
}
        

void PolyGlyph::TessBegin( GLenum type, GlyphGeometry* geo)
{
	geo->m_Meshes.push_back(GlyphGeometry::Mesh(type));
}


void PolyGlyph::TessEnd(GlyphGeometry* geo)
{
}

void PolyGlyph::RenderGeometry(const GlyphGeometry &geo)
{
	/*for (vector<GlyphGeometry::Mesh>::const_iterator i=geo.m_Meshes.begin(); i!=geo.m_Meshes.end(); i++)
	{
		glVertexPointer(3,GL_FLOAT,sizeof(float)*3,(void*)(&i->m_Data.begin()->x));
		glDrawArrays(i->m_Type,0,i->m_Data.size());
	}*/
		
	// i don't like em, but display lists + glbegin are faster than vertex arrays
	for (vector<GlyphGeometry::Mesh>::const_iterator i=geo.m_Meshes.begin(); i!=geo.m_Meshes.end(); i++)
	{
		glBegin(i->m_Type);
		for (vector<GlyphGeometry::Vec3<float> >::const_iterator p=i->m_Data.begin(); p!=i->m_Data.end(); p++)
		{
			glVertex3f(p->x,p->y,p->z);
		}
		glEnd();
	}
}

