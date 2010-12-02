// Copyright (C) 2008 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include <cstdlib>
#include <cstdio>
#include <algorithm>

#include "assert.h"
#include "PolyPrimitive.h"
#include "LocatorPrimitive.h"
#include "OBJPrimitiveIO.h"
#include "SceneGraph.h"
#include "Trace.h"

using namespace Fluxus;

OBJPrimitiveIO::OBJPrimitiveIO()
{
}

OBJPrimitiveIO::~OBJPrimitiveIO()
{
	// clear some mem
	m_Position.clear();
	m_Texture.clear();
	m_Normal.clear();
	m_Faces.clear();
}

Primitive *OBJPrimitiveIO::FormatRead(const string &filename)
{
	FILE *file = fopen(filename.c_str(),"r");
	if (file==NULL)
	{
		Trace::Stream<<"Cannot open .obj file: "<<filename<<endl;
		return NULL;
	}

	fseek(file,0,SEEK_END);
	m_DataSize = ftell(file);
	rewind(file);

	m_Data = new char[m_DataSize+1];
	if (m_DataSize!=fread(m_Data,1,m_DataSize,file))
	{
		Trace::Stream<<"Error reading .obj file: "<<filename<<endl;
		fclose(file);
		return NULL;
	}
	fclose(file);
	m_Data[m_DataSize]='\0';

	m_UnifiedIndices = true;
	ReadOBJ(m_Position, m_Texture, m_Normal, m_Faces);

	// now get rid of the text
	delete[] m_Data;

	// skip processing if all the indices are the same per vertex
	if (m_UnifiedIndices)
	{
		m_Indices.clear();
		for (vector<Face>::const_iterator fi=m_Faces.begin();
				fi!=m_Faces.end(); ++fi)
		{
			for (vector<Indices>::const_iterator ii=fi->Index.begin();
					ii!=fi->Index.end(); ++ii)
			{
				m_Indices.push_back(ii->Position);
			}
		}
	}
	else
	{
		// shuffle stuff around so we only have one set of indices
		vector<Indices> unique=RemoveDuplicateIndices();
		ReorderData(unique);
		UnifyIndices(unique);
	}

	if (m_Faces.empty()) return NULL;

	return MakePrimitive();
}

Primitive *OBJPrimitiveIO::MakePrimitive()
{
	// stick all the data in a primitive

	// what type?
	PolyPrimitive::Type type;
	switch (m_Faces[0].Index.size())
	{
		case 3: type=PolyPrimitive::TRILIST; break;
		case 4: type=PolyPrimitive::QUADS; break;
		default:
		{
			Trace::Stream<<"obj file needs to contain triangles or quads"<<endl;
			return NULL;
		}
	}

	PolyPrimitive *prim = new PolyPrimitive(type);
	prim->Resize(m_Position.size());

	TypedPData<dVector> *pos = new TypedPData<dVector>(m_Position);
	prim->SetDataRaw("p", pos);

	if (!m_Texture.empty())
	{
		assert(m_Texture.size()==m_Position.size());
		TypedPData<dVector> *tex = new TypedPData<dVector>(m_Texture);
		prim->SetDataRaw("t", tex);
	}

	if (!m_Normal.empty())
	{
		assert(m_Normal.size()==m_Position.size());
		TypedPData<dVector> *nrm = new TypedPData<dVector>(m_Normal);
		prim->SetDataRaw("n", nrm);
	}

	prim->GetIndex()=m_Indices;
	prim->SetIndexMode(true);
	return prim;
}

unsigned int OBJPrimitiveIO::TokeniseLine(unsigned int pos, vector<string> &output)
{
	char c=m_Data[pos];
	vector<string> temp;
	temp.push_back("");
	while(c!='\n' && pos<m_DataSize)
	{
		if (c==' ' && *temp.rbegin()!="") temp.push_back("");
		else temp.rbegin()->push_back(c);
		c=m_Data[++pos];
	}

	// get rid of whitespace
	output.clear();
	for(vector<string>::iterator i=temp.begin(); i!=temp.end(); ++i)
	{
		if (*i!="")	output.push_back(*i);
	}

	return pos+1;
}

void OBJPrimitiveIO::TokeniseIndices(const string &str, vector<string> &output)
{
	unsigned int pos=0;
	output.clear();
	output.push_back("");
	while(pos<str.size())
	{
		char c=str[pos++];
		if (c==' ' || c=='/') output.push_back("");
		else output.rbegin()->push_back(c);
	}
}

void OBJPrimitiveIO::ReadOBJ(std::vector<dVector> &positions,
		std::vector<dVector> &textures,
		std::vector<dVector> &normals,
		std::vector<Face> &faces)
{
	unsigned int pos=0;
	positions.clear();
	textures.clear();
	normals.clear();
	faces.clear();

	while (pos<m_DataSize)
	{
		vector<string> tokens;
		pos = TokeniseLine(pos, tokens);
		if (tokens.empty())
			continue;

		if ((tokens[0] == "v") && (tokens.size() == 4))
		{
			positions.push_back(dVector(atof(tokens[1].c_str()),
									 atof(tokens[2].c_str()),
									 atof(tokens[3].c_str())));
		}
		else if (tokens[0] == "vt")
		{
			if (tokens.size() == 4)
			{
				textures.push_back(dVector(atof(tokens[1].c_str()),
										 atof(tokens[2].c_str()),
										 atof(tokens[3].c_str())));
			}
			else if (tokens.size() == 3)
			{
				textures.push_back(dVector(atof(tokens[1].c_str()),
										 atof(tokens[2].c_str()),
										 0));
			}
		}
		else if ((tokens[0] == "vn") && (tokens.size() == 4))
		{
			normals.push_back(dVector(atof(tokens[1].c_str()),
									 atof(tokens[2].c_str()),
									 atof(tokens[3].c_str())));
		}
		else if (tokens[0] == "f")
		{
			Face f;
			for(unsigned int i=1; i<tokens.size(); i++)
			{
				vector<string> itokens;
				TokeniseIndices(tokens[i],itokens);
				if (itokens.size()==3)
				{
					Indices ind;
					if (itokens[0]!="") ind.Position=(unsigned int)atof(itokens[0].c_str())-1;
					if (itokens[1]!="") ind.Texture=(unsigned int)atof(itokens[1].c_str())-1;
					if (itokens[2]!="") ind.Normal=(unsigned int)atof(itokens[2].c_str())-1;
					f.Index.push_back(ind);

					if ((ind.Position != ind.Texture) ||
						(ind.Position != ind.Normal) ||
						(ind.Texture != ind.Normal))
					{
						m_UnifiedIndices = false;
					}
				}
				else if (itokens.size()==2)
				{
					Indices ind;
					if (itokens[0]!="") ind.Position=(unsigned int)atof(itokens[0].c_str())-1;
					if (itokens[1]!="") ind.Texture=(unsigned int)atof(itokens[1].c_str())-1;
					f.Index.push_back(ind);

					if (ind.Position != ind.Texture)
					{
						m_UnifiedIndices = false;
					}
				}
				else if (itokens.size()==1)
				{
					Indices ind;
					if (itokens[0]!="") ind.Position=(unsigned int)atof(itokens[0].c_str())-1;
					f.Index.push_back(ind);
				}
				else
				{
					Trace::Stream<<"Wrong number of indices in .obj file ("<<itokens.size()<<")"<<endl;
				}
			}

			// subdivide polygons to triangles
			if (f.Index.size() > 3)
			{
				Face tri;
				for (unsigned i = 0; i < 3; i++)
				{
					tri.Index.push_back(f.Index[i]);
				}
				faces.push_back(tri);

				for (unsigned i = 3; i < f.Index.size(); i++)
				{
					tri.Index.erase(tri.Index.begin() + 1);
					tri.Index.push_back(f.Index[i]);
					faces.push_back(tri);
				}
			}
			else
			{
				faces.push_back(f);
			}
		}
	}
}

vector<OBJPrimitiveIO::Indices> OBJPrimitiveIO::RemoveDuplicateIndices()
{
	vector<Indices> ret;
	for (vector<Face>::iterator fi=m_Faces.begin();
		fi!=m_Faces.end(); ++fi)
	{
		for (vector<Indices>::iterator ii=fi->Index.begin();
			ii!=fi->Index.end(); ++ii)
		{
			vector<Indices>::iterator result = find(ret.begin(), ret.end(), *ii);
			if (result == ret.end())
			{
				ii->UnifiedIndex = ret.size();
				ret.push_back(*ii);
			}
			else
			{
				ii->UnifiedIndex = result - ret.begin();
			}
		}
	}
	return ret;
}

void OBJPrimitiveIO::ReorderData(const vector<OBJPrimitiveIO::Indices> &unique)
{
	vector<dVector> NewPosition;
	vector<dVector> NewTexture;
	vector<dVector> NewNormal;

	for (vector<Indices>::const_iterator i=unique.begin();
		i!=unique.end(); ++i)
	{
		if (!m_Position.empty()) NewPosition.push_back(m_Position[i->Position]);
		if (!m_Texture.empty()) NewTexture.push_back(m_Texture[i->Texture]);
		if (!m_Normal.empty()) NewNormal.push_back(m_Normal[i->Normal]);
	}

	m_Position=NewPosition;
	m_Texture=NewTexture;
	m_Normal=NewNormal;
}

void OBJPrimitiveIO::UnifyIndices(const vector<Indices> &unique)
{
	m_Indices.clear();
	for (vector<Face>::const_iterator fi=m_Faces.begin();
		fi!=m_Faces.end(); ++fi)
	{
		for (vector<Indices>::const_iterator ii=fi->Index.begin();
			ii!=fi->Index.end(); ++ii)
		{
			m_Indices.push_back(ii->UnifiedIndex);
		}
	}
}

//////////////////////////////////

void OBJPrimitiveIO::WriteVertices(const string &pdataname, const string &objname,
		const Primitive *ob, dMatrix &t, FILE *file)
{
	char line[2048];
	const TypedPData<dVector> *pdata = dynamic_cast<const TypedPData<dVector> *>(ob->GetDataRawConst(pdataname));
	for (unsigned int i=0; i<ob->Size(); i++)
	{
		dVector p = pdata->m_Data[i];
		dVector o = t.transform(p);
		snprintf(line,2048,"%s %f %f %f\n",objname.c_str(),o.x,o.y,o.z);
		fwrite(line,1,strlen(line),file);
	}
}

void OBJPrimitiveIO::WriteIndices(const Primitive *ob, FILE *file)
{
	char line[2048];
	const PolyPrimitive *pp = dynamic_cast<const PolyPrimitive *>(ob);

	int facecount=3;
	switch (pp->GetType())
	{
		case PolyPrimitive::TRILIST: facecount=3; break;
		case PolyPrimitive::QUADS: facecount=4; break;
		default:
		{
			Trace::Stream<<"primitive can only be saved with type triangle-list or quad-list"<<endl;
			return;
		}
	}

	if (pp->IsIndexed())
	{
		vector<unsigned int> indices = pp->GetIndexConst();
		unsigned int i=0;
		while (i < indices.size())
		{
			snprintf(line, 2048,"f ");
			fwrite(line, 1, strlen(line), file);

			for (int c = 0; c < facecount; c++)
			{
				unsigned idx = indices[i] + 1 + m_WIndices;
				snprintf(line, 2048, "%d/%d/%d ", idx, idx, idx);
				fwrite(line, 1, strlen(line), file);
				i++;
			}

			snprintf(line, 2048, "\n");
			fwrite(line, 1, strlen(line), file);
		}
	}
	else
	{
		unsigned int i=0;
		while (i < pp->Size())
		{
			snprintf(line, 2048, "f ");
			fwrite(line, 1, strlen(line), file);

			for (int c = 0; c < facecount; c++)
			{
				unsigned idx = i + 1 + m_WIndices;
				snprintf(line, 2048, "%d/%d/%d ", idx, idx, idx);
				fwrite(line, 1, strlen(line), file);
				i++;
			}

			snprintf(line, 2048, "\n");
			fwrite(line, 1, strlen(line), file);
		}
	}
}

bool OBJPrimitiveIO::FormatWrite(const std::string &filename, const Primitive *ob,
		unsigned id, const SceneGraph &world)
{
	// only save obj files for polyprims
	const PolyPrimitive *pp = dynamic_cast<const PolyPrimitive*>(ob);
	const LocatorPrimitive *lp = dynamic_cast<const LocatorPrimitive*>(ob);
	if (!pp && !lp)
	{
		Trace::Stream<<"Can only save OBJ files from PolyPrimitives"<<endl;
		return false;
	}

	FILE *file = fopen(filename.c_str(), "w");
	if (file == NULL)
	{
		Trace::Stream << "Cannot open .obj file: " << filename << endl;
		return false;
	}

	const int dot_index = filename.rfind(".", filename.size());
	string mfilename = filename.substr(0, dot_index) + ".mtl";

	FILE *mfile = fopen(mfilename.c_str(), "w");
	if (mfile == NULL)
	{
		Trace::Stream << "Cannot open .mtl file: " << mfilename << endl;
		return false;
	}

	m_WIndices = 0;
	FormatWriteOBJ(ob, id, world, file, mfile);

	fclose(file);
	fclose(mfile);

	return true;
}

void OBJPrimitiveIO::FormatWriteOBJ(const Primitive *ob, unsigned id,
		const SceneGraph &world, FILE *file, FILE *mfile)
{
	const LocatorPrimitive *lp = dynamic_cast<const LocatorPrimitive*>(ob);
	Node *node = world.FindNode(id);

	if (lp) // new group for locators
	{
		fprintf(file, "g Group.%03d\n", id); // group
	}
	else // new object
	{
		// material
		fprintf(file, "usemtl Material.%03d\n", id);
		FormatWriteMTL(ob, id, mfile);

		// primitive data
		dMatrix t = world.GetGlobalTransform((SceneNode *)node);
		fprintf(file, "o Object.%03d\n", id); // objects
		WriteVertices("p", "v", ob, t, file);
		t.settranslate(dVector(0, 0, 0));
		WriteVertices("n", "vn", ob, t, file);
		t.init();
		WriteVertices("t", "vt", ob, t, file);
		WriteIndices(ob, file);
		m_WIndices += ob->Size(); // increase global vertex index
	}

	// save children
	for (vector<Node*>::iterator i = node->Children.begin(); i != node->Children.end(); ++i)
	{
		SceneNode *sn = (SceneNode *)*i;
		FormatWriteOBJ(sn->Prim, sn->ID, world, file, mfile);
	}
}

void OBJPrimitiveIO::FormatWriteMTL(const Primitive *ob, unsigned id, FILE *mfile)
{
	const State *state = ob->GetState();
	fprintf(mfile, "newmtl Material.%03d\n", id);
	dColour ambient = state->Ambient;
	fprintf(mfile, "Ka %4.3f %4.3f %4.3f\n", ambient.r, ambient.g, ambient.b);
	dColour diffuse = state->Colour;
	fprintf(mfile, "Kd %4.3f %4.3f %4.3f\n", diffuse.r, diffuse.g, diffuse.b);
	dColour specular = state->Specular;
	fprintf(mfile, "Ks %4.3f %4.3f %4.3f\n", specular.r, specular.g, specular.b);
	fprintf(mfile, "Ns %5.3f\n", state->Shinyness);
	fprintf(mfile, "d %2.1f\n", state->Opacity);
}

