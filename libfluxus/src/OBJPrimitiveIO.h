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
 
#ifndef FLUX_OBJ_PRIMITIVE_IO
#define FLUX_OBJ_PRIMITIVE_IO

#include "PrimitiveIO.h"
#include <vector>

namespace Fluxus
{

class OBJPrimitiveIO : public PrimitiveIO
{
public:
	OBJPrimitiveIO();
	virtual ~OBJPrimitiveIO();
	virtual Primitive *FormatRead(const std::string &filename);
	virtual bool FormatWrite(const std::string &filename, const Primitive *ob);
	
private:
	class Indices
	{
	public:
		Indices() : Position(0), Texture(0), Normal(0) {}
		
		bool operator==(const Indices &other) const
		{
			return Position==other.Position &&
				   Texture==other.Texture &&
				   Normal==other.Normal;
		}
		
		unsigned int Position;
		unsigned int Texture;
		unsigned int Normal;
	};
	
	struct Face
	{
		vector<Indices> Index;
	};
	
	unsigned int TokeniseLine(unsigned int pos, vector<string> &output);
	void TokeniseIndices(const string &str, vector<string> &output);
	void ReadVectors(const std::string &code, std::vector<dVector> &output);
	void ReadIndices(std::vector<Face> &output);
	vector<Indices> RemoveDuplicateIndices();
	void ReorderData(const vector<Indices> &unique);
	void UnifyIndices(const vector<Indices> &unique);
	Primitive *MakePrimitive();
	
	void WriteVertices(const std::string &pdataname, const std::string &objname, const Primitive *ob, FILE *file);
	void WriteIndices(const Primitive *ob, FILE *file);

	unsigned int m_DataSize;
	char *m_Data;
	
	vector<Face> m_Faces;
  	vector<dVector> m_Position;
  	vector<dVector> m_Texture;
  	vector<dVector> m_Normal;
	vector<unsigned int> m_Indices;
};

}

#endif
