#include <FL/Fl_Text_Editor.h>
#include <string>

using namespace std;

class Fl_Code_Editor : public Fl_Text_Editor
{
public:
	Fl_Code_Editor(int X, int Y, int W, int H, const char* l = 0);
	~Fl_Code_Editor();
	
	virtual int handle(int event);
private:
	string m_OpenChars;
	string m_CloseChars;
	
	void ParseOpenParentheses(int pos, int type);
	void ParseCloseParentheses(int pos, int type);
};
