uniform vec4 HighlightColour;
uniform vec4 MidColour;
uniform vec4 ShadowColour;
uniform float HighlightSize;
uniform float ShadowSize;

varying vec3 Normal;
varying vec3 LightVec;

void main()
{ 
    float lambert = dot(normalize(LightVec),normalize(Normal));
	vec4 colour = MidColour;
	if (lambert>1-HighlightSize) colour = HighlightColour;
	if (lambert<ShadowSize) colour = ShadowColour;
    gl_FragColor = colour;
}
