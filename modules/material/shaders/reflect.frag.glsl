uniform vec3 AmbientColour;
uniform vec3 DiffuseColour;
uniform vec3 SpecularColour;
uniform float AmbientIntensity;
uniform float DiffuseIntensity;
uniform samplerCube ReflectionMap;

varying vec3 Normal;
varying vec3 LightVec;
varying vec3 EyeVec;

void main()
{ 
	vec3 nn = normalize(Normal);
	vec3 en = normalize(EyeVec);

	//gl_FragColor = textureCube(ReflectionMap,normalize(reflect(en,nn)));
	gl_FragColor = vec4(normalize(reflect(en,nn)),1);
}
