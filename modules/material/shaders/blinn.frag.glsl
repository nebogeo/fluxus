uniform vec3 LightPos;
uniform vec3 AmbientColour;
uniform vec3 DiffuseColour;
uniform vec3 SpecularColour;
uniform float AmbientIntensity;
uniform float DiffuseIntensity;
uniform float SpecularIntensity;
uniform float Shinyness;

varying vec3 Normal;
varying vec3 Position;
	
void main()
{ 
	vec3 LightVec = LightPos-Position;
	vec3 EyeVec = -Position;
	
	vec3 nl = normalize(LightVec);
	vec3 nn = normalize(Normal);
	vec3 en = normalize(EyeVec);
	
    float diffuse = dot(nl,nn);
	float specular = pow(dot(reflect(nl,nn),en),Shinyness);
	
	gl_FragColor = vec4(AmbientColour*AmbientIntensity + 
				        DiffuseColour*diffuse*DiffuseIntensity +
				        SpecularColour*specular*SpecularIntensity,1);
}
