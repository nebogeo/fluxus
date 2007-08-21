// a daft little vertex shader to test fluxus

attribute vec3 testcol;
uniform float deformamount;
varying vec3 fragcol;

void main()
{     
     // apply the normal gl modelviewprojection to the vertex
       gl_Position = gl_ModelViewProjectionMatrix*gl_Vertex;
     
     // add our sinewave distortion based on the transformed world
     // coordinates of the vertex
     gl_Position += vec4(sin(gl_Position.y*6),0,0,1)*deformamount*0.5;
     
     // pass the colour through to the fragement shader
     fragcol=testcol;
}
