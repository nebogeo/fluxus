// a daft little fragment shader to test fluxus
// about as simple as it gets

varying vec3 fragcol;

void main()
{
     // apply the input colour (and add alpha manually)
     gl_FragColor = vec4(fragcol.x,fragcol.y,fragcol.z,1);
}
