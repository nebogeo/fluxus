rm helpmap.scm 
./makehelpmap.scm $1 ../ helpmap.scm
rm fluxus-all.texi 
./helpmap2texi.scm 
makeinfo --css-include=fluxusdoc.css --html fluxus-all.texi
cp images fluxus -R
