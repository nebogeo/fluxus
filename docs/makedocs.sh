rm helpmap.scm 
./makehelpmap.scm ../ helpmap.scm
rm fluxus.texi 
./helpmap2texi.scm 
makeinfo --css-include=fluxusdoc.css --html fluxus.texi
