# to build up the project just type this in project folder:

mkdir bin/
cp gui.glade bin/
ghc --make -o bin/photosort -outputdir bin/ gui.hs main.hs manager.hs photo.hs -threaded

# and run like this:

cd bin/
./photosort

