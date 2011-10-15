cabal configure --enable-tests
cabal build
./dist/build/runtests/runtests --plain $@

