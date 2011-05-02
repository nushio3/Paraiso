cabal configure
cabal hscolour
cabal haddock --internal '--haddock-options=--source-base=src/ --source-module=src/%{MODULE/./-}.html --source-entity=src/%{MODULE/./-}.html#%N'  --html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html' 
