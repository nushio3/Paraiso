#!/usr/bin/env ruby
commands = <<CMDS
cabal configure
cabal hscolour
cabal haddock '--haddock-options=--source-base=src/ --source-module=src/%{MODULE/./-}.html --source-entity=src/%{MODULE/./-}.html#%N'  --html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html' 
CMDS

commands.split(/\n/).each{|line|
  system line
}

fn = 'dist/doc/html/Paraiso/index.html'

contents = open(fn,'r'){|fp| fp.read}

open(fn,'w'){|fp| fp.puts contents.gsub('</body></html>', <<'ENDHTML')}
</body></html>
ENDHTML

