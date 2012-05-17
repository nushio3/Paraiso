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
<div style='position:absolute;bottom:10px;right:10px;float:right' align='right'>
This book was of great help when I wrote Paraiso!<br/>
<a href="http://www.amazon.co.jp/gp/product/1593272839/ref=as_li_ss_il?ie=UTF8&tag=nushio-22&linkCode=as2&camp=247&creative=7399&creativeASIN=1593272839"><img border="0" src="http://ws.assoc-amazon.jp/widgets/q?_encoding=UTF8&Format=_SL110_&ASIN=1593272839&MarketPlace=JP&ID=AsinImage&WS=1&tag=nushio-22&ServiceVersion=20070822" ></a><img src="http://www.assoc-amazon.jp/e/ir?t=nushio-22&l=as2&o=9&a=1593272839" width="1" height="1" border="0" alt="" style="border:none !important; margin:0px !important;" />
<a href="http://www.amazon.co.jp/gp/product/4274068854/ref=as_li_ss_il?ie=UTF8&tag=nushio-22&linkCode=as2&camp=247&creative=7399&creativeASIN=4274068854"><img border="0" src="http://ws.assoc-amazon.jp/widgets/q?_encoding=UTF8&Format=_SL110_&ASIN=4274068854&MarketPlace=JP&ID=AsinImage&WS=1&tag=nushio-22&ServiceVersion=20070822" ></a><img src="http://www.assoc-amazon.jp/e/ir?t=nushio-22&l=as2&o=9&a=4274068854" width="1" height="1" border="0" alt="" style="border:none !important; margin:0px !important;" />
</div>
</body></html>
ENDHTML

