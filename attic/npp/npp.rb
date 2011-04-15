#!/usr/bin/env ruby

TmpFn = "Tmp.hs"
MagicOpen  = "mMaGiCoPeN"
MagicClose = "mMaGiClOsE"

def gen(str)
  str2 = str.gsub('[',MagicOpen).gsub(']',MagicClose)
  words = str2.gsub(/[^\w\[\]]/,' ').split(/\s+/)
    
  vars = []
  words.each{|word|
    vars << word if word =~ /^[A-Za-z]/
  }
  vars = vars.uniq
  
  

  genvars = "[#{vars.join(',')}] = map Term $ words \"#{vars.join(' ')}\""
  
  open(TmpFn,'w') {|fp|
    fp.puts <<HASKELL
import NPP

input :: Expr
input = #{str2}
    where  
      #{genvars}

main :: IO ()
main = do
  putStrLn $ gen SSE2v2r8 $ optimize input
HASKELL
  }
  `runhaskell #{TmpFn}`.gsub(MagicOpen,'[').gsub(MagicClose,']')
end

while line = gets
  puts gen(line)
end
