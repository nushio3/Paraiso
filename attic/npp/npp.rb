#!/usr/bin/env ruby

TmpFn = "Tmp.hs"

def gen(str)
  words = str.gsub(/\W/,' ').split(/\s+/)
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
input = #{str}
    where  
      #{genvars}

main :: IO ()
main = do
  putStrLn $ gen input
HASKELL
  }
  `runhaskell #{TmpFn}`
end

while line = gets
  puts gen(line)
end
