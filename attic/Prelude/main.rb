#!/usr/bin/env ruby

def readsyn(fn)
  ret = []
  open(fn, 'r'){|fp|
    while line = fp.gets
      if line.index('::')
        ret << line.split('::')[0].split(/\s+/)[-1]
      end
    end
  }
  return ret
end



preludes = readsyn("Prelude.syn")

goods = ["Control.Applicative.syn",
         "Control.Monad.syn",
         "Data.Foldable.syn",
         "Data.Traversable.syn"].map{|fn|
  [fn, readsyn(fn)]
}

hidings = []
preludes.each{|x|
  goods.each{|fn, syns|
    if syns.index(x)
      puts "#{x} is also in #{fn}"
      hidings << x
    end
  }
}

puts hidings.uniq.join(', ')
