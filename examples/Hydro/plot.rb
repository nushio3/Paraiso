#!/usr/bin/env ruby

def gnuplot(fn, ofn)
  STDERR.puts "#{fn} #{ofn}"
  open('tmp.gnu','w'){|fp|
    fp.puts <<GNU
set term png
set out "#{ofn}"
set pm3d
set pm3d map
set cbrange [1.5:4]
set size ratio -1
splot "#{fn}" u 1:2:3
GNU
  }
  `gnuplot tmp.gnu`
end

ARGV.each{|fn|
  ofn = fn[0..-5] + '.png'
  gnuplot(fn, ofn)
}
