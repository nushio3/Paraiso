#!/usr/bin/env ruby

def gnuplot(tstr, fn, ofn)
  STDERR.puts "#{tstr} #{fn} #{ofn}"
  open('tmp.gnu','w'){|fp|
    fp.puts <<GNU
set term png
set out "#{ofn}"
set pm3d
set pm3d map
set cbrange [1.5:4]
set size ratio -1
splot "#{fn}" u 1:2:3 t 'density at t = #{tstr}'
GNU
  }
  `gnuplot tmp.gnu`
end

ARGV.each{|fn|
  ofn = fn[0..-5] + '.png'
  fn =~ /([0-9]+)/
  tstr = sprintf("%.1f",$1.to_f/10)
  gnuplot(tstr, fn, ofn)
}
