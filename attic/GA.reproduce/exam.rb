#!/usr/bin/env ruby

def exam(genomeFn, resolution)
  src = open('HydroMainTemplate.hs','r') {|fp| fp.read }
  src.gsub!('1024 :~ 1024', "#{resolution} :~ #{resolution}")
  open('HydroMain.hs','w') {|fp| fp.puts src }

  `cp #{genomeFn} your.dna`
end


`ls -1 examinee/*.dna`.split(/\n/).each{|genomeFn|
  genomeFn.strip!
  [128,256,512,1024].each{|resolution|
    exam(genomeFn,resolution)
    x = gets
  }
}
