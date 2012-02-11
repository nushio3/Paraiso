#!/usr/bin/env ruby

OutputDir = 'exam-result'

`mkdir -p #{OutputDir}`

def exam(genomeFn, resolution)
  genomeBody = genomeFn.split('/')[-1].split('.')[0]
  
  src = open('HydroMainTemplate.hs','r') {|fp| fp.read }
  src.gsub!('1024 :~ 1024', "#{resolution} :~ #{resolution}")
  open('HydroMain.hs','w') {|fp| fp.puts src }

  `cp #{genomeFn} your.dna`
  `make massive-test`
  `ls -1 *.exam`.split(/\n/).each{|fn|
    `mkdir -p #{OutputDir}/#{genomeBody}/`
    `mv #{fn} #{OutputDir}/#{genomeBody}/#{resolution}-#{fn}`
  }
end


`ls -1 examinee/*.dna`.split(/\n/).each{|genomeFn|
  genomeFn.strip!
  [128,256,512,1024].each{|resolution|
    exam(genomeFn,resolution)
    x = gets
  }
}
