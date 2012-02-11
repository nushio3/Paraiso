#!/usr/bin/env ruby

OutputDir = 'exam-result'

`mkdir -p #{OutputDir}`


def sh(cmd)
  STDERR.puts cmd
  system cmd
end

def exam(genomeFn, resolution)
  genomeBody = genomeFn.split('/')[-1].split('.')[0]
  
  src = open('HydroMainTemplate.hs','r') {|fp| fp.read }
  src.gsub!('1024 :~ 1024', "#{resolution} :~ #{resolution}")
  open('HydroMain.hs','w') {|fp| fp.puts src }

  sh "cp #{genomeFn} your.dna"
  sh "make exam"
  sh "mkdir -p #{OutputDir}/#{genomeBody}/"
  `ls -1 *.exam`.split(/\n/).each{|fn|
    `mv #{fn} #{OutputDir}/#{genomeBody}/#{resolution}-#{fn}`
  }
end


`ls -1 examinee/*.dna`.split(/\n/).each{|genomeFn|
  genomeFn.strip!
  [128,256,512,1024].each{|resolution|
    exam(genomeFn,resolution)
  }
}
