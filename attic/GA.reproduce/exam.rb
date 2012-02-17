#!/usr/bin/env ruby

OutputDir = 'exam-result'


def sh(cmd)
  STDERR.puts cmd
  system cmd
end

def submit_exam(genomeFn, resolution)
  genomeBody = genomeFn.split('/')[-1].split('.')[0]
  
  src = open('HydroMainTemplate.hs','r') {|fp| fp.read }
  src.gsub!('1024 :~ 1024', "#{resolution} :~ #{resolution}")
  open('HydroMain.hs','w') {|fp| fp.puts src }

  examDir = "#{OutputDir}/#{genomeBody}/#{resolution}"
  
  sh "mkdir -p #{examDir}"
  sh "cp #{genomeFn} #{examDir}/your.dna"
  sh "cp Makefile *.hs *.cu *.h #{examDir}/"
  sh "ln -s /home/usr5/11ITA066/.nvcc/include/thrust #{examDir}/thrust"

  pwd = `pwd`.strip
  jobFn = "#{examDir}/submit.sh"
  open(jobFn,'w') {|fp|
    fp.puts <<SCRIPT
cd #{pwd}/#{examDir}
make exam
SCRIPT
  }
  sh "chmod 755 #{jobFn}"
  if ARGV.index('-X')
    sh "t2sub -N job-exam -q G  -W group_list=t2g-ppc-all  -l select=1:gpus=3:mem=21gb -l walltime=24:00:00 ./#{jobFn}"
  end
end


`ls -1 examinee/*.dna`.split(/\n/).each{|genomeFn|
  genomeFn.strip!
  [16,32,64,128,256,512,1024].each{|resolution|
    submit_exam(genomeFn,resolution)
  }
}
