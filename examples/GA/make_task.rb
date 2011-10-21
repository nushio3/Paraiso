#!/usr/bin/env ruby

require 'optparse'

Home = `echo $HOME`.strip
WorkDir = '/work0/t2g-ppc-all/nushio/GA'

$genomeBank = {}

class Species
  def mean()
    return @m_mean if @m_mean
    return @m_mean = 0 if @scores.length <= 0
    sum = 0.0
    @scores.each{|x| sum+=x}
    return @m_mean = sum / @scores.length
  end
  def devi()
    return @m_devi if @m_devi
    return @m_devi = 0 if @scores.length <= 1
    vari = 0.0
    @scores.each{|x|
      vari += (x - mean()) ** 2
    }
    return @m_devi = Math::sqrt(vari / (@scores.length - 1))
  end
  def merge(other)
    @m_mean = nil;  @m_devi = nil
    @scores += other.scores
  end
  def stat()
    return sprintf("id:%08d\tn=%d\tscore = %f +/- %f",@id, @scores.length, mean(), devi())
  end
  attr_accessor :id, :dna, :scores, :m_mean, :m_devi
end

def rand_dna()
  ret = 'AT'
  8.times{
    ret += ['A','T','C','G'].sort_by{|x| rand()}[0]
  }
  return ret;
end

def indexToDir(i0)
  i = i0.to_i
  top = i / 1000
  bot = i % 1000
  return  WorkDir + "/" + sprintf("%04d/%04d", top, bot)
end

def loadSpecies(id, dir)
  begin
    ret = Species.new
    ret.id = id
    ret.dna = open(dir + '/your.dna','r'){|fp| fp.read }
    scores = []
    3.times{|gpuid|
      tmp = []
      open(dir + "/stdout#{gpuid}", 'r') {|fp|
        while line = fp.gets
          tmp << line.split(/\s+/)[1].to_f
        end
      }
      scores << tmp
    }
    n = scores.map{|xs|xs.length}.min
    
    ret.scores = []
    n.times{|i| 
      avg = (scores[0][i] + scores[1][i] + scores[2][i])/3
      ret.scores << avg
    }
    return ret
  rescue
    return nil
  end
end

opt = OptionParser.new
$newTasks = 0
opt.on('-n VAL') {|val| $newTasks = val.to_i}

opt.parse!(ARGV)


ctr = 1
freeIndex = 0
loop {
  dir = indexToDir(ctr)
  unless File.exist?(dir)
    freeIndex = ctr
    break
  end
  spec = loadSpecies(ctr, dir)
  if spec
    if $genomeBank[spec.dna]
      $genomeBank[spec.dna].merge(spec)
    else
      $genomeBank[spec.dna] = spec
    end
  end
  ctr+=1
}


$genomeBank.values.sort_by{|spec|spec.mean}.each{|spec| puts spec.stat}
STDERR.puts "free index is #{freeIndex}"


$newTasks.times {|i0|
  i = i0 + freeIndex

  pwd = indexToDir(i)
  `rm -fr #{pwd}`
  `mkdir -p #{pwd}`
  
  open(pwd + '/submit.sh','w') {|fp|
    fp.puts <<SCRIPT
t2sub -N #{rand_dna()} -q G -W group_list=t2g-ppc-all -l select=1:gpus=3:mem=21gb -l walltime=0:10:00 #{pwd}/exec.sh
SCRIPT
  }
  
  
  open(pwd + '/exec.sh','w') {|fp|
    fp.puts <<SCRIPT
cd #{pwd}
make kh-cuda.out > stdout 2> stderr
./kh-cuda.out 0 > stdout0 2> stderr0 &
./kh-cuda.out 1 > stdout1 2> stderr1 &
./kh-cuda.out 2 > stdout2 2> stderr2 
sleep 10
SCRIPT
  }
  


  `chmod 755 #{pwd}/submit.sh`
  `chmod 755 #{pwd}/exec.sh`
  
  `cp Makefile #{pwd}/`
  `cp Hydro.hs #{pwd}/`
  `cp HydroMain.hs #{pwd}/`
  `cp main-kh.cu #{pwd}/`
  `cp get_time.h #{pwd}/`
  `ln -s #{Home}/.nvcc/include/thrust #{pwd}/thrust`
  `mkdir -p #{pwd}/output`
  `./mutate.hs < Izanami.dna > #{pwd}/your.dna`

  open("#{pwd}/family-tree.txt",'w'){|fp|
    parent = open('Izanami.dna','r').read.strip
    fp.puts <<TREE
1P
#{parent}
TREE
  }
  

  `bash #{pwd}/submit.sh`
}
