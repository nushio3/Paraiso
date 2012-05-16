#!/usr/bin/env ruby

require 'optparse'

Home = `echo $HOME`.strip
WorkDir = '/work0/t2g-ppc-all/nushio/GA-S'

`mkdir -p #{WorkDir}`

opt = OptionParser.new
$newTasks = 0
opt.on('-n VAL') {|val| $newTasks = val.to_i}

$statFn = nil
opt.on('-s FILENAME') {|val| $statFn = val}

$injectDNA = nil
opt.on('-i INJECT_FN') {|fn|
  $injectDNA = []
  open(fn.to_s, 'r') {|fp|
    while line = fp.gets
      $injectDNA << line.strip
    end
  }
}

opt.parse!(ARGV)
$newTasks = $injectDNA.length  if $injectDNA 

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

class FsCache
  def initialize()
    @record = []
  end

  def loadSpecies(id, dir)
    begin
      return @record[id] if id < @record.length - 300
      
      ret = Species.new
      ret.id = id
      ret.dna = open(dir + '/your.dna','r'){|fp| fp.read.strip }
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
      return @record[id] = ret
    rescue
      return nil
    end
  end

  attr_accessor :record
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





ctr = 1
freeIndex = 0
ctr2 = 1

CacheFn = "#{WorkDir}/fs.cache"
$fsCache = FsCache.new
if File.exist?(CacheFn)
  open(CacheFn, 'r'){|fp|
    $fsCache = Marshal.load(fp)
  }
end

loop {
  dir = indexToDir(ctr)
  unless File.exist?(dir)
    freeIndex = ctr
    break
  end
  spec = $fsCache.loadSpecies(ctr, dir)
  if (ctr > ctr2 || ctr%1000 == 0) 
    STDERR.puts "scanning: #{ctr}"
    ctr2=2*ctr
  end
  if spec && spec.scores.length > 0
    if $genomeBank[spec.dna]
      $genomeBank[spec.dna].merge(spec)
    else
      $genomeBank[spec.dna] = spec
    end
  end
  ctr+=1
}

open(CacheFn, 'w'){|fp|
  Marshal.dump($fsCache, fp)
}


STDERR.puts "sorting..."
$genomeRanking = $genomeBank.values.sort_by{|spec| [spec.mean, -spec.id]}.reverse

STDERR.puts "sorted"
$genomeRanking.each{|spec| puts spec.stat}

$topMean = $genomeRanking[0].mean if $genomeRanking[0]
$topDevi = 0
$genomeRanking.each{|spec| 
  if spec.devi > 0
    $topDevi = spec.devi
    break
  end
}

if $statFn
  open($statFn, 'w') {|fp|
    $genomeRanking.each{|spec|
      fp.puts "#{spec.id} #{spec.scores.length} #{spec.mean} #{spec.devi}"
    }
  }
end

def randTemp()
  return 0 if $injectDNA
  
  lo = Math::log($topDevi) - 4
  #hi = Math::log($topMean) +2 # + Math::log($genomeBank.length.to_f)   
  hi = Math::log($topMean) + Math::log($genomeBank.length.to_f)   
  return Math::exp(lo + rand() * (hi-lo))
end

def randSpec(temp)
  $genomeBank.values.sort_by{|spec|
    diff = $topMean - spec.mean
    modTemp = temp+$topDevi+spec.devi

    envy = 1 #[1, 10 * (diff +$topDevi+spec.devi) / modTemp].min

    rand() * Math::exp((-diff)/modTemp) * envy
  }[-1]
end


STDERR.puts "free index is #{freeIndex}"


newGenomeBank = {}

$newTasks.times {|i0|
  i = i0 + freeIndex

  pwd = indexToDir(i)
  `rm -fr #{pwd}`
  `mkdir -p #{pwd}`
  
  open(pwd + '/submit.sh','w') {|fp|
    fp.puts <<SCRIPT
t2sub -N #{rand_dna()} -q S -W group_list=t2g-ppc-all -l select=1:gpus=3:mem=21gb -l walltime=0:10:00 #{pwd}/exec.sh
SCRIPT
  }
  
  
  open(pwd + '/exec.sh','w') {|fp|
    fp.puts <<SCRIPT
cd #{pwd}
make kh-cpp.out > stdout 2> stderr
OMP_NUM_THREADS=24 ./kh-cpp.out 0 > stdout0 2> stderr0 
rm ./HydroMain
rm *.o
rm ./kh-cpp.out
SCRIPT
  }
  


  `chmod 755 #{pwd}/submit.sh`
  `chmod 755 #{pwd}/exec.sh`
  
  `cp Makefile #{pwd}/`
  `cp Hydro.hs #{pwd}/`
  `cp HydroMain.hs #{pwd}/`
  `cp main-kh.cu #{pwd}/`
  `cp main-kh.cpp #{pwd}/`
  `cp get_time.h #{pwd}/`
  `ln -s #{Home}/.nvcc/include/thrust #{pwd}/thrust`
  `mkdir -p #{pwd}/output`


  temp = tempOrig = randTemp()
  STDERR.print "             #{sprintf('%0.3f',temp)} "[-16..-1]
  modifiedTemp = ''
  
  coin = rand()

  cmd = if $injectDNA
          :inject
        elsif coin < 0.33333333
          :mutate
        elsif coin < 0.66666666
          :cross
        else
          :triang
        end

  if cmd == :inject
    dna = $injectDNA[i0]
    STDERR.puts "injection #{dna}"
    open("#{pwd}/your.dna", 'w') {|fp|
      fp.puts <<DNA
#{dna}
DNA
    }
    
    open("#{pwd}/family-tree.txt",'w'){|fp|
      fp.puts <<TREE
OP
TREE
    }
    
    
  end
  while cmd == :cross
    a = randSpec(temp)
    b = randSpec(temp)
    100.times{
      break if a.id != b.id
      temp *= 1.2
      b = randSpec(temp)
      modifiedTemp = "(#{temp})"
    }

    dna = `./mutate.hs #{a.dna} #{b.dna}`.strip
    STDERR.puts "cross  #{a.id} #{b.id}  #{modifiedTemp}"
    if $genomeBank[dna] || newGenomeBank[dna]
      STDERR.puts 'duplicate'
      cmd = :mutate
      break
    end
    STDERR.puts 'not duplicate'
    open("#{pwd}/your.dna",'w'){|fp| fp.puts dna}
    open("#{pwd}/family-tree.txt",'w'){|fp|
      fp.puts <<TREE
2P
#{a.dna}
#{b.dna}
TREE
    }
    break
  end
  while cmd == :triang
    xs = [randSpec(temp), randSpec(temp), randSpec(temp)].sort_by{|spec| spec.mean}
    a = xs[0]
    b = xs[1]
    c = xs[2]
    100.times{
      break if a.id != b.id && a.id != c.id && b.id != c.id
      temp *= 1.2
      xs = [randSpec(temp), randSpec(temp), randSpec(temp)].sort_by{|spec| spec.mean}
      a = xs[0]
      b = xs[1]
      c = xs[2]
      modifiedTemp = "(#{temp})"
    }
    
    dna = `./mutate.hs #{a.dna} #{b.dna} #{c.dna}`.strip
    STDERR.puts "triang #{a.id} #{b.id} #{c.id} #{modifiedTemp}"
    if $genomeBank[dna] || newGenomeBank[dna]
      STDERR.puts 'duplicate'
      cmd = :mutate
      break
    end
    STDERR.puts 'not duplicate'
    open("#{pwd}/your.dna",'w'){|fp| fp.puts dna}

    open("#{pwd}/family-tree.txt",'w'){|fp|
      fp.puts <<TREE
3P
#{a.dna}
#{b.dna}
#{c.dna}
TREE
    }
    break
  end
  while cmd == :mutate
    temp = tempOrig
    a = randSpec(temp)

    STDERR.puts "mutate #{a.id}"
    dna = `./mutate.hs #{a.dna}`.strip
    if $genomeBank[dna] || newGenomeBank[dna]
      STDERR.puts 'duplicate'
      next
    end

    STDERR.puts 'not duplicate'
    open("#{pwd}/your.dna",'w'){|fp| fp.puts dna}
    open("#{pwd}/family-tree.txt",'w'){|fp|
      fp.puts <<TREE
1P
#{a.dna}
TREE
    }
    break
  end

  STDERR.puts "submitting..."
  exit() unless system("bash #{pwd}/submit.sh")
  newGenomeBank[dna] = true
}
