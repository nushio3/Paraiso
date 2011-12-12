#!/usr/bin/env ruby

[0,1,2].each{|instance|
  [true,false].each{|island|
    [false,true].each{|triang|
      tag = ''
      tag += if triang then '-wt' else '-nt' end
      tag += if island then 'i' else 'c' end
      tag += "-#{instance}"

      dir = '/work0/t2g-ppc-all/nushio/GACT' + tag

      no3 = ''
      no3 = '--no3' unless triang
      
      islandFlag = ''
      islandFlag = '--island' if island

      # job = ''
      # job = '-i genomeBank/Izanami-Gen2.dna'
      job = "-n 10 -s stat#{tag}"
      puts "./make_task.rb -w #{dir} #{no3} #{job} #{islandFlag}"
    }
  }
}
