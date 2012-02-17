#!/usr/bin/env ruby

if `t2stat G | grep ' Q '`.split(/\n/).length > 10
  exit
end


ret = []

[2,1,0].each{|instance|
  ['no3','on3','333'].each{|triang|
    [false].each{|island|
      tag = ''
      tag += '-'+triang
      tag += if island then '-i' else '-c' end
      tag += "-#{instance}"

      dir = '/data0/t2g-ppc-all/nushio/GACC' + tag

      no3 = '--' + triang
      
      islandFlag = ''
      islandFlag = '--island' if island


      #job = '-i genomeBank/Izanami-Gen2.dna'
      #ret << "./make_task.rb -w #{dir} #{no3} #{job} #{islandFlag}"
      #job = '-i genomeBank/Shinatsuhiko-Gen2.dna'
      #ret << "./make_task.rb -w #{dir} #{no3} #{job} #{islandFlag}"
      #job = '-i genomeBank/Hayaakitsuhime-Gen2.dna'
      #ret << "./make_task.rb -w #{dir} #{no3} #{job} #{islandFlag}"
      #job = '-i genomeBank/Iwatsuchibiko-Gen2.dna'
      #ret << "./make_task.rb -w #{dir} #{no3} #{job} #{islandFlag}"

      job = "-n 20 -s stat#{tag}"
      ret << "./make_task.rb -w #{dir} #{no3} #{job} #{islandFlag}"
    }
  }
}

# puts ret.sort_by{rand}.join("\n")
puts ret.join("\n")

