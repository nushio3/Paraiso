#!/usr/bin/env ruby

nodes = ''
edges = ''
mode = false

sepstr = 'nodesep = 0.2, ranksep = 0.25'
      

open('output/OM.txt','r'){|fp|
  while line = fp.gets
    if line[0..2] == "***"
      mode = (line.index('proceed')? true : false)
    end

    next unless mode
    if line =~ /^([0-9]+)(.*)/
      id = $1.to_i


      line1 = $2
      line2, outnodes = line1.split('->')

      inst = line2.split('<-')[0].split('{')[0]
      
      inst2 = inst.strip.split(/\s+/)[0]

      outnodes.strip.split(/\s+/).each{|str|
        outid = str.gsub(/\([0-9]+\)/, '').to_i
        option = ''
        option = '[dir=none]' unless inst2.index('DynValue')
        edges += <<DOT
#{id} -> #{outid} #{option};
DOT
      } if outnodes

      inst3 = inst2

      decor = 'shape = plaintext, height=0.2'
      
      if inst2 == 'Arith'
        inst3 = inst.strip.split(/\s+/)[1]
      elsif inst2 == 'Reduce'
        inst3 = inst
      elsif inst2 == 'Imm'
        inst3 = inst
        inst3 = 'Imm pi' if inst.index('3.14')
      elsif inst2 == 'Load' || inst2 == 'Store' 
        static = ['fieldF','fieldG','energy']
        rest = eval(inst.strip.split(/\s+/)[1])
        inst3 = "#{inst2} '#{rest}'"
        decor = 'shape=ellipse, style=dotted, fontcolor="#ff0000"'
        decor += ', rank=min' if inst2 == 'Load'
      end

      if inst.index('DynValue')
        nodes += <<DOT
#{id} [fixedsize = true, width = 0.1, height = 0.1, label = ""];
DOT
      else
        nodes += <<DOT
#{id} [#{decor}, label = "#{inst3}"];
DOT
      end
    end
  end
}



open('graph.dot','w'){|fp|
  fp.puts <<GRAPH
digraph sample{
{rank=same; 2 4};
{rank=same; 50 51};
# 80
graph[#{sepstr}];
#{nodes}
#{edges}
}
GRAPH
}

`dot -Teps graph.dot -o graph.eps`

