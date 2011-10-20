#!/usr/bin/env ruby

DirName = 'work'

`rm -fr #{DirName}`
`mkdir #{DirName}`

Pwd = `pwd`.strip + '/' + DirName
Home = `echo $HOME`.strip


open(Pwd + '/submit.sh','w') {|fp|
  fp.puts <<SCRIPT
t2sub -N autotuning -q G -W group_list=t2g-ppc-all -l select=1:gpus=3:mem=21gb -l walltime=1:00:00 #{Pwd}/exec.sh
SCRIPT
}


open(Pwd + '/exec.sh','w') {|fp|
  fp.puts <<SCRIPT
cd #{Pwd}
make kh-cuda.out > stdout 2> stderr
./kh-cuda.out 0 > stdout0 2> stderr0 &
./kh-cuda.out 1 > stdout1 2> stderr1 &
./kh-cuda.out 2 > stdout2 2> stderr2 &
SCRIPT
}

`chmod 755 #{Pwd}/submit.sh`
`chmod 755 #{Pwd}/exec.sh`

`cp Makefile #{Pwd}/`
`cp Hydro.hs #{Pwd}/`
`cp HydroMain.hs #{Pwd}/`
`cp main-kh.cu #{Pwd}/`
`ln -s #{Home}/.nvcc/include/thrust #{Pwd}/thrust`
`mkdir -p #{Pwd}/output`

