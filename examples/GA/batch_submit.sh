cd /home/usr5/11ITA066/Paraiso/examples/GA

rm GAATCT.e*
rm GAATCT.o*
mv GAATCTsub.*  /work0/t2g-ppc-all/nushio/sub-log
./batch_submit.rb | bash
#t2sub -N GAATCTsub -q G  -W group_list=t2g-ppc-all  -l select=1:gpus=1:mem=21gb -l walltime=03:00:00 ./batch_submit.sh
