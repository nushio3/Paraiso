set term png
set out 'heart.png'
set pm3d
set pm3d map
set size ratio -1
set palette rgbformulae 3,0,6
splot 'heart.txt' t ''
