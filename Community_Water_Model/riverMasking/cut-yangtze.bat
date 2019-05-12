.\catch\catchment 105.75 28.75 ldd_30min.map zhu1.map
pause
.\catch\catchment 111.25 30.75 ldd_30min.map yi1.map
pause
pcrcalc a2.map = cover(scalar(zhu1.map)*2,scalar(yi1.map))
pause
pcrcalc yichang.map = boolean(if(a2.map eq 1,a2.map))
pause
