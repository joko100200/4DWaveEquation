do for [ii=0:200]{ 
        set ticslevel 0
        set zrange [-2.5:2.5]
        splot 'xyztphi.out' every :::ii::ii
}










