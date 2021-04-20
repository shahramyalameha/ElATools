!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL , GENERATET GNUPOLT sqripts for temp. 2D pic.. < 2Dbox v1


SUBROUTINE plot2df(YN,yn_veloc)
   IMPLICIT NONE
   CHARACTER(len=1) :: YN,yn_veloc

   IF (YN.eq."Y")THEN
  
     OPEN(34,FILE="plot2d.gnu")   
       WRITE(34,"(a)")' #!/usr/bin/gnuplot -persist'
       WRITE(34,"(a)")' stats "2dcut_shear.dat" using 2 name "shearA" nooutput'
       WRITE(34,"(a)")' stats "2dcut_shear.dat" using 3 name "shearB" nooutput'
       WRITE(34,"(a)")' stats "2dcut_shear.dat" using 4 name "shearC" nooutput'

       WRITE(34,"(a)")' stats "2dcut_young.dat" using 2 name "youngA" nooutput'
       WRITE(34,"(a)")' stats "2dcut_young.dat" using 3 name "youngB" nooutput'
     
       WRITE(34,"(a)")' stats "2dcut_comp.dat" using 2 name "comA" nooutput'
       WRITE(34,"(a)")' stats "2dcut_comp.dat" using 3 name "comB" nooutput'

       WRITE(34,"(a)")' stats "2dcut_poisson.dat" using 2 name "poiA" nooutput'
       WRITE(34,"(a)")' stats "2dcut_poisson.dat" using 3 name "poiB" nooutput'
       WRITE(34,"(a)")' stats "2dcut_poisson.dat" using 4 name "poiC" nooutput'

       WRITE(34,"(a)")' stats "2dcut_pugh.dat" using 2 name "poghA" nooutput'
       WRITE(34,"(a)")' stats "2dcut_pugh.dat" using 3 name "poghB" nooutput'
       WRITE(34,"(a)")' stats "2dcut_pugh.dat" using 4 name "poghC" nooutput'

       WRITE(34,"(a)")' stats "2dcut_bulk.dat" using 2 name "bulkA" nooutput'
       WRITE(34,"(a)")' stats "2dcut_bulk.dat" using 3 name "bulkB" nooutput'

       WRITE(34,"(a)")'  set term postscript eps enhanced color "Times-Roman, 11"'
       WRITE(34,"(a)")'  set  output "2Dbox.ps"'
       WRITE(34,"(a)")'  set grid x,y'
       WRITE(34,"(a)")'  unset key'
       WRITE(34,"(a)")'  set   autoscale '
       WRITE(34,"(a)")'  set hidden3d'
       WRITE(34,"(a)")'  set bmargin 5'
       WRITE(34,"(a)")'  set border 4095'
       WRITE(34,"(a)")'  set ticslevel 0'
       WRITE(34,"(a)")'  set xtics rotate'
       WRITE(34,"(a)")'  set xlabel "Degree"'
       WRITE(34,"(a)")'  set ylabel "Y"'
       WRITE(34,"(a)")'  set xtics 30'
       WRITE(34,"(a)")'  #set palette rgb 30,31,32'
       WRITE(34,"(a)")'  set multiplot layout 2,3'
       WRITE(34,"(a)")'  set title "(Shear Modulus (GPa))"'
       WRITE(34,"(a)")'  set ylabel "Shear Modulus (GPa)"'
       WRITE(34,"(a)")'    if (shearA_mean == 0 && shearB_mean != 0 && shearC_mean != 0 ) {pl [0:360][] "2dcut_shear.dat" u 1:3  w l  lc  "green" lw 2, "2dcut_shear.dat" u 1:4 w l lc "red"   lw 2   }'
       WRITE(34,"(a)")'    if (shearA_mean != 0 && shearB_mean == 0 && shearC_mean != 0 ) {pl [0:360][] "2dcut_shear.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_shear.dat" u 1:4  w l lc "red"   lw 2  }'
       WRITE(34,"(a)")'    if (shearA_mean != 0 && shearB_mean != 0 && shearC_mean == 0 ) {pl [0:360][] "2dcut_shear.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_shear.dat" u 1:3 w l lc "green" lw 2   }'
       WRITE(34,"(a)")'    if (shearA_mean != 0 && shearB_mean != 0 && shearC_mean != 0 ) {pl [0:360][] "2dcut_shear.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_shear.dat" u 1:3 w l lc "green" lw 2, "2dcut_shear.dat" u 1:4 w l lc "red"   lw 2  }'
       WRITE(34,"(a)")'    if (shearA_mean != 0 && shearB_mean == 0 && shearC_mean == 0 ) {pl [0:360][] "2dcut_shear.dat" u 1:2  w l  lc  "blue"  lw 2 }'
       WRITE(34,"(a)")'    if (shearA_mean == 0 && shearB_mean != 0 && shearC_mean == 0 ) {pl [0:360][] "2dcut_shear.dat" u 1:3  w l  lc  "green" lw 2 }'
       WRITE(34,"(a)")'    if (shearA_mean == 0 && shearB_mean == 0 && shearC_mean != 0 ) {pl [0:360][] "2dcut_shear.dat" u 1:4  w l  lc  "red"   lw 2 }'
     
       WRITE(34,"(a)")'#  pl [0:360][] "2dcut_shear.dat" u 1:4 w l lc "red"   lw 2 ,"2dcut_shear.dat" u 1:2 w l  lc "blue" lw 2,"2dcut_shear.dat" u 1:3 w l lc "green" lw 2'
       WRITE(34,"(a)")'  set title "(Young Modulus (GPa))"'
       WRITE(34,"(a)")'  set ylabel "Young Modulus (GPa)"'
       WRITE(34,"(a)")'    if ( youngA_mean == 0 && youngB_mean != 0  ) {pl [0:360][] "2dcut_young.dat" u 1:3  w l  lc  "blue" lw 2  }'
       WRITE(34,"(a)")'    if ( youngA_mean != 0 && youngB_mean == 0  ) {pl [0:360][] "2dcut_young.dat" u 1:2  w l  lc  "green"  lw 2  }'
       WRITE(34,"(a)")'    if ( youngA_mean != 0 && youngB_mean != 0  ) {pl [0:360][] "2dcut_young.dat" u 1:2  w l  lc  "green"  lw 2, "2dcut_young.dat" u 1:3 w l lc "blue" lw 2   }'
       WRITE(34,"(a)")'  #pl [0:360][] "2dcut_young.dat" u 1:2 w l lc "green" lw 2 , 0 #,"2dcut_young.dat" u 1:3 w l lc "blue" lw 2'

       WRITE(34,"(a)")'  set title "(Linear Compressibiliy (1/TPa))"'
       WRITE(34,"(a)")'  set ylabel "Linear Compressibiliy (1/TPa)"'
       WRITE(34,"(a)")'    if ( comA_mean == 0 && comB_mean != 0  ) {pl [0:360][] "2dcut_comp.dat" u 1:3  w l  lc  "red" lw 2  }'
       WRITE(34,"(a)")'    if ( comA_mean != 0 && comB_mean == 0  ) {pl [0:360][] "2dcut_comp.dat" u 1:2  w l  lc  "green"  lw 2  }'
       WRITE(34,"(a)")'    if ( comA_mean != 0 && comB_mean != 0  ) {pl [0:360][] "2dcut_comp.dat" u 1:2  w l  lc  "green"  lw 2, "2dcut_comp.dat" u 1:3 w l lc "red" lw 2   }'

       WRITE(34,"(a)")' # pl [0:360][] "2dcut_comp.dat" u 1:2 w l lc "green" lw 2,"2dcut_comp.dat" u 1:3 w l lc "red"lw 2'
       WRITE(34,"(a)")'  set title "(Bulk Modulus (GPa))"'
       WRITE(34,"(a)")'  set ylabel "Bulk Modulus (GPa)"'
       WRITE(34,"(a)")'    if ( bulkA_mean == 0 && bulkB_mean != 0  ) {pl [0:360][] "2dcut_bulk.dat" u 1:3  w l  lc  "green" lw 2  }'
       WRITE(34,"(a)")'    if ( bulkA_mean != 0 && bulkB_mean == 0  ) {pl [0:360][] "2dcut_bulk.dat" u 1:2  w l  lc  "red"  lw 2  }'
       WRITE(34,"(a)")'    if ( bulkA_mean != 0 && bulkB_mean != 0  ) {pl [0:360][] "2dcut_bulk.dat" u 1:3  w l  lc  "green"  lw 2, "2dcut_bulk.dat" u 1:2 w l lc "red" lw 2   }'
       WRITE(34,"(a)")' # pl [0:360][] "2dcut_bulk.dat" u 1:2 w l lc "green" lw 2, "2dcut_bulk.dat" u 1:3  w l lc "blue" lw 2'

       WRITE(34,"(a)")'  set title "(Poissons Ratio)"'
       WRITE(34,"(a)")'  set ylabel "Poissons Ratio"'
       WRITE(34,"(a)")'    if (poiA_mean == 0 && poiB_mean != 0 && poiC_mean != 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:3  w l  lc  "green" lw 2, "2dcut_poisson.dat" u 1:4 w l lc "red"   lw 2   }'
       WRITE(34,"(a)")'    if (poiA_mean != 0 && poiB_mean == 0 && poiC_mean != 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_poisson.dat" u 1:4  w l lc "red"   lw 2  }'
       WRITE(34,"(a)")'    if (poiA_mean != 0 && poiB_mean != 0 && poiC_mean == 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_poisson.dat" u 1:3 w l lc "green" lw 2   }'
       WRITE(34,"(a)")'    if (poiA_mean != 0 && poiB_mean != 0 && poiC_mean != 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_poisson.dat" u 1:3 w l lc "green" lw 2, "2dcut_poisson.dat" u 1:4 w l lc "red"   lw 2  }'
       WRITE(34,"(a)")'    if (poiA_mean != 0 && poiB_mean == 0 && poiC_mean == 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:2  w l  lc  "blue"  lw 2 }'
       WRITE(34,"(a)")'    if (poiA_mean == 0 && poiB_mean != 0 && poiC_mean == 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:3  w l  lc  "green" lw 2 }'
       WRITE(34,"(a)")'    if (poiA_mean == 0 && poiB_mean == 0 && poiC_mean != 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:4  w l  lc  "red"   lw 2 }'

       WRITE(34,"(a)")'  #pl [0:360][] "2dcut_poisson.dat" u 1:4  w l lc "red" lw 2 , "2dcut_poisson.dat" u 1:3 w l lc "green" lw 2, "2dcut_poisson.dat" u 1:2 w l lc "blue" lw 2'
       !WRITE(34,"(a)")'  set title "(Sound)"'
       ! WRITE(34,"(a)")'  set ylabel "Sound"'
       !WRITE(34,"(a)")' pl [0:360][] "2dsoundTMmin.dat"   w l lc "red" lw 2 , "2dsoundTMmax.dat"   w l lc "blue" lw 2, "2dsoundLM.dat" w l lc "green" lw 2'
       WRITE(34,"(a)")'  set title "(Pugh Ratio)"'
       WRITE(34,"(a)")'  set ylabel "Pugh Ratio"'

       WRITE(34,"(a)")'    if (poghA_mean == 0 && poghB_mean != 0 && poghC_mean != 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:3  w l  lc  "green" lw 2, "2dcut_pugh.dat" u 1:4 w l lc "red"   lw 2   }'
       WRITE(34,"(a)")'    if (poghA_mean != 0 && poghB_mean == 0 && poghC_mean != 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_pugh.dat" u 1:4  w l lc "red"   lw 2  }'
       WRITE(34,"(a)")'    if (poghA_mean != 0 && poghB_mean != 0 && poghC_mean == 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_pugh.dat" u 1:3 w l lc "green" lw 2   }'
       WRITE(34,"(a)")'    if (poghA_mean != 0 && poghB_mean != 0 && poghC_mean != 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_pugh.dat" u 1:3 w l lc "green" lw 2, "2dcut_pugh.dat" u 1:4 w l lc "red"   lw 2  }'
       WRITE(34,"(a)")'    if (poghA_mean != 0 && poghB_mean == 0 && poghC_mean == 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:2  w l  lc  "blue"  lw 2 }'
       WRITE(34,"(a)")'    if (poghA_mean == 0 && poghB_mean != 0 && poghC_mean == 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:3  w l  lc  "green" lw 2 }'
       WRITE(34,"(a)")'    if (poghA_mean == 0 && poghB_mean == 0 && poghC_mean != 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:4  w l  lc  "red"   lw 2 }'
       WRITE(34,"(a)")' # pl [0:360][] "2dcut_pugh.dat" u 1:4  w l lc "red"   lw 2 ,"2dcut_pugh.dat" u 1:3 w l  lc "blue" lw 2,"2dcut_pugh.dat" u 1:2 w l lc "green" lw 2'


       WRITE(*,*) "> 2Dbox v1:   Plot"
       CALL SYSTEM('gnuplot plot2d.gnu')
       WRITE(*,*)"Please see  2Dbox.ps file"
       If (yn_veloc=='Y' .or. yn_veloc=='y') THEN
          CALL plot2df_veloc(YN)
          CALL SYSTEM('gnuplot plot2d_c.gnu')
          !call SYSTEM('rm plot2d_c.gnu')
     ENDIF
ELSE
     WRITE(*,*) "> 2Dbox v1:   not Plot"
  
ENDIF
CLOSE(34)
END SUBROUTINE
SUBROUTINE plot2df_veloc(YN)
IMPLICIT NONE
CHARACTER(len=1) :: YN


IF (YN.eq."Y")THEN
  
     OPEN(33,FILE="plot2d_c.gnu")   
     WRITE(33,"(a)")' #!/usr/bin/gnuplot -persist'
     WRITE(33,"(a)")'  set term postscript eps enhanced color "Times-Roman, 11"'
     WRITE(33,"(a)")'  set  output "2Dbox_veloc.ps"'
     WRITE(33,"(a)")'  set grid x,y'
     WRITE(33,"(a)")'  unset key'
     WRITE(33,"(a)")'  set   autoscale '
     WRITE(33,"(a)")'  set hidden3d'
     WRITE(33,"(a)")'  set bmargin 5'
     WRITE(33,"(a)")'  set border 4095'
     WRITE(33,"(a)")'  set ticslevel 0'
     WRITE(33,"(a)")'  set xtics rotate'
     WRITE(33,"(a)")'  set xlabel "Degree"'
     WRITE(33,"(a)")'  set ylabel "Y"'
     WRITE(33,"(a)")'  set xtics 30'
     WRITE(33,"(a)")'  #set palette rgb 30,31,32'
     WRITE(33,"(a)")'  set multiplot layout 3,1'
     WRITE(33,"(a)")'  set title "(Phase velocities (km/s))"'
     WRITE(33,"(a)")'  set ylabel "(km/s)"'
     WRITE(33,"(a)")'  pl [0:360][] "2dcut_pveloc.dat" u 1:2 w l lc   "blue"   lw 2 notitle, \'
     WRITE(33,"(a)")'               "2dcut_pveloc.dat" u 1:3 w l lc   "green"  lw 2 notitle,\'
     WRITE(33,"(a)")'               "2dcut_pveloc.dat" u 1:4 w l lc   "red"    lw 2 notitle '
     WRITE(33,"(a)")'  set title "(Group velocities (km/s))"'
     WRITE(33,"(a)")'  set ylabel "(km/s)"'
     WRITE(33,"(a)")'  pl [0:360][] "2dcut_gveloc.dat" u 1:2 w l lc   "blue"   lw 2 notitle, \'
     WRITE(33,"(a)")'               "2dcut_gveloc.dat" u 1:3 w l lc   "green"  lw 2 notitle,\'
     WRITE(33,"(a)")'               "2dcut_gveloc.dat" u 1:4 w l lc   "red"    lw 2 notitle '
     WRITE(33,"(a)")'  set title "(Power Flow angle)"'
     WRITE(33,"(a)")'  set ylabel "(Degree)"'
     WRITE(33,"(a)")'  pl [0:360][] "2dcut_pfaveloc.dat" u 1:2 w l lc   "blue"   lw 2 ti "P mode", \'
     WRITE(33,"(a)")'               "2dcut_pfaveloc.dat" u 1:3 w l lc   "green"  lw 2 ti "Fast mode",\'
     WRITE(33,"(a)")'               "2dcut_pfaveloc.dat" u 1:4 w l lc   "red"    lw 2 ti "Slow mode" '
     WRITE(*,*) "> 2Dbox_veloc v1:   Plot"
     CALL SYSTEM('gnuplot plot2d_c.gnu')
     WRITE(*,*)"Please see  2Dbox_veloc.ps file"
ELSE
     WRITE(*,*) "> 2Dbox_veloc v1:   not Plot"
ENDIF
CLOSE(33)  
END SUBROUTINE