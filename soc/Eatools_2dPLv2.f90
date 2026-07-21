!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, 
!               Please report bugs or suggestions to:  yalameha93@gmail.com                
!                                                                                          
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL , GENERATET GNUPOLT sqripts for temp. 2D pic.. < 2Dbox v2

SUBROUTINE plot2df_gnuplot52(YN,yom,shm,com,som,pom,bum,pum)
    DOUBLE PRECISION   :: yom,shm,com,som,pom,bum,pum,OUTMAX,TOUTMAX
    integer, DIMENSION(18) :: x1,y1,z1,k1,x11 ,y11,z11 
    integer::tt=0,chek
    CHARACTER(LEN=30)  :: NAME

    OPEN(40,FILE="plot2d52.gnu")
    WRITE(40,"(a)")'  set term postscript eps enhanced color "Times-Roman, 12"'
    
    ! --- Data Stats ---
    WRITE(40,"(a)")'  stats "2dcut_shear.dat" using 2 name "shearA" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_shear.dat" using 3 name "shearB" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_shear.dat" using 4 name "shearC" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_young.dat" using 2 name "youngA" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_young.dat" using 3 name "youngB" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_comp.dat" using 2 name "comA" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_comp.dat" using 3 name "comB" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_poisson.dat" using 2 name "poiA" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_poisson.dat" using 3 name "poiB" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_poisson.dat" using 4 name "poiC" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_pugh.dat" using 2 name "poghA" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_pugh.dat" using 3 name "poghB" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_pugh.dat" using 4 name "poghC" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_bulk.dat" using 2 name "bulkA" nooutput'
    WRITE(40,"(a)")'  stats "2dcut_bulk.dat" using 3 name "bulkB" nooutput'

    ! --- General Polar Plot Settings ---
    WRITE(40,"(a)")'  set angles degrees'
    WRITE(40,"(a)")'  unset key'
    WRITE(40,"(a)")'  set polar'
    WRITE(40,"(a)")'  set size square'         ! IMPORTANT: Keeps the polar plot perfectly circular
    WRITE(40,"(a)")'  set grid polar 30 lw 0.5 lc rgb "gray"' ! Adds a readable polar grid
    WRITE(40,"(a)")'  set rrange [0:*]'        ! Ensures the origin is exactly 0
    WRITE(40,"(a)")'  set zeroaxis'
    WRITE(40,"(a)")'  unset border'
    WRITE(40,"(a)")'  set xtics axis nomirror format ""' ! Hides Cartesian axis numbers
    WRITE(40,"(a)")'  set ytics axis nomirror format ""'
    WRITE(40,"(a)")'  set rtics format "%g"'             ! Shows numbers only on the radius
    WRITE(40,"(a)")'  set ttics 0,30 format "%g^o"'      ! Shows angle degrees around the circle

    !=========================================================================================================
    WRITE(40,"(a)")'  unset  output '
    WRITE(40,"(a)")'  set  output "2DYoung.ps"'
    WRITE(40,"(a)")'  set title "Young Modulus (GPa)" offset 0,1.5'
    WRITE(40,"(a)")'  if ( youngA_mean == 0 && youngB_mean != 0  ) {pl   "2dcut_young.dat" u 1:3  w l  lc  "blue" lw 2  }'
    WRITE(40,"(a)")'  if ( youngA_mean != 0 && youngB_mean == 0  ) {pl   "2dcut_young.dat" u 1:2  w l  lc  "green"  lw 2  }'
    
    !=========================================================================================================
    WRITE(40,"(a)")'  unset  output '
    WRITE(40,"(a)")'  set  output "2DShear.ps"'
    WRITE(40,"(a)")'  set title "Shear Modulus (GPa)" offset 0,1.5'
    WRITE(40,"(a)")'  if (shearA_mean == 0 && shearB_mean != 0 && shearC_mean != 0 ) {pl   "2dcut_shear.dat" u 1:3  w l  lc  "green" lw 2, "2dcut_shear.dat" u 1:4 w l lc "red"   lw 2   }'
    WRITE(40,"(a)")'  if (shearA_mean != 0 && shearB_mean == 0 && shearC_mean != 0 ) {pl   "2dcut_shear.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_shear.dat" u 1:4  w l lc "red"   lw 2  }'
    WRITE(40,"(a)")'  if (shearA_mean != 0 && shearB_mean != 0 && shearC_mean == 0 ) {pl   "2dcut_shear.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_shear.dat" u 1:3 w l lc "green" lw 2   }'
    WRITE(40,"(a)")'  if (shearA_mean != 0 && shearB_mean != 0 && shearC_mean != 0 ) {pl   "2dcut_shear.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_shear.dat" u 1:3 w l lc "green" lw 2, "2dcut_shear.dat" u 1:4 w l lc "red"   lw 2  }'
    WRITE(40,"(a)")'  if (shearA_mean != 0 && shearB_mean == 0 && shearC_mean == 0 ) {pl   "2dcut_shear.dat" u 1:2  w l  lc  "blue"  lw 2 }'
    WRITE(40,"(a)")'  if (shearA_mean == 0 && shearB_mean != 0 && shearC_mean == 0 ) {pl   "2dcut_shear.dat" u 1:3  w l  lc  "green" lw 2 }'
    WRITE(40,"(a)")'  if (shearA_mean == 0 && shearB_mean == 0 && shearC_mean != 0 ) {pl   "2dcut_shear.dat" u 1:4  w l  lc  "red"   lw 2 }'
    
    !=====================================================================================================  
    WRITE(40,"(a)")'  unset  output '
    WRITE(40,"(a)")'  set  output "2DCompressibiliy.ps"'
    WRITE(40,"(a)")'  set title "Linear Compressibility (1/TPa)" offset 0,1.5'
    WRITE(40,"(a)")'  if ( comA_mean == 0 && comB_mean != 0  ) {pl   "2dcut_comp.dat" u 1:3  w l  lc  "red" lw 2  }'
    WRITE(40,"(a)")'  if ( comA_mean != 0 && comB_mean == 0  ) {pl   "2dcut_comp.dat" u 1:2  w l  lc  "green"  lw 2  }'
    WRITE(40,"(a)")'  if ( comA_mean != 0 && comB_mean != 0  ) {pl   "2dcut_comp.dat" u 1:2  w l  lc  "green"  lw 2, "2dcut_comp.dat" u 1:3 w l lc "red" lw 2   }'    
    
    !=====================================================================================================
    WRITE(40,"(a)")'  unset  output '
    WRITE(40,"(a)")'  set  output "2DBulk.ps"'
    WRITE(40,"(a)")'  set title "Bulk Modulus (GPa)" offset 0,1.5'
    WRITE(40,"(a)")'  if ( bulkA_mean == 0 && bulkB_mean != 0  ) {pl   "2dcut_bulk.dat" u 1:3  w l  lc  "green" lw 2  }'
    WRITE(40,"(a)")'  if ( bulkA_mean != 0 && bulkB_mean == 0  ) {pl   "2dcut_bulk.dat" u 1:(floor($2))  w l  lc  "red"  lw 2  }'
    WRITE(40,"(a)")'  if ( bulkA_mean != 0 && bulkB_mean != 0  ) {pl   "2dcut_bulk.dat" u 1:3  w l  lc  "green"  lw 2, "2dcut_bulk.dat" u 1:(floor($2)) w l lc "red" lw 2   }'
    
    !=======================================================================================================
    WRITE(40,"(a)")'  unset  output '
    WRITE(40,"(a)")'  set  output "2DPoissons.ps"'
    WRITE(40,"(a)")'  set title "Poissons Ratio" offset 0,1.5'
    WRITE(40,"(a)")'  if (poiA_mean == 0 && poiB_mean != 0 && poiC_mean != 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:3  w l  lc  "green" lw 2, "2dcut_poisson.dat" u 1:4 w l lc "red"   lw 2   }'
    WRITE(40,"(a)")'  if (poiA_mean != 0 && poiB_mean == 0 && poiC_mean != 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_poisson.dat" u 1:4  w l lc "red"   lw 2  }'
    WRITE(40,"(a)")'  if (poiA_mean != 0 && poiB_mean != 0 && poiC_mean == 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_poisson.dat" u 1:3 w l lc "green" lw 2   }'
    WRITE(40,"(a)")'  if (poiA_mean != 0 && poiB_mean != 0 && poiC_mean != 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_poisson.dat" u 1:3 w l lc "green" lw 2, "2dcut_poisson.dat" u 1:4 w l lc "red"   lw 2  }'
    WRITE(40,"(a)")'  if (poiA_mean != 0 && poiB_mean == 0 && poiC_mean == 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:2  w l  lc  "blue"  lw 2 }'
    WRITE(40,"(a)")'  if (poiA_mean == 0 && poiB_mean != 0 && poiC_mean == 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:3  w l  lc  "green" lw 2 }'
    WRITE(40,"(a)")'  if (poiA_mean == 0 && poiB_mean == 0 && poiC_mean != 0 ) {pl [0:360][] "2dcut_poisson.dat" u 1:4  w l  lc  "red"   lw 2 }'

    !=========================================================================================================
    WRITE(40,"(a)")'  unset  output '
    WRITE(40,"(a)")'  set  output "2DPugh.ps"'
    WRITE(40,"(a)")'  set title "Pugh Ratio" offset 0,1.5'
    WRITE(40,"(a)")'  if (poghA_mean == 0 && poghB_mean != 0 && poghC_mean != 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:3  w l  lc  "green" lw 2, "2dcut_pugh.dat" u 1:4 w l lc "red"   lw 2   }'
    WRITE(40,"(a)")'  if (poghA_mean != 0 && poghB_mean == 0 && poghC_mean != 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_pugh.dat" u 1:4  w l lc "red"   lw 2  }'
    WRITE(40,"(a)")'  if (poghA_mean != 0 && poghB_mean != 0 && poghC_mean == 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_pugh.dat" u 1:3 w l lc "green" lw 2   }'
    WRITE(40,"(a)")'  if (poghA_mean != 0 && poghB_mean != 0 && poghC_mean != 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:2  w l  lc  "blue"  lw 2, "2dcut_pugh.dat" u 1:3 w l lc "green" lw 2, "2dcut_pugh.dat" u 1:4 w l lc "red"   lw 2  }'
    WRITE(40,"(a)")'  if (poghA_mean != 0 && poghB_mean == 0 && poghC_mean == 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:2  w l  lc  "blue"  lw 2 }'
    WRITE(40,"(a)")'  if (poghA_mean == 0 && poghB_mean != 0 && poghC_mean == 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:3  w l  lc  "green" lw 2 }'
    WRITE(40,"(a)")'  if (poghA_mean == 0 && poghB_mean == 0 && poghC_mean != 0 ) {pl [0:360][] "2dcut_pugh.dat" u 1:4  w l  lc  "red"   lw 2 }'
    
    WRITE(*,*) "> 2Dbox v2 : Plotting ..."
    CALL SYSTEM('gnuplot plot2d52.gnu')
    
    WRITE(*,*)"Please see  2DSound.ps,"
    WRITE(*,*)"            2DPoissons.ps,"
    WRITE(*,*)"            2DBulk.ps,"
    WRITE(*,*)"            2DCompressibiliy.ps,"
    WRITE(*,*)"            2DShear.ps,"
    WRITE(*,*)"            2DPugh.ps,"
    WRITE(*,*)"            2DYoung.ps files"

    CLOSE(40)
END SUBROUTINE plot2df_gnuplot52


SUBROUTINE plot2df_gnuplot52_2d(NAMEs)
    DOUBLE PRECISION   :: TOUTMAX,TOUTMIN
    integer::tt=0
    CHARACTER(LEN=3)  :: NAMEs ! NAMEs=poi NAMEs=young
    
    OPEN(40,FILE="plot2d52.gnu")
    WRITE(40,"(a)")'  set term postscript eps enhanced color "Times-Roman, 12"'
    
    ! --- Clean and Standard Polar Plot Settings ---
    WRITE(40,"(a)")'  set angles degrees'
    WRITE(40,"(a)")'  unset key'
    WRITE(40,"(a)")'  set polar'
    WRITE(40,"(a)")'  set size square'         ! Enforces circular shape
    WRITE(40,"(a)")'  set grid polar 30 lw 0.5 lc rgb "gray"' 
    WRITE(40,"(a)")'  set rrange [0:*]'
    WRITE(40,"(a)")'  set zeroaxis'
    WRITE(40,"(a)")'  unset border'
    WRITE(40,"(a)")'  set xtics axis nomirror format ""'
    WRITE(40,"(a)")'  set ytics axis nomirror format ""'
    WRITE(40,"(a)")'  set rtics format "%g"'
    WRITE(40,"(a)")'  set ttics 0,30 format "%g^o"'

    IF (NAMes=='poi') then
        WRITE(40,"(a)")'  unset  output '
        WRITE(40,"(a)")'  set  output "2D_sys_Poissons.ps"'
        WRITE(40,"(a)")'  set title "Poissons Ratio" offset 0,1.5'
        WRITE(40,"(a)")'  pl "poisson_2d_sys.dat" u 1:2  w l lc "green" lw 2 , "poisson_2d_sys.dat" u 1:3  w l lc "blue" lw 2 , "poisson_2d_sys.dat" u 1:4 w l lc "red" lw 2 '   
        WRITE(*,*)"Please see  2D_sys_Poissons.ps"  
        CALL SYSTEM('gnuplot plot2d52.gnu')  
    endif
    
    IF (NAMes=='you') then
        WRITE(40,"(a)")'  unset  output '
        WRITE(40,"(a)")'  set  output "2D_sys_Young.ps"'
        WRITE(40,"(a)")'  set title "Youngs Modulus (N/m)" offset 0,1.5'
        WRITE(40,"(a)")'  pl "young_2d_sys.dat" u 1:2  w l lc "blue" lw 2 '
        WRITE(*,*)"Please see  2D_sys_Young.ps."
        CALL SYSTEM('gnuplot plot2d52.gnu')
    endif      
    
    IF (NAMes=='she') then
        WRITE(40,"(a)")'  unset  output '
        WRITE(40,"(a)")'  set  output "2D_sys_Shear.ps"'
        WRITE(40,"(a)")'  set title "Shear Modulus (N/m)" offset 0,1.5'
        WRITE(40,"(a)")'  pl "shear_2d_sys.dat" u 1:2  w l lc "blue" lw 2 '
        WRITE(*,*)"Please see  2D_sys_Shear.ps."
        CALL SYSTEM('gnuplot plot2d52.gnu')
    endif
    
    CLOSE(40)
END SUBROUTINE plot2df_gnuplot52_2d
