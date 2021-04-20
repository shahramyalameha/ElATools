!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL , GENERAITE GNUPLOT sqtipt of Poissons Ratio for 3D temp.pics.

SUBROUTINE plotRatio3d(YN,p)
   IMPLICIT NONE
   integer          :: p
   CHARACTER(len=1) :: YN
   IF (YN.eq."Y")THEN
   
      OPEN(35,FILE="plot3dRatio.gnu")
      WRITE(35,"(a)")' #!/usr/bin/gnuplot -persist'
      WRITE(35,"(a)")' # set terminal qt 0 font "Sans,9"'
      WRITE(35,"(a)")' # set output'
      WRITE(35,"(a)")' set term pngcairo enhanced dashed font "Arial, 22" size 1300,1300 nocrop lw 2'
      WRITE(35,"(a)")' unset key'
      WRITE(35,"(a)")' set hidden3d'
      WRITE(35,"(a)")' set border 4095'
      WRITE(35,"(a)")' set ticslevel 0'
      WRITE(35,"(a)")' set xlabel "X"'
      WRITE(35,"(a)")' set ylabel "Y"'
      WRITE(35,"(a)")' set zlabel "Z"'
      WRITE(35,"(a)")' #set palette rgb 30,31,32'
      WRITE(35,"(a)")' #set multiplot layout 3,3'
   
      WRITE(35,"(a)")' set output "MaxP_ratio.png"'
      WRITE(35,"(a)")' set title "Maximum positive of Poissons Ratio"'
      WRITE(35,"(a)")' splot "3d_poissons.dat" u 1:2:3 w l lt 3,"3d_poissons.dat" u 1:2:3 w d lt -1 lw 0.5'
      WRITE(35,"(a)")' unset output'

      WRITE(35,"(a)")' set output "MimP_ratio.png"'
      WRITE(35,"(a)")' set title "Minimum positive of Poissons Ratio"'
      WRITE(35,"(a)")' splot "3d_poissons.dat" u 4:5:6 w l lt 5,"3d_poissons.dat" u 4:5:6 w d lt -1 lw 0.5'
      WRITE(35,"(a)")' unset output'
   
      WRITE(35,"(a)")' set output "AveP_ratio.png"'
      WRITE(35,"(a)")' set title "Average positive of Poissons Ratio"'
      WRITE(35,"(a)")' splot "3d_poissons.dat" u 10:11:12 w l lt 9,"3d_poissons.dat" u 10:11:12 w d lt -1 lw 0.5'
      WRITE(35,"(a)")' unset output'
      if (p==1) then
  
         WRITE(35,"(a)")' set output "MimN_ratio.png"'
         WRITE(35,"(a)")' set title "Minimum negative of Poissons Ratio"'
         WRITE(35,"(a)")' splot "3d_poissons.dat" u 7:8:9 w l lt 4,"3d_poissons.dat" u 7:8:9 w d lt -1 lw 0.5'
         WRITE(35,"(a)")' unset output'
   
            !WRITE(35,"(a)")' set output "AveN_ratio.png"'
            !WRITE(35,"(a)")'set title "Average negative of Poissons Ratio"'
            !WRITE(35,"(a)")'splot "AveN_ratio.dat" u 1:2:3 w l lt 7,"AveN_ratio.dat" u 1:2:3 w d lt -1 lw 0.5'
            !WRITE(35,"(a)")'#unset output'
      
      else
         WRITE(35,"(a)")' '

      ENDIF
      WRITE(*,*) "> Poissons Ratio:            plot"
      CALL SYSTEM('gnuplot plot3dRatio.gnu')

     !WRITE(*,*) ">>   MaxP_ratio.png,"
     !WRITE(*,*) "     MinN_ratio.png "
     !WRITE(*,*) " and MinN_ratio.png graphs plotted."
   ELSE
      WRITE(*,*) "> Poissons Ratio:   not plot"
   ENDIF
   CLOSE(35)
      END SUBROUTINE
