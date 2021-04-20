!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL , GENERAITE GNUPLOT sqtipt of Shear Modulus for 3D temp.pics.


SUBROUTINE plotshear3d(YN)
	IMPLICIT NONE
	CHARACTER(len=1) :: YN
	IF (YN.eq."Y")THEN		
	
		OPEN(33,FILE="plot3dShear.gnu")
		WRITE(33,"(a)")' #!/usr/bin/gnuplot -persist'
		WRITE(33,"(a)")' # set terminal qt 0 font "Sans,9"'
		WRITE(33,"(a)")' # set output'
		WRITE(33,"(a)")' set term pngcairo enhanced dashed font "Arial, 22" size 1300,1300 nocrop lw 2'
		WRITE(33,"(a)")' unset key'
		WRITE(33,"(a)")' set hidden3d'
		WRITE(33,"(a)")' set border 4095'
		WRITE(33,"(a)")' set ticslevel 0'
		WRITE(33,"(a)")' set xlabel "X"'
		WRITE(33,"(a)")' set ylabel "Y"'
		WRITE(33,"(a)")' set zlabel "Z"'
		WRITE(33,"(a)")' #set palette rgb 30,31,32'
		WRITE(33,"(a)")' #set multiplot layout 3,3'
		WRITE(33,"(a)")' set output "MaxP_shear.png"'
		WRITE(33,"(a)")' set title "Maximum positive of Shear Modulus"'
		WRITE(33,"(a)")' splot "3d_shear.dat" u 1:2:3 w l lt 3,"3d_shear.dat" u 1:2:3 w d lt -1 lw 0.5'
		WRITE(33,"(a)")' unset output'
		WRITE(33,"(a)")' set output "MimN_shear.png"'
		WRITE(33,"(a)")' set title "Minimum negative of Shear Modulus"'
		WRITE(33,"(a)")' splot "3d_shear.dat" u 7:8:9 w l lt 4,"3d_shear.dat" u 7:8:9 w d lt -1 lw 0.5'
		WRITE(33,"(a)")' unset output'
		WRITE(33,"(a)")' set output "MimP_shear.png"'
		WRITE(33,"(a)")' set title "Minimum positive of Shear Modulus"'
		WRITE(33,"(a)")' splot "3d_shear.dat" u 4:5:6 w l lt 5,"3d_shear.dat" u 4:5:6 w d lt -1 lw 0.5'
		WRITE(33,"(a)")' unset output'
		WRITE(33,"(a)")' set output "AveP_shear.png"'
		WRITE(33,"(a)")' set title "Average positive of Shear Modulus"'
		WRITE(33,"(a)")' splot "3d_shear.dat" u 10:11:12 w l lt 9,"3d_shear.dat" u 10:11:12 w d lt -1 lw 0.5'
		!WRITE(33,"(a)")' unset output'
		!WRITE(33,"(a)")' set output "AveN_shear.png"'
		!WRITE(33,"(a)")' set title "Average negative of Shear Modulus"'
		!WRITE(33,"(a)")' splot "shearN_Ave.dat" u 1:2:3 w l lt 7,"shearN_Ave.dat" u 1:2:3 w d lt -1 lw 0.5'
	    WRITE(*,*) "> Shear Modulus:		plot"
       CALL SYSTEM('gnuplot plot3dShear.gnu')
      	!WRITE(*,*) ">>   MaxP_shear.png," 
	   !WRITE(*,*) "     MinN_shear.png "
	   !WRITE(*,*) " and MinN_shear.png graphs plotted."
	
	ELSE
	
		WRITE(*,*) "> Shear Modulus:	not plot"
	ENDIF
	 CLOSE(33)
	end
