!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL , GENERAITE GNUPLOT sqtipt of Bulk Modulus for 3D temp.pics.
  
SUBROUTINE plotbulk3d(YN)
	IMPLICIT NONE
	integer          :: p
	CHARACTER(len=1) :: YN
	IF (YN.eq."Y")THEN
		OPEN(34,FILE="plot3dBulk.gnu")
		WRITE(34,"(a)")' #!/usr/bin/gnuplot -persist'
		WRITE(34,"(a)")' # set terminal qt 0 font "Sans,9"'
		WRITE(34,"(a)")' # set output'
		WRITE(34,"(a)")' set term pngcairo enhanced dashed font "Arial, 22" size 1300,1300 nocrop lw 2'
		WRITE(34,"(a)")' unset key'
		WRITE(34,"(a)")' set hidden3d'
		WRITE(34,"(a)")' set border 4095'
		WRITE(34,"(a)")' set ticslevel 0'
		WRITE(34,"(a)")' set xlabel "X"'
		WRITE(34,"(a)")' set ylabel "Y"'
		WRITE(34,"(a)")' set zlabel "Z"'
		WRITE(34,"(a)")' #set palette rgb 30,31,32'
		WRITE(34,"(a)")' #set multiplot layout 3,3'
		WRITE(34,"(a)")' set output "MaxP_bulk.png"'
		WRITE(34,"(a)")' set title "Maximum positive of Bulk Modulus"'
		WRITE(34,"(a)")' splot "3d_bulk.dat" u 1:2:3 w l lt 3,"3d_bulk.dat" u 1:2:3 w d lt -1 lw 0.5'
		!WRITE(34,"(a)")' unset output'
		!WRITE(34,"(a)")' set output "MinP_bulk.png"'
		!WRITE(34,"(a)")' set title "Minimum positive of Bulk Modulus"'
		!WRITE(34,"(a)")' splot "MinP_bulk.dat" u 1:2:3 w l lt 5,"MinP_bulk.dat" u 1:2:3 w d lt -1 lw 0.5'
		WRITE(*,*) "> Bulk Modulus:		Plot"
		CALL SYSTEM('gnuplot plot3dBulk.gnu')
	
		!WRITE(*,*) ">>  MaxP_bulk.png, "
	 
		!WRITE(*,*) "    MinP_bulk.png graphs plotted."
	ELSE
		WRITE(*,*) "> Bulk Modulus:	not Plot"
	ENDIF
	CLOSE(34)
	end SUBROUTINE
