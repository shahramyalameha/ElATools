!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL , GENERAITE GNUPLOT sqtipt of Linear Compressibiliy for 3D temp.pics.
  
SUBROUTINE plotcomp3d(YN,p)
	IMPLICIT NONE
	integer:: p
	CHARACTER(len=1) :: YN

	IF (YN.eq."Y")THEN

		OPEN(34,FILE="plot3dComp.gnu")
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
		if (p==1) then

			WRITE(34,"(a)")' set output "MimN_comp.png"'
			WRITE(34,"(a)")' set title "Minimum negative of Linear Compressibiliy"'
			WRITE(34,"(a)")' splot "3d_comp.dat" u 7:8:9 w l lt 4,"3d_comp.dat" u 7:8:9 w d lt -1 lw 0.5'
		else
			WRITE(34,"(a)")' set output "MaxP_comp.png"'
			WRITE(34,"(a)")' set title "Maximum positive of Linear Compressibiliy"'
			WRITE(34,"(a)")' splot "3d_comp.dat" u 1:2:3 w l lt 3,"3d_comp.dat" u 1:2:3 w d lt -1 lw 0.5'
			WRITE(34,"(a)")' unset output'
		ENDIF
		WRITE(34,"(a)")' unset output'
		WRITE(34,"(a)")' set output "MimP_comp.png"'
		WRITE(34,"(a)")' set title "Minimum positive of Linear Compressibiliy"'
		WRITE(34,"(a)")' splot "3d_comp.dat" u 4:5:6 w l lt 5,"3d_comp.dat" u 4:5:6 w d lt -1 lw 0.5'
		WRITE(*,*) "> Linear Compressibiliy:  	 Plot"
		CALL SYSTEM('gnuplot plot3dComp.gnu ')
		!WRITE(*,*) ">>    MaxP_comp.png, "
		!WRITE(*,*) "      MinN_comp.png "
		!WRITE(*,*) "  and MinN_comp.png graphs plotted."
	ELSE
		WRITE(*,*) "> Linear Compressibiliy:	not Plot"
	ENDIF
	CLOSE(34)
end
