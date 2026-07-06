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
		  	 WRITE(33,"(a)")' #!/usr/bin/gnuplot -persist'
      WRITE(33,"(a)")'set term pdfcairo font "Times-New-Roman,8"'
      WRITE(33,"(a)")'set output "bulk_modulus.pdf"'
      WRITE(33,"(a)")'#set term pngcairo enhanced dashed font "Times-New-Roman,15" size 1000,1000 nocrop lw 2'
      WRITE(33,"(a)")'#set output "gn.png"'
      WRITE(33,"(a)")'set multiplot layout 1,1'
      WRITE(33,"(a)")'set pm3d'
      WRITE(33,"(a)")'set view equal xyz'
      WRITE(33,"(a)")'set palette defined (-1 "blue", 0 "white",1 "red" )'
      WRITE(33,"(a)")'set style line 100 lt 5 lw 0.5'
      WRITE(33,"(a)")'#set pm3d depthorder border lc "black" lw 0.25'
      WRITE(33,"(a)")'set pm3d depthorder'
      WRITE(33,"(a)")'set view 50,220'
      WRITE(33,"(a)")'set ticslevel 0'
      WRITE(33,"(a)")'set grid x y z vertical'
      WRITE(33,"(a)")'set grid lt 1 lw 0.75 lc "grey"'
      WRITE(33,"(a)")'set border 31 + 768 + 96'
      WRITE(33,"(a)")'set autoscale z'
      WRITE(33,"(a)")'set view 66, 300, 1, 1.5'
      WRITE(33,"(a)")'set pm3d lighting primary 0.5 specular 0.5 spec2 0.1'
      WRITE(33,"(a)")'set pm3d depthorder base lighting noborder'
      WRITE(33,"(a)")'set xlabel "X"'
      WRITE(33,"(a)")'set ylabel "Y"'
      WRITE(33,"(a)")'set zlabel "Z"'
      WRITE(33,"(a)")'set mxtics 2'
      WRITE(33,"(a)")'set mytics 2'
      WRITE(33,"(a)")'set mztics 2'
      WRITE(33,"(a)")'unset colorbox'
      WRITE(33,"(a)")'splot "3d_bulk.dat"  u 1:2:3 w pm3d title "Bulk modulus"'
      WRITE(33,"(a)")'unset multiplot'
	!	WRITE(34,"(a)")' #!/usr/bin/gnuplot -persist'
		!WRITE(34,"(a)")' # set terminal qt 0 font "Sans,9"'
		!WRITE(34,"(a)")' # set output'
		!WRITE(34,"(a)")' set term pngcairo enhanced dashed font "Arial, 22" size 1300,1300 nocrop lw 2'
		!WRITE(34,"(a)")' unset key'
	!	WRITE(34,"(a)")' set hidden3d'
	!	WRITE(34,"(a)")' set border 4095'
	!	WRITE(34,"(a)")' set ticslevel 0'
	!	WRITE(34,"(a)")' set xlabel "X"'
	!	WRITE(34,"(a)")' set ylabel "Y"'
		!WRITE(34,"(a)")' set zlabel "Z"'
	!	WRITE(34,"(a)")' #set palette rgb 30,31,32'
	!	WRITE(34,"(a)")' #set multiplot layout 3,3'
		!WRITE(34,"(a)")' set output "MaxP_bulk.png"'
		!WRITE(34,"(a)")' set title "Maximum positive of Bulk Modulus"'
		!WRITE(34,"(a)")' splot "3d_bulk.dat" u 1:2:3 w l lt 3,"3d_bulk.dat" u 1:2:3 w d lt -1 lw 0.5'
		
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
