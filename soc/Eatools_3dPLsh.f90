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
      WRITE(33,"(a)")'set term pdfcairo font "Times-New-Roman,8"'
      WRITE(33,"(a)")'set output "shear_modulus.pdf"'
      WRITE(33,"(a)")'#set term pngcairo enhanced dashed font "Times-New-Roman,15" size 1000,1000 nocrop lw 2'
      WRITE(33,"(a)")'#set output "gn.png"'
      WRITE(33,"(a)")'set multiplot layout 1,2'
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
      WRITE(33,"(a)")'splot "3d_shear.dat"  u 1:2:3 w pm3d title "Max."'
      WRITE(33,"(a)")'splot "3d_shear.dat"  u 4:5:6 w pm3d title "Min."'
      WRITE(33,"(a)")'unset multiplot'
		!WRITE(33,"(a)")' #!/usr/bin/gnuplot -persist'
		!WRITE(33,"(a)")' # set terminal qt 0 font "Sans,9"'
		!WRITE(33,"(a)")' # set output'
		!WRITE(33,"(a)")' set term pngcairo enhanced dashed font "Arial, 22" size 1300,1300 nocrop lw 2'
		!WRITE(33,"(a)")' unset key'
		!WRITE(33,"(a)")' set hidden3d'
		!WRITE(33,"(a)")' set border 4095'
		!WRITE(33,"(a)")' set ticslevel 0'
		!WRITE(33,"(a)")' set xlabel "X"'
		!WRITE(33,"(a)")' set ylabel "Y"'
		!WRITE(33,"(a)")' set zlabel "Z"'
		!WRITE(33,"(a)")' #set palette rgb 30,31,32'
		!WRITE(33,"(a)")' #set multiplot layout 3,3'
		!WRITE(33,"(a)")' set output "MaxP_shear.png"'
		!WRITE(33,"(a)")' set title "Maximum positive of Shear Modulus"'
		!WRITE(33,"(a)")' splot "3d_shear.dat" u 1:2:3 w l lt 3,"3d_shear.dat" u 1:2:3 w d lt -1 lw 0.5'
		!WRITE(33,"(a)")' unset output'
		!WRITE(33,"(a)")' set output "MimN_shear.png"'
		!WRITE(33,"(a)")' set title "Minimum negative of Shear Modulus"'
		!WRITE(33,"(a)")' splot "3d_shear.dat" u 7:8:9 w l lt 4,"3d_shear.dat" u 7:8:9 w d lt -1 lw 0.5'
		!WRITE(33,"(a)")' unset output'
		!WRITE(33,"(a)")' set output "MimP_shear.png"'
		!WRITE(33,"(a)")' set title "Minimum positive of Shear Modulus"'
		!WRITE(33,"(a)")' splot "3d_shear.dat" u 4:5:6 w l lt 5,"3d_shear.dat" u 4:5:6 w d lt -1 lw 0.5'
		!WRITE(33,"(a)")' unset output'
		!WRITE(33,"(a)")' set output "AveP_shear.png"'
		!WRITE(33,"(a)")' set title "Average positive of Shear Modulus"'
		!WRITE(33,"(a)")' splot "3d_shear.dat" u 10:11:12 w l lt 9,"3d_shear.dat" u 10:11:12 w d lt -1 lw 0.5'
		
		!!WRITE(33,"(a)")' unset output'
		!!WRITE(33,"(a)")' set output "AveN_shear.png"'
		!!WRITE(33,"(a)")' set title "Average negative of Shear Modulus"'
		!!WRITE(33,"(a)")' splot "shearN_Ave.dat" u 1:2:3 w l lt 7,"shearN_Ave.dat" u 1:2:3 w d lt -1 lw 0.5'
	    WRITE(*,*) "> Shear Modulus:            plot"
       CALL SYSTEM('gnuplot plot3dShear.gnu')
    !WRITE(*,*) ">>   MaxP_shear.png," 
	   !WRITE(*,*) "     MinN_shear.png "
	   !WRITE(*,*) " and MinN_shear.png graphs plotted."
	
	ELSE
	
		WRITE(*,*) "> Shear Modulus:            not plot"
	ENDIF
	 CLOSE(33)
	end
