!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL , GENERAITE GNUPLOT sqtipt of phug Ratio for 3D temp.pics.

SUBROUTINE plotpugh3d(YN,p)
   IMPLICIT NONE
   integer          :: p
   CHARACTER(len=1) :: YN
 IF (YN.eq."Y")THEN

	   OPEN(35,FILE="plot3dPugh.gnu")
    if (p==1) then
      WRITE(35,"(a)")' #!/usr/bin/gnuplot -persist'
      WRITE(35,"(a)")'set term pdfcairo font "Times-New-Roman,8"'
      WRITE(35,"(a)")'set output "Pugh_ratio.pdf"'
      WRITE(35,"(a)")'#set term pngcairo enhanced dashed font "Times-New-Roman,15" size 1000,1000 nocrop lw 2'
      WRITE(35,"(a)")'#set output "gn.png"'
      WRITE(35,"(a)")'set multiplot layout 1,3'
      WRITE(35,"(a)")'set pm3d'
      WRITE(35,"(a)")'set samples 50; set isosamples 50'
      WRITE(35,"(a)")'set view equal xyz'
      WRITE(35,"(a)")'set palette defined (-1 "green", 0 "gold",1 "red" )'
      WRITE(35,"(a)")'set style line 100 lt 5 lw 0.5'
      WRITE(35,"(a)")'#set pm3d depthorder border lc "black" lw 0.25'
      WRITE(35,"(a)")'set pm3d depthorder'
      WRITE(35,"(a)")'set view 50,220'
      WRITE(35,"(a)")'set ticslevel 0'
      WRITE(35,"(a)")'set grid x y z vertical'
      WRITE(35,"(a)")'set grid lt 1 lw 0.75 lc "grey"'
      WRITE(35,"(a)")'set border 31 + 768 + 96'
      WRITE(35,"(a)")'set autoscale z'
      WRITE(35,"(a)")'set view 66, 300, 1, 1.5'
      WRITE(35,"(a)")'set pm3d lighting primary 0.5 specular 0.5 spec2 0.1'
      WRITE(35,"(a)")'set pm3d depthorder base lighting noborder'
      WRITE(35,"(a)")'set xlabel "X"'
      WRITE(35,"(a)")'set ylabel "Y"'
      WRITE(35,"(a)")'set zlabel "Z"'
      WRITE(35,"(a)")'set mxtics 2'
      WRITE(35,"(a)")'set mytics 2'
      WRITE(35,"(a)")'set mztics 2'
      WRITE(35,"(a)")'unset colorbox'
      WRITE(35,"(a)")'splot "3d_pugh.dat"  u 1:2:3 w pm3d title "Max. positive"'
      WRITE(35,"(a)")'splot "3d_pugh.dat"  u 4:5:6 w pm3d title "Min. positive"'
      WRITE(35,"(a)")'splot "3d_pugh.dat" u 7:8:9 w pm3d title "Negative"'
      WRITE(35,"(a)")'unset multiplot'
    else
      WRITE(35,"(a)")' #!/usr/bin/gnuplot -persist'
      WRITE(35,"(a)")'set term pdfcairo font "Times-New-Roman,8"'
      WRITE(35,"(a)")'set output "Pugh_ratio.pdf"'
      WRITE(35,"(a)")'#set term pngcairo enhanced dashed font "Times-New-Roman,15" size 1000,1000 nocrop lw 2'
      WRITE(35,"(a)")'#set output "gn.png"'
      WRITE(35,"(a)")'set multiplot layout 1,2'
      WRITE(35,"(a)")'set samples 50; set isosamples 50'      
      WRITE(35,"(a)")'set pm3d'
      WRITE(35,"(a)")'set view equal xyz'
      WRITE(35,"(a)")'set palette defined (-1 "green", 0 "gold",1 "red" )'
      WRITE(35,"(a)")'set style line 100 lt 5 lw 0.5'
      WRITE(35,"(a)")'#set pm3d depthorder border lc "black" lw 0.25'
      WRITE(35,"(a)")'set pm3d depthorder'
      WRITE(35,"(a)")'set view 50,220'
      WRITE(35,"(a)")'set ticslevel 0'
      WRITE(35,"(a)")'set grid x y z vertical'
      WRITE(35,"(a)")'set grid lt 1 lw 0.75 lc "grey"'
      WRITE(35,"(a)")'set border 31 + 768 + 96'
      WRITE(35,"(a)")'set autoscale z'
      WRITE(35,"(a)")'set view 66, 300, 1, 1.5'
      WRITE(35,"(a)")'set pm3d lighting primary 0.5 specular 0.5 spec2 0.1'
      WRITE(35,"(a)")'set pm3d depthorder base lighting noborder'
      WRITE(35,"(a)")'set xlabel "X"'
      WRITE(35,"(a)")'set ylabel "Y"'
      WRITE(35,"(a)")'set zlabel "Z"'
      WRITE(35,"(a)")'set mxtics 2'
      WRITE(35,"(a)")'set mytics 2'
      WRITE(35,"(a)")'set mztics 2'
      WRITE(35,"(a)")'unset colorbox'
      WRITE(35,"(a)")'splot "3d_pugh.dat"  u 1:2:3 w pm3d title "Max. positive"'
      WRITE(35,"(a)")'splot "3d_pugh.dat"  u 4:5:6 w pm3d title "Min. positive"'
      WRITE(35,"(a)")'#splot "3d_pugh.dat" u 7:8:9 w pm3d title "Negative"'
      WRITE(35,"(a)")'unset multiplot' 		
		
		!!WRITE(33,"(a)")' #!/usr/bin/gnuplot -persist'
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
		!WRITE(33,"(a)")' set output "MaxP_pugh.png"'
		!WRITE(33,"(a)")' set title "Maximum positive of Pugh Ratio"'
		!WRITE(33,"(a)")' splot "3d_pugh.dat" u 1:2:3 w l lt 3,"3d_pugh.dat" u 1:2:3 w d lt -1 lw 0.5'
		!WRITE(33,"(a)")' unset output'
		!WRITE(33,"(a)")' set output "MimN_pugh.png"'
		!WRITE(33,"(a)")' set title "Minimum negative of Pugh Ratio"'
		!WRITE(33,"(a)")' splot "3d_pugh.dat" u 7:8:9 w l lt 4,"3d_pugh.dat" u 7:8:9  w d lt -1 lw 0.5'
		!WRITE(33,"(a)")' unset output'
		!WRITE(33,"(a)")' set output "MimP_pugh.png"'
		!WRITE(33,"(a)")' set title "Minimum positive of Pugh Ratio"'
		!WRITE(33,"(a)")' splot "3d_pugh.dat" u 4:5:6 w l lt 5,"3d_pugh.dat" u 4:5:6 w d lt -1 lw 0.5'
		!WRITE(33,"(a)")' unset output'
		!WRITE(33,"(a)")' set output "AveP_pugh.png"'
		!WRITE(33,"(a)")' set title "Average positive of Pugh Ratio"'
		!WRITE(33,"(a)")' splot "3d_pugh.dat" u 10:11:12 w l lt 9,"3d_pugh.dat" u 10:11:12 w d lt -1 lw 0.5'
		!WRITE(33,"(a)")' unset output'
		!WRITE(33,"(a)")' set output "AveN_shear.png"'
		!WRITE(33,"(a)")' set title "Average negative of Shear Modulus"'
		!WRITE(33,"(a)")' splot "shearN_Ave.dat" u 1:2:3 w l lt 7,"shearN_Ave.dat" u 1:2:3 w d lt -1 lw 0.5'
	ENDIF	
		WRITE(*,*) "> Pugh ratio:			plot"

		CALL SYSTEM('gnuplot plot3dPugh.gnu')
       !WRITE(*,*) ">>   MaxP_shear.png," 
	  !WRITE(*,*) "     MinN_shear.png "
	!WRITE(*,*) " and MinN_shear.png graphs plotted."
	
	ELSE
	
		WRITE(*,*) "> Pugh ratio:	not plot"
	ENDIF
	CLOSE(35)
	end
