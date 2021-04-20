  set term postscript eps enhanced color "Times-Roman, 11"
  set angles degrees
  unset key
  set polar
  set zeroaxis
  set ytics axis nomirror
  set xtics axis nomirror
  set rmargin 18.5
  set lmargin 18.5
  unset border
  #unset xtic
  unset rtic
 #  set grid polar 30
 #  set border polar
 #  unset param
 #  set xtics rotate
 #  unset xtics
 #  unset ytics
 #  set ttics 0,30 
 #  set mttics 3
 #  set grid r polar 60
  # set border 0
  # set size square
  # set border polar
  # set bmargin at screen 0.5
  
  unset  output 
  set  output "2D_sys_Poissons.ps"
  set title "Poissons Ratio\n\n\n"
  pl "poisson_2d_sys.dat" u 1:2  w l lc "green" lw 2 , "poisson_2d_sys.dat" u 1:3  w l lc "blue" lw 2 , "poisson_2d_sys.dat" u 1:4 w l lc "red" lw 2 
  set term postscript eps enhanced color "Times-Roman, 11"
  set angles degrees
  unset key
  set polar
  set zeroaxis
  set ytics axis nomirror
  set xtics axis nomirror
  set rmargin 18.5
  set lmargin 18.5
  unset border
  #unset xtic
  unset rtic
 #  set grid polar 30
 #  set border polar
 #  unset param
 #  set xtics rotate
 #  unset xtics
 #  unset ytics
 #  set ttics 0,30 
 #  set mttics 3
 #  set grid r polar 60
  # set border 0
  # set size square
  # set border polar
  # set bmargin at screen 0.5
  
  unset  output 
  set  output "2D_sys_Young.ps"
  set title "Youngs Modulus (N/m)\n\n\n"
  pl "young_2d_sys.dat" u 1:2  w l lc "blue" lw 2 
  set term postscript eps enhanced color "Times-Roman, 11"
  set angles degrees
  unset key
  set polar
  set zeroaxis
  set ytics axis nomirror
  set xtics axis nomirror
  set rmargin 18.5
  set lmargin 18.5
  unset border
  #unset xtic
  unset rtic
 #  set grid polar 30
 #  set border polar
 #  unset param
 #  set xtics rotate
 #  unset xtics
 #  unset ytics
 #  set ttics 0,30 
 #  set mttics 3
 #  set grid r polar 60
  # set border 0
  # set size square
  # set border polar
  # set bmargin at screen 0.5
  
  unset  output 
  set  output "2D_sys_Shear.ps"
  set title "Shear Modulus (N/m)\n\n\n"
  pl "shear_2d_sys.dat" u 1:2  w l lc "blue" lw 2 
