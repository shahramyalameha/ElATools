
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 09/08/2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; convert polat to plane for agr file

SUBROUTINE polar2xy(theta,rho,x,y)
 	  REAL(8) :: rho,x,y,theta
	  x = (rho*(COSD(theta)))
	  y = (rho*(SIND(theta)))	  
 END SUBROUTINE polar2xy
