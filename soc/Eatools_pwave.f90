!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the P-wave modulus (M) calculation.. 

SUBROUTINE CPWave(G_min,G_max,BINver,Pw_max,Pw_min)

	  DOUBLE PRECISION                :: G_min,G_max,BINver,Pw_max,Pw_min
	  
	  
	  Pw_max = BINver + (4.d0*G_max)/3.D0
	  Pw_min = BINver + (4.d0*G_min)/3.D0
	  
END SUBROUTINE CPWave 	  

