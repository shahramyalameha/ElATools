!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL , Convert polar coordinates to Cartesian.

!===============================================================
	SUBROUTINE angl2cart(anglt,anglp,kv1,kv2,kv3)
	
		DOUBLE PRECISION                               :: anglt, anglp, kv1,kv2,kv3
		kv1 = SIN(anglt)*COS(anglp)
		kv2 = SIN(anglt)*SIN(anglp)
		kv3 = COS(anglt)
	
	END SUBROUTINE angl2cart
 !===============================================================
