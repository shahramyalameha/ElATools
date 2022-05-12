!```````````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018-2022 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                     `
!                                                                                               `
!```````````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the bulk Modulus method II calculation.. 

SUBROUTINE bulk_method(Pratio, sheainvar, bulk_m2)

DOUBLE PRECISION      :: Pratio, sheainvar, bulk_m2




bulk_m2 = ( 2.d0 * sheainvar * (1.d0 + Pratio) ) /   (3.0d0 * (1.0d0  - 2.0d0 * Pratio))


		
 


END SUBROUTINE bulk_method
