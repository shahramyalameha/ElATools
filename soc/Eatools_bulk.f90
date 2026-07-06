!```````````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018-2022 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                     `
!                                                                                               `
!```````````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the bulk Modulus method II and III calculation.. 
! method III ReF. : https://doi.org/10.1007/s00161-018-0635-9

SUBROUTINE bulk_method_one(S,v11,v12,v22,v13,v23,v33, bulk_m1)
 implicit none
 DOUBLE PRECISION                    ::  bulk_m1, BB, v11,v12,v22,v13,v23,v33
 DOUBLE PRECISION, DIMENSION(6,6)    ::  S
      BB=( S(1,1)+S(1,2)+S(1,3) )*v11&
        +( S(1,6)+S(2,6)+S(3,6) )*v12&
        +( S(1,5)+S(2,5)+S(3,5) )*v13&
        +( S(1,2)+S(2,2)+S(2,3) )*v22&
        +( S(1,4)+S(2,4)+S(3,4) )*v23&
        +( S(1,3)+S(2,3)+S(3,3) )*v33 
      bulk_m1 = 1D0/BB
END SUBROUTINE bulk_method_one

!==============================================
SUBROUTINE bulk_method_two(Pratio, sheainvar, bulk_m2)
 implicit none
   DOUBLE PRECISION      :: Pratio, sheainvar, bulk_m2
   bulk_m2 = ( 2.d0 * sheainvar * (1.d0 + Pratio) ) / (3.0d0 * (1.0d0  - 2.0d0 * Pratio))
END SUBROUTINE bulk_method_two
!==============================================

SUBROUTINE bulk_method_three(S, vecs,phi, theta, bulk_m3)
 implicit none
   DOUBLE PRECISION                    :: bulk_m3,d11,d22,d33,d23,d13,d12,phi,theta
   DOUBLE PRECISION, DIMENSION(6,6)    :: S
   DOUBLE PRECISION, DIMENSION(3)    :: vecs
   d11 =  vecs(1)*vecs(1)
   d22 =  vecs(2)*vecs(2)
   d33 =  vecs(3)*vecs(3)
   d23 = SQRT(2.0D0)* vecs(2)*vecs(3)
   d13 = SQRT(2.0D0)* vecs(1)*vecs(3)
   d12 = SQRT(2.0D0)* vecs(1)*vecs(2)
   
   bulk_m3 = 3.d0*( ( S(1,1) + S(2,1) + S(3,1) )*d11 +&
                    ( S(1,2) + S(2,2) + S(3,2) )*d22 +&
                    ( S(1,3) + S(2,3) + S(3,3) )*d33 +&
                    ( S(1,4) + S(2,4) + S(3,4) )*d23 +&
                    ( S(1,5) + S(2,5) + S(3,5) )*d13 +&
                    ( S(1,6) + S(2,6) + S(3,6) )*d12 ) 
   ! write(24,*)  bulk_m3       
END SUBROUTINE bulk_method_three
