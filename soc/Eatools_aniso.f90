!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL , Detection of anisotropy elastic modulus.

subroutine anisotropy(ma,mi,Axx)
  USE ieee_arithmetic
  implicit NONE
  
  DOUBLE PRECISION       ::  mi, ma, Axx
 if (mi > 0) then
   Axx=ma/mi
 else
   !Axx=1d0/0.0d0
     IF (ieee_support_inf(Axx)) THEN
    Axx = ieee_value(Axx,  ieee_negative_inf)
  END IF
 endif
End subroutine


 
