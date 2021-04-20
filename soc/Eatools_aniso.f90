!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL , Detection of anisotropy elastic modulus.

subroutine anisotropy(ma,mi,Axx)
  implicit NONE
  
  DOUBLE PRECISION       ::  mi, ma, Axx
 if (mi > 0) then
   Axx=ma/mi
 else
   Axx=1d0/0.d0
 endif
End subroutine
