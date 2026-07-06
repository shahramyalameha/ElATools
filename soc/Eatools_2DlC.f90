!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 2D MATERIAL , CALCULATED 2D linear compressibility
  
SUBROUTINE lcomp_2D(vec ,phi,phi_lcomp,l)! 
 implicit none

 DOUBLE PRECISION                  :: vv11,vv22,vv33,Y1,Y2,A,B,T,phi,k11,k22,k12
 DOUBLE PRECISION, DIMENSION(2010)  :: phi_lcomp,phi_lcomp_test
 DOUBLE PRECISION, DIMENSION(3,3)   :: C,S
  DOUBLE PRECISION, DIMENSION(2)  ::vec
 Integer::n,i,j,l
 n=3
 OPEN(58,FILE="Cij-2D.dat",STATUS='OLD',ACTION='READ')
 DO i=1,n
  READ(58,*) (C(i,j),j=1,n)
ENDDO
 close(58)
n=3
OPEN(51,FILE="Sij-2D.dat",STATUS='OLD',ACTION='READ')
DO i=1,n
  READ(51,*) (S(i,j),j=1,n)
ENDDO
 close(51)
 
        k11 = vec(2) * vec(2)
        k12 = vec(1) * vec(2)
        k22 = vec(1) * vec(1)

       ! # Calculate linear compressibility for 2D
  phi_lcomp(l) = ((S(1, 1) + S(1, 2)) * k11 +&
                  (S(1, 3) + S(2, 3)) * k12 +&
                  (S(1, 2) + S(2, 2)) * k22) 
 
 
 
Write(*,*) phi, phi_lcomp(l)

end SUBROUTINE
