!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 2D MATERIAL , CALCULATED young's modulus.
  
SUBROUTINE yound_2D(vv11,vv22,vv33,phi,phi_young,l )! 
 implicit none
 DOUBLE PRECISION :: vv11,vv22,vv33,Y1,Y2,A,B,T,phi,E,Pe
 DOUBLE PRECISION, DIMENSION(201)  :: phi_young,phi_young_test
 DOUBLE PRECISION, DIMENSION(3,3):: C,S
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
A=((C(1,1)*C(2,2)-C(1,2)*C(1,2))*(C(3,3)**(-1d0)))
B=2D0*C(1,2)
T=A-B
! phi_young(l)=A*((C(1,1)*vv11)+(C(2,2)*vv22)+T*vv33)**(-1d0) ! method_ 1 by Cij
E=(S(1,1)*(vv22)+S(2,2)*(vv11)+(S(3,3)+2.d0*S(1,2))*vv33)   ! method_ 2 by Sij (Recommend)
phi_young(l)=1.d0/E
   !Pe=vv33*(S(1,1)+S(2,2)-S(3,3))+S(1,2)*(vv11+vv22)
   !phi_young_test(l)=-(Pe/E)
    !write(*,*)phi_young_test(l)
end SUBROUTINE
