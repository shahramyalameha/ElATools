!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 2D MATERIAL , CALCULATED poiison's retio.
 SUBROUTINE poisson_2D(vv11,vv22,vv33,phi,phi_poisson,MaxPratio,MinPratio,l)
  implicit none
  DOUBLE PRECISION                    :: vv11,vv22,vv33,Y1,Y2,A,B,T,Y3,MaxPratio,MinPratio,phi,Pe,E
  DOUBLE PRECISION, DIMENSION(2010)    :: phi_poisson,Pratio_max_phi
  DOUBLE PRECISION, DIMENSION(3,3)    :: C,S
  Integer                             :: n,i,j,l
  
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
  A  = ((C(1,1)*C(2,2)-C(1,2)*C(1,2))*(C(3,3)**(-1d0)))
  B  = 2D0*C(1,2)
  T  = A-B
  Y1 = C(1,1)+C(2,2)
  Y2 = (Y1-A)*vv33-C(1,2)*(vv11+vv22)
  Y3 = C(1,1)*vv11+C(2,2)*vv22
  !phi_poisson(l) = -1D0*(Y2)/(Y3+A-2d0*B)                    ! methoe_ 1 by Cij
  E=(S(1,1)*(vv22)+S(2,2)*(vv11)+(S(3,3)+2.d0*S(1,2))*vv33)   
  Pe=vv33*(S(1,1)+S(2,2)-S(3,3))+S(1,2)*(vv11+vv22)
  phi_poisson(l) =-(Pe/E)                                  ! method_ 2 by Sij (Recommend)
  !write(*,*)phi_young_test(l)
  IF (l.EQ.0) THEN 
    MaxPratio=phi_poisson(l); MinPratio=phi_poisson(l);   
  ELSE
    IF (phi_poisson(l).GE.MaxPratio) THEN
      MaxPratio=phi_poisson(l)
    END IF  
    IF (phi_poisson(l).LE.MinPratio) THEN
      MinPratio=phi_poisson(l)  
      !write(*,*) MinPratio
    END IF  
  END IF
end SUBROUTINE
