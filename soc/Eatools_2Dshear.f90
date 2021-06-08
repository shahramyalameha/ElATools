!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 2D MATERIAL , CALCULATED shear modulus.
  
 SUBROUTINE shear_2D(vv11,vv22,vv33,phi,phi_shear,MaxShear ,l)
 implicit none
 DOUBLE PRECISION                    :: vv11,vv22,vv33,Y1,Y2,A,B,T,Y3,MaxShear,MinShear,phi,Pe,She
 DOUBLE PRECISION, DIMENSION(201)    :: phi_shear,Shear_max_phi
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
  She= (S(1,1)+S(2,2)-2.d0*S(1,2))*vv33+(1.d0/4.d0)*S(3,3)*(vv11+vv22-2.d0*vv33)
  phi_shear(l) =1D0/((4D0*She))                                ! method_ 2 by Sij (Recommend)
 !write(*,*)phi_shear(l)
  MaxShear=phi_shear(l)
 	 ! IF (l.EQ.0) THEN 
   !  MaxShear=phi_shear(l); MinShear=phi_shear(l);

	 ! ELSE
	  !  IF (phi_shear(l).GE.MaxShear) THEN
    !    MaxShear=phi_shear(l)

	  !  ENDIF  
	  !  IF (phi_shear(l).LE.MinShear) THEN
    !    MinShear=phi_shear(l)     
	  !  ENDIF  
	  !ENDIF

end SUBROUTINE
