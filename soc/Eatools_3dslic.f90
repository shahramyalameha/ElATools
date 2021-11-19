!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL 

SUBROUTINE slic3D_calc(mmx,kky,llz,smkl,i,theta_point,vec_3dslic)
    IMPLICIT NONE
	  DOUBLE PRECISION                  :: smkl,smkl2,&
		                                  twoDTheta=0D0,&
		                                           mmx, &
		                                           kky, &
		                                           llz, &
                                               vv11,&
		                                           vv12,&
		                                           vv13,&
                                               vv22,&
		                                           vv23,&
		                                           vv33,phi
	  DOUBLE PRECISION, DIMENSION(3)               :: vec_3dslic
	  DOUBLE PRECISION,PARAMETER                   :: pi=3.1415926535897932384626433832795D0,ee=0.00001D0
	  INTEGER                                      :: i,j,theta_point
	  
	  
 
 
    !twoDTheta=DBLE(i)/360D0*2D0*pi
    !twoDTheta=DBLE(i)/DBLE(theta)*PI*2.0d0
    twoDTheta=DBLE(i)*2D0*pi/theta_point
   ! write(*,*)twoDTheta, i, theta
    IF ((ABS(mmx).LE.ee).AND.(ABS(llz).LE.ee)) THEN  
      vec_3dslic(1) = COS(twoDTheta)
      vec_3dslic(2) = 0D0
      vec_3dslic(3) = SIN(twoDTheta)        
    ELSE  
      smkl=SQRT(mmx**2D0+llz**2D0)   
      vec_3dslic(1) = ( llz*COS(twoDTheta)-mmx*kky*SIN(twoDTheta)     )/smkl
      vec_3dslic(2) = ( (mmx**2D0+llz**2D0)*SIN(twoDTheta)            )/smkl
      vec_3dslic(3) = ( -mmx*COS(twoDTheta)-kky*llz*SIN(twoDTheta)    )/smkl
    ENDIF



   END SUBROUTINE
