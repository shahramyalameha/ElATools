!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 2D MATERIAL , CALCULATE vectors and (theta, pihi) in the (001) plane

SUBROUTINE twoD_calc(vv11,vv12,vv13,vv22,vv23,vv33,mmx,kky,llz,smkl,i,phi,theta,vec)
    IMPLICIT NONE
	  DOUBLE PRECISION                              :: smkl,smkl2,&
		                                            twoDTheta=0D0,&
		                                           mmx, &
		                                           kky, &
		                                           llz, &
                                                   vv11,&
		                                           vv12,&
		                                           vv13,&
		                                           vv22,&
		                                           vv23,&
		                                           vv33,theta,phi
	  DOUBLE PRECISION, DIMENSION(3)               :: vec
	  DOUBLE PRECISION,PARAMETER                   :: pi=3.1415926535897932384626433832795D0,ee=0.00001D0
	  INTEGER                                      :: i,j
 
    twoDTheta=DBLE(i)/360D0*2D0*pi
    IF ((ABS(mmx).LE.ee).AND.(ABS(llz).LE.ee)) THEN  
      vec(1) = COS(twoDTheta)
      vec(2) = 0D0
      vec(3) = SIN(twoDTheta)        
    ELSE  
      smkl=SQRT(mmx**2D0+llz**2D0)   
      vec(1) = ( llz*COS(twoDTheta)-mmx*kky*SIN(twoDTheta)     )/smkl
      vec(2) = ( (mmx**2D0+llz**2D0)*SIN(twoDTheta)            )/smkl
      vec(3) = ( -mmx*COS(twoDTheta)-kky*llz*SIN(twoDTheta)    )/smkl
    ENDIF

    IF (1D0-(ABS(vec(3))).LE.ee) THEN 
      theta=0D0
      phi=0D0
    ELSE  
      theta=ACOS(vec(3))
      IF (vec(2).LE.ee) THEN
        IF ( ( ABS(vec(1))-ABS( SIN(theta) ) ).LE.ee ) THEN 
          smkl2=0.999999D0*vec(1)/SIN(theta)
        ELSE
          smkl2=vec(1)/SIN(theta)
        ENDIF      
        phi=sign(ACOS(smkl2),vec(2))     
      ELSE
        phi=sign(ACOS(vec(1)/SIN(theta)),vec(2))            
      ENDIF
    ENDIF
    vv11 = vec(1)*vec(1) ; vv12 = vec(1)*vec(2)
    vv13 = vec(1)*vec(3) ; vv22 = vec(2)*vec(2)
   vv23 = vec(2)*vec(3) ; vv33 = vec(3)*vec(3)
   !write(*,*)vv11,vv13,vv23,vv12,vv22,vv33

   END SUBROUTINE
 
