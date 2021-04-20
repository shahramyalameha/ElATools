!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL , CALCULATE vectors on any custom plane

    SUBROUTINE twoD_calc_wave(mmx,kky,llz,smkl,j,vec)
    IMPLICIT NONE
    DOUBLE PRECISION                        :: smkl,smkl2,&
                                                twoDTheta=0D0,&
                                                mmx, &
                                                kky, &
                                                llz, &
                                                theta,phi
    DOUBLE PRECISION, DIMENSION(3)               :: vec
    DOUBLE PRECISION,PARAMETER                   :: pi=3.1415926535897932384626433832795D0,ee=0.00001D0
    INTEGER                                      :: i,j


    twoDTheta=DBLE(j)/360D0*2D0*pi

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

END SUBROUTINE
