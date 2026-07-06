 !```````````````````````````````````````````````````````````````````````````````````````````
 ! COPYRIGHT (C) 2018 SHAHRAM YALAMEHA <YALAMEHA93@GMAIL.COM> , <SH.YALAMEHA@SCI.UI.AC.IR>, `
 !               PLEASE REPORT BUGS OR SUGGESTIONS TO:  YALAMEHA93@GMAIL.COM                `
 !                                                                                          `
 !```````````````````````````````````````````````````````````````````````````````````````````
 ! SUBROUTINE: FOR 2D MATERIAL , CALCULATE VECTORS AND (THETA, PIH) IN THE (001) PLANE

SUBROUTINE TWOD_CALC(VV11,VV12,VV13,VV22,VV23,VV33,MMX,KKY,LLZ,SMKL,I,PHI,THETA,THETA_POINT,VEC)
   IMPLICIT NONE
   DOUBLE PRECISION                  :: SMKL,SMKL2,&
      TWODTHETA=0D0,&
      MMX, &
      KKY, &
      LLZ, &
      VV11,&
      VV12,&
      VV13,&
      VV22,&
      VV23,&
      VV33,THETA,PHI
   DOUBLE PRECISION, DIMENSION(3)               :: VEC
   DOUBLE PRECISION,PARAMETER                   :: PI=3.1415926535897932384626433832795D0,EE=0.00001D0
   INTEGER                                      :: I,J,THETA_POINT

   TWODTHETA=DBLE(I)*2D0*PI/THETA_POINT
   IF ((ABS(MMX).LE.EE).AND.(ABS(LLZ).LE.EE)) THEN
      VEC(1) = COS(TWODTHETA)
      VEC(2) = 0D0
      VEC(3) = SIN(TWODTHETA)
   ELSE
      SMKL=SQRT(MMX**2D0+LLZ**2D0)
      VEC(1) = ( LLZ*COS(TWODTHETA)-MMX*KKY*SIN(TWODTHETA)    )/SMKL
      VEC(2) = ( (MMX**2D0+LLZ**2D0)*SIN(TWODTHETA)           )/SMKL
      VEC(3) = ( -MMX*COS(TWODTHETA)-KKY*LLZ*SIN(TWODTHETA)   )/SMKL
   ENDIF

   IF (1D0-( ABS( VEC(3) ) ).LE.EE) THEN
      THETA = 0D0
      PHI   = 0D0
   ELSE
      THETA=ACOS( VEC(3) )
      IF (VEC(2).LE.EE) THEN
         IF ( ( ABS(VEC(1))-ABS( SIN(THETA) ) ).LE.EE ) THEN
            SMKL2=0.999999D0*VEC(1)/SIN(THETA)
         ELSE
            SMKL2=VEC(1)/SIN(THETA)
         ENDIF
         PHI=SIGN( ACOS(SMKL2),VEC(2) )
      ELSE
         PHI=SIGN( ACOS(VEC(1)/SIN(THETA)),VEC(2) )
      ENDIF
   ENDIF
   VV11 = VEC(1)*VEC(1) ; VV12 = VEC(1)*VEC(2)
   VV13 = VEC(1)*VEC(3) ; VV22 = VEC(2)*VEC(2)
   VV23 = VEC(2)*VEC(3) ; VV33 = VEC(3)*VEC(3)
   !WRITE(*,*)VV11,VV13,VV23,VV12,VV22,VV33

END SUBROUTINE





