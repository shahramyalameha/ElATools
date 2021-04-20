!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, Detection of max. value of data.

SUBROUTINE FINDMAX(NAME,OUTMAX,TOUTMAX,N)
IMPLICIT NONE
DOUBLE PRECISION :: x ,y, OUTMAX,mode,TOUTMAX,z
INTEGER:: l,N
CHARACTER(len=30) :: NAME
OUTMAX=0d0
!NAME=' '
IF (N.EQ.1) THEN
  OPEN (79,file='2dcut_poisson.dat')

  DO l=1,360
    READ(79,*)x,y,z
    IF (y > OUTMAX) then
    !WRITE(*,*) x,y,z
    OUTMAX=y
  ENDIF 
ENDDO

IF (OUTMAX <= 2.5D0) THEN
  TOUTMAX=NINT(OUTMAX*1000D0/3D0)
  TOUTMAX= TOUTMAX/1000D0  
ENDIF
CLOSE(79)
ELSE
  OPEN (78,file=NAME)
  DO l=1,360
    READ(78,*)x , y,z
    IF (y > OUTMAX) then
      OUTMAX=y
    ENDIF
  ENDDO
  OUTMAX=INT(OUTMAX)
  CLOSE(78)
  mode=MOD(INT(OUTMAX),2)
  IF (mode.eq.0) THEN
    TOUTMAX=INT(OUTMAX)/4D0
  ELSE
    TOUTMAX=INT(OUTMAX)+1D0
    TOUTMAX=TOUTMAX/4D0
  ENDIF
  ! WRITE(*,*)mode,Toutmax,outmax
ENDIF
!WRITE(*,*) maxy
END


