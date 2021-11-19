
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
!===========================
SUBROUTINE start_polar_web(polar_ename)
  IMPLICIT NONE
     ChARACTER(len=1)                :: polar_ename
  if (polar_ename=="r") then
    WRITE(66,"(a)")'   r    : ['
  endif
  if (polar_ename=="t") then
    WRITE(66,"(a)")'   theta: ['
  endif       
END SUBROUTINE
