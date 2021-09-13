
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
!===========================
SUBROUTINE start_cord_web(cord_ename)
  IMPLICIT NONE
     ChARACTER(len=1)                :: cord_ename
  if (cord_ename=="x") then
    WRITE(66,"(a)")'   x: ['
    WRITE(66,"(a)")'      ['
    endif
   if (cord_ename=="y") then
    WRITE(66,"(a)")'   y: ['
    WRITE(66,"(a)")'      ['
    endif
  if (cord_ename=="z") then
    WRITE(66,"(a)")'   z: ['
    WRITE(66,"(a)")'      ['
    endif       
END SUBROUTINE
