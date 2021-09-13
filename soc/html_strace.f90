
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
!===========================
SUBROUTINE start_trace_web(naumbers)
  IMPLICIT NONE
  integer                :: naumbers 
  
if (naumbers==1) then
    WRITE(66,"(a)")' var trace1 ='
    WRITE(66,"(a)")'{'
endif
if (naumbers==2) then
    WRITE(66,"(a)")' var trace2 ='
    WRITE(66,"(a)")'{'
endif
if (naumbers==3) then
    WRITE(66,"(a)")' var trace3 ='
    WRITE(66,"(a)")'{'
endif
END SUBROUTINE
