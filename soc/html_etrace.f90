
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
SUBROUTINE   end_trace_web(naumbers) 
  IMPLICIT NONE
      integer                        :: naumbers 
      
  if (naumbers==1) then
    WRITE(66,"(a)")"  var data = [trace1]"
  endif
      
  if (naumbers==2) then
    WRITE(66,"(a)")"  var data = [trace1, trace2]"
  endif
      
  if (naumbers==3) then
    WRITE(66,"(a)")"  var data = [trace1, trace2, trace3]"
  endif        
   
END SUBROUTINE  
