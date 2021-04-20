
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 09/08/2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; open new win. of agr file.

SUBROUTINE Win_start_agr(Ng,Ns)
   IMPLICIT  NONE
    INTEGER :: ng,ns
    WRITE(2,'(a,I1,a,I1)')'@target G',Ng,'.S',Ns
    WRITE(2,'(a)')'@type xy'
END SUBROUTINE


