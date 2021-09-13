
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
!===========================
SUBROUTINE setpara_web(zsmooth,showscale,opacity)
  IMPLICIT NONE
  ChARACTER(len=5)                 :: zsmooth 
  ChARACTER(len=5)                 :: showscale 
  real                             :: opacity
    WRITE(66,"(3a)")"   zsmooth: '",zsmooth,"',"              !fast and slow
    WRITE(66,"(3a)")"   showscale: ", showscale,","            ! true and false
    WRITE(66,"(a)") "   type: 'surface',"
    WRITE(66,"(a)") "   hoverinfo: 'text',"
    WRITE(66,"(a,F5.2,a)")"   opacity:", opacity,","
END SUBROUTINE   
   !==========================
SUBROUTINE contours_web(contours_line)   
     ChARACTER(len=5)                 :: contours_line
    WRITE(66,"(9a)")"    contours: {x :{ show:",contours_line,", color: 'rgb(192,192,192)'},&
                                   y :{ show:",contours_line,", color: 'rgb(192,192,192)'},&
                                   z :{ show:",contours_line,", color: 'rgb(192,192,192)'}}"
    WRITE(66,"(a)")"}; " 
END SUBROUTINE 
!=================================
