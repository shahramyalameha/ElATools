
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
    WRITE(66,"(a)") "   lighting: commonLighting, hovertemplate: commonHoverTemplate,"
    WRITE(66,"(a,F5.2,a)")"   opacity:", opacity,","
END SUBROUTINE   
!===========================

SUBROUTINE setparaslice_web(width, color, namepro)
  IMPLICIT NONE
  INTEGER                          :: width 
  ChARACTER(len=11)                 :: color 
  ChARACTER(len=10)                :: namepro, label_pro 
  If(namepro=="max") then
    label_pro="Maximum"
  elseIf(namepro=="min") then
    label_pro="Minimum"
  elseIf(namepro=="neg") then
  label_pro="Negatine"
   else
   label_pro=" "
  endif
     WRITE(66,"(3a)")"   type: 'scatter3d',"
     WRITE(66,"(3a)")"   mode: 'lines',"
     WRITE(66,"(A,I2,4A)")"   line: { width:", width,",color:'",trim(color),"'},"
     WRITE(66,"(3a)")"   zsmooth: 'fast ',"
     WRITE(66,"(3a)")"   showscale: false ,"
     WRITE(66,"(3a)")"   name: '", trim(label_pro),"' "     
     WRITE(66,"(3a)")"}; "
END SUBROUTINE 
   !==========================
SUBROUTINE contours_web(contours_line)   
     ChARACTER(len=5)                 :: contours_line
    WRITE(66,"(9a)")"    contours: {x :{ show:",contours_line,", color: 'rgb(192, 192, 192, 0.4)', width: 1},&
                                    y :{ show:",contours_line,", color: 'rgb(192, 192, 192, 0.4)', width: 1},&
                                    z :{ show:",contours_line,", color: 'rgb(192, 192, 192, 0.4)', width: 1}}"
    WRITE(66,"(a)")"}; " 
END SUBROUTINE 
!=================================
