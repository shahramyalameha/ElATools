
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; set color_shape format for wrl files: dat2wrl program. 

SUBROUTINE shape_appearance_wrl(color_shape)
     DOUBLE PRECISION, DIMENSION(4) :: color_shape
     WRITE(41,*) '    appearance Appearance {'
     WRITE(41,*) '       material Material {'
     WRITE(41,*) '         diffuseColor',color_shape(1),color_shape(2),color_shape(3)
     WRITE(41,*) '         transparency',color_shape(4)
     WRITE(41,*) '       }'
     WRITE(41,*) '     }'     
  END SUBROUTINE shape_appearance_wrl
