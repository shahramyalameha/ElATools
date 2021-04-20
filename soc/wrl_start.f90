
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; set start format for wrl files: dat2wrl program. 

SUBROUTINE start_wrl(color,color2,V_sc)
  DOUBLE PRECISION, DIMENSION(3) :: color,color2
  DOUBLE PRECISION:: V_sc
  
  
    WRITE(41,'(A)') '#VRML V2.0 utf8'
    WRITE(41,*) '    #YLM(c) 2018'        
    WRITE(41,*) ' Background {'
    WRITE(41,"(A,3F6.3,A)") '   skyColor[',color(1),color(2),color(3),']'
    WRITE(41,*) ' }'
    WRITE(41,*) 'Viewpoint {'
    WRITE(41,*) '  #fieldofview 0.78539'
    WRITE(41,*) '  position 0 0',V_sc
    WRITE(41,*) '  orientation 0 0 1 0'
    WRITE(41,*) '  jump TRUE'
    WRITE(41,*) '  description "top view"'
    WRITE(41,*) '}'
    WRITE(41,*) 'Viewpoint {'
    WRITE(41,*) '  #fieldofview 0.78539'
    WRITE(41,*) '  position 0 0',V_sc*2d0
    WRITE(41,*) '  orientation 0 0 1 0'
    WRITE(41,*) '  jump TRUE'
    WRITE(41,*) '  description "v2"'
    WRITE(41,*) '}'  
    WRITE(41,*) 'Viewpoint {'
    WRITE(41,*) '  #fieldofview 0.78539'
    WRITE(41,*) '  position 0 0 ',V_sc*5d0
    WRITE(41,*) '  orientation 0 0 1 0'
    WRITE(41,*) '  jump TRUE'
    WRITE(41,*) '  description "v5"'
    WRITE(41,*) '}'       
    WRITE(41,*) 'Viewpoint {'
    WRITE(41,*) '  #fieldofview 0.78539'
    WRITE(41,*) '  position 0 0',V_sc/2d0
    WRITE(41,*) '  orientation 0 0 1 0'
    WRITE(41,*) '  jump TRUE'
    WRITE(41,*) '  description "v/2"'
    WRITE(41,*) '}'   
    WRITE(41,*) 'Viewpoint {'
    WRITE(41,*) '  #fieldofview 0.78539'
    WRITE(41,*) '  position 0 0',V_sc/10d0
    WRITE(41,*) '  orientation 0 0 1 0'
    WRITE(41,*) '  jump TRUE'
    WRITE(41,*) '  description "v1/10"'
    WRITE(41,*) '}'  
    WRITE(41,*) 
    WRITE(41,*) ' DEF axisfont  FontStyle {'
    WRITE(41,*) ' size ',V_sc*.1d0
    WRITE(41,*) ' family "SANS"'
    WRITE(41,*) ' }' 
    WRITE(41,*)
    WRITE(41,*) ' Transform {'
    WRITE(41,*) ' translation',V_sc*.45d0  ,'0 0'
    WRITE(41,*) ' rotation 0 0 1 1.57'
    WRITE(41,*) '  children ['
    WRITE(41,*) '  DEF axis Shape{'
    WRITE(41,*) '   appearance Appearance {'
    WRITE(41,*) '        material Material {'
    WRITE(41,"(A,3F6.3,A)") '          diffuseColor',color2(1),color2(2),color2(3)
    WRITE(41,*) '        }'
    WRITE(41,*) '   }'
    WRITE(41,*) '   geometry Cylinder {'
    WRITE(41,*) '        height   ', V_sc*.9d0
    WRITE(41,*) '        radius   ', V_sc*.005d0
    WRITE(41,*) '   }'
    WRITE(41,*) '  }'
    WRITE(41,*) '  ]'
    WRITE(41,*) ' }'
    WRITE(41,*) ' Transform {'
    WRITE(41,*) ' translation',V_sc*.95d0  ,'0 0'
    WRITE(41,*) ' rotation 0 0 1 -1.57'
    WRITE(41,*) '  children ['
    WRITE(41,*) '  DEF arrow Shape{'
    WRITE(41,*) '   appearance Appearance {'
    WRITE(41,*) '        material Material {'
    WRITE(41,"(A,3F6.3,A)") '          diffuseColor',color2(1),color2(2),color2(3)
    WRITE(41,*) '        }'
    WRITE(41,*) '   }'
    WRITE(41,*) '   geometry Cone {'
    WRITE(41,*) '                height      ',V_sc*.1d0
    WRITE(41,*) '                bottomRadius',V_sc*.01d0
    WRITE(41,*) '   }'
    WRITE(41,*) '  }'
    WRITE(41,*) '  ]'
    WRITE(41,*) ' }'
    WRITE(41,*) ' Transform {'
    WRITE(41,*) ' translation',V_sc*.95d0  ,'0 0'
    WRITE(41,*) '  children ['
    WRITE(41,*) '  Shape{'
    WRITE(41,*) '   appearance Appearance {'
    WRITE(41,*) '        material Material {'
    WRITE(41,"(A,3F6.3,A)") '          diffuseColor',color2(1),color2(2),color2(3)
    WRITE(41,*) '        }'
    WRITE(41,*) '   }'
    WRITE(41,*) '   geometry Text {'
    WRITE(41,*) '     string ["X"]'
    WRITE(41,*) '     fontStyle USE axisfont'
    WRITE(41,*) '   }'
    WRITE(41,*) '  }'
    WRITE(41,*) '  ]'
    WRITE(41,*) ' }'
    WRITE(41,*) ' Transform {'
    WRITE(41,*) ' translation  0',V_sc*.45d0  ,'0'
    WRITE(41,*) '  children ['
    WRITE(41,*) ' USE axis'
    WRITE(41,*) ' ]'
    WRITE(41,*) ' }'
    WRITE(41,*) ' Transform {'
    WRITE(41,*) ' translation   0',V_sc*.95d0  ,'0'
    WRITE(41,*) '  children ['
    WRITE(41,*) ' USE arrow'
    WRITE(41,*) '  ]'
    WRITE(41,*) ' }'
    WRITE(41,*) ' Transform {'
    WRITE(41,*) ' translation   0',V_sc*.95d0  ,'0'
    WRITE(41,*) '  children ['
    WRITE(41,*) '  Shape{'
    WRITE(41,*) '   appearance Appearance {'
    WRITE(41,*) '        material Material {'
    WRITE(41,"(A,3F6.3,A)") '          diffuseColor',color2(1),color2(2),color2(3)
    WRITE(41,*) '        }'
    WRITE(41,*) '   }'
    WRITE(41,*) '   geometry Text {'
    WRITE(41,*) '     string ["Y"]'
    WRITE(41,*) '     fontStyle USE axisfont'
    WRITE(41,*) '   }'
    WRITE(41,*) '  }'
    WRITE(41,*) '  ]'
    WRITE(41,*) ' }'
    WRITE(41,*) ' Transform {'
    WRITE(41,*) ' translation  0 0',V_sc*.45d0
    WRITE(41,*) ' rotation 1 0 0 -1.57'
    WRITE(41,*) '  children ['
    WRITE(41,*) ' USE axis'
    WRITE(41,*) ' ]'
    WRITE(41,*) ' }'
    WRITE(41,*) ' Transform {'
    WRITE(41,*) ' translation  0 0',V_sc*.95d0
    WRITE(41,*) ' rotation 1 0 0 1.57'
    WRITE(41,*) '  children ['
    WRITE(41,*) ' USE arrow'
    WRITE(41,*) '  ]'
    WRITE(41,*) ' }'
    WRITE(41,*) ' Transform {'
    WRITE(41,*) ' translation  0 0',V_sc*.95d0
    WRITE(41,*) ' rotation 1 0 0 1.57'
    WRITE(41,*) '  children ['
    WRITE(41,*) '  Shape{'
    WRITE(41,*) '   appearance Appearance {'
    WRITE(41,*) '        material Material {'
    WRITE(41,"(A,3F6.3,A)") '          diffuseColor',color2(1),color2(2),color2(3)
    WRITE(41,*) '        }'
    WRITE(41,*) '   }'
    WRITE(41,*) '   geometry Text {'
    WRITE(41,*) '     string ["Z"]'
    WRITE(41,*) '     fontStyle USE axisfont'
    WRITE(41,*) '   }'
    WRITE(41,*) '  }'
    WRITE(41,*) '  ]'
    WRITE(41,*) ' }'
  END SUBROUTINE start_wrl
