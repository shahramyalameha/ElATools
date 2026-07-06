
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2021 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
!===========================
SUBROUTINE colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3)
  IMPLICIT NONE
  ChARACTER(len=11)                :: namepro ,cval1 ,cval2 ,cval3
    integer                        :: naumbers 
    
     write(*,*) cval1 ,cval2 ,cval3!,namepro,naumbers
!------------------------------------------------------------------------------ 
!------------------------------------------------------------------------------
 if (namepro=="hard".and. naumbers==1) then
   IF (cval1=="n") then   
    WRITE(66,"(a)")"   colorscale: [['0','#0021B7'],['0.5', '#959595'],['1', '#0021B7']],"
    !WRITE(66,"(a)")"   colorscale: [['0','rgb(22,136,51)'],['0.125','rgb(61,153,85)'],['0.25','rgb(121,178,136)'],['0.375','rgb(181,204,187)'],&
    !                               ['0.5','rgb(195,230,200)'],['0.625','rgb(181,204,187)'],['0.75','rgb(121,178,136)'],['0.875','rgb(61,153,85)'],['0.5', '#959595'],['1','rgb(22,136,51)']],"
   ELSE
        WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
   ENDIF    
 endif 
 if (namepro=="hard".and. naumbers==2) then
  IF (cval2=="n") then 
    WRITE(66,"(a)")"   colorscale: [['0','#018805'],['0.5', '#959595'],['1', '#018805']],"
    !WRITE(66,"(a)")"   colorscale: [['0','rgb(59,76,192)'],['0.125','rgb(98,130,234)'],['0.25','rgb(141,176,254)'],['0.375','rgb(184,208,249)'],&
     !                              ['0.5','rgb(207,223,250)'],['0.625','rgb(184,208,249)'],['0.75','rgb(141,176,254)'],['0.875','rgb(98,130,234)'],['0.5', '#959595'],['1','rgb(59,76,192)']],"
 ELSE
   WRITE(66,"(5a)")"   colorscale: [['0','",cval2,"'],['0.5', '#959595'],['1', '",cval2,"']],"  
 ENDIF     
 endif  
!------------------------------------------------------------------------------ 
 if (namepro=="young".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"   colorscale: [['0','#018805'],['0.5', '#959595'],['1', '#018805']],"
   ! WRITE(66,"(a)")"   colorscale: [['0','rgb(22,136,51)'],['0.125','rgb(61,153,85)'],['0.25','rgb(121,178,136)'],['0.375','rgb(181,204,187)'],&
                                  ! ['0.5','rgb(195,230,200)'],['0.625','rgb(181,204,187)'],['0.75','rgb(121,178,136)'],['0.875','rgb(61,153,85)'],['0.5', '#959595'],['1','rgb(22,136,51)']],"
  ELsE
     WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
  ENDIF
 endif
!------------------------------------------------------------------------------ 
 if (namepro=="bulk".and. naumbers==1) then
   IF (cval1=="n") then 
    WRITE(66,"(a)")"   colorscale: [['0','#018805'],['0.5', '#959595'],['1', '#018805']],"
   !WRITE(66,"(a)")"   colorscale: [['0','rgb(22,136,51)'],['0.125','rgb(61,153,85)'],['0.25','rgb(121,178,136)'],['0.375','rgb(181,204,187)'],&
   !                               ['0.5','rgb(195,230,200)'],['0.625','rgb(181,204,187)'],['0.75','rgb(121,178,136)'],['0.875','rgb(61,153,85)'],['0.5', '#959595'],['1','rgb(22,136,51)']],"
   ELSE
        WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
   ENDIF
endif
!------------------------------------------------------------------------------
 if (namepro=="shear".and. naumbers==1) then
   IF (cval1=="n") then   
    WRITE(66,"(a)")"   colorscale: [['0','#0021B7'],['0.5', '#959595'],['1', '#0021B7']],"
    !WRITE(66,"(a)")"   colorscale: [['0','rgb(22,136,51)'],['0.125','rgb(61,153,85)'],['0.25','rgb(121,178,136)'],['0.375','rgb(181,204,187)'],&
    !                               ['0.5','rgb(195,230,200)'],['0.625','rgb(181,204,187)'],['0.75','rgb(121,178,136)'],['0.875','rgb(61,153,85)'],['0.5', '#959595'],['1','rgb(22,136,51)']],"
   ELSE
        WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
   ENDIF    
 endif 
 if (namepro=="shear".and. naumbers==2) then
  IF (cval2=="n") then 
    WRITE(66,"(a)")"   colorscale: [['0','#018805'],['0.5', '#959595'],['1', '#018805']],"
    !WRITE(66,"(a)")"   colorscale: [['0','rgb(59,76,192)'],['0.125','rgb(98,130,234)'],['0.25','rgb(141,176,254)'],['0.375','rgb(184,208,249)'],&
     !                              ['0.5','rgb(207,223,250)'],['0.625','rgb(184,208,249)'],['0.75','rgb(141,176,254)'],['0.875','rgb(98,130,234)'],['0.5', '#959595'],['1','rgb(59,76,192)']],"
 ELSE
   WRITE(66,"(5a)")"   colorscale: [['0','",cval2,"'],['0.5', '#959595'],['1', '",cval2,"']],"  
 ENDIF     
 endif 
!------------------------------------------------------------------------------ 
 if (namepro=="poisson".or.namepro=="poi".and. naumbers==1) then
  IF (cval1=="n") then  
  WRITE(66,"(a)")"   colorscale: [['0','#0021B7'],['0.5', '#959595'],['1', '#0021B7']],"
    ! WRITE(66,"(a)")"   colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
    ! ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['0.5', '#959595'],['1','rgb(180,4,38)']],"
  ELSE
   WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
  ENDIF                                 
 endif
 if (namepro=="poisson".or.namepro=="poi".and. naumbers==2) then
  IF (cval2=="n") then 
   WRITE(66,"(a)")"   colorscale: [['0','#018805'],['0.5', '#959595'],['1', '#018805']],"
    ! WRITE(66,"(a)")"   colorscale: [['0','rgb(22,136,51)'],['0.125','rgb(61,153,85)'],['0.25','rgb(121,178,136)'],['0.375','rgb(181,204,187)'],&
    ! ['0.5','rgb(195,230,200)'],['0.625','rgb(181,204,187)'],['0.75','rgb(121,178,136)'],['0.875','rgb(61,153,85)'],['0.5', '#959595'],['1','rgb(22,136,51)']],"
  ELSE
    WRITE(66,"(5a)")"   colorscale: [['0','",cval2,"'],['0.5', '#959595'],['1', '",cval2,"']],"  
  ENDIF                                  
 endif
 if (namepro=="poisson".or.namepro=="poi".and. naumbers==3) then
 IF (cval3=="n") then 
  WRITE(66,"(a)")"   colorscale: [['0','#A00000'],['0.5', '#959595'],['1', '#A00000']],"
   ! WRITE(66,"(a)")"   colorscale: [['0','rgb(59,76,192)'],['0.125','rgb(98,130,234)'],['0.25','rgb(141,176,254)'],['0.375','rgb(184,208,249)'],&
   !  ['0.5','rgb(207,223,250)'],['0.625','rgb(184,208,249)'],['0.75','rgb(141,176,254)'],['0.875','rgb(98,130,234)'],['0.5', '#959595'],['1','rgb(59,76,192)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval3,"'],['0.5', '#959595'],['1', '",cval3,"']],"  
 ENDIF                                 
 endif
!------------------------------------------------------------------------------ 
if (namepro=="pugh".and. naumbers==1) then
   IF (cval1=="n") then   
    WRITE(66,"(a)")"   colorscale: [['0','#0021B7'],['0.5', '#959595'],['1', '#0021B7']],"
    !WRITE(66,"(a)")"   colorscale: [['0','rgb(22,136,51)'],['0.125','rgb(61,153,85)'],['0.25','rgb(121,178,136)'],['0.375','rgb(181,204,187)'],&
    !                               ['0.5','rgb(195,230,200)'],['0.625','rgb(181,204,187)'],['0.75','rgb(121,178,136)'],['0.875','rgb(61,153,85)'],['0.5', '#959595'],['1','rgb(22,136,51)']],"
   ELSE
        WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
   ENDIF    
 endif 
 if (namepro=="pugh".and. naumbers==2) then
  IF (cval2=="n") then 
    WRITE(66,"(a)")"   colorscale: [['0','#018805'],['0.5', '#959595'],['1', '#018805']],"
    !WRITE(66,"(a)")"   colorscale: [['0','rgb(59,76,192)'],['0.125','rgb(98,130,234)'],['0.25','rgb(141,176,254)'],['0.375','rgb(184,208,249)'],&
     !                              ['0.5','rgb(207,223,250)'],['0.625','rgb(184,208,249)'],['0.75','rgb(141,176,254)'],['0.875','rgb(98,130,234)'],['0.5', '#959595'],['1','rgb(59,76,192)']],"
 ELSE
   WRITE(66,"(5a)")"   colorscale: [['0','",cval2,"'],['0.5', '#959595'],['1', '",cval2,"']],"  
 ENDIF     
 endif   
!------------------------------------------------------------------------------    
 if (namepro=="comp".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#0021B7'],['0.5', '#959595'],['1', '#0021B7']],"
  !  WRITE(66,"(a)")"   colorscale: [['0','rgb(22,136,51)'],['0.125','rgb(61,153,85)'],['0.25','rgb(121,178,136)'],['0.375','rgb(181,204,187)'],&
   !                                ['0.5','rgb(195,230,200)'],['0.625','rgb(181,204,187)'],['0.75','rgb(121,178,136)'],['0.875','rgb(61,153,85)'],['0.5', '#959595'],['1','rgb(22,136,51)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
 ENDIF 
 endif  
 if (namepro=="comp".and. naumbers==2) then
 IF (cval2=="n") then 
  WRITE(66,"(a)")"   colorscale: [['0','#018805'],['0.5', '#959595'],['1', '#018805']],"
   ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
   !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['0.5', '#959595'],['1','rgb(180,4,38)']],"
  ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval2,"'],['0.5', '#959595'],['1', '",cval2,"']],"  
 ENDIF
 endif         
 
if (namepro=="comp".and. naumbers==3) then
 IF (cval3=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#A00000'],['0.5', '#959595'],['1', '#A00000']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['0.5', '#959595'],['1','rgb(180,4,38)']],"
  ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval3,"'],['0.5', '#959595'],['1', '",cval3,"']],"  
 ENDIF
endif  
!------------------------------------------------------------------------------
if (namepro=="pp".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#660066'],['0.5', '#959595'],['1', '#660066']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['0.5', '#959595'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
 ENDIF
endif 
!------------------------------------------------------------------------------
if (namepro=="pf".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#AA40FC'],['0.5', '#959595'],['1', '#AA40FC']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['0.5', '#959595'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
 ENDIF
endif  
!------------------------------------------------------------------------------
if (namepro=="ps".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#316395'],['0.5', '#959595'],['1', '#316395']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['0.5', '#959595'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
 ENDIF
endif 
!------------------------------------------------------------------------------
if (namepro=="gp".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#660000'],['0.5', '#959595'],['1', '#660000']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['0.5', '#959595'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
 ENDIF
endif 
!------------------------------------------------------------------------------
if (namepro=="gf".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#F00000'],['0.5', '#959595'],['1', '#F00000']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['0.5', '#959595'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
 ENDIF
endif  
!------------------------------------------------------------------------------
if (namepro=="gs".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#ff5f0e'],['0.5', '#959595'],['1', '#ff5f0e']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['0.5', '#959595'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
 ENDIF
endif 
!------------------------------------------------------------------------------
if (namepro=="pfp".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#4D20B3'],['0.5', '#959595'],['1', '#4D20B3']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['0.5', '#959595'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
 ENDIF
endif 
!------------------------------------------------------------------------------
if (namepro=="pff".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#B3204D'],['0.5', '#959595'],['1', '#B3204D']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['0.5', '#959595'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
 ENDIF
endif  
!------------------------------------------------------------------------------
if (namepro=="pfs".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#309D0D'],['0.5', '#959595'],['1', '#309D0D']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['0.5', '#959595'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
 ENDIF
endif 
!------------------------------------------------------------------------------
if (namepro=="km".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#B22471'],['0.5', '#959595'],['1', '#B22471']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['0.5', '#959595'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['0.5', '#959595'],['1', '",cval1,"']],"  
 ENDIF
endif 
END SUBROUTINE
!###########################################################################
!                           in       in      in     in     in     out
SUBROUTINE colorslice_web(namepro, naumbers, cval1t , cval2t , cval3t, color_line)
  IMPLICIT NONE
  ChARACTER(len=10)                :: namepro ,cval1t ,cval2t , cval3t, color_line
    integer                        :: naumbers 
    
   ! write(*,*) namepro,naumbers,cval1 ,cval2 ,cval3
   !------------------------------------------------------------------------------
 if (namepro=="you3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif  

 if (namepro=="km3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif 

 if (namepro=="hard3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif
  if (namepro=="hard3ds" .and. naumbers==2) then
  IF (cval1t=="n") then
   color_line = "#0021B7"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="bulk3ds" .or. namepro=="Bulk3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif 

if (namepro=="poi3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#0021B7"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="poi3ds" .and. naumbers==2) then
  IF (cval2t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval2t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif  

  if (namepro=="poi3ds" .and. naumbers==3) then
  IF (cval3t=="n") then
   color_line = "#A00000"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval3t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif 

 if (namepro=="she3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif
  if (namepro=="she3ds" .and. naumbers==2) then
  IF (cval2t=="n") then
   color_line = "#0021B7"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval2t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="comp3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif
  if (namepro=="comp3ds" .and. naumbers==2) then
  IF (cval2t=="n") then
   color_line = "#A00000"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval2t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif


 if (namepro=="pugh3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#0021B7"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="pugh3ds" .and. naumbers==2) then
  IF (cval1t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif  

  if (namepro=="pugh3ds" .and. naumbers==3) then
  IF (cval2t=="n") then
   color_line = "#A00000"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval2t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="pp3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#A00000"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="pf3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif  

  if (namepro=="ps3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#0021B7"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="pa3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#A00000"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="pa3ds" .and. naumbers==2) then
  IF (cval1t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval2t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif  

 if (namepro=="pa3ds" .and. naumbers==3) then
  IF (cval1t=="n") then
   color_line = "#0021B7"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval3t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif
 !----

  if (namepro=="gp3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#A00000"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="gf3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif  

  if (namepro=="gs3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#0021B7"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="ga3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#A00000"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="ga3ds" .and. naumbers==2) then
  IF (cval1t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval2t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif  

 if (namepro=="ga3ds" .and. naumbers==3) then
  IF (cval1t=="n") then
   color_line = "#0021B7"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval3t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif
  !----

  if (namepro=="pfp3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#A00000"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="pff3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif  

  if (namepro=="pfs3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#0021B7"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="pfa3ds" .and. naumbers==1) then
  IF (cval1t=="n") then
   color_line = "#A00000"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval1t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif

 if (namepro=="pfa3ds" .and. naumbers==2) then
  IF (cval1t=="n") then
   color_line = "#018805"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval2t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif  

 if (namepro=="pfa3ds" .and. naumbers==3) then
  IF (cval1t=="n") then
   color_line = "#0021B7"
   WRITE(*,"(2A)") " > Default color: ", color_line
  ELSE
     color_line= cval3t 
     WRITE(*,"(2A)") " > Custom color: ", color_line
  ENDIF                                
 endif
 END SUBROUTINE
