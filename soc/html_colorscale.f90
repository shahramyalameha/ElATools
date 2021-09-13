
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
!===========================
SUBROUTINE colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3)
  IMPLICIT NONE
  ChARACTER(len=10)                :: namepro ,cval1 ,cval2 ,cval3
    integer                        :: naumbers 

 if (namepro=="hard".and. naumbers==1) then
  IF (cval1=="n") then
   WRITE(66,"(a)")"   colorscale: [['0','#002EFF'],['1', '#002EFF']],"
   ! WRITE(66,"(a)")"   colorscale: [['0','rgb(22,136,51)'],['0.125','rgb(61,153,85)'],['0.25','rgb(121,178,136)'],['0.375','rgb(181,204,187)'],&
                                  ! ['0.5','rgb(195,230,200)'],['0.625','rgb(181,204,187)'],['0.75','rgb(121,178,136)'],['0.875','rgb(61,153,85)'],['1','rgb(22,136,51)']],"
  ELSE
     WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"
  ENDIF                                
 endif  
 if (namepro=="young".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"   colorscale: [['0','green'],['1', 'green']],"
   ! WRITE(66,"(a)")"   colorscale: [['0','rgb(22,136,51)'],['0.125','rgb(61,153,85)'],['0.25','rgb(121,178,136)'],['0.375','rgb(181,204,187)'],&
                                  ! ['0.5','rgb(195,230,200)'],['0.625','rgb(181,204,187)'],['0.75','rgb(121,178,136)'],['0.875','rgb(61,153,85)'],['1','rgb(22,136,51)']],"
  ELsE
     WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
  ENDIF
 endif
 if (namepro=="bulk".and. naumbers==1) then
   IF (cval1=="n") then 
    WRITE(66,"(a)")"   colorscale: [['0','green'],['1', 'green']],"
   !WRITE(66,"(a)")"   colorscale: [['0','rgb(22,136,51)'],['0.125','rgb(61,153,85)'],['0.25','rgb(121,178,136)'],['0.375','rgb(181,204,187)'],&
   !                               ['0.5','rgb(195,230,200)'],['0.625','rgb(181,204,187)'],['0.75','rgb(121,178,136)'],['0.875','rgb(61,153,85)'],['1','rgb(22,136,51)']],"
   ELSE
        WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
   ENDIF
endif
 if (namepro=="shear".and. naumbers==1) then
   IF (cval1=="n") then   
    WRITE(66,"(a)")"   colorscale: [['0','blue'],['1', 'blue']],"
    !WRITE(66,"(a)")"   colorscale: [['0','rgb(22,136,51)'],['0.125','rgb(61,153,85)'],['0.25','rgb(121,178,136)'],['0.375','rgb(181,204,187)'],&
    !                               ['0.5','rgb(195,230,200)'],['0.625','rgb(181,204,187)'],['0.75','rgb(121,178,136)'],['0.875','rgb(61,153,85)'],['1','rgb(22,136,51)']],"
   ELSE
        WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
   ENDIF    
 endif 
 if (namepro=="shear".and. naumbers==2) then
  IF (cval2=="n") then 
    WRITE(66,"(a)")"   colorscale: [['0','green'],['1', 'green']],"
    !WRITE(66,"(a)")"   colorscale: [['0','rgb(59,76,192)'],['0.125','rgb(98,130,234)'],['0.25','rgb(141,176,254)'],['0.375','rgb(184,208,249)'],&
     !                              ['0.5','rgb(207,223,250)'],['0.625','rgb(184,208,249)'],['0.75','rgb(141,176,254)'],['0.875','rgb(98,130,234)'],['1','rgb(59,76,192)']],"
 ELSE
   WRITE(66,"(5a)")"   colorscale: [['0','",cval2,"'],['1', '",cval2,"']],"  
 ENDIF     
 endif 
 if (namepro=="poisson".or.namepro=="poi".and. naumbers==1) then
  IF (cval1=="n") then  
  WRITE(66,"(a)")"   colorscale: [['0','blue'],['1', 'blue']],"
    ! WRITE(66,"(a)")"   colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
    ! ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['1','rgb(180,4,38)']],"
  ELSE
   WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
  ENDIF                                 
 endif
 if (namepro=="poisson".or.namepro=="poi".and. naumbers==2) then
  IF (cval2=="n") then 
   WRITE(66,"(a)")"   colorscale: [['0','green'],['1', 'green']],"
    ! WRITE(66,"(a)")"   colorscale: [['0','rgb(22,136,51)'],['0.125','rgb(61,153,85)'],['0.25','rgb(121,178,136)'],['0.375','rgb(181,204,187)'],&
    ! ['0.5','rgb(195,230,200)'],['0.625','rgb(181,204,187)'],['0.75','rgb(121,178,136)'],['0.875','rgb(61,153,85)'],['1','rgb(22,136,51)']],"
  ELSE
    WRITE(66,"(5a)")"   colorscale: [['0','",cval2,"'],['1', '",cval2,"']],"  
  ENDIF                                  
 endif
 if (namepro=="poisson".or.namepro=="poi".and. naumbers==3) then
 IF (cval3=="n") then 
  WRITE(66,"(a)")"   colorscale: [['0','red'],['1', 'red']],"
   ! WRITE(66,"(a)")"   colorscale: [['0','rgb(59,76,192)'],['0.125','rgb(98,130,234)'],['0.25','rgb(141,176,254)'],['0.375','rgb(184,208,249)'],&
   !  ['0.5','rgb(207,223,250)'],['0.625','rgb(184,208,249)'],['0.75','rgb(141,176,254)'],['0.875','rgb(98,130,234)'],['1','rgb(59,76,192)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval3,"'],['1', '",cval3,"']],"  
 ENDIF                                 
 endif   
 if (namepro=="comp".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','blue'],['1', 'blue']],"
  !  WRITE(66,"(a)")"   colorscale: [['0','rgb(22,136,51)'],['0.125','rgb(61,153,85)'],['0.25','rgb(121,178,136)'],['0.375','rgb(181,204,187)'],&
   !                                ['0.5','rgb(195,230,200)'],['0.625','rgb(181,204,187)'],['0.75','rgb(121,178,136)'],['0.875','rgb(61,153,85)'],['1','rgb(22,136,51)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
 ENDIF 
 endif  
 if (namepro=="comp".and. naumbers==2) then
 IF (cval2=="n") then 
  WRITE(66,"(a)")"   colorscale: [['0','green'],['1', 'green']],"
   ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
   !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['1','rgb(180,4,38)']],"
  ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval2,"'],['1', '",cval2,"']],"  
 ENDIF
 endif         
 
if (namepro=="comp".and. naumbers==3) then
 IF (cval3=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','red'],['1', 'red']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['1','rgb(180,4,38)']],"
  ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval3,"'],['1', '",cval3,"']],"  
 ENDIF
endif  

if (namepro=="pp".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#660066'],['1', '#660066']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
 ENDIF
endif 
if (namepro=="pf".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#AA40FC'],['1', '#AA40FC']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
 ENDIF
endif  
if (namepro=="ps".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#316395'],['1', '#316395']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
 ENDIF
endif 

if (namepro=="gp".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#660000'],['1', '#660000']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
 ENDIF
endif 
if (namepro=="gf".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#F00000'],['1', '#F00000']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
 ENDIF
endif  
if (namepro=="gs".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#ff5f0e'],['1', '#ff5f0e']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
 ENDIF
endif 

if (namepro=="pfp".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#4D20B3'],['1', '#4D20B3']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
 ENDIF
endif 
if (namepro=="pff".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#B3204D'],['1', '#B3204D']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
 ENDIF
endif  
if (namepro=="pfs".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#309D0D'],['1', '#309D0D']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
 ENDIF
endif 
if (namepro=="km".and. naumbers==1) then
 IF (cval1=="n") then
  WRITE(66,"(a)")"   colorscale: [['0','#B22471'],['1', '#B22471']],"
  ! WRITE(66,"(a)")"    colorscale: [['0','rgb(180,4,38)'],['0.125','rgb(222,96,77)'],['0.25','rgb(244,154,123)'],['0.375','rgb(245,196,173)'],&
  !                                 ['0.5','rgb(246,216,201)'],['0.625','rgb(245,196,173)'],['0.75','rgb(244,154,123)'],['0.875','rgb(222,96,77)'],['1','rgb(180,4,38)']],"
 ELSE
  WRITE(66,"(5a)")"   colorscale: [['0','",cval1,"'],['1', '",cval1,"']],"  
 ENDIF
endif 
END SUBROUTINE
