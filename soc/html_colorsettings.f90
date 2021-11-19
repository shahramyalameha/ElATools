
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2021 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
!===========================
SUBROUTINE color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3)
  IMPLICIT NONE
  ChARACTER(len=10)                :: namepro ,cval1 ,cval2 ,cval3
    integer                        :: naumbers 


 if (namepro=="young2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#1FCB00'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
 !==================
 if (namepro=="bulk2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: 'green'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif 
 !==================
 if (namepro=="pugh2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#4BDB24'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Max. Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif 
 if (namepro=="pugh2d".and. naumbers==2) then
  IF (cval2=="n") then 
   WRITE(66,"(a)")"line: {color: 'blue'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval2,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Min. Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
 !==================
 if (namepro=="shear2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#4BDB24'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Max. Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif 
 if (namepro=="shear2d".and. naumbers==2) then
  IF (cval2=="n") then 
   WRITE(66,"(a)")"line: {color: 'blue'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval2,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Min. Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
 !==================  
  if (namepro=="com2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#4BDB24'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif 
 if (namepro=="com2d".and. naumbers==2) then
  IF (cval2=="n") then 
   WRITE(66,"(a)")"line: {color: 'red'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval2,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Negative',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
 !==================
  !==================  
  if (namepro=="poi2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#4BDB24'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Max. Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif 
 if (namepro=="poi2d".and. naumbers==2) then
  IF (cval2=="n") then 
   WRITE(66,"(a)")"line: {color: '#003DFC'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval2,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Min. Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
  if (namepro=="poi2d".and. naumbers==3) then
  IF (cval3=="n") then 
   WRITE(66,"(a)")"line: {color: 'red'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval3,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Negative',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
 !==================
  if (namepro=="hard2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#1FCB00'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
  !==================
  if (namepro=="pp2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#660066'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
  !==================  
 !==================
  if (namepro=="pf2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#AA40FC'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
 !==================
  if (namepro=="ps2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#316395'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif  
  !==================
  if (namepro=="gp2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#660000'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
  !==================  
 !==================
  if (namepro=="gf2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#F00000'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
 !==================
  if (namepro=="gs2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#ff5f0e'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif 
   !==================
  if (namepro=="pfp2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#4D20B3'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
  !==================  
 !==================
  if (namepro=="pff2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#B3204D'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
 !==================
  if (namepro=="pfs2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#309D0D'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif
  !==================
  if (namepro=="km2d".and. naumbers==1) then
  IF (cval1=="n") then 
   WRITE(66,"(a)")"line: {color: '#B22471'},"
  ELsE
     WRITE(66,"(5a)")"line: {color: '",cval1,"'},"  
  ENDIF
   WRITE(66,"(a)")"   mode: 'lines',"
   WRITE(66,"(a)")"  name: 'Positive',"
   WRITE(66,"(a)")"  type: 'scatterpolar'"
   WRITE(66,"(a)")"};"  
 endif 
  
END SUBROUTINE   
