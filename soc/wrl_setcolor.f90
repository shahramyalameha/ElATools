
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2021 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
!!  "wrl_diffuse_color: data file ( of ELATools code) to wrl file. 

SUBROUTINE set_colors(val, num_color,color_val_out,color_val_out2)
implicit none
  ChARACTER(len=10)               :: val
  DOUBLE PRECISION, DIMENSION(4)  :: color_val_out,color_val_out2,color_val_out3
  integer                         :: i,num_color
  IF (val=='young' .or. val=='Young' .or. val=='yon') THEN
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Young’s modulus,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
    WRITE(*,*)"___________________________________________________"
  endif
!============================================================================================
  IF (val=='shear'.or. val=='Shear' .or. val=='she') THEN
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Shear modulus,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    WRITE(*,*)"========= color of Max. Posivive value =========="
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,*)"========= ============================ =========="
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
 !-----
  if(num_color==2)then
    WRITE(*,*)"========= color of Min. Posivive value =========="
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out2(i)
    END do
    WRITE(*,*)"========= ============================ =========="
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out2
    WRITE(*,*)""
    call check_color(color_val_out2)
  endif
    WRITE(*,*)"___________________________________________________"
endif
 !============================================================================================
  IF (val=='pugh'.or. val=='Pugh' .or. val=='pug') then
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Pugh ratio,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    WRITE(*,*)"========= color of Max. Posivive value =========="
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,*)"========= ============================ =========="
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
 !-----
  if(num_color==2)then
    WRITE(*,*)"========= color of Min. Posivive value =========="
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out2(i)
    END do
    WRITE(*,*)"========= ============================ =========="
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out2
    WRITE(*,*)""
    call check_color(color_val_out2)
  endif
    WRITE(*,*)"___________________________________________________"
endif
 !============================================================================================
    IF (val=='poisson'.or. val=='Poisson' .or. val=='poi')  THEN
      WRITE(*,*)"___________________________________________________"
      WRITE(*,*)"Set the color of the Poisson’s ratio,"
      WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
      WRITE(*,*)""
    if(num_color==1)then
      WRITE(*,*)"========= color of Max. Posivive value =========="
      Do i=1,4
        if (i==1) write(*,*) "> set diffuse red   color :"
        if (i==2) write(*,*) "> set diffuse green color :"
        if (i==3) write(*,*) "> set diffuse blue  color :"
        if (i==4) write(*,*) "> set transp.             :"
        read(*,*)color_val_out(i)
      END do
      WRITE(*,*)"========= ============================ =========="
      WRITE(*,"(A,4F8.5)")"Set:",color_val_out
      WRITE(*,*)""
      call check_color(color_val_out)
    endif
   !-----
    if(num_color==2)then
      WRITE(*,*)"========= color of Min. Posivive value =========="
      Do i=1,4
        if (i==1) write(*,*) "> set diffuse red   color :"
        if (i==2) write(*,*) "> set diffuse green color :"
        if (i==3) write(*,*) "> set diffuse blue  color :"
        if (i==4) write(*,*) "> set transp.             :"
        read(*,*)color_val_out2(i)
      END do
      WRITE(*,*)"========= ============================ =========="
      WRITE(*,"(A,4F8.5)")"Set:",color_val_out2
      WRITE(*,*)""
      call check_color(color_val_out2)
    endif
!---
    if(num_color==3)then
      WRITE(*,*)"========= color of Negative value =========="
      Do i=1,4
        if (i==1) write(*,*) "> set diffuse red   color :"
        if (i==2) write(*,*) "> set diffuse green color :"
        if (i==3) write(*,*) "> set diffuse blue  color :"
        if (i==4) write(*,*) "> set transp.             :"
        read(*,*)color_val_out3(i)
      END do
      WRITE(*,*)"========= ============================ =========="
      WRITE(*,"(A,4F8.5)")"Set:",color_val_out3
      WRITE(*,*)""
      call check_color(color_val_out3)
    endif        
      WRITE(*,*)"___________________________________________________"   
  endif  
  !============================================================================================
  IF (val=='compress'.or. val=='com' .or. val=='comp') then
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Linear Compressibiliy,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    WRITE(*,*)"========= color of Posivive value =========="
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,*)"========= ============================ =========="
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
 !-----
  if(num_color==2)then
    WRITE(*,*)"========= color of Negative value =========="
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out2(i)
    END do
    WRITE(*,*)"========= ============================ =========="
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out2
    WRITE(*,*)""
    call check_color(color_val_out2)
  endif
    WRITE(*,*)"___________________________________________________"
endif
 !============================================================================================
  IF (val=='bulk'.or. val=='Bulk' .or. val=='bul') then
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Bulk modulus,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
    WRITE(*,*)"___________________________________________________"
  endif
!============================================================================================
  if (val=='PhaseP'.or. val=='phasep' .or. val=='pp') then
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Phase velocity-P mode,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
    WRITE(*,*)"___________________________________________________"
  endif
!============================================================================================
  if (val=='PhaseF'.or. val=='phasef' .or. val=='pf') then
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Phase velocity-F mode,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
    WRITE(*,*)"___________________________________________________"
  endif
 !============================================================================================
  if (val=='PhaseS'.or. val=='phases' .or. val=='ps') then
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Phase velocity-S mode,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
    WRITE(*,*)"___________________________________________________"
  endif
 !============================================================================================
  if (val=='GroupP'.or. val=='groupp' .or. val=='gp') then
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Group velocity-P mode,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
    WRITE(*,*)"___________________________________________________"
  endif
  !============================================================================================
  if (val=='Groupf'.or. val=='groupf' .or. val=='gf') then
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Group velocity-F mode,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
    WRITE(*,*)"___________________________________________________"
  endif
  !============================================================================================
  if (val=='GroupS'.or. val=='groups' .or. val=='gs') then
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Group velocity-S mode,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
    WRITE(*,*)"___________________________________________________"
  endif  
   !============================================================================================
  if (val=='PFactP'.or. val=='pfoupp' .or. val=='pfp') THEN
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Power flow angle-P mode,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
    WRITE(*,*)"___________________________________________________"
  endif 
   !============================================================================================
  if (val=='PFactF'.or. val=='pfoupf' .or. val=='pff') then
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Power flow angle-F mode,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
    WRITE(*,*)"___________________________________________________"
  endif 
   !============================================================================================
  if (val=='PFactS'.or. val=='pfoups' .or. val=='pfs') then
    WRITE(*,*)"___________________________________________________"
    WRITE(*,*)"Set the color of the Power flow angle-S mode,"
    WRITE(*,*)"by [Red, Green, Blue, Transparency] between 0 to 1"
    WRITE(*,*)""
  if(num_color==1)then
    Do i=1,4
      if (i==1) write(*,*) "> set diffuse red   color :"
      if (i==2) write(*,*) "> set diffuse green color :"
      if (i==3) write(*,*) "> set diffuse blue  color :"
      if (i==4) write(*,*) "> set transp.             :"
      read(*,*)color_val_out(i)
    END do
    WRITE(*,"(A,4F8.5)")"Set:",color_val_out
    WRITE(*,*)""
    call check_color(color_val_out)
  endif
    WRITE(*,*)"___________________________________________________"
  endif    
  end subroutine



 !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
subroutine check_color(color_val_out)
  DOUBLE PRECISION, DIMENSION(4)  ::color_val_out
  if (color_val_out(1)>color_val_out(2).and.color_val_out(1)>color_val_out(3).and. color_val_out(2)==0.0.and.color_val_out(3)==0.0) WRITE(*,*)"---> Reddish"
  if (color_val_out(2)>color_val_out(1).and.color_val_out(2)>color_val_out(3).and. color_val_out(1)==0.0.and.color_val_out(3)==0.0) WRITE(*,*)"---> Greenish"
  if (color_val_out(3)>color_val_out(1).and.color_val_out(3)>color_val_out(2).and. color_val_out(1)==0.0.and.color_val_out(2)==0.0) WRITE(*,*)"---> Blueish"
  if (color_val_out(1)>color_val_out(2).and.color_val_out(3)>color_val_out(2)) WRITE(*,*)"---> Towards the Purple"
  if (color_val_out(2)>color_val_out(1).and.color_val_out(3)>color_val_out(1)) WRITE(*,*)"---> Towards the Cyan"
  if (color_val_out(1)>color_val_out(3).and.color_val_out(2)>color_val_out(3)) WRITE(*,*)"---> Towards the Yellow" 
  end subroutine
