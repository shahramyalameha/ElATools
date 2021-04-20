!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 2D MATERIAL , CALCULATE pro_2d_sys.dat (pro=young, shear and poisson)

  SUBROUTINE MAX_Min_val (datinput,Maximum, Minimum,pl)
 implicit none
  DOUBLE PRECISION :: Maximum,Minimum,phi,Input,phi_max,phi_min,Input1,Input2,Input3 
  INTEGER          :: i,pl,datinput ! 1=poisson; 2=young 3=shear| pl =1 for plot ; pl=2 for data max_min
    
  
 IF(pl==1) THEN    
   IF(datinput==2)then
      open(45,file="young_2d_sys.dat")   
      DO i=1,201
         read(45,*)phi,Input                              
         If (i == 1) Then
            Maximum = Input
            Minimum = Input
         END IF
         If (Input > Maximum) Then
            Maximum = Input
            phi_max=phi
         END IF
         If (Input < Minimum) Then
            Minimum = Input
            phi_min=phi
         END IF 
      END DO 
      close(45)
       
   END IF
   If(datinput==1)then
      open(45,file="poisson_2d_sys.dat")   
      DO i=1,201
         read(45,*)phi,Input1,Input2,Input3                         
         If (i == 1) Then
            Maximum = Input1
            Minimum = Input3
         END IF
         If (Input1 > Maximum) Then
            Maximum = Input1
            phi_max=phi
         END IF
         If (Input3 < Minimum) Then
            Minimum = Input3
            phi_min=phi
         END IF
      END DO
      CLOSE(45)
       
   END IF
   If(datinput==3)then
      open(45,file="shear_2d_sys.dat")   
      DO i=1,201
         read(45,*)phi,Input1,Input2,Input3                         
         If (i == 1) Then
            Maximum = Input1
            Minimum = Input3
         END IF
         If (Input1 > Maximum) Then
            Maximum = Input1
            phi_max=phi
         END IF
         If (Input3 < Minimum) Then
            Minimum = Input3
            phi_min=phi
         END IF
      END DO
      CLOSE(45)
      
   END IF

END IF
 !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

IF(pl==2) THEN
   IF(datinput==2)then
      OPEN(45,FILE="young_2d_sys.dat")   
      DO i=1,201
         READ(45,*)phi,Input                              
         IF (i == 1) Then
            Maximum = Input
            Minimum = Input
         END IF
         IF (Input > Maximum) Then
            Maximum = Input
            phi_max=phi
         END IF
         IF (Input < Minimum) Then
            Minimum = Input
            phi_min=phi
         END IF

      END DO
      CLOSE(45)
       
      WRITE(*,"(A,F10.4,A,F10.4,A)")   '  Maximum value of Youngs Modulus = ', Maximum, "       Phi=",     phi_max," degree"
      WRITE(*,"(A,F10.4,A,F10.4,A)")   '  MINimum value of Youngs Modulus = ', Minimum, "       Phi=",     phi_min," degree"
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  Maximum value of Youngs Modulus = ', Maximum, "       Phi=",     phi_max," degree"
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  MINimum value of Youngs Modulus = ', Minimum, "       Phi=",     phi_min," degree"
     
   END IF

   If(datinput==1)then
      OPEN(45,file="poisson_2d_sys.dat")   
      DO i=1,201
         READ(45,*)phi,Input1,Input2,Input3 
         If (i == 1) Then
            Maximum = Input1
            Minimum = Input3
         END IF
         If (Input1 > Maximum) Then
            Maximum = Input1
            phi_max=phi
         END IF

         If (Input3 < Minimum) Then
            Minimum = Input3
            phi_min=phi
         END IF
      END DO
      CLOSE(45)
      
      WRITE(*,"(A,F10.4,A,F10.4,A)")  '  Maximum value of Poissons Ratio = ', Maximum,  "       Phi=",     phi_max," degree"
      WRITE(*,"(A,F10.4,A,F10.4,A)")  '  Minimum value of Poissons Ratio = ', Minimum,  "       Phi=",     phi_min," degree"
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  Maximum value of Poissons Ratio = ', Maximum,  "       Phi=",     phi_max," degree"
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  Minimum value of Poissons Ratio = ', Minimum,  "       Phi=",     phi_min," degree"   
      
   END IF         
  
   If(datinput==3)then
      OPEN(45,file="shear_2d_sys.dat")   
      DO i=1,201
         READ(45,*)phi,Input1
         If (i == 1) Then
            Maximum = Input1
            Minimum = Input1
         END IF
         If (Input1 < Maximum) Then
            Maximum = Input1
            phi_max=phi
         END IF
         If (Input1 > Minimum) Then
            Minimum = Input1
            phi_min=phi
         END IF
      END DO
      CLOSE(45)
       
      WRITE(*,"(A,F10.4,A,F10.4,A)")  '  Maximum value of Shear Modulus = ', Maximum,  "       Phi=",     phi_max," degree"
      WRITE(*,"(A,F10.4,A,F10.4,A)")  '  Minimum value of Shear Modulus = ', Minimum,  "       Phi=",     phi_min," degree"
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  Maximum value of Shear Modulus = ', Maximum,  "       Phi=",     phi_max," degree"
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  Minimum value of Shear Modulus = ', Minimum,  "       Phi=",     phi_min," degree"   
      
   END IF         

END IF           

  
END  SUBROUTINE
    
    
