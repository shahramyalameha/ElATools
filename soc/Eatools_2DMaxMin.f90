! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, 
!                               Please report bugs or suggestions to:  yalameha93@gmail.com                                 
!                                                                                                                          
!-------------------------------------------------------------------------------------------------------------------------
! SUBROUTINE: fOR 2D MATERIAL , CALCULATE pro_2d_sys.dat (pro=young, shear and poisson)

  SUBROUTINE MAX_Min_val (datinput,Maximum, Minimum,npoit, pl)
  implicit none
  DOUBLE PRECISION :: Maximum,Minimum,phi,Input,phi_max,phi_min,Input1,Input2,Input3 
  INTEGER          :: i,pl,npoit,datinput ! 1=poisson; 2=young 3=shear| pl =1 for plot ; pl=2 for data max_min
  DOUBLE PRECISION :: angle(1000) ! This array might be unused for datinput=1 after the fix
  integer:: max_pos_idx, max_neg_idx ! These variables might be unused for datinput=1 after the fix

  IF(pl==1) THEN   
    IF(datinput==2)then
      open(45,file="young_2d_sys.dat")    
      DO i=1,npoit
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
      ! Corrected call to find_max_poisson_with_angles
      CALL find_max_poisson_with_angles(Maximum, Minimum, phi_max, phi_min)
      ! The following lines are no longer needed as phi_max and phi_min are returned by the subroutine
      ! phi_max=angle(max_pos_idx) 
      ! phi_min=angle(max_neg_idx)
      !  open(45,file="poisson_2d_sys.dat")    
      ! DO i=1,npoit
      !   read(45,*)phi,Input1,Input2,Input3               
      !   If (i == 1) Then
      !     Maximum = Input1
      !     Minimum = Input3
      !   END IF
      !   If (Input1 > Maximum) Then
      !     Maximum = Input1
      !     phi_max=phi
      !   END IF
      !   If (Input3 < Minimum) Then
      !     Minimum = Input3
      !     phi_min=phi
      !   END IF
      ! END DO
      ! CLOSE(45)
        
    END IF
    If(datinput==3)then
      open(45,file="shear_2d_sys.dat")    
      DO i=1,npoit
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
    If(datinput==4)then
      open(45,file="transverse_2d_sys.dat")    
      DO i=1,npoit
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
    
    If(datinput==5)then
      open(45,file="longitudinal_2d_sys.dat")    
      DO i=1,npoit
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
      DO i=1,npoit
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
        
      WRITE(*,"(A,F10.4,A,F10.4,A)")    '  Maximum value of Youngs Modulus     = ', Maximum, "        Phi=",     phi_max," degree"
      WRITE(*,"(A,F10.4,A,F10.4,A)")    '  MINimum value of Youngs Modulus     = ', Minimum, "        Phi=",     phi_min," degree"
      WRITE(*,"(A,F10.4                )")  '  Anisotropic ratio of Youngs Modulus = ',   abs(Maximum)/abs(Minimum)
      WRITE(99,"(A,F10.4,A,F10.4,A)")   '  Maximum value of Youngs Modulus     = ', Maximum, "        Phi=",     phi_max," degree"
      WRITE(99,"(A,F10.4,A,F10.4,A)")   '  MINimum value of Youngs Modulus     = ', Minimum, "        Phi=",     phi_min," degree"
      WRITE(99,"(A,F10.4                )")  '  Anisotropic ratio of Youngs Modulus = ',   abs(Maximum)/abs(Minimum)
    endif
    If(datinput==1)then 
    CALL find_max_poisson_with_angles(Maximum, Minimum,phi_max, phi_min )
    
      
      ! OPEN(45,file="poisson_2d_sys.dat")    
      !  DO i=1,npoit
      !   READ(45,*)phi,Input1,Input2,Input3 
      !   If (i == 1) Then
      !     Maximum = Input1
      !     Minimum = Input3
      !   END IF
      !   If (Input1 > Maximum) Then
      !     Maximum = Input1
      !     phi_max=phi
      !   END IF

      !   If (Input3 < Minimum) Then
      !     Minimum = Input3
      !     phi_min=phi
      !   END IF
      ! END DO
      ! CLOSE(45)
        
      WRITE(*,"(A,F10.4,A,F10.4,A)")  '  Maximum value of Poissons Ratio     = ', Maximum,  "        Phi=",     phi_max," degree"
      WRITE(*,"(A,F10.4,A,F10.4,A)")  '  Minimum value of Poissons Ratio     = ', Minimum,  "        Phi=",     phi_min," degree"
      WRITE(*,"(A,F10.4                )")  '  Anisotropic ratio of Poissons Ratio  = ',  abs(Maximum)/abs(Minimum)
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  Maximum value of Poissons Ratio     = ', Maximum,  "        Phi=",     phi_max," degree"
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  Minimum value of Poissons Ratio     = ', Minimum,  "        Phi=",     phi_min," degree"    
      WRITE(99,"(A,F10.4                )") '  Anisotropic ratio of Poissons Ratio  = ',  abs(Maximum)/abs(Minimum)
    END IF        
    
    If(datinput==3)then
      Input1=0.0
      OPEN(45,file="shear_2d_sys.dat")    
      DO i=1,npoit
        READ(45,*)phi,Input1
        If (i == 1) Then
          Maximum = Input1
          Minimum = Input1
        END IF
        If (Input1 > Maximum) Then
          Maximum = Input1
          phi_max=phi
        END IF
        If (Input1 < Minimum) Then
          Minimum = Input1
          phi_min=phi
        END IF
      END DO
      CLOSE(45)
        
      WRITE(*,"(A,F10.4,A,F10.4,A)")  '  Maximum value of Shear Modulus      = ', Maximum,  "        Phi=",     phi_max," degree"
      WRITE(*,"(A,F10.4,A,F10.4,A)")  '  Minimum value of Shear Modulus      = ', Minimum,  "        Phi=",     phi_min," degree"
      WRITE(*,"(A,F10.4                )") '  Anisotropic ratio of Shear Modulus  = ',   abs(Maximum)/abs(Minimum)
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  Maximum value of Shear Modulus      = ', Maximum,  "        Phi=",     phi_max," degree"
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  Minimum value of Shear Modulus      = ', Minimum,  "        Phi=",     phi_min," degree"    
      WRITE(99,"(A,F10.4                )") '  Anisotropic ratio of Shear Modulus  = ',   abs(Maximum)/abs(Minimum)     
    END IF        

    If(datinput==4)then
      Input1=0.0
      OPEN(45,file="transverse_2d_sys.dat")    
      DO i=1,npoit
        READ(45,*)phi,Input1
        If (i == 1) Then
          Maximum = Input1
          Minimum = Input1
        END IF
        If (Input1 > Maximum) Then
          Maximum = Input1
          phi_max=phi
        END IF
        If (Input1 < Minimum) Then
          Minimum = Input1
          phi_min=phi
        END IF
      END DO
      CLOSE(45)
        
      WRITE(*,"(A,F10.4,A,F10.4,A)")    '  Maximum value of Transverse Wave      = ', Maximum,  "        Phi=",     phi_max," degree"
      WRITE(*,"(A,F10.4,A,F10.4,A)")    '  Minimum value of Transverse Wave      = ', Minimum,  "        Phi=",     phi_min," degree"
      WRITE(*,"(A,F10.4                )")  '  Anisotropic ratio of Transverse Wave  = ',   abs(Maximum)/abs(Minimum)
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  Maximum value of Transverse Wave      = ', Maximum,  "        Phi=",     phi_max," degree"
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  Minimum value of Transverse Wave      = ', Minimum,  "        Phi=",     phi_min," degree"    
      WRITE(99,"(A,F10.4                )") '  Anisotropic ratio of Transverse Wave  = ',   abs(Maximum)/abs(Minimum)     
    END IF
    
    If(datinput==5)then
      Input1=0.0
      OPEN(45,file="longitudinal_2d_sys.dat")    
      DO i=1,npoit
        READ(45,*)phi,Input1
        If (i == 1) Then
          Maximum = Input1
          Minimum = Input1
        END IF
        If (Input1 > Maximum) Then
          Maximum = Input1
          phi_max=phi
        END IF
        If (Input1 < Minimum) Then
          Minimum = Input1
          phi_min=phi
        END IF
      END DO
      CLOSE(45)
        
      WRITE(*,"(A,F10.4,A,F10.4,A)")    '  Maximum value of Longitudinal Wave      = ', Maximum,  "        Phi=",     phi_max," degree"
      WRITE(*,"(A,F10.4,A,F10.4,A)")    '  Minimum value of Longitudinal Wave      = ', Minimum,  "        Phi=",     phi_min," degree"
      WRITE(*,"(A,F10.4                )")  '  Anisotropic ratio of Longitudinal Wave  = ',   abs(Maximum)/abs(Minimum)
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  Maximum value of Longitudinal Wave      = ', Maximum,  "        Phi=",     phi_max," degree"
      WRITE(99,"(A,F10.4,A,F10.4,A)")  '  Minimum value of Longitudinal Wave      = ', Minimum,  "        Phi=",     phi_min," degree"    
      WRITE(99,"(A,F10.4                )") '  Anisotropic ratio of Longitudinal Wave  = ',   abs(Maximum)/abs(Minimum)     
    END IF
  END IF         

    
  END SUBROUTINE MAX_Min_val
!==============================     
SUBROUTINE find_max_poisson_with_angles(overall_max,overall_min,max_angle,min_angle)
    implicit none
    
    ! Declare variables
    integer :: i, j, num_lines
    DOUBLE PRECISION, allocatable :: angle_data(:), poisson_rates(:,:) ! Renamed angle to angle_data to avoid conflict if used in a module
    DOUBLE PRECISION :: overall_max, overall_min
    DOUBLE PRECISION :: max_angle, min_angle
    logical :: all_zero_last_column
    character(len=100) :: filename = 'poisson_2d_sys.dat'
    integer :: io_status, max_col_idx, min_col_idx ! Renamed to avoid conflict with potential intrinsic functions
    
    ! First, count the number of lines in the file
    open(unit=10, file=trim(filename), status='old', action='read', iostat=io_status)
    if (io_status /= 0) then
        print *, 'Error opening file: ', trim(filename)
        stop
    end if
    
    num_lines = 0
    do
        read(10, *, iostat=io_status)
        if (io_status /= 0) exit ! Exit on EOF or error
        num_lines = num_lines + 1
    end do
    rewind(10)
    
    ! Allocate arrays based on number of lines
    if (num_lines > 0) then
        allocate(angle_data(num_lines))
        allocate(poisson_rates(num_lines, 3))
    else
        print *, 'File is empty or could not be read properly: ', trim(filename)
        close(10)
        ! Set default values or handle error appropriately
        overall_max = 0.0
        overall_min = 0.0
        max_angle = 0.0
        min_angle = 0.0
        return 
    end if
    
    ! Read the data
    do i = 1, num_lines
        read(10, *, iostat=io_status) angle_data(i), poisson_rates(i, 1), &
                                    poisson_rates(i, 2), poisson_rates(i, 3)
        if (io_status /= 0) then
            print *, 'Error reading line ', i, ' from file: ', trim(filename)
            close(10)
            deallocate(angle_data, poisson_rates)
            stop
        end if
    end do
    close(10)
    
    ! Check if all values in the last column are zero
    all_zero_last_column = .true.
    do i = 1, num_lines
        if (poisson_rates(i, 3) /= 0.0) then
            all_zero_last_column = .false.
            exit
        end if
    end do
    
    ! Find overall maximum and minimum
    if (all_zero_last_column) then
        ! Use the second column (index 2) if last column (index 3) is all zeros. 
        ! The problem description implies poisson_rates(i,2) for this case.
        overall_max = poisson_rates(1, 2)
        overall_min = poisson_rates(1, 2)
        max_col_idx = 2 
        min_col_idx = 2 
        max_angle = angle_data(1)
        min_angle = angle_data(1)
        
        do i = 1, num_lines
            ! Check for new maximum
            if (poisson_rates(i, 2) > overall_max) then
                overall_max = poisson_rates(i, 2)
                max_col_idx = 2
                max_angle = angle_data(i)
            end if
            
            ! Check for new minimum
            if (poisson_rates(i, 2) < overall_min) then
                overall_min = poisson_rates(i, 2)
                min_col_idx = 2
                min_angle = angle_data(i)
            end if
        end do
    else
        ! Normal case: search across all three poisson_rates columns (indices 1, 2, 3)
        overall_max = poisson_rates(1, 1)
        overall_min = poisson_rates(1, 1)
        max_col_idx = 1
        min_col_idx = 1
        max_angle = angle_data(1)
        min_angle = angle_data(1)
        
        do j = 1, 3  ! Loop through columns
            do i = 1, num_lines
                ! Check for new maximum
                if (poisson_rates(i, j) > overall_max) then
                    overall_max = poisson_rates(i, j)
                    max_col_idx = j
                    max_angle = angle_data(i)
                end if
                
                ! Check for new minimum
                if (poisson_rates(i, j) < overall_min) then
                    overall_min = poisson_rates(i, j)
                    min_col_idx = j
                    min_angle = angle_data(i)
                end if
            end do
        end do
    end if
    
    ! Print results (optional, can be commented out)
    ! print *, 'Poisson Rate Analysis Inside Subroutine:'
    ! if (all_zero_last_column) then
    !     print *, 'Note: Last data column (index 3) contains only zero values.'
    !     print *, 'Analysis performed on the second data column (index 2).'
    ! end if
    
    ! print '(A, F10.4, A, F10.4, A, I1)', 'Maximum: ', overall_max, &
    !       ' at angle ', max_angle, ' (From Column ', max_col_idx, ' of data)'
    ! print '(A, F10.4, A, F10.4, A, I1)', 'Minimum: ', overall_min, &
    !       ' at angle ', min_angle, ' (From Column ', min_col_idx, ' of data)'
    
    ! Clean up
    deallocate(angle_data, poisson_rates)
end SUBROUTINE find_max_poisson_with_angles

