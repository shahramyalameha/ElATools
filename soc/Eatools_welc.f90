!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: Hi ElATools. Welcome (: .
!-----------------------------------------------------------------
SUBROUTINE WELCOME()
WRITE(*,'(a)')'============== 🎈 Welcome to 🎈 ==============='
WRITE(*,'(a)')'                                               ' 
WRITE(*,'(a)')"          Anisotropic Elasticity Tools         "
WRITE(*,'(a)')"          -           --         -             "
WRITE(*,'(a)')"   ______  _      _______             _        "
WRITE(*,'(a)')"  |  ____|| |    |__   __|           | |       "
WRITE(*,'(a)')"  | |__   | |  __ _ | |  ___    ___  | | ___   "
WRITE(*,'(a)')"  |  __|  | | / _` || | / _ \  / _ \ | |/ __|  "
WRITE(*,'(a)')"  E |____ |L|| (A| ||T|| (O) || (O) ||L|\_S \  "
WRITE(*,'(a)')"  |______||_| \__,_||_| \___/  \___/ |_||___/  "
WRITE(*,'(a)')"            45 6C 41 74 6F 6F 6C 73            "
WRITE(*,*)'          Author : Shahram Yalameha           '                   
WRITE(*,*)'         Email : yalameha93@gmail.com          ' 
WRITE(*,'(a)')'                   v1.7.3                      '
WRITE(*,'(a)')'                    From                       '
WRITE(*,'(a)')'            Elastic Tools Project               '
WRITE(*,'(a)')'=========        (c)2018-2022        =========='
WRITE(*,*)'                                                   '
       
!!!!
WRITE(99,'(a)')'================= Welcome to =================='
WRITE(99,'(a)')'                                               '
WRITE(99,'(a)')"          Anisotropic Elasticity Tools         "
WRITE(99,'(a)')"          -           --         -             "
WRITE(99,'(a)')"   ______  _      _______             _        "
WRITE(99,'(a)')"  |  ____|| |    |__   __|           | |       "
WRITE(99,'(a)')"  | |__   | |  __ _ | |  ___    ___  | | ___   "
WRITE(99,'(a)')"  |  __|  | | / _` || | / _ \  / _ \ | |/ __|  "
WRITE(99,'(a)')"  E |____ |L|| (A| ||T|| (O) || (O) ||L|\_S \  "
WRITE(99,'(a)')"  |______||_| \__,_||_| \___/  \___/ |_||___/  "
WRITE(99,'(a)')"            45 6C 41 74 6F 6F 6C 73            "
WRITE(99,*)'          Author : Shahram Yalameha                '                   
WRITE(99,*)'         Email : yalameha93@gmail.com              '
WRITE(99,'(a)')'                   v1.7.3                      '
WRITE(99,'(a)')'                    From                       '
WRITE(99,'(a)')'            Elastic Tools Project              '
WRITE(99,'(a)')'=========        (c)2018-2022        =========='
WRITE(99,*)'                                                   '
WRITE(99,*)'                                                   '

END



SUBROUTINE log_start_time()
  !> Turn off implicit typing
  IMPLICIT NONE

  !> Variable declarations
  INTEGER, DIMENSION(8) :: values

  !> Get the current date and time from the system clock
  CALL DATE_AND_TIME(VALUES=values)

  !> Write the formatted date and time to unit 99 (DATA.out)
  !> Using I2.2 format to ensure a minimum of 2 digits with leading zeros
  WRITE(99, '(A)')    '==================================================='
  WRITE(99, '(A, I4.4, A, I2.2, A, I2.2)') ' > Program started on: ', values(1), '-', values(2), '-', values(3)
  WRITE(99, '(A, I2.2, A, I2.2, A, I2.2)') ' > at:                 ', values(5), ':', values(6), ':', values(7)
  WRITE(99, '(A)')    '==================================================='
  WRITE(99, *) '' !> Write a blank line for better readability

END SUBROUTINE log_start_time
