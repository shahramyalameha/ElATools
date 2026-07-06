!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 2D MATERIAL , CALCULATED 2D Stiffness Constant

SUBROUTINE stiffness_2D(vec, phi, phi_stiffness, l)!
   implicit none

   DOUBLE PRECISION                  :: vv11, vv22, vv33, Y1, Y2, A, B, T, phi, k11, k22, k12
   DOUBLE PRECISION, DIMENSION(2010) :: phi_stiffness, phi_stiffness_test
   DOUBLE PRECISION, DIMENSION(3, 3) :: C, S
   DOUBLE PRECISION, DIMENSION(2)    :: vec
   Integer                           :: n, i, j, l
   n = 3
   OPEN (58, FILE="Cij-2D.dat", STATUS='OLD', ACTION='READ')
   DO i = 1, n
      READ (58, *) (C(i, j), j=1, n)
   END DO
   close (58)
   n = 3
   OPEN (51, FILE="Sij-2D.dat", STATUS='OLD', ACTION='READ')
   DO i = 1, n
      READ (51, *) (S(i, j), j=1, n)
   END DO
   close (51)

   phi_stiffness(l) = (C(1, 1) + C(2, 2) - 2.d0*C(1, 2) + &
                      (C(1, 1) - C(1, 2) - C(2, 2))*vec(2))/2.d0

   !Write (*, *) phi, phi_stiffness(l)

end SUBROUTINE
