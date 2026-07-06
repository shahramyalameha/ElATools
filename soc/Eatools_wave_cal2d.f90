!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2024 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 2D MATERIAL , CALCULATE group_velocity_2D 

SUBROUTINE christoffel_solve_2D(rho,vec, jcont, tranlong, mod_pre)
   implicit none
   DOUBLE PRECISION, dimension(3, 3)       :: c
   DOUBLE PRECISION, dimension(2)          :: n
   DOUBLE PRECISION, dimension(2, 2)       :: Gamma
   DOUBLE PRECISION, dimension(3, 3, 3, 3) :: Cjikl
   INTEGER                                 ::  jcont, j, j2, l, k, num, i
   CHARACTER(4)                            :: mod_pre
   DOUBLE PRECISION, PARAMETER             :: pi = 3.141592653589793238462D0
   DOUBLE PRECISION                        :: rho
   DOUBLE PRECISION, DIMENSION(3)          :: vec  
   DOUBLE PRECISION                        :: tranlong
   DOUBLE PRECISION eigenvectors(2, 2), eigenvalues(2), work(8)
   INTEGER                                 :: info
   external :: dsyev
!==============================================================================
   OPEN (58, FILE="Cij-2D.dat")
   DO i = 1, 3
      READ (58, *) (c(i, j), j=1, 3)
      !Write(*,"(6f12.6)") (c(i,j),j=1,3)
   END DO
   close (58)
!-----------------------------------------
   DO i = 1, 3
      DO j = 1, 3
         Cjikl(i, j, i, j) = c(i, j)
         IF (i /= j) THEN
            Cjikl(i, i, j, j) = c(i, j)
            Cjikl(j, j, i, i) = c(i, j)
            Cjikl(i, j, j, i) = c(i, j)
            Cjikl(j, i, i, j) = c(i, j)
            !write(*,"(F8.4, 4I2)")Cjikl(i, j, i, j),i, j, i, j
         END if
      END DO
   END DO
!-----------------------------------------
!write(*,*)vec
   Gamma = 0.0d0

   do i = 1, 2
      do j = 1, 2
         do k = 1, 2
            do l = 1, 2
               Gamma(i, j) = Gamma(i, j) + Cjikl(i, k, j, l)*vec(k)*vec(l)
               ! Write(*,*)Gamma(i, j),vec(l)
            end do
         end do
      end do
   end do
   call dsyev('V', 'U', 2, Gamma, 2, eigenvalues, work, 8, info)

    !write (*, *)  eigenvalues(1) ,  eigenvalues(2) 
   IF(trim(mod_pre) == "tran") tranlong = sqrt(abs(eigenvalues(1))/rho)/1000 !km/s
   IF(trim(mod_pre) == "long") tranlong = sqrt(abs(eigenvalues(2))/rho)/1000 !km/s
   Gamma = 0.0d0

!-----------------------------------------
END SUBROUTINE christoffel_solve_2D
