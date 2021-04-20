!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 2D MATERIAL , Computing inverse2d matrix; Method: Based on the DOolittle LU method

SUBROUTINE C_Inv_M2D(n)
implicit none
integer :: n
DOuble precision a(n,n), c(n,n)
integer i,j

 !OPEN(88,FILE="Sij.dat")
	  OPEN(58,FILE="Cij-2D.dat",STATUS='OLD',ACTION='READ')
    DO i=1,n
		     READ(58,*) (a(i,j),j=1,n)
    ENDDO
    close(58)
! matrix A
  write(*,*)"########################################"
! print a header and the original matrix
  write (*,200)
  DO i=1,n
     write (*,201) (a(i,j),j=1,n)
  END DO
  call inverse2d(a,c,n)

! print the inverse2d matrix C = A^{-1} 
  open (83, FILE="Sij-2D.dat")
  !!!!!!!!!!1
  DO i = 1,n
     write (83,201)  (c(i,j),j=1,n)
  END DO
 close(83)
   !!!!!!!!!!1
  write (*,202)
  DO i = 1,n
     write (*,201)  (c(i,j),j=1,n)
  END DO
 510 format(6F9.4)
  520 format(6F9.4)
200 format (/, &
            'Cij:')
201 format (6f12.6)
202 format (/,' Sij:')
  write(*,*)" "
  write(*,*)"########################################"
END subroutine

  subroutine inverse2d(a,c,n)
!============================================================
! inverse2d matrix
! Method: Based on DOolittle LU factorization for Ax=b
! Alex G. December 2009
!-----------------------------------------------------------
! input ...
! a(n,n) - array of coefficients for matrix A
! n      - dimension
! output ...
! c(n,n) - inverse2d matrix of A
! comments ...
! the original matrix a(n,n) will be destroyed 
! during the calculation
!===========================================================
implicit none 
integer n
DOuble precision a(n,n), c(n,n)
DOuble precision L(n,n), U(n,n), b(n), d(n), x(n)
DOuble precision coeff
integer i, j, k

! step 0: initialization for matrices L and U and b
! Fortran 90/95 aloows such operations on matrices
L=0.0
U=0.0
b=0.0

! step 1: forward elimination
DO k=1, n-1
   DO i=k+1,n
      coeff=a(i,k)/a(k,k)
      L(i,k) = coeff
      DO j=k+1,n
         a(i,j) = a(i,j)-coeff*a(k,j)
      END DO
   END DO
END DO

! Step 2: prepare L and U matrices 
! L matrix is a matrix of the elimination coefficient
! + the diagonal elements are 1.0
DO i=1,n
  L(i,i) = 1.0
END DO
! U matrix is the upper triangular part of A
DO j=1,n
  DO i=1,j
    U(i,j) = a(i,j)
  END DO
END DO

! Step 3: compute columns of the inverse2d matrix C
DO k=1,n
  b(k)=1.0
  d(1) = b(1)
! Step 3a: Solve Ld=b using the forward substitution
  DO i=2,n
    d(i)=b(i)
    DO j=1,i-1
      d(i) = d(i) - L(i,j)*d(j)
    END DO
  END DO
! Step 3b: Solve Ux=d using the back substitution
  x(n)=d(n)/U(n,n)
  DO i = n-1,1,-1
    x(i) = d(i)
    DO j=n,i+1,-1
      x(i)=x(i)-U(i,j)*x(j)
    END DO
    x(i) = x(i)/u(i,i)
  END DO
! Step 3c: fill the solutions x(n) into column k of C
  DO i=1,n
    c(i,k) = x(i)
  END DO
  b(k)=0.0
END DO
END subroutine inverse2d
