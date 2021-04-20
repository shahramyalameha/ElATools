!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL , calculate Sij.

	      SUBROUTINE C_Inv_M(N)
	      IMPLICIT NONE
	      DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: U, Uinv
	      INTEGER, ALLOCATABLE, DIMENSION(:) :: IPIV,WORK
	      INTEGER :: i,j, LWORK
	      INTEGER :: INFO,LDA,M,N
	      INTEGER :: DeAllocateStatus
	      EXTERNAL DGETRF  ! by LAPACK
	      EXTERNAL DGETRI  ! by LAPACK
	        LDA = N
	      LWORK = N*N
	      ALLOCATE (U(LDA,N))
	      ALLOCATE (Uinv(LDA,N))
	      ALLOCATE (WORK(LWORK))
	      ALLOCATE (IPIV(N))
	      OPEN(88,FILE="Sij.dat")
		  OPEN(58,FILE="Cij.dat",STATUS='OLD',ACTION='READ')
		  DO i=1,N
			READ(58,*) (U(i,j),j=1,N)
	      ENDDO
		  close(58)
		  33    FORMAT(100(F12.8,1X))
		  14    FORMAT (6F8.2)
              !
	      M=N
	      LDA=N
		  Uinv = U
		  ! > Use LU-factorization to solve a SYSTEM of equations, given the LU-factorization
		  ! > of its coefficient matrix: Ax = b => (LU)x = b => L(Ux) = b >>>>> by DGETRF
              !
	      CALL DGETRF ( M, N, Uinv, LDA, IPIV, INFO )
              !
	      IF(INFO.EQ.0)THEN
			WRITE(*,*) ' '
			WRITE(*,*)" > LU decomposition successful "
			!WRITE(*,*) M, N, LDA, IPIV, INFO
	      ENDIF
              !
	      IF(INFO.LT.0)THEN
			WRITE(*,*) ' '
            WRITE(*,*)" > LU decomposition:  illegal value "
		    STOP
		ENDIF
		IF(INFO.GT.0)THEN
     	WRITE(*,35)INFO,INFO
		WRITE(*,*) ' '
		35       FORMAT( ' > LU decomposition: U(',I4,',',I4,') = 0 ')
	ENDIF
	!=========================================================
	CALL DGETRI(N, Uinv, N, IPIV, WORK, LWORK, INFO)
	IF (info.NE.0) THEN
		STOP ' > Matrix inversion failed!'
	ENDIF
	IF (info .EQ. 0) THEN
		WRITE(*,*) ' > Inverse Successful '
	ENDIF
	!=========================================================
	WRITE(*,*) '============================================='
	WRITE(*,*) ' > Inverse Elastic constant Matrix (Sij):'
	WRITE(*,*) ''
	DO i = 1, N
		! WRITE(30,33)(Ainv(i,j), j = 1, N)
		WRITE(*,33)(Uinv(i,j), j = 1, N)
		WRITE(88,33)(Uinv(i,j), j = 1, N)
	END DO
	CLOSE(88)
	DEALLOCATE (u,    STAT = DeAllocateStatus)
	DEALLOCATE (uinv, STAT = DeAllocateStatus)
	DEALLOCATE (IPIV, STAT = DeAllocateStatus)
	DEALLOCATE (WORK, STAT = DeAllocateStatus)
    WRITE(*,*) ' '
	WRITE(*,*) ' > Sij were produced!'
    !WRITE(*,*) '===============================================&
    !============================='
    CLOSE(88)
	END
