!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the phase and group vec. calculation.. v.0


         SUBROUTINE start_val(l,  v_f, v1, v2, v3, density, C)
         !                  in out out out out in    in  !
         INTEGER, PARAMETER                    :: qp = selected_real_kind(33, 4931)
         DOUBLE PRECISION, INTENT(in)                  :: density 
         DOUBLE PRECISION, DIMENSION (6,6)             :: C
         DOUBLE PRECISION, DIMENSION(3)                :: v_f, v1,v2,v3
         DOUBLE PRECISION, DIMENSION(3)                :: l
         DOUBLE PRECISION, PARAMETER                   :: PI_C=3.14159265358979323846264338327950D0
         DOUBLE PRECISION, DIMENSION (3,6)             :: D
         DOUBLE PRECISION, DIMENSION (6,3)             :: Dtras, multi
         DOUBLE PRECISION, DIMENSION (3,3)             :: A, Q, mvec
         DOUBLE PRECISION, DIMENSION (3)               :: W, vel
         INTEGER :: k, pos2
         INTEGER, DIMENSION(1) :: pos1, pos3
         D=0.0D0
        
         D(1,1)=l(1) ;  D(2,2)=l(2) ;  D(3,3)=l(3) ;
         D(1,5)=l(3) ;  D(2,4)=l(3) ;  D(1,6)=l(2) ;
         D(3,4)=l(2) ;  D(3,5)=l(1) ;  D(2,6)=l(1) ;
         Dtras=transpose(D)
         multi=matmul(C,Dtras)
         A=matmul(D,multi)
         call DSYEVJ3(A,Q,W)
         
         mvec(:,1)=Q(:,1)
         mvec(:,2)=Q(:,2)
         mvec(:,3)=Q(:,3)
         vel(:)=sqrt(W(:)/density)
       

         pos1 = maxloc(vel)
         pos3 = minloc(vel)
          v_f(1)=vel(pos1(1))
         v_f(3)=vel(pos3(1))
         do k=1,3
          if (k.ne.pos1(1).and.k.ne.pos3(1)) pos2=k
        end do
        v_f(2)=vel(pos2)
         
        v1(:)=mvec(:,pos1(1))
        v2(:)=mvec(:,pos2)
        v3(:)=mvec(:,pos3(1))
       
         
         
      END SUBROUTINE start_val
         
         
         
         
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !!                                                                     !!
         !!  Subroutine DSYEVJ3 taken from the reference:                       !!
         !!                                                                     !!
         !!  Joachim Kopp                                                       !!
         !!  Efficient numerical diagonalization of hermitian 3x3 matrices      !!
         !1  International Journal of Modern Physics C                          !!
         !!  Vol. 19, No. 3 (2008) 523-548                                      !!
         !!  arXiv.org: physics/0610206                                         !!
         !!                                                                     !!
         !!  Retrieved from http://www.mpi-hd.mpg.de/personalhomes/globes/3x3/  !!
         !!  (Last time checked 16/9/2014)                                      !!
         !!                                                                     !!
         !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         !*
         !* ----------------------------------------------------------------------------
               SUBROUTINE DSYEVJ3(A, Q, W)
         !* ----------------------------------------------------------------------------
         !* Calculates the eigenvalues and normalized eigenvectors of a symmetric 3x3
         !* matrix A using the Jacobi algorithm.
         !* The upper triangular part of A is destroyed during the calculation,
         !* the diagonal elements are read but not destroyed, and the lower
         !* triangular elements are not referenced at all.
         !* ----------------------------------------------------------------------------
         !* Parameters:
         !*   A: The symmetric input matrix
         !*   Q: Storage buffer for eigenvectors
         !*   W: Storage buffer for eigenvalues
         !* ----------------------------------------------------------------------------
         !*     .. Arguments ..
         INTEGER, PARAMETER :: qp = selected_real_kind(33, 4931)
         !
               DOUBLE PRECISION A(3,3)
               DOUBLE PRECISION Q(3,3)
               DOUBLE PRECISION W(3)
         
         !*     .. Parameters ..
               INTEGER          N
               PARAMETER        ( N = 3 )
             
         !*     .. Local Variables ..
               DOUBLE PRECISION SD, SO
               DOUBLE PRECISION S, C, T
               DOUBLE PRECISION G, H, Z, THETA
               DOUBLE PRECISION THRESH
               INTEGER          I, X, Y, R
         
        
               DO 10 X = 1, N
                 Q(X,X) = 1.0D0
                 DO 11, Y = 1, X-1
                   Q(X, Y) = 0.0D0
                   Q(Y, X) = 0.0D0
            11   CONTINUE
            10 CONTINUE
         
         !*     Initialize W to diag(A)
               DO 20 X = 1, N
                 W(X) = A(X, X)
            20 CONTINUE
         
         !*     Calculate SQR(tr(A))  
               SD = 0.0D0
               DO 30 X = 1, N
                 SD = SD + ABS(W(X))
            30 CONTINUE
               SD = SD**2
          
         !*     Main iteration loop
               DO 40 I = 1, 1000
         !*       Test for convergence
                 SO = 0.0D0
                 DO 50 X = 1, N
                   DO 51 Y = X+1, N
                     SO = SO + ABS(A(X, Y))
            51     CONTINUE
            50   CONTINUE
                 IF (SO .EQ. 0.0D0) THEN
                   RETURN
                 END IF
         
                 IF (I .LT. 4) THEN
                   THRESH = 0.2D0 * SO / N**2
                 ELSE
                   THRESH = 0.0D0
                 END IF
         
         !*       Do sweep
                 DO 60 X = 1, N
                   DO 61 Y = X+1, N
                     G = 100.0D0 * ( ABS(A(X, Y)) )
                     IF ( I .GT. 4 .AND. ABS(W(X)) + G .EQ. ABS(W(X))   &
                                   .AND. ABS(W(Y)) + G .EQ. ABS(W(Y)) ) THEN
                       A(X, Y) = 0.0D0
                     ELSE IF (ABS(A(X, Y)) .GT. THRESH) THEN
         !*             Calculate Jacobi transformation
                       H = W(Y) - W(X)
                       IF ( ABS(H) + G .EQ. ABS(H) ) THEN
                         T = A(X, Y) / H
                       ELSE
                         THETA = 0.5D0 * H / A(X, Y)
                         IF (THETA .LT. 0.0D0) THEN
                           T = -1.0D0 / (SQRT(1.0D0 + THETA**2) - THETA)
                         ELSE
                           T = 1.0D0 / (SQRT(1.0D0 + THETA**2) + THETA)
                         END IF
                       END IF
         
                       C = 1.0D0 / SQRT( 1.0D0 + T**2 )
                       S = T * C
                       Z = T * A(X, Y)
                       
         !*             Apply Jacobi transformation
                       A(X, Y) = 0.0D0
                       W(X)    = W(X) - Z
                       W(Y)    = W(Y) + Z
                       DO 70 R = 1, X-1
                         T       = A(R, X)
                         A(R, X) = C * T - S * A(R, Y)
                         A(R, Y) = S * T + C * A(R, Y)
            70         CONTINUE
                       DO 80, R = X+1, Y-1
                         T       = A(X, R)
                         A(X, R) = C * T - S * A(R, Y)
                         A(R, Y) = S * T + C * A(R, Y)
            80         CONTINUE
                       DO 90, R = Y+1, N
                         T       = A(X, R)
                         A(X, R) = C * T - S * A(Y, R)
                         A(Y, R) = S * T + C * A(Y, R)
            90         CONTINUE
         
         !*             Update eigenvectors
         !*             --- This loop can be omitted if only the eigenvalues are desired ---
                       DO 100, R = 1, N
                         T       = Q(R, X)
                         Q(R, X) = C * T - S * Q(R, Y)
                         Q(R, Y) = S * T + C * Q(R, Y)
           100         CONTINUE
                     END IF
            61     CONTINUE
            60   CONTINUE
            40 CONTINUE
         
         WRITE (*,*) 'DSYEVJ3: No convergence.'
                     
               END SUBROUTINE
         !* End of subroutine DSYEVJ3
         !
               SUBROUTINE callCij(CCo)
                  implicit NONE
               DOUBLE PRECISION, DIMENSION(6,6) :: CCo 
               CLOSE(UNIT=10)
               OPEN(40,file="Cij.dat")
               READ(40,*) CCo(1,1),CCo(1,2),CCo(1,3),CCo(1,4),CCo(1,5),CCo(1,6)
               READ(40,*) CCo(2,1),CCo(2,2),CCo(2,3),CCo(2,4),CCo(2,5),CCo(2,6)
               READ(40,*) CCo(3,1),CCo(3,2),CCo(3,3),CCo(3,4),CCo(3,5),CCo(3,6)
               READ(40,*) CCo(4,1),CCo(4,2),CCo(4,3),CCo(4,4),CCo(4,5),CCo(4,6)
               READ(40,*) CCo(5,1),CCo(5,2),CCo(5,3),CCo(5,4),CCo(5,5),CCo(5,6)
               READ(40,*) CCo(6,1),CCo(6,2),CCo(6,3),CCo(6,4),CCo(6,5),CCo(6,6)
           
               CLOSE(UNIT=40)
               END SUBROUTINE
         !
