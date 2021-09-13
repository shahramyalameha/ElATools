!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, Elastic Stability Conditions 3D and 2D materials.

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%%%%%                Elastic Stability Conditions (3D)        %%%% 
!%%%%%    The generic necessary and sufficient criterion that  %%%%
!%%%%%    all eigenvalues of C be position                     %%%%
!%%%%%                        REFERENCE                        %%%%
!%%%%%     Mouhat F, Coudert F X. Necessary and sufficient   %%%%
!%%%%%       elastic stability conditions in various crystal   %%%%
!%%%%%       systems[J]. Physical Review B, 2014, 90(22).      %%%%
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 subroutine stability3d( Stable, Celas,Eig3d)
        integer i, j, Stable
        dimension Celas(6,6)
        dimension Eig3d(6,6), Vector(6,6)
        double precision EPS
        double precision Celas, Eig3d, Vector
        
        do i=1,6
           do j=1,6
              Eig3d(i,j)=Celas(i,j)
           enddo
        enddo

        EPS=0.01 
        call cjcbj(Eig3d,6,EPS,Vector)

!C       Stable=0: Stable
!C       Stable=1: UNstable        

        Stable=0
        do i=1,6
           if (Eig3d(i,i) .LE. 0) then
!C                open(unit=49,position='Append',FILE='EDATA')
!C                   
!C                Write(49,*)    
!C                Write(49,*) "WARNING!!! The elastic UNstability"
!C                Write(49,*) 
!C                Write(49,*) "   All eigenvalues of C is:"
!C                Write(49,*)
!C
!                 do j=1,6
!                    Write(*,'(6f10.2)') Eig(j,j)
!                enddo
!       
!                close(49)
     WRITE(*,'(A,36f10.2)') "Eigenvalues (GPa):",Eig3d(1,1),Eig3d(2,2),Eig3d(3,3) ,Eig3d(4,4) ,Eig3d(5,5) ,Eig3d(6,6) 
     
     WRITE(*,'(A)')">> No further analysis will be performed."

 
                 Stable=1

           endif
        enddo
     !WRITE(99,'(A,36f10.2)') "Eigenvalues (GPa):",Eig3d(1,1),Eig3d(2,2),Eig3d(3,3) ,Eig3d(4,4) ,Eig3d(5,5) ,Eig3d(6,6) 
 end subroutine                                                                       

!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C%%%%%                Elastic Stability Conditions (2D)        %%%% 
!C%%%%%    The generic necessary and sufficient criterion that  %%%%
!C%%%%%    all eigenvalues of C be position                     %%%%
!C%%%%%                        REFERENCE                        %%%%
!C%%%%%     Mouhat F, Coudert F X. Necessary and sufficient     %%%%
!C%%%%%       elastic stability conditions in various crystal   %%%%
!C%%%%%       systems[J]. Physical Review B, 2014, 90(22).      %%%%
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 subroutine stability2d( Stable, Celas ,Eig2d)
        integer i, j, Stable
        dimension Celas(3,3)
        dimension Eig2d(3,3), Vector(3,3)
        double precision EPS
        double precision Celas, Eig2d, Vector
        do i=1,3
           do j=1,3
              Eig2d(i,j)=Celas(i,j)
           enddo
        enddo

        EPS=0.01 
        call cjcbj(Eig2d,3,EPS,Vector)

!C       Stable=0: Stable
!C       Stable=1: UNstable
          
        Stable=0
        do i=1,3
           if (Eig2d(i,i) .LE. 0) then
!C                open(unit=49,position='Append',FILE='EDATA')
!C
!C                Write(49,*)    
!C                Write(49,*) "WARNING!!! The elastic UNstability"
!C                Write(49,*) 
!C                Write(49,*) "   All eigenvalues of C is:"
!C                Write(49,*)
!C
              ! do j=1,3
                !    Write(*,'(6f10.2)') Eig(j,j)
               !  enddo
                call system("clear")
     WRITE(*,'(A,36f10.2)')"Eigenvalues (N/m):", Eig2d(1,1),Eig2d(2,2),Eig2d(3,3) 
     WRITE(*,'(A)')">> No further analysis will be performed."
!C                close(49)

                Stable=1   

           endif
        enddo

 end subroutine       
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!C%%%%%   subprogram "cjcbj" is used to solve the matrix        %%%% 
!C%%%%%   eigenvalues according the Jacobi iterate method       %%%%
!C%%%%                           REFERENCE                      %%%%
!C%%%%    1\ FORTRAN changyongsuanfachengxuji, XuShilang        %%%%      
!C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        SUBROUTINE cjcbj(A,N,EPS,V)
        DIMENSION A(N,N),V(N,N)
        DOUBLE PRECISION A,V,FF,FM,CN,SN,OMEGA,X,Y
        INTEGER P,Q
        DO 2020 I=1,N
          V(I,I)=1.0
          DO 2010 J=1,N
            IF (I.NE.J) V(I,J)=0.0
2010        CONTINUE
2020      CONTINUE
        FF=0.0
        DO 2500 I=2,N
        DO 2500 J=1,I-1
2500     FF=FF+A(I,J)*A(I,J)
        FF=SQRT(2.0*FF)
2205     FF=FF/(1.0*N)
2025      DO 2030 I=2,N
        DO 2030 J=1,I-1
          IF (ABS(A(I,J)).GE.FF) THEN
            P=I
            Q=J
            GOTO 2600
          END IF
2030      CONTINUE
        IF (FF.GE.EPS) GOTO 2205
        RETURN
2600     X=-A(P,Q)
        Y=(A(Q,Q)-A(P,P))/2.0
        OMEGA=X/SQRT(X*X+Y*Y)
        IF (Y.LT.0.0) OMEGA=-OMEGA
        SN=1.0+SQRT(1.0-OMEGA*OMEGA)
        SN=OMEGA/SQRT(2.0*SN)
        CN=SQRT(1.0-SN*SN)
        FM=A(P,P)
        A(P,P)=FM*CN*CN+A(Q,Q)*SN*SN+A(P,Q)*OMEGA
        A(Q,Q)=FM*SN*SN+A(Q,Q)*CN*CN-A(P,Q)*OMEGA
        A(P,Q)=0.0
        A(Q,P)=0.0
        DO 2060 J=1,N
          IF ((J.NE.P).AND.(J.NE.Q)) THEN
            FM=A(P,J)
            A(P,J)=FM*CN+A(Q,J)*SN
            A(Q,J)=-FM*SN+A(Q,J)*CN
          END IF
2060      CONTINUE

        DO 2070 I=1,N
          IF ((I.NE.P).AND.(I.NE.Q)) THEN
            FM=A(I,P)
            A(I,P)=FM*CN+A(I,Q)*SN
            A(I,Q)=-FM*SN+A(I,Q)*CN
          END IF
2070      CONTINUE
        DO 2080 I=1,N
          FM=V(I,P)
          V(I,P)=FM*CN+V(I,Q)*SN
          V(I,Q)=-FM*SN+V(I,Q)*CN
2080      CONTINUE
        GOTO 2025
        END
