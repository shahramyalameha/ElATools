!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 2D MATERIAL , CALCULATED shear modulus.

!REF: Jasiukiewicz, Cz, T. Paszkiewicz, and S. Wolski. "Auxetic properties and anisotropy of elastic material constants of 2D crystalline media." 
!physica status solidi (b) 245.3 (2008): 562-569.

 SUBROUTINE adv_2D(phi,phi_pro,l,pro,method,Max_pro, Min_pro)
    implicit none
    CHARACTER(len=5)                    :: pro 
    CHARACTER(len=3)                    :: method  ! 1 = o 2 = r 3 = q
    DOUBLE PRECISION                    :: Max_pro, Min_pro, phi, val_pro, sai_G, r_G, o_G, G0, G,G_inver, E_inver, poi_inver
    DOUBLE PRECISION, DIMENSION(201)    :: phi_pro,pro_max_phi
    DOUBLE PRECISION, DIMENSION(3,3)    :: C,S
    Integer                             :: n,i,j,l
     n=3
       
     OPEN(58,FILE="Cij-2D.dat",STATUS='OLD',ACTION='READ')
     DO i=1,n
       READ(58,*) (C(i,j),j=1,n)
     ENDDO
     close(58)
     n=3
     OPEN(51,FILE="Sij-2D.dat",STATUS='OLD',ACTION='READ')
     DO i=1,n
       READ(51,*) (S(i,j),j=1,n)
     ENDDO
     close(51)

     IF(method == 'adv' .and. pro == "shear" ) THEN
      G_inver     =  S(1,1) *      ( COS(phi)*COS(phi)*SIN(phi)*SIN(phi) ) +&
	                    S(1,2) *-2.D0*( COS(phi)*SIN(phi)*SIN(phi)*COS(phi) ) +&
	                    S(2,2) *      ( SIN(phi)*SIN(phi)*COS(phi)*COS(phi) ) +&
	                    S(1,3) *      ( SIN(phi)*SIN(phi)*COS(phi)*SIN(phi)   -      SIN(phi)*COS(phi)*COS(phi)*COS(phi) )+&
	                    S(2,3) *      ( COS(phi)*COS(phi)*COS(phi)*SIN(phi)   -      SIN(phi)*COS(phi)*SIN(phi)*SIN(phi) )+&
	                    S(3,3) *      ( COS(phi)*COS(phi)*COS(phi)*COS(phi)   - 2.D0*COS(phi)*SIN(phi)*SIN(phi)*COS(phi)  + SIN(phi)*SIN(phi)*SIN(phi)*SIN(phi) ) * (1.d0/4.d0)
      phi_pro(l)  = 1.D0/(4.D0*G_inver)
      
     ENDIF
     !===============================================
     IF(method == 'adv' .and. pro == "young" ) THEN
      E_inver     = S(1,1) *     ( COS(phi)*COS(phi)*COS(phi)*COS(phi) ) +&    
                    S(1,2) *2.d0*( SIN(phi)*COS(phi)*SIN(phi)*COS(phi) ) +&       
                    S(2,2) *     ( SIN(phi)*SIN(phi)*SIN(phi)*SIN(phi) ) +&
                    S(3,3) *     ( SIN(phi)*COS(phi)*SIN(phi)*COS(phi) ) +&
                    S(1,3) *2.d0*( COS(phi)*SIN(phi)*SIN(phi)*SIN(phi) ) +&
                    S(2,3) *2.d0*( SIN(phi)*COS(phi)*COS(phi)*COS(phi) ) 
      phi_pro(l)  = 1.D0 / E_inver
      
     ENDIF
     IF(method == 'adv' .and. pro == "poi" ) THEN
      E_inver     = S(1,1) *     ( COS(phi)*COS(phi)*COS(phi)*COS(phi) ) +&    
                    S(1,2) *2.d0*( SIN(phi)*COS(phi)*SIN(phi)*COS(phi) ) +&       
                    S(2,2) *     ( SIN(phi)*SIN(phi)*SIN(phi)*SIN(phi) ) +&
                    S(3,3) *     ( SIN(phi)*COS(phi)*SIN(phi)*COS(phi) ) +&
                    S(1,3) *2.d0*( COS(phi)*SIN(phi)*SIN(phi)*SIN(phi) ) +&
                    S(2,3) *2.d0*( SIN(phi)*COS(phi)*COS(phi)*COS(phi) ) 

      poi_inver =   S(1,1) * ( SIN(phi)*SIN(phi)*COS(phi)*COS(phi) )                                      +&
                    S(1,2) * ( SIN(phi)*SIN(phi)*SIN(phi)*SIN(phi) + COS(phi)*COS(phi)*COS(phi)*COS(phi) )+&
                    S(2,2) * ( SIN(phi)*SIN(phi)*COS(phi)*COS(phi) )                                      +&
                    S(1,3) * ( SIN(phi)*SIN(phi)*COS(phi)*SIN(phi) - SIN(phi)*COS(phi)*COS(phi)*COS(phi) )+&
                    S(2,3) * ( COS(phi)*COS(phi)*COS(phi)*SIN(phi) - SIN(phi)*COS(phi)*SIN(phi)*SIN(phi) )+&
                    S(3,3) * ( SIN(phi)*COS(phi)*COS(phi)*SIN(phi) * -1.0D0 )
      phi_pro(l)  = -(poi_inver / E_inver)
       !WRITE(*,*) phi*(180D0/3.1415),phi_pro(l) ,l
      IF (l.EQ.0) THEN 
        Max_pro=phi_pro(l); Min_pro=phi_pro(l);   
      ELSE
        IF (phi_pro(l).GE.Max_pro) THEN
          Max_pro=phi_pro(l)
          !WRITE(*,*) phi*(180D0/3.1415),Max_pro
        END IF  
        IF (phi_pro(l).LE.Min_pro) THEN
          Min_pro=phi_pro(l) 
          !WRITE(*,*) Min_pro    
        END IF  
      END IF
     ENDIF
     !===============================================     

    END SUBROUTINE
