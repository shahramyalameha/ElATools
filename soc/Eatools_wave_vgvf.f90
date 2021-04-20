!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the phase and group vec. calculation.. vector version
         
         
         SUBROUTINE C_Vg_Vf( l0, v_f0, v_g, density, C, n1, n2, n3)
         !
         INTEGER,  PARAMETER                          :: qp = selected_real_kind(33, 4931)
         DOUBLE PRECISION, PARAMETER                  :: PI_C=3.14159265358979323846264338327950D0
         DOUBLE PRECISION, PARAMETER                  :: incn = 0.0000001D0               
         INTEGER                                      :: i, j,k, tp, ac_mod 
         DOUBLE PRECISION                             :: density,theta, phi,phi2, alpha
         DOUBLE PRECISION, DIMENSION (3)              :: l0,  &
                                                         v_f0,&
                                                         l,   &
                                                         v_f, &
                                                         ver, &
                                                         hor, &
                                                         norm,&
                                                         v1,  &
                                                         v2,  &
                                                         v3,  &
                                                         v_g, &
                                                         n1,  &
                                                         n2,  &
                                                         n3
         DOUBLE PRECISION, DIMENSION (6,6)            :: C
         DOUBLE PRECISION, DIMENSION (3,4)            :: lent
         DOUBLE PRECISION, DIMENSION (4,3)            :: lenc, vecrot
       
            phi   =  ASIN ( l0(3))
            theta =  ATAN2( l0(2) , l0(1) )
            phi2  = phi + incn*PI_C / 180.0D0
         DO tp=1,4
            IF (tp .EQ. 1 )  THEN
               vecrot(tp,1) = COS(theta)*COS(phi2)
               vecrot(tp,2) = SIN(theta)*COS(phi2)
               vecrot(tp,3) = SIN(phi2)
            ELSE
               alpha=90.0D0*REAL(tp-1 , qp)     
               CALL rotate_MESH( l0, vecrot(1,:), alpha, vecrot(tp,:) )
            END IF  
            CALL start_val( vecrot(tp,:) , v_f,v1,v2,v3,density,C)
         
            lent(1,tp) = 1.0D0/v_f(1)
            lent(2,tp) = 1.0D0/v_f(2)
            lent(3,tp) = 1.0D0/v_f(3)
         END DO
         DO ac_mod=1,3 !--------- for each vibration ac_modde
              ! Change to cartesian coord.
            DO tp=1,4
               lenc(tp,:)=lent(ac_mod,tp)*vecrot(tp,:)   
            END DO
            DO k=1,3 
               ver(k)=lenc(3,k)-lenc(1,k) 
               hor(k)=lenc(4,k)-lenc(2,k)
            END DO
            norm(1)=(ver(2)*hor(3))-(ver(3)*hor(2))
            norm(2)=(ver(3)*hor(1))-(ver(1)*hor(3))
            norm(3)=(ver(1)*hor(2))-(ver(2)*hor(1))
            norm=norm*(1.0D0/sqrt(dot_product(norm,norm)))
            IF (ac_mod==1) n1=norm
            IF (ac_mod==2) n2=norm
            IF (ac_mod==3) n3=norm
            v_g(ac_mod)=v_f0(ac_mod)/(abs(dot_product(norm,l0)))
         END DO !----------------   
      END SUBROUTINE C_Vg_Vf
         
