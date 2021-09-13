!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the phase and group vec. calculation.. v.1

SUBROUTINE wave_cal(C,i,j,theta,phi,density,vec, VVG_P,    &
                                                          VVP_P,    &
                                                          VV_P_PF,  &
                                                          VVG_Sf,   &
                                                          VVP_Sf,   &
                                                          VV_Sf_PF, &
                                                          VVG_Ss,   &
                                                          VVP_Ss ,VV_Ss_PF   )
           IMPLICIT NONE
         
           INTEGER, PARAMETER :: qp = selected_real_kind(33, 931)
           
           DOUBLE PRECISION, PARAMETER :: PI_C=3.14159265358979323846264338327950D0
       
           DOUBLE PRECISION, DIMENSION (6,6) :: C
           DOUBLE PRECISION, DIMENSION (3) :: vec
           DOUBLE PRECISION :: theta, phi, density,theta3,phi3
           INTEGER :: i, j, k, n, m, ac_mod
           DOUBLE PRECISION:: VVG_P,    &
                              VVP_P,    &
                              VV_P_PF,  &
                              VVG_Sf,   &
                              VVP_Sf,   &
                              VV_Sf_PF, &
                              VVG_Ss,   &
                              VVP_Ss,   &
                              VV_Ss_PF
           DOUBLE PRECISION, DIMENSION (0:9,1:3) :: poin
           
           type normals
           DOUBLE PRECISION, DIMENSION (1:3,1:3) :: normod
           end type normals
           
           type(normals), dimension(0:8) :: normp
           
           DOUBLE PRECISION, DIMENSION(3) :: a, b
           DOUBLE PRECISION :: thetar, phir, supi, supt
           DOUBLE PRECISION :: cutnorm,Nmesh_thataf,Nmesh_phIF,Nmesh_phi,Nmesh_thata,Nmesh_phi2
           
           TYPE m_matrix_YLM
             DOUBLE PRECISION, DIMENSION (0:400,0:400) :: m_matrix
           END TYPE m_matrix_YLM
           
           TYPE mod_cal
           TYPE(m_matrix_YLM) :: v_f               ! Phase velocity
           TYPE(m_matrix_YLM) :: v_g               ! Group velocity
           TYPE(m_matrix_YLM), DIMENSION(3) :: ve  ! Polarization vectors
           TYPE(m_matrix_YLM), DIMENSION(3) :: normal   ! Normal to the slowness surface
           TYPE(m_matrix_YLM) :: pow_fact                    ! power flow angle
       
           END TYPE mod_cal
           
           TYPE(mod_cal), dimension(1:3) :: mv
           
           DOUBLE PRECISION :: phi2, alpha 
           
          
           DOUBLE PRECISION, parameter ::  inc  = 0.00001D0
         CALL start_val(vec,                     &
                        mv(:)%v_f%m_matrix(i,j) ,&
                        mv(1)%ve(:)%m_matrix(i,j),&
                        mv(2)%ve(:)%m_matrix(i,j),&
                        mv(3)%ve(:)%m_matrix(i,j),&
                        density,C )
       
       
       
       DO ac_mod=1,3 
        IF (dot_product(vec,mv(ac_mod)%ve(:)%m_matrix(i,j)).lt.0.0D0)   mv(ac_mod)%ve(:)%m_matrix(i,j)=-1.0D0*mv(ac_mod)%ve(:)%m_matrix(i,j)
      END DO
      CALL C_Vg_Vf(vec,                          &
                   mv(:)%v_f%m_matrix(i,j),       &
                   mv(:)%v_g%m_matrix(i,j),       &
                   density, C,                    &
                   mv(1)%normal(:)%m_matrix(i,j), &
                   mv(2)%normal(:)%m_matrix(i,j), &
                   mv(3)%normal(:)%m_matrix(i,j))
       DO ac_mod=1,3
        mv(ac_mod)%pow_fact%m_matrix(i,j)=(180.0D0/PI_C)* acos(min(1.0D0,abs(dot_product(mv(ac_mod)%normal(:)%m_matrix(i,j),vec))))
       END DO
       VVG_P   =mv(1)%v_g%m_matrix(i,j)
       VVP_P   =mv(1)%v_f%m_matrix(i,j)
       VV_P_PF =mv(1)%pow_fact%m_matrix(i,j)
       
       VVG_Sf   =mv(2)%v_g%m_matrix(i,j)
       VVP_Sf   =mv(2)%v_f%m_matrix(i,j)
       VV_Sf_PF =mv(2)%pow_fact%m_matrix(i,j)

       VVG_Ss   =mv(3)%v_g%m_matrix(i,j)
       VVP_Ss   =mv(3)%v_f%m_matrix(i,j)
       VV_Ss_PF =mv(3)%pow_fact%m_matrix(i,j)

      
      

       !OPEN(584, file='p')
       !write(584,"(12F45.25,2I5)") vec(1)*(mv(1)%v_g%m_matrix(i,j)), &
       !                            vec(2)*(mv(1)%v_g%m_matrix(i,j)), &
       !                            vec(3)*(mv(1)%v_g%m_matrix(i,j)), &
        !                           mv(1)%v_g%m_matrix(i,j),theta, phi
       end SUBROUTINE
