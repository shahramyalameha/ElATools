!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the phase and group vec. calculation.. v.2
! see this ref. :  Computer Physics Communications 207 (2016): 445-451.
!                        in in in   in  in  in  in       outs
        SUBROUTINE wave_main_AAEP(i,j,theta,phi,vec,CCo,density, VVG_P,    &
                                                                 VVP_P,    &
                                                                 VV_P_PF,  &
                                                                 VVG_Sf,   &
                                                                 VVP_Sf,   &
                                                                 VV_Sf_PF, &
                                                                 VVG_Ss,   &
                                                                 VVP_Ss,VVP_Ss_PF    )
         IMPLICIT NONE
         
         !INTEGER, PARAMETER  :: qp = selected_real_kind(33, 931)
         
         DOUBLE PRECISION, PARAMETER :: PI_C=3.14159265358979323846264338327950D0
         DOUBLE PRECISION::          VVG_P,    &
                                     VVP_P,    &
                                     VV_P_PF,  &
                                     VVG_Sf,   &
                                     VVP_Sf,   &
                                     VV_Sf_PF, &
                                     VVG_Ss,   &
                                     VVP_Ss,VVP_Ss_PF
         DOUBLE PRECISION, DIMENSION (6,6) :: CCO
         DOUBLE PRECISION, DIMENSION (3) :: vec
         DOUBLE PRECISION :: theta, phi, density,theta3,phi3
         INTEGER    :: i, j, k, n, m, ac_mod, plan
         DOUBLE PRECISION, DIMENSION (0:9,1:3) :: poin
         
         type normals
         DOUBLE PRECISION, DIMENSION (1:3,1:3) :: normod
         end type normals
         
         type(normals), dimension(0:8) :: normp
         
         DOUBLE PRECISION, DIMENSION(3) :: a, b
         DOUBLE PRECISION :: thetar, phir, supi, supt
         DOUBLE PRECISION :: cutnorm,Nmesh_thataf,Nmesh_phIF,Nmesh_phi,Nmesh_thata,Nmesh_phi2
         
         TYPE m_matrix_YLM
           DOUBLE PRECISION, DIMENSION (0:360,0:360) :: m_matrix
         END TYPE m_matrix_YLM
         
         TYPE mod_cal
         TYPE(m_matrix_YLM) :: v_f               ! Phase velocity
         TYPE(m_matrix_YLM) :: v_g               ! Group velocity
         TYPE(m_matrix_YLM), DIMENSION(3) :: ve  ! Polarization vectors
         TYPE(m_matrix_YLM), DIMENSION(3) :: normal   ! Normal to the slowness surface
         TYPE(m_matrix_YLM) :: pow_fact                    ! power flow angle
           DOUBLE PRECISION :: vmax, thetamax, phimax, vmin, thetamin, phimin
         END TYPE mod_cal
         
         TYPE(mod_cal), dimension(1:3) :: mv
         
         DOUBLE PRECISION :: phi2, alpha 
         
        
         DOUBLE PRECISION, parameter ::  inc  = 0.00001D0

       
         CCO=CCO*1.0d9 
 
         call wave_cal(CCO,i,j,theta,phi,density,vec, VVG_P,    &
                                              VVP_P,    &
                                              VV_P_PF,  &
                                              VVG_Sf,   &
                                              VVP_Sf,   &
                                              VV_Sf_PF, &
                                              VVG_Ss,   &
                                              VVP_Ss,VVP_Ss_PF )
                
       
         END SUBROUTINE 
