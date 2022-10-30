
!````````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
!!  ElATools: A tool for analyzing anisotropic elastic properties of the 2D and 3D materials


 PROGRAM AAEP_main
  IMPLICIT NONE
  ChARACTER(LEN=10)                  :: val,&                    !> for property
                                        e1,e2                       
  INTEGER, PARAMETER                 :: dp = selected_real_kind(15, 307)
  CHARACTER(LEN=1)                   :: YN="Y" ,&                !> for plot 3d and 2d data is  "Y" 
                                        yesno,yesno2,&
                                        yn_veloc,yn_ewp,&                
                                        order,yn_km                                   
  CHARACTER(LEN=25)                  :: myid                     !> for databank
  INTEGER                            :: CLaS,phi_meah,theta_meah,cutmesh,npoint,ewp
  DOUBLE PRECISION, PARAMETER        :: pi=3.14159265358979323846264338327950D0,ee=0.0001D0
  DOUBLE PRECISION, DIMENSION(6,6)   :: C1p=0D0,S1=0D0,C=0D0,S=0D0,CP=0D0,C3=0D0,CCo=0d0,Eig3d
  DOUBLE PRECISION, DIMENSION(80100) :: shear2dmax,  Ax,&
                                        shear2dminp,  &
                                        shear2dminn,  &
                                        comMIN2d,     &
                                        comMAX2d,     &
                                        bulkmin2d,    &
                                        bulkmax2d,    &
                                        young2dmin,   &
                                        young2dmax,   &
                                        hard2dmax,    &
                                        km2dmax  ,    &
                                        hard2dmin,    &
                                        poisson2dmax, &
                                        poisson2dminn,&
                                        poisson2dminp,&
                                        LM2d=0D0,     &
                                        TMmin2d=0D0,  &
                                        TMmax2d=0D0,  &
                                        pugh2dmax,    &
                                        pugh2dminp,   &
                                        pugh2dminn,   &
                                        VVG_P_2D,     & 
                                        VVP_P_2D,     &
                                        VV_P_PF_2D,   &
                                        VVG_Sf_2D,    &
                                        VVP_Sf_2D,    &
                                        VV_Sf_PF_2D,  &
                                        VVG_Ss_2D,    &
                                        VVP_Ss_2D,    &
                                        VV_Ss_PF_2D
 ChARACTER(LEN=3)                    :: adv
 DOUBLE PRECISION, DIMENSION(3,3)    :: EVe=0D0, C2D,Eig2d

 DOUBLE PRECISION, DIMENSION(3)      :: vec=0d0,             &
                                        vec_3dslic=0,        &
                                        v=0d0,               &
                                        vec1=0d0,            &
                                        EVa,                 &
                                        CElastcode=0d0
 INTEGER ::                            loop, d2d3 ,Stable,adv_mubner                !0> Stable, 1> unStable
 INTEGER ::                              i=0,             &
                                         num=1,           &
                                         lm,MinTm,        &
                                         MaxTm,           &
                                         Ncod,            &
                                         i2,              &
                                         j=0,             &
                                         jj=0,            &
                                         ii,              &
                                         Nmesh_thata=0d0, &
                                         Nmesh_thataf=0d0,&
                                         Nmesh_phi=0d0,   &
                                         Nmesh_phIF=0D0,  &
                                         Nmesh_phi2=0D0,  &
                                         Nmesh_phIF2=0D0, &
                                         kk=0D0,          &
                                         Nmesh_gamma=0D0
 
 DOUBLE PRECISION ::                     l,an,                         &
                                         maxEVaLM  = 0D0,              &
                                         maxEVaTM  = 0D0,              &
                                         minEVaTM  = 10d6,             &
                                         maxEVaLMf = 0D0,              &
                                         maxEVaTMf = 0D0,              &
                                         minEVaTMf = 10d6,test_n, new_num
 
 DOUBLE PRECISION ::                     v11=0d0,&
                                         t2d=0D0,                   &
                                         v12=0d0,                   &
                                         v13=0d0,                   &
                                         v22=0d0,                   &
                                         v23=0d0,                   &
                                         v33=0d0,                   &
                                         G_max=0D0,                 &
                                         G_min=0D0,                 &
                                         G_Ave=0D0,                 &
                                         G_max2=0D0,                &
                                         G_min2=1D0,                &
                                         G_max2_theta=0D0,          &
                                         G_max2_phi=0D0,            &
                                         G_min2_theta=0D0,          &
                                         G_min2_phi=0D0,            &

                                        Ha_max2       = 0D0,        &
                                        Ha_min2       = 0.0D0,      & 
                                        Ha_max2_theta = 0D0,        & 
                                        Ha_min2_theta = 0D0,        &
                                        Ha_max2_phi   = 0D0,        &
                                        Ha_min2_phi   = 0D0,        &

                                         Pugh_max=0D0,              &
                                         Pugh_min=0D0,              &
                                         Pugh_Ave=0D0,              &
                                         Pugh_max2=0D0,             &
                                         Pugh_min2=1D0,             &
                                         Pugh_max2_theta=0D0,       &
                                         Pugh_max2_phi=0D0,         &
                                         Pugh_min2_theta=0D0,       &
                                         Pugh_min2_phi=0D0,         &
                                         Maxcomp_theta=0D0,         &
                                         Maxcomp_phi=0D0,           &
                                         Mincomp_theta=0D0,         &
                                         Mincomp_phi=0D0,           &
                                         Mincomp=10d8,              &
                                         Maxcomp=0D0,               &
                                         NPratio_max=0D0,           &
                                         Pratio=0D0,                &
                                         NPratio_min=1D0,           &
                                         NPratio_ave=0D0,           &
                                         Pratio_max=0D0,            &
                                         Pratio_min=1D0,            &
                                         Pratio_max_theta=0D0,      &
                                         Pratio_max_phi=0D0,        &
                                         Pratio_min_theta=0D0,      &
                                         Pratio_min_phi=0D0,        &
                                         v1min=0D0,                 &
                                         v2min=0D0,                 &
                                         v3min=0D0,                 &
                                         v1max=0D0,                 &
                                         v2max=0D0,                 &
                                         v3max=0D0,                 &
                                         v1minmin=0D0,              &
                                         v2minmin=0D0,              &
                                         v3minmin=0D0,              &
                                         v1maxmax=0D0,              &
                                         v2maxmax=0D0,              &
                                         v3maxmax=0D0,              &
                                         pmax=0.0001D0,             &
                                         pminp=0.0001D0,            &
                                         pminn=0.0001D0,            &
                                         pavep=0D0,                 &
                                         paven=0.0D0,               &
                                         shmax=0.01D0,              &
                                         shminp=0.01D0,             &
                                         shminn=0.01D0,             &
                                         shavep=0D0,                &
                                         shaven=0D0,                &
                                         pughmax=0.01D0,            &
                                         pughminp=0.01D0,           &
                                         pughminn=0.01D0,           &
                                         pughavep=0D0,              &
                                         pughaven=0D0,              &
                                         comminn=0.1D0,             &
                                         comminp=0.1D0                
 REAL(dp) ::SS=1.0_dp,SINver=0.0_dp
 DOUBLE PRECISION ::                     inter_phi=0D0,                    &
                                         BB=1,                             &
                                         Maxyoung_phi=0D0,                 &
                                         Maxbulk_theta=0D0,                &
                                         Maxyoung_theta=0D0,               &
                                         Maxbulk_phi=0D0,                  &
                                         b_r=0D0,                          &
                                         c1=0D0,                           &
                                         inter_theta=0D0,                  &
                                         phi=0,                            &
                                         BINver=0D0,hardvar=0D0,           &
                                         a1,a2,a3
 
 DOUBLE PRECISION ::                     Minyoung=10d8,                    &
                                         Minbulk=0.0d0,                    &
                                         Maxyoung=0D0,                     &
                                         Maxbulk=0D0,                      &
                                         Minyoung_theta=0D0,               &
                                         Minbulk_theta=0D0,                &
                                         Maxbulkm2_theta=0D0,              &
                                         Minbulkm2_theta=0D0,              &
                                         Maxbulkm2_phi=0D0,                &
                                         Minbulkm2_phi=0D0,                &
                                         Minyoung_phi=0D0,                 &
                                         Minbulk_phi=0D0,                  &
                                         theta=0d0,                        &
                                         smkl=0D0,smkl2=0D0,               &
                                         twoDTheta=0D0,                    &
                                         planetheta,                       &
                                         planephi,                         &
                                         mmx=1d0,                          &
                                         kky=0d0,                          &
                                         llz=0d0,                          &
                                         mmx1=0d0,                         &
                                         kky1=0d0,                         &
                                         llz1=0d0


 DOUBLE PRECISION ::                     a6666=0D0,                    &
                                         sheainvar,CO=0D0,             &
                                         Minsheainvar,                 &
                                         Maxsheainvar,                 &
                                         sheainvar_ave,                &
                                         Minpughvar,                   &
                                         Maxpughvar,                   &
                                         pughvar_ave,                  & 
                                         bulk_m2,                      &
                                         Min_bulkm2=0.0d0,             &
                                         Max_bulkm2=0.0d0,             &
                                         gamma,                        &
                                         vv11=0D0,                     &
                                         vv12=0D0,                     &
                                         vv13=0D0,                     &
                                         vv22=0D0,                     &
                                         vv23=0D0,                     &
                                         vv33=0D0,                     &
                                         ave,                          &
                                         k11,                          &
                                         k12,                          &
                                         k13,                          &
                                         k22,                          &
                                         k23,                          &
                                         k33

 DOUBLE PRECISION, PARAMETER           ::  PI_C=3.14159265358979323846264338327950D0
 DOUBLE PRECISION                      ::  density,theta3,phi3,ma_avrag=1,dns,tot_at=0.0
 INTEGER                               ::  k, n, m, ac_mod, plan
 DOUBLE PRECISION, DIMENSION (0:9,1:3) ::  poin
 DOUBLE PRECISION                      ::VVG_P,          &
                                         VVP_P=0D0,        &
                                         VV_P_PF=0D0,      &
                                         VVG_Sf=0D0,       &
                                         VVP_Sf=0D0,       &
                                         VV_Sf_PF=0D0,     &
                                         VVG_Ss=0D0,       &
                                         VVP_Ss=0D0,       &
                                         VV_Ss_PF=0d0,     &
                                         VVG_P_max=0D0,    &
                                         VVP_P_max=0D0,    &
                                         VV_P_PF_max=0D0,  &
                                         VVG_Sf_max=0D0,   &
                                         VVP_Sf_max=0D0,   &
                                         VV_Sf_PF_max=0D0, &
                                         VVG_Ss_max=0D0,   &
                                         VVP_Ss_max=0D0,   &
                                         VV_Ss_PF_max=0D0, &
                                         VVG_P_min=0D0,    &
                                         VVP_P_min=0D0,    &
                                         VV_P_PF_min=0D0,  &
                                         VVG_Sf_min=0D0,   &
                                         VVP_Sf_min=0D0,   &
                                         VV_Sf_PF_min=0D0, &
                                         VVG_Ss_min=0D0,   &
                                         VVP_Ss_min=0.0001D0,  &
                                         VV_Ss_PF_min=0d0,     &
                                         VVG_P_max_phi=0D0,    &
                                         VVP_P_max_phi=0D0,    &
                                         VV_P_PF_max_phi=0D0,  &
                                         VVG_Sf_max_phi=0D0,   &
                                         VVP_Sf_max_phi=0D0,   &
                                         VV_Sf_PF_max_phi=0D0, &
                                         VVG_Ss_max_phi=0D0,   &
                                         VVP_Ss_max_phi=0D0,   &
                                         VVG_P_min_phi= 0D0,   &
                                         VVP_P_min_phi=0D0,    &
                                         VV_P_PF_min_phi=0D0,  &
                                         VVG_Sf_min_phi=0D0,   &
                                         VVP_Sf_min_phi=0D0,   &
                                         VV_Sf_PF_min_phi=0D0, &
                                         VVG_Ss_min_phi=0D0,   &
                                         VVP_Ss_min_phi=0D0,   &
                                         VV_Ss_PF_min_phi=0d0, &
                                         VV_Ss_PF_max_phi=0d0, &
                                         VVG_P_max_theta=0D0,    &
                                         VVP_P_max_theta=0D0,    &
                                         VV_P_PF_max_theta=0D0,  &
                                         VVG_Sf_max_theta=0D0,   &
                                         VVP_Sf_max_theta=0D0,   &
                                         VV_Sf_PF_max_theta=0D0, &
                                         VVG_Ss_max_theta=0D0,   &
                                         VVP_Ss_max_theta=0D0,   &
                                         VVG_P_min_theta=0D0,    &
                                         VVP_P_min_theta=0D0,    &
                                         VV_P_PF_min_theta=0D0,  &
                                         VVG_Sf_min_theta=0D0,   &
                                         VVP_Sf_min_theta=0D0,   &
                                         VV_Sf_PF_min_theta=0D0, &
                                         VVG_Ss_min_theta=0D0,   &
                                         VVP_Ss_min_theta=1D0,   &
                                         VV_Ss_PF_min_theta=0d0, &
                                         VV_Ss_PF_max_theta=0d0, &
                                         km                    , &
                                         km_max=0.d0           , &
                                         km_min=0.0d0          , &
                                         km_min_phi=0.0d0      , &
                                         km_max_phi=0.0d0      , &
                                         km_min_theta=0.0d0    , &
                                         km_max_theta=0.0d0   
 
 TYPE normals
 DOUBLE PRECISION, DIMENSION (1:3,1:3) :: normod
 END TYPE normals
 TYPE(normals),    DIMENSION(0:8) :: normp
 DOUBLE PRECISION, DIMENSION(3)   :: a, b
 DOUBLE PRECISION                 :: thetar, phir, supi, supt
 DOUBLE PRECISION                 :: cutnorm
 TYPE m_matrix_YLM
 DOUBLE PRECISION, DIMENSION (0:360,0:360) :: m_matrix
 END TYPE m_matrix_YLM
 TYPE mod_cal
 TYPE(m_matrix_YLM)               :: v_f               !> Phase velocity
 TYPE(m_matrix_YLM)               :: v_g               !> Group velocity
 TYPE(m_matrix_YLM), DIMENSION(3) :: ve                !> Polarization vectors
 TYPE(m_matrix_YLM), DIMENSION(3) :: normal            !> Normal to the slowness surface
 TYPE(m_matrix_YLM)               :: pow_fact          !> power flow angle
 END TYPE mod_cal
 TYPE(mod_cal),    DIMENSION(1:3) :: mv
 DOUBLE PRECISION                 :: phi2, alpha 
 DOUBLE PRECISION, PARAMETER      ::  inc  = 0.00001D0


  !=================v1.7.3=======================
 INTEGER                         :: iargc
 INTEGER                         :: argcount
 CHARACTER(LEN=80)               :: wq_char,    &
                                    input_m,    &
                                    input_d,    &
                                    input_ewp,  &
                                    input_mtc,  &
                                    input_opt,  &
                                    help_optin 
                                    
 DOUBLE PRECISION                :: input_dens
 ChARACTER(len=7), Dimension(15) :: arg_mane
  !=================v1.7.3=======================
!>>>>>>>>>>>>>>>>>>>>>>>>END<<<<<<<<<<<<<<<<<<<<<<<<< 
 OPEN(99,FILE="DATA.out") !> final static data 
 CALL SYSTEM('clear')
 WRITE(*,*) ' '
 CALL WELCOME()
 WRITE(*,*) ' '
 CALL SYSTEM('sleep 1.1')
  !=================v1.7.3======================= 
  input_d    = "N"
  input_m    = "N"
  input_ewp  = "N"
  input_mtc  = "N"
  input_opt  = "N"
  input_dens = 0.0
  argcount=iargc()
  DO i=1,argcount 
!   CALL GETARG(i,wq_char)
!   arg_mane(i)=wq_char
        CALL GETARG(i,wq_char)
     IF ( INDEX(wq_char,'-d').NE.0 ) THEN
        CALL GETARG(i+1,wq_char)
        READ(wq_char,*) input_d
     END IF
     ! 
     IF ( INDEX(wq_char,'-m').NE.0 ) THEN
        CALL GETARG(i+1,wq_char)
        READ(wq_char,*) input_m
     END IF
     ! 
     IF ( INDEX(wq_char,'-ewp').NE.0 ) THEN
        CALL GETARG(i+1,wq_char)
        READ(wq_char,*) input_ewp
     END IF 
     ! 
     IF ( INDEX(wq_char,'-ktc').NE.0 ) THEN
        CALL GETARG(i+1,wq_char)
        READ(wq_char,*) input_mtc
     END IF 
     ! 
     IF ( INDEX(wq_char,'-op').NE.0 ) THEN
        CALL GETARG(i+1,wq_char)
        READ(wq_char,*) input_opt
     END IF
     IF ( INDEX(wq_char,'-h').NE.0 ) THEN
        CALL GETARG(i+1,wq_char)
        READ(wq_char,*) help_optin
     END IF
     !           
  END DO
  !=================v1.7.3======================= 
  
   IF (help_optin == "help") THEN
       CALL HELP_me()
       STOP
   ENDIF

225 CALL SYSTEM('clear')
IF (input_d == "N") THEN    !!!!! FOR COMMAND LINE NEW!! ====> start 
 
 WRITE(*,*)" > Select system dimension:"  !> go to type of system
 CALL SYSTEM('tput setaf 36;tput bold; echo " ========================";tput sgr0')
 !WRITE(*,*)"========================" 
  WRITE(*,*)" 2D-Materials------ => 2"
  WRITE(*,*)" 3D-Materials------ => 3"
 !WRITE(*,*)"========================" 
 CALL SYSTEM('tput setaf 36;tput bold; echo " ========================";tput sgr0')
 READ(*,*) d2d3
  
ELSE !v1.7.3

  IF (input_d == "3" ) THEN
      d2d3=3
      WRITE(99,*) " > System Type: 3D"
  ELSE IF(input_d == "2") THEN
      d2d3=2
      WRITE(99,*) " > System Type: 2D"
  ELSE 
   WRITE(*,"(2A)") "Invalid input: -d ", input_d
   STOP 
  ENDIF
ENDIF   
 !=== !v1.7.3
IF(d2d3 == 3) THEN                               !@@@@@@@@@@@@@@@@@@@@@@@ 2D_3D system start
  665  CALL SYSTEM('clear')
  IF (input_m == "N") THEN !=== !v1.7.3
    WRITE(*,*)" > Select the desired method (code, file and databank):"
    CALL SYSTEM('tput setaf 33;tput bold; echo "============================================================";tput sgr0')
    !WRITE(*,*)"============================================================" 
    WRITE(*,*)" IRelast-----------------------(       WEIN2k         )-=> 1"
    WRITE(*,*)" Elast-------------------------(       WEIN2k         )-=> 2"
    WRITE(*,*)" AELAS-------------------------(        VASP          )-=> 3"
    WRITE(*,*)" ElaStic-----------------------(  QE,WEIN2k,Exciting  )-=> 4"   
    WRITE(*,*)" Using Cij Tensor in Cij.dat---(     Other codes      )-=> 5" 
    WRITE(*,*)" Using EC Databank-------------(      Offline MP      )-=> 6" 
    WRITE(*,*)" Using EC Databank-------------(      Online  MP      )-=> 7"
    WRITE(*,*)" Back --------------------------------------------------=> 0"
    !WRITE(*,*)"============================================================" 
    CALL SYSTEM('tput setaf 33;tput bold; echo "============================================================";tput sgr0')  
    READ(*,*) Ncod
    IF(Ncod .EQ. 0) THEN; Goto 225; ENDIF
  ELSE !=== !v1.7.3
    IF (input_m == "IRelast" .or. input_m == "irelast" .or. input_m == "1") THEN
      Ncod=1
      WRITE(99,*) " > Output code: IRelast"
    ELSE IF(input_m == "Elast" .or. input_m == "elast" .or. input_m == "2") THEN
      Ncod=2
      WRITE(99,*) " > Output code: Elast"
    ELSE IF(input_m == "AELAS" .or. input_m == "aelas" .or. input_m == "3") THEN
      Ncod=3
      WRITE(99,*) " > Output code: AELAS"
    ELSE IF(input_m == "ElaStic" .or. input_m == "elastic" .or. input_m == "4") THEN
      Ncod=4
      WRITE(99,*) " > Output code: ElaStic"    
    ELSE IF(input_m == "Cij" .or. input_m == "cij" .or. input_m == "5") THEN
      Ncod=5
      WRITE(99,*) " > Output code: Cij file"        
    ELSE IF(input_m == "offmp" .or. input_m == "6") THEN
      Ncod=6
      WRITE(99,*) " > Output code: offline Databank"            
    ELSE IF(input_m == "onmp" .or. input_m == "7") THEN
      Ncod=7 
      WRITE(99,*) " > Output code: Online Databank"  
    ELSE
      WRITE(*,"(2A)") "Invalid input: -m ", input_m
      STOP                 
    ENDIF 
  ENDIF
  !=== !v1.7.3
  IF(input_ewp == "n" .and. input_mtc == "n")THEN !=== !v1.7.3
    WRITE(*,"(A,A4,A,A4)") "Invalid input: -ewp ", input_ewp, "-ktc ", input_mtc
    STOP
  ENDIF
  IF(input_ewp == "n" .and. input_mtc == "y")THEN !=== !v1.7.3
    WRITE(*,"(A,A4,A,A4)") "Invalid input: -ewp ", input_ewp, "-ktc ", input_mtc
    STOP
  ENDIF
  IF(input_ewp == "N" .and. input_mtc == "y")THEN !=== !v1.7.3
    WRITE(*,"(A,A4,A,A4)") "Invalid input: -ewp ", input_ewp, "-ktc ", input_mtc
    STOP
  ENDIF
  IF(input_ewp == "n" .and. input_mtc == "N") THEN !=== !v1.7.3
    !WRITE(*,"(A,A4,A,A4)") "Invalid input: -ewp ", input_ewp, "-ktc ", input_mtc
    yn_veloc = "n"
    goto 5501
  ENDIF  
  IF(input_ewp == "N" .or. input_mtc == "N") THEN !=== !v1.7.3
      
    IF (Ncod .eq. 1 .OR. Ncod .eq. 2 .OR. Ncod .eq. 3 .OR. Ncod .eq. 4 .OR. Ncod .eq. 5 .OR. Ncod .eq. 6 .OR. Ncod .eq. 7) THEN
      WRITE(*,*)" > Do you want to calculate elastic wave properties? (Y/n):" !> select code for calculate of phase and group velocities
      READ(*,*) yn_veloc
      CALL SYSTEM('clear') 
      
      IF (yn_veloc == 'Y' .or. yn_veloc == 'y') THEN
        WRITE(99,*) " > Calculate elastic wave properties: On"
        WRITE(*,*)" > Select the desired option:"
        CALL SYSTEM('tput setaf 63;tput bold; echo "================================================================";tput sgr0')
        WRITE(*,"(a)")" Phase, group and PFA without Min. thermal conductivity-----=> 1"
        WRITE(*,"(a)")" Phase, group and PFA  with   Min. thermal conductivity-----=> 2"
        WRITE(*,"(a)")" Back ------------------------------------------------------=> 0"
        CALL SYSTEM('tput setaf 63;tput bold; echo "================================================================";tput sgr0')
        Read(*,*) ewp
        !#######################################################################
        IF (ewp == 1) THEN
          WRITE(99,*) " > Calculate Min. thermal conductivity: On"
          WRITE(*,*)"Density of Compound (kg/m^3):"
          WRITE(*,*)"Note: IF you don't know, enter 0"
          READ(*,*) density
          IF (density==0)THEN
            CALL system("dens.x dns")
            OPEN(845,file=".rhocom")
            READ(845,*)density
            CLOSE(845)
          END IF
          WRITE (99,*) " > Density of Compound =", density,"(kg/m^3)"
        ENDIF   
        !#######################################################################
        IF (ewp== 2) THEN
        WRITE(99,*) " > Calculate Min. thermal conductivity: On"
          yn_km='y'
          WRITE(*,*)"> Density of Compound (kg/m^3):"
          WRITE(*,*)"Note: IF you don't know, enter 0"
          READ(*,*) density
          IF (density==0)THEN
            CALL system("dens.x dns")
            OPEN(845,file=".rhocom")
            READ(845,*)density
            CLOSE(845)
          END IF
          !===================================
          open(124,file='.mavg_com')  
          WRITE(*,*)"> Mean mass of atoms in each unit cell (gr):"
          WRITE(*,*)"Note: IF you don't know, enter 0"
          READ(*,*) ma_avrag
          WRITE(124,*) ma_avrag
          WRITE(*,*)"> Total of atoms in each unit cell:"
          WRITE(*,*)"Note: IF you don't know, enter 0"
          READ(*,*) tot_at
          WRITE(124,*) tot_at
          Close(124)
          IF (ma_avrag==0 .or. tot_at==0)THEN    
            CALL system("dens.x mav") 
            OPEN(414,file=".mavg_com")
            READ(414,*) ma_avrag
            READ(414,*) tot_at
            CLOSE(414)
          ENDIF
          WRITE (99,*) " > Mean mass of atoms =", ma_avrag, "(gr)"
          WRITE (99,*) ""
        ENDIF
        !####################################################################### 
        IF (ewp== 0) THEN
          Goto 665
        ENDIF
      ELSE
        WRITE(99,*) " > Calculate elastic wave properties: Off" 
      ENDIF
    ENDIF 

  ELSE !=== !v1.7.3


    IF (input_ewp == "y" .and. input_mtc == "n" .or. input_mtc == "N") THEN !=== !v1.7.3
      yn_veloc = "y"
      ewp      = 1
      WRITE(99,*) " > Calculate Min. thermal conductivity: Off"
      !#######################################################################
      WRITE(*,*)"Density of Compound (kg/m^3):"
      WRITE(*,*)"Note: IF you don't know, enter 0"
      READ(*,*) density
  
      IF (density==0)THEN
        CALL system("dens.x dns")
        OPEN(845,file=".rhocom")
        READ(845,*)density
        CLOSE(845)
      END IF
      WRITE (99,*) " > Density of Compound =", density,"(kg/m^3)"
      !#######################################################################
    ENDIF
    IF (input_ewp == "y" .and. input_mtc == "y"  ) THEN
      yn_veloc = "y"  
      ewp      = 2
       WRITE(99,*) " > Calculate Min. thermal conductivity: On"
      !#######################################################################
      yn_km='y'
      WRITE(*,*)"> Density of Compound (kg/m^3):"
      WRITE(*,*)"Note: IF you don't know, enter 0"
      READ(*,*) density
      
      IF (density==0)THEN
        CALL system("dens.x dns")
        OPEN(845,file=".rhocom")
        READ(845,*)density
        CLOSE(845)
      END IF
      !!!!!!!!!!!!!!  
      open(124,file='.mavg_com')  
         
      WRITE(*,*)"> Mean mass of atoms in each unit cell (gr):"
      WRITE(*,*)"Note: IF you don't know, enter 0"
      READ(*,*) ma_avrag
      WRITE(124,*) ma_avrag
      WRITE(*,*)"> Total of atoms in each unit cell:"
      WRITE(*,*)"Note: IF you don't know, enter 0"
      READ(*,*) tot_at
      WRITE(124,*) tot_at
         
      close(124)
      IF(ma_avrag==0 .or. tot_at==0)THEN    
         CALL system("dens.x mav") 
         OPEN(414,file=".mavg_com")
         READ(414,*) ma_avrag
         READ(414,*) tot_at
         CLOSE(414)
      ENDIF
      WRITE (99,*) " > Mean mass of atoms =", ma_avrag, "(gr)"
      WRITE (99,*) "" 
    ENDIF 
        
  ENDIF 



  !===================================
5501  IF (Ncod .eq. 1) THEN
    OPEN(4,FILE="INVELC-matrix",status='old',err=1367)
    CALL system ('cp INVELC-matrix Cij.dat')
    CALL C_Inv_M(6) 
    CALL system ('cp Sij.dat Cij.dat')
    CALL C_Inv_M(6)
    CLOSE(4)
    CALL system('clear')
  ENDIF
  IF (Ncod .eq. 2) THEN
    OPEN(5,FILE="elast.output",status='old',err=1366)
    OPEN(6,FILE=".ELSEtcode") 
    OPEN(7,FILE="Cij.dat",status='old',err=1369) 
    CALL system ('cat elast.output | tail -1| cut -f2 -d=| cut -f1 -dc  > .ELSEtcode ')  !C11
    CALL system ('cat elast.output | tail -1| cut -f3 -d=| cut -f1 -dc >> .ELSEtcode ')  !C12
    CALL system ('cat elast.output | tail -1| cut -f4 -d=| cut -f1 -dc >> .ELSEtcode ')  !C44      
    DO i2=1,3
       READ(6,*) CElastcode(i2)
    ENDDO
    CLOSE(6)        
    C(1,1)=CElastcode(1) ; C(1,2)=CElastcode(2);C(4,4)=CElastcode(3)
    C(2,2)=C(1,1);C(3,3)=C(1,1)
    C(2,1)=C(1,2);C(1,3)=C(1,2);
    C(3,1)=C(1,2);C(2,3)=C(1,2);C(3,2)=C(1,2)
    C(5,5)=C(4,4);C(6,6)=C(4,4) 
 
    WRITE(7,50) C(1,1),C(1,2),C(1,3),C(1,4),C(1,5),C(1,6)
    WRITE(7,50) C(2,1),C(2,2),C(2,3),C(2,4),C(2,5),C(2,6)
    WRITE(7,50) C(3,1),C(3,2),C(3,3),C(3,4),C(3,5),C(3,6)
    WRITE(7,50) C(4,1),C(4,2),C(4,3),C(4,4),C(4,5),C(4,6)
    WRITE(7,50) C(5,1),C(5,2),C(5,3),C(5,4),C(5,5),C(5,6)
    WRITE(7,50) C(6,1),C(6,2),C(6,3),C(6,4),C(6,5),C(6,6) 
    CLOSE(7)        
    50 format(6F9.4)
    CLOSE(5)
    CALL system('clear')
  ENDIF 
  CALL system('clear')
  IF (Ncod .EQ. 3) THEN
    CALL system("sed '1,2d' ELADAT > ELADAT_temp")
    OPEN(79,FILE="Cij.dat")
    OPEN(53,FILE="ELADAT_temp",status='old',err=1361)
    DO i=1,6
      READ(53,*) (C3(i,j),j=1,6)
      WRITE(79,10) (C3(i,j),j=1,6)
    ENDDO
    CLOSE(53)
    CLOSE(79)
    !CALL sleep(5)
  ENDIF
  
  10 format(6F11.1)
  IF (Ncod.EQ.4) THEN
    WRITE(*,*)"> The program does not support 3rd order of the elastic constant!"
    WRITE(*,*)"Do you continue? (Y/n)"
    READ(*,*)order
    IF(order=='y'.or.order=='Y') THEN
       CALL system("sed '1,15d' ElaStic_2nd.out > ElaStic_2nd_temp")
       OPEN(79,FILE="Cij.dat")
       OPEN(53,FILE="ElaStic_2nd_temp",status='old',err=1341)
       DO i=1,6
         READ(53,*) (C3(i,j),j=1,6)
         WRITE(79,10) (C3(i,j),j=1,6)
       ENDDO
       CLOSE(53)
       CLOSE(79)
      ELSE
        stop
      end IF
    End IF
    IF (Ncod .EQ. 5) THEN
      GOTO 101
    ENDIF
    
    IF (Ncod .EQ. 6) THEN
      5050 WRITE(*,*)"> ENTER ID: (EXAMPEL:  mp-10 or mvc-916)"
      WRITE(*,*)"============================= offline"
      read(*,*)myid
      CALL databank(myid,yesno)
      
      IF (yesno=='N')THEN
        WRITE(*,*)"----------------------------------------------"
        WRITE(*,*)"> Want to repeat again?(Y/n)"
        READ(*,*)yesno2
        IF (yesno2=='Y'.or. yesno2=='y')THEN
          CALL SYSTEM('clear')
          GOTO 5050
        ELSE
          GOTO 1370
        END IF
      END IF     
      CALL sleep (2)
      CALL system('clear')
      CALL system('mv Cij-id.dat Cij.dat')
      GOTO 101
    ENDIF 
    
    IF (Ncod .EQ. 7) THEN
      5051 WRITE(*,*)"> ENTER ID: (EXAMPEL:  mp-10 or mvc-916)"
      WRITE(*,*)"============================= online"
      read(*,*)myid
      CALL aip_get_online(myid)
      CALL system ("chmod +x aip.py; ./aip.py > Cij-id.dat") 
      IF (yesno=='N')THEN
        WRITE(*,*)"----------------------------------------------"
        WRITE(*,*)"> Want to repeat again?(Y/n)"
        READ(*,*)yesno2
        IF (yesno2=='Y'.or. yesno2=='y')THEN
          CALL SYSTEM('clear')
          GOTO 5051
        ELSE
          GOTO 1370
        END IF
      END IF     
      
      CALL sleep (2)
      CALL system('clear')
      CALL system('mv Cij-id.dat Cij.dat')
      CALL system("rm aip.py")
      GOTO 101
    ENDIF
    CALL system('clear')
    101 OPEN(11,FILE="Cij.dat",status='old', err=1369)                             ! read cij data inpout
    READ(11,*) C(1,1),C(1,2),C(1,3),C(1,4),C(1,5),C(1,6)
    READ(11,*) C(2,1),C(2,2),C(2,3),C(2,4),C(2,5),C(2,6)
    READ(11,*) C(3,1),C(3,2),C(3,3),C(3,4),C(3,5),C(3,6)
    READ(11,*) C(4,1),C(4,2),C(4,3),C(4,4),C(4,5),C(4,6)
    READ(11,*) C(5,1),C(5,2),C(5,3),C(5,4),C(5,5),C(5,6)
    READ(11,*) C(6,1),C(6,2),C(6,3),C(6,4),C(6,5),C(6,6)
    CLOSE(11)
    CALL C_Inv_M(6) 
    CALL system('clear')
    !! for other code <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    OPEN(12,FILE="Sij.dat")                              ! read sij data input (generated by C_Inv_M(6) or other data and code)
    READ(12,*) S1(1,1),S1(1,2),S1(1,3),S1(1,4),S1(1,5),S1(1,6)
    READ(12,*) S1(2,1),S1(2,2),S1(2,3),S1(2,4),S1(2,5),S1(2,6)
    READ(12,*) S1(3,1),S1(3,2),S1(3,3),S1(3,4),S1(3,5),S1(3,6)
    READ(12,*) S1(4,1),S1(4,2),S1(4,3),S1(4,4),S1(4,5),S1(4,6)
    READ(12,*) S1(5,1),S1(5,2),S1(5,3),S1(5,4),S1(5,5),S1(5,6)
    READ(12,*) S1(6,1),S1(6,2),S1(6,3),S1(6,4),S1(6,5),S1(6,6)
    CLOSE(12)
    S(1,1)=S1(1,1); S(1,2)=S1(1,2); S(1,3)=S1(1,3); S(1,4)=S1(1,4); S(1,5)=S1(1,5); S(1,6)=S1(1,6)   ! TESTING....
    S(2,1)=S1(2,1); S(2,2)=S1(2,2); S(2,3)=S1(2,3); S(2,4)=S1(2,4); S(2,5)=S1(2,5); S(2,6)=S1(2,6)
    S(3,1)=S1(3,1); S(3,2)=S1(3,2); S(3,3)=S1(3,3); S(3,4)=S1(3,4); S(3,5)=S1(3,5); S(3,6)=S1(3,6)
    S(4,1)=S1(4,1); S(4,2)=S1(4,2); S(4,3)=S1(4,3); S(4,4)=S1(4,4); S(4,5)=S1(4,5); S(4,6)=S1(4,6)
    S(5,1)=S1(5,1); S(5,2)=S1(5,2); S(5,3)=S1(5,3); S(5,4)=S1(5,4); S(5,5)=S1(5,5); S(5,6)=S1(5,6)
    S(6,1)=S1(6,1); S(6,2)=S1(6,2); S(6,3)=S1(6,3); S(6,4)=S1(6,4); S(6,5)=S1(6,5); S(6,6)=S1(6,6)
    !> Stability 
    CALL stability3d( Stable, C,Eig3d )
    IF (Stable==1) THEN
      CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
      CALL system ('tput setaf 9;tput bold; echo " > Elastic Stability Conditions:  Unstable; STOP";tput sgr0')
      CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
      WRITE(99,*)" ========================================" 
      WRITE(99,"(a)")" > Elastic Stability Conditions:  Unstable; STOP"
      WRITE(99,*)" ========================================" 
    END IF
    IF (Stable==0) THEN
      CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
      CALL system ('tput setaf 10;tput bold; echo " > Elastic Stability Conditions:  Stable";tput sgr0')
      CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
      WRITE(99,*)" ========================================" 
      WRITE(99,"(a)")" > Elastic Stability Conditions:  Stable"
      WRITE(99,*)" ========================================" 
    END IF
    WRITE(*,'(A,36f10.2)') " > Eigenvalues (GPa):",Eig3d(1,1),Eig3d(2,2),Eig3d(3,3) ,Eig3d(4,4) ,Eig3d(5,5) ,Eig3d(6,6)
    WRITE(99,'(A,36f10.2)') " > Eigenvalues (GPa):",Eig3d(1,1),Eig3d(2,2),Eig3d(3,3) ,Eig3d(4,4) ,Eig3d(5,5) ,Eig3d(6,6)
    WRITE(99,*)" "
    CALL sleep(1) 
    !<
    CALL proelast()
  
    IF (yn_veloc=='Y' .or. yn_veloc=='y') WRITE (*,*) " > Density of Compound =", density,"(kg/m^3)"
    IF (ewp== 2)                          WRITE (*,*) " > Mean mass of atoms =", ma_avrag, "(gr)"
    IF (ewp== 2)                          CALL pro_wave(density,ma_avrag)
    WRITE(*,*)""
    CALL system ('tput setaf 122;tput bold; echo " > Enter phi-meah and theta-meah between 50 and 250 (Recom.: 150*150):";tput sgr0')
    !WRITE(*,*)" > Enter phi-meah and theta-meah between 50 and 250 (Recommended: 150):"
    OPEN(59,file='MESH')
    READ(*,*)phi_meah,theta_meah
    cutmesh=(phi_meah*theta_meah)+1
    WRITE(59,*)phi_meah,theta_meah,cutmesh
    close(59)
    WRITE (*,*) " "
    CALL system ('tput setaf 142;tput bold; echo " > Select the (hkl) Miller indices for 2D cut:";tput sgr0')
    !WRITE(*,*)"> Select the (hkl) Miller indices for 2D cut:" 
    READ(*,*) mmx,kky,llz
    mmx1=mmx
    kky1=kky
    llz1=llz
    CALL findpaln(mmx,kky,llz, e1,e2,yn_veloc)
    !WRITE(*,*)mmx,kky,llz
    !WRITE(*,*)mmx,kky,llz
    CALL sleep (1)
  IF (mmx.EQ.0 .AND. kky.EQ.0 .AND. llz.EQ.0) THEN
    mmx=1
    kky=0
    llz=0
    WRITE(*,*)'(0 0 0) is not accepted! => Chenged to (1 0 0).'
    CALL sleep (1)
  ENDIF
  CALL system('clear; sleep 1.5')
ELSE
  IF(d2d3 == 2) THEN
    OPEN(140,file="HKL")
    WRITE(140,*)"X"
    WRITE(140,*)"Y"
    WRITE(140,"(I2)")0 
    WRITE(140,"(I2)")0
    WRITE(140,"(I2)")1
    WRITE(140,*)'N'
    close(140)
    226 CALL system('clear')
    IF (input_m == "N")THEN
      WRITE(*,*)" > Select the desired method (code or file):"
      CALL SYSTEM('tput setaf 12;tput bold; echo " =====================================================";tput sgr0')
      !WRITE(*,*)"====================================================="
      WRITE(*,*)" AELAS                            (    VASP   ) => 1"
      WRITE(*,*)" IRelast                          (   WIEN2K  ) => 2"    
      WRITE(*,*)" Using Cij Tensor in Cij-2D.dat   (other codes) => 3"
      WRITE(*,*)" Back ----------------------------------------- => 0"    
      !WRITE(*,*)"====================================================="
      CALL SYSTEM('tput setaf 12;tput bold; echo " =====================================================";tput sgr0')
      read(*,*) Ncod
      IF(Ncod .EQ. 0) THEN; Goto 225; ENDIF
    ELSE
      IF (input_m == "AELAS" .or. input_m == "aelas" .or. input_m == "1") THEN
        Ncod=1
        !WRITE(*,*) "Output code: Elast"
      ELSE IF(input_m == "IRelast2D" .or. input_m == "irelast2D" .or. input_m == "2") THEN
        Ncod=2
        !WRITE(*,*) "Output code: Elast"
      ELSE IF(input_m == "Cij" .or. input_m == "cij" .or. input_m == "3") THEN
        Ncod=3
      ELSE
        WRITE(*,"(2A)") "Invalid input: -m ", input_m
        STOP                 
      ENDIF
    ENDIF
    IF (Ncod .EQ. 1) THEN
      CALL system('clear')
      IF(input_opt == "N" )THEN
        WRITE(*,*)" > Select the type of two-dimensional system:"    
        CALL SYSTEM('tput setaf 41;tput bold; echo " ====================================================";tput sgr0')  
        WRITE(*,*) " Default  option (Hex., Squ., and Rec. systems) => 1  "
        WRITE(*,*) " Advanced option (       Oblique systems      ) => 2  "
        WRITE(*,*) " Back ------------------------------------------=> 0  "      
        CALL SYSTEM('tput setaf 41;tput bold; echo " ====================================================";tput sgr0')  
        read(*,*)adv_mubner
      ELSE
        IF (input_opt == "def" .or. input_opt == "1") THEN
          adv_mubner=1
          !WRITE(*,*) "Output code: Elast"
        ELSE IF(input_opt == "adv"  .or. input_opt == "2") THEN
          adv_mubner=2
        ELSE
          WRITE(*,"(2A)") "Invalid input: -op ", adv_mubner
          STOP                 
        ENDIF
      ENDIF

      IF (adv_mubner == 2 ) adv = "adv"
      IF (adv_mubner == 1 ) adv = "ndv"
      IF (adv_mubner == 0 ) goto 226
    
      CALL system("sed '1,2d' ELADAT > ELADAT_temp")
      OPEN(79,FILE="Cij-2D.dat")
      OPEN(53,FILE="ELADAT_temp",status='old',err=1361)
      DO i=1,3
        READ(53,*) (C2D(i,j),j=1,3)
        WRITE(79,50) (C2D(i,j),j=1,3)
      ENDDO
      close(53)
      close(79)
      !CALL C_Inv_M2D(3)

      !> Stability
      CALL stability2d( Stable, C2D,Eig2d )
      IF (Stable == 1) THEN
        CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
        CALL system ('tput setaf 9;tput bold; echo " > Elastic Stability Conditions:  Unstable; STOP";tput sgr0')
        CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
        WRITE(99,*)" ========================================" 
        WRITE(99,*) " > Elastic Stability Conditions:  Unstable" 
        WRITE(99,*)" ========================================"  
               
        STOP
      END IF
      IF (Stable == 0) THEN
        CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
        CALL system ('tput setaf 10;tput bold; echo " > Elastic Stability Conditions:  Stable";tput sgr0')
        CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
        WRITE(99,*)" ========================================" 
        WRITE(99,*) " > Elastic Stability Conditions:  Stable" 
        WRITE(99,*)" ========================================" 
        
      END IF
      WRITE(*,'(A,36f10.2)')" > Eigenvalues (N/m):", Eig2d(1,1),Eig2d(2,2),Eig2d(3,3) 
      WRITE(99,'(A,36f10.2)')" > Eigenvalues (N/m):", Eig2d(1,1),Eig2d(2,2),Eig2d(3,3) 
      WRITE(99,*)" "
 
      !<
      CALL C_Inv_M2D(3)
      CALL proelast_2D ()
      CALL sleep(2)
      WRITE(*,*)""
      CALL system ('tput setaf 122;tput bold; echo " > Enter phi-mesh. Be divisible by 100 (e.g. 100, 200, 300, etc):";tput sgr0')
      !WRITE(*,*)" > Enter phi-mesh. Be divisible by 100.(e.g. 100, 200, 300, etc):"
      read(*,*)npoint
    ENDIF
    IF (Ncod .eq. 2) THEN                                   !> IRelast2D
      
      CALL system('clear')
      IF(input_opt == "N" )THEN
         WRITE(*,*)" > Select the type of two-dimensional system:"    
         CALL SYSTEM('tput setaf 41;tput bold; echo " ====================================================";tput sgr0')  
         WRITE(*,*) " Default  option (Hex., Squ., and Rec. systems) => 1  "
         WRITE(*,*) " Advanced option (       Oblique systems      ) => 2  "
         WRITE(*,*) " Back ------------------------------------------=> 0  "      
         CALL SYSTEM('tput setaf 41;tput bold; echo " ====================================================";tput sgr0')  
         read(*,*)adv_mubner
      ELSE
        IF (input_opt == "def" .or. input_opt == "1") THEN
          adv_mubner=1
          !WRITE(*,*) "Output code: Elast"
        ELSE IF(input_opt == "adv"  .or. input_opt == "2") THEN
          adv_mubner=2
        ELSE
          WRITE(*,"(2A)") "Invalid input: -op ", adv_mubner
          STOP                 
        ENDIF
      ENDIF

      IF (adv_mubner == 2 ) adv = "adv"
      IF (adv_mubner == 1 ) adv = "ndv"
      IF (adv_mubner == 0 ) goto 226
    
      OPEN(11,FILE="ELC-matrix",status='old', err=13692)        
      READ(11,*) C2D(1,1)
      READ(11,*) C2D(1,2)
      READ(11,*) C2D(1,3)
      READ(11,*) C2D(2,1)
      READ(11,*) C2D(2,2)
      READ(11,*) C2D(2,3)
      READ(11,*) C2D(3,1)
      READ(11,*) C2D(3,2)
      READ(11,*) C2D(3,3)
      close(11)
      OPEN(79,FILE="Cij-2D.dat")
      DO i=1,3
        WRITE(79,50) (C2D(i,j),j=1,3)
      ENDDO
      close(79)
      !CALL C_Inv_M2D(3)
      !> Stability
      CALL stability2d( Stable, C2D,Eig2d )
      IF (Stable == 1) THEN
        WRITE(*,*)""
        CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
        CALL system ('tput setaf 9;tput bold; echo " > Elastic Stability Conditions:  Unstable; STOP";tput sgr0')
        CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
        WRITE(99,*)" ========================================" 
        WRITE(99,*) " > Elastic Stability Conditions:  Unstable" 
        WRITE(99,*)" ========================================"         
        STOP
      END IF
      IF (Stable == 0) THEN
        WRITE(*,*)""
        CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
        CALL system ('tput setaf 10;tput bold; echo " > Elastic Stability Conditions:  Stable";tput sgr0')
        CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
        WRITE(99,*)" ========================================" 
        WRITE(99,*) " > Elastic Stability Conditions:  Stable" 
        WRITE(99,*)" ========================================" 
      END IF
      WRITE(*,'(A,36f10.2)')" > Eigenvalues (N/m):", Eig2d(1,1),Eig2d(2,2),Eig2d(3,3) 
      WRITE(99,'(A,36f10.2)')" > Eigenvalues (N/m):", Eig2d(1,1),Eig2d(2,2),Eig2d(3,3) 
      WRITE(99,*)" "
      !>
      CALL C_Inv_M2D(3)
      CALL proelast_2D ()
      CALL sleep(2)
      WRITE(*,*)""
      CALL system ('tput setaf 122;tput bold; echo " > Enter phi-mesh. Be divisible by 100 (e.g. 100, 200, 300, etc):";tput sgr0')
      !WRITE(*,*)" > Enter phi-mesh. Be divisible by 100.(e.g. 100, 200, 300, etc):"
      read(*,*)npoint
    END IF    
    IF (Ncod .eq. 3) THEN
      CALL system('clear')
      IF(input_opt == "N" )THEN

      WRITE(*,*)" > Select the type of two-dimensional system:"    
      CALL SYSTEM('tput setaf 41;tput bold; echo " ====================================================";tput sgr0')  
      WRITE(*,*) " Default  option (Hex., Squ., and Rec. systems) => 1  "
      WRITE(*,*) " Advanced option (       Oblique systems      ) => 2  "
      WRITE(*,*) " Back ------------------------------------------=> 0  "      
      CALL SYSTEM('tput setaf 41;tput bold; echo " ====================================================";tput sgr0')  
      read(*,*)adv_mubner
      ELSE
        IF (input_opt == "def" .or. input_opt == "1") THEN
          adv_mubner=1
          !WRITE(*,*) "Output code: Elast"
        ELSE IF(input_opt == "adv"  .or. input_opt == "2") THEN
          adv_mubner=2
        ELSE
          WRITE(*,"(2A)") "Invalid input: -op ", adv_mubner
          STOP                 
        ENDIF
      ENDIF
      IF (adv_mubner == 2 ) adv = "adv"
      IF (adv_mubner == 1 ) adv = "ndv"
      IF (adv_mubner == 0 ) goto 226
      OPEN(11,FILE="Cij-2D.dat",status='old', err=13691)        
      READ(11,*) C2D(1,1),C2D(1,2),C2D(1,3)
      READ(11,*) C2D(2,1),C2D(2,2),C2D(2,3)
      READ(11,*) C2D(3,1),C2D(3,2),C2D(3,3)
      close(11)
      
      !> Stability
      CALL stability2d( Stable, C2D,Eig2d )
      IF (Stable == 1) THEN
        WRITE(*,*)""
        CALL system ('tput setaf 9;tput bold; echo " ========================================";tput sgr0')     
        CALL system ('tput setaf 9;tput bold; echo " > Elastic Stability Conditions:  Unstable; STOP";tput sgr0')
        CALL system ('tput setaf 9;tput bold; echo " ========================================";tput sgr0')
        WRITE(99,*)" ========================================" 
        WRITE(99,*) " > Elastic Stability Conditions:  Unstable" 
        WRITE(99,*)" ========================================" 
        STOP
      END IF
      IF (Stable == 0) THEN
        WRITE(*,*)""
        CALL system ('tput setaf 10;tput bold; echo " ========================================";tput sgr0')
        CALL system ('tput setaf 10;tput bold; echo " > Elastic Stability Conditions:  Stable";tput sgr0')
        CALL system ('tput setaf 10;tput bold; echo " ========================================";tput sgr0')
        WRITE(99,*)" ========================================" 
        WRITE(99,*) " > Elastic Stability Conditions:  Stable" 
        WRITE(99,*)" ========================================" 
      END IF
      WRITE(*,'(A,36f10.2)')" > Eigenvalues (N/m):", Eig2d(1,1),Eig2d(2,2),Eig2d(3,3) 
      WRITE(99,'(A,36f10.2)')" > Eigenvalues (N/m):", Eig2d(1,1),Eig2d(2,2),Eig2d(3,3) 
      WRITE(99,*)" "
      !<
      CALL C_Inv_M2D(3)
      CALL proelast_2D()
      CALL sleep(1)
      WRITE(*,*)""
      CALL system ('tput setaf 122;tput bold; echo " > Enter phi-mesh. Be divisible by 100 (e.g. 100, 200, 300, etc):";tput sgr0')
      !WRITE(*,*)" > Enter phi-mesh. Be divisible by 100.(e.g. 100, 200, 300, etc):"
      read(*,*)npoint
    END IF    
    IF(d2d3 /= 3 .and. d2d3 /= 2) THEN
      WRITE(*,*)"Invalid Input!!";   
      STOP
    END IF 
  END IF
END IF  !@@@@@@@@@@@@@@@@@@@@@@@ 2D_3D end


WRITE(*,*)" > Preparing data. Please wait..."
IF(d2d3 == 3) THEN !@@@@@@@@@@@@@@@@@@@@@@@ 2D_3D system start
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Data clarity
  Nmesh_thata  = theta_meah!100
  Nmesh_phi    = phi_meah!100
  Nmesh_phIF   = 150
  Nmesh_thataf = 150
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ Preparing 3D data...
!open(78,file='uuu')
  OPEN(111, FILE="3d_SD.dat"       )
  OPEN(115, FILE="3d_vec.dat"      )
  OPEN(14,  FILE="3d_comp.dat"     )
  OPEN(28,  FILE="3d_hardness.dat" )
 ! OPEN(29,FILE="CompN_Min.dat"    )
  
  OPEN(65, FILE="3d_pugh.dat"    )
 ! OPEN(66,FILE="pughP_Min.dat"   )
 ! OPEN(67,FILE="pughN_Min.dat"   )
 ! OPEN(68,FILE="pughP_Ave.dat"   )
 
  OPEN(15, FILE="3d_young.dat")
  
  OPEN(16, FILE="3d_shear.dat"    )
  !OPEN(17,FILE="shearP_Min.dat"   )
 ! OPEN(8, FILE="shearN_Min.dat"   )
 ! OPEN(7, FILE="shearP_Ave.dat"   )
    !OPEN(18,FILE="shearN_Ave.dat"   )
  
  OPEN(20, FILE="3d_poissons.dat"   )
 ! OPEN(21,FILE="MinP_ratio.dat"   )
 ! OPEN(22,FILE="MinN_ratio.dat"   )
  !OPEN(23,FILE="AveP_ratio.dat"   )
  !OPEN(24,FILE="AveN_ratio.dat"   )
  
  OPEN(30, FILE="3d_bulk_m2.dat" )
  OPEN(31, FILE="3d_bulk.dat"      )
  
  OPEN(32, FILE="3d_sound.dat"     ) 
  !OPEN(33,FILE="MaxTm_sound.dat"  )
  !OPEN(34,FILE="MinTm_sound.dat"  )
  IF (yn_veloc=='Y' .or. yn_veloc=='y') THEN
    OPEN(100, FILE="3d_pp.dat"  ) ! 3d_pp.dat  > VVP_P.dat
    OPEN(101, FILE="3d_gp.dat"  ) ! 3d_gp.dat  > VVG_P.dat
    OPEN(102, FILE="3d_pfp.dat" ) ! 3d_pfp.dat > VV_P_PF.dat
    OPEN(103, FILE="3d_gf.dat"  ) ! 3d_gf.dat  > VVG_Sf.dat
    OPEN(104, FILE="3d_pf.dat"  ) ! 3d_pf.dat  > VVP_Sf.dat
    OPEN(105, FILE="3d_pff.dat" ) ! 3d_fpf.dat > VV_Sf_PF.dat
    OPEN(106, FILE="3d_gs.dat"  ) ! 3d_gs.dat  > VVG_Ss.dat
    OPEN(107, FILE="3d_ps.dat"  )	! 3d_ps.dat  > VVP_Ss.dat
    OPEN(108, FILE="3d_pfs.dat" ) ! 3d_spf.dat > VVP_Ss_PF.dat
  ENDIF  
   IF (yn_km=='Y' .or. yn_km=='y') THEN
    OPEN(110,FILE="3d_km.dat"       )
  ENDIF  
  !IF (MOD(Nmesh_thata,Nmesh_thataf).NE.0) THEN
   ! Nmesh_thata=(Nmesh_thata/Nmesh_thataf)*Nmesh_thataf
   ! WRITE(*,*)Nmesh_thata
  !ENDIF
        !
  !inter_theta=Nmesh_thata/Nmesh_thataf
 ! IF (MOD(Nmesh_phi,Nmesh_phIF).NE.0) THEN
   ! Nmesh_phi=(Nmesh_phi/Nmesh_phIF)*Nmesh_phIF
  !  WRITE(*,*)Nmesh_phi
  !ENDIF
    !inter_phi=Nmesh_phi/Nmesh_phIF 
        !
  open(59, FILE='.aelastpro')

  IF (yn_veloc=='Y' .or. yn_veloc=='y') THEN
    open(55, FILE='.aelastpro2')
  ENDIF
  
!! Start loop=0 for wave  and elste pro.============================

        CALL CALLCij (CCO)
        i=0
        j=0
        theta=DBLE(i)/DBLE(Nmesh_thata)*PI
        phi=DBLE(j)/DBLE(Nmesh_phi)*2D0*PI
        vec(1)=SIN(theta)*COS(phi)
        vec(2)=SIN(theta)*SIN(phi)
        vec(3)=COS(theta)
        v11=vec(1)*vec(1) ; v12=vec(1)*vec(2)
        v13=vec(1)*vec(3) ; v22=vec(2)*vec(2)
        v23=vec(2)*vec(3) ; v33=vec(3)*vec(3)
                
        BB=( S(1,1)+S(1,2)+S(1,3) )*v11&
          +( S(1,6)+S(2,6)+S(3,6) )*v12&
          +( S(1,5)+S(2,5)+S(3,5) )*v13&
          +( S(1,2)+S(2,2)+S(2,3) )*v22&
          +( S(1,4)+S(2,4)+S(3,4) )*v23&
          +( S(1,3)+S(2,3)+S(3,3) )*v33 
      BINver=1D0/BB
      Maxbulk = BINver
      Minbulk = BINver
      !!
      SS=v11*v11*S(1,1)                 &
      +2*v12*v12*S(1,2)                 &
      +2*v13*v13*S(1,3)                 &
      +2*v12*v13*S(1,4)                 &
      +2*v11*v13*S(1,5)                 &
      +2*v11*v12*S(1,6)+  v22*v22*S(2,2)&
      +2*v23*v23*S(2,3)                 &
      +2*v22*v23*S(2,4)                 &
      +2*v12*v23*S(2,5)                 &
      +2*v12*v22*S(2,6)+  v33*v33*S(3,3)&
      +2*v23*v33*S(3,4)                 &
      +2*v13*v33*S(3,5)                 &
      +2*v13*v23*S(3,6)+  v23*v23*S(4,4)&
      +2*v13*v23*S(4,5)                 &
      +2*v12*v23*S(4,6)+  v13*v13*S(5,5)&
      +2*v12*v13*S(5,6)+  v12*v12*S(6,6)
      SINver=1.0_dp/SS   !Young
      CALL CShear(G_min,G_max,G_Ave,phi,theta,v11,v12,v13,v22,v23,v33,a6666,sheainvar)
      !==================================================== bulk method 3       v1.7.3        
 CALL bulk_method(Pratio, sheainvar*1000D0, bulk_m2)
     
 ! Max_bulkm2 = bulk_m2
		Min_bulkm2 = bulk_m2
		
		!WRITE(*,*) bulk_m2
	IF (bulk_m2 .GE. Max_bulkm2)THEN
     Max_bulkm2      = bulk_m2
     Maxbulkm2_theta = theta
     Maxbulkm2_phi   = phi
    ! WRITE(*,*) Max_bulkm2
  ENDIF
 
	IF (bulk_m2 .LE. Min_bulkm2)THEN
		Min_bulkm2      = bulk_m2
    Minbulkm2_theta = theta
    Minbulkm2_phi   = phi
    !WRITE(*,*) Min_bulkm2
	ENDIF
     
!==================================================== bulk method 3       v1.7.3 
      CALL CHardness(BINver,SINver,sheainvar,hardvar)
      ! WRITE(*,*)hardvar
        Ha_max2 = hardvar
        Ha_min2 = hardvar
        
 IF (yn_veloc=='Y' .or. yn_veloc=='y') THEN        
        CALL wave_main_AAEP(i,j,theta,phi,vec,CCO,density,   VVG_P,    &
                                                             VVP_P,    &
                                                             VV_P_PF,  &
                                                             VVG_Sf,   &
                                                             VVP_Sf,   &
                                                             VV_Sf_PF, &
                                                             VVG_Ss,   &
                                                             VVP_Ss,   &
                                                             VV_Ss_PF  )

       
        VVG_P_min         = VVG_P 
        VVP_P_min         = VVP_P 
        VV_P_PF_min       = VV_P_PF 
        ! 
        VVG_P_max         = VVG_P 
        VVP_P_max         = VVP_P 
        VV_P_PF_max       = VV_P_PF
        
        VVG_Sf_min        = VVG_Sf 
        VVP_Sf_min        = VVP_Sf 
        VV_Sf_PF_min      = VV_Sf_PF 
        ! 
        VVG_Sf_max        = VVG_Sf 
        VVP_Sf_max        = VVP_Sf 
        VV_Sf_PF_max      = VV_Sf_PF
        ! 
        VVG_Ss_min        = VVG_Ss 
        VVP_Ss_min        = VVP_Ss  
        VV_Ss_PF_min      = VV_Ss_PF
        ! 
        VVG_Ss_max        = VVG_Ss 
        VVP_Ss_max        = VVP_Ss  
        VV_Ss_PF_max      = VV_Ss_PF
    
ENDIF 
 IF (yn_km == 'Y' .or. yn_km == 'y') THEN   
       CALL Ckm_cal(density, SINver, ma_avrag, km )
         km_max           = km
         km_min           = km      
 ENDIF 
!! Finish loop=0 for wave and elsat pro.============================

  OPEN(10, FILE="Cij.dat")
  DO i=0,Nmesh_thata
    theta=DBLE(i)/DBLE(Nmesh_thata)*PI
    
    Nmesh_phi2 = Nmesh_phi
    IF (i.EQ.0 .OR. i.EQ.Nmesh_thata) Nmesh_phi2=0D0 
    IF ( i==Nmesh_thata/4d0 ) THEN  ; CALL system( ' clear ' ) ;PRINT*, '|====%25====>-------------|'; ENDIF
    IF ( i==Nmesh_thata/3d0 ) THEN  ; CALL system( ' clear ' ) ;PRINT*, '|======%50======>---------|'; ENDIF
    IF ( i==Nmesh_thata/2d0 ) THEN  ; CALL system( ' clear ' ) ;PRINT*, '|========%75========>-----|'; ENDIF
    IF ( i==Nmesh_thata/1d0 ) THEN  ; CALL system( ' clear ' ) ;PRINT*, '|==========%100==========>|'; ENDIF
    
    DO j=0, Nmesh_phi2
      phi=DBLE(j)/DBLE(Nmesh_phi)*2D0*PI
      
      vec(1)=SIN(theta)*COS(phi)
      vec(2)=SIN(theta)*SIN(phi)
      vec(3)=COS(theta)
      v11=vec(1)*vec(1) ; v12=vec(1)*vec(2)
      v13=vec(1)*vec(3) ; v22=vec(2)*vec(2)
      v23=vec(2)*vec(3) ; v33=vec(3)*vec(3)
      IF (yn_veloc == 'Y' .OR. yn_veloc == 'y') THEN
       CALL CALLCij (CCO)

       CALL wave_main_AAEP( i,j,theta,phi,vec,CCO,density,   VVG_P,    &
                                                             VVP_P,    &
                                                             VV_P_PF,  &
                                                             VVG_Sf,   &
                                                             VVP_Sf,   &
                                                             VV_Sf_PF, &
                                                             VVG_Ss,   &
                                                             VVP_Ss,   &
                                                             VV_Ss_PF  )


        ! WRITE(*,*)VVP_P,VVP_Ss,VVP_Sf
        IF (VVG_P .GE. VVG_P_max) THEN
          VVG_P_max       =  VVG_P 
          VVG_P_max_phi   = (phi *180.0D0)/PI
          VVG_P_max_theta = (theta*180.0D0)/PI 
        ENDIF 
        IF (VVG_P .LE. VVG_P_min) THEN
          VVG_P_min       = VVG_P 
          VVG_P_min_phi   = (phi *180.0D0)/PI
          VVG_P_min_theta = (theta*180.0D0)/PI 
        ENDIF
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!&&&&&&&&&&&&&&&&&
        IF (VVP_P .GE. VVP_P_max) THEN
          VVP_P_max       = VVP_P 
          VVP_P_max_phi   = (phi *180.0D0)/PI
          VVP_P_max_theta = (theta*180.0D0)/PI 
        ENDIF 
        IF (VVP_P .LE. VVP_P_min) THEN
          VVP_P_min       = VVP_P
          VVP_P_min_phi   = (phi *180.0D0)/PI
          VVP_P_min_theta = (theta*180.0D0)/PI                   
        ENDIF
 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!&&&&&&&&&&&&&&&&&
        IF (VV_P_PF.GE.VV_P_PF_max) THEN
          VV_P_PF_max       = VV_P_PF
          VV_P_PF_max_phi   = (phi *180.0D0)/PI
          VV_P_PF_max_theta = (theta*180.0D0)/PI 
        ENDIF 
 
        IF (VV_P_PF.LE.VV_P_PF_min) THEN
          VV_P_PF_min       = VV_P_PF  
          VV_P_PF_min_phi   =(phi *180.0D0)/PI
          VV_P_PF_min_theta = (theta*180.0D0)/PI                     
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!&&&&&&&&&&&&&&&&&
        IF (VVG_Sf.GE.VVG_Sf_max) THEN
          VVG_Sf_max       = VVG_Sf
          VVG_Sf_max_phi   = (phi *180.0D0)/PI
          VVG_Sf_max_theta = (theta*180.0D0)/PI 
        ENDIF 
        IF (VVG_Sf.LE.VVG_Sf_min ) THEN
          VVG_Sf_min       = VVG_Sf  
          VVG_Sf_min_phi   =(phi *180.0D0)/PI
          VVG_Sf_min_theta = (theta*180.0D0)/PI                    
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!&&&&&&&&&&&&&&&&&
        IF (VVP_Sf.GE.VVP_Sf_max) THEN
          VVP_Sf_max       = VVP_Sf
          VVP_Sf_max_phi   = (phi *180.0D0)/PI
          VVP_Sf_max_theta = (theta*180.0D0)/PI 
        ENDIF 
        IF (VVP_Sf.LE.VVP_Sf_min ) THEN
          VVP_Sf_min       = VVP_Sf  
          VVP_Sf_min_phi   =(phi *180.0D0)/PI
          VVP_Sf_min_theta = (theta*180.0D0)/PI                     
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!&&&&&&&&&&&&&&&&&
        IF (VV_Sf_PF.GE.VV_Sf_PF_max) THEN
          VV_Sf_PF_max       = VV_Sf_PF
          VV_Sf_PF_max_phi   = (phi *180.0D0)/PI
          VV_Sf_PF_max_theta = (theta*180.0D0)/PI
        ENDIF 
        IF (VV_Sf_PF.LE.VV_Sf_PF_min ) THEN
          VV_Sf_PF_min       = VV_Sf_PF  
          VV_Sf_PF_min_phi   =(phi *180.0D0)/PI
          VV_Sf_PF_min_theta = (theta*180.0D0)/PI                    
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!&&&&&&&&&&&&&&&&&
        IF (VVP_Ss.GE.VVP_Ss_max) THEN
          VVP_Ss_max       = VVP_Ss
          VVP_Ss_max_phi   = (phi *180.0D0)/PI
          VVP_Ss_max_theta = (theta*180.0D0)/PI 
        ENDIF 
!
        IF (VVP_Ss.LE.VVP_Ss_min) THEN
          VVP_Ss_min       = VVP_Ss  
          VVP_Ss_min_phi   =(phi *180.0D0)/PI
          VVP_Ss_min_theta = (theta*180.0D0)/PI                     
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!&&&&&&&&&&&&&&&&
        IF (VVG_Ss.GE.VVG_Ss_max) THEN
          VVG_Ss_max       = VVG_Ss
          VVG_Ss_max_phi   = (phi *180.0D0)/PI
          VVG_Ss_max_theta = (theta*180.0D0)/PI 
        ENDIF 
        IF (VVG_Ss.LE.VVG_Ss_min) THEN
          VVG_Ss_min       = VVG_Ss  
          VVG_Ss_min_phi   =(phi *180.0D0)/PI
          VVG_Ss_min_theta = (theta*180.0D0)/PI                     
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!&&&&&&&&&&&&&&&&&
        IF (VV_Ss_PF.GE.VV_Ss_PF_max) THEN
          VV_Ss_PF_max       = VV_Ss_PF
          VV_Ss_PF_max_phi   = (phi *180.0D0)/PI
          VV_Ss_PF_max_theta = (theta*180.0D0)/PI 
        ENDIF 
        IF (VV_Ss_PF.LE.VV_Ss_PF_min) THEN
          VV_Ss_PF_min       = VV_Ss_PF  
          VV_Ss_PF_min_phi   =(phi *180.0D0)/PI
          VV_Ss_PF_min_theta = (theta*180.0D0)/PI                     
        ENDIF
      ENDIF

      CALL Csound(vec,v11,v12,v22,v13,v23,v33,EVa,Lm,MaxTm,MinTm)
      maxEVaLM = EVa(Lm)
      maxEVaTM = EVa(MaxTm)
      minEVaTM = EVa(MinTm)
      IF (EVa(Lm)    > maxEVaLMf )  maxEVaLMf=EVa(Lm)
      IF (EVa(MaxTm) > maxEVaTMf)  THEN
        maxEVaTMf=EVa(MaxTm)
      ENDIF
      IF (EVa(MinTm) < minEVaTMf) THEN
        minEVaTMf=EVa(MinTm)
      ENDIF
      !
        !open(1,FILE="l2")
        ! WRITE(1,*) vec(1)*abs(minEVaTM),vec(2)*abs(minEVaTM),vec(3)*abs(minEVaTM)
        !
        ! Triclinic
        !IF(CLaS .EQ. 7) THEN
!==================================================== bulk method 1       v1.7.2        
      BB=( S(1,1)+S(1,2)+S(1,3) )*v11&
        +( S(1,6)+S(2,6)+S(3,6) )*v12&
        +( S(1,5)+S(2,5)+S(3,5) )*v13&
        +( S(1,2)+S(2,2)+S(2,3) )*v22&
        +( S(1,4)+S(2,4)+S(3,4) )*v23&
        +( S(1,3)+S(2,3)+S(3,3) )*v33 
      BINver=1D0/BB
      IF (BINver.GE.Maxbulk) THEN
        Maxbulk       = BINver
        Maxbulk_theta = theta
        Maxbulk_phi   = phi
        !WRITE(*,*) Maxyoung
      ENDIF 
      !
      IF (BINver.LE.Minbulk) THEN
        Minbulk       = BINver                       
        Minbulk_theta = theta
        Minbulk_phi   = phi
        !WRITE(*,*)Minyoung
      ENDIF
!==================================================== bulk method 1        v1.7.2 

!==================================================== bulk method 3       v1.7.3        
 CALL bulk_method(Pratio, sheainvar*1000D0, bulk_m2)
     
 ! Max_bulkm2 = bulk_m2
		Min_bulkm2 = bulk_m2
		
		!WRITE(*,*) bulk_m2
	IF (bulk_m2 .GE. Max_bulkm2)THEN
     Max_bulkm2      = bulk_m2
     Maxbulkm2_theta = theta
     Maxbulkm2_phi   = phi
    ! WRITE(*,*) Max_bulkm2
  ENDIF
 
	IF (bulk_m2 .LE. Min_bulkm2)THEN
		Min_bulkm2      = bulk_m2
    Minbulkm2_theta = theta
    Minbulkm2_phi   = phi
    !WRITE(*,*) Min_bulkm2
	ENDIF
     
!==================================================== bulk method 3       v1.7.3        
      SS=v11*v11*S(1,1)&
      +2*v12*v12*S(1,2)&
      +2*v13*v13*S(1,3)&
      +2*v12*v13*S(1,4)&
      +2*v11*v13*S(1,5)&
      +2*v11*v12*S(1,6)+  v22*v22*S(2,2)&
      +2*v23*v23*S(2,3)&
      +2*v22*v23*S(2,4)&
      +2*v12*v23*S(2,5)&
      +2*v12*v22*S(2,6)+  v33*v33*S(3,3)&
      +2*v23*v33*S(3,4)&
      +2*v13*v33*S(3,5)&
      +2*v13*v23*S(3,6)+  v23*v23*S(4,4)&
      +2*v13*v23*S(4,5)&
      +2*v12*v23*S(4,6)+  v13*v13*S(5,5)&
      +2*v12*v13*S(5,6)+  v12*v12*S(6,6)
      SINver=1.0_dp/SS   !Young
          !open(1,file='d')
          !WRITE(1,*)theta,phi,SINver
      IF (SINver.GE.Maxyoung) THEN
        Maxyoung       = SINver
        Maxyoung_theta = theta
        Maxyoung_phi   = phi
        !WRITE(*,*) Maxyoung
      ENDIF 
       !
      IF (SINver.LE.Minyoung) THEN
        Minyoung       = SINver
        Minyoung_theta = theta
        Minyoung_phi   = phi
        !WRITE(*,*)Minyoung
      ENDIF
 !-----------------------------------------------
  IF (yn_km=='Y' .OR. yn_km=='y') THEN
    CALL Ckm_cal(density, SINver, ma_avrag, km )
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!&&&&&&&&&&&&&&&& > v1.7.0

        IF (km.GE.km_max) THEN
          km_max       = km
          km_max_phi   = (phi *180.0D0)/PI
          km_max_theta = (theta*180.0D0)/PI 
        ENDIF 
        IF (km.LE.km_min) THEN
          km_min       = km  
          km_min_phi   = (phi *180.0D0)/PI
          km_min_theta = (theta*180.0D0)/PI                     
        ENDIF
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!&&&&&&&&&&&&&&&&&
  ENDIF      
  !-----------------------------------------------
      CALL CShear(G_min,G_max,G_Ave,phi,theta,v11,v12,v13,v22,v23,v33,a6666,sheainvar)

      IF (G_max.GE.G_max2) THEN
        G_max2       = G_max
        G_max2_theta = theta
        G_max2_phi   = phi
      ENDIF  
      IF (G_min.LE.G_min2) THEN
        G_min2       = G_min
        G_min2_theta = theta
        G_min2_phi   = phi
      ENDIF
      IF (G_min.LE.0D0) shminn = -G_min
      IF (G_min.ge.0D0) shminp =  G_min
      IF (G_Ave.LE.0D0) shaven = -G_Ave
      IF (G_Ave.ge.0D0) shavep =  G_Ave

      CALL Cpugh(pugh_min,pugh_max,pugh_Ave,phi,theta,v11,v12,v13,v22,v23,v33,a6666,sheainvar)

      IF (pugh_max.GE.pugh_max2) THEN
        pugh_max2       = pugh_max
        pugh_max2_theta = theta
        pugh_max2_phi   = phi
      ENDIF  
      IF (pugh_min.LE.pugh_min2) THEN
        pugh_min2       = pugh_min
        pugh_min2_theta = theta
        pugh_min2_phi   = phi
      ENDIF

      IF (pugh_min.LE.0D0) pughminn = -pugh_min
      IF (pugh_min.ge.0D0) pughminp =  pugh_min
      IF (pugh_Ave.LE.0D0) pughaven = -pugh_Ave
      IF (pugh_Ave.ge.0D0) pughavep =  pugh_Ave


      CO= ( S(1,1)+S(1,2)+S(1,3) )*v11&
            +( S(1,6)+S(2,6)+S(3,6) )*v12&
            +( S(1,5)+S(2,5)+S(3,5) )*v13&
            +( S(1,2)+S(2,2)+S(2,3) )*v22&
            +( S(1,4)+S(2,4)+S(3,4) )*v23&
            +( S(1,3)+S(2,3)+S(3,3) )*v33 
 
         !WRITE(*,"(F22.18)")CO*1000D0
      IF ((CO*1000D0) .GE. Maxcomp) THEN 
        Maxcomp=CO*1000D0
        Maxcomp_theta=theta
        Maxcomp_phi=phi 
      ENDIF
 
      IF ((CO*1000D0) .LE. Mincomp) THEN
        Mincomp       = CO*1000D0
        Mincomp_theta = theta
        Mincomp_phi   = phi
      ENDIF
      comminn=0.0d0
      comminp=0.0d0
      ! WRITE(41,*) Mincomp
      IF (co .LE. 0.0D0) comminn = -1.0d0*CO
      IF (co .GE. 0.0D0) comminp =  CO

      NPratio_max=0D0
      NPratio_min=1D0
      CALL CPratio( SS,                     &
                       NPratio_min,            &
                       NPratio_max,            &
                       NPratio_ave,            &
                       v1min,                  &
                       v2min,                  &
                       v3min,                  &
                       v1max,                  &
                       v2max,                  &
                       v3max,                  &
                       theta,phi,              &
                       v11,v12,v13,v22,v23,v33,&
                       Pratio                  )
                    
      IF (NPratio_max.GE.Pratio_max) THEN
        Pratio_max       = NPratio_max
        Pratio_max_theta = theta
        Pratio_max_phi   = phi
        v1maxmax         = v1max
        v2maxmax         = v2max
        v3maxmax         = v3max
      ENDIF  
      IF (NPratio_min.LE.Pratio_min) THEN
        Pratio_min       = NPratio_min
        Pratio_min_theta = theta
        Pratio_min_phi   = phi
        v1minmin         = v1min
        v2minmin         = v2min
        v3minmin         = v3min
      ENDIF
                                   pmax  =  NPratio_max
      IF (NPratio_min.LE.0.0000D0) pminn = -NPratio_min
      IF (NPratio_min.ge.0.0000D0) pminp =  NPratio_min
      IF (NPratio_ave.LE.0.0000D0) paven = -NPratio_ave
      IF (NPratio_ave.ge.0.0000D0) pavep =  NPratio_ave
!!!!!!!!!!!!!!!!!!!  v1.6.3
      CALL CHardness(BINver,SINver,G_max*1000D0, hardvar)
 
      IF (hardvar.GE.Ha_max2) THEN
        Ha_max2       = hardvar   
        Ha_max2_theta = theta
        Ha_max2_phi   = phi
      ENDIF  
      IF (hardvar.LE.Ha_min2) THEN
        Ha_min2       = hardvar
        Ha_min2_theta = theta
        Ha_min2_phi   = phi
      ENDIF
!
      !open(41,file='o')
      WRITE(59,*) G_max*1000D0,shminp*1000D0,shminn*1000D0,shavep*1000D0,SINver,CO*1000d0,comminp*1000d0,&
                  comminn*1000d0,NPratio_max,pminp,pminn,pavep,paven,BINver,maxEVaLM,maxEVaTM,minEVaTM,pugh_max*1000D0,&
                  pughminp*1000D0,pughminn*1000D0,pughavep*1000D0,hardvar
                  
      IF (yn_veloc=='Y' .or. yn_veloc=='y') THEN
        WRITE(55,*) VVP_P/1000D0,VVG_P/1000D0,VVP_Sf/1000D0,VVG_Sf/1000D0,VVP_Ss/1000D0,VVG_Ss/1000D0,VV_P_PF,VV_Sf_PF,VV_Ss_PF,km
      ENDIF
      
      
      WRITE(111,*) theta, phi
      WRITE(115,"(3F30.15)")vec(1),vec(2),vec(3)
      
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! tests
WRITE(30,"(9F30.15)") vec(1)*ABS( bulk_m2 ),       vec(2)*ABS( bulk_m2    ),    vec(3)*ABS( bulk_m2 )   ,&     
                      vec(1)*ABS( Min_bulkm2 ),    vec(2)*ABS( Min_bulkm2 ),    vec(3)*ABS( Min_bulkm2 ),&    
                      vec(1)*ABS( Max_bulkm2 ),    vec(2)*ABS( Max_bulkm2 ),    vec(3)*ABS( Max_bulkm2 ) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      
      WRITE(28,"(4F30.15)")  vec(1)*ABS(   hardvar   ),    vec(2)*ABS(   hardvar   ),    vec(3)*ABS(   hardvar   )                     !>> hardness
      !WRITE(18,*) vec(1)*ABS( G_Ave*1000D0 ),    vec(2)*ABS( G_Ave*1000D0 ),    vec(3)*ABS( G_Ave*1000D0 )                !>> shear
      WRITE(16,"(12F30.15)") vec(1)*ABS( G_max*1000D0 ),    vec(2)*ABS( G_max*1000D0 ),    vec(3)*ABS( G_max*1000D0 ),&    !>> shear
                             vec(1)*ABS(shminp*1000D0 ),    vec(2)*ABS( shminp*1000D0),    vec(3)*ABS(shminp*1000D0 ),&    !>> shear
                             vec(1)*ABS(shminn*1000D0 ),    vec(2)*ABS( shminn*1000D0),    vec(3)*ABS(shminn*1000D0 ),&    !>> shear
                             vec(1)*ABS(shavep*1000D0 ),    vec(2)*ABS( shavep*1000D0),    vec(3)*ABS(shavep*1000D0 )      !>> shear
      !WRITE(18,*) vec(1)*ABS(shaven*1000D0 ),    vec(2)*ABS( shaven*1000D0),    vec(3)*ABS(shaven*1000D0 )   !>> shear
	
      WRITE(65,"(12F30.15)") vec(1)*ABS( pugh_max*1000D0  ),    vec(2)*ABS( pugh_max*1000D0  ),     vec(3)*ABS( pugh_max*1000D0  ),&    !>> pugh
                             vec(1)*ABS( pughminp*1000D0  ),    vec(2)*ABS( pughminp*1000D0  ),     vec(3)*ABS( pughminp*1000D0  ),&    !>> pugh
                             vec(1)*ABS( pughminn*1000D0  ),    vec(2)*ABS( pughminn*1000D0  ),     vec(3)*ABS( pughminn*1000D0  ),&    !>> pugh
                             vec(1)*ABS( pughavep*1000D0  ),    vec(2)*ABS( pughavep*1000D0  ),     vec(3)*ABS( pughavep*1000D0  )      !>> pugh
   
      WRITE(15,"(3F30.15)") vec(1)*ABS(    SINver        ),    vec(2)*ABS(    SINver        ),    vec(3)*ABS(   SINver          )    !>> young
	   
	   
      WRITE(14,"(9F30.15)")  vec(1)*ABS(  CO*1000d0   ),    vec(2)*ABS(  CO*1000d0   ),    vec(3)*ABS(  CO*1000d0   ),&    !>> compress
                             vec(1)*ABS(comminp*1000d0),    vec(2)*ABS(comminp*1000d0),    vec(3)*ABS(comminp*1000d0),&    !>> compress
                             vec(1)*ABS(comminn*1000d0),    vec(2)*ABS(comminn*1000d0),    vec(3)*ABS(comminn*1000d0)      !>> compress
	   
      WRITE(20,"(15F30.15)") vec(1)*ABS( NPratio_max  ),    vec(2)*ABS( NPratio_max  ),    vec(3)*ABS(  NPratio_max ),&    !>> poisson
                             vec(1)*ABS(    pminp     ),    vec(2)*ABS(    pminp     ),    vec(3)*ABS(    pminp     ),&    !>> poisson
                             vec(1)*ABS(    pminn     ),    vec(2)*ABS(    pminn     ),    vec(3)*ABS(    pminn     ),&    !>> poisson
                             vec(1)*ABS(    pavep     ),    vec(2)*ABS(    pavep     ),    vec(3)*ABS(    pavep     ),&    !>> poisson
                             vec(1)*ABS(    paven     ),    vec(2)*ABS(    paven     ),    vec(3)*ABS(    paven     )      !>> poisson
	   
      ! WRITE(30,*) vec(1)*ABS(   Minbulk    ),    vec(2)*ABS(   Minbulk    ),    vec(3)*ABS(   Minbulk    )         !>> bulk
      WRITE(31,"(3F30.15)") vec(1)*ABS(   BINver    ),    vec(2)*ABS(   BINver    ),    vec(3)*ABS(   BINver    )    !>> bulk
	   
      WRITE(32,"(9F30.15)") vec(1)*ABS(   maxEVaLM   ),    vec(2)*ABS(  maxEVaLM    ),    vec(3)*ABS(   maxEVaLM   ),&    !>> sound
                            vec(1)*ABS(   maxEVaTM   ),    vec(2)*ABS(  maxEVaTM    ),    vec(3)*ABS(   maxEVaTM   ),&    !>> sound
                            vec(1)*ABS(   minEVaTM   ),    vec(2)*ABS(  minEVaTM    ),    vec(3)*ABS(   minEVaTM   )      !>> sound

      IF(yn_veloc=='Y' .or. yn_veloc=='y') THEN
        WRITE(100,*) vec(1)*ABS(    VVP_P   ),    vec(2)*ABS(   VVP_P     ) ,vec(3)*ABS(   VVP_P    )   !>>  VVP_P
        WRITE(101,*) vec(1)*ABS(    VVG_P   ),    vec(2)*ABS(   VVG_P     ) ,vec(3)*ABS(   VVG_P    )   !>>  VVG_P
        WRITE(102,*) vec(1)*ABS(    VV_P_PF ),    vec(2)*ABS(   VV_P_PF   ) ,vec(3)*ABS(   VV_P_PF  )   !>>  VV_P_PF
        WRITE(103,*) vec(1)*ABS(    VVG_Sf  ),    vec(2)*ABS(   VVG_Sf    ) ,vec(3)*ABS(   VVG_Sf   )   !>>  VVG_Sf
        WRITE(104,*) vec(1)*ABS(    VVP_Sf  ),    vec(2)*ABS(   VVP_Sf    ) ,vec(3)*ABS(   VVP_Sf   )   !>>  VVP_Sf
        WRITE(105,*) vec(1)*ABS(    VV_Sf_PF),    vec(2)*ABS(   VV_Sf_PF  ) ,vec(3)*ABS(   VV_Sf_PF )   !>>  VV_Sf_PF
        WRITE(106,*) vec(1)*ABS(    VVG_Ss  ),    vec(2)*ABS(   VVG_Ss    ) ,vec(3)*ABS(   VVG_Ss   )   !>>  VVG_Ss
        WRITE(107,*) vec(1)*ABS(    VVP_Ss  ),    vec(2)*ABS(   VVP_Ss    ) ,vec(3)*ABS(   VVP_Ss   )   !>>  VVP_Ss
        WRITE(108,*) vec(1)*ABS(  VV_Ss_PF  ),    vec(2)*ABS(   VV_Ss_PF  ) ,vec(3)*ABS(  VV_Ss_PF  )   !>>  VVP_Ss_PF
      ENDIF
      
      IF(yn_km=='Y' .or. yn_km=='y') THEN
        WRITE(110,*) vec(1)*ABS(    km   ),    vec(2)*ABS(   km     ) ,vec(3)*ABS(   km    )     !>>  k_m
      ENDIF
      
    ENDDO 
     
    WRITE(16,*) " "
    !WRITE(111,*) " "
    !WRITE( 8,*) " "
    !WRITE( 7,*) " "

    WRITE(65,*) " "
    !WRITE(66,*) " "
    !WRITE(67,*) " "
    !WRITE(68,*) " "
    !WRITE(18,*) " "
    WRITE(15,*) " "
    WRITE(14,*) " "
    WRITE(28,*) " "
    !WRITE(29,*) " "
    WRITE(20,*) " "
    !WRITE(21,*) " "
    !WRITE(22,*) " "
    !WRITE(23,*) " "
    !WRITE(24,*) " "
    !WRITE(30,*) " "
    WRITE(31,*) " "
    WRITE(32,*) " "
    !WRITE(33,*) " "
    !WRITE(34,*) " "
    IF (yn_veloc=='Y' .or. yn_veloc=='y') THEN
      WRITE(100,*) " "
    ENDIF
  ENDDO
  CALL system('sleep 0.2; clear')
  CLOSE (15)
  CLOSE (16)
  CLOSE (111)
  CLOSE (18)
  CLOSE (20)
  !CLOSE(21)
  !CLOSE(22)
  !CLOSE(23)
  !CLOSE(24)
  !CLOSE(7 )
  !CLOSE(8 )
  CLOSE (30)
  CLOSE (31)
  CLOSE (32)
  !CLOSE(33)

  CLOSE (100)
  CLOSE (101)
  CLOSE (102)
  CLOSE (103)
  CLOSE (104)
  CLOSE (105)
  CLOSE (106)
  CLOSE (107)
  CLOSE (108)
  CLOSE (110)
  CLOSE (41)

  CLOSE (59)
  !CALL("touch .MaMiout2")
 ! IF (yn_veloc=='Y' .or. yn_veloc=='y') THEN
    OPEN(37,file='.MaMiout2')
    WRITE(37,*) VVP_P_max/1000D0,VVP_P_min/1000D0,VVP_Sf_max/1000D0,VVP_Sf_min/1000D0,VVP_Ss_max/1000D0,VVP_Ss_min/1000D0,VVG_P_max/1000D0,&
                VVG_P_min/1000D0,VVG_Sf_max/1000D0,VVG_Sf_min/1000D0,VVG_Ss_max/1000D0,VVG_Ss_min/1000D0,VV_P_PF_max,VV_Sf_PF_max,VV_Ss_PF_max,&
                VV_P_PF_min,VV_Sf_PF_min,VV_Ss_PF_min,km_max,km_min
    CLOSE (37)
 ! ENDIF
  OPEN(36,file='.MaMiout')
  WRITE(36,*)Maxyoung,Minyoung,Maxcomp,Mincomp,G_max2*1000D0,G_min2*1000D0,Maxbulk,Minbulk,&
             Pratio_max,Pratio_min,maxEVaTMf,maxEVaLM,minEVaTMf,pugh_max2*1000D0,pugh_min2*1000D0, Ha_max2,Ha_min2
  CLOSE(36)

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  Preparing features...
  WRITE(*,*)''
  CALL anisotropy(Maxyoung,Minyoung, Ax(1))
	CALL SYSTEM('tput setaf 196;tput bold; echo " ==================================================> Youngs Modulus"')
	CALL SYSTEM('tput setaf 196;tput bold; echo "  Max(GPa)             Min(GPa)  Anisotropy"')
	WRITE (*,'(3xF6.2,a,2xF6.2,a,2xF6.2)') Maxyoung,"             ",Minyoung," ",Ax(1)
	CALL angl2cart(Maxyoung_theta,Maxyoung_phi, vec(1),vec(2),vec(3))
	CALL angl2cart(Minyoung_theta,Minyoung_phi, vec1(1),vec1(2),vec1(3))
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "  Theta   Phi","          Theta   Phi"
	WRITE (*,'(2xF6.1,1xF6.1,8xF6.1,1xF6.1)') (Maxyoung_theta*180.0D0)/PI,(Maxyoung_phi*180.0D0)/PI,(Minyoung_theta*180.0D0)/PI,(Minyoung_phi*180.0D0)/PI
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "   x     y     z","        x     y     z"
	WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	CALL SYSTEM('tput setaf 196;tput bold; echo " ==================================================";tput sgr0')
	CALL SYSTEM('sleep 0.5')
	!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  WRITE(*,*)''
  CALL anisotropy(Maxcomp,Mincomp, Ax(1))
	CALL SYSTEM('tput setaf 202;tput bold; echo " ==================================================> Linear Compressibiliy";tput sgr0')
	CALL SYSTEM('tput setaf 202;tput bold; echo "  Max(TPa-1)           Min(TPa-1)  Anisotropy"')
	WRITE (*,'(1xF9.3,a,F9.3,a,2xF6.3)') Maxcomp,"             ",Mincomp," ",Ax(1)
	CALL angl2cart(Maxcomp_theta,Maxcomp_phi, vec(1),vec(2),vec(3))
	CALL angl2cart(Mincomp_theta,Mincomp_phi,vec1(1),vec1(2),vec1(3))
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "  Theta   Phi","          Theta   Phi"
	WRITE (*,'(1x2F6.1,11x2F6.1)') (Maxcomp_theta*180.0D0)/PI,(Maxcomp_phi*180.0D0)/PI,(Mincomp_theta*180.0D0)/PI,(Mincomp_phi*180.0D0)/PI
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "   x     y     z","        x     y     z"
	WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	WRITE (*,*) "=================================================="
	CALL SYSTEM('sleep 0.5')
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  WRITE(*,*)''
  CALL anisotropy(G_max2,G_min2, Ax(1))
	CALL SYSTEM('tput setaf 161;tput bold; echo " ==================================================> Shear Modulus";tput sgr0')
	CALL SYSTEM('tput setaf 161;tput bold; echo "  Max(GPa)             Min(GPa)  Anisotropy"')
	WRITE (*,'(3xF6.2,a,2xF6.2,a,2xF6.2)') G_max2*1000D0,"             ",G_min2*1000D0," ",Ax(1)
	CALL angl2cart(G_max2_theta,G_max2_phi, vec(1),vec(2),vec(3))
	CALL angl2cart(G_min2_theta,G_min2_phi,vec1(1),vec1(2),vec1(3))
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "  Theta   Phi","          Theta   Phi"
	WRITE (*,'(1x2F6.1,11x2F6.1)') (G_max2_theta*180.0D0)/PI,(G_max2_phi*180.0D0)/PI,(G_min2_theta*180.0D0)/PI,(G_min2_phi*180.0D0)/PI
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "   x     y     z","        x     y     z"
	WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	WRITE (*,*) "=================================================="
	CALL SYSTEM('sleep 0.5')
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  WRITE(*,*)''
  CALL anisotropy(Maxbulk,Minbulk, Ax(1))
	CALL SYSTEM('tput setaf 126;tput bold; echo " ==================================================> Bulk Modulus method 1 (Experimental)";tput sgr0')
	CALL SYSTEM('tput setaf 126;tput bold; echo "   Max(GPa/100)              Min(GPa/100)  Anisotropy"')
	WRITE (*,'(2xF11.2,a,1xF11.2,a,2xF6.2)') Maxbulk/100.d0,"            ",Minbulk/100.d0," ",Ax(1)
	CALL angl2cart(Maxbulk_theta,Maxbulk_phi, vec(1),vec(2),vec(3))
	CALL angl2cart(Minbulk_theta,Minbulk_phi,vec1(1),vec1(2),vec1(3))
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "  Theta   Phi","          Theta   Phi"
	WRITE (*,'(1x2F6.1,11x2F6.1)') (Maxbulk_theta*180.0D0)/PI,(Maxbulk_phi*180.0D0)/PI,(Minbulk_theta*180.0D0)/PI,(Minbulk_phi*180.0D0)/PI
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "   x     y     z","        x     y     z"
	WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
  WRITE (*,*) "=================================================="
  CALL SYSTEM('sleep 0.5')
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  WRITE(*,*)''
  CALL anisotropy(Max_bulkm2,Min_bulkm2, Ax(1))
	CALL SYSTEM('tput setaf 126;tput bold; echo " ==================================================> Bulk Modulus method 2 (Experimental)";tput sgr0')
	CALL SYSTEM('tput setaf 126;tput bold; echo "   Max(GPa/100)              Min(GPa/100)  Anisotropy"')
	WRITE (*,'(2xF11.2,a,1xF11.2,a,2xF6.2)') Max_bulkm2/100.d0,"            ",Min_bulkm2/100.d0," ",Ax(1)
	CALL angl2cart(Maxbulkm2_theta,Maxbulkm2_phi, vec(1), vec(2),vec(3))
	CALL angl2cart(Minbulkm2_theta,Minbulkm2_phi, vec1(1),vec1(2),vec1(3))
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "  Theta   Phi","          Theta   Phi"
	WRITE (*,'(1x2F6.1,11x2F6.1)') (Maxbulkm2_theta*180.0D0)/PI,(Maxbulkm2_phi*180.0D0)/PI,(Minbulkm2_theta*180.0D0)/PI,(Minbulkm2_phi*180.0D0)/PI
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "   x     y     z","        x     y     z"
	WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
  WRITE (*,*) "=================================================="
  CALL SYSTEM('sleep 0.5')
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  IF (yn_veloc=='Y' .or. yn_veloc=='y') THEN
    WRITE(*,*)''
    CALL anisotropy(VVP_P_max,VVP_P_min, Ax(1))
	  CALL SYSTEM('tput setaf 21;tput bold; echo " ==================================================> Phase P-Mode";tput sgr0')
    CALL SYSTEM('tput setaf 21;tput bold; echo "   Max(km/a)              Min(km/s)  Anisotropy"')
    WRITE (*,'(2xF11.3,a,1xF11.3,a,2xF6.2)') VVP_P_max/1000D0,"            ",VVP_P_min/1000D0," ",Ax(1)
    CALL angl2cart(VVP_P_max_theta,VVP_P_max_phi, vec(1),vec(2),vec(3))
    CALL angl2cart(VVP_P_min_theta,VVP_P_min_phi,vec1(1),vec1(2),vec1(3))
    WRITE(*,*)"------------------------------------------"
    WRITE (*,*) "  Theta   Phi","          Theta   Phi"
    WRITE (*,'(1x2F6.1,11x2F6.1)')  VVP_P_max_theta , VVP_P_max_phi , VVP_P_min_theta , VVP_P_min_phi 
    WRITE(*,*)"------------------------------------------"
    WRITE (*,*) "   x     y     z","        x     y     z"
    WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
    WRITE (*,*) "=================================================="
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    CALL SYSTEM('sleep 0.5')
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    WRITE(*,*)''
    CALL anisotropy(VVP_Sf_max,VVP_Sf_min, Ax(1))
    CALL SYSTEM('tput setaf 33;tput bold; echo " ==================================================> Phase Fast-Mode";tput sgr0')
    CALL SYSTEM('tput setaf 33;tput bold; echo "   Max(km/a)              Min(km/s)  Anisotropy"')
    WRITE (*,'(2xF11.3,a,1xF11.3,a,2xF6.2)') VVP_Sf_max/1000D0,"            ",VVP_Sf_min/1000D0," ",Ax(1)
    CALL angl2cart(VVP_Sf_max_theta,VVP_Sf_max_phi, vec(1),vec(2),vec(3))
    CALL angl2cart(VVP_Sf_min_theta,VVP_Sf_min_phi,vec1(1),vec1(2),vec1(3))
    WRITE(*,*)"------------------------------------------"
    WRITE (*,*) "  Theta   Phi","          Theta   Phi"
    WRITE (*,'(1x2F6.1,11x2F6.1)')  VVP_Sf_max_theta , VVP_Sf_max_phi , VVP_Sf_min_theta , VVP_Sf_min_phi 
    WRITE(*,*)"------------------------------------------"
    WRITE (*,*) "   x     y     z","        x     y     z"
    WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
    WRITE (*,*) "=================================================="
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    CALL SYSTEM('sleep 0.5')
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    WRITE(*,*)''
    CALL anisotropy(VVP_Ss_max,VVP_Ss_min, Ax(1))
    CALL SYSTEM('tput setaf 34;tput bold; echo " ==================================================> Phase Slow-Mode";tput sgr0')
    CALL SYSTEM('tput setaf 34;tput bold; echo "   Max(km/a)              Min(km/s)  Anisotropy"')
    WRITE (*,'(2xF11.3,a,1xF11.3,a,2xF6.2)') VVP_Ss_max/1000D0,"            ",VVP_Ss_min/1000D0," ",Ax(1)
    CALL angl2cart(VVP_Ss_max_theta,VVP_Ss_max_phi, vec(1),vec(2),vec(3))
    CALL angl2cart(VVP_Ss_min_theta,VVP_Ss_min_phi,vec1(1),vec1(2),vec1(3))
  	WRITE(*,*)"------------------------------------------"
   	WRITE (*,*) "  Theta   Phi","          Theta   Phi"
	  WRITE (*,'(1x2F6.1,11x2F6.1)')  VVP_Ss_max_theta , VVP_Ss_max_phi , VVP_Ss_min_theta , VVP_Ss_min_phi 
	  WRITE(*,*)"------------------------------------------"
	  WRITE (*,*) "   x     y     z","        x     y     z"
	  WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	  WRITE (*,*) "=================================================="
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    CALL SYSTEM('sleep 0.5')
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    WRITE(*,*)''
    CALL anisotropy(VVG_P_max,VVG_P_min, Ax(1))
	  CALL SYSTEM('tput setaf 40;tput bold; echo " ==================================================> Group P-Mode";tput sgr0')
	  CALL SYSTEM('tput setaf 40;tput bold; echo "   Max(km/a)              Min(km/s)  Anisotropy"')
	  WRITE (*,'(2xF11.3,a,1xF11.3,a,2xF6.2)') VVG_P_max/1000D0,"            ",VVG_P_min/1000D0," ",Ax(1)
	  CALL angl2cart(VVG_P_max_theta,VVG_P_max_phi, vec(1),vec(2),vec(3))
	  CALL angl2cart(VVG_P_min_theta,VVG_P_min_phi,vec1(1),vec1(2),vec1(3))
	  WRITE(*,*)"------------------------------------------"
	  WRITE (*,*) "  Theta   Phi","          Theta   Phi"
	  WRITE (*,'(1x2F6.1,11x2F6.1)')  VVG_P_max_theta , VVG_P_max_phi , VVG_P_min_theta , VVG_P_min_phi 
	  WRITE(*,*)"------------------------------------------"
	  WRITE (*,*) "   x     y     z","        x     y     z"
	  WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	  WRITE (*,*) "=================================================="
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    CALL SYSTEM('sleep 0.5')
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    WRITE(*,*)''
    CALL anisotropy(VVG_Sf_max,VVG_Sf_min, Ax(1))
	  CALL SYSTEM('tput setaf 226;tput bold; echo " ==================================================> Group Fast-Mode";tput sgr0')
	  CALL SYSTEM('tput setaf 226;tput bold; echo "   Max(km/a)              Min(km/s)  Anisotropy"')
	  WRITE (*,'(2xF11.3,a,1xF11.3,a,2xF6.2)') VVG_Sf_max/1000D0,"            ",VVG_Sf_min/1000D0," ",Ax(1)
	  CALL angl2cart(VVG_Sf_max_theta,VVG_Sf_max_phi, vec(1),vec(2),vec(3))
	  CALL angl2cart(VVG_Sf_min_theta,VVG_Sf_min_phi,vec1(1),vec1(2),vec1(3))
	  WRITE(*,*)"------------------------------------------"
	  WRITE (*,*) "  Theta   Phi","          Theta   Phi"
	  WRITE (*,'(1x2F6.1,11x2F6.1)')  VVG_Sf_max_theta , VVG_Sf_max_phi , VVG_Sf_min_theta , VVG_Sf_min_phi 
	  WRITE(*,*)"------------------------------------------"
	  WRITE (*,*) "   x     y     z","        x     y     z"
	  WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	  WRITE (*,*) "=================================================="
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    CALL SYSTEM('sleep 0.5')
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    WRITE(*,*)''
    CALL anisotropy(VVG_Ss_max,VVG_Ss_min, Ax(1))
	  CALL SYSTEM('tput setaf 22;tput bold; echo " ==================================================> Group Slow-Mode";tput sgr0')
	  CALL SYSTEM('tput setaf 22;tput bold; echo "   Max(km/a)              Min(km/s)  Anisotropy"')
  	WRITE (*,'(2xF11.3,a,1xF11.3,a,2xF6.2)') VVG_Ss_max/1000D0,"            ",VVG_Ss_min/1000D0," ",Ax(1)
	  CALL angl2cart(VVG_Ss_max_theta,VVG_Ss_max_phi, vec(1),vec(2),vec(3))
  	CALL angl2cart(VVG_Ss_min_theta,VVG_Ss_min_phi,vec1(1),vec1(2),vec1(3))
	  WRITE(*,*)"------------------------------------------"
  	WRITE (*,*) "  Theta   Phi","          Theta   Phi"
	  WRITE (*,'(1x2F6.1,11x2F6.1)')  VVG_Ss_max_theta , VVG_Ss_max_phi , VVG_Ss_min_theta , VVG_Ss_min_phi 
  	WRITE(*,*)"------------------------------------------"
	  WRITE (*,*) "   x     y     z","        x     y     z"
  	WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	  WRITE (*,*) "=================================================="
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      CALL SYSTEM('sleep 0.5')
  !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  IF (yn_km=='Y' .or. yn_km=='y') THEN
    WRITE(*,*)''
     CALL anisotropy(km_max,km_min, Ax(1))
	  CALL SYSTEM('tput setaf 12;tput bold; echo " ==================================================> Min. thermal conductivity";tput sgr0')
    CALL SYSTEM('tput setaf 12;tput bold; echo "   Max(W/K.m)              Min(W/K.m)        Anisotropy"')
    WRITE (*,'(2xF11.3,a,1xF11.3,a,2xF6.2)') km_max,"            ",km_min ,"           ",Ax(1) 
    CALL angl2cart(km_max_theta,km_max_phi, vec(1),vec(2),vec(3))
    CALL angl2cart(km_min_theta,km_min_phi,vec1(1),vec1(2),vec1(3))
    WRITE(*,*)"------------------------------------------"
    WRITE (*,*) "  Theta   Phi","          Theta   Phi"
    WRITE (*,'(1x2F6.1,11x2F6.1)')  km_max_theta , km_max_phi , km_min_theta , km_min_phi 
    WRITE(*,*)"------------------------------------------"
    WRITE (*,*) "   x     y     z","        x     y     z"
    WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
   	  WRITE (*,*) "=================================================="   
  ENDIF
  !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    CALL SYSTEM('sleep 0.5')
  END IF
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  WRITE(*,*)''
  CALL anisotropy(Pratio_max,Pratio_min, Ax(1))
  CALL SYSTEM('tput setaf 201;tput bold; echo " ==================================================> Poissons Ratio";tput sgr0')
	CALL SYSTEM('tput setaf 201;tput bold; echo "     Max                  Min  Anisotropy"')
	WRITE (*,'(2xF6.3,a,2xF6.3,a,2xF6.2)') Pratio_max,"             ",Pratio_min," ",Ax(1)
	CALL angl2cart(Pratio_max_theta,Pratio_max_phi, vec(1),vec(2),vec(3))
	CALL angl2cart(Pratio_min_theta,Pratio_min_phi, vec1(1),vec1(2),vec1(3))
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "  Theta   Phi","          Theta   Phi"
	WRITE (*,'(1x2F6.1,11x2F6.1)') (Pratio_max_theta*180.0D0)/PI,(Pratio_max_phi*180.0D0)/PI,(Pratio_min_theta*180.0D0)/PI,(Pratio_min_phi*180.0D0)/PI
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "   x     y     z","        x     y     z"
	WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	WRITE (*,*) "=================================================="
	CALL SYSTEM('sleep 0.5')
  WRITE(*,*)''
  CALL anisotropy(pugh_max2,pugh_min2, Ax(1))
       !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
	CALL SYSTEM('tput setaf 91;tput bold; echo " ==================================================> Pugh Ratio";tput sgr0')
	CALL SYSTEM('tput setaf 91;tput bold; echo "     Max                  Min  Anisotropy"')
	WRITE (*,'(3xF6.3,a,2xF6.3,a,2xF6.2)') pugh_max2*1000D0,"             ",pugh_min2*1000D0," ",Ax(1)
	CALL angl2cart(pugh_max2_theta,pugh_max2_phi, vec(1),vec(2),vec(3))
	CALL angl2cart(pugh_min2_theta,pugh_min2_phi,vec1(1),vec1(2),vec1(3))
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "  Theta   Phi","          Theta   Phi"
	WRITE (*,'(1x2F6.1,11x2F6.1)') (pugh_max2_theta*180.0D0)/PI,(pugh_max2_phi*180.0D0)/PI,(pugh_min2_theta*180.0D0)/PI,(pugh_min2_phi*180.0D0)/PI
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "   x     y     z","        x     y     z"
	WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	WRITE (*,*) "=================================================="
	CALL SYSTEM('sleep 0.5')
	WRITE(*,*)''
  	!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  WRITE(*,*)''
  CALL anisotropy(Maxbulk,Minbulk, Ax(1))
	CALL SYSTEM('tput setaf 55;tput bold; echo " ==================================================> Hardness (Experimental)";tput sgr0')
	CALL SYSTEM('tput setaf 55;tput bold; echo "         Max                     Min  Anisotropy"')
	WRITE (*,'(2xF10.3,a,1xF10.3,a,2xF6.2)') Ha_max2,"            ",Ha_min2," ",Ax(1)
	CALL angl2cart(Ha_max2_theta,Ha_max2_phi, vec(1),vec(2),vec(3))
	CALL angl2cart(Ha_min2_theta,Ha_min2_phi,vec1(1),vec1(2),vec1(3))
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "  Theta   Phi","          Theta   Phi"
	WRITE (*,'(1x2F6.1,11x2F6.1)') (Ha_max2_theta*180.0D0)/PI,(Ha_max2_phi*180.0D0)/PI,(Ha_min2_theta*180.0D0)/PI,(Ha_min2_phi*180.0D0)/PI
	WRITE(*,*)"------------------------------------------"
	WRITE (*,*) "   x     y     z","        x     y     z"
	WRITE (*,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
  WRITE (*,*) "=================================================="
  CALL SYSTEM('sleep 0.5')
  WRITE(*,*)''
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
	CALL SYSTEM('tput setaf 56;tput bold; echo " ==================================================> Sound < testing";tput sgr0')
	CALL SYSTEM('tput setaf 56;tput bold; echo "  Transverse high  Longitudinal   Transverse low "')
	WRITE (*,'(3xF6.2,a,2xF6.2,a,2xF6.2)') maxEVaTMf,"         ",maxEVaLM,"         ",minEVaTMf

	CALL SYSTEM('tput setaf 56;tput bold; echo " ==================================================";tput sgr0')
	WRITE(*,*)' '

!***************************************************************
  WRITE(*,*)" > NOTE :The above information is stored in  'DATA.out' file."
  WRITE (99,'(a,F4.1,a,F4.1,a,F4.1,a)') " > Output for  (",mmx,",",kky,",",llz,") plane"
  IF (Ncod .EQ. 6) THEN
    WRITE (99,'(2a)') " > MP ID:", myid
  ENDIF
  !WRITE (99,*) e1,e2,"plane"
  WRITE (99,*) ''
  CALL anisotropy(Maxyoung,Minyoung, Ax(1))
	WRITE (99,*) " =================================================> Youngs Modulus"
	WRITE (99,*) "  Max(GPa)             Min(GPa)          Anisotropy"
	WRITE (99,'(3xF6.2,a,2xF6.2,a,2xF6.2)') Maxyoung,"             ",Minyoung,"           ",Ax(1)
	CALL angl2cart(Maxyoung_theta,Maxyoung_phi, vec(1),vec(2),vec(3))
	CALL angl2cart(Minyoung_theta,Minyoung_phi, vec1(1),vec1(2),vec1(3))
	WRITE(99,*)"------------------------------------------"
	WRITE (99,*) "  Theta   Phi","          Theta   Phi"
	WRITE (99,'(2xF6.1,1xF6.1,8xF6.1,1xF6.1)') (Maxyoung_theta*180.0D0)/PI,(Maxyoung_phi*180.0D0)/PI,(Minyoung_theta*180.0D0)/PI,(Minyoung_phi*180.0D0)/PI
	WRITE(99,*)"------------------------------------------"
	WRITE (99,*) "   x     y     z","        x     y     z"
	WRITE (99,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	WRITE (99,*) "=================================================="

	!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  WRITE(99,*)' '
  CALL anisotropy(Maxcomp,Mincomp, Ax(1))
	WRITE(99,*) " ==================================================> Linear Compressibiliy"
	WRITE(99,*) "  Max(TPa-1)           Min(TPa-1)           Anisotropy"
	WRITE (99,'(1xF9.3,a,F9.3,a,2xF6.3)') Maxcomp,"             ",Mincomp,"           ",Ax(1)
	CALL angl2cart(Maxcomp_theta,Maxcomp_phi, vec(1),vec(2),vec(3))
	CALL angl2cart(Mincomp_theta,Mincomp_phi,vec1(1),vec1(2),vec1(3))
	WRITE(99,*)"------------------------------------------"
	WRITE(99,*) "  Theta   Phi","          Theta   Phi"
	WRITE(99,'(1x2F6.1,11x2F6.1)') (Maxcomp_theta*180.0D0)/PI,(Maxcomp_phi*180.0D0)/PI,(Mincomp_theta*180.0D0)/PI,(Mincomp_phi*180.0D0)/PI
	WRITE(99,*)"------------------------------------------"
	WRITE(99,*) "   x     y     z","        x     y     z"
	WRITE(99,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	WRITE(99,*) "=================================================="

	!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  WRITE(99,*)' '
  CALL anisotropy(G_max2,G_min2, Ax(1))
	WRITE(99,*) " ==================================================> Shear Modulus"
	WRITE(99,*) "  Max(GPa)             Min(GPa)           Anisotropy"
  WRITE (99,'(3xF6.2,a,2xF6.2,a,2xF6.2)') G_max2*1000D0,"             ",G_min2*1000D0,"           ",Ax(1)
	CALL angl2cart(G_max2_theta,G_max2_phi, vec(1),vec(2),vec(3))
	CALL angl2cart(G_min2_theta,G_min2_phi,vec1(1),vec1(2),vec1(3))
	WRITE(99,*)"------------------------------------------"
	WRITE(99,*) "  Theta   Phi","          Theta   Phi"
	WRITE(99,'(1x2F6.1,11x2F6.1)') (G_max2_theta*180.0D0)/PI,(G_max2_phi*180.0D0)/PI,(G_min2_theta*180.0D0)/PI,(G_min2_phi*180.0D0)/PI
	WRITE(99,*)"------------------------------------------"
	WRITE(99,*) "   x     y     z","        x     y     z"
	WRITE(99,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	WRITE(99,*) "=================================================="

	!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  WRITE(99,*)' '
  CALL anisotropy(Maxbulk,Minbulk, Ax(1))
	WRITE(99,*) " ==================================================> Bulk Modulus (Experimental)"
	WRITE(99,*) "   Max(GPa/100)              Min(GPa/100)           Anisotropy"
	WRITE (99,'(2xF11.2,a,1xF11.2,a,2xF6.2)') Maxbulk/100.d0,"            ",Minbulk/100.d0,"           ",Ax(1)
	CALL angl2cart(Maxbulk_theta,Maxbulk_phi, vec(1),vec(2),vec(3))
	CALL angl2cart(Minbulk_theta,Minbulk_phi,vec1(1),vec1(2),vec1(3))
	WRITE(99,*)"------------------------------------------"
	WRITE(99,*) "  Theta   Phi","          Theta   Phi"
	WRITE(99,'(1x2F6.1,11x2F6.1)') (Maxbulk_theta*180.0D0)/PI,(Maxbulk_phi*180.0D0)/PI,(Minbulk_theta*180.0D0)/PI,(Minbulk_phi*180.0D0)/PI
	WRITE(99,*)"------------------------------------------"
	WRITE(99,*) "   x     y     z","        x     y     z"
	WRITE(99,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	WRITE(99,*) "=================================================="
	!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
   !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  IF (yn_veloc=='Y' .or. yn_veloc=='y') THEN
    WRITE(99,*)' '
    CALL anisotropy(VVP_P_max,VVP_P_min, Ax(1))
	  WRITE(99,*) " ==================================================> Phase P-Mode" 
	  WRITE(99,*) "   Max(km/s)              Min(km/s)           Anisotropy"
	  WRITE (99,'(2xF11.3,a,1xF11.3,a,2xF6.2)') VVP_P_max/1000D0,"            ",VVP_P_min/1000D0,"           ",Ax(1)
	  CALL angl2cart(VVP_P_max_theta,VVP_P_max_phi, vec(1),vec(2),vec(3))
	  CALL angl2cart(VVP_P_min_theta,VVP_P_min_phi,vec1(1),vec1(2),vec1(3))
  	WRITE(99,*)"------------------------------------------"
	  WRITE (99,*) "  Theta   Phi","          Theta   Phi"
	  WRITE (99,'(1x2F6.1,11x2F6.1)')  VVP_P_max_theta , VVP_P_max_phi , VVP_P_min_theta , VVP_P_min_phi 
	  WRITE(99,*)"------------------------------------------"
	  WRITE (99,*) "   x     y     z","        x     y     z"
	  WRITE (99,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	  WRITE (99,*) "=================================================="
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
   !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    WRITE(99,*)' '
    CALL anisotropy(VVP_Sf_max,VVP_Sf_min, Ax(1))
	  WRITE(99,*) " ==================================================> Phase Fast-Mode" 
    WRITE(99,*) "   Max(km/s)              Min(km/s)           Anisotropy"
    WRITE (99,'(2xF11.3,a,1xF11.3,a,2xF6.2)') VVP_Sf_max/1000D0,"            ",VVP_Sf_min/1000D0,"           ",Ax(1)
    CALL angl2cart(VVP_Sf_max_theta,VVP_Sf_max_phi, vec(1),vec(2),vec(3))
    CALL angl2cart(VVP_Sf_min_theta,VVP_Sf_min_phi,vec1(1),vec1(2),vec1(3))
    WRITE(99,*)"------------------------------------------"
    WRITE (99,*) "  Theta   Phi","          Theta   Phi"
    WRITE (99,'(1x2F6.1,11x2F6.1)')  VVP_Sf_max_theta , VVP_Sf_max_phi , VVP_Sf_min_theta , VVP_Sf_min_phi 
    WRITE(99,*)"------------------------------------------"
    WRITE (99,*) "   x     y     z","        x     y     z"
    WRITE (99,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
    WRITE (99,*) "=================================================="
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
   !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    WRITE(99,*)' '
    CALL anisotropy(VVP_Ss_max,VVP_Ss_min, Ax(1))
    WRITE(99,*) " ==================================================> Phase Slow-Mode" 
    WRITE(99,*) "   Max(km/s)              Min(km/s)           Anisotropy"
    WRITE (99,'(2xF11.3,a,1xF11.3,a,2xF6.2)') VVP_Ss_max/1000D0,"            ",VVP_Ss_min/1000D0,"           ",Ax(1)
    CALL angl2cart(VVP_Ss_max_theta,VVP_Ss_max_phi, vec(1),vec(2),vec(3))
    CALL angl2cart(VVP_Ss_min_theta,VVP_Ss_min_phi,vec1(1),vec1(2),vec1(3))
    WRITE(99,*)"------------------------------------------"
    WRITE (99,*) "  Theta   Phi","          Theta   Phi"
    WRITE (99,'(1x2F6.1,11x2F6.1)')  VVP_Ss_max_theta , VVP_Ss_max_phi , VVP_Ss_min_theta , VVP_Ss_min_phi 
    WRITE(99,*)"------------------------------------------"
    WRITE (99,*) "   x     y     z","        x     y     z"
    WRITE (99,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
    WRITE (99,*) "=================================================="
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
   !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    WRITE(99,*)' '
    CALL anisotropy(VVG_P_max,VVG_P_min, Ax(1))
    WRITE(99,*) " ==================================================> Group P-Mode" 
    WRITE(99,*) "   Max(km/s)              Min(km/s)           Anisotropy"
    WRITE (99,'(2xF11.3,a,1xF11.3,a,2xF6.2)') VVG_P_max/1000D0,"            ",VVG_P_min/1000D0,"           ",Ax(1) 
    CALL angl2cart(VVG_P_max_theta,VVG_P_max_phi, vec(1),vec(2),vec(3))
    CALL angl2cart(VVG_P_min_theta,VVG_P_min_phi,vec1(1),vec1(2),vec1(3))
    WRITE(99,*)"------------------------------------------"
    WRITE (99,*) "  Theta   Phi","          Theta   Phi"
    WRITE (99,'(1x2F6.1,11x2F6.1)')  VVG_P_max_theta , VVG_P_max_phi , VVG_P_min_theta , VVG_P_min_phi 
    WRITE(99,*)"------------------------------------------"
    WRITE (99,*) "   x     y     z","        x     y     z"
    WRITE (99,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
    WRITE (99,*) "=================================================="
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
   !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    WRITE(99,*)' '
    CALL anisotropy(VVG_Sf_max,VVG_Sf_min, Ax(1))
    WRITE(99,*) " ==================================================> Group Fast-Mode" 
    WRITE(99,*) "   Max(km/s)              Min(km/s)           Anisotropy"
    WRITE (99,'(2xF11.3,a,1xF11.3,a,2xF6.2)') VVG_Sf_max/1000D0,"            ",VVG_Sf_min/1000D0,"           ",Ax(1) 
    CALL angl2cart(VVG_Sf_max_theta,VVG_Sf_max_phi, vec(1),vec(2),vec(3))
    CALL angl2cart(VVG_Sf_min_theta,VVG_Sf_min_phi,vec1(1),vec1(2),vec1(3))
    WRITE(99,*)"------------------------------------------"
    WRITE (99,*) "  Theta   Phi","          Theta   Phi"
    WRITE (99,'(1x2F6.1,11x2F6.1)')  VVG_Sf_max_theta , VVG_Sf_max_phi , VVG_Sf_min_theta , VVG_Sf_min_phi 
    WRITE(99,*)"------------------------------------------"
    WRITE (99,*) "   x     y     z","        x     y     z"
    WRITE (99,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
    WRITE (99,*) "=================================================="
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
   !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    WRITE(99,*)' '
    CALL anisotropy(VVG_Ss_max,VVG_Ss_min, Ax(1))
    WRITE(99,*) " ==================================================> Group Slow-Mode" 
    WRITE(99,*) "   Max(km/s)              Min(km/s)           Anisotropy"
    WRITE (99,'(2xF11.3,a,1xF11.3,a,2xF6.2)') VVG_Ss_max/1000D0,"            ",VVG_Ss_min/1000D0,"           ",Ax(1) 
    CALL angl2cart(VVG_Ss_max_theta,VVG_Ss_max_phi, vec(1),vec(2),vec(3))
    CALL angl2cart(VVG_Ss_min_theta,VVG_Ss_min_phi,vec1(1),vec1(2),vec1(3))
    WRITE(99,*)"------------------------------------------"
    WRITE (99,*) "  Theta   Phi","          Theta   Phi"
    WRITE (99,'(1x2F6.1,11x2F6.1)')  VVG_Ss_max_theta , VVG_Ss_max_phi , VVG_Ss_min_theta , VVG_Ss_min_phi 
    WRITE(99,*)"------------------------------------------"
    WRITE (99,*) "   x     y     z","        x     y     z"
    WRITE (99,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
    WRITE (99,*) "=================================================="
  ENDIF
  !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  IF (yn_km=='Y' .or. yn_km=='y') THEN
    WRITE(*,*)''
    CALL anisotropy(km_max,km_min, Ax(1))
    WRITE(99,*) " ==================================================> Min. thermal conductivity" 
    WRITE(99,*) "   Max(W/K.m)              Min(W/K.m)        Anisotropy"
    WRITE (99,'(2xF11.3,a,1xF11.3,a,2xF6.2)') km_max,"            ",km_min ,"           ",Ax(1) 
    CALL angl2cart(km_max_theta,km_max_phi, vec(1),vec(2),vec(3))
    CALL angl2cart(km_min_theta,km_min_phi,vec1(1),vec1(2),vec1(3))
    WRITE(99,*)"------------------------------------------"
    WRITE (99,*) "  Theta   Phi","          Theta   Phi"
    WRITE (99,'(1x2F6.1,11x2F6.1)')  km_max_theta , km_max_phi , km_min_theta , km_min_phi 
    WRITE(99,*)"------------------------------------------"
    WRITE (99,*) "   x     y     z","        x     y     z"
    WRITE (99,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
  ENDIF
  !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  WRITE(99,*)''
  CALL anisotropy(Pratio_max,Pratio_min, Ax(1))
	WRITE(99,*) " ==================================================> Poisson's Ratio"
	WRITE(99,*) "     Max                  Min            Anisotropy"
	WRITE (99,'(2xF6.3,a,2xF6.3,a,2xF6.2)') Pratio_max,"             ",Pratio_min,"           ",Ax(1)
	CALL angl2cart(Pratio_max_theta,Pratio_max_phi, vec(1),vec(2),vec(3))
	CALL angl2cart(Pratio_min_theta,Pratio_min_phi, vec1(1),vec1(2),vec1(3))
	WRITE(99,*)"------------------------------------------"
	WRITE(99,*) "  Theta   Phi","          Theta   Phi"
	WRITE(99,'(1x2F6.1,11x2F6.1)') (Pratio_max_theta*180.0D0)/PI,(Pratio_max_phi*180.0D0)/PI,(Pratio_min_theta*180.0D0)/PI,(Pratio_min_phi*180.0D0)/PI
	WRITE(99,*)"------------------------------------------"
	WRITE(99,*) "   x     y     z","        x     y     z"
	WRITE(99,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
	WRITE(99,*) "=================================================="
 WRITE(99,*)''
  CALL anisotropy(Maxbulk,Minbulk, Ax(1))
	WRITE(99,*) " ==================================================> Hardness (Experimental)"
	WRITE(99,*) "       Max                     Min  Anisotropy" 
	WRITE (99,'(2xF10.3,a,1xF10.3,a,2xF6.2)') Ha_max2,"            ",Ha_min2," ",Ax(1)
	CALL angl2cart(Ha_max2_theta,Ha_max2_phi, vec(1),vec(2),vec(3))
	CALL angl2cart(Ha_min2_theta,Ha_min2_phi,vec1(1),vec1(2),vec1(3))
	WRITE(99,*)"------------------------------------------"
	WRITE (99,*) "  Theta   Phi","          Theta   Phi"
	WRITE (99,'(1x2F6.1,11x2F6.1)') (Ha_max2_theta*180.0D0)/PI,(Ha_max2_phi*180.0D0)/PI,(Ha_min2_theta*180.0D0)/PI,(Ha_min2_phi*180.0D0)/PI
	WRITE(99,*)"------------------------------------------"
	WRITE (99,*) "   x     y     z","        x     y     z"
	WRITE (99,'(1x3F6.2,3x3F6.2)') vec(1),vec(2),vec(3),vec1(1),vec1(2),vec1(3)
  WRITE (99,*) "=================================================="
  WRITE(99,*)''
	WRITE(99,*) " ==================================================> Sound"
	WRITE(99,*) "  Transverse high  Longitudinal   Transverse low "
	WRITE(99,'(3xF6.2,a,2xF6.2,a,2xF6.2)') maxEVaTMf,"         ",maxEVaLM,"         ",minEVaTMf
	WRITE (99,*) "=================================================="
	WRITE(*,*)''
!***************************************************************
!***************************************************************
!
!Maxcomp=0D0
!Mincomp=10d8
  num=0
  smkl=SQRT(mmx**2D0 +kky**2D0 +llz**2D0)
  mmx = mmx/smkl; kky = kky/smkl; llz = llz/smkl   
  vec=0D0
  DO i=0,Nmesh_thata
  
!                  out   out  out out  out  out  in  in  in  in  in out  out   in         out
    CALL twoD_calc(vv11,vv12,vv13,vv22,vv23,vv33,mmx,kky,llz,smkl,i,phi,theta,Nmesh_thata,vec)
   !WRITE(*,*)i,phi,theta
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     CALL Csound(vec,vv11,vv12,vv22,vv13,vv23,vv33,EVa,Lm,MaxTm,MinTm)
     LM2d(num)   = Eva(LM)
     TMmax2d(num)= EVa(MaxTm)
     TMmin2d(num)= EVa(MinTm)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     SS=1D0*vv11*vv11*S(1,1)                 &
       +2D0*vv12*vv12*S(1,2)                 &
       +2D0*vv13*vv13*S(1,3)                 &
       +2D0*vv12*vv13*S(1,4)                 &
       +2D0*vv11*vv13*S(1,5)                 &
       +2D0*vv11*vv12*S(1,6)+vv22*vv22*S(2,2)&  
       +2D0*vv23*vv23*S(2,3)                 &
       +2D0*vv22*vv23*S(2,4)                 &
       +2D0*vv12*vv23*S(2,5)                 &
       +2D0*vv12*vv22*S(2,6)+vv33*vv33*S(3,3)& 
       +2D0*vv23*vv33*S(3,4)                 &
       +2D0*vv13*vv33*S(3,5)                 &
       +2D0*vv13*vv23*S(3,6)+vv23*vv23*S(4,4)&
       +2D0*vv13*vv23*S(4,5)                 &
       +2D0*vv12*vv23*S(4,6)+vv13*vv13*S(5,5)&
       +2D0*vv12*vv13*S(5,6)+vv12*vv12*S(6,6)
       SINver=1D0/SS
       IF (SINver.NE.0) THEN
        young2dmax(num)=SINver
       ELSE
        young2dmax(num)=0D0
       ENDIF
 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      CO=      ( S(1,1)+S(1,2)+S(1,3) )*vv11 &
      +( S(1,6)+S(2,6)+S(3,6) )*vv12         &
      +( S(1,5)+S(2,5)+S(3,5) )*vv13         &
      +( S(1,2)+S(2,2)+S(2,3) )*vv22         &
      +( S(1,4)+S(2,4)+S(3,4) )*vv23         &
      +( S(1,3)+S(2,3)+S(3,3) )*vv33 
!
      IF (CO*1000D0 .GE.0.0d0) THEN
        !Maxcomp=
        comMAX2d(num)=CO*1000D0 !Maxcomp
      ENDIF
      !WRITE(*,*)CO*1000D0             
      IF (CO *1000D0 .LE. 0.0d0) THEN
       !Mincomp=
        comMIN2d(num)=CO*1000D0 !Mincomp
      ENDIF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      BB =  ( S(1,1)+S(1,2)+S(1,3) )*vv11 &
           +( S(1,6)+S(2,6)+S(3,6) )*vv12 &
           +( S(1,5)+S(2,5)+S(3,5) )*vv13 &
           +( S(1,2)+S(2,2)+S(2,3) )*vv22 &
           +( S(1,4)+S(2,4)+S(3,4) )*vv23 &
           +( S(1,3)+S(2,3)+S(3,3) )*vv33 
      BINver = 1D0/BB
      bulkMax2d(num) = BINver
 !     IF (BINver.GE.0D0) THEN
 !	Maxbulk       = BINver
 !          bulkMax2d(num) = Maxbulk 
 !    ENDIF 


      IF(BINver.LE.0D0) THEN
        Minbulk        = BINver                       
        bulkMin2d(num) = Maxbulk
      ENDIF

      ave = 0D0
        v = 0
      Nmesh_gamma=180
      DO kk=0,Nmesh_gamma-1      
        gamma=DBLE(kk)/DBLE(Nmesh_gamma)*pi
        v(1)= COS(theta)*COS(phi)*COS(gamma)-SIN(phi)*SIN(gamma)   
        v(2)= COS(theta)*SIN(phi)*COS(gamma)+COS(phi)*SIN(gamma)   
        v(3)=-SIN(theta)*COS(gamma) 
        v11=v(1)*v(1); v12=v(1)*v(2) 
        v13=v(1)*v(3); v22=v(2)*v(2)
        v23=v(2)*v(3); v33=v(3)*v(3)
 
        a6666=( 2*vv12*v12*S(1,2)+vv11*v11*S(1,1)+ &
             2*vv13*v13*S(1,3)+vv22*v22*S(2,2)+ &
             2*vv23*v23*S(2,3)+vv33*v33*S(3,3) )
             
     
        a6666=a6666+( vv12*v13+vv13*v12 )*S(1,4)              &
        +( vv11*v13+vv13*v11 )*S(1,5)                         &
        +( vv11*v12+vv12*v11 )*S(1,6)                         &
        +( vv22*v23+vv23*v22 )*S(2,4)                         &
        +( vv12*v23+vv23*v12 )*S(2,5)                         &
        +( vv22*v12+vv12*v22 )*S(2,6)                         &
        +( vv33*v23+vv23*v33 )*S(3,4)                         &
        +( vv33*v13+vv13*v33 )*S(3,5)                         &
        +( vv13*v23+vv23*v13 )*S(3,6)                         &
        +1D0/4D0*(vv22*v33+2D0*vv23*v23+vv33*v22)     *S(4,4) &
        +1D0/2D0*(vv12*v33+vv23*v13+vv13*v23+vv33*v12)*S(4,5) &
        +1D0/2D0*(vv12*v23+vv22*v13+vv13*v22+vv23*v12)*S(4,6) & 
        +1D0/4D0*(vv11*v33+2D0*vv13*v13+vv33*v11)     *S(5,5) &
        +1D0/2D0*(vv11*v23+vv12*v13+vv13*v12+vv23*v11)*S(5,6) & 
        +1D0/4D0*(vv11*v22+2D0*vv12*v12+vv22*v11)     *S(6,6) 
      
        sheainvar=1D0/((4D0*a6666))
        IF (kk.EQ.0.D0) THEN 
          G_max = sheainvar; G_min = sheainvar
        ELSE
          IF (sheainvar.GE.G_max) G_max = sheainvar
          IF (sheainvar.LE.G_min) G_min = sheainvar
        ENDIF    
        ave = ave+sheainvar   
      ENDDO
      G_ave = ave/Nmesh_gamma
      shear2dmax(num) = G_max
      IF (G_max.GE.0D0) shear2dminp(num) = G_min
      IF (G_max.LE.0D0) shear2dminn(num) = G_min
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      pugh2dmax(num) =shear2dmax(num)/bulkMax2d(num)
      pugh2dminp(num)=shear2dminp(num)/bulkMax2d(num)
      pugh2dminn(num)=shear2dminn(num)/bulkMax2d(num)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      NPratio_max=0D0
      NPratio_min=1D0
      CALL CPratio(SS,NPratio_min,NPratio_max,NPratio_ave,&
                   v1min,v2min,v3min,v1max,v2max,v3max   ,&
                   theta,phi,vv11,vv12,vv13,vv22,vv23,vv33,Pratio)
 
      poisson2dmax(num) = NPratio_max
      IF (NPratio_min.GE.0D0) poisson2dminp(num) = NPratio_min
      IF (NPratio_min.LE.0D0) poisson2dminn(num) = NPratio_min 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CALL CHardness(BINver,SINver,sheainvar,hardvar)  
 
      IF (hardvar.NE.0.0) THEN
        hard2dmax(num) = hardvar   
      ELSE  
        hard2dmax(num) = 0D0
      ENDIF        
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    IF (yn_km=='Y' .OR. yn_km=='y') THEN
      CALL Ckm_cal(density, SINver, ma_avrag, km )
      IF (km.NE.0) THEN
        km2dmax(num)=km
      ELSE
        km2dmax(num)=0D0
      ENDIF
    ENDIF
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      num=num+1
    ENDDO
!!!!!!!!!!!!!!#######################################!!!!!!!!!!!!!!!!!!!!    
    IF (yn_veloc=='Y' .OR. yn_veloc=='y') THEN

       vec  = 0D0
       theta = 0.0d0
       DO i=0,180
        phi=0.0d0
        DO j=0,360
          CALL CALLCij (CCO)
          CALL twoD_calc_wave(mmx,kky,llz,smkl,j,vec)

          CALL wave_main_AAEP( i,j,theta,phi,vec,CCo,density,  VVG_P,    &
                                                         VVP_P,    &
                                                         VV_P_PF,  &
                                                         VVG_Sf,   &
                                                         VVP_Sf,   &
                                                         VV_Sf_PF, &
                                                         VVG_Ss,   &
                                                         VVP_Ss,   &
                                                         VV_Ss_PF    )
          IF (i==0)VVG_P_2D(j)   = VVG_P   /1000D0
          IF (i==0)VVP_P_2D(j)   = VVP_P   /1000D0
          IF (i==0)VV_P_PF_2D(j) = VV_P_PF !/1000D0
          IF (i==0)VVG_Sf_2D(j)  = VVG_Sf  /1000D0
          IF (i==0)VVP_Sf_2D(j)  = VVP_Sf  /1000D0
          IF (i==0)VV_Sf_PF_2D(j)= VV_Sf_PF!/1000D0
          IF (i==0)VVG_Ss_2D(j)  = VVG_Ss  /1000D0
          IF (i==0)VVP_Ss_2D(j)  = VVP_Ss  /1000D0
          IF (i==0)VV_Ss_PF_2D(j)= VV_Ss_PF!/1000D0

         
          ! WRITE(*,*)VV_Sf_PF  
          phi=phi+ 0.5d0
        ENDDO 
        theta=theta+0.5d0
      ENDDO
    ENDIF

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  PLOT 2D features...
    OPEN(  50,FILE="2dcut_young.dat"   )
    !OPEN(  51,FILE="2dyoungMin.dat"   )
    OPEN(  52,FILE="2dcut_shear.dat"   )
    !OPEN(  53,FILE="2dshearMinp.dat"  )
    !OPEN(  54,FILE="2dshearMinn.dat"  )
    OPEN(  55,FILE="2dcut_comp.dat"    )
   ! OPEN(  56,FILE="2dcompMin.dat"    )
    OPEN(  57,FILE="2dcut_poisson.dat" )
   ! OPEN(  58,FILE="2dpoissonMinp.dat")
   ! OPEN(  59,FILE="2dpoissonMinn.dat")
    OPEN(  60,FILE="2dcut_bulk.dat"   )
    !OPEN(  61,FILE="2dbulkMaxp.dat"   )
    OPEN(  62,FILE="2dcut_sound.dat"    )
    !OPEN(  63,FILE="2dsoundTMmax.dat" )
   ! OPEN(  64,FILE="2dsoundTMmin.dat" )
    OPEN(  71,FILE="2dcut_pugh.dat"   )
    OPEN(  72,FILE="2dcut_hardness.dat")
   ! OPEN(  73,FILE="2dpughMinn.dat"  )
   

    IF (yn_veloc=='Y' .or. yn_veloc=='y')   THEN !-----------------------------------------------------
      OPEN(500,FILE='2dcut_gveloc.dat')
      OPEN(501,FILE='2dcut_pveloc.dat')
      OPEN(502,FILE='2dcut_pfaveloc.dat')     
    ENDIF                                       !-----------------------------------------------------
    
    IF (yn_km=='Y' .or. yn_km=='y')   THEN   !!-----------------------------------------------------
      OPEN(499,FILE='2dcut_km.dat')
    ENDIF                                    !!-----------------------------------------------------
    
       num = 0
    test_n = 0.0
    DO num = 0,Nmesh_thata
    
      t2d = DBLE(num)*2D0*pi/Nmesh_thata
      new_num =  (360.0d0/Nmesh_thata)*num
       CALL slic3D_calc(mmx,kky,llz,smkl,num, Nmesh_thata, vec_3dslic)
      
      WRITE(50,"(7F30.20)")new_num,SQRT( (young2dmax(num)*COS(t2d)   )**2D0 + (young2dmax(num)*SIN(t2d)   )**2D0),& 
                                   SQRT( (young2dmin(num)*COS(t2d)   )**2D0 + (young2dmin(num)*SIN(t2d)   )**2D0), &
                                   vec_3dslic(1)*young2dmax(num), vec_3dslic(2)*young2dmax(num) ,vec_3dslic(3)*young2dmax(num) 

      WRITE(52,"(5F30.20)")new_num,SQRT( (shear2dmax(num)*COS(t2d)   )**2D0 + (shear2dmax(num)*SIN(t2d)   )**2D0),&
	                                  SQRT( (shear2dminp(num)*COS(t2d)  )**2D0 + (shear2dminp(num)*SIN(t2d)  )**2D0),&
                                   SQRT( (shear2dminn(num)*COS(t2d)  )**2D0 + (shear2dminn(num)*SIN(t2d)  )**2D0)
                     
	    WRITE(55,"(5F25.14)") new_num,SQRT( (comMAX2d(num)*COS(t2d)     )**2D0 + (comMAX2d(num)*SIN(t2d)     )**2D0),&
                                   SQRT( (comMIN2d(num)*COS(t2d)     )**2D0 + (comMIN2d(num)*SIN(t2d)     )**2D0)

      WRITE(57,"(5F30.20)")new_num,SQRT( (poisson2dmax(num)*COS(t2d) )**2D0 + (poisson2dmax(num)*SIN(t2d) )**2D0),&
                                   SQRT( (poisson2dminp(num)*COS(t2d))**2D0 + (poisson2dminp(num)*SIN(t2d))**2D0),&
                                   SQRT( (poisson2dminn(num)*COS(t2d))**2D0 + (poisson2dminn(num)*SIN(t2d))**2D0)

      WRITE(60,"(5F30.11)")new_num,SQRT( (bulkmin2d(num)*COS(t2d)    )**2D0 + (bulkmin2d(num)*SIN(t2d)    )**2D0),&
                                   SQRT( (bulkMax2d(num)*COS(t2d)    )**2D0 + (bulkmax2d(num)*SIN(t2d)    )**2D0)

      WRITE(62,"(5F30.20)")new_num,SQRT( (LM2d(num)*COS(t2d)         )**2D0 + (LM2d(num) *SIN(t2d)        )**2D0),&
                                   SQRT( (TMmax2d(num)*COS(t2d)      )**2D0 + (TMmax2d(num)*SIN(t2d)      )**2D0),&
                                   SQRT( (TMmin2d(num)*COS(t2d)      )**2D0 + (TMmin2d(num)*SIN(t2d)      )**2D0)

      WRITE(71,"(5F30.20)")new_num,SQRT( (pugh2dmax(num)*COS(t2d)   )**2D0 + (pugh2dmax(num)*SIN(t2d)     )**2D0),&
                                   SQRT( (pugh2dminp(num)*COS(t2d)  )**2D0 + (pugh2dminp(num)*SIN(t2d)    )**2D0),&
                                   SQRT( (pugh2dminn(num)*COS(t2d)  )**2D0 + (pugh2dminn(num)*SIN(t2d)    )**2D0) 

      WRITE(72,"(5F30.20)")new_num,SQRT( (hard2dmax(num)*COS(t2d)   )**2D0 + (hard2dmax(num)*SIN(t2d)   )**2D0),& 
                                  SQRT( (hard2dmin(num)*COS(t2d)   )**2D0 + (hard2dmin(num)*SIN(t2d)   )**2D0)


      
    IF (yn_km == 'Y' .or. yn_km == 'y') THEN        
      WRITE(499,"(5F30.20)")new_num,SQRT( (km2dmax(num)*COS(t2d)   )**2D0 + (km2dmax(num)*SIN(t2d)   )**2D0)  
    ENDIF  
        
    ENDDO
      IF (yn_veloc=='Y' .or. yn_veloc=='y') THEN
      
       DO num=0,360
        t2d=DBLE(num)/360D0*2D0*PI
        !WRITE(*,*)t2d
        WRITE(500,'(I3,3F30.15)')num,SQRT( (VVG_P_2D(num)*COS(t2d)   )**2D0 + (VVG_P_2D(num)*SIN(t2d)      )**2D0),&
                                     SQRT( (VVG_Sf_2D(num)*COS(t2d)   )**2D0 + (VVG_Sf_2D(num)*SIN(t2d)    )**2D0),&
                                     SQRT( (VVG_Ss_2D(num)*COS(t2d)   )**2D0 + (VVG_Ss_2D(num)*SIN(t2d)    )**2D0)

        WRITE(501,'(I3,3F30.15)')num,SQRT( (VVP_P_2D(num)*COS(t2d)   )**2D0 + (VVP_P_2D(num)*SIN(t2d)      )**2D0),&
                                     SQRT( (VVP_Sf_2D(num)*COS(t2d)   )**2D0 + (VVP_Sf_2D(num)*SIN(t2d)    )**2D0),&
                                     SQRT( (VVP_Ss_2D(num)*COS(t2d)   )**2D0 + (VVP_Ss_2D(num)*SIN(t2d)    )**2D0)

        WRITE(502,'(I3,3F30.15)')num,SQRT( (VV_P_PF_2D(num)*COS(t2d)   )**2D0 + (VV_P_PF_2D(num)*SIN(t2d)  )**2D0),&
                                     SQRT( (VV_Sf_PF_2D(num)*COS(t2d)   )**2D0 + (VV_Sf_PF_2D(num)*SIN(t2d))**2D0),&
                                     SQRT( (VV_Ss_PF_2D(num)*COS(t2d)   )**2D0 + (VV_Ss_PF_2D(num)*SIN(t2d))**2D0)   
       ENDDO                                  
      ENDIF    
      
    CLOSE(50)
   ! CLOSE(51)
    CLOSE(52)
   ! CLOSE(53)
    !CLOSE(55)
    CLOSE (55)
   ! CLOSE(56)
    CLOSE (57)
    !CLOSE(58)
   ! CLOSE(59)
    CLOSE (60)
   ! CLOSE(61)
    CLOSE (62)
    !CLOSE(63)
   ! CLOSE(64)
    CLOSE (71)
    CLOSE (72)
    !CLOSE(73)
    CLOSE (500)
    CLOSE (501)
    CLOSE (502)
    CLOSE (499)
   ! CLOSE(504)
   ! CLOSE(505)
   ! CLOSE(506)
   ! CLOSE(507)
   ! CLOSE(508)
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  PLOT 3D features...
    WRITE(*,*)"#==================================#"
    PRINT*," > Do you want to plot data? (Y/N):"
    READ(*,*)YN
    CALL system('clear')
    IF (YN.EQ."Y".OR. YN.EQ."y")THEN
      IF (YN.EQ."y") YN='Y'
      CALL SYSTEM('IF [ -e PicFile  ]; THEN rm -rf  PicFile; fi')
      CALL SYSTEM('IF [ -e DatFile  ]; THEN rm -rf  DatFile; fi')

      CALL SYSTEM('mkdir  PicFile')
      CALL SYSTEM('mkdir  DatFile')
      CALL plotshear3d(YN)
      WRITE(*,*)"=================================="
      CALL plotpugh3d(YN)
      WRITE(*,*)"=================================="
      IF (Mincomp < 0d0) CALL plotcomp3d(YN,1)
      IF (Mincomp > 0d0) CALL plotcomp3d(YN,2)	
      WRITE(*,*)"=================================="
      CALL plotbulk3d(YN)
      WRITE(*,*)"=================================="
      IF (Pratio_min < 0d0) CALL plotRatio3d(YN,1)
      IF (Pratio_min > 0d0) CALL plotRatio3d(YN,2)
      	!CALL plotRatio3d(YN)
      WRITE(*,*)"=================================="
      CALL plotyoung3d(YN)
      WRITE(*,*)"=================================="
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@   Preparing 2D data...
      CALL plot2df(YN,yn_veloc)
      CALL plot2df_gnuplot52(YN,Maxyoung,G_max2*1000D0,Maxcomp,maxEVaTMf,Pratio_max,pugh_max2*1000D0,Maxbulk)
      CALL sleep (1)
      CALL SYSTEM('mv *.dat ./DatFile')
      CALL SLEEP (1)
      CALL SYSTEM('mv *.ps PicFile')
      CALL SYSTEM('mv *.png PicFile')
      CALL SYSTEM('IF [ -e 2DPugh.ps ]; THEN mv 2DPugh.ps PicFile; fi') 
      !CALL SYSTEM('cat PicFile/2Dbox.ps            | epstopdf --filter > PicFile/2Dbox.pdf'                    )
      !CALL SYSTEM('cat PicFile/2DBulk.ps           | epstopdf --filter > PicFile/2DBulk.pdf'                    )
      !CALL SYSTEM('cat PicFile/2DCompressibiliy.ps | epstopdf --filter > PicFile/2DCompressibiliy.pdf')
      !CALL SYSTEM('cat PicFile/2DYoung.ps          | epstopdf --filter > PicFile/2DYoung.pdf'                  )
      !CALL SYSTEM('cat PicFile/2DShear.ps          | epstopdf --filter > PicFile/2DShear.pdf'                  )
      !CALL SYSTEM('cat PicFile/2DPoissons.ps       | epstopdf --filter > PicFile/2DPoissons.pdf'            )
      !CALL SYSTEM('cat PicFile/2DSound.ps         | epstopdf --filter > PicFile/2DSound.pdf'                )
      !CALL SYSTEM('cat PicFile/2DPugh.ps           | epstopdf --filter > PicFile/2DPugh.pdf'                  )
      !CALL SYSTEM('rm PicFile/2Dbox.ps PicFile/2DBulk.ps PicFile/2DCompressibiliy.ps PicFile/2DYoung.ps PicFile/2DShear.ps PicFile/2DPoissons.ps PicFile/2DPugh.ps')
      CALL SYSTEM('cp               DatFile/Cij.dat .')
      CALL SYSTEM('cp               DatFile/Sij.dat .')

      CALL SYSTEM('mv .aelastpro    DatFile')
      CALL SYSTEM('mv .MaMiout      DatFile')
      CALL SYSTEM('mv  MESH         DatFile')
      CALL SYSTEM('mv .MaMiout2     DatFile')
      CALL SYSTEM('IF [ -e .aelastpro2 ]; THEN mv .aelastpro2   DatFile; fi')
      !CALL SYSTEM('IF [ -e HKL ]; THEN mv HKL DatFile; fi')
    ELSE
      !CALL zerofile()
      PRINT*," > Data FILEs generated:"
      CALL SYSTEM('mkdir          DatFile')
      CALL SYSTEM('mv *.dat       DatFile ')
      CALL SYSTEM('ls             DatFile/*.dat')
      CALL SYSTEM('cp               DatFile/Cij.dat .')
      CALL SYSTEM('cp               DatFile/Sij.dat .')
      CALL SYSTEM('mv .aelastpro  DatFile')
      !CALL SYSTEM('mv  HKL        DatFile')
      CALL SYSTEM('mv .MaMiout    DatFile')
      CALL SYSTEM('mv  MESH       DatFile')
      CALL SYSTEM('mv .MaMiout2   DatFile')
      CALL SYSTEM('IF [ -e .aelastpro2 ]; THEN mv .aelastpro2   DatFile; fi')
    ENDIF
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

    CALL SYSTEM('rm -rf *.gnu')
  ELSE
    IF(d2d3 == 2) THEN      !@@@@@@@@@@@@@@@@@@@@@@@ 2D_3D system start  
      CALL analiz_2D_sys(adv,npoint)
    
 
      WRITE(*,*)"#==================================#"
      PRINT*," > Do you want to prepare the data for ploting? (Y/N):"
      READ(*,*)YN
      CALL system('clear')

      IF (YN.EQ."Y".OR. YN.EQ."y")THEN
        IF (YN.EQ."y") YN='Y'
        CALL SYSTEM('IF [ -e PicFile  ]; THEN rm -rf  PicFile; fi')
        CALL SYSTEM('IF [ -e DatFile ]; THEN rm -rf DatFile; fi')

        CALL SYSTEM('mkdir  PicFile')
        CALL SYSTEM('mkdir DatFile')
        CALL plot2df_gnuplot52_2d('poi')
        CALL plot2df_gnuplot52_2d('you')
        CALL plot2df_gnuplot52_2d('she')
        CALL SYSTEM(' mv 2D_sys_Poissons.ps 2D_sys_Shear.ps 2D_sys_Young.ps PicFile  ')
        CALL SYSTEM(' mv *.dat DatFile ')
        CALL SYSTEM('cp DatFile/Cij-2D.dat . ')
        !CALL SYSTEM(' mv MAX1 DatFile ')
        CALL SYSTEM('mv .MaMiout DatFile')
        !CALL system("dat2gnu_lapw D2")
      ELSE
        PRINT*," > Data FILEs generated:"
        CALL SYSTEM('mkdir DatFile')
        CALL SYSTEM('mv *.dat DatFile ')
        CALL SYSTEM('cp DatFile/Cij-2D.dat . ')
        CALL SYSTEM('ls  DatFile/*.dat ')
        !CALL SYSTEM(' mv MAX1 DatFile ')
        CALL SYSTEM(' mv .MaMiout DatFile ')
      ENDIF 
    END IF
  END IF   !@@@@@@@@@@@@@@@@@@@@@@@ 2D_3D system END 
  CLOSE(99)

 CALL checkfolder_old(1,0,d2d3)
 IF (YN.EQ."Y".OR. YN.EQ."y") THEN ; CALL checkfolder_old(0,1,d2d3); ENDIF
     
	CALL SYSTEM('tput setaf 55;tput bold; echo " ======================================= Visualization Tools =======================================";tput sgr0')  
	CALL SYSTEM('tput setaf 55;tput bold; echo " - dat2gnu.x  | To visualize data in polar, spherical coordinates and heat-maps diagrams: 2D sys., 2D projecton and 3D sys. ";tput sgr0') 
 CALL SYSTEM('tput setaf 55;tput bold; echo " - dat2agr.x  | To visualize data in polar, Cartesian coordinates                       : 2D projecton";tput sgr0') 
	CALL SYSTEM('tput setaf 55;tput bold; echo " - dat2wrl.x  | To visualize data in spherical coordinates                              : 3D sys. ";tput sgr0') 
	CALL SYSTEM('tput setaf 55;tput bold; echo " - dat2html.x | To visualize data in spherical coordinates                              : 3D sys. ";tput sgr0')
	CALL SYSTEM('tput setaf 55;tput bold; echo " ===================================================================================================";tput sgr0')  
	 WRITE(*,*)""
	 WRITE(*,*)"For any use of ElATools, please cite:"
	 WRITE(*,*)"Shahram Yalameha, Zahra Nourbakhsh, Daryoosh Vashaee,"
	 WRITE(*,*)"ElATools: A tool for analyzing anisotropic elastic "
	 WRITE(*,*)"properties of the 2D and 3D materials,"
	 WRITE(*,*)"Computer Physics Communications 271, 108195 (2022)"
	 WRITE(*,*)"https://doi.org/10.1016/j.cpc.2021.108195" 
	 WRITE(*,*)""
  WRITE(*,*)"                           ===================================================" 
  PRINT*,   "                                   Nothing happens until something moves.     "
  PRINT*,   "                                           >> Albert Einstion <<              "
  WRITE(*,*)"                           ---------------------------------------------------"
  PRINT*,   "                           >            | Have A BeautIFul Day |             <"
  PRINT*,   "                           >            |       Goodbye.       |             <"
  WRITE(*,*)"                           ===================================================" 
  WRITE(*,*)"" 

  CALL creadfolder(1, 0,mmx1,kky1,llz1)

  IF (YN.EQ."Y".OR. YN.EQ."y") THEN ; CALL creadfolder(0, 1,mmx1,kky1,llz1); ENDIF
  STOP       

  1369 WRITE(*,*)  " > NOT FOUNDE    Cij.dat    FILE"             ; STOP
  13691 WRITE(*,*) " > NOT FOUNDE   Cij-2D.dat  FILE"             ; STOP
  13692 WRITE(*,*) " > NOT FOUNDE   ELC-matrix  FILE"             ; STOP  
  1367 WRITE(*,*)  " > NOT FOUNDE INVELC-matrix FILE"             ; STOP
  1366 WRITE(*,*)  " > NOT FOUNDE elast.output  FILE "            ; STOP 
  1361 WRITE(*,*)  " > NOT FOUNDE    ELADAT     FILE "            ; STOP
  1341 WRITE(*,*)  " > NOT FOUNDE    ElaStic_2nd.out     FILE "   ; STOP
1370 END PROGRAM
