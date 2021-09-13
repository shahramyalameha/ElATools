
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
!!  "wrl_conv: data file ( of ElaTools code) to wrl file. 


PROGRAM wrl_conv
  USE ISO_FORTRAN_ENV
  DOUBLE PRECISION :: max=0d0,&
  Maxyoung,  &
  Minyoung,  &
  Maxcomp,   &
  Mincomp,   &
  Ha_max2,   &
  Ha_min2,   &
  G_max2,    &
  G_min2,    &
  pugh_max2, &
  pugh_min2, &
  Maxbulk,   &
  Minbulk,   &
  Pratio_max,&
  Pratio_min,&
  maxEVaTMf, &
  maxEVaLM,  &
  minEVaTMf, &
  VVP_P_max,&
  VVP_P_min,&
  VVP_Sf_max,&
  VVP_Sf_min,&
  VVP_Ss_max,&
  VVP_Ss_min,&
  VVG_P_max,&
  VVG_P_min,&
  VVG_Sf_max,&
  VVG_Sf_min,&
  VVG_Ss_max,&
  VVG_Ss_min,&
  VV_P_PF_max,&
  VV_Sf_PF_max,&
  VV_Ss_PF_max,&
  VV_P_PF_min,&
  VV_Sf_PF_min,&
  VV_Ss_PF_min,km_max,km_min
  INTEGER                             :: h_ex,k_ex,l_ex,num_color
  ChARACTER(LEN=6)                    :: e1,e2
  ChARACTER(LEN=2)                    :: ynveloc
  DOUBLE PRECISION, DIMENSION(3)        :: color_back =(/.0,.0,.0/),&
                                           color_front=(/1.,1.,1./),k
  DOUBLE PRECISION, DIMENSION(4)        :: color_pos  =(/.0,.8,.0,.0/),&  !< poi +min
                                           color_neg  =(/.8,.0,.0,.0/),&  !< poi -
                                           color_max  =(/.0,.0,.8,.5/),&  !< poi +max
                                           color_minp =(/.0,.8,.0,.0/),&
                                           color_minn =(/.8,.0,.0,.0/),&
                                           color_avep =(/.0,.0,.0,1./),&
                                           color_aven =(/.0,.0,.0,1./),&
                                          color_priam =(/.0,.0,.8,.0/),&
                                           color_fast =(/.0,.8,.0,.0/),&
                                           color_slow =(/.7,.0,.0,.0/),& 
                                           color_set1                 ,&
                                           color_ser2                 ,&
                                           color_set3             
                                         
  DOUBLE PRECISION, DIMENSION(1910000)  :: datapoints=0d0
  DOUBLE PRECISION, DIMENSION(1910000)  :: G_max,shminp,shminn,shavep,SINver,CO,comminp,pugh_max,pughminp,pughminn,pughavep,&
		                                       comminn,NPratio_max,pminp,pminn,pavep,paven,&
                                           BINver,maxEVaLM1,maxEVaTM1,minEVaTM1,VVG_P,VVP_P,VV_P_PF,VVG_Sf,VVP_Sf,VV_Sf_PF,hardvar ,&
                                           VVG_Ss,VVP_Ss,VV_Ss_PF,km
  ChARACTER(len=7), dimension(10)       :: arg_mane
  INTEGER,          DIMENSION(190300,4) :: mesh=0 
  INTEGER                               :: n_phif, n_thetaf, num_mesh,ii=0,argl,cutmesh
  character(len=10)                     :: val='',namepro 
  character(len=1)                      :: clor_val=" "
  ChARACTER(len=7)                      :: a=' '
  
    OPEN(69, file="MESH")
    read(69,*)n_phif,n_thetaf,cutmesh
    WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh

    close(69)

     ! Get command line args (Fortran 2003 standard)
  N_arg = 0
  DO
    CALL get_command_argument(N_arg, a)
    IF (LEN_TRIM(a) == 0) EXIT

    arg_mane(N_arg)=TRIM(a)
    N_arg = N_arg+1
  END DO

 val      = arg_mane(1)	
 clor_val = arg_mane(2)	
 
      WRITE(*,'(3a)')val,'was read well...'
     IF (val=='-h' .OR. val=="") then
       Write(*,*)'Using: dat2wrl_lapw [ Properties ] [colors: 1 ] out of DatFile_*** folder'
       WRITE(*,*)''
       CALL system ("sleep 1")
       WRITE(*,*)'[3D Properties]: '
       WRITE(*,*)' poi    =>  Poisson’s ratio                 '
       WRITE(*,*)' shear  =>  Shear modulus                   '
       WRITE(*,*)' young  =>  Young’s modulus                 '
       WRITE(*,*)' bulk   =>  Bulk modulus                    '
       WRITE(*,*)' comp   =>  Linear compressibility          '
       WRITE(*,*)' hard   =>  Hardness                        '
       WRITE(*,*)' pp     =>  Phase velocity: P-mode          '
       WRITE(*,*)' ps     =>  Phase velocity: Slow-mode       '
       WRITE(*,*)' pf     =>  Phase velocity: Fast-mode       '
       WRITE(*,*)' gp     =>  Group velocity: P-mode          '
       WRITE(*,*)' gs     =>  Group velocity: Slow-mode       '
       WRITE(*,*)' gf     =>  Group velocity: Fast-mode       '
       WRITE(*,*)' pfp    =>  Power Flow angle (PFA): P-mode  '
       WRITE(*,*)' pfs    =>  Power Flow angle: Slow-mode     '
       WRITE(*,*)' pff    =>  Group velocity: Fast-mode       '
       WRITE(*,*)' km     =>  Min. thermal conductivity       '
       STOP
     END IF
  !
 OPEN(123, FILE='HKL')
 read(123,*) e1
 read(123,*) e2
 read(123,*) h_ex
 read(123,*) k_ex
 read(123,*) l_ex
 read(123,*) ynveloc
 CLOSE(123)
 !WRITE(*,*) e1
 !WRITE(*,*) e2
 !WRITE(*,*) h_ex
 !WRITE(*,*) k_ex
 !WRITE(*,*) l_ex
  WRITE(*,*) ynveloc

 IF (ynveloc=='N' .OR. ynveloc=='n')THEN
  IF (val=='PhaseP'.or. val=='phasep' .or. val=='pp' .or. &
     val=='PhaseF'.or. val=='phasef' .or. val=='pf'   .or. &
     val=='PhaseS'.or. val=='phases' .or. val=='ps'   .or. &
     val=='GroupP'.or. val=='groupp' .or. val=='gp'   .or. &
     val=='Groupf'.or. val=='groupf' .or. val=='gf'   .or. &
     val=='GroupS'.or. val=='groups' .or. val=='gs'   .or. &
     val=='PFactP'.or. val=='pfoupp' .or. val=='pfp'  .or. &
     val=='PFactF'.or. val=='pfoupf' .or. val=='pff'  .or. &
     val=='PFactS'.or. val=='pfoups' .or. val=='pfs'  .or. &
     val=='km'    .or. val=='Km'     .or. val=='KM'   )then
     WRITE(*,*) "Sorry! Your request is invalid!"
     call sleep(1)
     WRITE(*,*) "Have phase and group velocity calculations been performed?!"    
     stop
  ENDif
 Endif
 OPEN(36,file='.MaMiout')
 read(36,*)  Maxyoung,Minyoung,Maxcomp,Mincomp,G_max2,G_min2,Maxbulk,Minbulk,Pratio_max,Pratio_min,maxEVaTMf,maxEVaLM,minEVaTMf,pugh_max2,pugh_min2 ,Ha_max2,Ha_min2
 IF (ynveloc=='Y' .OR. ynveloc=='y')THEN
   OPEN(37,file='.MaMiout2')
   read(37,*) VVP_P_max ,VVP_P_min ,VVP_Sf_max ,VVP_Sf_min ,VVP_Ss_max ,VVP_Ss_min ,VVG_P_max ,&
            VVG_P_min ,VVG_Sf_max ,VVG_Sf_min ,VVG_Ss_max ,VVG_Ss_min,&
            VV_P_PF_max,VV_Sf_PF_max,VV_Ss_PF_max,VV_P_PF_min,VV_Sf_PF_min,VV_Ss_PF_min,km_max,km_min
   close(37)
 
 ENDIF
  close(36)
  
      !  1 0 0	#color 0 r g b values = red
      !  1 1 0	#color 1 r g b values = yellow
      !  0 0 1	#color 2 r g b values = blue
      !  0 1 0	#color 3 r g b values = green
   
    OPEN(1,file='.aelastpro')
    OPEN(83,file='.aelastpro2')
    do ii=1, (n_phif*n_thetaf)+1
     ! print*, ii
     read(1,*) G_max(ii),shminp(ii),shminn(ii),shavep(ii),SINver(ii),CO(ii),comminp(ii),                             &   ! 7
		             comminn(ii),NPratio_max(ii),pminp(ii),pminn(ii),pavep(ii),paven(ii),BINver(ii),                       &   ! 14
               maxEVaLM1(ii),maxEVaTM1(ii),minEVaTM1(ii),pugh_max(ii),pughminp(ii),pughminn(ii),pughavep(ii),hardvar(ii) ! 22
     IF (ynveloc=='Y' .OR. ynveloc=='y')THEN
 
      read(83,*) VVP_P(ii),VVG_P(ii),VVP_Sf(ii),VVG_Sf(ii),VVP_Ss(ii),VVG_Ss(ii),VV_P_PF(ii),VV_Sf_PF(ii),VV_Ss_PF(ii),km(ii)
     ENDIF

        if (ii==(n_phif*n_thetaf)+1) exit
    ENDDO
    close(83)
    close(1)

if (val=='young' .or. val=='Young' .or. val=='yon') then   
   max=Maxyoung*1.5
   max=max*1.5  
   OPEN (41, FILE ='Young.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
  !CALL signـcolor_wrl(color_minp,color_minn,n_phif,n_thetaf,SINver)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,SINver)
! for v1.6.3
   if (clor_val=="1") THEN
     CALL set_colors(val, 1,color_set1,color_set2,color_set3)
     CALL shape_appearance_wrl(color_set1)
   else
    CALL shape_appearance_wrl(color_pos)
   endif
! rof v1.6.3
   CALL close_shape_wrl
   close(41)
     WRITE(*,'(2a)') ' > Young.wrl was created.' 
   STOP
ENDif
!> v1.6.5
if (val=='hardness' .or. val=='Hardness' .or. val=='hard') then   
  max=Ha_max2*1.5
  max=max*1.5  
  OPEN (41, FILE ='Hardness.wrl')
  CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
  CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
  CALL shape_wrl()
  CALL spheroid_wrl(n_phif,n_thetaf,mesh)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,hardvar)
! for v1.6.3
  if (clor_val=="1") THEN
    CALL set_colors(val, 1,color_set1,color_set2,color_set3)
    CALL shape_appearance_wrl(color_set1)
  else
   CALL shape_appearance_wrl(color_pos)
  endif
! rof v1.6.3
  CALL close_shape_wrl
  close(41)
    WRITE(*,'(2a)') ' > Hardness.wrl was created.' 
  STOP
ENDif
!< 1.6.5
if (val=='shear'.or. val=='Shear' .or. val=='she') then
   max=G_max2*1.5
   if (abs(G_min2).GE.max) max=-G_min2
   max=max*1.5d0 !150d0
   OPEN (41, FILE = 'Shear.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,G_max)
   ! for v1.6.3
    if (clor_val=="1") THEN
     CALL set_colors(val,1, color_set1,color_set2,color_set3)
     CALL shape_appearance_wrl(color_set1)
   else
    CALL shape_appearance_wrl(color_max)
   endif
! rof v1.6.3
   CALL close_shape_wrl
   
   CALL shape_wrl()
  CALL spheroid_wrl(n_phif,n_thetaf,mesh)
   ! for v1.6.3
    if (clor_val=="1") THEN
     CALL set_colors(val, 2,color_set1,color_set2,color_set3)
     CALL color_sign_wrl(color_set2,color_minn,n_phif,n_thetaf,shminp) 
  else
    CALL color_sign_wrl(color_minp,color_minn,n_phif,n_thetaf,shminp)   
  endif
! rof v1.6.3  
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,shminp)
   CALL shape_appearance_wrl((/0d0,0d0,0d0/))

   CALL close_shape_wrl
   
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
   CALL color_sign_wrl(color_avep,color_aven,n_phif,n_thetaf,shavep)   
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,shavep)
   CALL shape_appearance_wrl((/0d0,0d0,0d0,color_avep(4)/))
   CALL close_shape_wrl
   close(41)
   WRITE(*,'(2a)') ' > Shear.wrl was created.'  
   
ENDif

if (val=='pugh'.or. val=='Pugh' .or. val=='pug') then
   max=pugh_max2 *1.5
   if (abs(pugh_min2).GE.max) max=-pugh_min2
   max=max !*1.5d0
   OPEN (41, FILE = 'Pugh.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,pugh_max)
      ! for v1.6.3
   if (clor_val=="1") THEN
    CALL set_colors(val,1, color_set1,color_set2,color_set3)
    CALL shape_appearance_wrl(color_set1)
  else
   CALL shape_appearance_wrl(color_max)
  endif
! rof v1.6.3
   CALL close_shape_wrl
   
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
   ! for v1.6.3
   if (clor_val=="1") THEN
    CALL set_colors(val, 2,color_set1,color_set2)
    CALL color_sign_wrl(color_set2,color_minn,n_phif,n_thetaf,pughminp)  
 else
  CALL color_sign_wrl(color_minp,color_minn,n_phif,n_thetaf,pughminp)   
endif
! rof v1.6.3 
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,pughminp)
   CALL shape_appearance_wrl((/0d0,0d0,0d0/))
   CALL close_shape_wrl
   
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
   CALL color_sign_wrl(color_avep,color_aven,n_phif,n_thetaf,pughavep)   
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,pughavep)
   CALL shape_appearance_wrl((/0d0,0d0,0d0,color_avep(4)/))
   CALL close_shape_wrl
   close(41)
   WRITE(*,'(2a)') ' > Pugh.wrl was created.'  
   
ENDif



if (val=='poisson'.or. val=='Poisson' .or. val=='poi') then
   max=Pratio_max*1.5d0
   if (abs(Pratio_min).GE.max) max=-Pratio_min
   max=max
   OPEN (41, FILE = 'Poisson.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   
   if (color_max(4).LT.1d0) then
     CALL shape_wrl()
     CALL spheroid_wrl(n_phif,n_thetaf,mesh)
     CALL mesh_datapoints_wrl(n_phif,n_thetaf,NPratio_max)
  ! for v1.6.3
     if (clor_val=="1") THEN
      CALL set_colors(val, 1,color_set1,color_set2,color_set3)
      CALL shape_appearance_wrl(color_set1)
    else
      CALL shape_appearance_wrl(color_max)
    endif
 ! rof v1.6.3
     CALL close_shape_wrl
   ENDif
   
   if (color_minp(4).LT.1d0) then
     CALL shape_wrl()
     CALL spheroid_wrl(n_phif,n_thetaf,mesh)
     CALL mesh_datapoints_wrl(n_phif,n_thetaf,pminp)
       ! for v1.6.3
     if (clor_val=="1") THEN
      CALL set_colors(val, 2,color_set1,color_set2,color_set3)
      CALL shape_appearance_wrl(color_set2)
    else
      CALL shape_appearance_wrl(color_minp)
    endif
 ! rof v1.6.3
     CALL close_shape_wrl
   ENDif  
   
   if (color_minn(4).LT.1d0) then
     CALL shape_wrl()
     CALL spheroid_wrl(n_phif,n_thetaf,mesh)
     CALL mesh_datapoints_wrl(n_phif,n_thetaf,pminn)
 ! for v1.6.3
     if (clor_val=="1") THEN
      CALL set_colors(val, 3,color_set1,color_set2,color_set3)
      CALL shape_appearance_wrl(color_set3)
    else
      CALL shape_appearance_wrl(color_minn)
    endif
 ! rof v1.6.3
     CALL close_shape_wrl
   ENDif
   
   if (color_avep(4).LT.1d0) then
     CALL shape_wrl()
     CALL spheroid_wrl(n_phif,n_thetaf,mesh)
     CALL mesh_datapoints_wrl(n_phif,n_thetaf,pavep)
     CALL shape_appearance_wrl(color_avep)
     CALL close_shape_wrl
   ENDif  
   
   if (color_aven(4).LT.1d0) then
     CALL shape_wrl()
     CALL spheroid_wrl(n_phif,n_thetaf,mesh)
     CALL mesh_datapoints_wrl(n_phif,n_thetaf,paven)
     CALL shape_appearance_wrl(color_aven)
     CALL close_shape_wrl
   ENDif  
      WRITE(*,'(2a)') ' > Poisson.wrl was created.' 
   close(41)
ENDif

if (val=='compress'.or. val=='com' .or. val=='comp') then
   max=Maxcomp*1.5
   if (abs(Mincomp).GE. max) max=Mincomp
   max=max
   OPEN (41, FILE ='Compress.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
   ! for v1.6.3
   if (clor_val=="1") THEN
    CALL set_colors(val, 1,color_set1,color_set2)
    CALL set_colors(val, 2,color_set1,color_set2)
    CALL color_sign_wrl(color_set2,color_set2,n_phif,n_thetaf,co)  
   else
    CALL color_sign_wrl(color_pos,color_neg,n_phif,n_thetaf,co)  
  endif
! rof v1.6.3 
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,co)
   CALL shape_appearance_wrl((/0d0,0d0,0d0,color_pos(4)/))
   CALL close_shape_wrl
   close(41)
         WRITE(*,'(2a)') ' > Compress.wrl was created.' 
ENDif 

if (val=='bulk'.or. val=='Bulk' .or. val=='bul') then
   max=Maxbulk*1.5
   if (abs(Minbulk).GE. max) max=Minbulk
   max=max
   OPEN (41, FILE ='Bulk.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
   !CALL color_sign_wrl((/0d0,0d0,.9d0/),color_neg,n_phif,n_thetaf,BINver)  
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,BINver)
   ! for v1.6.3
  if (clor_val=="1") THEN
     CALL set_colors(val, 1,color_set1,color_set2,color_set3)
     CALL shape_appearance_wrl(color_set1)
  else
    CALL shape_appearance_wrl(color_pos)
  endif
! rof v1.6.3
   CALL close_shape_wrl
   close(41)
         WRITE(*,'(2a)') ' > Bulk.wrl was created.' 
ENDif
if (val=='sound'.or. val=='Sound' .or. val=='sou') then
   max=maxEVaLM
   if (abs(maxEVaTMf).GE.max) max=maxEVaTMf
   max=max*1.5
   OPEN (41, FILE = 'Sound.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,maxEVaTM1)
   CALL shape_appearance_wrl((/0d0,0d0,0.8d0,0.5d0/))
   CALL close_shape_wrl
   
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,minEVaTM1)
   CALL shape_appearance_wrl((/0.5d0,0d0,0d0,0d0/))
   CALL close_shape_wrl
   
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,maxEVaLM1)
   CALL shape_appearance_wrl((/0d0,0.5d0,0d0,0.5d0/))
   CALL close_shape_wrl
   close(41)
   WRITE(*,'(2a)') ' > Sound.wrl was created.'    
ENDif

if (val=='PhaseP'.or. val=='phasep' .or. val=='pp') then
   max=VVP_P_max*1.2D0
   max=max 
   OPEN (41, FILE ='Phase-P.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
  !CALL signـcolor_wrl(color_minp,color_minn,n_phif,n_thetaf,SINver)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,VVP_P)
  ! for v1.6.3
   if (clor_val=="1") THEN
    CALL set_colors(val, 1,color_set1,color_set2,color_set3)
    CALL shape_appearance_wrl(color_set1)
  else
    CALL shape_appearance_wrl(color_priam)
  endif
! rof v1.6.3  
   CALL close_shape_wrl
   close(41)
     WRITE(*,'(2a)') ' > Phase-P.wrl was created.' 
   STOP
ENDif
if (val=='PhaseF'.or. val=='phasef' .or. val=='pf') then
   max=VVP_Sf_max*1.2D0
   max=max 
   OPEN (41, FILE ='Phase-Fast.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
  !CALL signـcolor_wrl(color_minp,color_minn,n_phif,n_thetaf,SINver)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,VVP_Sf)
  ! for v1.6.3
   if (clor_val=="1") THEN
    CALL set_colors(val, 1,color_set1,color_set2,color_set3)
    CALL shape_appearance_wrl(color_set1)
  else
    CALL shape_appearance_wrl(color_fast)
  endif
! rof v1.6.3     
   CALL close_shape_wrl
   close(41)
     WRITE(*,'(2a)') ' > Phase-Fast.wrl was created.' 
   STOP 
ENDif
if (val=='PhaseS'.or. val=='phases' .or. val=='ps') then
   max=VVP_Ss_max*1.2D0
   max=max 
   OPEN (41, FILE ='Phase-Slow.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
  !CALL signـcolor_wrl(color_minp,color_minn,n_phif,n_thetaf,SINver)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,VVP_Ss)
  ! for v1.6.3
   if (clor_val=="1") THEN
    CALL set_colors(val, 1,color_set1,color_set2,color_set3)
    CALL shape_appearance_wrl(color_set1)
  else
    CALL shape_appearance_wrl(color_slow)
  endif
! rof v1.6.3    
   CALL close_shape_wrl
   close(41)
     WRITE(*,'(2a)') ' > Phase-Slow.wrl was created.' 
   STOP
ENDif
if (val=='GroupP'.or. val=='groupp' .or. val=='gp') then
   max=VVG_P_max*1.2D0
   max=max 
   OPEN (41, FILE ='Group-P.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
  !CALL signـcolor_wrl(color_minp,color_minn,n_phif,n_thetaf,SINver)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,VVG_P)
  ! for v1.6.3
   if (clor_val=="1") THEN
    CALL set_colors(val, 1,color_set1,color_set2,color_set3)
    CALL shape_appearance_wrl(color_set1)
  else
    CALL shape_appearance_wrl(color_priam)  
  endif
! rof v1.6.3    

   CALL close_shape_wrl
   close(41)
     WRITE(*,'(2a)') ' > Group-P.wrl was created.' 
   STOP
ENDif
if (val=='Groupf'.or. val=='groupf' .or. val=='gf') then
   max=VVG_Sf_max*1.2D0
   max=max*1.5
   OPEN (41, FILE ='Group-Fast.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
  !CALL signـcolor_wrl(color_minp,color_minn,n_phif,n_thetaf,SINver)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,VVG_Sf)
  ! for v1.6.3
   if (clor_val=="1") THEN
    CALL set_colors(val, 1,color_set1,color_set2,color_set3)
    CALL shape_appearance_wrl(color_set1)
  else
    CALL shape_appearance_wrl(color_fast)
  endif
! rof v1.6.3    
   CALL close_shape_wrl
   close(41)
     WRITE(*,'(2a)') ' > Group-Fast.wrl was created.' 
   STOP
ENDif
if (val=='GroupS'.or. val=='groups' .or. val=='gs') then
   max=VVG_Ss_max*1.2D0
   max=max!*1.5  
   OPEN (41, FILE ='Group-Slow.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
  !CALL signـcolor_wrl(color_minp,color_minn,n_phif,n_thetaf,SINver)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,VVG_Ss)
   CALL shape_appearance_wrl(color_slow)
   CALL close_shape_wrl
   close(41)
     WRITE(*,'(2a)') ' > Group-Slow.wrl was created.' 
   STOP 
ENDif
 if (val=='PFactP'.or. val=='pfoupp' .or. val=='pfp') then
    max=VV_P_PF_max*1.2D0
   max=max
   OPEN (41, FILE ='Power-Flow-P.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
  !CALL signـcolor_wrl(color_minp,color_minn,n_phif,n_thetaf,SINver)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,VV_P_PF)
  ! for v1.6.3
   if (clor_val=="1") THEN
    CALL set_colors(val, 1,color_set1,color_set2,color_set3)
    CALL shape_appearance_wrl(color_set1)
  else
    CALL shape_appearance_wrl(color_priam)
  endif
! rof v1.6.3     
   CALL close_shape_wrl
   close(41)
     WRITE(*,'(2a)') ' > Power-Flow-P.wrl was created.' 
   STOP 
 ENDif
 if (val=='PFactF'.or. val=='pfoupf' .or. val=='pff') then
   max=VV_Sf_PF_max 
   max=max!*1.5  
   WRITE(*,*)max
   OPEN (41, FILE ='Power-Flow-Fast.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
  !CALL signـcolor_wrl(color_minp,color_minn,n_phif,n_thetaf,SINver)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,VV_Sf_PF)
  ! for v1.6.3
   if (clor_val=="1") THEN
    CALL set_colors(val, 1,color_set1,color_set2,color_set3)
    CALL shape_appearance_wrl(color_set1)
  else
    CALL shape_appearance_wrl(color_fast)
  endif
! rof v1.6.3    
   CALL close_shape_wrl
   close(41)
     WRITE(*,'(2a)') ' > Power-Flow-Fast.wrl was created.' 
   STOP  
 ENDif
 if (val=='PFactS'.or. val=='pfoups' .or. val=='pfs') then
   max=VV_Ss_PF_max*1.2D0
   max=max!*1.5  
   OPEN (41, FILE ='Power-Flow-slow.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
  !CALL signـcolor_wrl(color_minp,color_minn,n_phif,n_thetaf,SINver)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,VV_Sf_PF)
     ! for v1.6.3
   if (clor_val=="1") THEN
    CALL set_colors(val, 1,color_set1,color_set2,color_set3)
    CALL shape_appearance_wrl(color_set1)
  else
    CALL shape_appearance_wrl(color_fast)
  endif
! rof v1.6.3  
   CALL close_shape_wrl
   close(41)
     WRITE(*,'(2a)') ' > Power-Flow-slow.wrl was created.' 
   STOP    
 ENDif
 if (val=='km' .or. val=='Km' .or. val=='KM') then   
   max=km_max*1.5
   max=max*1.5  
   OPEN (41, FILE ='Conductivity.wrl')
   CALL cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
   CALL start_wrl((/0.75d0,0.75d0,0.75d0/),(/1.0d0,0.0d0,0.75d0/),max)
   CALL shape_wrl()
   CALL spheroid_wrl(n_phif,n_thetaf,mesh)
  !CALL signـcolor_wrl(color_minp,color_minn,n_phif,n_thetaf,km)
   CALL mesh_datapoints_wrl(n_phif,n_thetaf,km)
! for v1.6.3
   if (clor_val=="1") THEN
     CALL set_colors(val, 1,color_set1,color_set2,color_set3)
     CALL shape_appearance_wrl(color_set1)
   else
    CALL shape_appearance_wrl(color_pos)
   endif
! rof v1.6.3
   CALL close_shape_wrl
   close(41)
     WRITE(*,'(2a)') ' > Conductivity.wrl was created.' 
   STOP
ENDif
END program
