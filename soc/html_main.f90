
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
!!  "html_conv: data file ( of ElaTools code) to html for plotly file. 

program web_conv
  USE ISO_FORTRAN_ENV
  IMPLICIT NONE
  ChARACTER(len=10)                :: namepro=' ',type_pro,val=" ",clor_val=" ",cval1 ,cval2 ,cval3 
  ChARACTER(len=5)                 :: contours_line
  ChARACTER(len=30)                :: filename='data.html',id_pro
  ChARACTER(len=1)                 :: cord_ename=""
  ChARACTER(len=5)                 :: zsmooth 
  ChARACTER(len=5)                 :: showscale 
  ChARACTER(len=20)                :: titel
  integer                          :: naumbers 
  real                             :: opacity
  integer                          :: height,width
  INTEGER,     DIMENSION(190300,4) :: mesh=0
  INTEGER                          :: n_phif, n_thetaf, num_mesh,cutmesh
  ChARACTER(len=7), dimension(10)  :: arg_mane
  ChARACTER(len=7)                 :: a=' ' 
  INTEGER                          :: argl,i,N_frame=0,Nploter,Ng,Ns,N_arg
  ChARACTER(LEN=6)                 :: e1,e2
  ChARACTER(LEN=2)                 :: ynveloc
  INTEGER                          :: h_ex,k_ex,l_ex
  
  DOUBLE PRECISION, DIMENSION(999999)  :: young_x,young_y,young_z,young0_x,young0_y,young0_z,younge_x,younge_y,younge_z,&
                                           bulk_x,bulk_y,bulk_z,bulk0_x,bulk0_y,bulk0_z,bulke_x,bulke_y,bulke_z,&
                                           shear_max_x ,shear_max_y ,shear_max_z,      &
                                           shear0_max_x ,shear0_max_y ,shear0_max_z,   &
                                           sheare_max_x ,sheare_max_y ,sheare_max_z,   &
                                           shear_min_x ,shear_min_y ,shear_min_z,      &
                                           shear0_min_x ,shear0_min_y ,shear0_min_z,   &
                                           sheare_min_x ,sheare_min_y ,sheare_min_z,   &
                                           shear_neg_x ,shear_neg_y ,shear_neg_z,      &
                                           shear_avg_x ,shear_avg_y ,shear_avg_z ,     &
                                           copm0_max_x ,copm0_max_y ,copm0_max_z ,        &
                                           copme_max_x ,copme_max_y ,copme_max_z ,        &
                                           copm_max_x ,copm_max_y ,copm_max_z ,        &
                                           copm0_min_x ,copm0_min_y ,copm0_min_z ,        &
                                           copme_min_x ,copme_min_y ,copme_min_z ,        &
                                           copm_min_x ,copm_min_y ,copm_min_z ,        &
                                           copm0_neg_x ,copm0_neg_y ,copm0_neg_z , &
                                           copme_neg_x ,copme_neg_y ,copme_neg_z , &
                                           copm_neg_x ,copm_neg_y ,copm_neg_z,     &
                                           poi0_max_x  ,poi0_max_y  ,poi0_max_z ,&
                                           poie_max_x  ,poie_max_y  ,poie_max_z  ,&
                                           poi_max_x  ,poi_max_y  ,poi_max_z  ,&
                                           poi0_min_x  ,poi0_min_y  ,poi0_min_z  ,&
                                           poie_min_x  ,poie_min_y  ,poie_min_z  ,&
                                           poi_min_x  ,poi_min_y  ,poi_min_z  ,&
                                           poi0_neg_x  ,poi0_neg_y  ,poi0_neg_z  ,& 
                                           poie_neg_x  ,poie_neg_y  ,poie_neg_z  ,&
                                           poi_neg_x  ,poi_neg_y  ,poi_neg_z  ,&
                                           poi_max_avg_x  ,poi_max_avg_y  ,poi_max_avg_z  ,&
                                           poi_min_avg_x  ,poi_min_avg_y  ,poi_min_avg_z   

DOUBLE PRECISION, DIMENSION(999999)  ::   VVP_P0_x, VVP_P0_y, VVP_P0_z, & 
                                          VVP_Pe_x, VVP_Pe_y, VVP_Pe_z, & 
                                          VVP_P_x, VVP_P_y, VVP_P_z ,&
                                          VVP_Sf0_x,VVP_Sf0_y,VVP_Sf0_z,&
                                          VVP_Sfe_x,VVP_Sfe_y,VVP_Sfe_z,&
                                          VVP_Sf_x,VVP_Sf_y,VVP_Sf_z ,&
                                          VVG_P0_x, VVG_P0_y, VVG_P0_z, & 
                                          VVG_Pe_x, VVG_Pe_y, VVG_Pe_z, & 
                                          VVG_P_x, VVG_P_y, VVG_P_z ,&
                                          VVG_Sf0_x,VVG_Sf0_y,VVG_Sf0_z,&
                                          VVG_Sfe_x,VVG_Sfe_y,VVG_Sfe_z,&
                                          VVG_Sf_x,VVG_Sf_y,VVG_Sf_z,&
                                          VVG_Ss0_x,VVG_Ss0_y,VVG_Ss0_z,&
                                          VVG_Sse_x,VVG_Sse_y,VVG_Sse_z,&
                                          VVG_Ss_x,VVG_Ss_y,VVG_Ss_z,  &
                                          VV_P_FA0_x,VV_P_FA0_y,VV_P_FA0_z,&
                                          VV_P_FAe_x,VV_P_FAe_y,VV_P_FAe_z,&
                                          VV_P_FA_x,VV_P_FA_y,VV_P_FA_z ,  &
                                          VV_Ss_FA0_x,VV_Ss_FA0_y,VV_Ss_FA0_z,&
                                          VV_Ss_FAe_x,VV_Ss_FAe_y,VV_Ss_FAe_z,&
                                          VV_Ss_FA_x,VV_Ss_FA_y,VV_Ss_FA_z ,  &
                                          VV_Sf_FA0_x,VV_Sf_FA0_y,VV_Sf_FA0_z,&
                                          VV_Sf_FAe_x,VV_Sf_FAe_y,VV_Sf_FAe_z,&
                                          VV_Sf_FA_x,VV_Sf_FA_y,VV_Sf_FA_z   ,&
                                          km0_x      ,km0_y      ,km0_z      ,&
                                          kme_x      ,kme_y      ,kme_z      ,&
                                          km_x       ,km_y       ,km_z                  
!*********set data************    
  !namepro="poi"
  !naumbers=1
  !type_pro = 'max'
  zsmooth='fast'
  showscale='false'
  !contours_line='true'
  height=800
  width=850
!*********************  
  clor_val=" "
  arg_mane(2)="n" 
  arg_mane(3)="n" 
  arg_mane(4)="n" 
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
 
 cval1    = arg_mane(2)
 cval2    = arg_mane(3)
 cval3    = arg_mane(4)
 
 ! write(*,*) val,clor_val
  IF (val == "-h" .OR. val == "")THEN
       WRITE(*,*)'Using: dat2html.x [ Properties ] [ colors ] in the  DatFile_*** folder' 
       WRITE(*,*)''
       call system ("sleep 0.5")
       WRITE(*,*)'[3D Properties]:                                    '
       WRITE(*,*)' poi     => Poisson’s ratio                         '
       WRITE(*,*)' shear   => Shear modulus                           '
       WRITE(*,*)' young   => Young’s modulus                         '
       WRITE(*,*)' bulk    => Bulk modulus                            '
       WRITE(*,*)' comp    => Linear compressibility                  '
       WRITE(*,*)' hard    => Hardness                               '
       WRITE(*,*)' pp      => Phase velocity: P-mode                  '
       WRITE(*,*)' ps      => Phase velocity: Slow-mode               '
       WRITE(*,*)' pf      => Phase velocity: Fast-mode               '
       WRITE(*,*)' gp      => Group velocity: P-mode                  '
       WRITE(*,*)' gs      => Group velocity: Slow-mode               '
       WRITE(*,*)' gf      => Group velocity: Fast-mode               '
       WRITE(*,*)' pfp     => Power Flow angle (PFA): P-mode          '
       WRITE(*,*)' pfs     => Power Flow angle: Slow-mode             '
       WRITE(*,*)' pff     => Power Flow angle: Fast-mode             '
       WRITE(*,*)' km      => Min. thermal conductivity              '
       WRITE(*,*)' '
 
       STOP
  endif
       namepro= val
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
 !WRITE(*,*) ynveloc

 IF (ynveloc=='N' .OR. ynveloc=='n')THEN
  if (val=='PhaseP'.or. val=='phasep' .or. val=='pp' .or. &
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
  endif      
If(namepro=="poi" .or. namepro=="poisson") id_pro="Poisson"
If(namepro=="yon" .or. namepro=="young") id_pro="Young"
If(namepro=="sh" .or. namepro=="shear") id_pro="Shear"
If(namepro=="com" .or. namepro=="comp") id_pro="Compressibility"
If(namepro=="bul" .or. namepro=="bulk") id_pro="Bulk"
If(namepro=="hard" .or. namepro=="hardness") id_pro="Hardness"

If(namepro=='PhaseP'.or. namepro=='phasep' .or. namepro=='pp') id_pro="P-Phase"
If(namepro=='PhaseF'.or. namepro=='Phasef' .or. namepro=='pf') id_pro="Phase-Fast"
If(namepro=='PhaseS'.or. namepro=='phases' .or. namepro=='ps') id_pro="Phase-Slow"
If(namepro=='GroupP'.or. namepro=='groupp' .or. namepro=='gp') id_pro="P-Group"
If(namepro=='GroupF'.or. namepro=='groupf' .or. namepro=='gf') id_pro="Group-Fast"
If(namepro=='GroupS'.or. namepro=='groups' .or. namepro=='gs') id_pro="Group-Slow"


If(namepro=='PFactP'.or. namepro=='pfoupp' .or. namepro=='pfp') id_pro="Power-Flow-P"
If(namepro=='PFactF'.or. namepro=='pfoupf' .or. namepro=='pff') id_pro="Power-Flow-Fast"
If(namepro=='PFactS'.or. namepro=='pfoups' .or. namepro=='pfs') id_pro="Power-Flow-Slow"
If(namepro=='km'    .or. namepro=='Km'     .or. namepro=='KM')  id_pro="Conductivity"

   filename=trim(id_pro)//".html"
  open(66, FILE=filename,STATUS='replace',ACTION='write')
  
 !========================================================================== 
  
if (namepro=="young") then
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()
  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Young.html* was generated."
endif
!###################################################################################
 if (namepro=="bulk") then
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'

  call swin_web(namepro)
  call stitle_web()
  naumbers=1
  type_pro= 'max'
  call start_layout_web()
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()
  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )


  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Bulk.html* was generated."

 endif
!###################################################################################
  if (namepro=="shear") then

  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  naumbers      = 1
  type_pro      = 'max'
  contours_line = 'false'
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()

  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,0.25)
  call contours_web(contours_line) 

!!!!!!!!!!!!!!!!!!!!!
  naumbers=2
  type_pro= 'min'
  contours_line = 'true'
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()

  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Shear.html* was generated."
endif
!###################################################################################
if (namepro=="comp") then

  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  naumbers      = 1
  type_pro      = 'max'
  contours_line = 'false'
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()

  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,0.25)
  call contours_web(contours_line) 

!!!!!!!!!!!!!!!!!!!!!
  naumbers=2
  type_pro= 'min'
  contours_line = 'true'
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()

  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 

  !!!!!!!!!!!!!!!!!!!!!
  naumbers=3
  type_pro= 'neg'
  contours_line = 'true'
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()

  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Compressibility.html* was generated."
endif
!###################################################################################
if (namepro=="poi") then

  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  naumbers      = 1
  type_pro      = 'max'
  contours_line = 'false'
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()

  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,0.25)
  call contours_web(contours_line) 

!!!!!!!!!!!!!!!!!!!!!
  naumbers=2
  type_pro= 'min'
  contours_line = 'true'
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()

  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 

  !!!!!!!!!!!!!!!!!!!!!
  naumbers=3
  type_pro= 'neg'
  contours_line = 'true'
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()

  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Poisson.html* was generated."
endif

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if (namepro=="hard") then
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()
  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Hardness.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="pp") then
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()
  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Phase-P.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="pf") then
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()
  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Phase-Fast.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="ps") then
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()
  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Phase-Slow.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="gp") then
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_data_velog_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_data_velog_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_data_velog_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()
  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Group-P.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="gf") then
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_data_velog_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_data_velog_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_data_velog_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()
  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Group-Fast.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="gs") then
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_data_velog_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_data_velog_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_data_velog_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()
  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Group-Slow.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="pfp") then
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()
  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Power-Flow-P.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="pfs") then
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()
  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Power-Flow-slow.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="pff") then
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_data_velop_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()
  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Power-Flow-Fast.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Min th. cond.
if (namepro=="km") then
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  call swin_web(namepro)
  call stitle_web()
  call start_layout_web()
  call start_trace_web(naumbers)
    cord_ename="x"
  call start_cord_web(cord_ename)
  call get_data_mthcond_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
    cord_ename="y"
  call start_cord_web(cord_ename)
  call get_data_mthcond_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
   call end_cord_web()
     cord_ename="z"
  call start_cord_web(cord_ename)
  call get_data_mthcond_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! call middle_cord_web()
  call end_cord_web()
  call colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  call setpara_web(zsmooth,showscale,1.0)
  call contours_web(contours_line) 
  call end_trace_web(naumbers)
  call end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Conductivity.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Min th. cond.
  close(66)
end program 
!-------------------------------------------
