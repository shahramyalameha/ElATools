
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
    DOUBLE PRECISION  ::&
       Maxyoung,  &
   Minyoung,  &
   Maxcomp,   &
   Mincomp,   &
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
   VVP_P_max, &
   VVP_P_min, &
   VVP_Sf_max,&
   VVP_Sf_min,&
   VVP_Ss_max,&
   VVP_Ss_min,&
   VVG_P_max, &
   VVG_P_min, &
   VVG_Sf_max,&
   VVG_Sf_min,&
   VVG_Ss_max,&
   VVG_Ss_min,&
   VV_P_PF_max, &
   VV_Sf_PF_max,&
   VV_Ss_PF_max,&
   VV_P_PF_min, &
   VV_Sf_PF_min,&
   VV_Ss_PF_min,&
   Ha_max2,     &
   Ha_min2, km_min,km_max 
    !=================v1.7.3=======================
  INTEGER :: iargc
  INTEGER :: argcount
  CHARACTER(LEN=80) :: wq_char
  !=================v1.7.3=======================                                                 
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
  !=================v1.7.3=======================  
  argcount=iargc()
  DO i=1,argcount
   CALL GETARG(i,wq_char)
   arg_mane(i)=wq_char
  END DO
  !=================v1.7.3======================= 
  
!  N_arg = 0
!  DO
!    CALL get_command_argument(N_arg, a)
!    IF (LEN_TRIM(a) == 0) EXIT

!    arg_mane(N_arg)=TRIM(a)
!    N_arg = N_arg+1
!  END DO

 val      = arg_mane(1)	
 clor_val = arg_mane(2)	
 
 cval1    = arg_mane(2)
 cval2    = arg_mane(3)
 cval3    = arg_mane(4)
 
 ! write(*,*) val,clor_val
  IF (val == "-h" .OR. val == "")THEN
       WRITE(*,*)'Using: dat2html.x [ Properties ] [ colors ] in the  DatFile_*** folder' 
       WRITE(*,*)''
       CALL system ("sleep 0.5")
       WRITE(*,*)'[3D Properties]:                                     '
       WRITE(*,*)' poi      => Poisson’s ratio                         '
       WRITE(*,*)' pugh     => Pugh’s ratio                            '       
       WRITE(*,*)' shear    => Shear modulus                           '
       WRITE(*,*)' young    => Young’s modulus                         '
       WRITE(*,*)' bulk     => Bulk modulus                            '
       WRITE(*,*)' comp     => Linear compressibility                  '
       WRITE(*,*)' hard     => Hardness                                '
       WRITE(*,*)' pp       => Phase velocity: P-mode                  '
       WRITE(*,*)' ps       => Phase velocity: Slow-mode               '
       WRITE(*,*)' pf       => Phase velocity: Fast-mode               '
       WRITE(*,*)' gp       => Group velocity: P-mode                  '
       WRITE(*,*)' gs       => Group velocity: Slow-mode               '
       WRITE(*,*)' gf       => Group velocity: Fast-mode               '
       WRITE(*,*)' pfp       => Power Flow angle (PFA): P-mode          '
       WRITE(*,*)' pfs       => Power Flow angle: Slow-mode             '
       WRITE(*,*)' pff       => Power Flow angle: Fast-mode             '
       WRITE(*,*)' km        => Min. thermal conductivity               '
       WRITE(*,*)'     '
       WRITE(*,*)'[2D Cut Properties]:                                  '       
       WRITE(*,*)' poi2d     => 2D cut of Poisson’s ratio               '
       WRITE(*,*)' pugh2d    => 2D cut of Pugh’s ratio                  '       
       WRITE(*,*)' shear2d   => 2D cut of Shear modulus                 '
       WRITE(*,*)' young2d   => 2D cut of Young’s modulus               '
       WRITE(*,*)' bulk2d    => 2D cut of Bulk modulus                  '
       WRITE(*,*)' comp2d    => 2D cut of Linear compressibility        '
       WRITE(*,*)' hard2d    => 2D cut of Hardness                      '
       WRITE(*,*)' pp2d      => 2D cut of Phase velocity: P-mode        '
       WRITE(*,*)' ps2d      => 2D cut of Phase velocity: Slow-mode     '
       WRITE(*,*)' pf2d      => 2D cut of Phase velocity: Fast-mode     '
       WRITE(*,*)' gp2d      => 2D cut of Group velocity: P-mode        '
       WRITE(*,*)' gs2d      => 2D cut of Group velocity: Slow-mode     '
       WRITE(*,*)' gf2d      => 2D cut of Group velocity: Fast-mode     '
       WRITE(*,*)' pfp2d     => 2D cut of Power Flow angle (PFA): P-mode'
       WRITE(*,*)' pfs2d     => 2D cut of Power Flow angle: Slow-mode   '
       WRITE(*,*)' pff2d     => 2D cut of Power Flow angle: Fast-mode   '
       WRITE(*,*)' km2d      => 2D cut of Min. thermal conductivity     '
 
       STOP
  endif
       namepro= val
  OPEN(123, FILE='HKL')
 READ(123,*) e1
 READ(123,*) e2
 READ(123,*) h_ex
 READ(123,*) k_ex
 READ(123,*) l_ex
 READ(123,*) ynveloc
 CLOSE(123)
 !WRITE(*,*) e1
 !WRITE(*,*) e2
 !WRITE(*,*) h_ex
 !WRITE(*,*) k_ex
 !WRITE(*,*) l_ex
 !WRITE(*,*) ynveloc

 IF (ynveloc=='N' .OR. ynveloc=='n')THEN
  if (val=='PhaseP'.or. val=='phasep' .or. val=='pp'   .or. &
      val=='PhaseF'.or. val=='phasef' .or. val=='pf'   .or. &
      val=='PhaseS'.or. val=='phases' .or. val=='ps'   .or. &
      val=='GroupP'.or. val=='groupp' .or. val=='gp'   .or. &
      val=='Groupf'.or. val=='groupf' .or. val=='gf'   .or. &
      val=='GroupS'.or. val=='groups' .or. val=='gs'   .or. &
      val=='PFactP'.or. val=='pfoupp' .or. val=='pfp'  .or. &
      val=='PFactF'.or. val=='pfoupf' .or. val=='pff'  .or. &
      val=='PFactS'.or. val=='pfoups' .or. val=='pfs'  .or. &
      val=='km'    .or. val=='Km'     .or. val=='KM'       )THEN
    WRITE(*,*) "Sorry! Your request is invalid!"
    CALL sleep(1)
    WRITE(*,*) "Have phase and group velocity calculations been performed?!"    
    stop
    ENDif
  endif  
  !$=====================================================================
      OPEN(32,file='.MaMiout',status='old')
     read(32,*) Maxyoung,Minyoung,Maxcomp,Mincomp,G_max2,G_min2,Maxbulk,Minbulk,Pratio_max,Pratio_min,maxEVaTMf,maxEVaLM,minEVaTMf,pugh_max2,pugh_min2,Ha_max2,Ha_min2
     !WRITE(*,*)Maxyoung,Maxcomp,G_max2,Maxbulk,Pratio_max,pugh_max2,maxEVaTMf
     close(32)
     OPEN(30,file='.MaMiout2',status='old')
     read(30,*)VVP_P_max ,VVP_P_min ,VVP_Sf_max ,VVP_Sf_min ,VVP_Ss_max ,VVP_Ss_min ,VVG_P_max , & !7
               VVG_P_min ,VVG_Sf_max ,VVG_Sf_min ,VVG_Ss_max ,VVG_Ss_min,                        & !12
               VV_P_PF_max,VV_Sf_PF_max,VV_Ss_PF_max,VV_P_PF_min,VV_Sf_PF_min,VV_Ss_PF_min,km_min,km_max
     close(30) 
    !$=====================================================================    
If(namepro=="poi"  .or. namepro=="poisson" ) id_pro="Poisson"
If(namepro=="pug"  .or. namepro=="pugh"    ) id_pro="Pugh"
If(namepro=="you"  .or. namepro=="young"   ) id_pro="Young"
If(namepro=="sh"   .or. namepro=="shear"   ) id_pro="Shear"
If(namepro=="com"  .or. namepro=="comp"    ) id_pro="Compressibility"
If(namepro=="bul"  .or. namepro=="bulk"    ) id_pro="Bulk"
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
If(namepro=='km'    .or. namepro=='Km'     .or. namepro=='KM' ) id_pro="Conductivity"

If(namepro=='you2d'   .or. namepro=='young2d'     .or. namepro=='Young2d' ) id_pro="Young-2Dcut"
If(namepro=='bul2d'   .or. namepro=='bulk2d'      .or. namepro=='Bulk2d'  ) id_pro="Bulk-2Dcut"
If(namepro=="pug2d"   .or. namepro=="pugh2d"                              ) id_pro="Pugh-2Dcut"
If(namepro=="sh2d"   .or. namepro=="shear2d"                              ) id_pro="Shear-2Dcut"
If(namepro=="com2d"  .or. namepro=="comp2d"                               ) id_pro="Compressibility-2Dcut"
If(namepro=="poi2d"  .or. namepro=="poisson2d"                            ) id_pro="Poisson-2Dcut"
If(namepro=="hard2d" .or. namepro=="hardness2d"                           ) id_pro="Hardness-2Dcut"
If(namepro=='PhaseP2d'.or. namepro=='phasep2d' .or. namepro=='pp2d'       ) id_pro="P-Phase-2Dcut"
If(namepro=='PhaseF2d'.or. namepro=='Phasef2d' .or. namepro=='pf2d'       ) id_pro="Phase-Fast-2Dcut"
If(namepro=='PhaseS2d'.or. namepro=='phases2d' .or. namepro=='ps2d'       ) id_pro="Phase-Slow-2Dcut"
If(namepro=='GroupP2d'.or. namepro=='groupp2d' .or. namepro=='gp2d'       ) id_pro="P-Group-2dcut"
If(namepro=='GroupF2d'.or. namepro=='groupf2d' .or. namepro=='gf2d'       ) id_pro="Group-Fast-2Dcut"
If(namepro=='GroupS2d'.or. namepro=='groups2d' .or. namepro=='gs2d'       ) id_pro="Group-Slow-2Dcut"
If(namepro=='PFactP2d'.or. namepro=='pfoupp2d' .or. namepro=='pfp2d'      ) id_pro="Power-Flow-P-2Dcut"
If(namepro=='PFactF2d'.or. namepro=='pfoupf2d' .or. namepro=='pff2d'      ) id_pro="Power-Flow-Fast-2Dcut"
If(namepro=='PFactS2d'.or. namepro=='pfoups2d' .or. namepro=='pfs2d'      ) id_pro="Power-Flow-Slow-2Dcut"

If(namepro=='km2d'    .or. namepro=='Km2d'     .or. namepro=='KM2d'       ) id_pro="Conductivity-2Dcut"

If(namepro=='2dyoung'   .or. namepro=='2dyou'     .or. namepro=='2dYoung' ) id_pro="Young-2D"
If(namepro=="2dsh"   .or. namepro=="2dshear"                              ) id_pro="Shear-2D"
If(namepro=="2dpoi"  .or. namepro=="2dpoisson"                            ) id_pro="Poisson-2D"

   filename=trim(id_pro)//".html"
  open(66, FILE=filename,STATUS='replace',ACTION='write')
    !========================================================================== 


  !========================================================================== 
  
if (namepro=="2dyoung") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        = 600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
  CALL buttone_polar_web(Maxcomp)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Young-2Dcut.html* was generated."
endif
!################################################2D----------------------  
if (namepro=="km2d") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        =600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
  CALL buttone_polar_web(km_max)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Conductivity-2Dcut.html* was generated."
endif 
  !========================================================================== 
  
if (namepro=="young2d") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        =600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
  CALL buttone_polar_web(Maxcomp)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Young-2Dcut.html* was generated."
endif 
 !========================================================================== 
  
if (namepro=="bulk2d") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        =600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(Maxbulk)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Bulk-2Dcut.html* was generated."
endif 
!################################################################################### 
   if (namepro=="pugh2d") THEN
  naumbers      = 1
  type_pro      = 'max'
  contours_line = 'true'
  height        =600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)

     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )

!!!!!!!!!!!!!!!!!!!!!
  naumbers=2
  type_pro= 'min'

  CALL start_trace_web(naumbers)

     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(pugh_max2)
  CALL end_polarlayout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Pugh-2Dcut.html* was generated."
endif
!################################################################################### 
   if (namepro=="shear2d") THEN
  naumbers      = 1
  type_pro      = 'max'
  contours_line = 'true'
  height        =600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)

     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )

!!!!!!!!!!!!!!!!!!!!!
  naumbers=2
  type_pro= 'min'

  CALL start_trace_web(naumbers)

     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(G_max2)
  CALL end_polarlayout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Shear-2Dcut.html* was generated."
endif
!################################################################################### 
   if (namepro=="com2d") THEN
  naumbers      = 1
  type_pro      = 'max'
  contours_line = 'true'
  height        =600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)

     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  naumbers=2
  type_pro= 'neg'

  CALL start_trace_web(naumbers)

     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(Maxcomp)
  CALL end_polarlayout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Compressibility-2Dcut.html* was generated."
endif
!################################################################################### 
   if (namepro=="poi2d") THEN
  naumbers      = 1
  type_pro      = 'max'
  contours_line = 'true'
  height        = 600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)

     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  naumbers=2
  type_pro= 'min'
  CALL start_trace_web(naumbers)

     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
  naumbers=3
  type_pro= 'neg'

   CALL start_trace_web(naumbers)

     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
  CALL buttone_polar_web(Pratio_max)
  CALL end_polarlayout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Poisson-2Dcut.html* was generated."
endif
!#######################################################################
if (namepro=="hard2d") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        = 600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(Ha_max2)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Hardness-2Dcut.html* was generated."
endif 
!#######################################################################
if (namepro=="pp2d") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        = 600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(VVP_P_max)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Phase-P-2Dcut.html* was generated."
endif
!#######################################################################
if (namepro=="pf2d") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        = 600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(VVP_Sf_max)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Phase-Fast-2Dcut.html* was generated."
endif
!#######################################################################
if (namepro=="ps2d") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        = 600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(VVP_Ss_max)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Phase-Slow-2Dcut.html* was generated."
endif
!#######################################################################
if (namepro=="gp2d") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        = 600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(VVG_P_max)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Group-P-2Dcut.html* was generated."
endif
!#######################################################################
if (namepro=="gf2d") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        = 600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(VVG_Sf_max)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Group-Fast-2Dcut.html* was generated."
endif
!#######################################################################
if (namepro=="gs2d") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        = 600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(VVG_Ss_max)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Group-Slow-2Dcut.html* was generated."
endif
!#######################################################################
if (namepro=="pfp2d") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        = 600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(VV_P_PF_max)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Power-Flow_P-2Dcut.html* was generated."
endif
!#######################################################################
if (namepro=="pff2d") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        = 600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(VV_Sf_PF_max)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Power-Flow_Fast-2Dcut.html* was generated."
endif
!#######################################################################
if (namepro=="pfs2d") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  height        = 600
  width         = 600
  CALL swin_web(namepro)
  CALL stitle_web2D()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
  
     cord_ename="t"
   CALL start_polar_web(cord_ename)
   CALL get_dataplotly_polar(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
   CALL end_polar_web()
 
    cord_ename="r"
  CALL start_polar_web(cord_ename)
  CALL get_dataplotly_polar(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
  CALL end_polar_web()
   
  CALL color_settings_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
 
  CALL end_trace_web(naumbers)
    CALL buttone_polar_web(VV_Ss_PF_max)
  CALL end_polarlayout_web(namepro,height,width)
   
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Power-Flow_Slow-2Dcut.html* was generated."
endif
 !==========================================================================   
if (namepro=="young") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()
  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Young.html* was generated."
endif
!###################################################################################
 if (namepro=="bulk") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'

  CALL swin_web(namepro)
  CALL stitle_web()
  naumbers=1
  type_pro= 'max'
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()
  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )


  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Bulk.html* was generated."

 endif
!################################################################################### 
   if (namepro=="pugh") THEN

  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  naumbers      = 1
  type_pro      = 'max'
  contours_line = 'true'
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()

  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.00)
  CALL contours_web(contours_line) 

!!!!!!!!!!!!!!!!!!!!!
  naumbers=2
  type_pro= 'min'
  contours_line = 'false'
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()

  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,0.5)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Pugh.html* was generated."
endif

!###################################################################################
  if (namepro=="shear") THEN

  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  naumbers      = 1
  type_pro      = 'max'
  contours_line = 'false'
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()

  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,0.25)
  CALL contours_web(contours_line) 

!!!!!!!!!!!!!!!!!!!!!
  naumbers=2
  type_pro= 'min'
  contours_line = 'true'
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()

  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Shear.html* was generated."
endif
!###################################################################################
if (namepro=="comp") THEN

  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  naumbers      = 1
  type_pro      = 'max'
  contours_line = 'false'
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()

  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,0.25)
  CALL contours_web(contours_line) 

!!!!!!!!!!!!!!!!!!!!!
  naumbers=2
  type_pro= 'min'
  contours_line = 'true'
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()

  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 

  !!!!!!!!!!!!!!!!!!!!!
  naumbers=3
  type_pro= 'neg'
  contours_line = 'true'
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()

  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Compressibility.html* was generated."
endif
!###################################################################################
if (namepro=="poi") THEN

  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  naumbers      = 1
  type_pro      = 'max'
  contours_line = 'false'
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()

  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,0.25)
  CALL contours_web(contours_line) 

!!!!!!!!!!!!!!!!!!!!!
  naumbers=2
  type_pro= 'min'
  contours_line = 'true'
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()

  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 

  !!!!!!!!!!!!!!!!!!!!!
  naumbers=3
  type_pro= 'neg'
  contours_line = 'true'
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()

  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Poisson.html* was generated."
endif

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

if (namepro=="hard") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_dataplotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()
  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Hardness.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="pp") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()
  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Phase-P.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="pf") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()
  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Phase-Fast.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="ps") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()
  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Phase-Slow.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="gp") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_data_velog_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_data_velog_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_data_velog_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()
  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Group-P.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="gf") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_data_velog_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_data_velog_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_data_velog_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()
  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Group-Fast.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="gs") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_data_velog_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_data_velog_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_data_velog_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()
  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Group-Slow.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="pfp") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()
  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Power-Flow-P.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="pfs") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()
  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Power-Flow-slow.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% velocitis
if (namepro=="pff") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_data_velop_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()
  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Power-Flow-Fast.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Min th. cond.
if (namepro=="km") THEN
  contours_line = 'true'
  naumbers      = 1
  type_pro      = 'max'
  CALL swin_web(namepro)
  CALL stitle_web()
  CALL start_layout_web()
  CALL start_trace_web(naumbers)
    cord_ename="x"
  CALL start_cord_web(cord_ename)
  CALL get_data_mthcond_plotly(namepro,1,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
    cord_ename="y"
  CALL start_cord_web(cord_ename)
  CALL get_data_mthcond_plotly(namepro,2,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
   CALL end_cord_web()
     cord_ename="z"
  CALL start_cord_web(cord_ename)
  CALL get_data_mthcond_plotly(namepro,3,n_phif,n_thetaf,cutmesh,type_pro)
 ! CALL middle_cord_web()
  CALL end_cord_web()
  CALL colorscale_web(namepro,naumbers,cval1 ,cval2 ,cval3 )
  CALL setpara_web(zsmooth,showscale,1.0)
  CALL contours_web(contours_line) 
  CALL end_trace_web(naumbers)
  CALL end_layout_web(namepro,height,width)
  WRITE(*,*)"meash:",n_phif,n_thetaf,cutmesh
  WRITE(*,*) "File *Conductivity.html* was generated."
endif
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Min th. cond.
  close(66)
end program 
!-------------------------------------------
