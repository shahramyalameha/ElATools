
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D and 2D matereials; "gnu_conv: data file ( of AAEP code) to gpi file. 


PROGRAM gnu_conv
  USE ISO_FORTRAN_ENV
  INTEGER                          :: argl,i,N_frame=0,Nploter,Ng,Ns,N_arg
  ChARACTER(len=7)                 :: a=' ' 
  ChARACTER(len=10)                :: namepro=' ' ,val0,namepro2=' ',val1='',val='',clor_val=" "
  ChARACTER(len=7), dimension(10) :: arg_mane
  real(8)                          :: MAXpo, MAXyo, MAXsh
  
  clor_val=" "
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
 
 ! write(*,*) val,clor_val
  IF (val == "-h" .OR. val == "")THEN
       WRITE(*,*)'Using: dat2gnu_lapw [ Properties ] [hmap colors] in the  DatFile_*** folder' 
       WRITE(*,*)''
       call system ("sleep 0.5")
       WRITE(*,*)'[3D Properties]:                                         '
       WRITE(*,*)' poi     => Poisson’s ratio                         '
       WRITE(*,*)' shear   => Shear modulus                           '
       WRITE(*,*)' young   => Young’s modulus                         '
       WRITE(*,*)' bulk    => Bulk modulus                            '
       WRITE(*,*)' comp    => Linear compressibility                  '
       WRITE(*,*)' pp      => Phase velocity: P-mode                  '
       WRITE(*,*)' ps      => Phase velocity: Slow-mode               '
       WRITE(*,*)' pf      => Phase velocity: Fast-mode               '
       WRITE(*,*)' gp      => Group velocity: P-mode                  '
       WRITE(*,*)' gs      => Group velocity: Slow-mode               '
       WRITE(*,*)' gf      => Group velocity: Fast-mode               '
       WRITE(*,*)' pall    => Phase and PFA: all-mode                 '
       WRITE(*,*)' gall    => Group and PFA all-mode                  '
       WRITE(*,*)' hmpoi   => 2D heat mape of Poisson’s ratio         '
       WRITE(*,*)' hmcomp  => 2D heat mape of Linear compressibility  '
       WRITE(*,*)' hmshear => 2D heat mape of Shear modulus           '
       WRITE(*,*)' hmbulk  => 2D heat mape of Bulk modulus            '
       WRITE(*,*)' hmyoung => 2D heat mape of Young’s modulus         '
       WRITE(*,*)' hmpall  => 2D heat mape of Phase velocity: all-mode'
       WRITE(*,*)' hmgall  => 2D heat mape of Group velocity: all-mode'
       WRITE(*,*)' hmpfall => 2D heat mape of PFA: all-mode           '
       WRITE(*,*)''
       WRITE(*,*)'[2D Properties]:                                      '
       WRITE(*,*)' 2dpoi   => Poisson’s ratio                         '
       WRITE(*,*)' 2dyoung => Young’s modulus                         '
       WRITE(*,*)' 2dshear => Shear modulus                           '
       WRITE(*,*)' 2d      => Generate all propreites                 '
       WRITE(*,*)' '
       WRITE(*,*)'[hmap colors]:                                      '
       WRITE(*,*)'bbry     => black-blue-red-yellow                   '
       WRITE(*,*)'grv      => green-red-violet                        '
       WRITE(*,*)'bbvy     => black-blue-violet-yellow-white          '
       WRITE(*,*)'bgyr     => blue-green-yellow-red                   '
       WRITE(*,*)'bryw     => black-red-yellow-white                  '
       STOP
  ENDIF

   
IF (val=='all' .OR. val=='All') then
  do i=1,25
   IF (i==1) then;  val='poi';   call ploter(val,clor_val); endif
   IF (i==2) then;  val='comp';  call ploter(val,clor_val); endif
   IF (i==3) then;  val='shear'; call ploter(val,clor_val); endif
   IF (i==4) then;  val='sound'; call ploter(val,clor_val); endif
   IF (i==5) then;  val='bulk';  call ploter(val,clor_val); endif	
   IF (i==6) then;  val='young'; call ploter(val,clor_val); endif
   IF (i==7) then;  val='pugh';  call ploter(val,clor_val); endif
   IF (i==8) then;  val='pp';    call ploter(val,clor_val); endif
   IF (i==9) then;  val='pf';    call ploter(val,clor_val); endif
   IF (i==10) then; val='ps';    call ploter(val,clor_val); endif
   IF (i==10) then; val='gp';    call ploter(val,clor_val); endif
   IF (i==11) then; val='gf';    call ploter(val,clor_val); endif
   IF (i==12) then; val='gs';    call ploter(val,clor_val); endif
   IF (i==13) then; val='pall';  call ploter(val,clor_val); endif
   IF (i==14) then; val='gall';  call ploter(val,clor_val); endif
   IF (i==15) then; val='pfall'; call ploter(val,clor_val); endif

   IF (i==16) then;  val='hmpoi';   call ploter(val,clor_val); endif
   IF (i==17) then;  val='hmcomp';  call ploter(val,clor_val); endif
   IF (i==18) then;  val='hmshear'; call ploter(val,clor_val); endif
   IF (i==19) then;  val='hmsound'; call ploter(val,clor_val); endif
   IF (i==20) then;  val='hmbulk';  call ploter(val,clor_val); endif	
   IF (i==21) then;  val='hmyoung'; call ploter(val,clor_val); endif
   IF (i==22) then;  val='hmpugh';  call ploter(val,clor_val); endif
   IF (i==23) then; val='hmpall';  call ploter(val,clor_val); endif
   IF (i==24) then; val='hmgall';  call ploter(val,clor_val); endif
   IF (i==25) then; val='hmpfall'; call ploter(val,clor_val); endif
   write(*,*)'======='
  enddo
else
  call ploter(val,clor_val)
 endif
 OPEN(22,file='.MaMiout') 
 READ(22,*) MAXyo
 READ(22,*) MAXsh
 READ(22,*) MAXpo
close(22) 
!WRITE(*,*)MAXyo,MAXsh, MAXpo
  IF (val=='2d' .OR. val=='2D') then
    val='2dpoi';  call twoDplot(val,MAXpo, MAXyo, MAXsh) 
    val='2dyoung'; call twoDplot(val,MAXpo, MAXyo, MAXsh) 
    val='2dshear'; call twoDplot(val,MAXpo, MAXyo, MAXsh) 
  endif
  IF (val=='2dpoi' .OR. val=='2dpoisson') then
    val='2dpoi';  call twoDplot(val,MAXpo, MAXyo, MAXsh) 
  endif
  IF (val=='2dyoung' .OR. val=='2dyoun') then
    val='2dyoung'; call twoDplot(val,MAXpo, MAXyo, MAXsh) 
  endif
  IF (val=='2dshear' .OR. val=='2dshea') then
    val='2dshear'; call twoDplot(val,MAXpo, MAXyo, MAXsh) 
  endif
END PROGRAM
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

 SUBROUTINE ploter(val,clor_val)
   ChARACTER(len=10) :: val ,val0,clor_val
   DOUBLE PRECISION  :: max=0d0,&
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
   VV_Ss_PF_min

   if (val=='all' .OR. val=='poi' .OR. val=='comp'.OR. val=='shear' .OR.val=='sound' .OR. val=='bulk' .or. val=='young'&
                  .OR.val=='pugh' .OR. val=='pp' .OR.  val=='pf'.OR. val=='ps' .OR. val=='gp' .OR.val=='gf' &
                  .OR.val=='gs' .OR. val=='pall' .OR. val=='gall' .OR.val=='pfall' .OR. val=='hmpoi' .OR. val=='hmcomp'&
                  .OR.val=='hmshear' .OR.val=='hmsound' .OR. val=='hmbulk' &
                  .OR.val=='hmpugh' .OR. val=='hmpall' .OR. val=='hmgall' .OR.val=='hmpfall' ) then
     OPEN(32,file='.MaMiout',status='old')
     read(32,*) Maxyoung,Minyoung,Maxcomp,Mincomp,G_max2,G_min2,Maxbulk,Minbulk,Pratio_max,Pratio_min,maxEVaTMf,maxEVaLM,minEVaTMf,pugh_max2,pugh_min2
     !write(*,*)Maxyoung,Maxcomp,G_max2,Maxbulk,Pratio_max,pugh_max2,maxEVaTMf
     close(32)
     OPEN(30,file='.MaMiout2',status='old')
     read(30,*)VVP_P_max ,VVP_P_min ,VVP_Sf_max ,VVP_Sf_min ,VVP_Ss_max ,VVP_Ss_min ,VVG_P_max , &
               VVG_P_min ,VVG_Sf_max ,VVG_Sf_min ,VVG_Ss_max ,VVG_Ss_min,                        &
               VV_P_PF_max,VV_Sf_PF_max,VV_Ss_PF_max,VV_P_PF_min,VV_Sf_PF_min,VV_Ss_PF_min
     close(30)
   endif
 if (val=='poi' .OR. val=='poissons' ) then
        WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='poissons.gpi'); val0='poi' 	
   CALL setreset()
   CALL unset1() 
   WRITE(2,'(a)')' stats "2dcut_poisson.dat" u 2 nooutput'
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_poisson.dat" u 4 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif}'
   WRITE(2,'(a)')'stats "2dcut_poisson.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxipfa=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxipfa > maxi) {maxi=maxipfa}'
   CALL set1()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "2dcut_poisson.dat"  u 1:4 w l lc "red" lw 2 ,\'
   WRITE(2,'(a)')'    "2dcut_poisson.dat" u 1:3 w l lc "green" lw 2 ,\'
   WRITE(2,'(a)')'    "2dcut_poisson.dat" u 1:2 w l lc "blue" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')' stats "2dcut_poisson.dat" u 2 nooutput'
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_poisson.dat" u 4 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif}'
   WRITE(2,'(a)')'stats "2dcut_poisson.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxipfa=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxipfa > maxi) {maxi=maxipfa}' 
      CALL set1()
  CALL settit(val0)  
   WRITE(2,'(a)')'pl  "2dcut_poisson.dat"  u 1:4  w l lc "red" lw 2 ,\'
   WRITE(2,'(a)')'    "2dcut_poisson.dat" u 1:3  w l lc "green" lw 2 ,\'
   WRITE(2,'(a)')'    "2dcut_poisson.dat" u 1:2  w l lc "blue" lw 2'
        write(*,"(A,F4.2,A,F5.3,A)")" > Using: go to gnuplot, call 'poissons.gpi' '",Pratio_max/4,"' '",Pratio_max+0.1d0,"' (or other scale)  "
 endif

 if (val=='hmpoi' .OR. val=='hmpoissons' ) then
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='poissons_smap.gpi')
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')'set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')'set output  "poissons_smap.png"'
WRITE(2,'(a)')'set multiplot layout 1,3'
WRITE(2,'(a)')'set format z "%11.4e"'
WRITE(2,'(a)')'set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')'set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')'set view map'
IF      (clor_val=='bbry') then
 WRITE(2,'(a)')' set palette rgb 7,5,15'
else IF (clor_val=='grv') then
 WRITE(2,'(a)')' set palette rgb 3,11,6'
else IF (clor_val=='bbvy') then
 WRITE(2,'(a)')' set palette rgb 30,31,32'
else IF (clor_val=='bgyr') then
 WRITE(2,'(a)')' set palette rgb 33,13,10'
else IF (clor_val=='bryw') then
 WRITE(2,'(a)')' set palette rgb 34,35,36'
else
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF

WRITE(2,'(a)')'unset ztics'

WRITE(2,'(a)')'set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')'set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')'set size 0.333 ,1.1'
WRITE(2,'(a)')'set origin 0.003,-0.03'
WRITE(2,'(a)')'set title "({/Symbol n}_{ Max-Positive}) x 10^{-2}"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($11*100) w  pm3d notitle'
WRITE(2,'(a)')'set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.3333,-0.03'
WRITE(2,'(a)')'set title "({/Symbol n}_{ Min-Positive}) x 10^{-2}"'

WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($12*100) w  pm3d notitle'

WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.66,-0.03'
WRITE(2,'(a)')'set title "({/Symbol n}_{ Negative}) x 10^{-2}"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):(-1*$13*100) w  pm3d notitle'

WRITE(2,'(a)')'unset multiplot'
WRITE(2,'(a)')'unset output'
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'print "> poissons_smap.png was generated... "'
write(*,"(A)")" > Using: gnuplot poissons_smap.gpi"
endif
!///////////////////
 if (val=='comp' .OR. val=='compressibiliy' .or. val=='com') then
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='compressibiliy.gpi'); val0='com' 	
   CALL setreset()
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_comp.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_comp.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "2dcut_comp.dat" u 1:2 w l lc "green" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_comp.dat" u 1:3 w l lc "red" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_comp.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_comp.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
      CALL set1()
  CALL settit(val0)  
   WRITE(2,'(a)')'pl "2dcut_comp.dat" u 1:2 w l lc "green" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_comp.dat" u 1:3 w l lc "red" lw 2'
        write(*,"(A,F6.2,A,F6.2,A)")" > Using: go to gnuplot, call 'compressibiliy.gpi' '",Maxcomp/4,"' '",Maxcomp+1d0,"' (or other scale)  "
 endif
  !/////////////////
 if (val=='hmcomp' .OR. val=='hmcompressibiliy' .or. val=='hmcom') then
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='compressibiliy_smap.gpi');  	
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')' set output  "compressibiliy_smap.png"'
WRITE(2,'(a)')' set multiplot layout 1,2'
WRITE(2,'(a)')' set format z "%11.4e"'
WRITE(2,'(a)')' set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')' set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')' set view map'
IF      (clor_val=='bbry') then
 WRITE(2,'(a)')' set palette rgb 7,5,15'
else IF (clor_val=='grv') then
 WRITE(2,'(a)')' set palette rgb 3,11,6'
else IF (clor_val=='bbvy') then
 WRITE(2,'(a)')' set palette rgb 30,31,32'
else IF (clor_val=='bgyr') then
 WRITE(2,'(a)')' set palette rgb 33,13,10'
else IF (clor_val=='bryw') then
 WRITE(2,'(a)')' set palette rgb 34,35,36'
else
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF

WRITE(2,'(a)')' unset ztics'
WRITE(2,'(a)')' set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')' set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')' set size 0.333 ,1.1'
WRITE(2,'(a)')' set origin 0.15,-0.03'
WRITE(2,'(a)')' set title "({LC}_{ Posivive}) x 10^{2} (TPa^{-1})"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($9/100) w  pm3d notitle'
WRITE(2,'(a)')' set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.5,-0.03'
WRITE(2,'(a)')'set title "({LC}_{ Negative}) x 10^{2} (TPa^{-1})"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):(-1*$10/100) w  pm3d notitle'
WRITE(2,'(a)')' unset multiplot'
WRITE(2,'(a)')' unset output'
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')'  print "> compressibiliy_smap.png was generated... "'
write(*,"(A)")" > Using: gnuplot compressibiliy_smap.gpi"

endif
  !/////////////////
  if (val=='shear' .OR. val=='she' .or. val=='Shear') then
         WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='shear.gpi'); val0='she' 	
   CALL setreset()
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_shear.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_shear.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "2dcut_shear.dat" u 1:2 w l lc "blue" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_shear.dat" u 1:3 w l lc "green" lw 2,\'
   WRITE(2,'(a)')'   "2dcut_shear.dat" u 1:4 w l lc "red" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_shear.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_shear.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
  CALL settit(val0)  
   WRITE(2,'(a)')'pl "2dcut_shear.dat" u 1:2 w l lc "blue" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_shear.dat" u 1:3 w l lc "green" lw 2,\'
   WRITE(2,'(a)')'   "2dcut_shear.dat" u 1:4 w l lc "red" lw 2'
        write(*,"(A,F5.2,A,F6.2,A)")" > Using: go to gnuplot, call 'shear.gpi' '",(G_max2)/4d0,"' '",G_max2+1d0,"' (or other scale)  "
 endif
!\\\\\\\\\\\\\\\\\\\\\
 if (val=='hmshear' .OR. val=='hmshe' .or. val=='hmShear') then
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='shear_smap.gpi') 
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')' set output  "shear_smap.png"'
WRITE(2,'(a)')' set multiplot layout 1,2'
WRITE(2,'(a)')' set format z "%11.4e"'
WRITE(2,'(a)')' set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')' set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')' set view map'
IF      (clor_val=='bbry') then
 WRITE(2,'(a)')' set palette rgb 7,5,15'
else IF (clor_val=='grv') then
 WRITE(2,'(a)')' set palette rgb 3,11,6'
else IF (clor_val=='bbvy') then
 WRITE(2,'(a)')' set palette rgb 30,31,32'
else IF (clor_val=='bgyr') then
 WRITE(2,'(a)')' set palette rgb 33,13,10'
else IF (clor_val=='bryw') then
 WRITE(2,'(a)')' set palette rgb 34,35,36'
else
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')' unset ztics'
WRITE(2,'(a)')' set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')' set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')' set size 0.333 ,1.1'
WRITE(2,'(a)')' set origin 0.15,-0.03'
WRITE(2,'(a)')' set title "({G}_{max}) x 10^{2} (GPa)"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($3/100) w  pm3d notitle'
WRITE(2,'(a)')' set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.5,-0.03'
WRITE(2,'(a)')'set title "({G}_{min}) x 10^{2} (GPa)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($4/100) w  pm3d notitle'
 
WRITE(2,'(a)')' unset multiplot'
WRITE(2,'(a)')' unset output'
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' print "> shear_smap.png was generated... "'
write(*,"(A)")" > Using: gnuplot shear_smap.gpi"

endif
   !/////////////////

  if (val=='pugh' .OR. val=='pug' .or. val=='Pugh') then
         WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='pugh.gpi'); val0='pug' 	
   CALL setreset()
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_pugh.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_pugh.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "2dcut_pugh.dat"  u 1:2 w l lc "blue" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_pugh.dat" u 1:3 w l lc "green" lw 2,\'
   WRITE(2,'(a)')'   "2dcut_pugh.dat" u 1:4 w l lc "red" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_pugh.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_pugh.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
  CALL settit(val0)  
   WRITE(2,'(a)')'pl "2dcut_pugh.dat" u 1:2  w l lc "blue" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_pugh.dat" u 1:3 w l lc "green" lw 2,\'
   WRITE(2,'(a)')'   "2dcut_pugh.dat" u 1:4 w l lc "red" lw 2'
        write(*,"(A,F6.3,A,F6.3,A)")" > Using: go to gnuplot, call 'pugh.gpi' '",pugh_max2/4,"' '",pugh_max2+0.1d0,"' (or other scale)  "
 endif
 !\\\\\\\\\\\\\\\\\\\\\
 if (val=='hmpugh' .OR. val=='hmpug' .or. val=='hmPugh') then
    call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='phug_smap.gpi') 
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')' set output  "phug_smap.png"'
WRITE(2,'(a)')' set multiplot layout 1,2'
WRITE(2,'(a)')' set format z "%11.4e"'
WRITE(2,'(a)')' set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')' set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')' set view map'
IF      (clor_val=='bbry') then
 WRITE(2,'(a)')' set palette rgb 7,5,15'
else IF (clor_val=='grv') then
 WRITE(2,'(a)')' set palette rgb 3,11,6'
else IF (clor_val=='bbvy') then
 WRITE(2,'(a)')' set palette rgb 30,31,32'
else IF (clor_val=='bgyr') then
 WRITE(2,'(a)')' set palette rgb 33,13,10'
else IF (clor_val=='bryw') then
 WRITE(2,'(a)')' set palette rgb 34,35,36'
else
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')' unset ztics'
WRITE(2,'(a)')' set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')' set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')' set size 0.333 ,1.1'
WRITE(2,'(a)')' set origin 0.15,-0.03'
WRITE(2,'(a)')' set title "({Pugh}_{max}) x 10^{-2}"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($20*100) w  pm3d notitle'
WRITE(2,'(a)')' set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.5,-0.03'
WRITE(2,'(a)')'set title "({Pugh}_{min}) x 10^{-2}"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($21*100) w  pm3d notitle'
 
WRITE(2,'(a)')' unset multiplot'
WRITE(2,'(a)')' unset output'
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' print "> phug_smap.png was generated... "'
write(*,"(A)")" > Using: gnuplot phug_smap.gpi"

endif
  !/////////////////
  if (val=='sound' .OR. val=='sou' .or. val=='Sound') then
         WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='sound.gpi'); val0='sou' 	
   CALL setreset()
   CALL unset1() 
  WRITE(2,'(a)')"stats '2dcut_sound.dat' u 4 nooutput"
 WRITE(2,'(a)')'maxig=ceil(STATS_max)'
 WRITE(2,'(a)')'maxi=maxig'
 WRITE(2,'(a)')'stats "2dcut_sound.dat" u 3 nooutput'
 WRITE(2,'(a)')'maxif=ceil(STATS_max)'
 WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif}'
 WRITE(2,'(a)')'stats "2dcut_sound.dat" u 2 nooutput'
 WRITE(2,'(a)')'maxipfa=ceil(STATS_max)'
 WRITE(2,'(a)')'if (maxipfa > maxi) {maxi=maxipfa}'
   CALL set1()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "2dcut_sound.dat" u 1:4  w l lc "red" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_sound.dat" u 1:3  w l lc "green" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_sound.dat" u 1:2  w l lc "blue" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
  WRITE(2,'(a)')"stats '2dcut_sound.dat' u 4 nooutput"
 WRITE(2,'(a)')'maxig=ceil(STATS_max)'
 WRITE(2,'(a)')'maxi=maxig'
 WRITE(2,'(a)')'stats "2dcut_sound.dat" u 3 nooutput'
 WRITE(2,'(a)')'maxif=ceil(STATS_max)'
 WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif}'
 WRITE(2,'(a)')'stats "2dcut_sound.dat" u 2 nooutput'
 WRITE(2,'(a)')'maxipfa=ceil(STATS_max)'
 WRITE(2,'(a)')'if (maxipfa > maxi) {maxi=maxipfa}'
   CALL set1()
  CALL settit(val0)  
   WRITE(2,'(a)')'pl "2dcut_sound.dat" u 1:4 w l lc "red" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_sound.dat" u 1:3 w l lc "green" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_sound.dat" u 1:2 w l lc "blue" lw 2'
        write(*,"(A,F5.2,A,F6.2,A)")" > Using: go to gnuplot, call 'sound.gpi' '",maxEVaTMf/4.0d0,"' '",maxEVaTMf+1d0,"' (or other scale)  "
 endif
 !///////////////////

  if (val=='bulk' .OR. val=='bul' .or. val=='Bulk') then
  
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='bulk.gpi'); val0='bul' 	
   CALL setreset()
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_bulk.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_bulk.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "2dcut_bulk.dat" u 1:2 w l lc "red" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_bulk.dat" u 1:3 w l lc "green"lw 2 '
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_bulk.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_bulk.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
  CALL settit(val0)  
   WRITE(2,'(a)')'pl "2dcut_bulk.dat" u 1:2 w l lc "red" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_bulk.dat" u 1:3 w l lc "green"lw 2 '
   write(*,"(A,F7.2,A,F9.2,A)")" > Using: go to gnuplot, call 'bulk.gpi' '",Maxbulk*100d0/4,"' '",(Maxbulk*100d0)+1.d0,"' (or other scale)  "
 endif
!///////////////////

 if (val=='hmbulk' .OR. val=='hmbul' .or. val=='hmBulk') then
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='bulk_smap.gpi'); 
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')' set output  "bulk_smap.png"'
WRITE(2,'(a)')' set multiplot layout 1,1'
WRITE(2,'(a)')' set format z "%11.4e"'
WRITE(2,'(a)')' set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')' set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')' set view map'
IF      (clor_val=='bbry') then
 WRITE(2,'(a)')' set palette rgb 7,5,15'
else IF (clor_val=='grv') then
 WRITE(2,'(a)')' set palette rgb 3,11,6'
else IF (clor_val=='bbvy') then
 WRITE(2,'(a)')' set palette rgb 30,31,32'
else IF (clor_val=='bgyr') then
 WRITE(2,'(a)')' set palette rgb 33,13,10'
else IF (clor_val=='bryw') then
 WRITE(2,'(a)')' set palette rgb 34,35,36'
else
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')' unset ztics'
WRITE(2,'(a)')' set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')' set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')' set size 0.333 ,1.1'
WRITE(2,'(a)')' set origin 0.3333,-0.03'
WRITE(2,'(a)')' set title "({B}_{Posivive/Negative})x 10^{2} (GPa)"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($16/100) w  pm3d notitle'
WRITE(2,'(a)')' unset multiplot'
WRITE(2,'(a)')' unset output'
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' print "> bulk_smap.png was generated... "'
write(*,"(A)")" > Using: gnuplot bulk_smap.gpi"

endif
  !///////////////////
  if (val=='young' .OR. val=='you' .or. val=='Young') then
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='young.gpi'); val0='you' 	
   CALL setreset()
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_young.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_young.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "2dcut_young.dat" u 1:2 w l lc "green" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_young.dat" u 1:3 w l lc "blue" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_young.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_young.dat" u 2 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
  CALL settit(val0)  
   WRITE(2,'(a)')'pl "2dcut_young.dat" u 1:2 w l lc "green" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_young.dat" u 1:3 w l lc "blue" lw 2'
   write(*,"(A,F7.2,A,F8.2,A)")" > Using: go to gnuplot, call 'young.gpi' '",Maxyoung/2,"' '",Maxyoung+1d0,"'(or other scale)  "
   !write(*,*)Maxyoung
 endif
!///////////////////

 if (val=='hmyoung' .OR. val=='hmyou' .or. val=='hmYoung') then
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='young_smap.gpi'); 
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')' set output  "young_smap.png"'
WRITE(2,'(a)')' set multiplot layout 1,1'
WRITE(2,'(a)')' set format z "%11.4e"'
WRITE(2,'(a)')' set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')' set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')' set view map'
IF      (clor_val=='bbry') then
 WRITE(2,'(a)')' set palette rgb 7,5,15'
else IF (clor_val=='grv') then
 WRITE(2,'(a)')' set palette rgb 3,11,6'
else IF (clor_val=='bbvy') then
 WRITE(2,'(a)')' set palette rgb 30,31,32'
else IF (clor_val=='bgyr') then
 WRITE(2,'(a)')' set palette rgb 33,13,10'
else IF (clor_val=='bryw') then
 WRITE(2,'(a)')' set palette rgb 34,35,36'
else
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')' unset ztics'
WRITE(2,'(a)')' set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')' set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')' set size 0.333 ,1.1'
WRITE(2,'(a)')' set origin 0.3333,-0.03'
WRITE(2,'(a)')' set title "({E})x 10^{2} (GPa)"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($7/100) w  pm3d notitle'
WRITE(2,'(a)')' unset multiplot'
WRITE(2,'(a)')' unset output'
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' print "> young_smap.png was generated... "'
write(*,"(A)")" > Using: gnuplot young_smap.gpi"

endif
!///////////////
if (val=='hmphaseall'.or. val=='hmpall' .or. val=='hmpa') then
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
  open(2,file='phase_smap.gpi'); 
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')'set output  "phase_smap.png"'
WRITE(2,'(a)')'set multiplot layout 1,3'
WRITE(2,'(a)')'set format z "%11.4e"'
WRITE(2,'(a)')'set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')'set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')'set view map'
IF      (clor_val=='bbry') then
 WRITE(2,'(a)')' set palette rgb 7,5,15'
else IF (clor_val=='grv') then
 WRITE(2,'(a)')' set palette rgb 3,11,6'
else IF (clor_val=='bbvy') then
 WRITE(2,'(a)')' set palette rgb 30,31,32'
else IF (clor_val=='bgyr') then
 WRITE(2,'(a)')' set palette rgb 33,13,10'
else IF (clor_val=='bryw') then
 WRITE(2,'(a)')' set palette rgb 34,35,36'
else
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')'unset ztics'
WRITE(2,'(a)')'set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')'set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')'set size 0.333 ,1.1'
WRITE(2,'(a)')'set origin 0.003,-0.03'
WRITE(2,'(a)')'set title "V_{p}-P Mode (m/s)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($24) w  pm3d notitle'
WRITE(2,'(a)')'set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.3333,-0.03'
WRITE(2,'(a)')'set title "V_{p}-Fast Mode (m/s)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($26) w  pm3d notitle'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.66,-0.03'
WRITE(2,'(a)')'set title "V_{p}-Slow Mode (m/s)"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($28) w  pm3d notitle'
WRITE(2,'(a)')'unset multiplot'
WRITE(2,'(a)')'unset output'
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'print "> phase_smap.png was generated... "'
write(*,"(A)")" > Using: gnuplot phase_smap.gpi"
endif
!\\\\\\\\\\\\\\
if (val=='hmgroupall'.or. val=='hmgall' .or. val=='hmga') then
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
  open(2,file='group_smap.gpi');
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')'set output  "group_smap.png"'
WRITE(2,'(a)')'set multiplot layout 1,3'
WRITE(2,'(a)')'set format z "%11.4e"'
WRITE(2,'(a)')'set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')'set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')'set view map'
WRITE(2,'(a)')'set palette rgb 30,13,10'
WRITE(2,'(a)')'unset ztics'
WRITE(2,'(a)')'set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')'set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')'set size 0.333 ,1.1'
WRITE(2,'(a)')'set origin 0.003,-0.03'
WRITE(2,'(a)')'set title "V_{g}-P Mode (m/s)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($25) w  pm3d notitle'
WRITE(2,'(a)')'set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.3333,-0.03'
WRITE(2,'(a)')'set title "V_{g}-Fast Mode (m/s)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($27) w  pm3d notitle'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.66,-0.03'
WRITE(2,'(a)')'set title "V_{g}-Slow Mode (m/s)"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($29) w  pm3d notitle'
WRITE(2,'(a)')'unset multiplot'
WRITE(2,'(a)')'unset output'
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'print "> group_smap.png was generated... "'
write(*,"(A)")" > Using: gnuplot group_smap.gpi"
endif
!/////////////
  if (val=='PhaseP'.or. val=='phasep' .or. val=='pp') then
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='phase-p.gpi'); val0='pp'   
   CALL setreset()
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_pveloc.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_pveloc.dat" u 2 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:2 w l lc "blue" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_pveloc.dat" u 1:2 w l lc "blue" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_pveloc.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_pveloc.dat" u 2 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
  CALL settit(val0)  
   WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:2  w l lc "blue" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_pveloc.dat" u 1:2  w l lc "blue" lw 2'
   write(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'phase-p.gpi' '",(VVP_P_max/4d0),"' '",VVP_P_max+1d0,"' (or other scale)  "
 endif 
!///////////////////

  if (val=='PhaseF'.or. val=='phasef' .or. val=='pf') then
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='phase-fast.gpi'); val0='pf'   
   CALL setreset()
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_pveloc.dat' u 3 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_pveloc.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:3 w l lc "green" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_pveloc.dat" u 1:3 w l lc "green" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_pveloc.dat' u 3 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_pveloc.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
  CALL settit(val0)  
   WRITE(2,'(a)')'pl "2dcut_pveloc.dat"  u 1:3 w l lc "green" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_pveloc.dat"  u 1:3 w l lc "green" lw 2'
   write(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'phase-fast.gpi' '",(VVP_Sf_max/4d0),"' '",VVP_Sf_max+1.d0,"' (or other scale)  "
 endif 
!///////////////////

  if (val=='PhaseS'.or. val=='phases' .or. val=='ps') then
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='phase-slow.gpi'); val0='ps'   
   CALL setreset()
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_pveloc.dat' u 4 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_pveloc.dat" u 4 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:4 w l lc "red" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_pveloc.dat" u 1:4 w l lc "red" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_pveloc.dat' u 4 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_pveloc.dat" u 4 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
  CALL settit(val0)  
   WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:4 w l lc "red" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_pveloc.dat" u 1:4 w l lc "red" lw 2'
   write(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'phase-slow.gpi' '",VVP_Ss_max/4.0d0,"' '",VVP_Ss_max+1d0,"' (or other scale)  "
 endif 
 !///////////////////

  if (val=='GroupP'.or. val=='groupp' .or. val=='gp') then
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='group-p.gpi'); val0='ps'   
   CALL setreset()
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_gveloc.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_gveloc.dat" u 2 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "2dcut_gveloc.dat"  u 1:2 w l lc "blue" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_gveloc.dat"  u 1:2 w l lc "blue" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_gveloc.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_gveloc.dat" u 2 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
  CALL settit(val0)  
   WRITE(2,'(a)')'pl "2dcut_gveloc.dat"  u 1:2 w l lc "blue" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_gveloc.dat"  u 1:2 w l lc "blue" lw 2'
   write(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'group-p.gpi' '",VVG_P_max/4d0,"' '",VVG_P_max+1d0,"' (or other scale)  "
 endif 
 !///////////////////
  if (val=='GroupF'.or. val=='groupf' .or. val=='gf') then
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='group-fast.gpi'); val0='pf'   
   CALL setreset()
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_gveloc.dat' u 3 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_gveloc.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_gveloc.dat' u 3 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_gveloc.dat" u 3 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
  CALL settit(val0)  
   WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2'
   write(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'group-fast.gpi' '",VVG_Sf_max/4d0,"' '",VVG_Sf_max+1d0,"' (or other scale)  "
 endif 
!///////////////////

  if (val=='GroupS'.or. val=='groups' .or. val=='gs') then
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='group-slow.gpi'); val0='ps'   
   CALL setreset()
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_gveloc.dat' u 4 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_gveloc.dat" u 4 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:4 w l lc "red" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:4 w l lc "red" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_gveloc.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_gveloc.dat" u 2 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
  CALL settit(val0)  
   WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:4 w l lc "red" lw 2 ,\'
   WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:4 w l lc "red" lw 2'
   write(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'group-slow.gpi' '",VVG_Ss_max/4d0,"' '",VVG_Ss_max+1d0,"' (or other scale)  "
 endif 
 !///////////////////

 if (val=='phaseall'.or. val=='pall' .or. val=='pa') then
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='phase.gpi'); val0='pall'   
CALL setreset()
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_pveloc.dat' u 2 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_pveloc.dat" u 3 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0) 
WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:2 w l lc "red"   lw 2 title "P-mode",\'
WRITE(2,'(a)')'   "2dcut_pveloc.dat" u 1:3 w l lc "green" lw 2 title "Fast-mode",\'
WRITE(2,'(a)')'   "2dcut_pveloc.dat" u 1:4 w l lc "blue"  lw 2 title "Slow-mode"'
CALL setterm() 
CALL setoutput(val0)
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_pveloc.dat' u 2 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_pveloc.dat" u 3 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0)  
WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:2 w l lc "red"   lw 2 title "P-mode",\'
WRITE(2,'(a)')'   "2dcut_pveloc.dat" u 1:3 w l lc "green" lw 2 title "Fast-mode",\'
WRITE(2,'(a)')'   "2dcut_pveloc.dat" u 1:4 w l lc "blue"  lw 2 title "Slow-mode"'
write(*,*)" > Using: go to gnuplot, call 'phase.gpi' '10' '50'(or other scale)  "
endif 
 !///////////////////

if (val=='groupall'.or. val=='gall' .or. val=='ga') then
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='group.gpi'); val0='gall'   
CALL setreset()
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_gveloc.dat' u 2 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_gveloc.dat" u 3 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0) 
WRITE(2,'(a)')'set key'
WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:2 w l lc "red"   lw 2   title "P-mode",\'
WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2   title "Fast-mode",\'
WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:4 w l lc "blue"  lw 2   title "Slow-mode"'
CALL setterm() 
CALL setoutput(val0)
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_gveloc.dat' u 2 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_gveloc.dat" u 3 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0)  
WRITE(2,'(a)')'set key'
WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:2 w l lc "red"   lw 2   title "P-mode",\'
WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2   title "Fast-mode",\'
WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:4 w l lc "blue"  lw 2   title "Slow-mode"'
write(*,*)" > Using: go to gnuplot, call 'group.gpi' '5' '20' (or other scale)  "
endif
!\\\\\\\\\\\\\\
if (val=='hmpowerfall'.or. val=='hmpfall' .or. val=='hmpfa') then
    call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
  open(2,file='powerfolw_smap.gpi');
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')'set output  "powerfolw_smap.png"'
WRITE(2,'(a)')'set multiplot layout 1,3'
WRITE(2,'(a)')'set format z "%11.4e"'
WRITE(2,'(a)')'set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')'set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')'set view map'
IF      (clor_val=='bbry') then
 WRITE(2,'(a)')' set palette rgb 7,5,15'
else IF (clor_val=='grv') then
 WRITE(2,'(a)')' set palette rgb 3,11,6'
else IF (clor_val=='bbvy') then
 WRITE(2,'(a)')' set palette rgb 30,31,32'
else IF (clor_val=='bgyr') then
 WRITE(2,'(a)')' set palette rgb 33,13,10'
else IF (clor_val=='bryw') then
 WRITE(2,'(a)')' set palette rgb 34,35,36'
else
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')'unset ztics'
WRITE(2,'(a)')'set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')'set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')'set size 0.333 ,1.1'
WRITE(2,'(a)')'set origin 0.003,-0.03'
WRITE(2,'(a)')'set title "PFA-P Mode (Degree)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($30) w  pm3d notitle'
WRITE(2,'(a)')'set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.3333,-0.03'
WRITE(2,'(a)')'set title "PFA-Fast Mode (Degree)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($31) w  pm3d notitle'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.66,-0.03'
WRITE(2,'(a)')'set title "PFA-Slow Mode (Degree)"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($32) w  pm3d notitle'
WRITE(2,'(a)')'unset multiplot'
WRITE(2,'(a)')'unset output'
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'print "> powerfolw_smap.png was generated... "'
write(*,"(A)")" > Using: gnuplot powerfolw_smap.gpi"
endif
!/////////////
 !///////////////////

if (val=='powerfall'.or. val=='pfall' .or. val=='pfa') then
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='powerfolw.gpi'); val0='gall'   
CALL setreset()
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_pfaveloc.dat' u 2 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_pfaveloc.dat" u 3 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0) 
WRITE(2,'(a)')'pl "2dcut_pfaveloc.dat" u 1:($2*1000) w l lc "red" lw 2 ,\'
WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:($3*1000) w l lc "green" lw 2 ,\'
WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:($4*1000) w l lc "blue" lw 2'
CALL setterm() 
CALL setoutput(val0)
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_pfaveloc.dat' u 2 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_pfaveloc.dat" u 3 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0)  
WRITE(2,'(a)')'pl "2dcut_pfaveloc.dat" u 1:($2*1000) w l lc "red" lw 2 ,\'
WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:($3*1000) w l lc "green" lw 2 ,\'
WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:($4*1000) w l lc "blue" lw 2'
write(*,*)" > Using: go to gnuplot, call 'powerfolw.gpi' '1' '5'(or other scale)  "
endif
!100 WRITE(*,*)" > NOT FOUNDE MAX1 FILE"; STOP
!101 WRITE(*,*)" > NOT FOUNDE MAX2 FILE";STOP
 end SUBROUTINE  

 !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$4
  !2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2
 SUBROUTINE twoDplot(val,MAXpo, MAXyo, MAXsh )
     ChARACTER(len=10) :: val ,val0
     real(8)           :: MAXpo, MAXyo, MAXsh 

 if (val=='2dpoi' ) then
       ! WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='2Dpoissons.gpi'); val0='2poi' 	
   CALL setreset()
   CALL unset1() 
   CALL set1_2d()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "poisson_2d_sys.dat" u 1:4 w l lc "red"   lw 2 ,\'
   WRITE(2,'(a)')'   "poisson_2d_sys.dat" u 1:3 w l lc "green"  lw 2,\'
   WRITE(2,'(a)')'   #"poisson_2d_sys.dat" u 1:2 w l lc "blue" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   CALL set1_2d()
   CALL settit(val0)  
   WRITE(2,'(a)')'pl "poisson_2d_sys.dat" u 1:4 w l lc "red"   lw 2 ,\'
   WRITE(2,'(a)')'   "poisson_2d_sys.dat" u 1:3 w l lc "green"  lw 2,\'
   WRITE(2,'(a)')'   #"poisson_2d_sys.dat" u 1:2 w l lc "blue" lw 2'
        write(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call '2Dpoissons.gpi' '",MAXpo/4d0,"' '",MAXpo+0.1d0,"' (or other scale)  "
 endif
 
  if (val=='2dyoung') then
      ! WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='2Dyoung.gpi'); val0='2you' 	
   CALL setreset()
   CALL unset1() 
   CALL set1_2d()
   CALL settit(val0) 
   WRITE(2,'(a)')'pl "young_2d_sys.dat" w l lc "blue" lw 2'
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   CALL set1_2d()
   CALL settit(val0)  
   WRITE(2,'(a)')'pl "young_2d_sys.dat" w l lc "blue" lw 2'
   write(*,"(A,F6.2,A,F6.2,A)")" > Using: go to gnuplot, call '2Dyoung.gpi' '",MAXyo/4d0,"' '",MAXyo+2d0,"' (or other scale)  "
 endif
   if (val=='2dshear') then
      ! WRITE(*,'(2a)') val,'was READ well...'
      open(2,file='2Dshear.gpi'); val0='2she'  
      CALL setreset()
      CALL unset1() 
      CALL set1_2d()
      CALL settit(val0) 
      WRITE(2,'(a)')'pl "shear_2d_sys.dat" w l lc "blue" lw 2'
      CALL setterm() 
      CALL setoutput(val0)
      CALL unset1() 
      CALL set1_2d()
      CALL settit(val0)  
      WRITE(2,'(a)')'pl "shear_2d_sys.dat" w l lc "blue" lw 2'
      write(*,"(A,F6.2,A,F6.2,A)")" > Using: go to gnuplot, call '2Dshear.gpi' '",MAXsh/4d0,"' '",MAXsh+2d0,"' (or other scale)  "
 endif
 
 end SUBROUTINE
