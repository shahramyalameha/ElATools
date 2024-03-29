
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! Main Program: For 3D and 2D matereials; "gnu_conv: data file ( of ELATOOLS code) to gpi file. 


PROGRAM gnu_conv
  USE ISO_FORTRAN_ENV
  INTEGER                          :: argl,i,N_frame=0,Nploter,Ng,Ns,N_arg
  ChARACTER(len=10)                :: a=' ' 
  ChARACTER(len=10)                :: namepro=' ' ,val0,namepro2=' ',val1='',val='',clor_val="",cval1 ,cval2 ,cval3 
  ChARACTER(len=10), dimension(10) :: arg_mane
  real(8)                          :: MAXpo, MAXyo, MAXsh
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ChARACTER(LEN=6)                 :: e1,e2
  ChARACTER(LEN=2)                 :: ynveloc
  INTEGER                          :: h_ex,k_ex,l_ex
  !=================v1.7.3=======================
  INTEGER :: iargc
  INTEGER :: argcount
  CHARACTER(LEN=80) :: wq_char
  !=================v1.7.3=======================
  
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
  
     ! Get command line args (Fortran 2003 standard)
!  N_arg = 0
!  DO
!    CALL get_command_argument(N_arg, a)
 !   IF (LEN_TRIM(a) == 0) EXIT

 !   arg_mane(N_arg)=TRIM(a)
 !   N_arg = N_arg+1
!  END DO

 val      = arg_mane(1)	
 clor_val = arg_mane(2)
 
 cval1    = arg_mane(2)
 cval2    = arg_mane(3)
 cval3    = arg_mane(4)
  !WRITE(*,*)cval1,cval2,cval3
 ! WRITE(*,*) val,clor_val
  IF (val == "-h" .OR. val == "")THEN 
       WRITE(*,*)'Using: dat2gnu.x [ Properties ] [hmap colors] in the  DatFile_*** folder' 
       WRITE(*,*)''
       call system ("sleep 0.5")
       WRITE(*,*)'[3D Properties]:                                      '
       WRITE(*,*)' poi      => Poisson’s ratio                          '
       WRITE(*,*)' pugh     => Phug’s ratio                          '       
       WRITE(*,*)' shear    => Shear modulus                            '
       WRITE(*,*)' young    => Young’s modulus                          '
       WRITE(*,*)' bulk     => Bulk modulus                             '
       WRITE(*,*)' comp     => Linear compressibility                   '
       WRITE(*,*)' hard     => Hardness                                 '       
       WRITE(*,*)' pp       => Phase velocity  : P-mode                   '
       WRITE(*,*)' ps       => Phase velocity  : Slow-mode                '
       WRITE(*,*)' pf       => Phase velocity  : Fast-mode                '
       WRITE(*,*)' gp       => Group velocity  : P-mode                   '
       WRITE(*,*)' gs       => Group velocity  : Slow-mode                '
       WRITE(*,*)' gf       => Group velocity  : Fast-mode                '
       WRITE(*,*)' pall     => Phase velocities: all-mode               '
       WRITE(*,*)' gall     => Group velocities: all-mode               '
       WRITE(*,*)' pfall    => Power Flow angle (PFA) : all-mode         '
       WRITE(*,*)' pfp      => Power Flow angles (PFA): P-mode          '
       WRITE(*,*)' pfs      => Power Flow angles      : Slow-mode       '
       WRITE(*,*)' pff      => Power Flow angles      : Fast-mode       '
       WRITE(*,*)' km       => Min. thermal conductivity                '
       WRITE(*,*)' hmpoi    => 2D heat map of Poisson’s ratio          '
       WRITE(*,*)' hmphug   => 2D heat map of Poisson’s ratio          '       
       WRITE(*,*)' hmcomp   => 2D heat map of Linear compressibility   '
       WRITE(*,*)' hmshear  => 2D heat map of Shear modulus            '
       WRITE(*,*)' hmbulk   => 2D heat map of Bulk modulus             '
       WRITE(*,*)' hmyoung  => 2D heat map of Young’s modulus          '
       WRITE(*,*)' hmhard   => 2D heat map of Hardness                 '
       WRITE(*,*)' hmpall   => 2D heat map of Phase velocity: all-mode '
       WRITE(*,*)' hmgall   => 2D heat map of Group velocity: all-mode '
       WRITE(*,*)' hmpfall  => 2D heat map of PFA: all-mode            '
       WRITE(*,*)' hmkm     => 2D heat map of Min. thermal conductivity'
       
       WRITE(*,*)' '
       WRITE(*,*)'[2D Properties]:                                      '
       WRITE(*,*)' 2dpoi     => Poisson’s ratio                         '
       WRITE(*,*)' 2dyoung   => Young’s modulus                         '
       WRITE(*,*)' 2dshear   => Shear modulus                           '
       WRITE(*,*)' 2d        => Generate all 2d-propreites              '
       WRITE(*,*)' phmpoi    => 2D polar-heat map of Poisson’s ratio   '
       WRITE(*,*)' phmyou    => 2D polar-heat map of Young’s modulus   '
       WRITE(*,*)' phmshe    => 2D polar-heat map of Shear modulus     '    
       WRITE(*,*)' phmall    => 2D polar-heat map of all properties    '    

       WRITE(*,*)' '
       WRITE(*,*)'[hmap colors]:                                       '
       WRITE(*,*)'bbry      => black-blue-red-yellow                   '
       WRITE(*,*)'grv       => green-red-violet                        '
       WRITE(*,*)'bbvy      => black-blue-violet-yellow-white          '
       WRITE(*,*)'bgyr      => blue-green-yellow-red                   '
       WRITE(*,*)'bryw      => black-red-yellow-white                  '
       STOP
  ENDIF
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
  IF (val=='PhaseP'.or. val=='phasep' .or. val=='pp' .or. &
      val=='PhaseF'.or. val=='phasef' .or. val=='pf' .or. &
      val=='PhaseS'.or. val=='phases' .or. val=='ps' .or. &
      val=='GroupP'.or. val=='groupp' .or. val=='gp' .or. &
      val=='Groupf'.or. val=='groupf' .or. val=='gf' .or. &
      val=='GroupS'.or. val=='groups' .or. val=='gs' .or. &
      val=='PFactP'.or. val=='pfoupp' .or. val=='pfp' .or. &
      val=='PFactF'.or. val=='pfoupf' .or. val=='pff' .or. &
      val=='PFactS'.or. val=='pfoups' .or. val=='pfs' .or. &
      val=='km'    .or. val=='Km'     .or. val=='KM' )THEN
     WRITE(*,*) "Sorry! Your request is invalid!"
     call sleep(1)
     WRITE(*,*) "Have phase and group velocity calculations been performed?!"    
     stop
  ENDIF
 ENDIF
   
IF (val=='all' .OR. val=='All') THEN
  do i=1,30
   IF (i==1) THEN;  val='poi'  ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==2) THEN;  val='comp' ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==3) THEN;  val='shear'; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==4) THEN;  val='sound'; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==5) THEN;  val='bulk' ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF	
   IF (i==6) THEN;  val='young'; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==7) THEN;  val='pugh' ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==8) THEN;  val='pp'   ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==9) THEN;  val='pf'   ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==10) THEN; val='ps'   ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==11) THEN; val='gp'   ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==12) THEN; val='gf'   ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==13) THEN; val='gs'   ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==14) THEN; val='pall' ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==15) THEN; val='gall' ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==16) THEN; val='pfall'; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF

   IF (i==17) THEN;  val='hmpoi'  ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==18) THEN;  val='hmcomp' ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==19) THEN;  val='hmshear'; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==20) THEN;  val='hmsound'; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==21) THEN;  val='hmbulk' ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF	
   IF (i==22) THEN;  val='hmyoung'; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==23) THEN;  val='hmpugh' ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==24) THEN; val='hmpall'  ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==25) THEN; val='hmgall'  ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==26) THEN; val='hmpfall' ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
! new!
   IF (i==27) THEN; val='hard'  ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==28) THEN; val='hmhard'; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
! new 1.7
   IF (i==29) THEN; val='km'    ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==30) THEN; val='hmkm'  ; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   IF (i==31) THEN; val='hmphug'; call ploter(val,clor_val,cval1,cval2,cval3); ENDIF
   WRITE(*,*)'======='
  enddo
    ELSE
      CALL ploter(val,clor_val,cval1,cval2,cval3)
ENDIF
 

!WRITE(*,*)MAXyo,MAXsh, MAXpo
  IF (val=='2d' .OR. val=='2D') THEN
    OPEN(22,file='.MaMiout') 
    READ(22,*) MAXyo
    READ(22,*) MAXsh
    READ(22,*) MAXpo
    close(22) 
    val='2dpoi'  ; call twoDplot(val,MAXpo, MAXyo, MAXsh,cval1,cval2,cval3) 
    val='2dyoung'; call twoDplot(val,MAXpo, MAXyo, MAXsh,cval1,cval2,cval3) 
    val='2dshear'; call twoDplot(val,MAXpo, MAXyo, MAXsh,cval1,cval2,cval3) 
  ENDIF
  IF (val=='2dpoi' .OR. val=='2dpoisson') THEN
    OPEN(22,file='.MaMiout') 
    READ(22,*) MAXyo
    READ(22,*) MAXsh
    READ(22,*) MAXpo
    close(22) 
    val='2dpoi';  call twoDplot(val,MAXpo, MAXyo, MAXsh,cval1,cval2,cval3) 
  ENDIF
  IF (val=='2dyoung' .OR. val=='2dyoun') THEN
    OPEN(22,file='.MaMiout') 
    READ(22,*) MAXyo
    READ(22,*) MAXsh
    READ(22,*) MAXpo
    close(22)   
    val='2dyoung'; call twoDplot(val,MAXpo, MAXyo, MAXsh,cval1,cval2,cval3) 
  ENDIF
  IF (val=='2dshear' .OR. val=='2dshea') THEN
    OPEN(22,file='.MaMiout') 
    READ(22,*) MAXyo
    READ(22,*) MAXsh
    READ(22,*) MAXpo
    close(22) 
    val='2dshear'; call twoDplot(val,MAXpo, MAXyo, MAXsh,cval1,cval2,cval3) 
  ENDIF

  IF (val=='phmshear' .OR. val=='phmshe') THEN
    val='phmshe'; call twoD_phm(val,clor_val)
  ENDIF  
  IF (val=='phmpoisson' .OR. val=='phmpoi') THEN
    val='phmpoi'; call twoD_phm(val,clor_val)
  ENDIF 
  IF (val=='phmyoung' .OR. val=='phmyou') THEN
    val='phmyon'; call twoD_phm(val,clor_val)
  ENDIF 
   IF (val=='phmall' .OR. val=='phma') THEN
    val='phmall'; call twoD_phm(val,clor_val)
  ENDIF 

END PROGRAM
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

 SUBROUTINE ploter(val,clor_val,cval1,cval2,cval3)
   ChARACTER(len=10) :: val ,val0,clor_val,cval1,cval2,cval3
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

   if (val=='all' .OR. val=='poi' .OR. val=='comp'.OR. val=='shear' .OR.val=='sound' .OR. val=='bulk' .or. val=='young'&
                  .OR.val=='pugh' .OR. val=='pp' .OR.  val=='pf'.OR. val=='ps' .OR. val=='gp' .OR.val=='gf' &
                  .OR.val=='gs' .OR. val=='pall' .OR. val=='gall' .OR.val=='pfall' .OR. val=='hmpoi'.OR. val=='hmpugh' .OR. val=='hmcomp'&
                  .OR.val=='hmshear' .OR.val=='hmsound' .OR. val=='hmbulk' &
                  .OR.val=='hmpugh' .OR. val=='hmpall' .OR. val=='hmgall' .OR.val=='hmpfall' .OR. val == "hard" &
                  .OR. val == "hmhard".OR. val == "km" .OR. val == "hmkm") THEN
                  
     OPEN(32,file='.MaMiout',status='old')
     read(32,*) Maxyoung,Minyoung,Maxcomp,Mincomp,G_max2,G_min2,Maxbulk,Minbulk,Pratio_max,Pratio_min,maxEVaTMf,maxEVaLM,minEVaTMf,pugh_max2,pugh_min2,Ha_max2,Ha_min2
     !WRITE(*,*)Maxyoung,Maxcomp,G_max2,Maxbulk,Pratio_max,pugh_max2,maxEVaTMf
     close(32)
     OPEN(30,file='.MaMiout2',status='old')
     read(30,*)VVP_P_max ,VVP_P_min ,VVP_Sf_max ,VVP_Sf_min ,VVP_Ss_max ,VVP_Ss_min ,VVG_P_max , & !7
               VVG_P_min ,VVG_Sf_max ,VVG_Sf_min ,VVG_Ss_max ,VVG_Ss_min,                        & !12
               VV_P_PF_max,VV_Sf_PF_max,VV_Ss_PF_max,VV_P_PF_min,VV_Sf_PF_min,VV_Ss_PF_min,km_min,km_max
     close(30)
   ENDIF
 if (val=='poi' .OR. val=='poissons' ) THEN
        WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='poissons.gpi'); val0='poi' 	
   Call copyri()
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
   IF (cval1=="n" .or. cval2=="n" .or. cval3=="n") THEN
     WRITE(2,'(a)')'pl  "2dcut_poisson.dat"  u 1:4 w l lc "red"   lw 2 title "Negative",\'
     WRITE(2,'(a)')'    "2dcut_poisson.dat" u 1:3 w l lc  "green" lw 2 title "Min. positive",\'
     WRITE(2,'(a)')'    "2dcut_poisson.dat" u 1:2 w l lc  "blue"  lw 2 title "Max. positive"'
     ELSE
     WRITE(2,'(4a)')'pl  "2dcut_poisson.dat"  u 1:4 w l lc "',cval1,'"   lw 2  title "Negative",\'
     WRITE(2,'(4a)')'    "2dcut_poisson.dat" u 1:3 w l lc  "',cval2,'" lw 2 title "Min. positive",\'
     WRITE(2,'(4a)')'    "2dcut_poisson.dat" u 1:2 w l lc  "',cval3,'"  lw 2title "Max. positive"'
   ENDIF
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
   IF (cval1=="n" .or. cval2=="n" .or. cval3=="n") THEN
     WRITE(2,'(a)')'pl  "2dcut_poisson.dat"  u 1:4 w l lc "red"   lw 2 title "Negative",\'
     WRITE(2,'(a)')'    "2dcut_poisson.dat" u 1:3 w l lc  "green" lw 2 title "Min. positive",\'
     WRITE(2,'(a)')'    "2dcut_poisson.dat" u 1:2 w l lc  "blue"  lw 2 title "Max. positive"'
   ELSE
     WRITE(2,'(4a)')'pl  "2dcut_poisson.dat"  u 1:4 w l lc "',cval1,'"   lw 2  title "Negative",\'
     WRITE(2,'(4a)')'    "2dcut_poisson.dat" u 1:3 w l lc  "',cval2,'" lw 2 title "Min. positive",\'
     WRITE(2,'(4a)')'    "2dcut_poisson.dat" u 1:2 w l lc  "',cval3,'"  lw 2title "Max. positive"'  
   ENDIF     
        WRITE(*,"(A,F4.2,A,F5.3,A)")" > Using: go to gnuplot, call 'poissons.gpi' '",Pratio_max/4,"' '",Pratio_max+0.1d0,"' (or other scale)  "
 ENDIF

 if (val=='hmpoi' .OR. val=='hmpoissons' ) THEN
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='poissons_smap.gpi')
Call copyri()
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')'set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')'set output  "poissons_smap.png"'
WRITE(2,'(a)')'set multiplot layout 1,3'
WRITE(2,'(a)')'set format z "%11.4e"'
WRITE(2,'(a)')'set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')'set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')'set view map'
IF      (clor_val=='bbry') THEN
 WRITE(2,'(a)')' set palette rgb 7,5,15'
ELSE IF (clor_val=='grv') THEN
 WRITE(2,'(a)')' set palette rgb 3,11,6'
ELSE IF (clor_val=='bbvy') THEN
 WRITE(2,'(a)')' set palette rgb 30,31,32'
ELSE IF (clor_val=='bgyr') THEN
 WRITE(2,'(a)')' set palette rgb 33,13,10'
ELSE IF (clor_val=='bryw') THEN
 WRITE(2,'(a)')' set palette rgb 34,35,36'
ELSE
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF

WRITE(2,'(a)')'unset ztics'

WRITE(2,'(a)')'set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')'set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')'set size 0.333 ,1.1'
WRITE(2,'(a)')'set origin 0.003,-0.03'
WRITE(2,'(a)')'set cblabel "({/Symbol n}_{ Max-Positive}) x 10^{-2}"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($11*100) w  pm3d notitle'
WRITE(2,'(a)')'set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.3333,-0.03'
WRITE(2,'(a)')'set cblabel "({/Symbol n}_{ Min-Positive}) x 10^{-2}"'

WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($12*100) w  pm3d notitle'

WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.66,-0.03'
WRITE(2,'(a)')'set cblabel "({/Symbol n}_{ Negative}) x 10^{-2}"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):(-1*$13*100) w  pm3d notitle'

WRITE(2,'(a)')'unset multiplot'
WRITE(2,'(a)')'unset output'
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'print "> poissons_smap.png was generated... "'
WRITE(*,"(A)")" > Using: gnuplot poissons_smap.gpi"
ENDIF
!///////////////////
 if (val=='comp' .OR. val=='compressibiliy' .or. val=='com') THEN
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='compressibiliy.gpi'); val0='com' 	
   Call copyri()
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
   IF (cval1=="n" .or. cval2=="n" ) THEN
    WRITE(2,'(a)')'pl "2dcut_comp.dat" u 1:2 w l lc "green" lw 2 title "Positive",\'
    WRITE(2,'(a)')'   "2dcut_comp.dat" u 1:3 w l lc "red"   lw 2 title "Negative"'
   ELSE
    WRITE(2,'(4a)')'pl "2dcut_comp.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Positive",\'
    WRITE(2,'(4a)')'   "2dcut_comp.dat" u 1:3 w l lc "',cval2,'" lw 2 title "Negative"'
   ENDIF
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
   IF (cval1=="n" .or. cval2=="n" ) THEN
     WRITE(2,'(a)')'pl "2dcut_comp.dat" u 1:2 w l lc "green" lw 2 title "Positive",\'
     WRITE(2,'(a)')'   "2dcut_comp.dat" u 1:3 w l lc "red"   lw 2 title "Negative"'
   ELSE
     WRITE(2,'(4a)')'pl "2dcut_comp.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Positive",\'
     WRITE(2,'(4a)')'   "2dcut_comp.dat" u 1:3 w l lc "',cval2,'" lw 2 title "Negative"'
   ENDIF
        WRITE(*,"(A,F6.2,A,F6.2,A)")" > Using: go to gnuplot, call 'compressibiliy.gpi' '",Maxcomp/4,"' '",Maxcomp+1d0,"' (or other scale)  "
 ENDIF
  !/////////////////
 if (val=='hmcomp' .OR. val=='hmcompressibiliy' .or. val=='hmcom') THEN
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='compressibiliy_smap.gpi');  	
Call copyri()
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')' set output  "compressibiliy_smap.png"'
WRITE(2,'(a)')' set multiplot layout 1,2'
WRITE(2,'(a)')' set format z "%11.4e"'
WRITE(2,'(a)')' set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')' set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')' set view map'
IF      (clor_val=='bbry') THEN
 WRITE(2,'(a)')' set palette rgb 7,5,15'
ELSE IF (clor_val=='grv') THEN
 WRITE(2,'(a)')' set palette rgb 3,11,6'
ELSE IF (clor_val=='bbvy') THEN
 WRITE(2,'(a)')' set palette rgb 30,31,32'
ELSE IF (clor_val=='bgyr') THEN
 WRITE(2,'(a)')' set palette rgb 33,13,10'
ELSE IF (clor_val=='bryw') THEN
 WRITE(2,'(a)')' set palette rgb 34,35,36'
ELSE
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF

WRITE(2,'(a)')' unset ztics'
WRITE(2,'(a)')' set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')' set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')' set size 0.333 ,1.1'
WRITE(2,'(a)')' set origin 0.15,-0.03'
WRITE(2,'(a)')' set cblabel "({LC}_{ Posivive}) x 10^{2} (TPa^{-1})"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($9/100) w  pm3d notitle'
WRITE(2,'(a)')' set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.5,-0.03'
WRITE(2,'(a)')'set cblabel "({LC}_{ Negative}) x 10^{2} (TPa^{-1})"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):(-1*$10/100) w  pm3d notitle'
WRITE(2,'(a)')' unset multiplot'
WRITE(2,'(a)')' unset output'
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')'  print "> compressibiliy_smap.png was generated... "'
WRITE(*,"(A)")" > Using: gnuplot compressibiliy_smap.gpi"

ENDIF
  !/////////////////
  if (val=='shear' .OR. val=='she' .or. val=='Shear') THEN
         WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='shear.gpi'); val0='she' 	
   Call copyri()
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
   IF (cval1=="n" .or. cval2=="n") THEN
    WRITE(2,'(a)')'pl "2dcut_shear.dat" u 1:2 w l lc "blue" lw 2 title "Max. Shear",\'
    WRITE(2,'(a)')'   "2dcut_shear.dat" u 1:3 w l lc "green" lw 2 title "Min. Shear"'
   ! WRITE(2,'(a)')'   "2dcut_shear.dat" u 1:4 w l lc "red" lw 2'
   ELSE
    WRITE(2,'(4a)')'pl "2dcut_shear.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Max. Shear",\'
    WRITE(2,'(4a)')'   "2dcut_shear.dat" u 1:3 w l lc "',cval2,'" lw 2 title "Min. Shear"'
    !WRITE(2,'(3a)')'   "2dcut_shear.dat" u 1:4 w l lc "',cval2,'" lw 2'    
   ENDIF
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
   IF (cval1=="n" .or. cval2=="n") THEN
    WRITE(2,'(a)')'pl "2dcut_shear.dat" u 1:2 w l lc "blue" lw 2 title "Max. Shear",\'
    WRITE(2,'(a)')'   "2dcut_shear.dat" u 1:3 w l lc "green" lw 2 title "Min. Shear"'
   ! WRITE(2,'(a)')'   "2dcut_shear.dat" u 1:4 w l lc "red" lw 2'
   ELSE
    WRITE(2,'(4a)')'pl "2dcut_shear.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Max. Shear",\'
    WRITE(2,'(4a)')'   "2dcut_shear.dat" u 1:3 w l lc "',cval2,'" lw 2 title "Min. Shear"'
    !WRITE(2,'(3a)')'   "2dcut_shear.dat" u 1:4 w l lc "',cval2,'" lw 2'    
   ENDIF
        WRITE(*,"(A,F5.2,A,F6.2,A)")" > Using: go to gnuplot, call 'shear.gpi' '",(G_max2)/4d0,"' '",G_max2+1d0,"' (or other scale)  "
 ENDIF
!\\\\\\\\\\\\\\\\\\\\\
 if (val=='hmshear' .OR. val=='hmshe' .or. val=='hmShear') THEN
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='shear_smap.gpi') 
Call copyri()
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')' set output  "shear_smap.png"'
WRITE(2,'(a)')' set multiplot layout 1,2'
WRITE(2,'(a)')' set format z "%11.4e"'
WRITE(2,'(a)')' set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')' set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')' set view map'
IF      (clor_val=='bbry') THEN
 WRITE(2,'(a)')' set palette rgb 7,5,15'
ELSE IF (clor_val=='grv') THEN
 WRITE(2,'(a)')' set palette rgb 3,11,6'
ELSE IF (clor_val=='bbvy') THEN
 WRITE(2,'(a)')' set palette rgb 30,31,32'
ELSE IF (clor_val=='bgyr') THEN
 WRITE(2,'(a)')' set palette rgb 33,13,10'
ELSE IF (clor_val=='bryw') THEN
 WRITE(2,'(a)')' set palette rgb 34,35,36'
ELSE
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')' unset ztics'
WRITE(2,'(a)')' set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')' set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')' set size 0.333 ,1.1'
WRITE(2,'(a)')' set origin 0.15,-0.03'
WRITE(2,'(a)')' set cblabel "({G}_{max}) x 10^{2} (GPa)"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($3/100) w  pm3d notitle'
WRITE(2,'(a)')' set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.5,-0.03'
WRITE(2,'(a)')'set cblabel "({G}_{min}) x 10^{2} (GPa)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($4/100) w  pm3d notitle'
 
WRITE(2,'(a)')' unset multiplot'
WRITE(2,'(a)')' unset output'
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' print "> shear_smap.png was generated... "'
WRITE(*,"(A)")" > Using: gnuplot shear_smap.gpi"

ENDIF
   !/////////////////

  if (val=='pugh' .OR. val=='pug' .or. val=='Pugh') THEN
         WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='pugh.gpi'); val0='pug' 	
   Call copyri()
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
   IF (cval1=="n" .or. cval2=="n".or. cval3=="n") THEN
    WRITE(2,'(a)')'pl "2dcut_pugh.dat"  u 1:2 w l lc "blue" lw 2 title "Max.",\'
    WRITE(2,'(a)')'   "2dcut_pugh.dat" u 1:3 w l lc "green" lw 2 title "Min.",\'
    WRITE(2,'(a)')'   "2dcut_pugh.dat" u 1:4 w l lc "red" lw 2 title "Negative"'
   ELSE
    WRITE(2,'(4a)')'pl "2dcut_pugh.dat"  u 1:2 w l lc "',cval1,'" lw 2 title "Max.",\'
    WRITE(2,'(4a)')'   "2dcut_pugh.dat" u 1:3 w l lc "',cval2,'" lw 2 title "Min.",\'
    WRITE(2,'(4a)')'   "2dcut_pugh.dat" u 1:4 w l lc "',cval3,'" lw 2 title "Negative"'
   ENDIF    
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
   IF (cval1=="n" .or. cval2=="n".or. cval3=="n") THEN
    WRITE(2,'(a)')'pl "2dcut_pugh.dat"  u 1:2 w l lc "blue" lw 2 title "Max.",\'
    WRITE(2,'(a)')'   "2dcut_pugh.dat" u 1:3 w l lc "green" lw 2 title "Min.",\'
    WRITE(2,'(a)')'   "2dcut_pugh.dat" u 1:4 w l lc "red" lw 2 title "Negative"'
   ELSE
    WRITE(2,'(3a)')'pl "2dcut_pugh.dat"  u 1:2 w l lc "',cval1,'" lw 2 title "Max.",\'
    WRITE(2,'(3a)')'   "2dcut_pugh.dat" u 1:3 w l lc "',cval2,'" lw 2 title "Min.",\'
    WRITE(2,'(3a)')'   "2dcut_pugh.dat" u 1:4 w l lc "',cval3,'" lw 2 title "Negative"'
   ENDIF 
        WRITE(*,"(A,F6.3,A,F6.3,A)")" > Using: go to gnuplot, call 'pugh.gpi' '",pugh_max2/4,"' '",pugh_max2+0.1d0,"' (or other scale)  "
 ENDIF
 !\\\\\\\\\\\\\\\\\\\\\
 if (val=='hmpugh' .OR. val=='hmpug' .or. val=='hmPugh') THEN
    call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='phug_smap.gpi') 
Call copyri()
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')' set output  "phug_smap.png"'
WRITE(2,'(a)')' set multiplot layout 1,2'
WRITE(2,'(a)')' set format z "%11.4e"'
WRITE(2,'(a)')' set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')' set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')' set view map'
IF      (clor_val=='bbry') THEN
 WRITE(2,'(a)')' set palette rgb 7,5,15'
ELSE IF (clor_val=='grv') THEN
 WRITE(2,'(a)')' set palette rgb 3,11,6'
ELSE IF (clor_val=='bbvy') THEN
 WRITE(2,'(a)')' set palette rgb 30,31,32'
ELSE IF (clor_val=='bgyr') THEN
 WRITE(2,'(a)')' set palette rgb 33,13,10'
ELSE IF (clor_val=='bryw') THEN
 WRITE(2,'(a)')' set palette rgb 34,35,36'
ELSE
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')' unset ztics'
WRITE(2,'(a)')' set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')' set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')' set size 0.333 ,1.1'
WRITE(2,'(a)')' set origin 0.15,-0.03'
WRITE(2,'(a)')' set cblabel "({Pugh}_{max}) x 10^{-2}"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($20*100) w  pm3d notitle'
WRITE(2,'(a)')' set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.5,-0.03'
WRITE(2,'(a)')'set cblabel "({Pugh}_{min}) x 10^{-2}"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($21*100) w  pm3d notitle'
 
WRITE(2,'(a)')' unset multiplot'
WRITE(2,'(a)')' unset output'
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' print "> phug_smap.png was generated... "'
WRITE(*,"(A)")" > Using: gnuplot phug_smap.gpi"

ENDIF
  !/////////////////
  if (val=='sound' .OR. val=='sou' .or. val=='Sound') THEN
         WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='sound.gpi'); val0='sou' 	
   Call copyri()
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
        WRITE(*,"(A,F5.2,A,F6.2,A)")" > Using: go to gnuplot, call 'sound.gpi' '",maxEVaTMf/4.0d0,"' '",maxEVaTMf+1d0,"' (or other scale)  "
 ENDIF
 !///////////////////

  if (val=='bulk' .OR. val=='bul' .or. val=='Bulk') THEN
  
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='bulk.gpi'); val0='bul' 	
   Call copyri()
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
   IF (cval1=="n" ) THEN  
    !WRITE(2,'(a)')'pl "2dcut_bulk.dat" u 1:2 w l lc "red" lw 2 ,\'
    WRITE(2,'(a)')'pl   "2dcut_bulk.dat" u 1:3 w l lc "green"lw 2 title "Bulk modulus "'
    ELSE
    ! WRITE(2,'(2a)')'pl "2dcut_bulk.dat" u 1:2 w l lc "',cval1,'" lw 2 ,\'
     WRITE(2,'(3a)')'pl   "2dcut_bulk.dat" u 1:3 w l lc "',cval1,'"lw 2 title "Bulk modulus "'  
    ENDIF
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
   IF (cval1=="n") THEN  
   ! WRITE(2,'(a)')'pl "2dcut_bulk.dat" u 1:2 w l lc "red" lw 2 ,\'
    WRITE(2,'(a)')'pl   "2dcut_bulk.dat" u 1:3 w l lc "green"lw 2 title "Bulk modulus "'
   ELSE
     WRITE(2,'(3a)')'pl   "2dcut_bulk.dat" u 1:3 w l lc "',cval1,'"lw 2 title "Bulk modulus "' 
 
   ENDIF
   WRITE(*,"(A,F7.2,A,F9.2,A)")" > Using: go to gnuplot, call 'bulk.gpi' '",Maxbulk*100d0/4,"' '",(Maxbulk*100d0)+1.d0,"' (or other scale)  "
 ENDIF
!///////////////////

 if (val=='hmbulk' .OR. val=='hmbul' .or. val=='hmBulk') THEN
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='bulk_smap.gpi'); 
Call copyri()
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')' set output  "bulk_smap.png"'
WRITE(2,'(a)')' set multiplot layout 1,1'
WRITE(2,'(a)')' set format z "%11.4e"'
WRITE(2,'(a)')' set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')' set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')' set view map'
IF      (clor_val=='bbry') THEN
 WRITE(2,'(a)')' set palette rgb 7,5,15'
ELSE IF (clor_val=='grv') THEN
 WRITE(2,'(a)')' set palette rgb 3,11,6'
ELSE IF (clor_val=='bbvy') THEN
 WRITE(2,'(a)')' set palette rgb 30,31,32'
ELSE IF (clor_val=='bgyr') THEN
 WRITE(2,'(a)')' set palette rgb 33,13,10'
ELSE IF (clor_val=='bryw') THEN
 WRITE(2,'(a)')' set palette rgb 34,35,36'
ELSE
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')' unset ztics'
WRITE(2,'(a)')' set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')' set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')' set size 0.333 ,1.1'
WRITE(2,'(a)')' set origin 0.3333,-0.03'
WRITE(2,'(a)')' set cblabel "({B}_{Posivive/Negative})x 10^{2} (GPa)"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($16/100) w  pm3d notitle'
WRITE(2,'(a)')' unset multiplot'
WRITE(2,'(a)')' unset output'
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' print "> bulk_smap.png was generated... "'
WRITE(*,"(A)")" > Using: gnuplot bulk_smap.gpi"

ENDIF
!///////////////////
if (val=='hard' .OR. val=='hardness' .or. val=='Hard') THEN
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='hardness.gpi'); val0='hard' 	
Call copyri()
CALL setreset()
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_hardness.dat' u 2 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_hardness.dat" u 3 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0) 
   IF (cval1=="n") THEN  
WRITE(2,'(a)')'pl "2dcut_hardness.dat" u 1:2 w l lc "green" lw 2 title "Hardness"'
!WRITE(2,'(a)')'   "2dcut_hardness.dat" u 1:3 w l lc "blue" lw 2'
ELSE
WRITE(2,'(3a)')'pl "2dcut_hardness.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Hardness"'
!WRITE(2,'(a)')'   "2dcut_hardness.dat" u 1:3 w l lc "',cval1,'" lw 2'
ENDIF
CALL setterm() 
CALL setoutput(val0)
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_hardness.dat' u 2 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_hardness.dat" u 2 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0)  
   IF (cval1=="n") THEN  
WRITE(2,'(a)')'pl "2dcut_hardness.dat" u 1:2 w l lc "green" lw 2 title "Hardness"'
!WRITE(2,'(a)')'   "2dcut_hardness.dat" u 1:3 w l lc "blue" lw 2'
ELSE
WRITE(2,'(3a)')'pl "2dcut_hardness.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Hardness"'
!WRITE(2,'(a)')'   "2dcut_hardness.dat" u 1:3 w l lc "',cval1,'" lw 2'
ENDIF
WRITE(*,"(A,F7.2,A,F8.2,A)")" > Using: go to gnuplot, call 'hardness.gpi' '",Ha_max2/2,"' '",Ha_max2+1d0,"'(or other scale)  "
 
ENDIF
!///////////////////

if (val=='hmhard' .OR. val=='hmhar' .or. val=='Hmhardness') THEN
call threeDdmap()
WRITE(*,'(2a)') val,'was READ well...'
open(2,file='hardness_smap.gpi'); 
Call copyri()
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')' set output  "hardness_smap.png"'
WRITE(2,'(a)')' set multiplot layout 1,1'
WRITE(2,'(a)')' set format z "%11.4e"'
WRITE(2,'(a)')' set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')' set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')' set view map'
IF      (clor_val=='bbry') THEN
WRITE(2,'(a)')' set palette rgb 7,5,15'
ELSE IF (clor_val=='grv') THEN
WRITE(2,'(a)')' set palette rgb 3,11,6'
ELSE IF (clor_val=='bbvy') THEN
WRITE(2,'(a)')' set palette rgb 30,31,32'
ELSE IF (clor_val=='bgyr') THEN
WRITE(2,'(a)')' set palette rgb 33,13,10'
ELSE IF (clor_val=='bryw') THEN
WRITE(2,'(a)')' set palette rgb 34,35,36'
ELSE
WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')' unset ztics'
WRITE(2,'(a)')' set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')' set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')' set size 0.333 ,1.1'
WRITE(2,'(a)')' set origin 0.3333,-0.03'
WRITE(2,'(a)')' set cblabel "Hardness"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($24) w  pm3d notitle'
WRITE(2,'(a)')' unset multiplot'
WRITE(2,'(a)')' unset output'
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' print "> hardness_smap.png was generated... "'
WRITE(*,"(A)")" > Using: gnuplot hardness_smap.gpi"

ENDIF
  !///////////////////
  if (val=='young' .OR. val=='you' .or. val=='Young') THEN
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='young.gpi'); val0='you' 	
   Call copyri()
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
   IF (cval1=="n" ) THEN   
    WRITE(2,'(a)')'pl "2dcut_young.dat" u 1:2 w l lc "green" lw 2 title "Young modulus"'
   ! WRITE(2,'(a)')'   "2dcut_young.dat" u 1:3 w l lc "blue" lw 2'
   ELSE
    WRITE(2,'(3a)')'pl "2dcut_young.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Young modulus"'
   ! WRITE(2,'(2a)')'   "2dcut_young.dat" u 1:3 w l lc "',cval1,'" lw 2'
   ENDIF
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
   IF (cval1=="n" ) THEN   
    WRITE(2,'(a)')'pl "2dcut_young.dat" u 1:2 w l lc "green" lw 2 title "Young modulus"'
    !WRITE(2,'(a)')'   "2dcut_young.dat" u 1:3 w l lc "blue" lw 2'
   ELSE
    WRITE(2,'(3a)')'pl "2dcut_young.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Young modulus"'
   ! WRITE(2,'(2a)')'   "2dcut_young.dat" u 1:3 w l lc "',cval1,'" lw 2'
   ENDIF
   WRITE(*,"(A,F7.2,A,F8.2,A)")" > Using: go to gnuplot, call 'young.gpi' '",Maxyoung/2,"' '",Maxyoung+1d0,"'(or other scale)  "
   !WRITE(*,*)Maxyoung
 ENDIF
!///////////////////

 if (val=='hmyoung' .OR. val=='hmyou' .or. val=='hmYoung') THEN
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='young_smap.gpi'); 
Call copyri()
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')' set output  "young_smap.png"'
WRITE(2,'(a)')' set multiplot layout 1,1'
WRITE(2,'(a)')' set format z "%11.4e"'
WRITE(2,'(a)')' set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')' set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')' set view map'
IF      (clor_val=='bbry') THEN
 WRITE(2,'(a)')' set palette rgb 7,5,15'
ELSE IF (clor_val=='grv') THEN
 WRITE(2,'(a)')' set palette rgb 3,11,6'
ELSE IF (clor_val=='bbvy') THEN
 WRITE(2,'(a)')' set palette rgb 30,31,32'
ELSE IF (clor_val=='bgyr') THEN
 WRITE(2,'(a)')' set palette rgb 33,13,10'
ELSE IF (clor_val=='bryw') THEN
 WRITE(2,'(a)')' set palette rgb 34,35,36'
ELSE
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')' unset ztics'
WRITE(2,'(a)')' set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')' set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')' set size 0.333 ,1.1'
WRITE(2,'(a)')' set origin 0.3333,-0.03'
WRITE(2,'(a)')' set cblabel "({E})x 10^{2} (GPa)"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($7/100) w  pm3d notitle'
WRITE(2,'(a)')' unset multiplot'
WRITE(2,'(a)')' unset output'
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' print "> young_smap.png was generated... "'
WRITE(*,"(A)")" > Using: gnuplot young_smap.gpi"

ENDIF
!///////////////
if (val=='hmphaseall'.or. val=='hmpall' .or. val=='hmpa') THEN
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
  open(2,file='phase_smap.gpi'); 
  Call copyri()
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')'set output  "phase_smap.png"'
WRITE(2,'(a)')'set multiplot layout 1,3'
WRITE(2,'(a)')'set format z "%11.4e"'
WRITE(2,'(a)')'set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')'set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')'set view map'
IF      (clor_val=='bbry') THEN
 WRITE(2,'(a)')' set palette rgb 7,5,15'
ELSE IF (clor_val=='grv') THEN
 WRITE(2,'(a)')' set palette rgb 3,11,6'
ELSE IF (clor_val=='bbvy') THEN
 WRITE(2,'(a)')' set palette rgb 30,31,32'
ELSE IF (clor_val=='bgyr') THEN
 WRITE(2,'(a)')' set palette rgb 33,13,10'
ELSE IF (clor_val=='bryw') THEN
 WRITE(2,'(a)')' set palette rgb 34,35,36'
ELSE
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')'unset ztics'
WRITE(2,'(a)')'set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')'set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')'set size 0.333 ,1.1'
WRITE(2,'(a)')'set origin 0.003,-0.03'
WRITE(2,'(a)')'set cblabel "V_{p}-P Mode (m/s)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($25) w  pm3d notitle'
WRITE(2,'(a)')'set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.3333,-0.03'
WRITE(2,'(a)')'set cblabel "V_{p}-Fast Mode (m/s)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($27) w  pm3d notitle'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.66,-0.03'
WRITE(2,'(a)')'set cblabel "V_{p}-Slow Mode (m/s)"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($29) w  pm3d notitle'
WRITE(2,'(a)')'unset multiplot'
WRITE(2,'(a)')'unset output'
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'print "> phase_smap.png was generated... "'
WRITE(*,"(A)")" > Using: gnuplot phase_smap.gpi"
ENDIF
!=================
if (val=='hmgroupall'.or. val=='hmgall' .or. val=='hmga') THEN
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
  open(2,file='group_smap.gpi');
  Call copyri()
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')'set output  "group_smap.png"'
WRITE(2,'(a)')'set multiplot layout 1,3'
WRITE(2,'(a)')'set format z "%11.4e"'
WRITE(2,'(a)')'set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')'set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')'set view map'

IF      (clor_val=='bbry') THEN
 WRITE(2,'(a)')' set palette rgb 7,5,15'
ELSE IF (clor_val=='grv') THEN
 WRITE(2,'(a)')' set palette rgb 3,11,6'
ELSE IF (clor_val=='bbvy') THEN
 WRITE(2,'(a)')' set palette rgb 30,31,32'
ELSE IF (clor_val=='bgyr') THEN
 WRITE(2,'(a)')' set palette rgb 33,13,10'
ELSE IF (clor_val=='bryw') THEN
 WRITE(2,'(a)')' set palette rgb 34,35,36'
ELSE
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')'unset ztics'
WRITE(2,'(a)')'set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')'set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')'set size 0.333 ,1.1'
WRITE(2,'(a)')'set origin 0.003,-0.03'
WRITE(2,'(a)')'set cblabel "V_{g}-P Mode (m/s)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($26) w  pm3d notitle'
WRITE(2,'(a)')'set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.3333,-0.03'
WRITE(2,'(a)')'set cblabel "V_{g}-Fast Mode (m/s)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($28) w  pm3d notitle'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.66,-0.03'
WRITE(2,'(a)')'set cblabel "V_{g}-Slow Mode (m/s)"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($30) w  pm3d notitle'
WRITE(2,'(a)')'unset multiplot'
WRITE(2,'(a)')'unset output'
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'print "> group_smap.png was generated... "'
WRITE(*,"(A)")" > Using: gnuplot group_smap.gpi"
ENDIF
!/////////////
  if (val=='PhaseP'.or. val=='phasep' .or. val=='pp') THEN
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='phase-p.gpi'); val0='pp'   
   Call copyri()
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
   IF (cval1=="n") THEN    
   ! WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:2 w l lc "blue" lw 2 ,\'
    WRITE(2,'(a)')'pl     "2dcut_pveloc.dat" u 1:2 w l lc "blue" lw 2 title "Phase velocity: P-mode"'
   ELSE
   ! WRITE(2,'(2a)')'pl "2dcut_pveloc.dat" u 1:2 w l lc "',cval1,'" lw 2 ,\'
    WRITE(2,'(3a)')'pl     "2dcut_pveloc.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Phase velocity: P-mode"'
   ENDIF
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
   IF (cval1=="n" ) THEN    
   ! WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:2 w l lc "blue" lw 2 ,\'
    WRITE(2,'(a)')'pl     "2dcut_pveloc.dat" u 1:2 w l lc "blue" lw 2 title "Phase velocity: P-mode"'
   ELSE
   ! WRITE(2,'(2a)')'pl "2dcut_pveloc.dat" u 1:2 w l lc "',cval1,'" lw 2 ,\'
    WRITE(2,'(2a)')'pl     "2dcut_pveloc.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Phase velocity: P-mode"'
   ENDIF
   WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'phase-p.gpi' '",(VVP_P_max/4d0),"' '",VVP_P_max+1d0,"' (or other scale)  "
 ENDIF 
!///////////////////

  if (val=='PhaseF'.or. val=='phasef' .or. val=='pf') THEN
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='phase-fast.gpi'); val0='pf' 
   Call copyri()  
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
   IF (cval1=="n") THEN   
   ! WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:3 w l lc "green" lw 2 ,\'
    WRITE(2,'(a)')'pl     "2dcut_pveloc.dat" u 1:3 w l lc "green" lw 2 title "Phase velocity: Fast-mode"'
   ELSE
   ! WRITE(2,'(2a)')'pl "2dcut_pveloc.dat" u 1:3 w l lc "',cval1,'" lw 2 ,\'
    WRITE(2,'(3a)')'pl     "2dcut_pveloc.dat" u 1:3 w l lc "',cval1,'" lw 2 title "Phase velocity: Fast-mode"'
   ENDIF
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
   IF (cval1=="n" ) THEN   
   ! WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:3 w l lc "green" lw 2 ,\'
    WRITE(2,'(a)')'pl     "2dcut_pveloc.dat" u 1:3 w l lc "green" lw 2 title "Phase velocity: Fast-mode"'
   ELSE
   ! WRITE(2,'(2a)')'pl "2dcut_pveloc.dat" u 1:3 w l lc "',cval1,'" lw 2 ,\'
    WRITE(2,'(2a)')'pl     "2dcut_pveloc.dat" u 1:3 w l lc "',cval1,'" lw 2 title "Phase velocity: Fast-mode"'
   ENDIF
   WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'phase-fast.gpi' '",(VVP_Sf_max/4d0),"' '",VVP_Sf_max+1.d0,"' (or other scale)  "
 ENDIF 
!///////////////////

  if (val=='PhaseS'.or. val=='phases' .or. val=='ps') THEN
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='phase-slow.gpi'); val0='ps'   
   Call copyri()
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
   IF (cval1=="n") THEN 
   ! WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:4 w l lc "red" lw 2 ,\'
    WRITE(2,'(a)')'pl     "2dcut_pveloc.dat" u 1:4 w l lc "red" lw 2 title "Phase velocity: Slow-mode"'
   ELSE
  !  WRITE(2,'(2a)')'pl "2dcut_pveloc.dat" u 1:4 w l lc "',cval1,'" lw 2 ,\'
    WRITE(2,'(3a)')'pl     "2dcut_pveloc.dat" u 1:4 w l lc "',cval2,'" lw 2 title "Phase velocity: Slow-mode"'    
   ENDIF
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
   IF (cval1=="n") THEN 
   ! WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:4 w l lc "red" lw 2 ,\'
    WRITE(2,'(a)')'pl     "2dcut_pveloc.dat" u 1:4 w l lc "red" lw 2 title "Phase velocity: Slow-mode"'
   ELSE
   ! WRITE(2,'(2a)')'pl "2dcut_pveloc.dat" u 1:4 w l lc "',cval1,'" lw 2 ,\'
    WRITE(2,'(3a)')'pl     "2dcut_pveloc.dat" u 1:4 w l lc "',cval1,'" lw 2 title "Phase velocity: Slow-mode"'    
   ENDIF
   WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'phase-slow.gpi' '",VVP_Ss_max/4.0d0,"' '",VVP_Ss_max+1d0,"' (or other scale)  "
 ENDIF 
 !///////////////////

  if (val=='GroupP'.or. val=='groupp' .or. val=='gp') THEN
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='group-p.gpi'); val0='ps'   
   Call copyri()
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
   IF (cval1=="n") THEN  
   ! WRITE(2,'(a)')'pl "2dcut_gveloc.dat"  u 1:2 w l lc "blue" lw 2 ,\'
    WRITE(2,'(a)')'pl     "2dcut_gveloc.dat"  u 1:2 w l lc "blue" lw 2 title "Group velocity: P-mode"'
   ELSE
   ! (2,'(a)')'pl "2dcut_gveloc.dat"  u 1:2 w l lc "cval1" lw 2 ,\'
    WRITE(2,'(3a)')'pl     "2dcut_gveloc.dat"  u 1:2 w l lc "',cval1,'" lw 2 title "Group velocity: P-mode"'    
   ENDIF
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
   IF (cval1=="n") THEN  
   ! WRITE(2,'(a)')'pl "2dcut_gveloc.dat"  u 1:2 w l lc "blue" lw 2 ,\'
    WRITE(2,'(a)')'pl     "2dcut_gveloc.dat"  u 1:2 w l lc "blue" lw 2 title "Group velocity: P-mode"'
   ELSE
   ! WRITE(2,'(a)')'pl "2dcut_gveloc.dat"  u 1:2 w l lc "cval1" lw 2 ,\'
    WRITE(2,'(3a)')'pl     "2dcut_gveloc.dat"  u 1:2 w l lc "',cval1,'" lw 2 title "Group velocity: P-mode"'    
   ENDIF
   WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'group-p.gpi' '",VVG_P_max/4d0,"' '",VVG_P_max+1d0,"' (or other scale)  "
 ENDIF 
 !///////////////////
  if (val=='GroupF'.or. val=='groupf' .or. val=='gf') THEN
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='group-fast.gpi'); val0='pf'   
   Call copyri()
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
   IF (cval1=="n") THEN  
   ! WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2 ,\'
    WRITE(2,'(a)')'pl     "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2 title "Group velocity: Fast-mode"'
    ELSE
   ! WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2 ,\'
     WRITE(2,'(3a)')'pl     "2dcut_gveloc.dat" u 1:3 w l lc "',cval1,'" lw 2 title "Group velocity: Fast-mode"'    
    ENDIF
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
   IF (cval1=="n") THEN  
   ! WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2 ,\'
    WRITE(2,'(a)')'pl   "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2 title "Group velocity: Fast-mode"'
    ELSE
   ! WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2 ,\'
     WRITE(2,'(3a)')'pl   "2dcut_gveloc.dat" u 1:3 w l lc "',cval1,'" lw 2 title "Group velocity: Fast-mode"'    
    ENDIF
   WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'group-fast.gpi' '",VVG_Sf_max/4d0,"' '",VVG_Sf_max+1d0,"' (or other scale)  "
 ENDIF 
!///////////////////

  if (val=='GroupS'.or. val=='groups' .or. val=='gs') THEN
       WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='group-slow.gpi'); val0='ps'   
   Call copyri()
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
   IF (cval1=="n") THEN  
    WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:4 w l lc "red" lw 2 title "Group velocity: Slow-mode"'
    !WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:4 w l lc "red" lw 2'
   ELSE
    WRITE(2,'(3a)')'pl "2dcut_gveloc.dat" u 1:4 w l lc "',cval1,'" lw 2 title "Group velocity: Slow-mode"'
   ENDIF
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_gveloc.dat' u 4 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_gveloc.dat" u 4 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
  CALL settit(val0)  
   IF (cval1=="n") THEN  
    WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:4 w l lc "red" lw 2 title "Group velocity: Slow-mode"'
    !WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:4 w l lc "red" lw 2'
   ELSE
    WRITE(2,'(3a)')'pl "2dcut_gveloc.dat" u 1:4 w l lc "',cval1,'" lw 2 title "Group velocity: Slow-mode"'
   ENDIF
   WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'group-slow.gpi' '",VVG_Ss_max/4d0,"' '",VVG_Ss_max+1d0,"' (or other scale)  "
 ENDIF 
 !///////////////////
 if (val=='PFactP'.or. val=='pfoupp' .or. val=='pfp') THEN
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='power-flow-p.gpi'); val0='pfp'   
Call copyri()
CALL setreset()
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_pfaveloc.dat' u 2 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_pfaveloc.dat" u 2 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0)
IF (cval1=="n") THEN   
 WRITE(2,'(a)')'pl "2dcut_pfaveloc.dat" u 1:2 w l lc "blue" lw 2 title "Power flow angle: P-mode"'
 !WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:2 w l lc "blue" lw 2'
ELSE
 WRITE(2,'(3a)')'pl "2dcut_pfaveloc.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Power flow angle: P-mode"'
ENDIF
CALL setterm() 
CALL setoutput(val0)
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_pfaveloc.dat' u 2 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_pfaveloc.dat" u 2 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0)  
IF (cval1=="n") THEN   
 WRITE(2,'(a)')'pl "2dcut_pfaveloc.dat" u 1:2 w l lc "blue" lw 2 title "Power flow angle: P-mode"'
 !WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:2 w l lc "blue" lw 2'
ELSE
 WRITE(2,'(3a)')'pl "2dcut_pfaveloc.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Power flow angle: P-mode"'
ENDIF
WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'power-flow-p.gpi' '",1.0+(VV_P_PF_max/4d0),"' '",VV_P_PF_max+1d0,"' (or other scale)  "
ENDIF 
!///////////////////
if (val=='PFactF'.or. val=='pfoupf' .or. val=='pff') THEN
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='power-flow-fast.gpi'); val0='pff'   
Call copyri()
CALL setreset()
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_pfaveloc.dat' u 3 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_pfaveloc.dat" u 3 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0) 
IF (cval1=="n") THEN   
WRITE(2,'(a)')'pl "2dcut_pfaveloc.dat" u 1:3 w l lc "green" lw 2 title "Power flow angle: Fast-mode"'
!WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:3 w l lc "green" lw 2'
ELSE
WRITE(2,'(3a)')'pl "2dcut_pfaveloc.dat" u 1:3 w l lc "',cval1,'" lw 2 title "Power flow angle: Fast-mode"'
ENDIF
CALL setterm() 
CALL setoutput(val0)
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_pfaveloc.dat' u 3 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_pfaveloc.dat" u 3 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0)  
IF (cval1=="n") THEN   
WRITE(2,'(a)')'pl "2dcut_pfaveloc.dat" u 1:3 w l lc "green" lw 2 title "Power flow angle: Fast-mode"'
!WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:3 w l lc "green" lw 2'
ELSE
WRITE(2,'(3a)')'pl "2dcut_pfaveloc.dat" u 1:3 w l lc "',cval1,'" lw 2 title "Power flow angle: Fast-mode"'
ENDIF
WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'power-flow-fast.gpi' '",0.5+(VV_Sf_PF_max/4d0),"' '",VV_Sf_PF_max+1d0,"' (or other scale)  "
ENDIF 
!///////////////////
if (val=='PFactS'.or. val=='pfoups' .or. val=='pfs') THEN
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='power-flow-slow.gpi'); val0='pfs'   
Call copyri()
CALL setreset()
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_pfaveloc.dat' u 4 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_pfaveloc.dat" u 4 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0) 
IF (cval1=="n") THEN   
 WRITE(2,'(a)')'pl "2dcut_pfaveloc.dat" u 1:4 w l lc "red" lw 2 title "Power flow angle: Slow-mode"'
!WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:4 w l lc "red" lw 2'
ELSE
 WRITE(2,'(3a)')'pl "2dcut_pfaveloc.dat" u 1:4 w l lc "',cval1,'" lw 2 title "Power flow angle: Slow-mode"'
ENDIF
CALL setterm() 
CALL setoutput(val0)
CALL unset1() 
WRITE(2,'(a)')"stats '2dcut_pfaveloc.dat' u 4 nooutput"
WRITE(2,'(a)')'maxig=ceil(STATS_max)'
WRITE(2,'(a)')'maxi=maxig'
WRITE(2,'(a)')'stats "2dcut_pfaveloc.dat" u 4 nooutput'
WRITE(2,'(a)')'maxif=ceil(STATS_max)'
WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
CALL set1()
CALL settit(val0)  
IF (cval1=="n") THEN   
 WRITE(2,'(a)')'pl "2dcut_pfaveloc.dat" u 1:4 w l lc "red" lw 2 title "Power flow angle: Slow-mode"'
!WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:4 w l lc "red" lw 2'
ELSE
 WRITE(2,'(3a)')'pl "2dcut_pfaveloc.dat" u 1:4 w l lc "',cval1,'" lw 2 title "Power flow angle: Slow-mode"'
ENDIF
WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'power-flow-slow.gpi' '",0.5+(VV_Sf_PF_max/4d0),"' '",VV_Sf_PF_max+1d0,"' (or other scale)  "
ENDIF 
!///////////////////
 if (val=='phaseall'.or. val=='pall' .or. val=='pa') THEN
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='phase.gpi'); val0='pall'   
Call copyri()
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
WRITE(2,'(a)')'set key'
IF (cval1=="n" .or. cval3=="n" .or. cval3=="n") THEN   
WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:2 w l lc "red"   lw 2 title "P-mode",\'
WRITE(2,'(a)')'   "2dcut_pveloc.dat" u 1:3 w l lc "green" lw 2 title "Fast-mode",\'
WRITE(2,'(a)')'   "2dcut_pveloc.dat" u 1:4 w l lc "blue"  lw 2 title "Slow-mode"'
ELSE
WRITE(2,'(3a)')'pl "2dcut_pveloc.dat" u 1:2 w l lc "',cval1,'"   lw 2 title "P-mode",\'
WRITE(2,'(3a)')'   "2dcut_pveloc.dat" u 1:3 w l lc "',cval2,'" lw 2 title "Fast-mode",\'
WRITE(2,'(3a)')'   "2dcut_pveloc.dat" u 1:4 w l lc "',cval3,'"  lw 2 title "Slow-mode"'
ENDIF
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
WRITE(2,'(a)')'set key'  
IF (cval1=="n" .or. cval3=="n" .or. cval3=="n") THEN   
WRITE(2,'(a)')'pl "2dcut_pveloc.dat" u 1:2 w l lc "red"   lw 2 title "P-mode",\'
WRITE(2,'(a)')'   "2dcut_pveloc.dat" u 1:3 w l lc "green" lw 2 title "Fast-mode",\'
WRITE(2,'(a)')'   "2dcut_pveloc.dat" u 1:4 w l lc "blue"  lw 2 title "Slow-mode"'
ELSE
WRITE(2,'(3a)')'pl "2dcut_pveloc.dat" u 1:2 w l lc "',cval1,'"   lw 2 title "P-mode",\'
WRITE(2,'(3a)')'   "2dcut_pveloc.dat" u 1:3 w l lc "',cval2,'" lw 2 title "Fast-mode",\'
WRITE(2,'(3a)')'   "2dcut_pveloc.dat" u 1:4 w l lc "',cval3,'"  lw 2 title "Slow-mode"'
ENDIF
WRITE(*,*)" > Using: go to gnuplot, call 'phase.gpi' '10' '50'(or other scale)  "
ENDIF 
 !///////////////////

if (val=='groupall'.or. val=='gall' .or. val=='ga') THEN
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='group.gpi'); val0='gall'   
Call copyri()
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
IF (cval1=="n" .or. cval3=="n" .or. cval3=="n") THEN   
WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:2 w l lc "red"   lw 2   title "P-mode",\'
WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2   title "Fast-mode",\'
WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:4 w l lc "blue"  lw 2   title "Slow-mode"'
ELSE
WRITE(2,'(3a)')'pl "2dcut_gveloc.dat" u 1:2 w l lc "',cval1,'"   lw 2   title "P-mode",\'
WRITE(2,'(3a)')'   "2dcut_gveloc.dat" u 1:3 w l lc "',cval2,'" lw 2   title "Fast-mode",\'
WRITE(2,'(3a)')'   "2dcut_gveloc.dat" u 1:4 w l lc "',cval3,'"  lw 2   title "Slow-mode"'
ENDIF
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
IF (cval1=="n" .or. cval3=="n" .or. cval3=="n") THEN   
WRITE(2,'(a)')'pl "2dcut_gveloc.dat" u 1:2 w l lc "red"   lw 2   title "P-mode",\'
WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:3 w l lc "green" lw 2   title "Fast-mode",\'
WRITE(2,'(a)')'   "2dcut_gveloc.dat" u 1:4 w l lc "blue"  lw 2   title "Slow-mode"'
ELSE
WRITE(2,'(3a)')'pl "2dcut_gveloc.dat" u 1:2 w l lc "',cval1,'"   lw 2   title "P-mode",\'
WRITE(2,'(3a)')'   "2dcut_gveloc.dat" u 1:3 w l lc "',cval2,'" lw 2   title "Fast-mode",\'
WRITE(2,'(3a)')'   "2dcut_gveloc.dat" u 1:4 w l lc "',cval3,'"  lw 2   title "Slow-mode"'
ENDIF
WRITE(*,*)" > Using: go to gnuplot, call 'group.gpi' '5' '20' (or other scale)  "
ENDIF
!\\\\\\\\\\\
if (val=='hmpowerfall'.or. val=='hmpfall' .or. val=='hmpfa') THEN
    call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
  open(2,file='powerfolw_smap.gpi');
  Call copyri()
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')'set output  "powerfolw_smap.png"'
WRITE(2,'(a)')'set multiplot layout 1,3'
WRITE(2,'(a)')'set format z "%11.4e"'
WRITE(2,'(a)')'set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')'set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')'set view map'
IF      (clor_val=='bbry') THEN
 WRITE(2,'(a)')' set palette rgb 7,5,15'
ELSE IF (clor_val=='grv') THEN
 WRITE(2,'(a)')' set palette rgb 3,11,6'
ELSE IF (clor_val=='bbvy') THEN
 WRITE(2,'(a)')' set palette rgb 30,31,32'
ELSE IF (clor_val=='bgyr') THEN
 WRITE(2,'(a)')' set palette rgb 33,13,10'
ELSE IF (clor_val=='bryw') THEN
 WRITE(2,'(a)')' set palette rgb 34,35,36'
ELSE
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')'unset ztics'
WRITE(2,'(a)')'set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')'set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')'set size 0.333 ,1.1'
WRITE(2,'(a)')'set origin 0.003,-0.03'
WRITE(2,'(a)')'set cblabel "PFA-P Mode (Degree)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($31) w  pm3d notitle'
WRITE(2,'(a)')'set format y  "";unset ylabel'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.3333,-0.03'
WRITE(2,'(a)')'set cblabel "PFA-Fast Mode (Degree)"'
WRITE(2,'(a)')'sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($32) w  pm3d notitle'
WRITE(2,'(a)')'set size 0.333,1.1'
WRITE(2,'(a)')'set origin 0.66,-0.03'
WRITE(2,'(a)')'set cblabel "PFA-Slow Mode (Degree)"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($33) w  pm3d notitle'
WRITE(2,'(a)')'unset multiplot'
WRITE(2,'(a)')'unset output'
WRITE(2,'(a)')'reset'
WRITE(2,'(a)')'print "> powerfolw_smap.png was generated... "'
WRITE(*,"(A)")" > Using: gnuplot powerfolw_smap.gpi"
ENDIF
!/////////////
 !///////////////////

if (val=='powerfall'.or. val=='pfall' .or. val=='pfa') THEN
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='powerfolw.gpi'); val0='gall'   
Call copyri()
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
WRITE(2,'(a)')'set key' 
IF (cval1=="n" .or. cval3=="n" .or. cval3=="n") THEN   
WRITE(2,'(a)')'pl "2dcut_pfaveloc.dat" u 1:($2) w l lc "red"   lw 2  title "P-mode",\'
WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:($3) w l lc "green" lw 2  title "Fast-mode",\'
WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:($4) w l lc "blue"  lw 2  title "Slow-mode"'
ELSE
WRITE(2,'(3a)')'pl "2dcut_pfaveloc.dat" u 1:($2) w l lc "',cval1,'"   lw 2  title "P-mode",\'
WRITE(2,'(3a)')'   "2dcut_pfaveloc.dat" u 1:($3) w l lc "',cval2,'" lw 2  title "Fast-mode",\'
WRITE(2,'(3a)')'   "2dcut_pfaveloc.dat" u 1:($4) w l lc "',cval3,'"  lw 2  title "Slow-mode"' 
ENDIF  
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
WRITE(2,'(a)')'set key' 
IF (cval1=="n" .or. cval3=="n" .or. cval3=="n") THEN   
WRITE(2,'(a)')'pl "2dcut_pfaveloc.dat" u 1:($2) w l lc "red"   lw 2  title "P-mode",\'
WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:($3) w l lc "green" lw 2  title "Fast-mode",\'
WRITE(2,'(a)')'   "2dcut_pfaveloc.dat" u 1:($4) w l lc "blue"  lw 2  title "Slow-mode"'
ELSE
WRITE(2,'(3a)')'pl "2dcut_pfaveloc.dat" u 1:($2) w l lc "',cval1,'"   lw 2  title "P-mode",\'
WRITE(2,'(3a)')'   "2dcut_pfaveloc.dat" u 1:($3) w l lc "',cval2,'" lw 2  title "Fast-mode",\'
WRITE(2,'(3a)')'   "2dcut_pfaveloc.dat" u 1:($4) w l lc "',cval3,'"  lw 2  title "Slow-mode"' 
ENDIF 
WRITE(*,*)" > Using: go to gnuplot, call 'powerfolw.gpi' '1' '5'(or other scale)  "
ENDIF
 !///////////////////

if (val=='km'.or. val=='kM' .or. val=='KM') THEN
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='mthconductivity.gpi'); val0='km'   
   Call copyri()
   CALL setreset()
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_km.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_km.dat" u 2 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
   CALL settit(val0)
   IF (cval1=="n") THEN 
    WRITE(2,'(a)')'pl "2dcut_km.dat" u 1:2 w l lc "violet" lw 2 title "Min. thermal conductivity"'
   ELSE
     WRITE(2,'(3a)')'pl "2dcut_km.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Min. thermal conductivity"'
   ENDIF
 
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   WRITE(2,'(a)')"stats '2dcut_km.dat' u 2 nooutput"
   WRITE(2,'(a)')'maxig=ceil(STATS_max)'
   WRITE(2,'(a)')'maxi=maxig'
   WRITE(2,'(a)')'stats "2dcut_km.dat" u 2 nooutput'
   WRITE(2,'(a)')'maxif=ceil(STATS_max)'
   WRITE(2,'(a)')'if (maxif > maxig) {maxi=maxif} '
   CALL set1()
  CALL settit(val0)  
   IF (cval1=="n") THEN 
    WRITE(2,'(a)')'pl "2dcut_km.dat" u 1:2 w l lc "violet" lw 2 title "Min. thermal conductivity"'
   ELSE
     WRITE(2,'(3a)')'pl "2dcut_km.dat" u 1:2 w l lc "',cval1,'" lw 2 title "Min. thermal conductivity"'
   ENDIF
   WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call 'mthconductivity.gpi' '",0.5+(km_max/4d0),"' '",km_max+1.5d0,"' (or other scale)  "
ENDIF
!\\\\\\\\\\\\\\\
 if (val=='hmkm' .OR. val=='Hmkm' .or. val=='hmKM') THEN
  call threeDdmap()
  WRITE(*,'(2a)') val,'was READ well...'
open(2,file='mthconductivity_smap.gpi'); 
Call copyri()
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' set term pngcairo enhanced dashed font "Arial, 19" size 2300,700 nocrop lw 2'
WRITE(2,'(a)')' set output  "mthconductivity_smap.png"'
WRITE(2,'(a)')' set multiplot layout 1,1'
WRITE(2,'(a)')' set format z "%11.4e"'
WRITE(2,'(a)')' set xlabel "{/Symbol q} (Degree)"  '
WRITE(2,'(a)')' set ylabel "{/Symbol f} (Degree)"  '
WRITE(2,'(a)')' set view map'
IF      (clor_val=='bbry') THEN
 WRITE(2,'(a)')' set palette rgb 7,5,15'
ELSE IF (clor_val=='grv') THEN
 WRITE(2,'(a)')' set palette rgb 3,11,6'
ELSE IF (clor_val=='bbvy') THEN
 WRITE(2,'(a)')' set palette rgb 30,31,32'
ELSE IF (clor_val=='bgyr') THEN
 WRITE(2,'(a)')' set palette rgb 33,13,10'
ELSE IF (clor_val=='bryw') THEN
 WRITE(2,'(a)')' set palette rgb 34,35,36'
ELSE
  WRITE(2,'(a)')' set palette rgb 30,13,10'
END IF
WRITE(2,'(a)')' unset ztics'
WRITE(2,'(a)')' set xtics format "%3.0f"; set xtics 45  in offset 0, 0.3; set mxtics 5;'
WRITE(2,'(a)')' set ytics format  "%3.0f" 45.0   in offset 0.7,0; set mytics 5;'
WRITE(2,'(a)')' set size 0.333 ,1.1'
WRITE(2,'(a)')' set origin 0.3333,-0.03'
WRITE(2,'(a)')' set cblabel "k_{m} (W/K.m)"'
WRITE(2,'(a)')' sp [0:180][0:180][] ".SDdat" u (($1*180/3.141592654)):($2*180/3.141592654):($33) w  pm3d notitle'
WRITE(2,'(a)')' unset multiplot'
WRITE(2,'(a)')' unset output'
WRITE(2,'(a)')' reset'
WRITE(2,'(a)')' print "> mthconductivity_smap.png was generated... "'
WRITE(*,"(A)")" > Using: gnuplot mthconductivity_smap.gpi"

ENDIF 

!100 WRITE(*,*)" > NOT FOUNDE MAX1 FILE"; STOP
!101 WRITE(*,*)" > NOT FOUNDE MAX2 FILE";STOP
 end SUBROUTINE  

 !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$4
  !2D2D2D2D2D2D2D2D2D2D2D2D2D2D2D2
 SUBROUTINE twoDplot(val,MAXpo, MAXyo, MAXsh ,cval1,cval2,cval3)
     ChARACTER(len=10) :: val ,val0,cval1,cval2,cval3
     real(8)           :: MAXpo, MAXyo, MAXsh 

 if (val=='2dpoi' ) THEN
       ! WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='2Dpoissons.gpi'); val0='2poi' 	
   Call copyri()
   CALL setreset()
   CALL unset1() 
   CALL set1_2d()
   CALL settit(val0)
   IF (cval1=="n" .or. cval2=="n") THEN 
    WRITE(2,'(a)')'pl "poisson_2d_sys.dat" u 1:4 w l lc "red"   lw 2 title "Negative",\'
    WRITE(2,'(a)')'   "poisson_2d_sys.dat" u 1:3 w l lc "green"  lw 2 title "Positive",\'
    WRITE(2,'(a)')'   #"poisson_2d_sys.dat" u 1:2 w l lc "blue" lw 2'
   ELSE
    WRITE(2,'(3a)')'pl "poisson_2d_sys.dat" u 1:4 w l lc "',cval1,'"   lw 2 title "Negative",\'
    WRITE(2,'(3a)')'   "poisson_2d_sys.dat" u 1:3 w l lc "',cval2,'"  lw 2 title "Positive",\'
    WRITE(2,'(3a)')'   #"poisson_2d_sys.dat" u 1:2 w l lc "',cval2,'" lw 2'   
   ENDIF
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   CALL set1_2d()
   CALL settit(val0)  
   IF (cval1=="n" .or. cval2=="n") THEN 
    WRITE(2,'(a)')'pl "poisson_2d_sys.dat" u 1:4 w l lc "red"   lw 2 title "Negative",\'
    WRITE(2,'(a)')'   "poisson_2d_sys.dat" u 1:3 w l lc "green"  lw 2 title "Positive",\'
    WRITE(2,'(a)')'   #"poisson_2d_sys.dat" u 1:2 w l lc "blue" lw 2'
   ELSE
    WRITE(2,'(3a)')'pl "poisson_2d_sys.dat" u 1:4 w l lc "',cval1,'"   lw 2 ,\'
    WRITE(2,'(3a)')'   "poisson_2d_sys.dat" u 1:3 w l lc "',cval2,'"  lw 2 title "Negative",\'
    WRITE(2,'(3a)')'   #"poisson_2d_sys.dat" u 1:2 w l lc "',cval2,'" lw 2 title "Positive"'   
   ENDIF
        WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: go to gnuplot, call '2Dpoissons.gpi' '",MAXpo/4d0,"' '",MAXpo+0.1d0,"' (or other scale)  "
 ENDIF
 
  if (val=='2dyoung') THEN
      ! WRITE(*,'(2a)') val,'was READ well...'
   open(2,file='2Dyoung.gpi'); val0='2you' 	
   Call copyri()
   CALL setreset()
   CALL unset1() 
   CALL set1_2d()
   CALL settit(val0)
  IF (cval1=="n" ) THEN
   WRITE(2,'(a)')'pl "young_2d_sys.dat" w l lc "blue" lw 2 title "Young Modulus"'
  ELSE
      WRITE(2,'(3a)')'pl "young_2d_sys.dat" w l lc "',cval1,'" lw 2 title "Young Modulus"'
  ENDIF
   CALL setterm() 
   CALL setoutput(val0)
   CALL unset1() 
   CALL set1_2d()
   CALL settit(val0)  
  IF (cval1=="n" ) THEN
   WRITE(2,'(a)')'pl "young_2d_sys.dat" w l lc "blue" lw 2 title "Young Modulus"'
  ELSE
      WRITE(2,'(3a)')'pl "young_2d_sys.dat" w l lc "',cval1,'" lw 2 title "Young Modulus"'
  ENDIF
   WRITE(*,"(A,F6.2,A,F6.2,A)")" > Using: go to gnuplot, call '2Dyoung.gpi' '",MAXyo/4d0,"' '",MAXyo+2d0,"' (or other scale)  "
 ENDIF
   if (val=='2dshear') THEN
      ! WRITE(*,'(2a)') val,'was READ well...'
      open(2,file='2Dshear.gpi'); val0='2she'  
      Call copyri()
      CALL setreset()
      CALL unset1() 
      CALL set1_2d()
      CALL settit(val0)
  IF (cval1=="n" ) THEN       
      WRITE(2,'(a)')'pl "shear_2d_sys.dat" w l lc "blue" lw 2 title "Shear Modulus"'   
   ELSE
      WRITE(2,'(3a)')'pl "shear_2d_sys.dat" w l lc "',cval1,'" lw 2 title "Shear Modulus"'   
   ENDIF
      CALL setterm() 
      CALL setoutput(val0)
      CALL unset1() 
      CALL set1_2d()
      CALL settit(val0)  
  IF (cval1=="n" ) THEN       
      WRITE(2,'(a)')'pl "shear_2d_sys.dat" w l lc "blue" lw 2 title "Shear Modulus"'   
   ELSE
      WRITE(2,'(3a)')'pl "shear_2d_sys.dat" w l lc "',cval1,'" lw 2 title "Shear Modulus"'   
   ENDIF
      WRITE(*,"(A,F6.2,A,F6.2,A)")" > Using: go to gnuplot, call '2Dshear.gpi' '",MAXsh/4d0,"' '",MAXsh+2d0,"' (or other scale)  "
 ENDIF
 
 end SUBROUTINE
 !2D2DD##############################################

 SUBROUTINE twoD_phm(val,clor_val)
 ChARACTER(len=10) :: val ,clor_val
 
if (val=='phmpoi' ) THEN
  ! WRITE(*,'(2a)') val,'was READ well...'
  open(2,file='phm_2Dpoissons.gpi') 
  Call copyri()
  call setterm_phm()
  call setoutput(val)  
  CALL setphm(val) 
  CALL unset2()
  IF      (clor_val=='bbry') THEN
    WRITE(2,'(a)')' set palette rgb 7,5,15'
   ELSE IF (clor_val=='grv') THEN
    WRITE(2,'(a)')' set palette rgb 3,11,6'
   ELSE IF (clor_val=='bbvy') THEN
    WRITE(2,'(a)')' set palette rgb 30,31,32'
   ELSE IF (clor_val=='bgyr') THEN
    WRITE(2,'(a)')' set palette rgb 33,13,10'
   ELSE IF (clor_val=='bryw') THEN
    WRITE(2,'(a)')' set palette rgb 34,35,36'
   ELSE
     WRITE(2,'(a)')' set palette rgb 30,13,10'
   END IF

   WRITE(2,'(a)')'#set size   0.3333,1.0'
   WRITE(2,'(a)')'#set origin 0.5, 0.0'
  WRITE(2,'(a)')'set title "Positive Poissons ratio"'
  WRITE(2,'(a)')'set cblabel "{/Symbol n}_{ Positive}"'
  WRITE(2,'(a)')'pl "poisson_2d_sys.dat" u 1:(1):3 w l  pal lw 8,\'
  WRITE(2,'(a)')'   "poisson_2d_sys.dat" u 1:(1.03):3 w l  pal lw 8'

  WRITE(2,'(a)')'#set size   0.3333, 1.0'
  WRITE(2,'(a)')'#set origin 0.15 , 0.0'
  WRITE(2,'(a)')'set title "Negative Poissons ratio"'
  WRITE(2,'(a)')'set cblabel "{/Symbol n}_{ Negative}"'
  WRITE(2,'(a)')'pl   "poisson_2d_sys.dat" u 1:(1.0):4 w l  pal lw 8,\'
  WRITE(2,'(a)')'     "poisson_2d_sys.dat" u 1:(1.03):4 w l  pal lw 8'

  WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: gnuplot phm_2Dpoissons.gpi"
ENDIF

if (val=='phmyon' ) THEN
  ! WRITE(*,'(2a)') val,'was READ well...'
  open(2,file='phm_2Dyoung.gpi') 
  Call copyri()
  call setterm_phm()
  call setoutput(val)  
  CALL setphm(val) 
  CALL unset2()
  IF      (clor_val=='bbry') THEN
    WRITE(2,'(a)')' set palette rgb 7,5,15'
   ELSE IF (clor_val=='grv') THEN
    WRITE(2,'(a)')' set palette rgb 3,11,6'
   ELSE IF (clor_val=='bbvy') THEN
    WRITE(2,'(a)')' set palette rgb 30,31,32'
   ELSE IF (clor_val=='bgyr') THEN
    WRITE(2,'(a)')' set palette rgb 33,13,10'
   ELSE IF (clor_val=='bryw') THEN
    WRITE(2,'(a)')' set palette rgb 34,35,36'
   ELSE
     WRITE(2,'(a)')' set palette rgb 30,13,10'
   END IF

  WRITE(2,'(a)')'# set size   0.3333,1.0'
  WRITE(2,'(a)')'# set origin 0.3333, 0.0'
  WRITE(2,'(a)')'set title "Young modulus"'
  WRITE(2,'(a)')'set cblabel "N/m"'
  WRITE(2,'(a)')'pl   "young_2d_sys.dat" u 1:(1.0):2 w l  pal lw 8,"young_2d_sys.dat" u 1:(1.03):2 w l  pal lw 8'

  WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: gnuplot phm_2Dyoung.gpi"
ENDIF

if (val=='phmshe' ) THEN
   ! WRITE(*,'(2a)') val,'was READ well...'
  open(2,file='phm_2Dshear.gpi') 
  Call copyri()
  call setterm_phm()
  call setoutput(val)  
  CALL setphm(val) 
  CALL unset2()
  IF      (clor_val=='bbry') THEN
    WRITE(2,'(a)')' set palette rgb 7,5,15'
   ELSE IF (clor_val=='grv') THEN
    WRITE(2,'(a)')' set palette rgb 3,11,6'
   ELSE IF (clor_val=='bbvy') THEN
    WRITE(2,'(a)')' set palette rgb 30,31,32'
   ELSE IF (clor_val=='bgyr') THEN
    WRITE(2,'(a)')' set palette rgb 33,13,10'
   ELSE IF (clor_val=='bryw') THEN
    WRITE(2,'(a)')' set palette rgb 34,35,36'
   ELSE
     WRITE(2,'(a)')' set palette rgb 30,13,10'
   END IF

  WRITE(2,'(a)')'#set size   0.3333,1.0'
  WRITE(2,'(a)')'#set origin 0.3333, 0.0'
  WRITE(2,'(a)')'set title "Shear modulus"'
  WRITE(2,'(a)')'set cblabel "N/m"'
  WRITE(2,'(a)')'pl  "shear_2d_sys.dat" u 1:(1.0):2 w l  pal lw 8,"shear_2d_sys.dat" u 1:(1.03):2 w l  pal lw 8'

  WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: gnuplot phm_2Dshear.gpi"
ENDIF

if (val=='phmall' ) THEN
  ! WRITE(*,'(2a)') val,'was READ well...'
  open(2,file='phm_All.gpi') 
  Call copyri()
  call setterm_phm()
  call setoutput(val)  
  CALL setphm(val) 
  CALL unset2()
 
  IF      (clor_val=='bbry') THEN
    WRITE(2,'(a)')' set palette rgb 7,5,15'
   ELSE IF (clor_val=='grv') THEN
    WRITE(2,'(a)')' set palette rgb 3,11,6'
   ELSE IF (clor_val=='bbvy') THEN
    WRITE(2,'(a)')' set palette rgb 30,31,32'
   ELSE IF (clor_val=='bgyr') THEN
    WRITE(2,'(a)')' set palette rgb 33,13,10'
   ELSE IF (clor_val=='bryw') THEN
    WRITE(2,'(a)')' set palette rgb 34,35,36'
   ELSE
     WRITE(2,'(a)')' set palette rgb 30,13,10'
   END IF


  WRITE(2,'(a)')'set title "Positive Poissons ratio"'
  WRITE(2,'(a)')'set cblabel "{/Symbol n}_{ Positive}"'
  WRITE(2,'(a)')'pl "poisson_2d_sys.dat" u 1:(1):3 w l  pal lw 8, "poisson_2d_sys.dat" u 1:(1.03):3 w l  pal lw 8'

  WRITE(2,'(a)')'#set size   0.3333, 1.0'
  WRITE(2,'(a)')'#set origin 0.15 , 0.0'
  WRITE(2,'(a)')'set title "Negative Poissons ratio"'
  WRITE(2,'(a)')'set cblabel "{/Symbol n}_{ Negative}"'
  WRITE(2,'(a)')'pl   "poisson_2d_sys.dat" u 1:(1.0):4 w l  pal lw 8,"poisson_2d_sys.dat" u 1:(1.03):4 w l  pal lw 8'
  WRITE(2,'(a)')'set title "Young modulus"'
  WRITE(2,'(a)')'set cblabel "N/m"'
  WRITE(2,'(a)')'pl   "young_2d_sys.dat" u 1:(1.0):2 w l  pal lw 8,"young_2d_sys.dat" u 1:(1.03):2 w l  pal lw 8'
  WRITE(2,'(a)')'set title "Shear modulus"'
  WRITE(2,'(a)')'set cblabel "N/m"'
  WRITE(2,'(a)')'pl  "shear_2d_sys.dat" u 1:(1.0):2 w l  pal lw 8,"shear_2d_sys.dat" u 1:(1.03):2 w l  pal lw 8'
  
  WRITE(*,"(A,F5.2,A,F5.2,A)")" > Using: gnuplot phm_All.gpi"
ENDIF
end SUBROUTINE twoD_phm
