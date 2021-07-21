
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 09/08/2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; "agr_conv: data file ( of AAEP code) to agr file. 

PROGRAM data2D_to_agr
   
 INTEGER :: argl,i,N_frame=0,Nploter,Ng,Ns
 ChARACTER(len=10) :: val='',namepro=' '
 ChARACTER(len=:), allocatable :: a
 DOUBLE PRECISION :: rho1,rho2,rho3,rho4,rho5,x, y,theta1,theta2
 ChARACTER(7),DIMENSION(5) :: colorr  
 REAL(8) :: xticmin,yticmin,xticmax,yticmax,tmajor_yy,tmajor_xx=90,x1,y1,x2,y2,Maxyoung,Minyoung,Maxcomp,ASF=1d0,&
            Mincomp,G_max2,G_min2,pugh_max2,pugh_min2,Maxbulk,Minbulk,Pratio_max,Pratio_min,maxEVaTMf,maxEVaLM,minEVaTMf,Minimum, Maximum,ac,min0,max0
 INTEGER, DIMENSION(6) :: colora=4
 ChARACTER(30) :: nlabel,nameinp
 ChARACTER(6) :: Xlabel,FT='false'
 ChARACTER(4) :: onoff
 REAL(8), DIMENSION(400) :: sh,shtheta, com,comtheta, poi,poitheta, bu,butheta, yo,yotheta, so,sotheta,temp	
	
	
     ! Get command line args (Fortran 2003 standard)
  if (command_argument_count() > 0) then
     CALL get_command_argument(1, length=argl)
     allocate(ChARACTER(argl) :: a)
     CALL get_command_argument(1, a)
     READ(a,*) namepro
     val=namepro	

  endif
	 if (val == "-h" .OR. val=="") then
		WRITE(*,*)'Using: dat2agr_lapw [ Properties ] in the  DatFile_*** folder '
  WRITE(*,*)''
  CALL system ("sleep 1")
  WRITE(*,*)'[3D Properties]: '
  WRITE(*,*)' box   => All elastic properties in Cartesian coordinates '
  WRITE(*,*)' polar => All elastic properties in polar coordinates     '
		STOP
	 endif

	 if ( val=='Box' .OR. val=='box' .OR. val=='bo'.or. val=='Polar' .OR. val=='polar' .OR. val=='po') then
		write(*,*)''
	 else
		WRITE(*,*)'Using: dat2agr_lapw [ box or polar] in the  DatFile_*** folder ';STOP
	 endif
	 

	 WRITE(*,*) " > Scale factor for ploting (If needed):"
	 READ(*,*)ASF
     WRITE(*,'(2a)') namepro,'was READ well...'
     WRITE(*,'(a,F4.2)') "Scale factor is  ",ASF 
     WRITE(*,*) "========================"
 
!===========================================================================
		  !===frame===
		  !view 0.150000, 0.531818, 0.442388, 0.850000  ! 0.150000, 0.554545, 0.442388, 0.850000
		  !view 0.500865, 0.531818, 0.793253, 0.850000  ! 0.500865, 0.554545, 0.793253, 0.850000
		  !view 0.851730, 0.531818, 1.144118, 0.850000  ! 0.851730, 0.554545, 1.144118, 0.850000
		  !view 0.150000, 0.150000, 0.442388, 0.468182  ! 0.150000, 0.200000, 0.442388, 0.495455
		  !view 0.500865, 0.150000, 0.793253, 0.468182  ! 0.500865, 0.200000, 0.793253, 0.495455
		  !view 0.851730, 0.150000, 1.144118, 0.468182  ! 0.851730, 0.200000, 1.144118, 0.495455
!===========================================================================	
  	  colorr(1)="black"
	  colorr(2)="red"
	  colorr(3)="green"
	  colorr(4)="blue"
!===========================================================================	  
   if (val=='Box' .OR. val=='box' .OR. val=='bo')     then; open(2,file='2Dbox.agr') 	;endif
   if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; open(2,file='2Dpolar.agr'); FT='true' 	;endif   
	 
	     CALL start_agr(colorr) 
!===============================================================================sss
if (val=='Box' .OR. val=='box' .OR. val=='bo') then; Xlabel=' '; endif
if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; onoff="on"; Xlabel=' '; endif
if (val=='Box' .OR. val=='box' .OR. val=='bo') then;onoff="off"; endif
!=========================================================================== Start Shear
	  N_frame=0 !g0
	  CALL curve_frame1_agr(N_frame)
	    nameinp='shear'
       CALL autoscale(tmajor_yy,G_min2,G_max2,nameinp)	     
	  xticmin=0; yticmin=G_min2  ; xticmax=360; yticmax=G_max2; nlabel='Shear Modulus (GPa)'
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90
      if (val=='Polar' .OR. val=='polar' .OR. val=='po') then
	     xticmin=-G_max2; yticmin=-G_max2  ; xticmax=G_max2; yticmax=G_max2;tmajor_xx=tmajor_yy*ASF
	  endif	 
      x1=0.150000; y1=0.554545; x2=0.442388; y2=0.850000  !0.150000, 0.554545, 0.442388, 0.850000
	  CALL set_xy_agr(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT)
!!	  
	  Nploter=0 ;colora=2  !s0
	  CALL set_nplot_agr(Nploter,colora)
      Nploter=1; colora=4  !s1
	  CALL set_nplot_agr(Nploter,colora)
      Nploter=2; colora=3  !s2
	  CALL set_nplot_agr(Nploter,colora)
	  
	  Ng=0;Ns=0  !g0.s0
	  CALL Win_start_agr(Ng,Ns)
!====box
  	   open(4,file='2dcut_shear.dat',err=44)
          do i=1,361
        READ(4,*) theta2,rho2,rho3,rho4
        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho4; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho4,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====box
	  CALL Win_end_agr()
	  
	  !========================================

	  Ng=0;Ns=1  
	  CALL Win_start_agr(Ng,Ns)	  
!====
  	   open(3,file='2dcut_shear.dat',err=44)
          do i=1,361
             READ(3,*) theta2,rho2,rho3,rho4
        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(3)
!====	  
	  CALL Win_end_agr()
	  
	  !========================================	 
	  
	  Ng=0;Ns=2  
	  CALL Win_start_agr(Ng,Ns)	  
!====
  	   open(3,file='2dcut_shear.dat',err=44)
          do i=1,361
             READ(3,*) theta2,rho2,rho3,rho4
        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho3; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho3,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(3)
!====	  
	  CALL Win_end_agr()
	  
	  !========================================		  

!=========================================================================== END Shear
if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; onoff="on" ; endif
if (val=='Box' .OR. val=='box' .OR. val=='bo') then;onoff="off"; endif
!=========================================================================== Start Young 
	  
	  N_frame=1 !g1
	  CALL curve_frame1_agr(N_frame)
	    nameinp='young'
       CALL autoscale(tmajor_yy,Minyoung,Maxyoung,nameinp)	
	  xticmin=0; yticmin=Minyoung  ; xticmax=360; yticmax=Maxyoung; nlabel='Young Modulus (GPa)'
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90
      if (val=='Polar' .OR. val=='polar' .OR. val=='po') then
	     xticmin=-Maxyoung; yticmin=-Maxyoung  ; xticmax=Maxyoung; yticmax=Maxyoung;tmajor_xx=tmajor_yy*ASF
	  endif    
	  x1=0.500865; y1=0.554545; x2=0.793253; y2=0.850000  !0.500865, 0.554545, 0.793253, 0.850000
	  CALL set_xy_agr(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT)

	  Nploter=0 ;colora=3  !s0
	  CALL set_nplot_agr(Nploter,colora)
	  
	  Ng=1;Ns=0  !g1.s0
	  CALL Win_start_agr(Ng,Ns)
	  
 !====
  	   open(4,file='2dcut_young.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3
        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr()
	  !========================================	

!=========================================================================== END Young
if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; onoff="on" ; endif
if (val=='Box' .OR. val=='box' .OR. val=='bo') then;onoff="off"; endif
!=========================================================================== Start comper... 
	 
	  N_frame=2 !g2
	  CALL curve_frame1_agr(N_frame)
     	    nameinp='comp'
       CALL autoscale(tmajor_yy,Mincomp,Maxcomp,nameinp)
	  xticmin=0; yticmin=Mincomp ; xticmax=360; yticmax=Maxcomp; nlabel='Linear Compressibiliy (1/TPa)'
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90
      if (val=='Polar' .OR. val=='polar' .OR. val=='po') then
	     xticmin=-Maxcomp; yticmin=-Maxcomp ; xticmax=Maxcomp; yticmax=Maxcomp; tmajor_xx=tmajor_yy*ASF
	  endif
	  x1=0.851730; y1=0.554545; x2=1.144118; y2=0.850000 !0.851730, 0.554545, 1.144118, 0.850000
	  CALL set_xy_agr(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT)

	  Nploter=0 ;colora=3  !s0
	  CALL set_nplot_agr(Nploter,colora)
	   Nploter=1 ;colora=2  !s1
	  CALL set_nplot_agr(Nploter,colora)
	  
	  Ng=2;Ns=0  !g2.s0
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_comp.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3
        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================
	 
	  Ng=2;Ns=1  !g2.s1
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_comp.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3
        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho3; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho3,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================	  
	  
!=========================================================================== END comper...	  
if (val=='Box' .OR. val=='box' .OR. val=='bo') then; Xlabel="Degree"; onoff="on"; endif  
if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; onoff="on" ;Xlabel=' '; endif
!=========================================================================== Start bulk

	  N_frame=3 !g3
	  CALL curve_frame1_agr(N_frame)
     	    nameinp='bulk'
       CALL autoscale(tmajor_yy,Minbulk,Maxbulk,nameinp)
	  xticmin=0; yticmin=Minbulk  ; xticmax=360; yticmax=Maxbulk; nlabel='Bulk Modulus (GPa)'
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90
     if (val=='Polar' .OR. val=='polar' .OR. val=='po') then
	   xticmin=-Maxbulk; yticmin=-Maxbulk; xticmax=Maxbulk; yticmax=Maxbulk;tmajor_xx=tmajor_yy*ASF*3d0 
	 endif	
     x1=0.150000; y1=0.200000; x2=0.442388; y2=0.495455  !0.150000, 0.200000, 0.442388, 0.468182
	  CALL set_xy_agr(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF*3d0 ,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT)

	  Nploter=0 ;colora=3  !s0
	  CALL set_nplot_agr(Nploter,colora)
	   Nploter=1 ;colora=2  !s1
	  CALL set_nplot_agr(Nploter,colora)
	  
	  Ng=3;Ns=0  !g3.s0
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_bulk.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3 
        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho3; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho3,x,y); WRITE(2,'(F12.6,2X,F12.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================
	 
	  Ng=3;Ns=1  !g3.s1
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_bulk.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3
        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F12.6,2X,F12.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================	  
	  
!=========================================================================== END Bulk
!if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; onoff="on" ; endif	  
!=========================================================================== Start Poissons
	  N_frame=4 !g4
	  CALL curve_frame1_agr(N_frame)
     	    nameinp='poisson'
       CALL autoscale(tmajor_yy,Pratio_min,Pratio_max,nameinp)
	  xticmin=0; yticmin=Pratio_min  ; xticmax=360; yticmax=Pratio_max; nlabel='Poissons Ratio'
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90
     if (val=='Polar' .OR. val=='polar' .OR. val=='po') then
	 xticmin=-Pratio_max; yticmin=-Pratio_max  ; xticmax=Pratio_max; yticmax=Pratio_max; tmajor_xx=tmajor_yy*ASF
	 endif
	  x1=0.500865; y1=0.200000; x2=0.793253; y2=0.495455  !0.500865, 0.200000, 0.793253, 0.468182
	  CALL set_xy_agr(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT)
	   Nploter=0 ;colora=2  !s0
	  CALL set_nplot_agr(Nploter,colora)
	   Nploter=1 ;colora=3  !s1
	  CALL set_nplot_agr(Nploter,colora)
	   Nploter=2 ;colora=4  !s1
	  CALL set_nplot_agr(Nploter,colora)	  
	  Ng=4;Ns=0  !g4.s0
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_poisson.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3,rho4
	        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho4; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho4,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================
	 
	  Ng=4;Ns=1  !g4.s1
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_poisson.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3,rho4
        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho3; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho3,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================
	 
	  Ng=4;Ns=2  !g4.s2
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_poisson.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3,rho4
        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================	 	 
	  
!=========================================================================== END Poissons	  
	  
!=========================================================================== Start Sound
!	  N_frame=5 !g5
!	  CALL curve_frame1_agr(N_frame)
!     	    nameinp='sound'
!       CALL autoscale(tmajor_yy,minEVaTMf,maxEVaTMf,nameinp)
! 	   xticmin=0; yticmin=minEVaTMf  ; xticmax=360; yticmax=maxEVaTMf; nlabel='Sound'
!	   tmajor_yy=tmajor_yy*ASF
!	   tmajor_xx=90
!	   if (val=='Polar' .OR. val=='polar' .OR. val=='po') then
!!	   endif
!	   x1=0.851730; y1=0.200000; x2=1.144118; y2=0.495455  !0.851730, 0.200000, 1.144118, 0.468182
!	  CALL set_xy_agr(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT)
!
!	   Nploter=0 ;colora=2  !s0
!	  CALL set_nplot_agr(Nploter,colora)
!	   Nploter=1 ;colora=4  !s1
!	  CALL set_nplot_agr(Nploter,colora)
!	   Nploter=2 ;colora=3  !s1
!	  CALL set_nplot_agr(Nploter,colora)	  
!	  Ng=5;Ns=0  !g5.s0
!	  CALL Win_start_agr(Ng,Ns)
!====
!  	   open(4,file='2dsoundTMmin.dat',err=44)
 !         do i=1,361
!             READ(4,*) theta2,rho2
!        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2; endif
!        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
!         enddo  
!	   CLOSE(4)
!====	   
!	  CALL Win_end_agr() 
	 !========================================
	 
!	  Ng=5;Ns=1  !g4.s1
!	  CALL Win_start_agr(Ng,Ns)
!====
!  	   open(4,file='2dsoundTMmax.dat',err=44)
!          do i=1,361
!             READ(4,*) theta2,rho2
!        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2; endif
!        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
!         enddo  
!	   CLOSE(4)
!====	   
!	  CALL Win_end_agr() 
	 !========================================
	 
!	  Ng=5;Ns=2  !g4.s2
!	  CALL Win_start_agr(Ng,Ns)
!====
!  	   open(4,file='2dsoundLM.dat',err=44)
!          do i=1,361
!             READ(4,*) theta2,rho2
!        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2; endif
!        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
!         enddo  
!	   CLOSE(4)
!====	   
!	  CALL Win_end_agr() 
	 !========================================	 	 
	  
!=========================================================================== END Sound	  

!=========================================================================== Start Pugh
	  N_frame=5 !g0
	  CALL curve_frame1_agr(N_frame)
	    nameinp='pugh'
	   CALL autoscale(tmajor_yy,pugh_min2,pugh_max2,nameinp)
	  xticmin=0; yticmin=pugh_min2  ; xticmax=360; yticmax=pugh_max2; nlabel='Pugh Ratio'
	  	   tmajor_yy=(tmajor_yy/10)*ASF
	   tmajor_xx=90
      if (val=='Polar' .OR. val=='polar' .OR. val=='po') then
	     xticmin=-pugh_max2; yticmin=-pugh_max2  ; xticmax=pugh_max2; yticmax=pugh_max2;tmajor_xx=tmajor_yy*ASF
	  endif	 
	   x1=0.851730; y1=0.200000; x2=1.144118; y2=0.495455  !0.851730, 0.200000, 1.144118, 0.468182
	  CALL set_xy_agr(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT)
!!	  
	  Nploter=0 ;colora=2  !s0
	  CALL set_nplot_agr(Nploter,colora)
      Nploter=1; colora=4  !s1
	  CALL set_nplot_agr(Nploter,colora)
      Nploter=2; colora=3  !s2
	  CALL set_nplot_agr(Nploter,colora)
	  
	  Ng=5;Ns=0  !g0.s0
	  CALL Win_start_agr(Ng,Ns)
!====box
  	   open(4,file='2dcut_pugh.dat',err=44)
          do i=1,361
        READ(4,*) theta2,rho2,rho3,rho4
        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho4; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho4,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====box
	  CALL Win_end_agr()
	  
	  !========================================

	  Ng=5;Ns=1  
	  CALL Win_start_agr(Ng,Ns)	  
!====
  	   open(3,file='2dcut_pugh.dat',err=44)
          do i=1,361
             READ(3,*) theta2,rho2,rho3,rho4
        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(3)
!====	  
	  CALL Win_end_agr()
	  
	  !========================================	 
	  
	  Ng=5;Ns=2  
	  CALL Win_start_agr(Ng,Ns)	  
!====
  	   open(3,file='2dcut_pugh.dat',err=44)
          do i=1,361
             READ(3,*) theta2,rho2,rho3,rho4
        if (val=='Box' .OR. val=='box' .OR. val=='bo') then;   WRITE(2,'(F11.6,2X,F11.6)')theta2,rho3; endif
        if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; CALL polar2xy(theta2,rho3,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(3)
!====	  
	  CALL Win_end_agr()
	  
	  !========================================		  

!=========================================================================== END Pugh


	    
	      CLOSE(2);  if (val=='Box' .OR. val=='box' .OR. val=='bo') then     
		             print*, "========================" ;print*, " => 2Dbox.agr generated" ;endif
	                 if (val=='Polar' .OR. val=='polar' .OR. val=='po') then 
					 print*, "========================" ;print*, " => 2Dpolar.agr generated";endif
!#############################################################################################	  
!#############################################################################################

	 44  stop 
  END PROGRAM
