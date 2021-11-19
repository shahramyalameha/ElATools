
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 09/08/2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; "agr_conv: data file ( of AAEP code) to agr file. 

PROGRAM data2D_to_agr
   
 INTEGER :: argl,i,N_frame=0,Nploter,Ng,Ns,phi_meah,theta_meah,cutmesh
 ChARACTER(len=10) :: val='',namepro=' '
 ChARACTER(len=:), allocatable :: a
 DOUBLE PRECISION :: rho1,rho2,rho3,rho4,rho5,x, y,theta1,theta2
 ChARACTER(7),DIMENSION(5) :: colorr  
 REAL(8) :: xticmin,yticmin,xticmax,yticmax,tmajor_yy,tmajor_xx=90,x1,y1,x2,y2,Maxyoung,Minyoung,Maxcomp,ASF=1d0,&
            Mincomp,G_max2,G_min2,pugh_max2,pugh_min2,Maxbulk,Minbulk,Pratio_max,Pratio_min,maxEVaTMf,maxEVaLM,minEVaTMf,Minimum, Maximum,ac,min0,max0,&
            char_z_x,char_z_y,Npro,frame_style=1.0, Minhard, Maxhard,Minkm, Maxkm
 INTEGER, DIMENSION(6) :: colora=4
 ChARACTER(30) :: nlabel,nameinp,label_dat 
 ChARACTER(30) :: Xlabel
 ChARACTER(6) :: FT='false'
 ChARACTER(4) :: onoff
 REAL(8), DIMENSION(400) :: sh,shtheta, com,comtheta, poi,poitheta, bu,butheta, yo,yotheta, so,sotheta,temp	
	
	open(875, file="MESH")
	 read(875, *) phi_meah, theta_meah, cutmesh
	close(875)
	
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
  WRITE(*,*)' box        => All elastic properties in Cartesian coordinates    '
  WRITE(*,*)' boxpoi     => Poisson ratio in Cartesian coordinates             '
  WRITE(*,*)' boxyoung   => Young’s modulus in Cartesian coordinates           '
  WRITE(*,*)' boxshear   => Shear modulus in Cartesian coordinates             '
  WRITE(*,*)' boxcomp    => Linear compressibility  in Cartesian coordinates   '
  WRITE(*,*)' boxbulk    => Bulk modulus in Cartesian coordinates              '
  WRITE(*,*)' boxhard    => Hardness in Cartesian coordinates                  '
  WRITE(*,*)' boxkm      => Min. thermal conductivity in Cartesian coordinates '
  WRITE(*,*)' boxpall    => All phase velocities in Cartesian coordinates '  
  WRITE(*,*)' boxgall    => All group velocities in Cartesian coordinates '  
  WRITE(*,*)' boxpfall   => All power flow angles in Cartesian coordinates '  
  WRITE(*,*)""
  WRITE(*,*)' polar      => All elastic properties in polar coordinates     '
  WRITE(*,*)' polarpoi   => Poisson ratio in polar coordinates             '
  WRITE(*,*)' polaryoung => Young’s modulus in polar coordinates           '
  WRITE(*,*)' polarshear => Shear modulus in polar coordinates             '
  WRITE(*,*)' polarcomp  => Linear compressibility  in polar coordinates   '
  WRITE(*,*)' polarbulk  => Bulk modulus in polar coordinates              '
  WRITE(*,*)' polarhard  => Hardness in polar coordinates                  '
  WRITE(*,*)' polarkm    => Min. thermal conductivity in polar coordinates '
  WRITE(*,*)' polarpall  => All phase velocities in polar coordinates '  
  WRITE(*,*)' polargall  => All group velocities in polar coordinates '  
  WRITE(*,*)' polarpfall => All power flow angles in polar coordinates ' 
 
		STOP
	 endif

	 if ( val=='Box' .OR. val=='box' .OR. val=='bo'.or. val=='Polar' .OR. val=='polar' .OR. val=='po' .or. &
	      val=='polarpoi' .OR. val=='Polarpoi' .OR. val=='popoi' .or. &
	      val=='Boxpoi' .OR. val=='boxpoi' .OR. val=='bopoi' .or. val=='Boxshear' .OR. val=='boxshear' .OR. val=='boshear' .or. &
	      val=='Boxyoung' .OR. val=='boxyoung' .OR. val=='boyoung' .or. val=='Boxbulk' .OR. val=='boxbulk' .OR. val=='bobulk' .or. &
	      val=='Boxcomp' .OR. val=='boxcomp' .OR. val=='bocomp'.or. val=='polarshear' .OR. val=='Polarshear' .OR. val=='pshear' .or. &
	      val=='polaryoung' .OR. val=='Polaryoung' .OR. val=='pyoung' .or. val=='polarbulk' .OR. val=='Polarbulk' .OR. val=='pbulk'.or. &
	      val=='polarcomp' .OR. val=='Polarcomp' .OR. val=='pocomp'.or. val=='Boxgall' .OR. val=='boxgall' .OR. val=='bogall'.or. &
	      val=='polargall' .OR. val=='Polargall' .OR. val=='pogall'.or. val=='Boxpall' .OR. val=='boxpall' .OR. val=='bopall'.or.&
	      val=='polarpall' .OR. val=='Polarpall' .OR. val=='popall'.or. val=='Boxhard' .OR. val=='boxhard' .OR. val=='bohard' .or. &
       val=='polarhard' .OR. val=='Polarhard' .OR. val=='pohard'.or. val=='Boxkm' .OR. val=='boxkm' .OR. val=='bokm' .or. &
       val=='polarkm' .OR. val=='Polarkm' .OR. val=='pokm'.or. val=='Boxpfall' .OR. val=='boxpfall' .OR. val=='bopfall'.or. &
       val=='polarpfall' .OR. val=='Polarpfall' .OR. val=='popfall') then
		write(*,*)''
	 else
		WRITE(*,*)'Using: dat2agr_lapw [ box or polar] in the  DatFile_*** folder ';STOP
	 endif
	 

	 WRITE(*,*) " > Scale factor for ploting (If needed):"
	 READ(*,*)ASF
     WRITE(*,'(2a)') namepro,'was READ well...'
     WRITE(*,'(a,F4.2)') "Scale factor is  ",ASF 
     WRITE(*,*) "========================"
if (val=='Box' .OR. val=='box' .OR. val=='bo'.or. val=='Polar' .OR. val=='polar' .OR. val=='po') Npro=1.0
!===========================================================================
		  !===frame=== 3*2
		  !view 0.150000, 0.531818, 0.442388, 0.850000  ! 0.150000, 0.554545, 0.442388, 0.850000
		  !view 0.500865, 0.531818, 0.793253, 0.850000  ! 0.500865, 0.554545, 0.793253, 0.850000
		  !view 0.851730, 0.531818, 1.144118, 0.850000  ! 0.851730, 0.554545, 1.144118, 0.850000
		  !view 0.150000, 0.150000, 0.442388, 0.468182  ! 0.150000, 0.200000, 0.442388, 0.495455
		  !view 0.500865, 0.150000, 0.793253, 0.468182  ! 0.500865, 0.200000, 0.793253, 0.495455
		  !view 0.851730, 0.150000, 1.144118, 0.468182  ! 0.851730, 0.200000, 1.144118, 0.495455
		  !===frame=== 1*1	  
		  ! view 0.150000, 0.150000, 1.150000, 0.850000
!===========================================================================	
  	colorr(1)="black"
	  colorr(2)="red"
	  colorr(3)="green"
	  colorr(4)="blue"
!===========================================================================	 

!---------------------------------------------------------------------------
!                            START 2Dbox Poisson        
!---------------------------------------------------------------------------
if (val=='Boxpoi' .OR. val=='boxpoi' .OR. val=='bopoi'.or. &
    val=='polarpoi' .OR. val=='Polarpoi' .OR. val=='popoi') then
 if (val=='Boxpoi' .OR. val=='boxpoi' .OR. val=='bopoi')      then;  frame_style=1.0; open(2,file='2Dbox_poisson.agr'); endif
 if (val=='polarpoi' .OR. val=='Polarpoi' .OR. val=='popoi' ) then;  FT='true'; frame_style=0.0 ;open(2,file='2Dpolar_poisson.agr'); endif   
 
  CALL start_agr(colorr)    	 
if (val=='Boxpoi' .OR. val=='boxpoi' .OR. val=='bopoi')  Xlabel='Degree'    
if (val=='Boxpoi' .OR. val=='boxpoi' .OR. val=='bopoi')  onoff ="on"  

if (val=='polarpoi' .OR. val=='Polarpoi' .OR. val=='popoi')   Xlabel=' '
if (val=='polarpoi' .OR. val=='Polarpoi' .OR. val=='popoi')   onoff ="off"  
  N_frame=0 !g0
	 CALL curve_frame1_agr(N_frame)
  nameinp='poisson'
  call automakescal(tmajor_yy, Pratio_min, Pratio_max, nameinp)
 ! CALL autoscale(tmajor_yy,Pratio_min,Pratio_max,nameinp)
	  xticmin=0; yticmin=Pratio_min  ; xticmax=360; yticmax=Pratio_max; nlabel='Poissons Ratio'
   tmajor_yy=tmajor_yy*ASF
   tmajor_xx=90
  if (val=='polarpoi' .OR. val=='Polarpoi' .OR. val=='popoi') then
	  xticmin=-Pratio_max; yticmin=-Pratio_max  ; xticmax=Pratio_max; yticmax=Pratio_max; tmajor_xx=tmajor_yy*ASF
	 endif
 	
   	  x1=0.200000; y1=0.200000; x2=0.900000; y2=0.900000   !0.150000, 0.150000, 1.150000, 0.850000
   	  char_z_x=1.1
   	  char_z_y=1.1
	  CALL set_xy_agr_1frame(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT,char_z_x,char_z_y,frame_style)
	   Nploter=0 ;colora=2; label_dat="Negative" !s0
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	   Nploter=1 ;colora=3; label_dat="Min. Positive"  !s1
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	   Nploter=2 ;colora=4; label_dat="Max. Pisitive"  !s1
	  CALL set_nplot_agr(Nploter,colora,label_dat)	
	  !====
  	   open(4,file='2dcut_poisson.dat',err=44)
          do i=1,phi_meah+1
             READ(4,*) theta2,rho2,rho3,rho4
	          if (val=='Boxpoi' .OR. val=='boxpoi' .OR. val=='bopoi')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho4 
	          if (val=='polarpoi' .OR. val=='Polarpoi' .OR. val=='popoi') then; CALL polar2xy(theta2,rho4,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif         
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr()
	 !========================================
	 
	  Ng=0;Ns=1  !g4.s1
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_poisson.dat',err=44)
          do i=1,phi_meah+1
           READ(4,*) theta2,rho2,rho3,rho4
           if (val=='Boxpoi' .OR. val=='boxpoi' .OR. val=='bopoi')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho3 
           if (val=='polarpoi' .OR. val=='Polarpoi' .OR. val=='popoi') then; CALL polar2xy(theta2,rho3,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif         
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================
	 
	  Ng=0;Ns=2  !g4.s2
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_poisson.dat',err=44)
          do i=1,phi_meah+1
             READ(4,*) theta2,rho2,rho3,rho4
           if (val=='Boxpoi' .OR. val=='boxpoi' .OR. val=='bopoi')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2 
           if (val=='polarpoi' .OR. val=='Polarpoi' .OR. val=='popoi') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif         
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================	 		  
	  close(2)
	                if (val=='Boxpoi' .OR. val=='boxpoi' .OR. val=='bopoi') then     
		                  print*, "========================" ;print*, " => 2Dbox_poisson.agr generated" ; endif
	                 if (val=='polarpoi' .OR. val=='Polarpoi' .OR. val=='popoi') then 
					           print*, "========================" ;print*, " => 2Dpolar_poisson.agr generated";endif
endif	
!---------------------------------------------------------------------------
!                            END 2Dbox Poisson        
!---------------------------------------------------------------------------

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!---------------------------------------------------------------------------
!                            START 2Dbox Shear        
!---------------------------------------------------------------------------
if (val=='Boxshear'   .OR. val=='boxshear'   .OR. val=='boshear' .or. &
    val=='polarshear' .OR. val=='Polarshear' .OR. val=='poshear') then
if (val=='Boxshear'   .OR. val=='boxshear'   .OR. val=='boshear') then;            frame_style=1.0; open(2,file='2Dbox_shear.agr'); endif
if (val=='polarshear' .OR. val=='Polarshear' .OR. val=='poshear' ) then;  FT='true';frame_style=0.0 ;open(2,file='2Dpolar_shear.agr'); endif

  CALL start_agr(colorr)    	 
if (val=='Boxshear'   .OR. val=='boxshear'   .OR. val=='boshear')  Xlabel='Degree'    
if (val=='Boxshear'   .OR. val=='boxshear'   .OR. val=='boshear')  onoff ="on"  

if (val=='polarshear' .OR. val=='Polarshear' .OR. val=='poshear' )   Xlabel=' '
if (val=='polarshear' .OR. val=='Polarshear' .OR. val=='poshear' )   onoff ="off" 
  N_frame=0 !g0
	 CALL curve_frame1_agr(N_frame)
	  N_frame=0 !g0
	  CALL curve_frame1_agr(N_frame)
	    nameinp='shear'
	    call automakescal(tmajor_yy, G_min2, G_max2, nameinp)
       !CALL autoscale(tmajor_yy,G_min2,G_max2,nameinp)	     
	  xticmin=0; yticmin=G_min2  ; xticmax=360; yticmax=G_max2; nlabel='Shear modulus (GPa)'
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90
     if (val=='polarshear' .OR. val=='Polarshear' .OR. val=='poshear') then
	     xticmin=-G_max2; yticmin=-G_max2  ; xticmax=G_max2; yticmax=G_max2;tmajor_xx=tmajor_yy*ASF
	  endif
   	  x1=0.200000; y1=0.200000; x2=0.900000; y2=0.900000   !0.150000, 0.150000, 1.150000, 0.850000
	     char_z_x=1.1
   	  char_z_y=1.1
	  CALL set_xy_agr_1frame(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT,char_z_x,char_z_y,frame_style)
	  	  
	  Nploter=0 ;colora=2  !s0
	  CALL set_nplot_agr(Nploter,colora,label_dat)
      Nploter=1; colora=4  !s1
	  CALL set_nplot_agr(Nploter,colora,label_dat)
      Nploter=2; colora=3  !s2
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	  
	  Ng=0;Ns=0  !g0.s0
	  CALL Win_start_agr(Ng,Ns)
!====box
  	   open(4,file='2dcut_shear.dat',err=44)
          do i=1,phi_meah+1
        READ(4,*) theta2,rho2,rho3,rho4
         if (val=='Boxshear'   .OR. val=='boxshear'   .OR. val=='boshear') WRITE(2,'(F11.6,2X,F11.6)')theta2,rho4 
         if (val=='polarshear' .OR. val=='Polarshear' .OR. val=='poshear') then; CALL polar2xy(theta2,rho4,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif         
         enddo  
	   CLOSE(4)
!====box
	  CALL Win_end_agr()
	  
	  !========================================

	  Ng=0;Ns=1  
	  CALL Win_start_agr(Ng,Ns)	  
!====
  	   open(3,file='2dcut_shear.dat',err=44)
          do i=1,phi_meah+1
             READ(3,*) theta2,rho2,rho3,rho4
             if (val=='Boxshear'   .OR. val=='boxshear'   .OR. val=='boshear') WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2 
             if (val=='polarshear' .OR. val=='Polarshear' .OR. val=='poshear') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif 
         enddo  
	   CLOSE(3)
!====	  
	  CALL Win_end_agr()
	  
	  !========================================	 
	  
	  Ng=0;Ns=2  
	  CALL Win_start_agr(Ng,Ns)	  
!====
  	   open(3,file='2dcut_shear.dat',err=44)
          do i=1,phi_meah+1
             READ(3,*) theta2,rho2,rho3,rho4
             if (val=='Boxshear'   .OR. val=='boxshear'   .OR. val=='boshear')  WRITE(2,'(F11.6,2X,F11.6)')theta2,rho3
             if (val=='polarshear' .OR. val=='Polarshear' .OR. val=='poshear') then; CALL polar2xy(theta2,rho3,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(3)
!====	  
	  CALL Win_end_agr()
	  !========================================		
	   close(2); if (val=='Boxshear'   .OR. val=='boxshear'   .OR. val=='boshear') then     
		             print*, "========================" ;print*, " => 2Dbox_shear.agr generated" ;endif
	                 if (val=='polarshear' .OR. val=='Polarshear' .OR. val=='poshear') then 
					           print*, "========================" ;print*, " => 2Dpolar_shear.agr generated";endif
	endif  
!---------------------------------------------------------------------------
!                           END 2Dbox Shear        
!---------------------------------------------------------------------------
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!---------------------------------------------------------------------------
!                            START 2Dbox Young        
!---------------------------------------------------------------------------
if (val=='Boxyoung' .OR. val=='boxyoung' .OR. val=='boyoung' .or. &
    val=='polaryoung' .OR. val=='Polaryoung' .OR. val=='poyoung') then
if (val=='Boxyoung'   .OR. val=='boxyoung'   .OR. val=='boyoung') then; frame_style=1.0; open(2,file='2Dbox_young.agr'); endif
if (val=='polaryoung' .OR. val=='Polaryoung' .OR. val=='poyoung' ) then;  FT='true'; frame_style=0.0 ;open(2,file='2Dpolar_young.agr'); endif
 
  CALL start_agr(colorr)    	 
if (val=='Boxyoung'   .OR. val=='boxyoung'   .OR. val=='boyoung')  Xlabel='Degree'    
if (val=='Boxyoung'   .OR. val=='boxyoung'   .OR. val=='boyoung')  onoff ="on"  

if (val=='polaryoung' .OR. val=='Polaryoung' .OR. val=='poyoung')   Xlabel=' '
if (val=='polaryoung' .OR. val=='Polaryoung' .OR. val=='poyoung')   onoff ="off"
  N_frame=0 !g0
	 CALL curve_frame1_agr(N_frame)
	  N_frame=0 !g0
	  CALL curve_frame1_agr(N_frame)
	    nameinp='young'
    call automakescal(tmajor_yy, Minyoung, Maxyoung, nameinp)
       !CALL autoscale(tmajor_yy,Minyoung,Maxyoung,nameinp)	
	  xticmin=0; yticmin=Minyoung  ; xticmax=360; yticmax=Maxyoung;  nlabel='Young modulus (GPa)'
 
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90 
	   if (val=='polaryoung' .OR. val=='Polaryoung' .OR. val=='poyoung') then
	     xticmin=-Maxyoung; yticmin=-Maxyoung  ; xticmax=Maxyoung; yticmax=Maxyoung;tmajor_xx=tmajor_yy*ASF
	   endif    
	   	 x1=0.200000; y1=0.200000; x2=0.900000; y2=0.900000   !0.150000, 0.150000, 1.150000, 0.850000
	     char_z_x=1.1
   	  char_z_y=1.1
	  CALL set_xy_agr_1frame(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT,char_z_x,char_z_y,frame_style)

	  Nploter=0 ;colora=3  !s0
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	  
	  Ng=0;Ns=0  !g1.s0
	  CALL Win_start_agr(Ng,Ns)
	  
 !====
  	   open(4,file='2dcut_young.dat',err=44)
          do i=1,phi_meah+1
             READ(4,*) theta2,rho2,rho3
        if (val=='Boxyoung' .OR. val=='boxyoung' .OR. val=='boyoung')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2 
        if (val=='polaryoung' .OR. val=='Polaryoung' .OR. val=='poyoung') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr()
	  !========================================	
	  	   close(2) ; if (val=='Boxyoung'   .OR. val=='boxyoung'   .OR. val=='boyoung') then     
		             print*, "========================" ;print*, " => 2Dbox_young.agr generated" ;endif
	                 if (val=='polaryoung' .OR. val=='Polaryoung' .OR. val=='pyoung') then 
					           print*, "========================" ;print*, " => 2Dpolar_young.agr generated";endif
	endif 
!---------------------------------------------------------------------------
!                           END 2Dbox Young        
!---------------------------------------------------------------------------
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!---------------------------------------------------------------------------
!                            START 2Dbox hardness        
!---------------------------------------------------------------------------
if (val=='Boxhard' .OR. val=='boxhard' .OR. val=='bohard' .or. &
    val=='polarhard' .OR. val=='Polarhard' .OR. val=='pohard') then
if (val=='Boxhard'   .OR. val=='boxhard'   .OR. val=='bohard') then; frame_style=1.0; open(2,file='2Dbox_hard.agr'); endif
if (val=='polarhard' .OR. val=='Polarhard' .OR. val=='pohard' ) then;  FT='true'; frame_style=0.0 ;open(2,file='2Dpolar_hard.agr'); endif
 
  CALL start_agr(colorr)    	 
if (val=='Boxhard'   .OR. val=='boxhard'   .OR. val=='bohard')  Xlabel='Degree'    
if (val=='Boxhard'   .OR. val=='boxhard'   .OR. val=='bohard')  onoff ="on"  

if (val=='polarhard' .OR. val=='Polarhard' .OR. val=='pohard')   Xlabel=' '
if (val=='polarhard' .OR. val=='Polarhard' .OR. val=='pohard')   onoff ="off"
  N_frame=0 !g0
	 CALL curve_frame1_agr(N_frame)
	  N_frame=0 !g0
	  CALL curve_frame1_agr(N_frame)
	    nameinp='hard'
    call automakescal(tmajor_yy, Minhard, Maxhard, nameinp)
       !CALL autoscale(tmajor_yy,Minhard,Maxhard,nameinp)	
	  xticmin=0; yticmin=Minhard  ; xticmax=360; yticmax=Maxhard;  nlabel='Hardness'
 
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90 
	   if (val=='polarhard' .OR. val=='Polarhard' .OR. val=='pohard') then
	     xticmin=-Maxhard; yticmin=-Maxhard  ; xticmax=Maxhard; yticmax=Maxhard;tmajor_xx=tmajor_yy*ASF
	   endif    
	   	 x1=0.200000; y1=0.200000; x2=0.900000; y2=0.900000   !0.150000, 0.150000, 1.150000, 0.850000
	     char_z_x=1.1
   	  char_z_y=1.1
	  CALL set_xy_agr_1frame(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT,char_z_x,char_z_y,frame_style)

	  Nploter=0 ;colora=3  !s0
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	  
	  Ng=0;Ns=0  !g1.s0
	  CALL Win_start_agr(Ng,Ns)
	  
 !====
  	   open(4,file='2dcut_hardness.dat',err=44)
          do i=1,phi_meah+1
             READ(4,*) theta2,rho2,rho3
            if (val=='Boxhard' .OR. val=='boxhard' .OR. val=='bohard')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2 
            if (val=='polarhard' .OR. val=='Polarhard' .OR. val=='pohard') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr()
	  !========================================	
	  	   close(2) ; if (val=='Boxhard'   .OR. val=='boxhard'   .OR. val=='bohard') then     
		             print*, "========================" ;print*, " => 2Dbox_hardness.agr generated" ;endif
	                 if (val=='polarhard' .OR. val=='Polarhard' .OR. val=='phard') then 
					           print*, "========================" ;print*, " => 2Dpolar_hardness.agr generated";endif
	endif 
!---------------------------------------------------------------------------
!                           END 2Dbox hardness        
!---------------------------------------------------------------------------
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!---------------------------------------------------------------------------
!                             START 2Dbox Bulk        
!---------------------------------------------------------------------------
if (val=='Boxbulk' .OR. val=='boxbulk' .OR. val=='bobulk'  .or. &
    val=='polarbulk' .OR. val=='Polarbulk' .OR. val=='pobulk') then
if (val=='Boxbulk'   .OR. val=='boxbulk'   .OR. val=='bobulk') then; frame_style=1.0; open(2,file='2Dpolar_bulk.agr'); endif
if (val=='polarbulk' .OR. val=='Polarbulk' .OR. val=='pobulk' ) then;  FT='true'; frame_style=0.0 ;open(2,file='2Dbox_bulk.agr'); endif
 
  CALL start_agr(colorr)    	 
if (val=='Boxbulk' .OR. val=='boxbulk' .OR. val=='bobulk')  Xlabel='Degree'    
if (val=='Boxbulk' .OR. val=='boxbulk' .OR. val=='bobulk')  onoff ="on"  

if (val=='polarbulk' .OR. val=='Polarbulk' .OR. val=='pbulk' )   Xlabel=' '
if (val=='polarbulk' .OR. val=='Polarbulk' .OR. val=='pobulk' )   onoff ="off" 
  N_frame=0 !g0
	 CALL curve_frame1_agr(N_frame)
	  N_frame=0 !g0
	  CALL curve_frame1_agr(N_frame)
    	    nameinp='bulk'
    	    call automakescal(tmajor_yy, Minbulk, Maxbulk, nameinp)
      ! CALL autoscale(tmajor_yy,Minbulk,Maxbulk,nameinp)
	  xticmin=0; yticmin=Minbulk  ; xticmax=360; yticmax=Maxbulk; nlabel='Bulk modulus (GPa)'
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90
     if (val=='polarbulk' .OR. val=='Polarbulk' .OR. val=='pobulk') then
	     xticmin=-Maxbulk; yticmin=-Maxbulk; xticmax=Maxbulk; yticmax=Maxbulk;tmajor_xx=tmajor_yy*ASF*3d0 
	   endif	 	
	   	 x1=0.200000; y1=0.200000; x2=0.900000; y2=0.900000   !0.150000, 0.150000, 1.150000, 0.850000
	     char_z_x=1.1
   	  char_z_y=1.1
	  CALL set_xy_agr_1frame(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT,char_z_x,char_z_y,frame_style)


	  Nploter=0 ;colora=3  !s0
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	   Nploter=1 ;colora=2  !s1
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	  
	  Ng=0;Ns=0  !g3.s0
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_bulk.dat',err=44)
          do i=1,phi_meah+1
             READ(4,*) theta2,rho2,rho3 
            if (val=='Boxbulk' .OR. val=='boxbulk' .OR. val=='bobulk')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho3 
            if (val=='polarbulk' .OR. val=='Polarbulk' .OR. val=='pobulk') then; CALL polar2xy(theta2,rho3,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
  
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================
	 
	  Ng=3;Ns=1  !g3.s1
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_bulk.dat',err=44)
          do i=1,phi_meah+1
             READ(4,*) theta2,rho2,rho3
             if (val=='Boxbulk' .OR. val=='boxbulk' .OR. val=='bobulk')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2 
            if (val=='polarbulk' .OR. val=='Polarbulk' .OR. val=='pbulk') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif 
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================	  
	 !========================================	 		  
	  close(2); if (val=='Boxbulk' .OR. val=='boxbulk' .OR. val=='bobulk') then     
		              print*, "========================" ;print*, " => 2Dbox_bulk.agr generated" ;endif
	                 if (val=='polarbulk' .OR. val=='Polarbulk' .OR. val=='pbulk') then 
					           print*, "========================" ;print*, " => 2Dpolar_bulk.agr generated";endif
endif		 
!---------------------------------------------------------------------------
!                              END 2Dbox Bulk        
!---------------------------------------------------------------------------
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
 
!---------------------------------------------------------------------------
!                      START 2Dbox Linear Compressibiliy        
!---------------------------------------------------------------------------
if (val=='Boxcomp' .OR. val=='boxcomp' .OR. val=='bocomp' .or. &
    val=='polarcomp' .OR. val=='Polarcomp' .OR. val=='pcomp') then
if (val=='Boxcomp'   .OR. val=='boxcomp'   .OR. val=='bocomp') then; frame_style=1.0; open(2,file='2Dpolar_comp.agr'); endif
if (val=='polarcomp' .OR. val=='Polarcomp' .OR. val=='pocomp' ) then;  FT='true'; frame_style=0.0 ;open(2,file='2Dbox_comp.agr'); endif
 
  CALL start_agr(colorr)    	 
if (val=='Boxcomp' .OR. val=='boxcomp' .OR. val=='bocomp')  Xlabel='Degree'    
if (val=='Boxcomp' .OR. val=='boxcomp' .OR. val=='bocomp')  onoff ="on"  

if (val=='polarcomp' .OR. val=='Polarcomp' .OR. val=='pocomp')   Xlabel=' '
if (val=='polarcomp' .OR. val=='Polarcomp' .OR. val=='pocomp')   onoff ="off"   
  N_frame=0 !g0
	 CALL curve_frame1_agr(N_frame)
	  N_frame=0 !g0
	  CALL curve_frame1_agr(N_frame)
     	    nameinp='comp'
     	 call automakescal(tmajor_yy, Mincomp,Maxcomp, nameinp)
     	 !CALL automakescal(tmajor_yy, Maxcomp, Mincomp,nameinp)
      ! CALL autoscale(tmajor_yy,Mincomp,Maxcomp,nameinp)
	  xticmin=0; yticmin=Mincomp ; xticmax=360; yticmax=Maxcomp; nlabel='Linear compressibiliy (1/TPa)'
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90
	    if (val=='polarcomp' .OR. val=='Polarcomp' .OR. val=='pocomp') then
	     xticmin=-Maxcomp; yticmin=-Maxcomp ; xticmax=Maxcomp; yticmax=Maxcomp; tmajor_xx=tmajor_yy*ASF
	    endif
	   	  x1=0.200000; y1=0.200000; x2=0.900000; y2=0.900000   !0.150000, 0.150000, 1.150000, 0.850000
	     char_z_x=1.1
   	  char_z_y=1.1
	  CALL set_xy_agr_1frame(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT,char_z_x,char_z_y,frame_style)

	  Nploter=0 ;colora=3  !s0
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	   Nploter=1 ;colora=2  !s1
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	  
	  Ng=0;Ns=0  !g2.s0
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_comp.dat',err=44)
          do i=1,phi_meah+1
             READ(4,*) theta2,rho2,rho3
            if (val=='Boxcomp' .OR. val=='boxcomp' .OR. val=='bocomp')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2 
            if (val=='polarcomp' .OR. val=='Polarcomp' .OR. val=='pocomp') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================
	 
	  Ng=0;Ns=1  !g2.s1
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_comp.dat',err=44)
          do i=1,phi_meah+1
             READ(4,*) theta2,rho2,rho3
            if (val=='Boxcomp' .OR. val=='boxcomp' .OR. val=='bocomp')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho3 
            if (val=='polarcomp' .OR. val=='Polarcomp' .OR. val=='pocomp') then; CALL polar2xy(theta2,rho3,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================	
	  close(2); if (val=='Boxcomp' .OR. val=='boxcomp' .OR. val=='bocomp') then     
		              print*, "========================" ;print*, " => 2Dbox_comp.agr generated" ;endif
	                 if (val=='polarcomp' .OR. val=='Polarcomp' .OR. val=='pocomp') then 
					           print*, "========================" ;print*, " => 2Dpolar_comp.agr generated";endif
endif		   
!---------------------------------------------------------------------------
!                     END 2Dbox Linear Compressibiliy        
!---------------------------------------------------------------------------
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
 !---------------------------------------------------------------------------
!                            START 2Dbox groups        
!---------------------------------------------------------------------------
if (val=='Boxgall' .OR. val=='boxgall' .OR. val=='bogall'.or. &
    val=='polargall' .OR. val=='Polargall' .OR. val=='pogall') then
 if (val=='Boxgall' .OR. val=='boxgall' .OR. val=='bogall')      then;  frame_style=1.0; open(2,file='2Dbox_gall.agr'); endif
 if (val=='polargall' .OR. val=='Polargall' .OR. val=='pogall' ) then;  FT='true'; frame_style=0.0 ;open(2,file='2Dpolar_gall.agr'); endif   
 
  CALL start_agr(colorr)    	 
if (val=='Boxgall' .OR. val=='boxgall' .OR. val=='bogall')  Xlabel='Degree'    
if (val=='Boxgall' .OR. val=='boxgall' .OR. val=='bogall')  onoff ="on"  

if (val=='polargall' .OR. val=='Polargall' .OR. val=='pogall')   Xlabel=' '
if (val=='polargall' .OR. val=='Polargall' .OR. val=='pogall')   onoff ="off"  
  N_frame=0 !g0
	 CALL curve_frame1_agr(N_frame)
  nameinp='gall'
  call automakescal(tmajor_yy, Pratio_min, Pratio_max, nameinp)
 ! CALL autoscale(tmajor_yy,Pratio_min,Pratio_max,nameinp)
	  xticmin=0; yticmin=Pratio_min  ; xticmax=360; yticmax=Pratio_max; nlabel='Group velocities'
   tmajor_yy=tmajor_yy*ASF
   tmajor_xx=90
  if (val=='polargall' .OR. val=='Polargall' .OR. val=='pogall') then
	  xticmin=-Pratio_max; yticmin=-Pratio_max  ; xticmax=Pratio_max; yticmax=Pratio_max; tmajor_xx=tmajor_yy*ASF
	 endif
 	
   	  x1=0.200000; y1=0.200000; x2=0.900000; y2=0.900000  !0.150000, 0.150000, 1.150000, 0.850000
   	  char_z_x=1.1
   	  char_z_y=1.1
	  CALL set_xy_agr_1frame(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT,char_z_x,char_z_y,frame_style)
	   Nploter=0 ;colora=4;label_dat="Slow-mode"  !s0
	  CALL set_nplot_velocity_agr(Nploter,colora,label_dat)
	   Nploter=1 ;colora=3;label_dat="Fast-mode"  !s1
	  CALL set_nplot_velocity_agr(Nploter,colora,label_dat)
	   Nploter=2 ;colora=2;label_dat="P-mode"  !s1
	  CALL set_nplot_velocity_agr(Nploter,colora,label_dat)	
	  !====
  	   open(4,file='2dcut_gveloc.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3,rho4
	          if (val=='Boxgall' .OR. val=='boxgall' .OR. val=='bogall')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho4 
	          if (val=='polargall' .OR. val=='Polargall' .OR. val=='pogall') then; CALL polar2xy(theta2,rho4,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif         
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr()
	 !========================================
	 
	  Ng=0;Ns=1  !g4.s1
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_gveloc.dat',err=44)
          do i=1,361
           READ(4,*) theta2,rho2,rho3,rho4
           if (val=='Boxgall' .OR. val=='boxgall' .OR. val=='bogall')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho3 
           if (val=='polargall' .OR. val=='Polargall' .OR. val=='pogall') then; CALL polar2xy(theta2,rho3,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif         
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================
	 
	  Ng=0;Ns=2  !g4.s2
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_gveloc.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3,rho4
           if (val=='Boxgall' .OR. val=='boxgall' .OR. val=='bogall')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2 
           if (val=='polargall' .OR. val=='Polargall' .OR. val=='pogall') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif         
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================	 		  
	  close(2)
	                if (val=='Boxgall' .OR. val=='boxgall' .OR. val=='bogall') then     
		                  print*, "========================" ;print*, " => 2Dbox_gall.agr generated" ; endif
	                 if (val=='polargall' .OR. val=='Polargall' .OR. val=='pogall') then 
					           print*, "========================" ;print*, " => 2Dpolar_gall.agr generated";endif
endif	
!---------------------------------------------------------------------------
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
 !---------------------------------------------------------------------------
!                            START 2Dbox groups        
!---------------------------------------------------------------------------
if (val=='Boxpall' .OR. val=='boxpall' .OR. val=='bopall'.or. &
    val=='polarpall' .OR. val=='Polarpall' .OR. val=='popall') then
 if (val=='Boxpall' .OR. val=='boxpall' .OR. val=='bopall')      then;  frame_style=1.0; open(2,file='2Dbox_pall.agr'); endif
 if (val=='polarpall' .OR. val=='Polarpall' .OR. val=='popall' ) then;  FT='true'; frame_style=0.0 ;open(2,file='2Dpolar_pall.agr'); endif   
 
  CALL start_agr(colorr)    	 
if (val=='Boxpall' .OR. val=='boxpall' .OR. val=='bopall')  Xlabel='Degree'    
if (val=='Boxpall' .OR. val=='boxpall' .OR. val=='bopall')  onoff ="on"  

if (val=='polarpall' .OR. val=='Polarpall' .OR. val=='popall')   Xlabel=' '
if (val=='polarpall' .OR. val=='Polarpall' .OR. val=='popall')   onoff ="off"  
  N_frame=0 !g0
	 CALL curve_frame1_agr(N_frame)
  nameinp='pall'
  call automakescal(tmajor_yy, Pratio_min, Pratio_max, nameinp)
 ! CALL autoscale(tmajor_yy,Pratio_min,Pratio_max,nameinp)
	  xticmin=0; yticmin=Pratio_min  ; xticmax=360; yticmax=Pratio_max; nlabel='Phase velocities'
   tmajor_yy=tmajor_yy*ASF
   tmajor_xx=90
  if (val=='polarpall' .OR. val=='Polarpall' .OR. val=='popall') then
	  xticmin=-Pratio_max; yticmin=-Pratio_max  ; xticmax=Pratio_max; yticmax=Pratio_max; tmajor_xx=tmajor_yy*ASF
	 endif
 	
   	  x1=0.200000; y1=0.200000; x2=0.900000; y2=0.900000   !0.150000, 0.150000, 1.150000, 0.850000
   	  char_z_x=1.1
   	  char_z_y=1.1
	  CALL set_xy_agr_1frame(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT,char_z_x,char_z_y,frame_style)
	   Nploter=0 ;colora=4;label_dat="Slow-mode"  !s0
	  CALL set_nplot_velocity_agr(Nploter,colora,label_dat)
	   Nploter=1 ;colora=3;label_dat="Fast-mode"  !s1
	  CALL set_nplot_velocity_agr(Nploter,colora,label_dat)
	   Nploter=2 ;colora=2;label_dat="P-mode"  !s1
	  CALL set_nplot_velocity_agr(Nploter,colora,label_dat)	
	  !====
  	   open(4,file='2dcut_gveloc.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3,rho4
	          if (val=='Boxpall' .OR. val=='boxpall' .OR. val=='bopall')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho4 
	          if (val=='polarpall' .OR. val=='Polarpall' .OR. val=='popall') then; CALL polar2xy(theta2,rho4,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif         
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr()
	 !========================================
	 
	  Ng=0;Ns=1  !g4.s1
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_gveloc.dat',err=44)
          do i=1,361
           READ(4,*) theta2,rho2,rho3,rho4
           if (val=='Boxpall' .OR. val=='boxpall' .OR. val=='bopall')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho3 
           if (val=='polarpall' .OR. val=='Polarpall' .OR. val=='popall') then; CALL polar2xy(theta2,rho3,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif         
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================
	 
	  Ng=0;Ns=2  !g4.s2
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_gveloc.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3,rho4
           if (val=='Boxpall' .OR. val=='boxpall' .OR. val=='bopall')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2 
           if (val=='polarpall' .OR. val=='Polarpall' .OR. val=='popall') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif         
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================	 		  
	  close(2)
	                if (val=='Boxpall' .OR. val=='boxpall' .OR. val=='bopall') then     
		                  print*, "========================" ;print*, " => 2Dbox_pall.agr generated" ; endif
	                 if (val=='polarpall' .OR. val=='Polarpall' .OR. val=='popall') then 
					           print*, "========================" ;print*, " => 2Dpolar_pall.agr generated";endif
endif	
!---------------------------------------------------------------------------
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
 !---------------------------------------------------------------------------
!                            START 2Dbox power flow angle        
!---------------------------------------------------------------------------
if (val=='Boxpfall' .OR. val=='boxpfall' .OR. val=='bopfall'.or. &
    val=='polarpfall' .OR. val=='Polarpfall' .OR. val=='popfall') then
 if (val=='Boxpfall' .OR. val=='boxpfall' .OR. val=='bopfall')      then;  frame_style=1.0; open(2,file='2Dbox_pfall.agr'); endif
 if (val=='polarpfall' .OR. val=='Polarpfall' .OR. val=='popfall' ) then;  FT='true'; frame_style=0.0 ;open(2,file='2Dpolar_pfall.agr'); endif   
 
  CALL start_agr(colorr)    	 
if (val=='Boxpfall' .OR. val=='boxpfall' .OR. val=='bopfall')  Xlabel='Degree'    
if (val=='Boxpfall' .OR. val=='boxpfall' .OR. val=='bopfall')  onoff ="on"  

if (val=='polarpfall' .OR. val=='Polarpfall' .OR. val=='popfall')   Xlabel=' '
if (val=='polarpfall' .OR. val=='Polarpfall' .OR. val=='popfall')   onoff ="off"  
  N_frame=0 !g0
	 CALL curve_frame1_agr(N_frame)
  nameinp='pfall'
  call automakescal(tmajor_yy, Pratio_min, Pratio_max, nameinp)
 ! CALL autoscale(tmajor_yy,Pratio_min,Pratio_max,nameinp)
	  xticmin=0; yticmin=Pratio_min  ; xticmax=360; yticmax=Pratio_max; nlabel='Power Flow anglses'
   tmajor_yy=tmajor_yy*ASF
   tmajor_xx=90
  if (val=='polarpfall' .OR. val=='Polarpfall' .OR. val=='popfall') then
	  xticmin=-Pratio_max; yticmin=-Pratio_max  ; xticmax=Pratio_max; yticmax=Pratio_max; tmajor_xx=tmajor_yy*ASF
	 endif
 	
   	  x1=0.200000; y1=0.200000; x2=0.900000; y2=0.900000   !0.150000, 0.150000, 1.150000, 0.850000
   	  char_z_x=1.1
   	  char_z_y=1.1
	  CALL set_xy_agr_1frame(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT,char_z_x,char_z_y,frame_style)
	   Nploter=0 ;colora=4;label_dat="Slow-mode"  !s0
	  CALL set_nplot_velocity_agr(Nploter,colora,label_dat)
	   Nploter=1 ;colora=3;label_dat="Fast-mode"  !s1
	  CALL set_nplot_velocity_agr(Nploter,colora,label_dat)
	   Nploter=2 ;colora=2;label_dat="P-mode"  !s1
	  CALL set_nplot_velocity_agr(Nploter,colora,label_dat)	
	  !====
  	   open(4,file='2dcut_pfaveloc.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3,rho4
	          if (val=='Boxpfall' .OR. val=='boxpfall' .OR. val=='bopfall')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho4 
	          if (val=='polarpfall' .OR. val=='Polarpfall' .OR. val=='popfall') then; CALL polar2xy(theta2,rho4,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif         
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr()
	 !========================================
	 
	  Ng=0;Ns=1  !g4.s1
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_pfaveloc.dat',err=44)
          do i=1,361
           READ(4,*) theta2,rho2,rho3,rho4
           if (val=='Boxpfall' .OR. val=='boxpfall' .OR. val=='bopfall')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho3 
           if (val=='polarpfall' .OR. val=='Polarpfall' .OR. val=='popfall') then; CALL polar2xy(theta2,rho3,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif         
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================
	 
	  Ng=0;Ns=2  !g4.s2
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_pfaveloc.dat',err=44)
          do i=1,361
             READ(4,*) theta2,rho2,rho3,rho4
           if (val=='Boxpfall' .OR. val=='boxpfall' .OR. val=='bopfall')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2 
           if (val=='polarpfall' .OR. val=='Polarpfall' .OR. val=='popfall') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif         
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr() 
	 !========================================	 		  
	  close(2)
	                if (val=='Boxpfall' .OR. val=='boxpfall' .OR. val=='bopfall') then     
		                  print*, "========================" ;print*, " => 2Dbox_pfall.agr generated" ; endif
	                 if (val=='polarpfall' .OR. val=='Polarpfall' .OR. val=='popfall') then 
					           print*, "========================" ;print*, " => 2Dpolar_pfall.agr generated";endif
endif	
!---------------------------------------------------------------------------
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!                           END power flow angle 
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!---------------------------------------------------------------------------
!                            START 2Dbox km        
!---------------------------------------------------------------------------
if (val=='Boxkm' .OR. val=='boxkm' .OR. val=='bokm' .or. &
    val=='polarkm' .OR. val=='Polarkm' .OR. val=='pokm') then
if (val=='Boxkm'   .OR. val=='boxkm'   .OR. val=='bokm') then; frame_style=1.0; open(2,file='2Dbox_conductivity.agr'); endif
if (val=='polarkm' .OR. val=='Polarkm' .OR. val=='pokm' ) then;  FT='true'; frame_style=0.0 ;open(2,file='2Dpolar_conductivity.agr'); endif
 
  CALL start_agr(colorr)    	 
if (val=='Boxkm'   .OR. val=='boxkm'   .OR. val=='bokm')  Xlabel='Degree'    
if (val=='Boxkm'   .OR. val=='boxkm'   .OR. val=='bokm')  onoff ="on"  

if (val=='polarkm' .OR. val=='Polarkm' .OR. val=='pokm')   Xlabel=' '
if (val=='polarkm' .OR. val=='Polarkm' .OR. val=='pokm')   onoff ="off"
  N_frame=0 !g0
	 CALL curve_frame1_agr(N_frame)
	  N_frame=0 !g0
	  CALL curve_frame1_agr(N_frame)
	    nameinp='km'
    call automakescal(tmajor_yy, Minkm, Maxkm, nameinp)
       !CALL autoscale(tmajor_yy,Minkm,Maxkm,nameinp)	
	  xticmin=0; yticmin=Minkm  ; xticmax=360; yticmax=Maxkm;  nlabel='Min. th. conductivity (W/K.m)'
 
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90 
	   if (val=='polarkm' .OR. val=='Polarkm' .OR. val=='pokm') then
	     xticmin=-Maxkm; yticmin=-Maxkm  ; xticmax=Maxkm; yticmax=Maxkm;tmajor_xx=tmajor_yy*ASF
	   endif    
	   	 x1=0.200000; y1=0.200000; x2=0.900000; y2=0.900000   !0.150000, 0.150000, 1.150000, 0.850000
	     char_z_x=1.1
   	  char_z_y=1.1
	  CALL set_xy_agr_1frame(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT,char_z_x,char_z_y,frame_style)

	  Nploter=0 ;colora=3  !s0
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	  
	  Ng=0;Ns=0  !g1.s0
	  CALL Win_start_agr(Ng,Ns)
	  
 !====
  	   open(4,file='2dcut_km.dat',err=44)
          do i=1,phi_meah+1
             READ(4,*) theta2,rho2 
            if (val=='Boxkm' .OR. val=='boxkm' .OR. val=='bokm')    WRITE(2,'(F11.6,2X,F11.6)')theta2,rho2 
            if (val=='polarkm' .OR. val=='Polarkm' .OR. val=='pokm') then; CALL polar2xy(theta2,rho2,x,y); WRITE(2,'(F11.6,2X,F11.6)')x,y; endif
         enddo  
	   CLOSE(4)
!====	   
	  CALL Win_end_agr()
	  !========================================	
	  	   close(2) ; if (val=='Boxkm'   .OR. val=='boxkm'   .OR. val=='bokm') then     
		             print*, "========================" ;print*, " => 2Dbox_conductivity.agr generated" ;endif
	                 if (val=='polarkm' .OR. val=='Polarkm' .OR. val=='pkm') then 
					           print*, "========================" ;print*, " => 2Dpolar_conductivity.agr generated";endif
	endif 
!---------------------------------------------------------------------------
!                           END 2Dbox km        
!---------------------------------------------------------------------------
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
if (Npro==1)then
   if (val=='Box' .OR. val=='box' .OR. val=='bo')     then; open(2,file='2Dbox.agr') 	;endif
   if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; open(2,file='2Dpolar.agr'); FT='true' 	;endif   
!----------------------------------------------------------------------------	 
	     CALL start_agr(colorr) 
!===============================================================================sss
if (val=='Box' .OR. val=='box' .OR. val=='bo') then; Xlabel=' '; endif
if (val=='Polar' .OR. val=='polar' .OR. val=='po') then; onoff="on"; Xlabel=' '; endif
if (val=='Box' .OR. val=='box' .OR. val=='bo') then;onoff="off"; endif
!=========================================================================== Start Shear
	  N_frame=0 !g0
	  CALL curve_frame1_agr(N_frame)
	    nameinp='shear'
	    call automakescal(tmajor_yy, G_min2, G_max2, nameinp)
     !  CALL autoscale(tmajor_yy,G_min2,G_max2,nameinp)	     
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
	  CALL set_nplot_agr(Nploter,colora,label_dat)
      Nploter=1; colora=4  !s1
	  CALL set_nplot_agr(Nploter,colora,label_dat)
      Nploter=2; colora=3  !s2
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	  
	  Ng=0;Ns=0  !g0.s0
	  CALL Win_start_agr(Ng,Ns)
!====box
  	   open(4,file='2dcut_shear.dat',err=44)
          do i=1,phi_meah+1
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
          do i=1,phi_meah+1
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
          do i=1,phi_meah+1
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
	    call automakescal(tmajor_yy, Minyoung, Maxyoung, nameinp)
       !CALL autoscale(tmajor_yy,Minyoung,Maxyoung,nameinp)	
	  xticmin=0; yticmin=Minyoung  ; xticmax=360; yticmax=Maxyoung; nlabel='Young Modulus (GPa)'
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90
      if (val=='Polar' .OR. val=='polar' .OR. val=='po') then
	     xticmin=-Maxyoung; yticmin=-Maxyoung  ; xticmax=Maxyoung; yticmax=Maxyoung;tmajor_xx=tmajor_yy*ASF
	  endif    
	  x1=0.500865; y1=0.554545; x2=0.793253; y2=0.850000  !0.500865, 0.554545, 0.793253, 0.850000
	  CALL set_xy_agr(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT)

	  Nploter=0 ;colora=3  !s0
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	  
	  Ng=1;Ns=0  !g1.s0
	  CALL Win_start_agr(Ng,Ns)
	  
 !====
  	   open(4,file='2dcut_young.dat',err=44)
          do i=1,phi_meah+1
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
       call automakescal(tmajor_yy, Mincomp, Maxcomp, nameinp)
      ! CALL autoscale(tmajor_yy,Mincomp,Maxcomp,nameinp)
	  xticmin=0; yticmin=Mincomp ; xticmax=360; yticmax=Maxcomp; nlabel='Linear Compressibiliy (1/TPa)'
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90
      if (val=='Polar' .OR. val=='polar' .OR. val=='po') then
	     xticmin=-Maxcomp; yticmin=-Maxcomp ; xticmax=Maxcomp; yticmax=Maxcomp; tmajor_xx=tmajor_yy*ASF
	  endif
	  x1=0.851730; y1=0.554545; x2=1.144118; y2=0.850000 !0.851730, 0.554545, 1.144118, 0.850000
	  CALL set_xy_agr(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT)

	  Nploter=0 ;colora=3  !s0
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	   Nploter=1 ;colora=2  !s1
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	  
	  Ng=2;Ns=0  !g2.s0
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_comp.dat',err=44)
          do i=1,phi_meah+1
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
          do i=1,phi_meah+1
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
      ! CALL autoscale(tmajor_yy,Minbulk,Maxbulk,nameinp)
      call automakescal(tmajor_yy, Minbulk, Maxbulk, nameinp)
	  xticmin=0; yticmin=Minbulk  ; xticmax=360; yticmax=Maxbulk; nlabel='Bulk Modulus (GPa)'
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90
     if (val=='Polar' .OR. val=='polar' .OR. val=='po') then
	   xticmin=-Maxbulk; yticmin=-Maxbulk; xticmax=Maxbulk; yticmax=Maxbulk;tmajor_xx=tmajor_yy*ASF*3d0 
	 endif	
     x1=0.150000; y1=0.200000; x2=0.442388; y2=0.495455  !0.150000, 0.200000, 0.442388, 0.468182
	  CALL set_xy_agr(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF*3d0 ,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT)

	  Nploter=0 ;colora=3  !s0
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	   Nploter=1 ;colora=2  !s1
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	  
	  Ng=3;Ns=0  !g3.s0
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_bulk.dat',err=44)
          do i=1,phi_meah+1
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
          do i=1,phi_meah+1
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
       !CALL autoscale(tmajor_yy,Pratio_min,Pratio_max,nameinp)
       call automakescal(tmajor_yy, Pratio_min, Pratio_max, nameinp)
	  xticmin=0; yticmin=Pratio_min  ; xticmax=360; yticmax=Pratio_max; nlabel='Poissons Ratio'
	  	   tmajor_yy=tmajor_yy*ASF
	   tmajor_xx=90
     if (val=='Polar' .OR. val=='polar' .OR. val=='po') then
	 xticmin=-Pratio_max; yticmin=-Pratio_max  ; xticmax=Pratio_max; yticmax=Pratio_max; tmajor_xx=tmajor_yy*ASF
	 endif
	  x1=0.500865; y1=0.200000; x2=0.793253; y2=0.495455  !0.500865, 0.200000, 0.793253, 0.468182
	  CALL set_xy_agr(xticmin,yticmin,xticmax,yticmax,nlabel,tmajor_yy*ASF,tmajor_xx,x1,y1,x2,y2,onoff,Xlabel,FT)
	   Nploter=0 ;colora=2  !s0
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	   Nploter=1 ;colora=3  !s1
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	   Nploter=2 ;colora=4  !s1
	  CALL set_nplot_agr(Nploter,colora,label_dat)	  
	  Ng=4;Ns=0  !g4.s0
	  CALL Win_start_agr(Ng,Ns)
!====
  	   open(4,file='2dcut_poisson.dat',err=44)
          do i=1,phi_meah+1
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
          do i=1,phi_meah+1
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
          do i=1,phi_meah+1
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
	   !CALL autoscale(tmajor_yy,pugh_min2,pugh_max2,nameinp)
	   call automakescal(tmajor_yy, pugh_min2, pugh_max2, nameinp)
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
	  CALL set_nplot_agr(Nploter,colora,label_dat)
      Nploter=1; colora=4  !s1
	  CALL set_nplot_agr(Nploter,colora,label_dat)
      Nploter=2; colora=3  !s2
	  CALL set_nplot_agr(Nploter,colora,label_dat)
	  
	  Ng=5;Ns=0  !g0.s0
	  CALL Win_start_agr(Ng,Ns)
!====box
  	   open(4,file='2dcut_pugh.dat',err=44)
          do i=1,phi_meah+1
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
          do i=1,phi_meah+1
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
          do i=1,phi_meah+1
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
endif
	 44  stop 
  END PROGRAM
  
  
