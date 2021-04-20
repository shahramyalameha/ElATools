!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; autoscale for creat agr file.

SUBROUTINE autoscale(ac,min0,max0,nameinp)
  REAL(8) :: min0,max0, Maxyoung,Minyoung,Maxcomp,Mincomp,G_max2,G_min2,pugh_max2,pugh_min2,Maxbulk,Minbulk,&
             Pratio_max,Pratio_min,maxEVaTMf,maxEVaLM,minEVaTMf,Minimum, Maximum
  REAL(8) :: ac
  ChARACTER(30) ::  nameinp
  
  CALL MinMax(Minimum,Maximum,nameinp)
   
  if (Minimum==Maximum) then
    if (     Maximum > 0.d0 .and. abs(Maximum) < 1)        then
	ac=0.25d0; max0=Maximum+ac; min0=Maximum-ac                 ; endif
    if (abs(Maximum) >= 1    .and. abs(Maximum) < 10 )     then
	ac=NINT(abs(Maximum)/4d0)+NINT(2d0+(Maximum/4d0));  max0=NINT(Maximum)+NINT(Maximum)/10d0; min0=NINT(Maximum)-NINT(Maximum)/10d0 ; endif
	
    if (abs(Maximum) >= 10   .and. abs(Maximum) < 100 )    then
	ac=NINT(abs(Maximum)/4d0)+NINT(2d0+(Maximum/4d0));  max0=NINT(Maximum)+NINT(Maximum)/10d0; min0=NINT(Maximum)-NINT(Maximum)/10d0 ; endif
	
    if (abs(Maximum) >= 100  .and. abs(Maximum) < 1000 )   then
	ac=NINT(abs(Maximum)/4d0)+NINT(2d0+(Maximum/4d0));  max0=NINT(Maximum)+NINT(Maximum)/10d0; min0=NINT(Maximum)-NINT(Maximum)/10d0 ; endif
	
    if (abs(Maximum) >= 1000 .and. abs(Maximum) < 10000 )  then
	ac=NINT(abs(Maximum)/4d0)+NINT(2d0+(Maximum/4d0));  max0=NINT(Maximum)+NINT(Maximum)/10d0; min0=NINT(Maximum)-NINT(Maximum)/10d0 ; endif
	
    if (abs(Maximum) >= 100000  )                          then
	ac=NINT(abs(Maximum)/4d0)+NINT(2d0+(Maximum/4d0)); max0=NINT(Maximum)+NINT(Maximum)/10d0; min0=NINT(Maximum)-NINT(Maximum)/10d0 ; endif
 ENDIF
   if (Minimum/=Maximum) then 
     if (abs(Maximum-Minimum) < 1    .and. Maximum-Minimum > 0.0)          then
	 ac=0.2d0/2d0+NINT(0.5d0+(Maximum/6d0)); max0=(Maximum)+NINT(ac/20d0) ; min0=(Minimum)-NINT(ac/20d0)                  ; endif
	 
     if (abs(Maximum-Minimum) < 1    .and. Maximum-Minimum < 0.0)          then
	 ac=0.2d0/2d0+NINT(0.5d0+(Maximum/6d0)); max0=(Minimum)-NINT(ac/20d0) ; min0=(Maximum)+NINT(ac/20d0)                  ; endif
	 
     if (abs(Maximum-Minimum) >= 1   .and. abs(Maximum-Minimum) < 10 )     then
	 ac=NINT(abs(Maximum-Minimum)/3d0)+NINT(2d0+(Maximum/4d0)); max0=NINT(Maximum)+NINT(ac/10d0); min0=NINT(Minimum)-NINT(ac/10d0)  ; endif
	 
     if (abs(Maximum-Minimum) >= 10   .and. abs(Maximum-Minimum) < 100 )   then
	 ac=NINT(abs(Maximum-Minimum)/3d0)+NINT(2d0+(Maximum/4d0)); max0=NINT(Maximum)+NINT(ac/10d0) ; min0=NINT(Minimum)-NINT(ac/10d0)  ; endif
	 
     if (abs(Maximum-Minimum) >= 100  .and. abs(Maximum-Minimum) < 1000 )  then
	 ac=NINT(abs(Maximum-Minimum)/3d0)+NINT(2d0+(Maximum/4d0)); max0=NINT(Maximum)+NINT(ac/10d0) ; min0=NINT(Minimum)-NINT(ac/10d0)  ; endif
	 
     if (abs(Maximum-Minimum) >= 1000 .and. abs(Maximum-Minimum) < 10000 ) then
	 ac=NINT(abs(Maximum-Minimum)/3d0)+NINT(2d0+(Maximum/4d0)); max0=NINT(Maximum)+NINT(ac/10d0) ; min0=NINT(Minimum)-NINT(ac/10d0)  ; endif
	 
     if (abs(Maximum-Minimum) >= 100000  )                                 then
	 ac=NINT(abs(Maximum-Minimum)/3d0)+NINT(2d0+(Maximum/4d0)); max0=NINT(Maximum)+NINT(ac/10d0) ; min0=NINT(Minimum)-NINT(ac/10d0)  ; endif
  endif
 WRITE(*,'(3a)') '  yautoscale: ','minscale: ','maxscale:'
  WRITE(*,'(5F11.5)') ac,min0,max0
  WRITE(*,*)"~~~~~~~~~~~~~~~~~~~~~~"
  CALL system('sleep 0.5')
END SUBROUTINE autoscale


