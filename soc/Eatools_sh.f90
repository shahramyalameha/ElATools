!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the Shear calculation..

SUBROUTINE CShear(Minsheainvar,Maxsheainvar,sheainvar_ave,phi,theta,k11,k12,k13,k22,k23,k33,a6666,sheainvar)
        
	  DOUBLE PRECISION, PARAMETER                    ::  pi=3.1415926535897932384626433832795d0
	  DOUBLE PRECISION, DIMENSION(6,6)               ::  C=0D0,S=0D0,CP=0D0
	  DOUBLE PRECISION, DIMENSION(3)                 ::  k=0D0,v=0D0
	  DOUBLE PRECISION                               ::  a6666, sheainvar,Minsheainvar,Maxsheainvar,sheainvar_ave,gamma,&
                                                         v11,v12,v13,v22,v23,v33,ave,theta, phi,k11,k12,k13,k22,k23,k33
	  INTEGER                                        ::  Nmesh_gamma=0,kk=0

	OPEN(12,FILE="Sij.dat")
	READ(12,*) S(1,1),S(1,2),S(1,3),S(1,4),S(1,5),S(1,6)
	READ(12,*) S(2,1),S(2,2),S(2,3),S(2,4),S(2,5),S(2,6)
	READ(12,*) S(3,1),S(3,2),S(3,3),S(3,4),S(3,5),S(3,6)
	READ(12,*) S(4,1),S(4,2),S(4,3),S(4,4),S(4,5),S(4,6)
	READ(12,*) S(5,1),S(5,2),S(5,3),S(5,4),S(5,5),S(5,6)
	READ(12,*) S(6,1),S(6,2),S(6,3),S(6,4),S(6,5),S(6,6)
	 CLOSE(12) 
	!WRITE(*,*)S(6,1),S(6,2),S(6,3),S(6,4),S(6,5),S(6,6)
	ave=0D0
	Nmesh_gamma=180D0
	DO kk=0,Nmesh_gamma-1      
		gamma=DBLE(kk)/DBLE(Nmesh_gamma)*PI
	    v(1) =  COS(theta)*COS(phi)*COS(gamma)-SIN(phi)*SIN(gamma)
	    v(2) =  COS(theta)*SIN(phi)*COS(gamma)+COS(phi)*SIN(gamma)
	    v(3) = -SIN(theta)*COS(gamma)

	    v11 = v(1)*v(1); v12 = v(1)*v(2)
	    v13 = v(1)*v(3); v22 = v(2)*v(2)
	    v23 = v(2)*v(3); v33 = v(3)*v(3)

	    a6666 =   ( 2*k12*v12*S(1,2)+k11*v11*S(1,1)&
                 +2*k13*v13*S(1,3)+k22*v22*S(2,2)&
                 +2*k23*v23*S(2,3)+k33*v33*S(3,3) )
  
	    a6666=    (a6666+(k12*v13+k13*v12)*S(1,4)                        &
                      +(k11*v13+k13*v11)*S(1,5)                        &
                      +(k11*v12+k12*v11)*S(1,6)                        &
                      +(k22*v23+k23*v22)*S(2,4)                        &
                      +(k12*v23+k23*v12)*S(2,5)                        &
                      +(k22*v12+k12*v22)*S(2,6)                        &
                      +(k33*v23+k23*v33)*S(3,4)                        &
                      +(k33*v13+k13*v33)*S(3,5)                        &
                      +(k13*v23+k23*v13)*S(3,6)                        &
                      +1D0/4D0*(k22*v33+2D0*k23*v23+k33*v22)*S(4,4)    &
                      +1D0/2D0*(k12*v33+k23*v13+k13*v23+k33*v12)*S(4,5)&
                      +1D0/2D0*(k12*v23+k22*v13+k13*v22+k23*v12)*S(4,6)& 
                      +1D0/4D0*(k11*v33+2D0*k13*v13+k33*v11)*S(5,5)    &
                      +1D0/2D0*(k11*v23+k12*v13+k13*v12+k23*v11)*S(5,6)& 
                      +1D0/4D0*(k11*v22+2D0*k12*v12+k22*v11)*S(6,6)  )*10000D0

	   sheainvar = 10D0/((4D0*a6666))
	   !WRITE(*,*) sheainvar
	   IF (kk.EQ.0.0) THEN 
		Maxsheainvar=sheainvar; Minsheainvar=sheainvar
	ELSE
		IF (sheainvar.GE.Maxsheainvar)THEN
			Maxsheainvar = sheainvar
			!Maxsheainvar=Maxsheainvar*100D0
		ENDIF
		IF (sheainvar.LE.Minsheainvar)THEN
			Minsheainvar = (sheainvar)
		ENDIF
	ENDIF    
	ave=ave+sheainvar   
ENDDO

sheainvar_ave = ave/Nmesh_gamma
END
