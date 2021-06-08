!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the Poisson's ratio calculation..

SUBROUTINE CPratio(a1111,MinPratio,MaxPratio,AvePratio,&
                       vec1min,vec2min,vec3min,vec1max,vec2max,vec3max,&
		               theta,phi,k11,k12,k13,k22,k23,k33,Pratio)

	  DOUBLE PRECISION, PARAMETER       ::  pi=3.1415926535897932384626433832795d0
	  DOUBLE PRECISION, DIMENSION(6,6)  ::  S=0D0
	  DOUBLE PRECISION, DIMENSION(3)    ::  k=0D0,v=0D0
	  DOUBLE PRECISION                  :: theta,&
		                               phi,      &
		                               k11,      &
		                               k12,      &
		                               k13,      &
		                               k22,      &
		                               k23,      &
		                               k33,      &
		                               a1111,    &
		                               a1122,    &
		                               Pratio,   &
		                               MinPratio,&
		                               MaxPratio,&
		                               AvePratio,&
		                               gamma,    &
		                               v11,      &
		                               v12,      &
		                               v13,      &
		                               v22,      &
		                               v23,      &
		                               v33,      &
		                               ave,      &
		                               vec1min,  &
		                               vec2min,  &
		                               vec3min,  &
		                               vec1max,  &
		                               vec2max,  &
		                               vec3max

	INTEGER                             :: kk=0D0,Nmesh_gamma=0D0

	OPEN(12,FILE="Sij.dat")
	READ(12,*) S(1,1),S(1,2),S(1,3),S(1,4),S(1,5),S(1,6)
	READ(12,*) S(2,1),S(2,2),S(2,3),S(2,4),S(2,5),S(2,6)
	READ(12,*) S(3,1),S(3,2),S(3,3),S(3,4),S(3,5),S(3,6)
	READ(12,*) S(4,1),S(4,2),S(4,3),S(4,4),S(4,5),S(4,6)
	READ(12,*) S(5,1),S(5,2),S(5,3),S(5,4),S(5,5),S(5,6)
	READ(12,*) S(6,1),S(6,2),S(6,3),S(6,4),S(6,5),S(6,6)
	CLOSE(12)

	ave=0D0
	Nmesh_gamma=181
	DO kk=0,Nmesh_gamma-1      
		gamma= DBLE(kk)/DBLE(Nmesh_gamma)*pi
	    v(1) = COS(theta)*COS(phi)*COS(gamma)-SIN(phi)*SIN(gamma)
	    v(2) = COS(theta)*SIN(phi)*COS(gamma)+COS(phi)*SIN(gamma)
		v(3) =-SIN(theta)*COS(gamma)
		v11  = v(1)*v(1); v12  = v(1)*v(2)
		v13  = v(1)*v(3); v22  = v(2)*v(2)
		v23  = v(2)*v(3); v33  = v(3)*v(3) 


		a1122 =   k11*v11*S(1,1)          &
                +(k11*v22+k22*v11)*S(1,2) &
                +(k11*v33+k33*v11)*S(1,3) &
                + k22*v22*S(2,2)          &
                +(k22*v33+k33*v22)*S(2,3) &
                + k33*v33*S(3,3)  
	   

    	a1122  =a1122                                     &
                 +(k11*v23+k23*v11)*S(1,4)                &
                 +(k11*v13+k13*v11)*S(1,5)                &
                 +(k11*v12+k12*v11)*S(1,6)                &
		         +(k22*v23+k23*v22)*S(2,4)                &
                 +(k22*v13+k13*v22)*S(2,5)                &
                 +(k22*v12+k12*v22)*S(2,6)                &
		         +(k33*v23+k23*v33)*S(3,4)                &
                 +(k33*v13+k13*v33)*S(3,5)                &
                 +(k33*v12+k12*v33)*S(3,6)+k23*v23*S(4,4) &
                 +(k23*v13+k13*v23)*S(4,5)                &
                 +(k23*v12+k12*v23)*S(4,6)+k13*v13*S(5,5) &
				 +(k13*v12+k12*v13)*S(5,6)+k12*v12*S(6,6) 
		Pratio = -a1122/a1111
	IF (kk.EQ.0) THEN 
			MaxPratio=Pratio; MinPratio=Pratio;
			vec1max = v(1);  vec2max = v(2); vec3max = v(3) 
			vec1min = v(1);  vec2min = v(2); vec3min = v(3)      
    ELSE
		IF (Pratio.GE.MaxPratio) THEN
			MaxPratio=Pratio
	       vec1max = v(1); vec2max = v(2); vec3max = v(3)
	    ENDIF  
		IF (Pratio.LE.MinPratio) THEN
			MinPratio=Pratio
	        vec1min = v(1); vec2min = v(2); vec3min = v(3)      
		ENDIF  
	ENDIF
	ave = Pratio+ave
ENDDO
AvePratio=ave/Nmesh_gamma
END 
