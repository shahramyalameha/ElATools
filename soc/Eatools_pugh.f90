!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the Pugh ratio calculation.. 

SUBROUTINE Cpugh(Minpughvar,Maxpughvar,G_min,G_max,BINver)
        
	  DOUBLE PRECISION, PARAMETER                    ::  pi=3.1415926535897932384626433832795d0
	  DOUBLE PRECISION, DIMENSION(6,6)               ::  C=0D0,S=0D0,CP=0D0
	  DOUBLE PRECISION, DIMENSION(3)                 ::  k=0D0,v=0D0
	  DOUBLE PRECISION                               ::  G_min,G_max,a6666, sheainvar,Minpughvar,Maxpughvar,pughvar_ave,gamma,BINver,BB,&
                                                         v11,v12,v13,v22,v23,v33,ave,theta, phi,k11,k12,k13,k22,k23,k33,Minsheainvar,Maxsheainvar,sheainvar_ave
	  INTEGER                                        ::  Nmesh_gamma=0,kk=0



Maxpughvar   =  BINver*(G_min**-1)
Minpughvar  =  BINver*(G_max**-1)
 


	END




