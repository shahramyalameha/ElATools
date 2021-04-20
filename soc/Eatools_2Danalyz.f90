!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 2D MATERIAL , CALCULATE Youngs Modulus, Shear Modulus AND Poissons Ratio
SUBROUTINE analiz_2D_sys()
implicit none

 DOUBLE PRECISION, PARAMETER     :: pi=3.141592653589793238462D0
 DOUBLE PRECISION                :: phi=0.0D0,&
                                   vv11=0.d0,&
                                   vv22=0d0,&
                                   vv33=0.0d0,&
                                   NPratio_max=0D0,&
                                   NPratio_min=0.0D0,&
                                   Pratio_max=0d0,&
                                   Pratio_max_phi=0d0,&
                                   Pratio_min_phi=0d0,&
                                   Pratio_min=1d0,&
                                   MaxPratio,MinPratio,&
                                   Maximum,Minimum,MaxShear,MinShear,&
                                   Shear_max_phi
                                   
 DOUBLE PRECISION, DIMENSION(6000)  :: phi_young,phi_poisson=0.d0, poisson2dmax, poisson2dminn, poisson2dminp,phii,phi_shear,&
                                       shear2dmax, shear2dmin
 DOUBLE PRECISION, DIMENSION(3)     :: vec=0d0
 DOUBLE PRECISION, DIMENSION(3,3)   :: c
 INTEGER                            :: Nmesh_phi,j, Nmesh_phiF  ,num,i
 !========================================================
   
 Nmesh_phi    = 200 ! mesh
 Nmesh_phiF   = 100
 IF (MOD(Nmesh_phi,Nmesh_phIF).NE.0) THEN
   Nmesh_phi=(Nmesh_phi/Nmesh_phiF)*Nmesh_phiF
ENDIF
OPEN(61,FILE="young_2d_sys.dat")
OPEN(62,FILE="poisson_2d_sys.dat")
OPEN(63,FILE="shear_2d_sys.dat")
DO j=0, Nmesh_phi
   phi   = DBLE(j)/DBLE(Nmesh_phi)*2D0*PI
   vec(1) = SIN(phi)
   vec(2) = COS(phi)
   vv11   = SIN(phi)**4.0d0
   vv22   = COS(phi)**4.0d0
   vv33   =(COS(phi)**2.0d0)*(SIN(phi)**2.0d0)
   CALL yound_2D(vv11,vv22,vv33,phi,phi_young,j )
   WRITE(61,"(F8.4,2F21.16)") phi*(180D0/PI), phi_young(j) 
   NPratio_max = 0D0
   NPratio_min = 3.5D0
   CALL poisson_2D(vv11,vv22,vv33,phi,phi_poisson,NPratio_max,NPratio_min,j)           
   poisson2dmax(j)  = NPratio_max
   IF (NPratio_min.GE.0D0)  poisson2dminp(j) = NPratio_min  
   IF (NPratio_min.LE.0D0)  poisson2dminn(j) = NPratio_min 
   WRITE(62,"(F8.4,3F20.16)")phi*(180d0/PI), poisson2dmax(j) ,poisson2dminp(j), poisson2dminn(j)
   CALL shear_2D(vv11,vv22,vv33,phi,phi_shear,MaxShear,j)
   shear2dmax(j) = MaxShear
   WRITE(63,"(F8.4,2F20.14)") phi*(180d0/PI), shear2dmax(j)

END DO
        
CLOSE(61)
CLOSE(62)
CLOSE(63)
CALL SYSTEM('sleep 0.5')
WRITE(*,*)''
OPEN(32,file='.MaMiout')  
WRITE (99,*) "==================================================> Youngs Modulus"
CALL SYSTEM('tput setaf 32;tput bold; echo " ==================================================> Youngs Modulus"')
CALL  MAX_Min_val(2,Maximum,Minimum,2)
WRITE(32,*)Maximum
CALL SYSTEM('tput setaf 32;tput bold; echo " ==================================================";tput sgr0')
WRITE (99,*) "==================================================<"
CALL SYSTEM('sleep 0.5')
WRITE(*,*)''
WRITE (99,*) "==================================================> Shear Modulus"
CALL SYSTEM('tput setaf 23;tput bold; echo " ==================================================> Shear Modulus"')
CALL  MAX_Min_val(3,Maximum,Minimum,2)
WRITE(32,*)Maximum
CALL SYSTEM('tput setaf 23;tput bold; echo " ==================================================> ";tput sgr0')
WRITE (99,*) "==================================================<"
CALL SYSTEM('sleep 0.5')
WRITE(*,*)''
WRITE (99,*) "==================================================> Poissons Ratio"
CALL SYSTEM('tput setaf 6;tput bold; echo " ==================================================> Poissons Ratio"')
CALL  MAX_Min_val(1,Maximum,Minimum,2)
WRITE(32,*)Maximum
CALL SYSTEM('tput setaf 6;tput bold; echo " ==================================================> ";tput sgr0')
WRITE (99,*) "==================================================<"
CLOSE(32)
END SUBROUTINE 

