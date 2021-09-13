
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; calculatet  Minimum thermal conductivity.

subroutine Ckm_cal(dens, E, Ma,  km_Clarke )
                !  in  in  in    out-in 
 DOUBLE PRECISION                   :: E,E1,km_Clarke,Ma,Ma1,kb,dens
 
 Ma1=Ma*0.001D0
 E1=E*1e9
 kb=1.380649E-23
 km_Clarke = 0.87D0*(kb)*(Ma1**(-2.0/3.0))*E1**(0.5D0)*dens**(1.0/6.0)
 ! write(*,*)dens,Ma1,E1
 !!!! Input
 !km_Clarke =[W/m.K]=[Kg.m^2/m.K.s^3]
 ! Ma       = [Kg]
 ! dens     = [Kg/m^3]
 ! E        = [Kg/m.s^2]
 ! kb       = [m^2.Kg/K.s^2]
 
 !!!! Output
 ! Ma       = [gr]
 ! dens     = [Kg/m^3]
 ! E        = [Gpa]
 
end subroutine
