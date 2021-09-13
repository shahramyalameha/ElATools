!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, calculate main WAVE properties. 

subroutine pro_wave(density, ma_avrag)
IMPLICIT NONE
 DOUBLE PRECISION                  :: density,ma_avrag,vm,vt,vl, av,&
                                                                 bv,&
                                                                 cv,&
                                                                 ar,&
                                                                 br,&
                                                                 cr,&
                                                                 kv,&
                                                                 kr,&
                                                                 kh,&
                                                                 gv,&
                                                                 gr,&
                                                                 gh,&
                                                                 Ev,&
                                                                 Er,&
                                                                 Eh,kbb,tot_at,Clarke_km,Cahill_km,E1,Ma1,kb,n
 DOUBLE PRECISION, DIMENSION(6,6)  :: C=0D0,S=0D0				 
 INTEGER                           :: i,j
 
    OPEN(221,file=".mavg_com")
    READ(221,*) ma_avrag
    READ(221,*) tot_at
    CLOSE(221)
    
    OPEN(44,FILE="Cij.dat",status='old')                             ! read cij data inpout
    READ(44,*) C(1,1),C(1,2),C(1,3),C(1,4),C(1,5),C(1,6)
    READ(44,*) C(2,1),C(2,2),C(2,3),C(2,4),C(2,5),C(2,6)
    READ(44,*) C(3,1),C(3,2),C(3,3),C(3,4),C(3,5),C(3,6)
    READ(44,*) C(4,1),C(4,2),C(4,3),C(4,4),C(4,5),C(4,6)
    READ(44,*) C(5,1),C(5,2),C(5,3),C(5,4),C(5,5),C(5,6)
    READ(44,*) C(6,1),C(6,2),C(6,3),C(6,4),C(6,5),C(6,6)
    CLOSE(44)

    OPEN(10,FILE="Sij.dat",status='old')                             ! read cij data inpout
    READ(10,*) S(1,1),S(1,2),S(1,3),S(1,4),S(1,5),S(1,6)
    READ(10,*) S(2,1),S(2,2),S(2,3),S(2,4),S(2,5),S(2,6)
    READ(10,*) S(3,1),S(3,2),S(3,3),S(3,4),S(3,5),S(3,6)
    READ(10,*) S(4,1),S(4,2),S(4,3),S(4,4),S(4,5),S(4,6)
    READ(10,*) S(5,1),S(5,2),S(5,3),S(5,4),S(5,5),S(5,6)
    READ(10,*) S(6,1),S(6,2),S(6,3),S(6,4),S(6,5),S(6,6)
    CLOSE(10)
    WRITE(*,*)'#########################################################################'	
    WRITE(99,*)'#########################################################################'
     av=(C(1,1)+C(2,2)+C(3,3))/3d0
    bv=(C(1,2)+C(2,3)+C(1,3))/3d0
    cv=(C(4,4)+C(5,5)+C(6,6))/3d0
    ar=(S(1,1)+S(2,2)+S(3,3))/3d0
    br=(S(1,2)+S(2,3)+S(1,3))/3d0
    cr=(S(4,4)+S(5,5)+S(6,6))/3d0
!
    kv = (av+2d0*bv)/3d0
    kr = 1d0/(3d0*ar+6d0*br)
    kh = 0.5d0*(kv+kr) 
    gv = (av-bv+3d0*cv)/5d0
    gr = 5d0/(4d0*ar-4d0*br+3d0*cr)
    gh = 0.5d0*(gv+gr)
    Ev = 1d0/(1d0/(3d0*gv)+1d0/(9d0*kv))
    Er = 1d0/(1d0/(3d0*gr)+1d0/(9d0*kr))
    Eh = 0.5d0*(Ev+Er)  
    
    vl = ( (gh*1e9)/density )**(0.5)
    vt = ( (3.0d0*kh*1e9 + 4.0d0*gh*1e9) / (3.0d0*density) ) **(0.5d0)
    vm = ( (1.0D0/3.0D0)*( (2.0/vt**3.0) + (1.0/vl**3.0) ) ) **(-1.0/3.0)
    Ma1=ma_avrag*0.001D0
    E1=Eh*1E9
    kbb=5.56713306451613e-24
    kb=1.380649e-23	
    n=(density*6.022e23)/(tot_at*Ma1)
    Clarke_km = 0.87D0*(kb)*(Ma1**(-2.0/3.0))*E1**(0.5D0)*density**(1.0/6.0)
    Cahill_km = (vl + 2.0d0* vt )*(n**(2.0/3.0)) *kbb
   write(*,*)
    
    WRITE(*,*)'========================================================================='
    WRITE(*,*)'                 Elastic Wave properties                     '
    WRITE(*,*)'========================================================================='   
    WRITE(*,'(a,1F10.3,a)')' = Longitudinal sound velocity (m/s)                   |    ',vl,'  ='
    WRITE(*,'(a,1F10.3,a)')' = Transverse   sound velocity (m/s)                   |    ',vt,'  ='
    WRITE(*,'(a,1F10.3,a)')' = Average      sound velocity (m/s)                   |    ',vm,'  ='  
    WRITE(*,'(a,1F10.3,a)')' = Min. thermal conductivity (Clarke model) (W/m.K)  |    ',Clarke_km,'  ='
    !WRITE(*,'(a,1F10.3,a)')' = Min. thermal conductivity (Cahill model) (W/m.K)  |    ',Cahill_km,'  ='
    WRITE(*,*)'========================================================================='  
    WRITE(99,*)''
    WRITE(99,*)'========================================================================='
    WRITE(99,*)'                 Elastic Wave properties                     '
    WRITE(99,*)'========================================================================='   
    WRITE(99,'(a,1F10.3,a)')' = Longitudinal sound velocity (m/s)                   |    ',vl,'  ='
    WRITE(99,'(a,1F10.3,a)')' = Transverse   sound velocity (m/s)                   |    ',vt,'  ='
    WRITE(99,'(a,1F10.3,a)')' = Average      sound velocity (m/s)                   |    ',vm,'  ='  
    WRITE(99,'(a,1F10.3,a)')' = Min. thermal conductivity (Clarke model) (W/m.K)  |    ',Clarke_km,'  ='
    !WRITE(99,'(a,1F10.3,a)')' = Min. thermal conductivity (Cahill model) (W/m.K)  |    ',Cahill_km,'  ='
    WRITE(99,*)'=========================================================================' 
    WRITE(99,*)''     
end subroutine    
    
    
