!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, calculate main properties. 
SUBROUTINE proelast()
 IMPLICIT NONE
 DOUBLE PRECISION :: av,&
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
                     Eh,&
                     nuv,&
                     nur,&
                     nuh,&
					                Kgv,&
                     Kgr,&
                     kgh,&
					                mv,&
					                mr,&
					                mh,&
					                Au,&
					                Al,&
					                Pc,&
					                Ac
 CHARACTER(LEN=10) :: bdout1,bdout2
 CHARACTER(LEN=23) :: Cov_met				 
 DOUBLE PRECISION, DIMENSION(6,6) ::C=0D0,S=0D0				 
 INTEGER::i,j
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

    WRITE(*,*)''
    WRITE(*,*)'Cij:'
    WRITE(99,*)'Cij:'
    DO i=1,6
      WRITE(*,'(2X,6( F12.6))') (C(i,j), j=1,6)
      WRITE(99,'(2X,6(1X,F12.6))') (C(i,j), j=1,6)
    END do
    call sleep(1)
    WRITE(*,*)''
    WRITE(99,*)''
    WRITE(*,*)'Sij:'
    WRITE(99,*)'Sij:'

    DO i=1,6
      WRITE(*,'(2X,7( F12.7))') (S(i,j), j=1,6)
      WRITE(99,'(2X,7(1X,F12.7))') (S(i,j), j=1,6)
    END do
    WRITE(*,*)''
    WRITE(*,*)'#########################################################################' 
    WRITE(*,*)''
    WRITE(99,*)''
    WRITE(99,*)'#########################################################################' 
    WRITE(99,*)''
    call sleep(1)
    av=(C(1,1)+C(2,2)+C(3,3))/3d0
    bv=(C(1,2)+C(2,3)+C(1,3))/3d0
    cv=(C(4,4)+C(5,5)+C(6,6))/3d0
    ar=(S(1,1)+S(2,2)+S(3,3))/3d0
    br=(S(1,2)+S(2,3)+S(1,3))/3d0
    cr=(S(4,4)+S(5,5)+S(6,6))/3d0
!
    kv=(av+2d0*bv)/3d0
    kr=1d0/(3d0*ar+6d0*br)
    kh=0.5d0*(kv+kr) 
    gv=(av-bv+3d0*cv)/5d0
    gr=5d0/(4d0*ar-4d0*br+3d0*cr)
    gh=0.5d0*(gv+gr)
    Ev=1d0/(1d0/(3d0*gv)+1d0/(9d0*kv))
    Er=1d0/(1d0/(3d0*gr)+1d0/(9d0*kr))
    Eh=0.5d0*(Ev+Er)
    nuv=0.5d0*(1d0-(3d0*gv)/(3d0*kv+gv))
    nur=0.5d0*(1d0-(3d0*gr)/(3d0*kv+gr))
    nuh=0.5d0*(nuv+nur)
    Kgv=kv/gv
    Kgr=kr/gr
    kgh=kh/gh
    mv = kv + (4d0*gv/3d0)
    mr = kr + (4d0*gr/3d0)
    mh = (mv + mr)/2d0
    Pc= C(1,1)- C(4,4)
    CALL ductiletester_KG(kgh,bdout1)
    CALL ductiletester_PR(nuh,bdout2)
    Call coval_metal_Pc(Pc,Cov_met)
 
    WRITE(*,*)'=========================================================='
    WRITE(*,*)' Elastic properties  |     Voigt     Reuss     Average  '
    WRITE(*,*)'=========================================================='
    WRITE(*,'(a,3F10.3,a)')' = Bulk modulus  (GPa)  | ', kv,kr,kh    ,'   ='
    WRITE(*,'(a,3F10.3,a)')' = Shear modulus (GPa)  | ', gv,gr,gh    ,'   ='
    WRITE(*,'(a,3F10.3,a)')' = Young modulus (GPa)  | ', Ev,Er,Eh    ,'   ='
    WRITE(*,'(a,3F10.4,a)')' = P-wave modulus(GPa)  | ', mv,mr,mh    ,'   ='
    WRITE(*,'(a,3F10.4,a,3a)')' = Poisson ratio        | ', nuv,nur,nuh ,'   = ','<--(  ',bdout1,'regime      )' !| WRITE(*,'(a,3F10.4,a,3a)')' = Pugh ratio           | ', kgv,kgr,kgh ,'   = ','<--(  ',bdout2,'regime   )' !|==>article:http://dx.doi.org/10.1080/09500839.2016.1243264
    WRITE(*,'(a,3F10.4,a,3a)')' = Pugh ratio           | ', kgv,kgr,kgh ,'   = ','<--(  ',bdout2,'regime      )' !|==> article:http://dx.doi.org/10.1080/09500839.2016.1243264

    WRITE(*,*)'=========================================================='
 
    WRITE(99,*)'=========================================================='
    WRITE(99,*)' Elastic properties  |     Voigt     Reuss     Average  '
    WRITE(99,*)'=========================================================='
    WRITE(99,'(a,3F10.3,a)')' = Bulk modulus  (GPa)| ', kv,kr,kh    ,'   ='
    WRITE(99,'(a,3F10.3,a)')' = Shear modulus (GPa)| ', gv,gr,gh    ,'   ='
    WRITE(99,'(a,3F10.3,a)')' = Young modulus (GPa)| ', Ev,Er,Eh    ,'   ='
    WRITE(99,'(a,3F10.4,a)')' = P-wave modulus(GPa)| ', mv,mr,mh    ,'   ='
    WRITE(99,'(a,3F10.4,a,3a)')' = Poisson ratio      | ', nuv,nur,nuh ,'   = ','<--(  ',bdout1,'regime      )' !|
    WRITE(99,'(a,3F10.4,a,3a)')' = Pugh ratio         | ', kgv,kgr,kgh ,'   = ','<--(  ',bdout2,'regime      )' !|==> article:http://dx.doi.org/10.1080/09500839.2016.1243264
    WRITE(99,*)'=========================================================='
    !Universal anisotropy index (Ranganathan and Ostoja-Starzewski method; PRL 101, 055504 (2008)) 
    !Log-Euclidean anisotropy parameter by Christopher M. Kube, AIP Advances 6, 095209 (2016)
    AU = (kv/kr) + 5d0*(gv/gr) - 6.0d0
    AL = sqrt(5d0)*2.303d0*log(1 + (AU/5))
    Ac = (gv-gr)/(gv+gr)
    WRITE(*,'(a,3F10.4)')' > Universal anisotropy index (AU)        :', au
    WRITE(*,'(a,3F10.4)')' > Log-Euclidean anisotropy parameter (AL):',al
    WRITE(*,'(a,3F10.4)')' > Chung-Buessem Anisotropy Index (Ac)    :',ac 
    WRITE(*,'(a,F10.4,a,a,a)')' > Cauchy pressure(GPa) (Pc)              : ',Pc,'       <--(  ',Cov_met,')'
    WRITE(*,*)'----------------------------------------------------------'
    WRITE(*,*)''
 
    WRITE(99,'(a,3F10.4)')' > Universal anisotropy index (AU)        :', au
    WRITE(99,'(a,3F10.4)')' > Log-Euclidean anisotropy parameter (AL):',al
    WRITE(99,'(a,3F10.4)')' > Chung-Buessem Anisotropy Index (Ac)    :',ac 
    WRITE(99,'(a,F10.4,a,a21,a)')' > Cauchy pressure(GPa) (Pc)              : ',Pc,'     <--(  ',Cov_met,')'
    WRITE(99,*)'----------------------------------------------------------'
    WRITE(99,*)''
 
END SUBROUTINE

