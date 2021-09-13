!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 2D MATERIAL , CALCULATED main mechanical properties for 2D compounds.

!                                                  << Ref. of anistropy >>
!=============================================================================================================================
! Li, Ruishan, et al. "Elastic anisotropy measure for two-dimensional crystals." Extreme Mechanics Letters 34 (2020): 100615.
!=============================================================================================================================

SUBROUTINE proelast_2D()
implicit none
DOuble precision :: c(3,3),a(3,3)
integer          :: i,j
DOuble precision :: Ex,Ey,Gxy,vxy,vyx,Bv,Gv,Br,Gr,A_SU,A_Ranganathan,A_Kube,A_Fang

Write(99,"(a)")"Cij:"
OPEN(58,FILE="Cij-2D.dat",STATUS='OLD',ACTION='READ')

DO i=1,3
  READ(58,*) (c(i,j),j=1,3)
  Write(99,"(6f12.6)") (c(i,j),j=1,3)
ENDDO
close(58)
Write(99,"(a)")"Sij:"
OPEN(58,FILE="Sij-2D.dat",STATUS='OLD',ACTION='READ')
DO i=1,3
  READ(58,*) (a(i,j),j=1,3)
  Write(99,"(6f12.6)") (a(i,j),j=1,3)
ENDDO
close(58)
Bv            = (C(1,1)+C(2,2) + 2D0*C(1,2))/4D0
Gv            = (C(1,1)+C(2,2) - 2D0*C(1,2)+4d0*C(3,3))/8d0
Br            = 1d0/(a(1,1) + a(2,2) + 2d0*a(1,2))
Gr            = 2d0/(a(1,1) + a(2,2) - 2d0*a(1,2)+a(3,3))
A_SU          = SQRT( (((Bv/Br) - 1.0d0)**2d0) + 2d0*(((Gv/Gr) - 1.0d0)**2d0) )
A_Ranganathan = (Bv/Br)+(2d0*(Gv/Gr)) - 3d0
A_Kube        = SQRT( (LOG10((Bv/Br)))**2d0 + 2d0* ((LOG10(Gv/Gr))**2d0) )
Ex            = (c(1,1)*c(2,2) - c(1,2)*c(2,1))/c(2,2)
Ey            = (c(1,1)*c(2,2) - c(1,2)*c(2,1))/c(1,1)
Gxy           = c(3,3)
vxy           = c(2,1)/c(2,2)
vyx           = c(1,2)/c(1,1)


WRITE(*,*)'================================================'
WRITE(*,*)'             Elastic properties                 '
WRITE(*,*)'================================================'
WRITE(*,'(a,F10.3,a)')' = Young modulus [Ex]  (N/m)',Ex,"          ="
WRITE(*,'(a,F10.3,a)')' = Young modulus [Ey]  (N/m)',Ey,"          ="
WRITE(*,'(a,F10.3,a)')' = Shaer modulus [Gxy] (N/m)',Gxy,"          ="
WRITE(*,'(a,F10.3,a)')' = Shaer modulus [Gv]  (N/m)',Gv,"          ="
WRITE(*,'(a,F10.3,a)')' = Shaer modulus [Gr]  (N/m)',Gr,"          ="
WRITE(*,'(a,F10.3,a)')' = Area modulus  [Kv]  (N/m)',Bv,"          ="
WRITE(*,'(a,F10.3,a)')' = Area modulus  [Kr]  (N/m)',Br,"          ="
WRITE(*,'(a,F10.3,a)')' = Poisson ratio [vxy]      ',vxy,"          ="
WRITE(*,'(a,F10.3,a)')' = Poisson ratio [vyx]      ',vyx,"          ="
WRITE(*,*)'================================================================== '
WRITE(*,'(a,F10.4,a)')' = Elastic anisotropy index (A_SU):           ',A_SU,"          ="
WRITE(*,'(a,F10.4,a)')' = Ranganathan Elastic anisotropy index (A_R):',A_Ranganathan,"          ="
WRITE(*,'(a,F10.4,a)')' = Kube Elastic anisotropy index (A_K):       ',A_Kube,"          ="
WRITE(*,*)'================================================================== '
  
WRITE(99,*)'================================================'
WRITE(99,*)'             Elastic properties                '
WRITE(99,*)'================================================'
WRITE(99,'(a,F10.3,a)')' = Young modulus [Ex]  (N/m)',Ex,"          ="
WRITE(99,'(a,F10.3,a)')' = Young modulus [Ey]  (N/m)',Ey,"          ="
WRITE(99,'(a,F10.3,a)')' = Shaer modulus [Gxy] (N/m)',Gxy,"          ="
WRITE(99,'(a,F10.3,a)')' = Shaer modulus [Gv]  (N/m)',Gv,"          ="
WRITE(99,'(a,F10.3,a)')' = Shaer modulus [Gr]  (N/m)',Gr,"          ="
WRITE(99,'(a,F10.3,a)')' = Area modulus  [Kv]  (N/m)',Bv,"          ="
WRITE(99,'(a,F10.3,a)')' = Area modulus  [Kr]  (N/m)',Br,"          ="
WRITE(99,'(a,F10.3,a)')' = Poisson ratio [vxy]      ',vxy,"          ="
WRITE(99,'(a,F10.3,a)')' = Poisson ratio [vyx]      ',vyx,"          ="
WRITE(99,*)'================================================================== '
WRITE(99,'(a,F10.4,a)')' = Elastic anisotropy index (A_SU):           ',A_SU,"          ="
WRITE(99,'(a,F10.4,a)')' = Ranganathan Elastic anisotropy index (A_R):',A_Ranganathan,"          ="
WRITE(99,'(a,F10.4,a)')' = Kube Elastic anisotropy index (A_K):       ',A_Kube,"          ="
WRITE(99,*)'================================================================== '
  
end SUBROUTINE
