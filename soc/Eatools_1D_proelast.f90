!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 1D MATERIAL , CALCULATED main mechanical properties for 1D compounds.

!                                                  << Ref. of 1d SYS. >>
!=============================================================================================================================
! An Automated Toolkit for Elastic and Mechanical Properties of Tubular 2D-Based Nanostructures and Nanotubes - Ekuma and Liu (under review)
!=============================================================================================================================

SUBROUTINE proelast_1D(a)
implicit none
DOuble precision :: c(2,2),a(2,2)
integer          :: i,j
DOuble precision :: Ex,Ey,Gxy,vxy,vyx,Bv,Gv,Br,Gr,A_SU,A_Ranganathan,A_Kube,A_Fang

Write(99,"(a)")"Cij:"
OPEN(58,FILE="Cij-1D.dat",STATUS='OLD',ACTION='READ')
DO i=1,2
  READ(58,*) (c(i,j),j=1,2)
  Write(99,"(6f12.6)") (c(i,j),j=1,2)
ENDDO
 close(58)
!---------------------- 
Write(99,"(a)")"Sij:"
DO i=1,2
  Write(99,"(6f12.6)") (a(i,j),j=1,2)
ENDDO
 close(58)
 
Ex            =  c(2,2)  
Gxy           = (c(2,2) - c(1,2))/2.0d0
vxy           = -c(1,2) / c(2,2) 
 
WRITE(*,*)'================================================'
WRITE(*,*)'             Elastic properties                 '
WRITE(*,*)'================================================'
WRITE(*,'(a,F10.3,a)')' = Young modulus  ',Ex, "          ="
WRITE(*,'(a,F10.3,a)')' = Shaer modulus  ',Gxy,"          ="
WRITE(*,'(a,F10.3,a)')' = Poisson ratio  ',vxy,"          ="
WRITE(*,*)'================================================'
 
  
WRITE(99,*)'================================================'
WRITE(99,*)'             Elastic properties                '
WRITE(99,*)'================================================'
WRITE(99,'(a,F10.3,a)')' = Young modulus  ',Ex, "          ="
WRITE(99,'(a,F10.3,a)')' = Shaer modulus  ',Gxy,"          ="
WRITE(99,'(a,F10.3,a)')' = Poisson ratio  ',vxy,"          ="
WRITE(99,*)'================================================' 

end SUBROUTINE


