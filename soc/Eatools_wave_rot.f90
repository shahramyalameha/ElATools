!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the phase and group vec. calculation.. v.3


SUBROUTINE rotate_MESH(l, poi_mesh, alpha, prot_mesh)  
         ! intent         in   in    in     out
         INTEGER, PARAMETER            :: qp = selected_real_kind(33, 4931)
         DOUBLE PRECISION, PARAMETER           :: PI_C=3.14159265358979323846264338327950D0
         DOUBLE PRECISION, dimension(1:3,1:3)  :: idensy, vx, vv, rotate_p
         DOUBLE PRECISION, dimension(3)        :: l, poi_mesh
         DOUBLE PRECISION, dimension(3)        :: prot_mesh
         DOUBLE PRECISION                      :: alpha ,alphar
         INTEGER                       :: i,j
         
         idensy      = 0.0D0
         idensy(1,1) = 1.0D0; idensy(2,2) = 1.0D0; idensy(3,3) = 1.0D0
         alphar      = alpha*PI_C/180.0D0
       
         vx(1,1:3)   = (/ 0.0D0,  -l(3) ,  l(2)  /)
         vx(2,1:3)   = (/  l(3) ,  0.0D0, -l(1)  /)
         vx(3,1:3)   = (/ -l(2) ,   l(1) , 0.0D0 /)
         
         vv(1,1:3)   = (/ l(1)*l(1) , l(1)*l(2), l(1)*l(3) /)
         vv(2,1:3)   = (/ l(1)*l(2) , l(2)*l(2), l(2)*l(3) /)
         vv(3,1:3)   = (/ l(1)*l(3) , l(3)*l(2), l(3)*l(3) /)
         
         rotate_p    = cos(alphar)*idensy + sin(alphar)*vx + ( 1.0D0 - cos(alphar) )*vv
         prot_mesh   = 0.0D0
         
         DO i=1,3
          DO j=1,3
            prot_mesh(i) = prot_mesh(i)+rotate_p(i,j)*poi_mesh(j)
          END DO
         END DO
         
         END SUBROUTINE rotate_MESH
         !$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
