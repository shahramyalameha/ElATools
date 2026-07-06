!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
SUBROUTINE get_data_velogslice_plotly(namepro,xyz,n_phif,n_thetaf,cutmesh, type_pro)
  implicit NONE
  DOUBLE PRECISION, DIMENSION(19999)  ::    VVG_P_x, VVG_P_y, VVG_P_z ,&
                                            VVG_Sf_x, VVG_Sf_y, VVG_Sf_z,&
                                            VVG_Ss_x, VVG_Ss_y, VVG_Ss_z 

                                             
 DOUBLE PRECISION, DIMENSION(19999)   ::   temp1, temp2, temp3, temp4, temp5, temp6,&
                                            temp7, temp8, temp9, temp10, temp11, temp12
                                          
                                         
  INTEGER,          DIMENSION(1900,4)   :: mesh=0
  INTEGER                               :: n_phif, n_thetaf, num_mesh,i,ii,argl,cutmesh,k,&
                                           start_new_reng,end_new_reng,xyz
  character(len=10)                     :: val='',namepro ,type_pro ! type_pro : max, min, neg
  
!=============================================read mesh
    !OPEN(69, file="MESH")
    !read(69,*)
    n_phif=360
    n_thetaf=180
    cutmesh=n_phif*n_thetaf
    
   ! close(69)
!=============================================    
IF (namepro=="gp3ds" .or. namepro=="gf3ds" .or. namepro=="gs3ds" .or. namepro=="ga3ds") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,n_phif+1
    open(10, file="2dcut_gveloc.dat")
    read(10,*) temp1(ii), temp2(ii), temp3(ii),temp4(ii),&
               VVG_P_x(ii) , VVG_P_y(ii),  VVG_P_z(ii),  &
               VVG_Sf_x(ii), VVG_Sf_y(ii), VVG_Sf_z(ii), &
               VVG_Ss_x(ii), VVG_Ss_y(ii), VVG_Ss_z(ii)
     !write(*,"(10F23.15 )")temp1(ii), temp2(ii), temp3(ii), young_x(ii),young_y(ii),young_z(ii),temp4(ii), temp5(ii), temp6(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  if(type_pro=='gp') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVG_P_x(i) 
     else
       write(66,"(F23.15,A)") VVG_P_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVG_P_y(i) 
     else
       write(66,"(F23.15,A)") VVG_P_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVG_P_z(i) 
     else
       write(66,"(F23.15,A)") VVG_P_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
!!!!
  if(type_pro=='gf') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVG_Sf_x(i) 
     else
       write(66,"(F23.15,A)") VVG_Sf_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVG_Sf_y(i) 
     else
       write(66,"(F23.15,A)") VVG_Sf_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVG_Sf_z(i) 
     else
       write(66,"(F23.15,A)") VVG_Sf_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
!!!
  if(type_pro=='gs') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVG_Ss_x(i) 
     else
       write(66,"(F23.15,A)") VVG_Ss_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVG_Ss_y(i) 
     else
       write(66,"(F23.15,A)") VVG_Ss_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVG_Ss_z(i) 
     else
       write(66,"(F23.15,A)") VVG_Ss_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif

ENDif
!################################################################################
Close(10)

END SUBROUTINE
