!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
SUBROUTINE get_data_velopslice_plotly(namepro,xyz,n_phif,n_thetaf,cutmesh, type_pro)
  implicit NONE
  DOUBLE PRECISION, DIMENSION(19999)  ::    VVP_P_x, VVP_P_y, VVP_P_z ,&
                                            VVP_Sf_x,VVP_Sf_y,VVP_Sf_z,&
                                            VVP_Ss_x,VVP_Ss_y,VVP_Ss_z 

                                             
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
IF (namepro=="pp3ds" .or. namepro=="pf3ds" .or. namepro=="ps3ds" .or. namepro=="pa3ds") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,n_phif+1
    open(10, file="2dcut_pveloc.dat")
    read(10,*) temp1(ii), temp2(ii), temp3(ii),temp4(ii),&
               VVP_P_x(ii) , VVP_P_y(ii),  VVP_P_z(ii),  &
               VVP_Sf_x(ii), VVP_Sf_y(ii), VVP_Sf_z(ii), &
               VVP_Ss_x(ii), VVP_Ss_y(ii), VVP_Ss_z(ii)
     !write(*,"(10F23.15 )")temp1(ii), temp2(ii), temp3(ii), young_x(ii),young_y(ii),young_z(ii),temp4(ii), temp5(ii), temp6(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  if(type_pro=='pp') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVP_P_x(i) 
     else
       write(66,"(F23.15,A)") VVP_P_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVP_P_y(i) 
     else
       write(66,"(F23.15,A)") VVP_P_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVP_P_z(i) 
     else
       write(66,"(F23.15,A)") VVP_P_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
!!!!
  if(type_pro=='pf') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVP_Sf_x(i) 
     else
       write(66,"(F23.15,A)") VVP_Sf_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVP_Sf_y(i) 
     else
       write(66,"(F23.15,A)") VVP_Sf_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVP_Sf_z(i) 
     else
       write(66,"(F23.15,A)") VVP_Sf_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
!!!
  if(type_pro=='ps') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVP_Ss_x(i) 
     else
       write(66,"(F23.15,A)") VVP_Ss_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVP_Ss_y(i) 
     else
       write(66,"(F23.15,A)") VVP_Ss_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVP_Ss_z(i) 
     else
       write(66,"(F23.15,A)") VVP_Ss_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif

ENDif
!################################################################################
Close(10)

END SUBROUTINE