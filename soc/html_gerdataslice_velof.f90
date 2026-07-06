!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
SUBROUTINE get_data_velofslice_plotly(namepro,xyz,n_phif,n_thetaf,cutmesh, type_pro)
  implicit NONE
  DOUBLE PRECISION, DIMENSION(19999)  ::    VVF_P_x, VVF_P_y, VVF_P_z ,&
                                            VVF_Sf_x, VVF_Sf_y, VVF_Sf_z,&
                                            VVF_Ss_x, VVF_Ss_y, VVF_Ss_z 

                                             
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
IF (namepro=="pfp3ds" .or. namepro=="pff3ds" .or. namepro=="pfs3ds" .or. namepro=="pfa3ds") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,n_phif+1
    open(10, file="2dcut_pfaveloc.dat")
    read(10,*) temp1(ii), temp2(ii), temp3(ii),temp4(ii),&
               VVF_P_x(ii) , VVF_P_y(ii),  VVF_P_z(ii),  &
               VVF_Sf_x(ii), VVF_Sf_y(ii), VVF_Sf_z(ii), &
               VVF_Ss_x(ii), VVF_Ss_y(ii), VVF_Ss_z(ii)
     !write(*,"(10F23.15 )")temp1(ii), temp2(ii), temp3(ii), young_x(ii),young_y(ii),young_z(ii),temp4(ii), temp5(ii), temp6(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  if(type_pro=='pfp') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVF_P_x(i) 
     else
       write(66,"(F23.15,A)") VVF_P_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVF_P_y(i) 
     else
       write(66,"(F23.15,A)") VVF_P_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVF_P_z(i) 
     else
       write(66,"(F23.15,A)") VVF_P_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
!!!!
  if(type_pro=='pff') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVF_Sf_x(i) 
     else
       write(66,"(F23.15,A)") VVF_Sf_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVF_Sf_y(i) 
     else
       write(66,"(F23.15,A)") VVF_Sf_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVF_Sf_z(i) 
     else
       write(66,"(F23.15,A)") VVF_Sf_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
!!!
  if(type_pro=='pfs') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVF_Ss_x(i) 
     else
       write(66,"(F23.15,A)") VVF_Ss_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVF_Ss_y(i) 
     else
       write(66,"(F23.15,A)") VVF_Ss_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") VVF_Ss_z(i) 
     else
       write(66,"(F23.15,A)") VVF_Ss_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif

ENDif
!################################################################################
Close(10)

END SUBROUTINE
