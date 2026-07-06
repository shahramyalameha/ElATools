!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
SUBROUTINE get_dataplotly_3dslice( namepro,xyz,n_phif,n_thetaf,cutmesh, type_pro)
  implicit NONE


  DOUBLE PRECISION, DIMENSION(10000)  ::    young_x,young_y,young_z,young0_x,young0_y,young0_z,younge_x,younge_y,younge_z,&
                                           hardmax_x,hardmax_y,hardmax_z, hardmin_x, hardmin_y, hardmin_z, harde_x,harde_y,harde_z,&
                                           bulk_x,bulk_y,bulk_z,bulk0_x,bulk0_y,bulk0_z,bulke_x,bulke_y,bulke_z,&
                                           shear_max_x ,shear_max_y ,shear_max_z,      &
                                           shear0_max_x ,shear0_max_y ,shear0_max_z,   &
                                           sheare_max_x ,sheare_max_y ,sheare_max_z,   &
                                           shear_min_x ,shear_min_y ,shear_min_z,      &
                                           shear0_min_x ,shear0_min_y ,shear0_min_z,   &
                                           sheare_min_x ,sheare_min_y ,sheare_min_z,   &
                                           shear_neg_x ,shear_neg_y ,shear_neg_z,      &
                                           shear_avg_x ,shear_avg_y ,shear_avg_z ,     &
                                           copm0_max_x ,copm0_max_y ,copm0_max_z ,        &
                                           copme_max_x ,copme_max_y ,copme_max_z ,        &
                                           copm_max_x ,copm_max_y ,copm_max_z ,        &
                                           copm0_min_x ,copm0_min_y ,copm0_min_z ,        &
                                           copme_min_x ,copme_min_y ,copme_min_z ,        &
                                           copm_min_x ,copm_min_y ,copm_min_z ,        &
                                           copm0_neg_x ,copm0_neg_y ,copm0_neg_z , &
                                           copme_neg_x ,copme_neg_y ,copme_neg_z , &
                                           copm_neg_x ,copm_neg_y ,copm_neg_z,     &
                                           poi0_max_x  ,poi0_max_y  ,poi0_max_z ,&
                                           poie_max_x  ,poie_max_y  ,poie_max_z  ,&
                                           poi_max_x  ,poi_max_y  ,poi_max_z  ,&
                                           poi0_min_x  ,poi0_min_y  ,poi0_min_z  ,&
                                           poie_min_x  ,poie_min_y  ,poie_min_z  ,&
                                           poi_min_x  ,poi_min_y  ,poi_min_z  ,&
                                           poi0_neg_x  ,poi0_neg_y  ,poi0_neg_z  ,& 
                                           poie_neg_x  ,poie_neg_y  ,poie_neg_z  ,&
                                           poi_neg_x  ,poi_neg_y  ,poi_neg_z  ,&
                                           poi_max_avg_x  ,poi_max_avg_y  ,poi_max_avg_z  ,&
                                           poi_min_avg_x  ,poi_min_avg_y  ,poi_min_avg_z  ,&
                                           pugh_max_x ,pugh_max_y ,pugh_max_z,      &
                                           pugh0_max_x ,pugh0_max_y ,pugh0_max_z,   &
                                           pughe_max_x ,pughe_max_y ,pughe_max_z,   &
                                           pugh_min_x ,pugh_min_y ,pugh_min_z,      &
                                           pugh0_min_x ,pugh0_min_y ,pugh0_min_z,   &
                                           pughe_min_x ,pughe_min_y ,pughe_min_z,   &
                                           pugh_neg_x ,pugh_neg_y ,pugh_neg_z,      &
                                           pugh_avg_x ,pugh_avg_y ,pugh_avg_z,      &
                                           km_x, km_y, km_z ,                       &
                                           temp1, temp2, temp3, temp4, temp5, temp6,&
                                           temp7, temp8, temp9, temp10, temp11, temp12
                                          
                                         
  INTEGER,          DIMENSION(190300,4) :: mesh=0
  INTEGER                               :: n_phif, n_thetaf, num_mesh,i,ii,argl,cutmesh,k, &
                                           start_new_reng,end_new_reng,xyz
  character(len=10)                     :: val='',namepro ,type_pro ! type_pro : max, min, neg
  
!=============================================read mesh
    OPEN(69, file="MESH")
    read(69,*)n_phif,n_thetaf,cutmesh
    close(69)
!=============================================    
IF (namepro=="km3ds") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,n_phif+1
    open(10, file="2dcut_km.dat")
    read(10,*) temp1(ii), temp2(ii), km_x(ii), km_y(ii), km_z(ii)
   enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
   
  if(type_pro=='max') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") km_x(i) 
     else
       write(66,"(F23.15,A)") km_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") km_y(i) 
     else
       write(66,"(F23.15,A)") km_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") km_z(i) 
     else
       write(66,"(F23.15,A)") km_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
ENDIF
!#######################################################################
IF (namepro=="you3ds") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,n_phif+1
    open(10, file="2dcut_young.dat")
    read(10,*) temp1(ii), temp2(ii), temp3(ii), young_x(ii),young_y(ii),young_z(ii),temp4(ii), temp5(ii), temp6(ii)
     !write(*,"(10F23.15 )")temp1(ii), temp2(ii), temp3(ii), young_x(ii),young_y(ii),young_z(ii),temp4(ii), temp5(ii), temp6(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
   
  if(type_pro=='max') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") young_x(i) 
     else
       write(66,"(F23.15,A)") young_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") young_y(i) 
     else
       write(66,"(F23.15,A)") young_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") young_z(i) 
     else
       write(66,"(F23.15,A)") young_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
ENDIF
!#######################################################################
IF (namepro=="bulk3ds") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,n_phif+1
    open(10, file="2dcut_bulk.dat")
    read(10,*) temp1(ii), temp2(ii), temp3(ii), temp4(ii), temp5(ii), temp6(ii), bulk_x(ii),bulk_y(ii),bulk_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
   
  if(type_pro=='max') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") bulk_x(i) 
     else
       write(66,"(F23.15,A)") bulk_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") bulk_y(i) 
     else
       write(66,"(F23.15,A)") bulk_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") bulk_z(i) 
     else
       write(66,"(F23.15,A)") bulk_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
ENDIF
!#######################################################################
IF (namepro=="poi3ds") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,n_phif+1
    open(10, file="2dcut_poisson.dat")
        read(10,*) temp1(ii), temp2(ii), temp3(ii), temp4(ii),&
                   poi_max_x(ii)  ,poi_max_y(ii)  ,poi_max_z(ii),&
                   poi_min_x(ii)  ,poi_min_y(ii)  ,poi_min_z(ii),&
                   poi_neg_x(ii)  ,poi_neg_y(ii)  ,poi_neg_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  if(type_pro=='max') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") poi_max_x(i) 
     else
       write(66,"(F23.15,A)") poi_max_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") poi_max_y(i) 
     else
       write(66,"(F23.15,A)") poi_max_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") poi_max_z(i) 
     else
       write(66,"(F23.15,A)") poi_max_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
!!!!
  if(type_pro=='min') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") poi_min_x(i) 
     else
       write(66,"(F23.15,A)") poi_min_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") poi_min_y(i) 
     else
       write(66,"(F23.15,A)") poi_min_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") poi_min_z(i) 
     else
       write(66,"(F23.15,A)") poi_min_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
!!!
  if(type_pro=='neg') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") poi_neg_x(i) 
     else
       write(66,"(F23.15,A)") poi_neg_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") poi_neg_y(i) 
     else
       write(66,"(F23.15,A)") poi_neg_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") poi_neg_z(i) 
     else
       write(66,"(F23.15,A)") poi_neg_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif

ENDif
!#######################################################################
IF (namepro=="comp3ds") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,n_phif+1
    open(10, file="2dcut_comp.dat")
        Read(10,*) temp1(ii), temp2(ii), temp3(ii),&
                   copm_max_x(ii)  ,copm_max_y(ii)  ,copm_max_z(ii),&
                   copm_neg_x(ii)  ,copm_neg_y(ii)  ,copm_neg_z(ii) 

  Enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  if(type_pro=='max') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") copm_max_x(i) 
     else
       write(66,"(F23.15,A)") copm_max_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") copm_max_y(i) 
     else
       write(66,"(F23.15,A)") copm_max_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") copm_max_z(i) 
     else
       write(66,"(F23.15,A)") copm_max_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
!!!!
  if(type_pro=='neg') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") copm_neg_x(i) 
     else
       write(66,"(F23.15,A)") copm_neg_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") copm_neg_y(i) 
     else
       write(66,"(F23.15,A)") copm_neg_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") copm_neg_z(i) 
     else
       write(66,"(F23.15,A)") copm_neg_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif

ENDif
!#######################################################################
!#######################################################################
IF (namepro=="she3ds") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,n_phif+1
    open(10, file="2dcut_shear.dat")
        Read(10,*) temp1(ii), temp2(ii), temp3(ii),temp4(ii),&
                   shear_max_x(ii) ,shear_max_y(ii) ,shear_max_z(ii),&
                   shear_min_x(ii) ,shear_min_y(ii) ,shear_min_z(ii),&
                   shear_neg_x(ii) ,shear_neg_y(ii) ,shear_neg_z(ii) 

  Enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  if(type_pro=='max') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") shear_max_x(i) 
     else
       write(66,"(F23.15,A)") shear_max_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") shear_max_y(i) 
     else
       write(66,"(F23.15,A)") shear_max_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") shear_max_z(i) 
     else
       write(66,"(F23.15,A)") shear_max_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
!!!!
  if(type_pro=='min') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") shear_min_x(i) 
     else
       write(66,"(F23.15,A)") shear_min_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") shear_min_y(i) 
     else
       write(66,"(F23.15,A)") shear_min_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") shear_min_z(i) 
     else
       write(66,"(F23.15,A)") shear_min_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif

ENDif
!#######################################################################
IF (namepro=="pugh3ds") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,n_phif+1
    open(10, file="2dcut_pugh.dat")
        read(10,*) temp1(ii), temp2(ii), temp3(ii), temp4(ii),&
                   pugh_max_x(ii)  ,pugh_max_y(ii)  ,pugh_max_z(ii),&
                   pugh_min_x(ii)  ,pugh_min_y(ii)  ,pugh_min_z(ii),&
                   pugh_neg_x(ii)  ,pugh_neg_y(ii)  ,pugh_neg_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  if(type_pro=='max') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") pugh_max_x(i) 
     else
       write(66,"(F23.15,A)") pugh_max_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") pugh_max_y(i) 
     else
       write(66,"(F23.15,A)") pugh_max_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") pugh_max_z(i) 
     else
       write(66,"(F23.15,A)") pugh_max_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
!!!!
  if(type_pro=='min') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") pugh_min_x(i) 
     else
       write(66,"(F23.15,A)") pugh_min_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") pugh_min_y(i) 
     else
       write(66,"(F23.15,A)") pugh_min_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") pugh_min_z(i) 
     else
       write(66,"(F23.15,A)") pugh_min_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
!!!
  if(type_pro=='neg') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") pugh_neg_x(i) 
     else
       write(66,"(F23.15,A)") pugh_neg_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") pugh_neg_y(i) 
     else
       write(66,"(F23.15,A)") pugh_neg_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

      If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") pugh_neg_z(i) 
     else
       write(66,"(F23.15,A)") pugh_neg_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif

ENDif

!#######################################################################
IF (namepro=="hard3ds") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,n_phif+1
    open(10, file="2dcut_hardness.dat")
    read(10,*) temp1(ii), temp2(ii), temp3(ii), &
               hardmax_x(ii), hardmax_y(ii), hardmax_z(ii),&
               hardmin_x(ii), hardmin_y(ii), hardmin_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  if(type_pro=='max') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") hardmax_x(i) 
     else
       write(66,"(F23.15,A)") hardmax_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") hardmax_y(i) 
     else
       write(66,"(F23.15,A)") hardmax_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") hardmax_z(i) 
     else
       write(66,"(F23.15,A)") hardmax_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
!!!!
  if(type_pro=='min') then

   If(xyz==1) then
   !============================================================= start x-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") hardmin_x(i) 
     else
       write(66,"(F23.15,A)") hardmin_x(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==2) then
   !============================================================= start y-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") hardmin_y(i) 
     else
       write(66,"(F23.15,A)") hardmin_y(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif

   If(xyz==3) then
   !============================================================= start z-points 
    do i=1,n_phif+1
     if(i==n_phif+1) then
       write(66,"(F23.15 )") hardmin_z(i) 
     else
       write(66,"(F23.15,A)") hardmin_z(i),","
     endif
    enddo
    call middle_cordslice_web()
   endif
  endif
  
ENDIF
!#######################################################################
 close(10)
END SUBROUTINE
