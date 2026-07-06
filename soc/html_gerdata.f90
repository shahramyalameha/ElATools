
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
SUBROUTINE get_dataplotly(namepro, xyz, n_phif, n_thetaf, cutmesh, type_pro)
   implicit NONE

   DOUBLE PRECISION, DIMENSION(1599000)  :: young_x, young_y, young_z, young0_x, young0_y, young0_z, younge_x, younge_y, younge_z, &
      hardmax_x, hardmax_y, hardmax_z, hardmin_x, hardmin_y, hardmin_z, &
      hard0_max_x, hard0_max_y, hard0_max_z, harde_max_x, harde_max_y, harde_max_z, &
      hard0_min_x, hard0_min_y, hard0_min_z, harde_min_x, harde_min_y, harde_min_z, &
      bulk_x, bulk_y, bulk_z, bulk0_x, bulk0_y, bulk0_z, bulke_x, bulke_y, bulke_z, &
      shear_max_x, shear_max_y, shear_max_z, &
      shear0_max_x, shear0_max_y, shear0_max_z, &
      sheare_max_x, sheare_max_y, sheare_max_z, &
      shear_min_x, shear_min_y, shear_min_z, &
      shear0_min_x, shear0_min_y, shear0_min_z, &
      sheare_min_x, sheare_min_y, sheare_min_z, &
      shear_neg_x, shear_neg_y, shear_neg_z, &
      shear_avg_x, shear_avg_y, shear_avg_z, &
      copm0_max_x, copm0_max_y, copm0_max_z, &
      copme_max_x, copme_max_y, copme_max_z, &
      copm_max_x, copm_max_y, copm_max_z, &
      copm0_min_x, copm0_min_y, copm0_min_z, &
      copme_min_x, copme_min_y, copme_min_z, &
      copm_min_x, copm_min_y, copm_min_z, &
      copm0_neg_x, copm0_neg_y, copm0_neg_z, &
      copme_neg_x, copme_neg_y, copme_neg_z, &
      copm_neg_x, copm_neg_y, copm_neg_z, &
      poi0_max_x, poi0_max_y, poi0_max_z, &
      poie_max_x, poie_max_y, poie_max_z, &
      poi_max_x, poi_max_y, poi_max_z, &
      poi0_min_x, poi0_min_y, poi0_min_z, &
      poie_min_x, poie_min_y, poie_min_z, &
      poi_min_x, poi_min_y, poi_min_z, &
      poi0_neg_x, poi0_neg_y, poi0_neg_z, &
      poie_neg_x, poie_neg_y, poie_neg_z, &
      poi_neg_x, poi_neg_y, poi_neg_z, &
      poi_max_avg_x, poi_max_avg_y, poi_max_avg_z, &
      poi_min_avg_x, poi_min_avg_y, poi_min_avg_z, &
      pugh_max_x, pugh_max_y, pugh_max_z, &
      pugh0_max_x, pugh0_max_y, pugh0_max_z, &
      pughe_max_x, pughe_max_y, pughe_max_z, &
      pugh_min_x, pugh_min_y, pugh_min_z, &
      pugh0_min_x, pugh0_min_y, pugh0_min_z, &
      pughe_min_x, pughe_min_y, pughe_min_z, &
      pugh_neg_x, pugh_neg_y, pugh_neg_z, &
      pugh_avg_x, pugh_avg_y, pugh_avg_z, &
      km_x, km_y, km_z

   INTEGER, DIMENSION(190300, 4) :: mesh = 0
   INTEGER                               :: n_phif, n_thetaf, num_mesh, i, ii, argl, cutmesh, k, &
                                            start_new_reng, end_new_reng, xyz
   character(len=10)                     :: val = '', namepro, type_pro ! type_pro : max, min, neg
   Write (*, *) n_phif, n_thetaf, cutmesh
!=============================================read mesh
   OPEN (69, file="MESH")
   read (69, *) n_phif, n_thetaf, cutmesh
   !Write(*,*)n_phif,n_thetaf,cutmesh
   close (69)
!=============================================

   IF (namepro == "young") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
      Do ii = 1, cutmesh
         open (10, file="3d_young.dat")
         read (10, *) young_x(ii), young_y(ii), young_z(ii)
      end do
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      !>> first and end point
      young0_x(1) = young_x(1); young0_y(1) = young_y(1); young0_z(1) = young_z(1)
      younge_x(1) = young_x(cutmesh); younge_y(1) = young_y(cutmesh); younge_z(1) = young_z(cutmesh)
      !<<
      !============================================================= start x-points
      if (type_pro == 'max') then

         If (xyz == 1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") young0_x(1)
               else
                  write (66, "(F23.15,A)") young0_x(1), ","
               end if
            end do
            call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!! end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") young_x(i)
                  else
                     write (66, "(F23.15,A)") young_x(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") younge_x(1)
               else
                  write (66, "(F23.15,A)") younge_x(1), ","
               end if
            end do
    !!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
         if (xyz == 2) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") young0_y(1)
               else
                  write (66, "(F23.15,A)") young0_y(1), ","
               end if
            end do
            call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") young_y(i)
                  else
                     write (66, "(F23.15,A)") young_y(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") younge_y(1)
               else
                  write (66, "(F23.15,A)") younge_y(1), ","
               end if
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
         if (xyz == 3) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") young0_z(1)
               else
                  write (66, "(F23.15,A)") young0_z(1), ","
               end if
            end do
            call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") young_z(i)
                  else
                     write (66, "(F23.15,A)") young_z(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") younge_z(1)
               else
                  write (66, "(F23.15,A)") younge_z(1), ","
               end if
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if
   End if
!$#######################################################################################
   IF (namepro == "bulk") THEN

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
      Do ii = 1, cutmesh
         open (10, file="3d_bulk.dat")
         read (10, *) bulk_x(ii), bulk_y(ii), bulk_z(ii)
      end do
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      !>> first and end point
      bulk0_x(1) = bulk_x(1); bulk0_y(1) = bulk_y(1); bulk0_z(1) = bulk_z(1)
      bulke_x(1) = bulk_x(cutmesh); bulke_y(1) = bulk_y(cutmesh); bulke_z(1) = bulk_z(cutmesh)
      !<<
      !============================================================= start x-points
      !============================================================= start x-points
      if (type_pro == 'max') then

         If (xyz == 1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") bulk0_x(1)
               else
                  write (66, "(F23.15,A)") bulk0_x(1), ","
               end if
            end do
            call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!! end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") bulk_x(i)
                  else
                     write (66, "(F23.15,A)") bulk_x(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") bulke_x(1)
               else
                  write (66, "(F23.15,A)") bulke_x(1), ","
               end if
            end do
    !!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
         if (xyz == 2) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") bulk0_y(1)
               else
                  write (66, "(F23.15,A)") bulk0_y(1), ","
               end if
            end do
            call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") bulk_y(i)
                  else
                     write (66, "(F23.15,A)") bulk_y(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") bulke_y(1)
               else
                  write (66, "(F23.15,A)") bulke_y(1), ","
               end if
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
         if (xyz == 3) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") bulk0_z(1)
               else
                  write (66, "(F23.15,A)") bulk0_z(1), ","
               end if
            end do
            call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") bulk_z(i)
                  else
                     write (66, "(F23.15,A)") bulk_z(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") bulke_z(1)
               else
                  write (66, "(F23.15,A)") bulke_z(1), ","
               end if
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if
   End if
!$#######################################################################################
   IF (namepro == "shear") THEN

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
      Do ii = 1, cutmesh
         open (10, file="3d_shear.dat")
         read (10, *) shear_max_x(ii), shear_max_y(ii), shear_max_z(ii), &
            shear_min_x(ii), shear_min_y(ii), shear_min_z(ii), &
            shear_neg_x(ii), shear_neg_y(ii), shear_neg_z(ii), &
            shear_avg_x(ii), shear_avg_y(ii), shear_avg_z(ii)
      end do
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      !>> first and end point
      shear0_max_x(1) = shear_max_x(1); shear0_max_y(1) = shear_max_y(1); shear0_max_z(1) = shear_max_z(1)
      sheare_max_x(1) = shear_max_x(cutmesh); sheare_max_y(1) = shear_max_y(cutmesh); sheare_max_z(1) = shear_max_z(cutmesh)

      shear0_min_x(1) = shear_min_x(1); shear0_min_y(1) = shear_min_y(1); shear0_min_z(1) = shear_min_z(1)
      sheare_min_x(1) = shear_min_x(cutmesh); sheare_min_y(1) = shear_min_y(cutmesh); sheare_min_z(1) = shear_min_z(cutmesh)
      !<<
      !============================================================= start x-points
      !============================================================= start x-points
      if (type_pro == 'max') then
      If (xyz == 1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") shear0_max_x(1)
            else
               write (66, "(F23.15,A)") shear0_max_x(1), ","
            end if
         end do
         call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!! end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") shear_max_x(i)
               else
                  write (66, "(F23.15,A)") shear_max_x(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") sheare_max_x(1)
            else
               write (66, "(F23.15,A)") sheare_max_x(1), ","
            end if
         end do
    !!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
      if (xyz == 2) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") shear0_max_y(1)
            else
               write (66, "(F23.15,A)") shear0_max_y(1), ","
            end if
         end do
         call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") shear_max_y(i)
               else
                  write (66, "(F23.15,A)") shear_max_y(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") sheare_max_y(1)
            else
               write (66, "(F23.15,A)") sheare_max_y(1), ","
            end if
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
      if (xyz == 3) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") shear0_max_z(1)
            else
               write (66, "(F23.15,A)") shear0_max_z(1), ","
            end if
         end do
         call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") shear_max_z(i)
               else
                  write (66, "(F23.15,A)") shear_max_z(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") sheare_max_z(1)
            else
               write (66, "(F23.15,A)") sheare_max_z(1), ","
            end if
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if
      !==========================<<<<<<<< max shear end
      !============================================================= start x-points
      !============================================================= start x-points
      if (type_pro == 'min') then

         If (xyz == 1) then
  !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") shear0_min_x(1)
               else
                  write (66, "(F23.15,A)") shear0_min_x(1), ","
               end if
            end do
            call middle_cord_web()
  !!!!!!!!!!!!!!!!!!!!!!!!! end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") shear_min_x(i)
                  else
                     write (66, "(F23.15,A)") shear_min_x(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") sheare_min_x(1)
               else
                  write (66, "(F23.15,A)") sheare_min_x(1), ","
               end if
            end do
  !!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
!!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
         if (xyz == 2) then
  !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") shear0_min_y(1)
               else
                  write (66, "(F23.15,A)") shear0_min_y(1), ","
               end if
            end do
            call middle_cord_web()
  !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") shear_min_y(i)
                  else
                     write (66, "(F23.15,A)") shear_min_y(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") sheare_min_y(1)
               else
                  write (66, "(F23.15,A)") sheare_min_y(1), ","
               end if
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
!!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
         if (xyz == 3) then
  !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") shear0_min_z(1)
               else
                  write (66, "(F23.15,A)") shear0_min_z(1), ","
               end if
            end do
            call middle_cord_web()
  !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") shear_min_z(i)
                  else
                     write (66, "(F23.15,A)") shear_min_z(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") sheare_min_z(1)
               else
                  write (66, "(F23.15,A)") sheare_min_z(1), ","
               end if
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if

   End if
!==========================<<<<<<<< min shear end

!$#######################################################################################

!$#######################################################################################
   IF (namepro == "pugh") THEN

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
      Do ii = 1, cutmesh
         open (10, file="3d_pugh.dat")
         read (10, *) pugh_max_x(ii), pugh_max_y(ii), pugh_max_z(ii), &
            pugh_min_x(ii), pugh_min_y(ii), pugh_min_z(ii), &
            pugh_neg_x(ii), pugh_neg_y(ii), pugh_neg_z(ii), &
            pugh_avg_x(ii), pugh_avg_y(ii), pugh_avg_z(ii)
      end do
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      !>> first and end point
      pugh0_max_x(1) = pugh_max_x(1); pugh0_max_y(1) = pugh_max_y(1); pugh0_max_z(1) = pugh_max_z(1)
      pughe_max_x(1) = pugh_max_x(cutmesh); pughe_max_y(1) = pugh_max_y(cutmesh); pughe_max_z(1) = pugh_max_z(cutmesh)

      pugh0_min_x(1) = pugh_min_x(1); pugh0_min_y(1) = pugh_min_y(1); pugh0_min_z(1) = pugh_min_z(1)
      pughe_min_x(1) = pugh_min_x(cutmesh); pughe_min_y(1) = pugh_min_y(cutmesh); pughe_min_z(1) = pugh_min_z(cutmesh)
      !<<
      !============================================================= start x-points
      !============================================================= start x-points
      if (type_pro == 'max') then
      If (xyz == 1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") pugh0_max_x(1)
            else
               write (66, "(F23.15,A)") pugh0_max_x(1), ","
            end if
         end do
         call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!! end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") pugh_max_x(i)
               else
                  write (66, "(F23.15,A)") pugh_max_x(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") pughe_max_x(1)
            else
               write (66, "(F23.15,A)") pughe_max_x(1), ","
            end if
         end do
    !!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
      if (xyz == 2) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") pugh0_max_y(1)
            else
               write (66, "(F23.15,A)") pugh0_max_y(1), ","
            end if
         end do
         call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") pugh_max_y(i)
               else
                  write (66, "(F23.15,A)") pugh_max_y(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") pughe_max_y(1)
            else
               write (66, "(F23.15,A)") pughe_max_y(1), ","
            end if
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
      if (xyz == 3) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") pugh0_max_z(1)
            else
               write (66, "(F23.15,A)") pugh0_max_z(1), ","
            end if
         end do
         call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") pugh_max_z(i)
               else
                  write (66, "(F23.15,A)") pugh_max_z(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") pughe_max_z(1)
            else
               write (66, "(F23.15,A)") pughe_max_z(1), ","
            end if
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if
      !==========================<<<<<<<< max pugh end
      !============================================================= start x-points
      !============================================================= start x-points
      if (type_pro == 'min') then

         If (xyz == 1) then
  !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") pugh0_min_x(1)
               else
                  write (66, "(F23.15,A)") pugh0_min_x(1), ","
               end if
            end do
            call middle_cord_web()
  !!!!!!!!!!!!!!!!!!!!!!!!! end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") pugh_min_x(i)
                  else
                     write (66, "(F23.15,A)") pugh_min_x(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") pughe_min_x(1)
               else
                  write (66, "(F23.15,A)") pughe_min_x(1), ","
               end if
            end do
  !!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
!!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
         if (xyz == 2) then
  !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") pugh0_min_y(1)
               else
                  write (66, "(F23.15,A)") pugh0_min_y(1), ","
               end if
            end do
            call middle_cord_web()
  !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") pugh_min_y(i)
                  else
                     write (66, "(F23.15,A)") pugh_min_y(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") pughe_min_y(1)
               else
                  write (66, "(F23.15,A)") pughe_min_y(1), ","
               end if
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
!!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
         if (xyz == 3) then
  !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") pugh0_min_z(1)
               else
                  write (66, "(F23.15,A)") pugh0_min_z(1), ","
               end if
            end do
            call middle_cord_web()
  !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") pugh_min_z(i)
                  else
                     write (66, "(F23.15,A)") pugh_min_z(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") pughe_min_z(1)
               else
                  write (66, "(F23.15,A)") pughe_min_z(1), ","
               end if
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if

   End if
!==========================<<<<<<<< min pugh end

!$#######################################################################################

   IF (namepro == "comp") THEN

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
      Do ii = 1, cutmesh
         open (10, file="3d_comp.dat")
         read (10, *) copm_max_x(ii), copm_max_y(ii), copm_max_z(ii), &
            copm_min_x(ii), copm_min_y(ii), copm_min_z(ii), &
            copm_neg_x(ii), copm_neg_y(ii), copm_neg_z(ii)

      end do
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      !>> first and end point
      copm0_max_x(1) = copm_max_x(1); copm0_max_y(1) = copm_max_y(1); copm0_max_z(1) = copm_max_z(1)
      copme_max_x(1) = copm_max_x(cutmesh); copme_max_y(1) = copm_max_y(cutmesh); copme_max_z(1) = copm_max_z(cutmesh)

      copm0_min_x(1) = copm_min_x(1); copm0_min_y(1) = copm_min_y(1); copm0_min_z(1) = copm_min_z(1)
      copme_min_x(1) = copm_min_x(cutmesh); copme_min_y(1) = copm_min_y(cutmesh); copme_min_z(1) = copm_min_z(cutmesh)

      copm0_neg_x(1) = copm_neg_x(1); copm0_neg_y(1) = copm_min_y(1); copm0_neg_z(1) = copm_neg_z(1)
      copme_neg_x(1) = copm_neg_x(cutmesh); copme_neg_y(1) = copm_neg_y(cutmesh); copme_neg_z(1) = copm_neg_z(cutmesh)
      !<<
      !============================================================= start x-points
      !============================================================= start x-points
      if (type_pro == 'max') then
      If (xyz == 1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") copm0_max_x(1)
            else
               write (66, "(F23.15,A)") copm0_max_x(1), ","
            end if
         end do
         call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!! end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") copm_max_x(i)
               else
                  write (66, "(F23.15,A)") copm_max_x(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") copme_max_x(1)
            else
               write (66, "(F23.15,A)") copme_max_x(1), ","
            end if
         end do
    !!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
      if (xyz == 2) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") copm0_max_y(1)
            else
               write (66, "(F23.15,A)") copm0_max_y(1), ","
            end if
         end do
         call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") copm_max_y(i)
               else
                  write (66, "(F23.15,A)") copm_max_y(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") copme_max_y(1)
            else
               write (66, "(F23.15,A)") copme_max_y(1), ","
            end if
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
      if (xyz == 3) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") copm0_max_z(1)
            else
               write (66, "(F23.15,A)") copm0_max_z(1), ","
            end if
         end do
         call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") copm_max_z(i)
               else
                  write (66, "(F23.15,A)") copm_max_z(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") copme_max_z(1)
            else
               write (66, "(F23.15,A)") copme_max_z(1), ","
            end if
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if
      !==========================<<<<<<<< max copm end
      !============================================================= start x-points
      !============================================================= start x-points
      if (type_pro == 'min') then

         If (xyz == 1) then
  !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") copm0_min_x(1)
               else
                  write (66, "(F23.15,A)") copm0_min_x(1), ","
               end if
            end do
            call middle_cord_web()
  !!!!!!!!!!!!!!!!!!!!!!!!! end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") copm_min_x(i)
                  else
                     write (66, "(F23.15,A)") copm_min_x(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") copme_min_x(1)
               else
                  write (66, "(F23.15,A)") copme_min_x(1), ","
               end if
            end do
  !!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
!!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
         if (xyz == 2) then
  !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") copm0_min_y(1)
               else
                  write (66, "(F23.15,A)") copm0_min_y(1), ","
               end if
            end do
            call middle_cord_web()
  !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") copm_min_y(i)
                  else
                     write (66, "(F23.15,A)") copm_min_y(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") copme_min_y(1)
               else
                  write (66, "(F23.15,A)") copme_min_y(1), ","
               end if
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
!!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
         if (xyz == 3) then
  !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") copm0_min_z(1)
               else
                  write (66, "(F23.15,A)") copm0_min_z(1), ","
               end if
            end do
            call middle_cord_web()
  !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") copm_min_z(i)
                  else
                     write (66, "(F23.15,A)") copm_min_z(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") copme_min_z(1)
               else
                  write (66, "(F23.15,A)") copme_min_z(1), ","
               end if
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if

      !============================================================= start x-points
      !============================================================= start x-points
      if (type_pro == 'neg') then

         If (xyz == 1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") copm0_neg_x(1)
               else
                  write (66, "(F23.15,A)") copm0_neg_x(1), ","
               end if
            end do
            call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!! end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") copm_neg_x(i)
                  else
                     write (66, "(F23.15,A)") copm_neg_x(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") copme_neg_x(1)
               else
                  write (66, "(F23.15,A)") copme_neg_x(1), ","
               end if
            end do
    !!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
         if (xyz == 2) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") copm0_neg_y(1)
               else
                  write (66, "(F23.15,A)") copm0_neg_y(1), ","
               end if
            end do
            call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") copm_neg_y(i)
                  else
                     write (66, "(F23.15,A)") copm_neg_y(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") copme_neg_y(1)
               else
                  write (66, "(F23.15,A)") copme_neg_y(1), ","
               end if
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
         if (xyz == 3) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") copm0_neg_z(1)
               else
                  write (66, "(F23.15,A)") copm0_neg_z(1), ","
               end if
            end do
            call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") copm_neg_z(i)
                  else
                     write (66, "(F23.15,A)") copm_neg_z(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") copme_neg_z(1)
               else
                  write (66, "(F23.15,A)") copme_neg_z(1), ","
               end if
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if
   end if
!$#######################################################################################
   IF (namepro == "poi") THEN

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
      Do ii = 1, cutmesh
         open (10, file="3d_poissons.dat")
         read (10, *) poi_max_x(ii), poi_max_y(ii), poi_max_z(ii), &
            poi_min_x(ii), poi_min_y(ii), poi_min_z(ii), &
            poi_neg_x(ii), poi_neg_y(ii), poi_neg_z(ii), &
            poi_max_avg_x(ii), poi_max_avg_y(ii), poi_max_avg_z(ii), &
            poi_min_avg_x(ii), poi_min_avg_y(ii), poi_min_avg_z(ii)
      end do
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      !>> first and end point
      poi0_max_x(1) = poi_max_x(1); poi0_max_y(1) = poi_max_y(1); poi0_max_z(1) = poi_max_z(1)
      poie_max_x(1) = poi_max_x(cutmesh); poie_max_y(1) = poi_max_y(cutmesh); poie_max_z(1) = poi_max_z(cutmesh)

      poi0_min_x(1) = poi_min_x(1); poi0_min_y(1) = poi_min_y(1); poi0_min_z(1) = poi_min_z(1)
      poie_min_x(1) = poi_min_x(cutmesh); poie_min_y(1) = poi_min_y(cutmesh); poie_min_z(1) = poi_min_z(cutmesh)

      poi0_neg_x(1) = poi_neg_x(1); poi0_neg_y(1) = poi_min_y(1); poi0_neg_z(1) = poi_neg_z(1)
      poie_neg_x(1) = poi_neg_x(cutmesh); poie_neg_y(1) = poi_neg_y(cutmesh); poie_neg_z(1) = poi_neg_z(cutmesh)
      !<<
      !============================================================= start x-points
      !============================================================= start x-points
      if (type_pro == 'max') then
      If (xyz == 1) then
      !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") poi0_max_x(1)
            else
               write (66, "(F23.15,A)") poi0_max_x(1), ","
            end if
         end do
         call middle_cord_web()
      !!!!!!!!!!!!!!!!!!!!!!!!! end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") poi_max_x(i)
               else
                  write (66, "(F23.15,A)") poi_max_x(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
      !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") poie_max_x(1)
            else
               write (66, "(F23.15,A)") poie_max_x(1), ","
            end if
         end do
      !!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
    !!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
      if (xyz == 2) then
      !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") poi0_max_y(1)
            else
               write (66, "(F23.15,A)") poi0_max_y(1), ","
            end if
         end do
         call middle_cord_web()
      !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") poi_max_y(i)
               else
                  write (66, "(F23.15,A)") poi_max_y(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
      !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") poie_max_y(1)
            else
               write (66, "(F23.15,A)") poie_max_y(1), ","
            end if
         end do
      !!!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
    !!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
      if (xyz == 3) then
      !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") poi0_max_z(1)
            else
               write (66, "(F23.15,A)") poi0_max_z(1), ","
            end if
         end do
         call middle_cord_web()
      !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") poi_max_z(i)
               else
                  write (66, "(F23.15,A)") poi_max_z(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
      !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") poie_max_z(1)
            else
               write (66, "(F23.15,A)") poie_max_z(1), ","
            end if
         end do
      !!!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if
      !==========================<<<<<<<< max poi end
      !============================================================= start x-points
      !============================================================= start x-points
      if (type_pro == 'min') then

         If (xyz == 1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") poi0_min_x(1)
               else
                  write (66, "(F23.15,A)") poi0_min_x(1), ","
               end if
            end do
            call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!! end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") poi_min_x(i)
                  else
                     write (66, "(F23.15,A)") poi_min_x(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") poie_min_x(1)
               else
                  write (66, "(F23.15,A)") poie_min_x(1), ","
               end if
            end do
    !!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
         if (xyz == 2) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") poi0_min_y(1)
               else
                  write (66, "(F23.15,A)") poi0_min_y(1), ","
               end if
            end do
            call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") poi_min_y(i)
                  else
                     write (66, "(F23.15,A)") poi_min_y(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") poie_min_y(1)
               else
                  write (66, "(F23.15,A)") poie_min_y(1), ","
               end if
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
         if (xyz == 3) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") poi0_min_z(1)
               else
                  write (66, "(F23.15,A)") poi0_min_z(1), ","
               end if
            end do
            call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") poi_min_z(i)
                  else
                     write (66, "(F23.15,A)") poi_min_z(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") poie_min_z(1)
               else
                  write (66, "(F23.15,A)") poie_min_z(1), ","
               end if
            end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if

      !============================================================= start x-points
      !============================================================= start x-points
      if (type_pro == 'neg') then

         If (xyz == 1) then
      !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") poi0_neg_x(1)
               else
                  write (66, "(F23.15,A)") poi0_neg_x(1), ","
               end if
            end do
            call middle_cord_web()
      !!!!!!!!!!!!!!!!!!!!!!!!! end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") poi_neg_x(i)
                  else
                     write (66, "(F23.15,A)") poi_neg_x(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
      !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") poie_neg_x(1)
               else
                  write (66, "(F23.15,A)") poie_neg_x(1), ","
               end if
            end do
      !!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
    !!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
         if (xyz == 2) then
      !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") poi0_neg_y(1)
               else
                  write (66, "(F23.15,A)") poi0_neg_y(1), ","
               end if
            end do
            call middle_cord_web()
      !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") poi_neg_y(i)
                  else
                     write (66, "(F23.15,A)") poi_neg_y(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
      !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") poie_neg_y(1)
               else
                  write (66, "(F23.15,A)") poie_neg_y(1), ","
               end if
            end do
      !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
    !!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
         if (xyz == 3) then
      !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") poi0_neg_z(1)
               else
                  write (66, "(F23.15,A)") poi0_neg_z(1), ","
               end if
            end do
            call middle_cord_web()
      !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") poi_neg_z(i)
                  else
                     write (66, "(F23.15,A)") poi_neg_z(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
      !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") poie_neg_z(1)
               else
                  write (66, "(F23.15,A)") poie_neg_z(1), ","
               end if
            end do
      !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if
   End if
!#=====================================================
   IF (namepro == "hard") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
      Do ii = 1, cutmesh
         open (10, file="3d_hardness.dat")
         read (10, *) hardmax_x(ii), hardmax_y(ii), hardmax_z(ii), &
            hardmin_x(ii), hardmin_y(ii), hardmin_z(ii)
      end do
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      !>> first and end point
      hard0_max_x(1) = hardmax_x(1); hard0_max_y(1) = hardmax_y(1); hard0_max_z(1) = hardmax_z(1)
      harde_max_x(1) = hardmax_x(cutmesh); harde_max_y(1) = hardmax_y(cutmesh); harde_max_z(1) = hardmax_z(cutmesh)

      hard0_min_x(1) = hardmax_x(1); hard0_max_y(1) = hardmin_y(1); hard0_min_z(1) = hardmin_z(1)
      harde_min_x(1) = hardmin_x(cutmesh); harde_min_y(1) = hardmin_y(cutmesh); harde_min_z(1) = hardmin_z(cutmesh)
      !<<
      !============================================================= start x-points
      if (type_pro == 'max') then
      If (xyz == 1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") hard0_max_x(1)
            else
               write (66, "(F23.15,A)") hard0_max_x(1), ","
            end if
         end do
         call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!! end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") hardmax_x(i)
               else
                  write (66, "(F23.15,A)") hardmax_x(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") harde_max_x(1)
            else
               write (66, "(F23.15,A)") harde_max_x(1), ","
            end if
         end do
    !!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
      if (xyz == 2) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") hard0_max_y(1)
            else
               write (66, "(F23.15,A)") hard0_max_y(1), ","
            end if
         end do
         call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") hardmax_y(i)
               else
                  write (66, "(F23.15,A)") hardmax_y(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") harde_max_y(1)
            else
               write (66, "(F23.15,A)") harde_max_y(1), ","
            end if
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
      if (xyz == 3) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") hard0_max_z(1)
            else
               write (66, "(F23.15,A)") hard0_max_z(1), ","
            end if
         end do
         call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
         start_new_reng = 2
         do k = 1, n_thetaf - 1
            end_new_reng = (k*(n_thetaf + 1)) + 1
            !write(*,*) start_new_reng,end_new_reng
            do i = start_new_reng, end_new_reng
               if (i == end_new_reng) then
                  write (66, "(F23.15)") hardmax_z(i)
               else
                  write (66, "(F23.15,A)") hardmax_z(i), ","
               end if
            end do
            call middle_cord_web()
            start_new_reng = end_new_reng + 1
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! end points
         do i = 1, n_phif + 1
            if (i == n_phif + 1) then
               write (66, "(F23.15)") harde_max_z(1)
            else
               write (66, "(F23.15,A)") harde_max_z(1), ","
            end if
         end do
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
      end if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if
      !==========================<<<<<<<< max shear end
      !============================================================= start x-points
      !============================================================= start x-points
      if (type_pro == 'min') then

         If (xyz == 1) then
  !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") hard0_min_x(1)
               else
                  write (66, "(F23.15,A)") hard0_min_x(1), ","
               end if
            end do
            call middle_cord_web()
  !!!!!!!!!!!!!!!!!!!!!!!!! end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") hardmin_x(i)
                  else
                     write (66, "(F23.15,A)") hardmin_x(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") harde_min_x(1)
               else
                  write (66, "(F23.15,A)") harde_min_x(1), ","
               end if
            end do
  !!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
!!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
         if (xyz == 2) then
  !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") hard0_min_y(1)
               else
                  write (66, "(F23.15,A)") hard0_min_y(1), ","
               end if
            end do
            call middle_cord_web()
  !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") hardmin_y(i)
                  else
                     write (66, "(F23.15,A)") hardmin_y(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") harde_min_y(1)
               else
                  write (66, "(F23.15,A)") harde_min_y(1), ","
               end if
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
!!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
         if (xyz == 3) then
  !!!!!!!!!!!!!!!!!!!!!!!!! first points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") hard0_min_z(1)
               else
                  write (66, "(F23.15,A)") hard0_min_z(1), ","
               end if
            end do
            call middle_cord_web()
  !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
            start_new_reng = 2
            do k = 1, n_thetaf - 1
               end_new_reng = (k*(n_thetaf + 1)) + 1
               !write(*,*) start_new_reng,end_new_reng
               do i = start_new_reng, end_new_reng
                  if (i == end_new_reng) then
                     write (66, "(F23.15)") hardmin_z(i)
                  else
                     write (66, "(F23.15,A)") hardmin_z(i), ","
                  end if
               end do
               call middle_cord_web()
               start_new_reng = end_new_reng + 1
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! end points
            do i = 1, n_phif + 1
               if (i == n_phif + 1) then
                  write (66, "(F23.15)") harde_min_z(1)
               else
                  write (66, "(F23.15,A)") harde_min_z(1), ","
               end if
            end do
  !!!!!!!!!!!!!!!!!!!!!!!!! finish
         end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
      end if
   End if
!$#######################################################################################
   close (10)
end SUBROUTINE
