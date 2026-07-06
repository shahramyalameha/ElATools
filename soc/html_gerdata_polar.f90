
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
SUBROUTINE get_dataplotly_polar(namepro, rtheta, n_phif, n_thetaf, cutmesh, type_pro)
   implicit NONE
   DOUBLE PRECISION, DIMENSION(10000)  :: young_x, young_y, young_z, long_x , long_y, tran_x,tran_y, &
      young_x1, young_y1, young_z1, &
      bulk_x, bulk_y, bulk_z, &
      pugh_max_x, pugh_max_y, pugh_max_z, pugh_max_k, &
      shear_max_x, shear_max_y, shear_max_z, shear_max_k, &
      copm_max_x, copm_max_y, copm_max_z, &
      poi_max_x, poi_max_y, poi_max_z, poi_max_k, &
      hardmax_x, hardmax_y, hardmax_z, &
      hardmin_x, hardmin_y, hardmin_z, &
      VVP_i_x, VVP_P_y, VVP_SF_z, VVP_SS_k, &
      VVG_i_x, VVG_P_y, VVG_SF_z, VVG_SS_k, &
      VVF_i_x, VVF_P_y, VVF_SF_z, VVF_SS_k, &
      km_x, km_y
   REAL(8)                             :: temp_x, temp_y

   INTEGER, DIMENSION(190300, 4) :: mesh = 0
   INTEGER                               :: n_phif, n_thetaf, cutmesh, num_mesh, i, ii, argl, k, io, tem_i, &
                                            start_new_reng, end_new_reng, rtheta
   character(len=10)                     :: val = '', namepro, type_pro ! type_pro : max, min, neg

!=============================================READ mesh
   OPEN (69, file="MESH")
   READ (69, *) n_phif, n_thetaf, cutmesh

   CLOSE (69)
!=============================================
   IF (namepro == "young2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, n_phif + 1
         open (10, file="2dcut_young.dat")
         READ (10, *) young_x(ii), young_y(ii), young_z(ii), young_x1(ii), young_y1(ii), young_z1(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") young_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") young_y(i), ","
            END DO
         end if

      End if
   END IF
!$============================================================================================
   IF (namepro == "bulk2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, n_phif + 1
         open (10, file="2dcut_bulk.dat")
         READ (10, *) bulk_x(ii), bulk_y(ii), bulk_z(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") bulk_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") bulk_z(i), ","
            END DO
         end if

      End if
   END IF
!$============================================================================================
   IF (namepro == "pugh2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, n_phif + 1
         open (10, file="2dcut_pugh.dat")
         READ (10, *) pugh_max_x(ii), pugh_max_y(ii), pugh_max_z(ii), pugh_max_k(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") pugh_max_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") pugh_max_y(i), ","
            END DO
         end if
      End if

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'min') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") pugh_max_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") pugh_max_z(i), ","
            END DO
         end if
      End if
   END IF
!$============================================================================================
   IF (namepro == "shear2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, n_phif + 1
         open (10, file="2dcut_shear.dat")
         READ (10, *) shear_max_x(ii), shear_max_y(ii), shear_max_z(ii), shear_max_k(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") shear_max_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") shear_max_y(i), ","
            END DO
         end if
      End if

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'min') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") shear_max_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") shear_max_z(i), ","
            END DO
         end if
      End if

   END IF

!$============================================================================================
   IF (namepro == "com2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, n_phif + 1
         open (10, file="2dcut_comp.dat")
         READ (10, *) copm_max_x(ii), copm_max_y(ii), copm_max_z(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") copm_max_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") copm_max_y(i), ","
            END DO
         end if
      End if

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'neg') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") copm_max_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") copm_max_z(i), ","
            END DO
         end if
      End if
   END IF
!$============================================================================================
   IF (namepro == "poi2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, n_phif + 1
         open (10, file="2dcut_poisson.dat")
         READ (10, *) poi_max_x(ii), poi_max_y(ii), poi_max_z(ii), poi_max_k(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") poi_max_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") poi_max_y(i), ","
            END DO
         end if
      End if
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'min') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") poi_max_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") poi_max_z(i), ","
            END DO
         end if
      End if
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'neg') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") poi_max_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") poi_max_k(i), ","
            END DO
         end if
      End if
   END IF
!$============================================================================================
   IF (namepro == "hard2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, n_phif + 1
         open (10, file="2dcut_hardness.dat")
         READ (10, *) hardmax_x(ii), hardmax_y(ii), hardmax_z(ii)!,hardmin_x(ii),hardmin_y(ii),hardmin_z(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") hardmax_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") hardmax_y(i), ","
            END DO
         end if
      End if

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'min') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") hardmax_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") hardmax_z(i), ","
            END DO
         end if
      End if

   END IF
!$============================================================================================
   IF (namepro == "pp2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, 360 + 1!n_phif+1
         open (10, file="2dcut_pveloc.dat")
         READ (10, *) VVP_i_x(ii), VVP_P_y(ii), VVP_SF_z(ii), VVP_SS_k(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, 360 + 1!n_phif+1
               WRITE (66, "(F23.15,A)") VVP_i_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, 360 + 1!n_phif+1
               WRITE (66, "(F23.15,A)") VVP_P_y(i), ","
            END DO
         end if
      End if
   END IF
!$============================================================================================
   IF (namepro == "pf2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, 360 + 1!n_phif+1
         open (10, file="2dcut_pveloc.dat")
         READ (10, *) VVP_i_x(ii), VVP_P_y(ii), VVP_SF_z(ii), VVP_SS_k(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, 360 + 1 !n_phif+1
               WRITE (66, "(F23.15,A)") VVP_i_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, 360 + 1 !n_phif+1
               WRITE (66, "(F23.15,A)") VVP_SF_z(i), ","
            END DO
         end if
      End if
   END IF
!$============================================================================================
   IF (namepro == "ps2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, 360 + 1!n_phif+1
         open (10, file="2dcut_pveloc.dat")
         READ (10, *) VVP_i_x(ii), VVP_P_y(ii), VVP_SF_z(ii), VVP_SS_k(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, 360 + 1!n_phif+1
               WRITE (66, "(F23.15,A)") VVP_i_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, 360 + 1!n_phif+1
               WRITE (66, "(F23.15,A)") VVP_SS_k(i), ","
            END DO
         end if
      End if
   END IF
!$============================================================================================
   IF (namepro == "gp2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, 360 + 1!n_phif+1
         open (10, file="2dcut_gveloc.dat")
         READ (10, *) VVG_i_x(ii), VVG_P_y(ii), VVG_SF_z(ii), VVG_SS_k(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, 360 + 1!n_phif+1
               WRITE (66, "(F23.15,A)") VVG_i_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, 360 + 1!n_phif+1
               WRITE (66, "(F23.15,A)") VVG_P_y(i), ","
            END DO
         end if
      End if
   END IF
!$============================================================================================
   IF (namepro == "gf2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, 360 + 1!n_phif+1
         open (10, file="2dcut_gveloc.dat")
         READ (10, *) VVG_i_x(ii), VVG_P_y(ii), VVG_SF_z(ii), VVG_SS_k(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, 360 + 1 !n_phif+1
               WRITE (66, "(F23.15,A)") VVG_i_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, 360 + 1 !n_phif+1
               WRITE (66, "(F23.15,A)") VVG_SF_z(i), ","
            END DO
         end if
      End if
   END IF
!$============================================================================================
   IF (namepro == "gs2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, 360 + 1!n_phif+1
         open (10, file="2dcut_gveloc.dat")
         READ (10, *) VVG_i_x(ii), VVG_P_y(ii), VVG_SF_z(ii), VVG_SS_k(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, 360 + 1!n_phif+1
               WRITE (66, "(F23.15,A)") VVG_i_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, 360 + 1!n_phif+1
               WRITE (66, "(F23.15,A)") VVG_SS_k(i), ","
            END DO
         end if
      End if
   END IF

!$============================================================================================
   IF (namepro == "pfp2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, 360 + 1!n_phif+1
         open (10, file="2dcut_pfaveloc.dat")
         READ (10, *) VVF_i_x(ii), VVF_P_y(ii), VVF_SF_z(ii), VVF_SS_k(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, 360 + 1!n_phif+1
               WRITE (66, "(F23.15,A)") VVG_i_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, 360 + 1!n_phif+1
               WRITE (66, "(F23.15,A)") VVG_P_y(i), ","
            END DO
         end if
      End if
   END IF
!$============================================================================================
   IF (namepro == "pff2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, 360 + 1!n_phif+1
         open (10, file="2dcut_pfaveloc.dat")
         READ (10, *) VVG_i_x(ii), VVF_P_y(ii), VVF_SF_z(ii), VVF_SS_k(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, 360 + 1 !n_phif+1
               WRITE (66, "(F23.15,A)") VVF_i_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, 360 + 1 !n_phif+1
               WRITE (66, "(F23.15,A)") VVF_SF_z(i), ","
            END DO
         end if
      End if
   END IF
!$============================================================================================
   IF (namepro == "pfs2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, 360 + 1!n_phif+1
         open (10, file="2dcut_pfaveloc.dat")
         READ (10, *) VVF_i_x(ii), VVF_P_y(ii), VVF_SF_z(ii), VVF_SS_k(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, 360 + 1!n_phif+1
               WRITE (66, "(F23.15,A)") VVF_i_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, 360 + 1!n_phif+1
               WRITE (66, "(F23.15,A)") VVF_SS_k(i), ","
            END DO
         end if
      End if
   END IF
!$============================================================================================
   IF (namepro == "km2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
      DO ii = 1, n_phif + 1
         open (10, file="2dcut_km.dat")
         READ (10, *) km_x(ii), km_y(ii)

      END DO
      CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
      If (type_pro == 'max') THEN
         If (rtheta == 1) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") km_x(i), ","
            END DO
         end if
!===========================================
         If (rtheta == 2) THEN
            DO i = 1, n_phif + 1
               WRITE (66, "(F23.15,A)") km_y(i), ","
            END DO
         end if
      End if
   END IF
!$============================================================================================
end SUBROUTINE

!$============================================================================================
SUBROUTINE get_dataplotly_2dpolar(namepro, rtheta, type_pro)

   implicit NONE
   DOUBLE PRECISION, DIMENSION(10000)  :: young_x, young_y, young_z, long_x, long_y, tran_x, tran_y,&
      young_x1, young_y1, young_z1, &
      bulk_x, bulk_y, bulk_z, &
      pugh_max_x, pugh_max_y, pugh_max_z, pugh_max_k, &
      shear_max_x, shear_max_y, shear_max_z, shear_max_k, &
      copm_max_x, copm_max_y, copm_max_z, &
      poi_max_x, poi_max_y, poi_max_z, poi_max_k, &
      hardmax_x, hardmax_y, hardmax_z, &
      hardmin_x, hardmin_y, hardmin_z, &
      VVP_i_x, VVP_P_y, VVP_SF_z, VVP_SS_k, &
      VVG_i_x, VVG_P_y, VVG_SF_z, VVG_SS_k, &
      VVF_i_x, VVF_P_y, VVF_SF_z, VVF_SS_k, &
      km_x, km_y
   REAL(8)                             :: temp_x, temp_y

   INTEGER, DIMENSION(190300, 4)    :: mesh = 0
   INTEGER                             :: n_phif, n_thetaf, cutmesh, num_mesh, i, ii, argl, k, io, tem_i, &
                                          start_new_reng, end_new_reng, rtheta
   character(len=10)                   :: val = '', namepro, type_pro ! type_pro : max, min, neg
 
   IF (namepro == "2dlong" .OR. namepro == "2dtran") THEN
      tem_i = 0
      OPEN (12, file="longitudinal_2d_sys.dat")
      DO

         READ (12, *, iostat=io) temp_x, temp_y
         ! write(*,*) temp_x , temp_y
         tem_i = tem_i + 1
         if (io < 0) exit
      END DO
      Close (12)  
      !Write(*,*)tem_i
!---------------------------------------------------
      IF (namepro == "2dlong") THEN

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
         DO ii = 1, tem_i - 1
            open (10, file="longitudinal_2d_sys.dat")
            READ (10, *) long_x(ii), long_y(ii)
            ! write(*,*)young_x(ii),young_y(ii)
         END DO
         CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
         If (type_pro == 'max') THEN
            If (rtheta == 1) THEN
               DO i = 1, tem_i - 1
                  WRITE (66, "(F23.15,A)") long_x(i), ","

               END DO
            end if
!===========================================
            If (rtheta == 2) THEN
               DO i = 1, tem_i - 1
                  WRITE (66, "(F23.15,A)") long_y(i), ","
               END DO
            end if

         End if
      End if
!===========================================   
!---------------------------------------------------
      IF (namepro == "2dtran") THEN

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
         DO ii = 1, tem_i - 1
            open (10, file="transverse_2d_sys.dat")
            READ (10, *) tran_x(ii), tran_y(ii)
            ! write(*,*)young_x(ii),young_y(ii)
         END DO
         CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
         If (type_pro == 'max') THEN
            If (rtheta == 1) THEN
               DO i = 1, tem_i - 1
                  WRITE (66, "(F23.15,A)") tran_x(i), ","

               END DO
            end if
!===========================================
            If (rtheta == 2) THEN
               DO i = 1, tem_i - 1
                  WRITE (66, "(F23.15,A)") tran_y(i), ","
               END DO
            end if

         End if
      End if
!==========================================  
   ENDIF
 



   IF (namepro == "2dpoi" .OR. namepro == "2dshear" .OR. namepro == "2dyoung" .OR. &
       namepro == "2dyou" .OR. namepro == "2dshe"   ) THEN
      tem_i = 0
      OPEN (12, file="young_2d_sys.dat")
      DO

         READ (12, *, iostat=io) temp_x, temp_y
         ! write(*,*) temp_x , temp_y
         tem_i = tem_i + 1
         if (io < 0) exit
      END DO
      Close (12)
      ! write(*,*) tem_i
!---------------------------------------------------
      IF (namepro == "2dyoung") THEN

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
         DO ii = 1, tem_i - 1
            open (10, file="young_2d_sys.dat")
            READ (10, *) young_x(ii), young_y(ii)
            ! write(*,*)young_x(ii),young_y(ii)
         END DO
         CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
         If (type_pro == 'max') THEN
            If (rtheta == 1) THEN
               DO i = 1, tem_i - 1
                  WRITE (66, "(F23.15,A)") young_x(i), ","

               END DO
            end if
!===========================================
            If (rtheta == 2) THEN
               DO i = 1, tem_i - 1
                  WRITE (66, "(F23.15,A)") young_y(i), ","
               END DO
            end if

         End if
      End if
      
!===========================================
      IF (namepro == "2dshear") THEN
         DO ii = 1, tem_i - 1
            open (10, file="shear_2d_sys.dat")
            READ (10, *) shear_max_x(ii), shear_max_y(ii)
            ! write(*,*)young_x(ii),young_y(ii)
         END DO
         CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
         If (type_pro == 'max') THEN
            If (rtheta == 1) THEN
               DO i = 1, tem_i - 1
                  WRITE (66, "(F23.15,A)") shear_max_x(i), ","
                  !write (*, *) shear_max_x(i)
               END DO
            end if
!===========================================
            If (rtheta == 2) THEN
               DO i = 1, tem_i - 1
                  WRITE (66, "(F23.15,A)") shear_max_y(i), ","
               END DO
            end if

         End if
      END IF
!===========================================
      IF (namepro == "2dpoi") THEN
         DO ii = 1, tem_i - 1
            open (10, file="poisson_2d_sys.dat")
            READ (10, *) poi_max_x(ii), poi_max_y(ii), poi_max_z(ii), poi_max_k(ii)
            ! write(*,*)young_x(ii),young_y(ii)
         END DO
         CLOSE (10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
         If (type_pro == 'max') THEN
            If (rtheta == 1) THEN
               DO i = 1, tem_i - 1
                  WRITE (66, "(F23.15,A)") poi_max_x(i), ","
 
               END DO
            end if
!===========================================
            If (rtheta == 2) THEN
               DO i = 1, tem_i - 1
                  WRITE (66, "(F23.15,A)") poi_max_z(i), ","
               END DO
            end if

         End if
!===========================================         
         If (type_pro == 'neg') THEN
            If (rtheta == 1) THEN
               DO i = 1, tem_i - 1
                  WRITE (66, "(F23.15,A)") poi_max_x(i), ","
 
               END DO
            end if
!===========================================
            If (rtheta == 2) THEN
               DO i = 1, tem_i - 1
                  WRITE (66, "(F23.15,A)") abs(poi_max_k(i)), ","
               END DO
            end if

         End if         
      END IF
!===========================================!===========================================

!    
!===========================================
   END IF

!$============================================================================================
end SUBROUTINE

