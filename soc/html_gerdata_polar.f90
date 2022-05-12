
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
SUBROUTINE get_dataplotly_polar( namepro,rtheta, n_phif,n_thetaf,cutmesh,type_pro)
  implicit NONE
  DOUBLE PRECISION, DIMENSION(10000)  :: young_x,young_y,young_z,&
                                           young_x1,young_y1,young_z1,&
                                           bulk_x,bulk_y,bulk_z,&
                                           pugh_max_x ,pugh_max_y ,pugh_max_z, pugh_max_k ,&
                                           shear_max_x ,shear_max_y ,shear_max_z ,shear_max_k,&
                                           copm_max_x ,copm_max_y ,copm_max_z  ,&
                                           poi_max_x  ,poi_max_y  ,poi_max_z,poi_max_k,&
                                           hard_x,hard_y,hard_z,&
                                           VVP_i_x, VVP_P_y, VVP_SF_z,VVP_SS_k,&
                                           VVG_i_x, VVG_P_y, VVG_SF_z,VVG_SS_k,&
                                           VVF_i_x, VVF_P_y, VVF_SF_z,VVF_SS_k,&
                                           km_x, km_y ,temp_x, temp_y
 
  INTEGER,          DIMENSION(190300,4) :: mesh=0
  INTEGER                               :: n_phif, n_thetaf,cutmesh ,num_mesh,i,ii,argl,k,io,tem_i,&
                                           start_new_reng,end_new_reng,rtheta
  character(len=10)                     :: val='',namepro ,type_pro ! type_pro : max, min, neg
  
  
IF (namepro=="2dpoi" .OR. namepro=="2dshear" .OR. namepro=="2dyoung" .OR. namepro=="2dyou" .OR. namepro=="2dshe") THEN
   tem_i=0
       OPEN(12, file="young_2d_sys.dat")
       DO
        tem_i=tem_i + 1
        READ(12,*, iostat = io ) temp_x, temp_y
        if (io < 0) exit
       ENDDO
       Close(12)
!---------------------------------------------------       
 IF (namepro=="2dyoung") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,tem_i+1
    open(10, file="young_2d_sys.dat")
    READ(10,*) young_x(ii),young_y(ii) 
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  If(type_pro=='max') THEN
   If(rtheta==1) THEN
    DO i=1,tem_i+1
        WRITE(66,"(F23.15,A)") young_x(i),","
    ENDDO
   endif
  Endif
 ENDIF 
!===========================================      
       
ENDIF
 
!=============================================READ mesh
    OPEN(69, file="MESH")
    READ(69,*)n_phif,n_thetaf,cutmesh
    
    CLOSE(69)
!============================================= 
IF (namepro=="young2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,n_phif+1
    open(10, file="2dcut_young.dat")
    READ(10,*) young_x(ii),young_y(ii),young_z(ii),young_x1(ii),young_y1(ii),young_z1(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") young_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") young_y(i),","
    ENDDO
  endif
    
    
 Endif
ENDIF
!$============================================================================================
IF (namepro=="bulk2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,n_phif+1
    open(10, file="2dcut_bulk.dat")
    READ(10,*) bulk_x(ii),bulk_y(ii),bulk_z(ii) 
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") bulk_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") bulk_z(i),","
    ENDDO
  endif
    
    
 Endif
ENDIF
!$============================================================================================
IF (namepro=="pugh2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,n_phif+1
    open(10, file="2dcut_pugh.dat")
    READ(10,*) pugh_max_x(ii), pugh_max_y(ii) ,pugh_max_z(ii), pugh_max_k(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") pugh_max_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") pugh_max_y(i),","
    ENDDO
  endif
 Endif
 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='min') THEN
  If(rtheta==1) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") pugh_max_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") pugh_max_z(i),","
    ENDDO
  endif
 Endif
 ENDIF
!$============================================================================================
IF (namepro=="shear2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,n_phif+1
    open(10, file="2dcut_shear.dat")
    READ(10,*) shear_max_x(ii), shear_max_y(ii) ,shear_max_z(ii), shear_max_k(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") shear_max_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") shear_max_y(i),","
    ENDDO
  endif
 Endif
 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='min') THEN
  If(rtheta==1) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") shear_max_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") shear_max_z(i),","
    ENDDO
  endif
 Endif  
ENDIF

!$============================================================================================
IF (namepro=="com2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,n_phif+1
    open(10, file="2dcut_comp.dat")
    READ(10,*) copm_max_x(ii) ,copm_max_y(ii) ,copm_max_z(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") copm_max_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") copm_max_y(i),","
    ENDDO
  endif
 Endif
 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='neg') THEN
  If(rtheta==1) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") copm_max_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") copm_max_z(i),","
    ENDDO
  endif
 Endif  
ENDIF
!$============================================================================================
IF (namepro=="poi2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,n_phif+1
    open(10, file="2dcut_poisson.dat")
    READ(10,*) poi_max_x(ii)  ,poi_max_y(ii)  ,poi_max_z(ii), poi_max_k(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") poi_max_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") poi_max_y(i),","
    ENDDO
  endif
 Endif
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='min') THEN
  If(rtheta==1) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") poi_max_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") poi_max_z(i),","
    ENDDO
  endif
 Endif
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='neg') THEN
  If(rtheta==1) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") poi_max_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") poi_max_k(i),","
    ENDDO
  endif
 Endif  
ENDIF
!$============================================================================================
IF (namepro=="hard2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,n_phif+1
    open(10, file="2dcut_hardness.dat")
    READ(10,*) hard_x(ii),hard_y(ii),hard_z(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") hard_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,n_phif+1
        WRITE(66,"(F23.15,A)") hard_y(i),","
    ENDDO
  endif
 Endif
ENDIF
!$============================================================================================
IF (namepro=="pp2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,360+1!n_phif+1
    open(10, file="2dcut_pveloc.dat")
    READ(10,*) VVP_i_x(ii), VVP_P_y(ii), VVP_SF_z(ii),VVP_SS_k(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,360+1!n_phif+1
        WRITE(66,"(F23.15,A)") VVP_i_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,360+1!n_phif+1
        WRITE(66,"(F23.15,A)") VVP_P_y(i),","
    ENDDO
  endif
 Endif
ENDIF
!$============================================================================================
IF (namepro=="pf2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,360+1!n_phif+1
    open(10, file="2dcut_pveloc.dat")
    READ(10,*) VVP_i_x(ii), VVP_P_y(ii), VVP_SF_z(ii),VVP_SS_k(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,360+1 !n_phif+1
        WRITE(66,"(F23.15,A)") VVP_i_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,360+1 !n_phif+1
        WRITE(66,"(F23.15,A)") VVP_SF_z(i),","
    ENDDO
  endif
 Endif
ENDIF
!$============================================================================================
IF (namepro=="ps2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,360+1!n_phif+1
    open(10, file="2dcut_pveloc.dat")
    READ(10,*) VVP_i_x(ii), VVP_P_y(ii), VVP_SF_z(ii),VVP_SS_k(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,360+1!n_phif+1
        WRITE(66,"(F23.15,A)") VVP_i_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,360+1!n_phif+1
        WRITE(66,"(F23.15,A)") VVP_SS_k(i),","
    ENDDO
  endif
 Endif
ENDIF
!$============================================================================================
IF (namepro=="gp2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,360+1!n_phif+1
    open(10, file="2dcut_gveloc.dat")
    READ(10,*) VVG_i_x(ii), VVG_P_y(ii), VVG_SF_z(ii),VVG_SS_k(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,360+1!n_phif+1
        WRITE(66,"(F23.15,A)") VVG_i_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,360+1!n_phif+1
        WRITE(66,"(F23.15,A)") VVG_P_y(i),","
    ENDDO
  endif
 Endif
ENDIF
!$============================================================================================
IF (namepro=="gf2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,360+1!n_phif+1
    open(10, file="2dcut_gveloc.dat")
    READ(10,*) VVG_i_x(ii), VVG_P_y(ii), VVG_SF_z(ii),VVG_SS_k(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,360+1 !n_phif+1
        WRITE(66,"(F23.15,A)") VVG_i_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,360+1 !n_phif+1
        WRITE(66,"(F23.15,A)") VVG_SF_z(i),","
    ENDDO
  endif
 Endif
ENDIF
!$============================================================================================
IF (namepro=="gs2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,360+1!n_phif+1
    open(10, file="2dcut_gveloc.dat")
    READ(10,*) VVG_i_x(ii), VVG_P_y(ii), VVG_SF_z(ii),VVG_SS_k(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,360+1!n_phif+1
        WRITE(66,"(F23.15,A)") VVG_i_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,360+1!n_phif+1
        WRITE(66,"(F23.15,A)") VVG_SS_k(i),","
    ENDDO
  endif
 Endif
ENDIF


!$============================================================================================
IF (namepro=="pfp2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,360+1!n_phif+1
    open(10, file="2dcut_pfaveloc.dat")
    READ(10,*) VVF_i_x(ii), VVF_P_y(ii), VVF_SF_z(ii),VVF_SS_k(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,360+1!n_phif+1
        WRITE(66,"(F23.15,A)") VVG_i_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,360+1!n_phif+1
        WRITE(66,"(F23.15,A)") VVG_P_y(i),","
    ENDDO
  endif
 Endif
ENDIF
!$============================================================================================
IF (namepro=="pff2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,360+1!n_phif+1
    open(10, file="2dcut_pfaveloc.dat")
    READ(10,*) VVG_i_x(ii), VVF_P_y(ii), VVF_SF_z(ii),VVF_SS_k(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,360+1 !n_phif+1
        WRITE(66,"(F23.15,A)") VVF_i_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,360+1 !n_phif+1
        WRITE(66,"(F23.15,A)") VVF_SF_z(i),","
    ENDDO
  endif
 Endif
ENDIF
!$============================================================================================
IF (namepro=="pfs2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1,360+1!n_phif+1
    open(10, file="2dcut_pfaveloc.dat")
    READ(10,*) VVF_i_x(ii), VVF_P_y(ii), VVF_SF_z(ii),VVF_SS_k(ii)
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1,360+1!n_phif+1
        WRITE(66,"(F23.15,A)") VVF_i_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1,360+1!n_phif+1
        WRITE(66,"(F23.15,A)") VVF_SS_k(i),","
    ENDDO
  endif
 Endif
ENDIF
!$============================================================================================
IF (namepro=="km2d") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-READer
  DO ii=1, n_phif+1
    open(10, file="2dcut_km.dat")
    READ(10,*) km_x(ii), km_y(ii) 
     
  ENDDO
   CLOSE(10)
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
 If(type_pro=='max') THEN
  If(rtheta==1) THEN
    DO i=1, n_phif+1
        WRITE(66,"(F23.15,A)") km_x(i),","
    ENDDO
  endif
!=========================================== 
  If(rtheta==2) THEN
    DO i=1, n_phif+1
        WRITE(66,"(F23.15,A)") km_y(i),","
    ENDDO
  endif
 Endif
ENDIF
!$============================================================================================
 end SUBROUTINE
