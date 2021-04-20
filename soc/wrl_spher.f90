
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; set spheroid format for wrl files: dat2wrl program. 

SUBROUTINE spheroid_wrl(n_phif,n_thetaf,mesh)
    INTEGER                       :: i,j,POCO, looper
    INTEGER, DIMENSION(100300,4)  :: mesh
    INTEGER                       :: n_phif, n_thetaf
    looper=1
    WRITE(41,*) '    coordIndex ['
    DO j=0, n_phif
      WRITE(41,'(4(I5,A1))',ADVANCE='NO') (mesh(looper,POCO),',', POCO=1,3), -1,','
      looper=looper+1
    ENDDO
    WRITE(41,*)
    DO i=2,n_thetaf-1
      DO j=0, n_phif
        WRITE(41,'(5(I5,A1))',ADVANCE='NO') (mesh(looper,POCO),',', POCO=1,4), -1,','
        looper=looper+1        
      ENDDO
      WRITE(41,*)
    ENDDO
    DO j=0, n_phif-1
      WRITE(41,'(4(I5,A1))',ADVANCE='NO') (mesh(looper,POCO),',', POCO=1,3), -1,','
      looper=looper+1
    ENDDO             
    j=n_phif
    WRITE(41,'(3(I5,A1))',ADVANCE='NO') (mesh(looper,POCO),',', POCO=1,3)
    WRITE(41,'(I5)') -1
    WRITE(41,*) "]"
 
  END SUBROUTINE spheroid_wrl
