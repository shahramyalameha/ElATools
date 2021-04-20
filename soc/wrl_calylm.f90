
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; set mesh for wrl files: dat2wrl program.

SUBROUTINE cal_YLM(num_mesh,n_phif,n_thetaf,mesh)
    INTEGER :: i,j, looper
    INTEGER :: n_phif, n_thetaf, num_mesh
    INTEGER, DIMENSION(100300,4) :: mesh        
    looper=1
    DO j=0, n_phif-1
      mesh(looper,1)=0
      mesh(looper,2)=j+1
      mesh(looper,3)=j+2
!      WRITE (*,*) mesh(looper,1),mesh(looper,2),mesh(looper,3)
      looper=looper+1
    ENDDO
      mesh(looper,1)=0
      mesh(looper,2)=n_phif+1
      mesh(looper,3)=1
      looper=looper+1
    DO i=2,n_thetaf-1
      DO j=0, n_phif-1
        mesh(looper,1)=(i-2)*(n_phif+1)+j+1
        mesh(looper,2)=(i-1)*(n_phif+1)+j+1
        mesh(looper,3)=(i-1)*(n_phif+1)+j+2
        mesh(looper,4)=(i-2)*(n_phif+1)+j+2
        looper=looper+1
      ENDDO
      mesh(looper,1)=(i-1)*(n_phif+1)
      mesh(looper,2)=i*(n_phif+1)
      mesh(looper,3)=(i-1)*(n_phif+1)+1
      mesh(looper,4)=(i-2)*(n_phif+1)+1
      looper=looper+1
    ENDDO
    DO j=0, n_phif-1
      mesh(looper,1)=(n_thetaf-2)*(n_phif+1)+j+1
      mesh(looper,2)=(n_thetaf-1)*(n_phif+1)+1
      mesh(looper,3)=(n_thetaf-2)*(n_phif+1)+j+2
      looper=looper+1
    ENDDO
    mesh(looper,1)=(n_thetaf-1)*(n_phif+1)
    mesh(looper,2)=(n_thetaf-1)*(n_phif+1)+1
    mesh(looper,3)=(n_thetaf-2)*(n_phif+1)+1
    num_mesh=looper 

  END  SUBROUTINE
