
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; calculate mesh data for wrl files: dat2wrl program. 

SUBROUTINE mesh_datapoints_wrl(n_phif,n_thetaf,datapoints)
    INTEGER                             :: i,j, looper, n_phif2
    DOUBLE PRECISION                    :: theta,phi
    DOUBLE PRECISION, DIMENSION(100300) :: datapoints
    DOUBLE PRECISION, DIMENSION(3)      :: color_back=1D0,color_front=0D0,k
    INTEGER                             :: ii=0
    INTEGER                             :: n_phif, n_thetaf

        
    WRITE(41,*) '  coord Coordinate {'
    WRITE(41,*) '      point [ '
         
    looper=1
    DO i=0,n_thetaf-1
      theta=dble(i)/dble(n_thetaf)*3.141592653589793238462D0
      n_phif2=n_phif ; if (i==0 .OR. i==n_thetaf) n_phif2=0D0  

      DO j=0, n_phif2
         phi=dble(j)/dble(n_phif)*2D0*3.141592653589793238462D0
         k(1)=SIN(theta)*COS(phi)*ABS(datapoints(looper))
         k(2)=SIN(theta)*SIN(phi)*ABS(datapoints(looper))
         k(3)=COS(theta)*ABS(datapoints(looper))
         looper=looper+1
         WRITE(41,'(3F10.3,A1)',ADVANCE='NO') k(1),k(2),k(3),','
      ENDDO
    ENDDO
    i=n_thetaf
    j=0
    theta=dble(i)/dble(n_thetaf)*3.141592653589793238462D0
    phi=dble(j)/dble(n_phif)*2D0*3.141592653589793238462D0
    k(1)=SIN(theta)*COS(phi)*ABS(datapoints(looper))
    k(2)=SIN(theta)*SIN(phi)*ABS(datapoints(looper))
    k(3)=COS(theta)*ABS(datapoints(looper))
    WRITE(41,'(3F10.3)') k(1),k(2),k(3)
    WRITE(41,*) '      ]'
    WRITE(41,*) '    }'
    WRITE(41,*) '  }'
          
  END SUBROUTINE mesh_datapoints_wrl
