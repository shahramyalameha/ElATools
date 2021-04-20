
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; set color for wrl files: dat2wrl program. 

SUBROUTINE color_sign_wrl(color1,color2,n_phif,n_thetaf,datapoints)
    INTEGER                             :: i,j, looper, n_phif2
    DOUBLE PRECISION, DIMENSION(100300) :: datapoints
    DOUBLE PRECISION                    :: r,g,b
    DOUBLE PRECISION, DIMENSION(4)      :: color1, color2
    DOUBLE PRECISION, PARAMETER                  :: pi=3.141592653589793238462D0

    WRITE(41,*) '  color Color {'
    WRITE(41,*) '      color [ '       
    looper=1
    DO i=0,n_thetaf-1
      theta=dble(i)/dble(n_thetaf)*pi
      n_phif2=n_phif;if (i==0 .OR. i==n_thetaf) n_phif2=0  
      DO j=0, n_phif2
         if (datapoints(looper).LE.0) then
           r=color2(1);g=color2(2);b=color2(3);
         else
           r=color1(1);g=color1(2);b=color1(3);
         ENDif    
         looper=looper+1
         WRITE(41,'(3F5.1,A1)',ADVANCE='NO') r,g,b,','
      ENDDO
    ENDDO
    i=n_thetaf
    j=0
    if (datapoints(looper).LE.0) then
      r=color2(1);g=color2(2);b=color2(3);
    else
      r=color1(1);g=color1(2);b=color1(3);
    ENDif    
    WRITE(41,'(3F5.1)') r,g,b
    WRITE(41,*) '      ]'
    WRITE(41,*) '    }'

          
  END SUBROUTINE color_sign_wrl


