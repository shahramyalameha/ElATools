
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
SUBROUTINE get_data_velog_plotly( namepro,xyz,n_phif,n_thetaf,cutmesh, type_pro)
  implicit NONE
  
  DOUBLE PRECISION, DIMENSION(193000)  ::  VVG_P0_x, VVG_P0_y, VVG_P0_z, & 
                                            VVG_Pe_x, VVG_Pe_y, VVG_Pe_z, & 
                                            VVG_P_x, VVG_P_y, VVG_P_z ,&
                                            VVG_Sf0_x, VVG_Sf0_y, VVG_Sf0_z,&
                                            VVG_Sfe_x, VVG_Sfe_y, VVG_Sfe_z,&
                                            VVG_Sf_x, VVG_Sf_y, VVG_Sf_z,&
                                            VVG_Ss0_x ,VVG_Ss0_y, VVG_Ss0_z,&
                                            VVG_Sse_x, VVG_Sse_y, VVG_Sse_z,&
                                            VVG_Ss_x, VVG_Ss_y, VVG_Ss_z                                          
                                         
  INTEGER,          DIMENSION(19030,4) :: mesh=0
  INTEGER                               :: n_phif, n_thetaf, num_mesh,i,ii,argl,cutmesh,k,&
                                           start_new_reng,end_new_reng,xyz
  character(len=10)                     :: val='',namepro ,type_pro ! type_pro : max, min, neg
  
!=============================================read mesh
    OPEN(69, file="MESH")
    read(69,*)n_phif,n_thetaf,cutmesh
    
    close(69)
!=============================================    
 !$#######################################################################################
IF (namepro=="gp") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,cutmesh
    open(10, file="3d_gp.dat")
    read(10,*) VVG_P_x(ii),VVG_P_y(ii),VVG_P_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  !>> first and end point 
  VVG_P0_x(1)=VVG_P_x(1) ;VVG_P0_y(1)=VVG_P_y(1);VVG_P0_z(1)= VVG_P_z(1)
  VVG_Pe_x(1)=VVG_P_x(cutmesh) ;VVG_Pe_y(1)=VVG_P_y(cutmesh);VVG_Pe_z(1)= VVG_P_z(cutmesh)
  !<< 
  !============================================================= start x-points 
  if(type_pro=='max') then

  If(xyz==1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_P0_x(1)
      else
        write(66,"(F23.15,A)") VVG_P0_x(1),","
      endif
    enddo
    call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!! end first points
    start_new_reng = 2
    do k=1,n_thetaf -1
      end_new_reng = (k*(n_thetaf+1)) + 1
      !write(*,*) start_new_reng,end_new_reng
      do i=start_new_reng, end_new_reng
        if (i==end_new_reng) then
          write(66,"(F23.15)") VVG_P_x(i)  
        else
          write(66,"(F23.15,A)") VVG_P_x(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Pe_x(1)
      else
        write(66,"(F23.15,A)") VVG_Pe_x(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!! finish
  endif 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
  if(xyz==2) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_P0_y(1)
      else
        write(66,"(F23.15,A)") VVG_P0_y(1),","
      endif
    enddo
    call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
    start_new_reng = 2
    do k=1,n_thetaf -1
      end_new_reng = (k*(n_thetaf+1)) + 1
      !write(*,*) start_new_reng,end_new_reng
      do i=start_new_reng, end_new_reng
        if (i==end_new_reng) then
          write(66,"(F23.15)") VVG_P_y(i)  
        else
          write(66,"(F23.15,A)") VVG_P_y(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Pe_y(1)
      else
        write(66,"(F23.15,A)") VVG_Pe_y(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
  endif  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
  if(xyz==3) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_P0_z(1)
      else
        write(66,"(F23.15,A)") VVG_P0_z(1),","
      endif
    enddo
    call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
    start_new_reng = 2
    do k=1,n_thetaf -1
      end_new_reng = (k*(n_thetaf+1)) + 1
      !write(*,*) start_new_reng,end_new_reng
      do i=start_new_reng, end_new_reng
        if (i==end_new_reng) then
          write(66,"(F23.15)") VVG_P_z(i)  
        else
          write(66,"(F23.15,A)") VVG_P_z(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Pe_z(1)
      else
        write(66,"(F23.15,A)") VVG_Pe_z(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
  endif 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
endif
Endif
!$#######################################################################################
IF (namepro=="gf") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,cutmesh
    open(10, file="3d_gf.dat")
    read(10,*) VVG_Sf_x(ii),VVG_Sf_y(ii),VVG_Sf_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  !>> first and end point 
  VVG_Sf0_x(1)=VVG_Sf_x(1) ;VVG_Sf0_y(1)=VVG_Sf_y(1);VVG_Sf0_z(1)= VVG_Sf_z(1)
  VVG_Sfe_x(1)=VVG_Sf_x(cutmesh) ;VVG_Sfe_y(1)=VVG_Sf_y(cutmesh);VVG_Sfe_z(1)= VVG_Sf_z(cutmesh)
  !<< 
  !============================================================= start x-points 
  if(type_pro=='max') then

  If(xyz==1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Sf0_x(1)
      else
        write(66,"(F23.15,A)") VVG_Sf0_x(1),","
      endif
    enddo
    call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!! end first points
    start_new_reng = 2
    do k=1,n_thetaf -1
      end_new_reng = (k*(n_thetaf+1)) + 1
      !write(*,*) start_new_reng,end_new_reng
      do i=start_new_reng, end_new_reng
        if (i==end_new_reng) then
          write(66,"(F23.15)") VVG_Sf_x(i)  
        else
          write(66,"(F23.15,A)") VVG_Sf_x(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Sfe_x(1)
      else
        write(66,"(F23.15,A)") VVG_Sfe_x(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!! finish
  endif 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
  if(xyz==2) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Sf0_y(1)
      else
        write(66,"(F23.15,A)") VVG_Sf0_y(1),","
      endif
    enddo
    call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
    start_new_reng = 2
    do k=1,n_thetaf -1
      end_new_reng = (k*(n_thetaf+1)) + 1
      !write(*,*) start_new_reng,end_new_reng
      do i=start_new_reng, end_new_reng
        if (i==end_new_reng) then
          write(66,"(F23.15)") VVG_Sf_y(i)  
        else
          write(66,"(F23.15,A)") VVG_Sf_y(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Sfe_y(1)
      else
        write(66,"(F23.15,A)") VVG_Sfe_y(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
  endif  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
  if(xyz==3) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Sf0_z(1)
      else
        write(66,"(F23.15,A)") VVG_Sf0_z(1),","
      endif
    enddo
    call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
    start_new_reng = 2
    do k=1,n_thetaf -1
      end_new_reng = (k*(n_thetaf+1)) + 1
      !write(*,*) start_new_reng,end_new_reng
      do i=start_new_reng, end_new_reng
        if (i==end_new_reng) then
          write(66,"(F23.15)") VVG_Sf_z(i)  
        else
          write(66,"(F23.15,A)") VVG_Sf_z(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Sfe_z(1)
      else
        write(66,"(F23.15,A)") VVG_Sfe_z(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
  endif 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
endif
Endif
!$#######################################################################################
!$#######################################################################################
IF (namepro=="gs") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,cutmesh
    open(10, file="3d_gs.dat")
    read(10,*) VVG_Ss_x(ii),VVG_Ss_y(ii),VVG_Ss_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  !>> first and end point 
  VVG_Ss0_x(1)=VVG_Ss_x(1) ;VVG_Ss0_y(1)=VVG_Ss_y(1);VVG_Ss0_z(1)= VVG_Ss_z(1)
  VVG_Sse_x(1)=VVG_Ss_x(cutmesh) ;VVG_Sse_y(1)=VVG_Ss_y(cutmesh);VVG_Sse_z(1)= VVG_Ss_z(cutmesh)
  !<< 
  !============================================================= start x-points 
  if(type_pro=='max') then

  If(xyz==1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Ss0_x(1)
      else
        write(66,"(F23.15,A)") VVG_Ss0_x(1),","
      endif
    enddo
    call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!! end first points
    start_new_reng = 2
    do k=1,n_thetaf -1
      end_new_reng = (k*(n_thetaf+1)) + 1
      !write(*,*) start_new_reng,end_new_reng
      do i=start_new_reng, end_new_reng
        if (i==end_new_reng) then
          write(66,"(F23.15)") VVG_Ss_x(i)  
        else
          write(66,"(F23.15,A)") VVG_Ss_x(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Sse_x(1)
      else
        write(66,"(F23.15,A)") VVG_Sse_x(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!! finish
  endif 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end x-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start y-points
  if(xyz==2) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Ss0_y(1)
      else
        write(66,"(F23.15,A)") VVG_Ss0_y(1),","
      endif
    enddo
    call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
    start_new_reng = 2
    do k=1,n_thetaf -1
      end_new_reng = (k*(n_thetaf+1)) + 1
      !write(*,*) start_new_reng,end_new_reng
      do i=start_new_reng, end_new_reng
        if (i==end_new_reng) then
          write(66,"(F23.15)") VVG_Ss_y(i)  
        else
          write(66,"(F23.15,A)") VVG_Ss_y(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Sse_y(1)
      else
        write(66,"(F23.15,A)") VVG_Sse_y(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
  endif  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end y-point
  !!!!!!!!!!!!!!!!!!!!!!!!!! start z-points
  if(xyz==3) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Ss0_z(1)
      else
        write(66,"(F23.15,A)") VVG_Ss0_z(1),","
      endif
    enddo
    call middle_cord_web()
    !!!!!!!!!!!!!!!!!!!!!!!!!1 end first points
    start_new_reng = 2
    do k=1,n_thetaf -1
      end_new_reng = (k*(n_thetaf+1)) + 1
      !write(*,*) start_new_reng,end_new_reng
      do i=start_new_reng, end_new_reng
        if (i==end_new_reng) then
          write(66,"(F23.15)") VVG_Ss_z(i)  
        else
          write(66,"(F23.15,A)") VVG_Ss_z(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVG_Sse_z(1)
      else
        write(66,"(F23.15,A)") VVG_Sse_z(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
  endif 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
endif
Endif

    close(10)
 end SUBROUTINE
