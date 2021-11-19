
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
SUBROUTINE get_data_velop_plotly( namepro,xyz,n_phif,n_thetaf,cutmesh, type_pro)
  implicit NONE
  DOUBLE PRECISION, DIMENSION(199999)  ::   VVP_P0_x, VVP_P0_y, VVP_P0_z, & 
                                            VVP_Pe_x, VVP_Pe_y, VVP_Pe_z, & 
                                            VVP_P_x, VVP_P_y, VVP_P_z ,&
                                            VVP_Sf0_x,VVP_Sf0_y,VVP_Sf0_z,&
                                            VVP_Sfe_x,VVP_Sfe_y,VVP_Sfe_z,&
                                            VVP_Sf_x,VVP_Sf_y,VVP_Sf_z,&
                                            VVP_Ss0_x,VVP_Ss0_y,VVP_Ss0_z,&
                                            VVP_Sse_x,VVP_Sse_y,VVP_Sse_z,&
                                            VVP_Ss_x,VVP_Ss_y,VVP_Ss_z 

                                             
 DOUBLE PRECISION, DIMENSION(199999)   ::  VV_P_FA0_x,VV_P_FA0_y,VV_P_FA0_z,    &
                                            VV_P_FAe_x,VV_P_FAe_y,VV_P_FAe_z,   &
                                            VV_P_FA_x,VV_P_FA_y,VV_P_FA_z ,     &
                                            VV_Ss_FA0_x,VV_Ss_FA0_y,VV_Ss_FA0_z,&
                                            VV_Ss_FAe_x,VV_Ss_FAe_y,VV_Ss_FAe_z,&
                                            VV_Ss_FA_x,VV_Ss_FA_y,VV_Ss_FA_z ,  &
                                            VV_Sf_FA0_x,VV_Sf_FA0_y,VV_Sf_FA0_z,&
                                            VV_Sf_FAe_x,VV_Sf_FAe_y,VV_Sf_FAe_z,&
                                            VV_Sf_FA_x,VV_Sf_FA_y,VV_Sf_FA_z
           
                                          
                                         
  INTEGER,          DIMENSION(1900,4) :: mesh=0
  INTEGER                               :: n_phif, n_thetaf, num_mesh,i,ii,argl,cutmesh,k,&
                                           start_new_reng,end_new_reng,xyz
  character(len=10)                     :: val='',namepro ,type_pro ! type_pro : max, min, neg
  
!=============================================read mesh
    OPEN(69, file="MESH")
    read(69,*)n_phif,n_thetaf,cutmesh
    
    close(69)
!=============================================    
 
!$#######################################################################################
IF (namepro=="pp") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,cutmesh
    open(10, file="3d_pp.dat")
    read(10,*) VVP_P_x(ii),VVP_P_y(ii),VVP_P_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  !>> first and end point 
  VVP_P0_x(1)=VVP_P_x(1) ;VVP_P0_y(1)=VVP_P_y(1);VVP_P0_z(1)= VVP_P_z(1)
  VVP_Pe_x(1)=VVP_P_x(cutmesh) ;VVP_Pe_y(1)=VVP_P_y(cutmesh);VVP_Pe_z(1)= VVP_P_z(cutmesh)
  !<< 
  !============================================================= start x-points 
  if(type_pro=='max') then

  If(xyz==1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVP_P0_x(1)
      else
        write(66,"(F23.15,A)") VVP_P0_x(1),","
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
          write(66,"(F23.15)") VVP_P_x(i)  
        else
          write(66,"(F23.15,A)") VVP_P_x(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVP_Pe_x(1)
      else
        write(66,"(F23.15,A)") VVP_Pe_x(1),","
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
        write(66,"(F23.15)") VVP_P0_y(1)
      else
        write(66,"(F23.15,A)") VVP_P0_y(1),","
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
          write(66,"(F23.15)") VVP_P_y(i)  
        else
          write(66,"(F23.15,A)") VVP_P_y(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVP_Pe_y(1)
      else
        write(66,"(F23.15,A)") VVP_Pe_y(1),","
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
        write(66,"(F23.15)") VVP_P0_z(1)
      else
        write(66,"(F23.15,A)") VVP_P0_z(1),","
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
          write(66,"(F23.15)") VVP_P_z(i)  
        else
          write(66,"(F23.15,A)") VVP_P_z(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVP_Pe_z(1)
      else
        write(66,"(F23.15,A)") VVP_Pe_z(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
  endif 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
endif
Endif
!$#######################################################################################
IF (namepro=="pf") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,cutmesh
    open(10, file="3d_pf.dat")
    read(10,*) VVP_Sf_x(ii),VVP_Sf_y(ii),VVP_Sf_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  !>> first and end point 
  VVP_Sf0_x(1)=VVP_Sf_x(1) ;VVP_Sf0_y(1)=VVP_Sf_y(1);VVP_Sf0_z(1)= VVP_Sf_z(1)
  VVP_Sfe_x(1)=VVP_Sf_x(cutmesh) ;VVP_Sfe_y(1)=VVP_Sf_y(cutmesh);VVP_Sfe_z(1)= VVP_Sf_z(cutmesh)
  !<< 
  !============================================================= start x-points 
  if(type_pro=='max') then

  If(xyz==1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVP_Sf0_x(1)
      else
        write(66,"(F23.15,A)") VVP_Sf0_x(1),","
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
          write(66,"(F23.15)") VVP_Sf_x(i)  
        else
          write(66,"(F23.15,A)") VVP_Sf_x(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVP_Sfe_x(1)
      else
        write(66,"(F23.15,A)") VVP_Sfe_x(1),","
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
        write(66,"(F23.15)") VVP_Sf0_y(1)
      else
        write(66,"(F23.15,A)") VVP_Sf0_y(1),","
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
          write(66,"(F23.15)") VVP_Sf_y(i)  
        else
          write(66,"(F23.15,A)") VVP_Sf_y(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVP_Sfe_y(1)
      else
        write(66,"(F23.15,A)") VVP_Sfe_y(1),","
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
        write(66,"(F23.15)") VVP_Sf0_z(1)
      else
        write(66,"(F23.15,A)") VVP_Sf0_z(1),","
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
          write(66,"(F23.15)") VVP_Sf_z(i)  
        else
          write(66,"(F23.15,A)") VVP_Sf_z(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVP_Sfe_z(1)
      else
        write(66,"(F23.15,A)") VVP_Sfe_z(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
  endif 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
endif
Endif
!$#######################################################################################
IF (namepro=="ps") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,cutmesh
    open(10, file="3d_ps.dat")
    read(10,*) VVP_Ss_x(ii),VVP_Ss_y(ii),VVP_Ss_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  !>> first and end point 
  VVP_Ss0_x(1)=VVP_Ss_x(1) ;VVP_Ss0_y(1)=VVP_Ss_y(1);VVP_Ss0_z(1)= VVP_Ss_z(1)
  VVP_Sse_x(1)=VVP_Ss_x(cutmesh) ;VVP_Sse_y(1)=VVP_Ss_y(cutmesh);VVP_Sse_z(1)= VVP_Ss_z(cutmesh)
  !<< 
  !============================================================= start x-points 
  if(type_pro=='max') then

  If(xyz==1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVP_Ss0_x(1)
      else
        write(66,"(F23.15,A)") VVP_Ss0_x(1),","
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
          write(66,"(F23.15)") VVP_Ss_x(i)  
        else
          write(66,"(F23.15,A)") VVP_Ss_x(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVP_Sse_x(1)
      else
        write(66,"(F23.15,A)") VVP_Sse_x(1),","
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
        write(66,"(F23.15)") VVP_Ss0_y(1)
      else
        write(66,"(F23.15,A)") VVP_Ss0_y(1),","
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
          write(66,"(F23.15)") VVP_Ss_y(i)  
        else
          write(66,"(F23.15,A)") VVP_Ss_y(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVP_Sse_y(1)
      else
        write(66,"(F23.15,A)") VVP_Sse_y(1),","
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
        write(66,"(F23.15)") VVP_Ss0_z(1)
      else
        write(66,"(F23.15,A)") VVP_Ss0_z(1),","
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
          write(66,"(F23.15)") VVP_Ss_z(i)  
        else
          write(66,"(F23.15,A)") VVP_Ss_z(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VVP_Sse_z(1)
      else
        write(66,"(F23.15,A)") VVP_Sse_z(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
  endif 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
endif
Endif
!$#######################################################################################
IF (namepro=="pfp") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,cutmesh
    open(10, file="3d_pfp.dat")
    read(10,*) VV_P_FA_x(ii),VV_P_FA_y(ii),VV_P_FA_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  !>> first and end point 
  VV_P_FA0_x(1)=VV_P_FA_x(1) ;VV_P_FA0_y(1)=VV_P_FA_y(1);VV_P_FA0_z(1)= VV_P_FA_z(1)
  VV_P_FAe_x(1)=VV_P_FA_x(cutmesh) ;VV_P_FAe_y(1)=VV_P_FA_y(cutmesh);VV_P_FAe_z(1)= VV_P_FA_z(cutmesh)
  !<< 
  !============================================================= start x-points 
  if(type_pro=='max') then

  If(xyz==1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VV_P_FA0_x(1)
      else
        write(66,"(F23.15,A)") VV_P_FA0_x(1),","
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
          write(66,"(F23.15)") VV_P_FA_x(i)  
        else
          write(66,"(F23.15,A)") VV_P_FA_x(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VV_P_FAe_x(1)
      else
        write(66,"(F23.15,A)") VV_P_FAe_x(1),","
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
        write(66,"(F23.15)") VV_P_FA0_y(1)
      else
        write(66,"(F23.15,A)") VV_P_FA0_y(1),","
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
          write(66,"(F23.15)") VV_P_FA_y(i)  
        else
          write(66,"(F23.15,A)") VV_P_FA_y(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VV_P_FAe_y(1)
      else
        write(66,"(F23.15,A)") VV_P_FAe_y(1),","
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
        write(66,"(F23.15)") VV_P_FA0_z(1)
      else
        write(66,"(F23.15,A)") VV_P_FA0_z(1),","
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
          write(66,"(F23.15)") VV_P_FA_z(i)  
        else
          write(66,"(F23.15,A)") VV_P_FA_z(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VV_P_FAe_z(1)
      else
        write(66,"(F23.15,A)") VV_P_FAe_z(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
  endif 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
endif
Endif
!$#######################################################################################
IF (namepro=="pfs") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,cutmesh
    open(10, file="3d_pfs.dat")
    read(10,*) VV_Ss_FA_x(ii),VV_Ss_FA_y(ii),VV_Ss_FA_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  !>> first and end point 
  VV_Ss_FA0_x(1)=VV_Ss_FA_x(1) ;VV_Ss_FA0_y(1)=VV_Ss_FA_y(1);VV_Ss_FA0_z(1)= VV_Ss_FA_z(1)
  VV_Ss_FAe_x(1)=VV_Ss_FA_x(cutmesh) ;VV_Ss_FAe_y(1)=VV_Ss_FA_y(cutmesh);VV_Ss_FAe_z(1)= VV_Ss_FA_z(cutmesh)
  !<< 
  !============================================================= start x-points 
  if(type_pro=='max') then

  If(xyz==1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VV_Ss_FA0_x(1)
      else
        write(66,"(F23.15,A)") VV_Ss_FA0_x(1),","
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
          write(66,"(F23.15)") VV_Ss_FA_x(i)  
        else
          write(66,"(F23.15,A)") VV_Ss_FA_x(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VV_Ss_FAe_x(1)
      else
        write(66,"(F23.15,A)") VV_Ss_FAe_x(1),","
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
        write(66,"(F23.15)") VV_Ss_FA0_y(1)
      else
        write(66,"(F23.15,A)") VV_Ss_FA0_y(1),","
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
          write(66,"(F23.15)") VV_Ss_FA_y(i)  
        else
          write(66,"(F23.15,A)") VV_Ss_FA_y(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VV_Ss_FAe_y(1)
      else
        write(66,"(F23.15,A)") VV_Ss_FAe_y(1),","
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
        write(66,"(F23.15)") VV_Ss_FA0_z(1)
      else
        write(66,"(F23.15,A)") VV_Ss_FA0_z(1),","
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
          write(66,"(F23.15)") VV_Ss_FA_z(i)  
        else
          write(66,"(F23.15,A)") VV_Ss_FA_z(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VV_Ss_FAe_z(1)
      else
        write(66,"(F23.15,A)") VV_Ss_FAe_z(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
  endif 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
endif
Endif
!$#######################################################################################
IF (namepro=="pff") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,cutmesh
    open(10, file="3d_pff.dat")
    read(10,*) VV_Sf_FA_x(ii),VV_Sf_FA_y(ii),VV_Sf_FA_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  !>> first and end point 
  VV_Sf_FA0_x(1)=VV_Sf_FA_x(1) ;VV_Sf_FA0_y(1)=VV_Sf_FA_y(1);VV_Sf_FA0_z(1)= VV_Sf_FA_z(1)
  VV_Sf_FAe_x(1)=VV_Sf_FA_x(cutmesh) ;VV_Sf_FAe_y(1)=VV_Sf_FA_y(cutmesh);VV_Sf_FAe_z(1)= VV_Sf_FA_z(cutmesh)
  !<< 
  !============================================================= start x-points 
  if(type_pro=='max') then

  If(xyz==1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VV_Sf_FA0_x(1)
      else
        write(66,"(F23.15,A)") VV_Sf_FA0_x(1),","
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
          write(66,"(F23.15)") VV_Sf_FA_x(i)  
        else
          write(66,"(F23.15,A)") VV_Sf_FA_x(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VV_Sf_FAe_x(1)
      else
        write(66,"(F23.15,A)") VV_Sf_FAe_x(1),","
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
        write(66,"(F23.15)") VV_Sf_FA0_y(1)
      else
        write(66,"(F23.15,A)") VV_Sf_FA0_y(1),","
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
          write(66,"(F23.15)") VV_Sf_FA_y(i)  
        else
          write(66,"(F23.15,A)") VV_Sf_FA_y(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VV_Sf_FAe_y(1)
      else
        write(66,"(F23.15,A)") VV_Sf_FAe_y(1),","
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
        write(66,"(F23.15)") VV_Sf_FA0_z(1)
      else
        write(66,"(F23.15,A)") VV_Sf_FA0_z(1),","
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
          write(66,"(F23.15)") VV_Sf_FA_z(i)  
        else
          write(66,"(F23.15,A)") VV_Sf_FA_z(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") VV_Sf_FAe_z(1)
      else
        write(66,"(F23.15,A)") VV_Sf_FAe_z(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
  endif 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
endif
Endif

!$#######################################################################################
    close(10)
 end SUBROUTINE
 
 
 
!ttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt
!$ttttttttttttttttttttttttttttttttttttttttt  STARTSUBROUTINE ttttttttttttttttttttttttttttttttttttttttttttttt
!ttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt


 SUBROUTINE get_data_mthcond_plotly( namepro,xyz,n_phif,n_thetaf,cutmesh, type_pro)
  implicit NONE
                                             
 DOUBLE PRECISION, DIMENSION(699999)   ::   km0_x      ,km0_y      ,km0_z      ,&
                                            kme_x      ,kme_y      ,kme_z      ,&
                                            km_x       ,km_y       ,km_z             
                                          
                                         
  INTEGER,          DIMENSION(1900,4)   :: mesh=0
  INTEGER                               :: n_phif, n_thetaf, num_mesh,i,ii,argl,cutmesh,k,&
                                           start_new_reng,end_new_reng,xyz
  character(len=10)                     :: val='',namepro ,type_pro ! type_pro : max, min, neg
  
!=============================================read mesh
    OPEN(69, file="MESH")
    read(69,*)n_phif,n_thetaf,cutmesh
    
    close(69)
!============================================= 
!$#######################################################################################
IF (namepro=="km") THEN
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! loop-reader
  Do ii=1,cutmesh
    open(10, file="3d_km.dat")
    read(10,*) km_x(ii),km_y(ii),km_z(ii)
  enddo
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end loop
  !>> first and end point 
  km0_x(1)=km_x(1) ;km0_y(1)=km_y(1);km0_z(1)= km_z(1)
  kme_x(1)=km_x(cutmesh) ;kme_y(1)=km_y(cutmesh);kme_z(1)= km_z(cutmesh)
  !<< 
  !============================================================= start x-points 
  if(type_pro=='max') then

  If(xyz==1) then
    !!!!!!!!!!!!!!!!!!!!!!!!! first points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") km0_x(1)
      else
        write(66,"(F23.15,A)") km0_x(1),","
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
          write(66,"(F23.15)") km_x(i)  
        else
          write(66,"(F23.15,A)") km_x(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") kme_x(1)
      else
        write(66,"(F23.15,A)") kme_x(1),","
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
        write(66,"(F23.15)") km0_y(1)
      else
        write(66,"(F23.15,A)") km0_y(1),","
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
          write(66,"(F23.15)") km_y(i)  
        else
          write(66,"(F23.15,A)") km_y(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") kme_y(1)
      else
        write(66,"(F23.15,A)") kme_y(1),","
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
        write(66,"(F23.15)") km0_z(1)
      else
        write(66,"(F23.15,A)") km0_z(1),","
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
          write(66,"(F23.15)") km_z(i)  
        else
          write(66,"(F23.15,A)") km_z(i),"," 
        endif
      enddo
      call middle_cord_web()
      start_new_reng = end_new_reng    + 1 
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! end points 
    do i=1,n_phif+1
      if (i==n_phif+1) then
        write(66,"(F23.15)") kme_z(1)
      else
        write(66,"(F23.15,A)") kme_z(1),","
      endif
    enddo
    !!!!!!!!!!!!!!!!!!!!!!!!! finish
  endif 
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! end z-point
endif
Endif
!$#######################################################################################
    close(10)
    
 end SUBROUTINE    
