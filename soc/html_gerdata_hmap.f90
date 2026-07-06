
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
SUBROUTINE get_dataplotly_hmap(namepro,cord_ename,chengnum) !namepro, xyz, n_phif, n_thetaf, cutmesh, type_pro
   implicit NONE
  integer, parameter :: array_size = 1000000
DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: G_max, shminp, shminn, shavep, SINver, CO, comminp, pugh_max, pughminp, pughminn, &
                                               pughavep, comminn, NPratio_max, pminp, pminn, pavep, paven, BINver, maxEVaLM1, &
                                               maxEVaTM1, minEVaTM1, VVG_P, VVP_P, VV_P_PF, VVG_Sf, VVP_Sf, VV_Sf_PF, VVG_Ss, VVP_Ss, &
                                               VV_Ss_PF, theta, phi, Minbulk, Maxbulk, hardvar_max, hardvar_min, km


   !INTEGER, DIMENSION(190300, 4) :: mesh = 0
   INTEGER                               :: n_phif, n_thetaf, num_mesh, i, j, argl, cutmesh, k, &
                                            start_new_reng, end_new_reng, xyz
   character(len=10)                     :: val = '', namepro, type_pro ! type_pro : max, min, neg
    INTEGER                               :: h_ex0,k_ex0,l_ex0, chengnum
    ChARACTER(LEN=6)                      :: e10,e20
    ChARACTER(LEN=2)                      :: ynveloc0
    ChARACTER(LEN=1)                      ::  cord_ename
ALLOCATE(G_max(array_size), shminp(array_size), shminn(array_size), shavep(array_size), SINver(array_size), CO(array_size), comminp(array_size), pugh_max(array_size), &
         pughminp(array_size), pughminn(array_size), pughavep(array_size), comminn(array_size), NPratio_max(array_size), pminp(array_size), pminn(array_size), &
         pavep(array_size), paven(array_size), BINver(array_size), maxEVaLM1(array_size), maxEVaTM1(array_size), minEVaTM1(array_size), VVG_P(array_size), &
         VVP_P(array_size), VV_P_PF(array_size), VVG_Sf(array_size), VVP_Sf(array_size), VV_Sf_PF(array_size), VVG_Ss(array_size), VVP_Ss(array_size), &
         VV_Ss_PF(array_size), theta(array_size), phi(array_size), Minbulk(array_size), Maxbulk(array_size), hardvar_max(array_size), hardvar_min(array_size), km(array_size))
    
!=============================================read mesh
   OPEN (69, file="MESH")
   read (69, *) n_phif, n_thetaf, cutmesh
    !Write(*,*)n_phif,n_thetaf,cutmesh
   close (69)
!=============================================
    OPEN(123, FILE='HKL')
     read(123,*) e10
     read(123,*) e20
     read(123,*) h_ex0
     read(123,*) k_ex0
     read(123,*) l_ex0
     read(123,*) ynveloc0
    CLOSE(123)
IF (ynveloc0=='Y' .OR. ynveloc0=='y')THEN
 open(13, file=".SDdat")

    do j=1, (n_phif*n_thetaf) +n_thetaf+2
  ! write(*,*)j
     READ(13,*) theta(j), phi(j), G_max(j),shminp(j),shminn(j),shavep(j),SINver(j),CO(j),comminp(j),&                                    !9
                comminn(j),NPratio_max(j),pminp(j),pminn(j),pavep(j),paven(j),BINver(j),&                                                !16
                maxEVaLM1(j),maxEVaTM1(j),minEVaTM1(j),pugh_max(j),pughminp(j),pughminn(j),pughavep(j),hardvar_max(j),hardvar_min(j),&   !25
                VVP_P(j),VVG_P(j),VVP_Sf(j),VVG_Sf(j),VVP_Ss(j),VVG_Ss(j),VV_P_PF(j),VV_Sf_PF(j),VV_Ss_PF(j),km(j)                       !35
                
    if (cord_ename=="x")  write(66,"(4XF25.15, A)")theta(j)*180/3.141592654, ","
    if (cord_ename=="y")  write(66,"(4XF25.15, A)")phi(j)*180/3.141592654, ","
    if (cord_ename=="z" .and. namepro=="hmshear" .and. chengnum==1)  write(66,"(4XF25.15, A)")G_max(j), ","
    if (cord_ename=="z" .and. namepro=="hmshear" .and. chengnum==2)  write(66,"(4XF25.15, A)")shminp(j), ","
    if (cord_ename=="z" .and. namepro=="hmshear" .and. chengnum==3)  write(66,"(4XF25.15, A)")shminn(j), ","
    if (cord_ename=="z" .and. namepro=="hmshear" .and. chengnum==4)  write(66,"(4XF25.15, A)")shavep(j), ","
    
    if (cord_ename=="z" .and. namepro=="hmyoung" .and. chengnum==0)  write(66,"(4XF25.15, A)")SINver(j), ","

    if (cord_ename=="z" .and. namepro=="hmcomp" .and. chengnum==1)  write(66,"(4XF25.15, A)")CO(j), ","
    if (cord_ename=="z" .and. namepro=="hmcomp" .and. chengnum==2)  write(66,"(4XF25.15, A)")comminp(j), ","
    if (cord_ename=="z" .and. namepro=="hmcomp" .and. chengnum==3)  write(66,"(4XF25.15, A)")comminn(j), "," 

    if (cord_ename=="z" .and. namepro=="hmpoi" .and. chengnum==1)  write(66,"(4XF25.15, A)")NPratio_max(j), "," 
    if (cord_ename=="z" .and. namepro=="hmpoi" .and. chengnum==2)  write(66,"(4XF25.15, A)")pminp(j), "," 
    if (cord_ename=="z" .and. namepro=="hmpoi" .and. chengnum==3)  write(66,"(4XF25.15, A)")pminn(j), ","  
    if (cord_ename=="z" .and. namepro=="hmpoi" .and. chengnum==4)  write(66,"(4XF25.15, A)")pavep(j), ","    
    if (cord_ename=="z" .and. namepro=="hmpoi" .and. chengnum==5)  write(66,"(4XF25.15, A)")paven(j), ","  
    
    if (cord_ename=="z" .and. namepro=="hmbulk" .and. chengnum==0)  write(66,"(4XF25.15, A)")BINver(j), ","
    
    if (cord_ename=="z" .and. namepro=="hmpugh" .and. chengnum==1)  write(66,"(4XF25.15, A)")pugh_max(j), ","
    if (cord_ename=="z" .and. namepro=="hmpugh" .and. chengnum==2)  write(66,"(4XF25.15, A)")pughminp(j), ","
    if (cord_ename=="z" .and. namepro=="hmpugh" .and. chengnum==3)  write(66,"(4XF25.15, A)")pughminn(j), ","
    if (cord_ename=="z" .and. namepro=="hmpugh" .and. chengnum==4)  write(66,"(4XF25.15, A)")pughavep(j), ","
    
    if (cord_ename=="z" .and. namepro=="hmhard" .and. chengnum==1)  write(66,"(4XF25.15, A)")hardvar_max(j), ","
    if (cord_ename=="z" .and. namepro=="hmhard" .and. chengnum==2)  write(66,"(4XF25.15, A)")hardvar_min(j), ","
    !!!!!===========EWP   
    if (cord_ename=="z" .and. namepro=="hmpall"  .and. chengnum==1)  write(66,"(4XF25.15, A)")VVP_P(j), ","    ! 26
    if (cord_ename=="z" .and. namepro=="hmgall"  .and. chengnum==1)  write(66,"(4XF25.15, A)")VVG_P(j), ","    ! 27
    if (cord_ename=="z" .and. namepro=="hmpall"  .and. chengnum==3)  write(66,"(4XF25.15, A)")VVP_Sf(j), ","   ! 28
     if (cord_ename=="z" .and. namepro=="hmgall"  .and. chengnum==3)  write(66,"(4XF25.15, A)")VVG_Sf(j), ","  ! 29
    if (cord_ename=="z" .and. namepro=="hmgall"  .and. chengnum==2)  write(66,"(4XF25.15, A)")VVG_Ss(j), ","   ! 31
    if (cord_ename=="z" .and. namepro=="hmpall"  .and. chengnum==2)  write(66,"(4XF25.15, A)")VVP_Ss(j), ","   ! 30
    if (cord_ename=="z" .and. namepro=="hmpfall" .and. chengnum==1)  write(66,"(4XF25.15, A)")VV_P_PF(j), ","  ! 32
    if (cord_ename=="z" .and. namepro=="hmpfall" .and. chengnum==3)  write(66,"(4XF25.15, A)")VV_Sf_PF(j), "," ! 33
    if (cord_ename=="z" .and. namepro=="hmpfall" .and. chengnum==2)  write(66,"(4XF25.15, A)")VV_Ss_PF(j), "," ! 34
    if (cord_ename=="z" .and. namepro=="hmkm" .and. chengnum==0)  write(66,"(4XF25.15, A)")km(j), ","        ! 35
    
    !!!!!===========EWP             
    enddo
            
 close(13)  
else
 open(13, file=".SDdat")
    do j=1, (n_phif*n_thetaf) 

     READ(13,*) theta(j), phi(j), G_max(j),shminp(j),shminn(j),shavep(j),SINver(j),CO(j),comminp(j),&                  !9
                comminn(j),NPratio_max(j),pminp(j),pminn(j),pavep(j),paven(j),BINver(j),&                             !16
                maxEVaLM1(j),maxEVaTM1(j),minEVaTM1(j),pugh_max(j),pughminp(j),pughminn(j),pughavep(j),hardvar_max(j),hardvar_min(j)   !25
                                
    if (cord_ename=="x")  write(66,"(4XF25.15, A)")theta(j)*180/3.141592654, ","
    if (cord_ename=="y")  write(66,"(4XF25.15, A)")phi(j)*180/3.141592654, ","
    if (cord_ename=="z" .and. namepro=="hmshear" .and. chengnum==1)  write(66,"(4XF25.15, A)")G_max(j), ","      ! 3
    if (cord_ename=="z" .and. namepro=="hmshear" .and. chengnum==2)  write(66,"(4XF25.15, A)")shminp(j), ","     ! 4
    if (cord_ename=="z" .and. namepro=="hmshear" .and. chengnum==3)  write(66,"(4XF25.15, A)")shminn(j), ","     ! 5
    if (cord_ename=="z" .and. namepro=="hmshear" .and. chengnum==4)  write(66,"(4XF25.15, A)")shavep(j), ","     ! 6
    
    if (cord_ename=="z" .and. namepro=="hmyoung" .and. chengnum==0)  write(66,"(4XF25.15, A)")SINver(j), ","     ! 7 

    if (cord_ename=="z" .and. namepro=="hmcomp" .and. chengnum==1)  write(66,"(4XF25.15, A)")CO(j), ","          ! 8
    if (cord_ename=="z" .and. namepro=="hmcomp" .and. chengnum==2)  write(66,"(4XF25.15, A)")comminp(j), ","     ! 9
    if (cord_ename=="z" .and. namepro=="hmcomp" .and. chengnum==3)  write(66,"(4XF25.15, A)")comminn(j), ","     ! 10

    if (cord_ename=="z" .and. namepro=="hmpoi" .and. chengnum==1)  write(66,"(4XF25.15, A)")NPratio_max(j), ","  ! 11
    if (cord_ename=="z" .and. namepro=="hmpoi" .and. chengnum==2)  write(66,"(4XF25.15, A)")pminp(j), ","        ! 12
    if (cord_ename=="z" .and. namepro=="hmpoi" .and. chengnum==3)  write(66,"(4XF25.15, A)")pminn(j), ","        ! 13
    if (cord_ename=="z" .and. namepro=="hmpoi" .and. chengnum==4)  write(66,"(4XF25.15, A)")pavep(j), ","        ! 14
    if (cord_ename=="z" .and. namepro=="hmpoi" .and. chengnum==5)  write(66,"(4XF25.15, A)")paven(j), ","        ! 15
    
    if (cord_ename=="z" .and. namepro=="hmbulk" .and. chengnum==0)  write(66,"(4XF25.15, A)")BINver(j), ","      !16
    
    if (cord_ename=="z" .and. namepro=="hmpugh" .and. chengnum==1)  write(66,"(4XF25.15, A)")pugh_max(j), ","    ! 20
    if (cord_ename=="z" .and. namepro=="hmpugh" .and. chengnum==2)  write(66,"(4XF25.15, A)")pughminp(j), ","    ! 21
    if (cord_ename=="z" .and. namepro=="hmpugh" .and. chengnum==3)  write(66,"(4XF25.15, A)")pughminn(j), ","    ! 22
    if (cord_ename=="z" .and. namepro=="hmpugh" .and. chengnum==4)  write(66,"(4XF25.15, A)")pughavep(j), ","    ! 23
    
    if (cord_ename=="z" .and. namepro=="hmhard" .and. chengnum==1)  write(66,"(4XF25.15, A)")hardvar_max(j), "," ! 24
    if (cord_ename=="z" .and. namepro=="hmhard" .and. chengnum==2)  write(66,"(4XF25.15, A)")hardvar_min(j), "," ! 25

    enddo
    
 close(13)    
ENDIF
 
  
DEALLOCATE(G_max, shminp, shminn, shavep, SINver, CO, comminp, pugh_max, &
      pughminp, pughminn, pughavep, comminn, NPratio_max, pminp, pminn, &
      pavep, paven, BINver, maxEVaLM1, maxEVaTM1, minEVaTM1, VVG_P, &
      VVP_P, VV_P_PF, VVG_Sf, VVP_Sf, VV_Sf_PF, VVG_Ss, VVP_Ss, VV_Ss_PF, &
      theta, phi, Minbulk, Maxbulk, hardvar_max, hardvar_min, km)
end SUBROUTINE
