
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; 3d surface map : data file ( of AAEP code) to gpi file. 

subroutine threeDdmap()
    implicit none
    DOUBLE PRECISION, DIMENSION(1930000)  :: G_max,shminp,shminn,shavep,SINver,CO,comminp,pugh_max,pughminp,pughminn,pughavep,&
                                             comminn,NPratio_max,pminp,pminn,pavep,paven, BINver,maxEVaLM1,maxEVaTM1,minEVaTM1,&
                                             VVG_P,VVP_P,VV_P_PF,VVG_Sf,VVP_Sf,VV_Sf_PF,VVG_Ss,VVP_Ss,VV_Ss_PF, theta, phi, Minbulk,Maxbulk
    INTEGER                               :: h_ex,k_ex,l_ex, i, ii,iii,cutmesh,n_phif, n_thetaf,j
    ChARACTER(LEN=6)                      :: e1,e2
    ChARACTER(LEN=2)                      :: ynveloc
    character(len=10)                     :: val=' '

    OPEN(123, FILE='HKL')
    read(123,*) e1
    read(123,*) e2
    read(123,*) h_ex
    read(123,*) k_ex
    read(123,*) l_ex
    read(123,*) ynveloc
    CLOSE(123)
    OPEN(69, file="MESH")
    read(69,*)n_phif,n_thetaf,cutmesh
    close(69)
      !print*, cutmesh

    OPEN(1,file='.aelastpro')
    OPEN(5,file='.aelastpro2')
    do ii=1, n_phif*n_thetaf
     ! print*, ii
     read(1,*) G_max(ii),shminp(ii),shminn(ii),shavep(ii),SINver(ii),CO(ii),comminp(ii),&
               comminn(ii),NPratio_max(ii),pminp(ii),pminn(ii),pavep(ii),paven(ii),BINver(ii),&
               maxEVaLM1(ii),maxEVaTM1(ii),minEVaTM1(ii),pugh_max(ii),pughminp(ii),pughminn(ii),pughavep(ii) 
    IF (ynveloc=='Y' .OR. ynveloc=='y')THEN
     read(5,*) VVP_P(ii),VVG_P(ii),VVP_Sf(ii),VVG_Sf(ii),VVP_Ss(ii),VVG_Ss(ii),VV_P_PF(ii),VV_Sf_PF(ii),VV_Ss_PF(ii)
          
      
    ENDIF

        !if (ii==cutmesh) exit
    ENDDO
    close(5)
    close(1)
     OPEN(50,file='3d_SD.dat')   
    do iii=1, n_phif*n_thetaf
        read(50,*)theta(iii), phi(iii)
    ENDDO
     close(50)

     open(13, file=".SDdat")
    do j=1, n_phif*n_thetaf

     IF (ynveloc=='Y' .OR. ynveloc=='y')THEN
        if (phi(j) == 0.or. theta(j)==3.14159265358979 .or. theta(j)==0.0d0)   Write(13,*)"  "
        Write(13,"(36F25.15)") theta(j), phi(j),G_max(j),shminp(j),shminn(j),shavep(j),SINver(j),CO(j),comminp(j),&                  !9N
                               comminn(j),NPratio_max(j),pminp(j),pminn(j),pavep(j),paven(j),BINver(j),&                             !16
                               maxEVaLM1(j),maxEVaTM1(j),minEVaTM1(j),pugh_max(j),pughminp(j),pughminn(j),pughavep(j),&              !23
                               VVP_P(j),VVG_P(j),VVP_Sf(j),VVG_Sf(j),VVP_Ss(j),VVG_Ss(j),VV_P_PF(j),VV_Sf_PF(j),VV_Ss_PF(j) !32

     else
        if (phi(j) == 0.or. theta(j)==3.14159265358979 .or. theta(j)==0.0d0) Write(13,*)"  "
        Write(13,"(26F25.15)")theta(j), phi(j),G_max(j),shminp(j),shminn(j),shavep(j),SINver(j),CO(j),comminp(j),&
                              comminn(j),NPratio_max(j),pminp(j),pminn(j),pavep(j),paven(j),BINver(j),&
                              maxEVaLM1(j),maxEVaTM1(j),minEVaTM1(j),pugh_max(j),pughminp(j),pughminn(j),pughavep(j)
     ENDIF
    enddo
    close(13)
end subroutine