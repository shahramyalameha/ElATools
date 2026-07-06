
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
                                             VVG_P,VVP_P,VV_P_PF,VVG_Sf,VVP_Sf,VV_Sf_PF,VVG_Ss,VVP_Ss,VV_Ss_PF, theta, phi, Minbulk,Maxbulk, &
                                             hardvar_max, hardvar_min, km
    INTEGER                               :: h_ex0,k_ex0,l_ex0, i, ii,iii,cutmesh,n_phif, n_thetaf,j, st
    ChARACTER(LEN=6)                      :: e10,e20
    ChARACTER(LEN=2)                      :: ynveloc0
    character(len=10)                     :: val=' '

    OPEN(123, FILE='HKL')
     read(123,*) e10
     read(123,*) e20
     read(123,*) h_ex0
     read(123,*) k_ex0
     read(123,*) l_ex0
     read(123,*) ynveloc0
    CLOSE(123)
     !write(*,*)ynveloc0
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
               maxEVaLM1(ii),maxEVaTM1(ii),minEVaTM1(ii),pugh_max(ii),pughminp(ii),pughminn(ii),pughavep(ii),hardvar_max(ii), hardvar_min(ii)
    IF (ynveloc0=='Y' .OR. ynveloc0=='y'.or. ynveloc0=='yy')THEN
     read(5,*) VVP_P(ii),VVG_P(ii),VVP_Sf(ii),VVG_Sf(ii),VVP_Ss(ii),VVG_Ss(ii),VV_P_PF(ii),VV_Sf_PF(ii),VV_Ss_PF(ii),km(ii)
          
      
    ENDIF

        !if (ii==cutmesh) exit
    ENDDO
    close(5)
    close(1)
     OPEN(50,file='3d_SD.dat')   
    do iii=1, (n_phif*n_thetaf)+1
        read(50,*)theta(iii), phi(iii)
        
    ENDDO
     close(50)
     !write(*,*) n_phif*n_thetaf
     open(13, file=".SDdat")
    do j=1, (n_phif*n_thetaf)

     IF (ynveloc0=='Y' .OR. ynveloc0=='y')THEN
        
        if (phi(j) == 0.or. theta(j)==3.14159265358979 .or. theta(j)==0.0d0)   Write(13,*)"  "
        Write(13,"(38F25.15)") theta(j), phi(j),G_max(j),shminp(j),shminn(j),shavep(j),SINver(j),CO(j),comminp(j),&                  !9
                               comminn(j),NPratio_max(j),pminp(j),pminn(j),pavep(j),paven(j),BINver(j),&                             !16
                               maxEVaLM1(j),maxEVaTM1(j),minEVaTM1(j),pugh_max(j),pughminp(j),pughminn(j),pughavep(j),hardvar_max(j),hardvar_min(j),&   !25
                               VVP_P(j),VVG_P(j),VVP_Sf(j),VVG_Sf(j),VVP_Ss(j),VVG_Ss(j),VV_P_PF(j),VV_Sf_PF(j),VV_Ss_PF(j),km(j)    !35

     else
         if (phi(j) == 0.or. theta(j+1)==3.141592653589790 .or. theta(j)==0.0d0) Write(13,*)"  "
!===============================First point>       
         IF (j==1) then
            DO st=1, n_phif
            Write(13,"(28F25.15)")theta(1), phi(1),G_max(1),shminp(1),shminn(1),shavep(1),SINver(1),CO(1),comminp(1),&              !9
                                  comminn(1),NPratio_max(1),pminp(1),pminn(1),pavep(1),paven(1),BINver(1),&                         !16
                                   maxEVaLM1(1),maxEVaTM1(1),minEVaTM1(1),pugh_max(1),pughminp(1),pughminn(1),pughavep(1),hardvar_max(1),hardvar_min(1) !25
                              
            enddo
         ENDIF 
!===============================First point<         
                          
        Write(13,"(28F25.15)")theta(j), phi(j),G_max(j),shminp(j),shminn(j),shavep(j),SINver(j),CO(j),comminp(j),&              !9
                              comminn(j),NPratio_max(j),pminp(j),pminn(j),pavep(j),paven(j),BINver(j),&                         !16
                              maxEVaLM1(j),maxEVaTM1(j),minEVaTM1(j),pugh_max(j),pughminp(j),pughminn(j),pughavep(j),hardvar_max(j),hardvar_min(j) !25
                              
!===============================END point>                         
         IF (j==(n_phif*n_thetaf)) then
            Write(13,*)"  "
            DO st=1, n_phif
            Write(13,"(28F25.15)")theta((n_phif*n_thetaf)+1),phi((n_phif*n_thetaf)+1), G_max(1),shminp(1),shminn(1),shavep(1),SINver(1),CO(1),comminp(1),&              !9
                                  comminn(1),NPratio_max(1),pminp(1),pminn(1),pavep(1),paven(1),BINver(1),&                         !16
                                   maxEVaLM1(1),maxEVaTM1(1),minEVaTM1(1),pugh_max(1),pughminp(1),pughminn(1),pughavep(1),hardvar_max(1),hardvar_min(1) !25
                              
            Enddo
         ENDIF 
!===============================END point<                                                       
     ENDIF
    enddo
    close(13)
end subroutine

!============================================================================

subroutine sp_neg_val(val,ngpos)
    implicit none
    DOUBLE PRECISION                      :: Maxyoung,Minyoung,Maxcomp,Mincomp,G_max2,G_min2,Maxbulk,Minbulk,&
                                             Pratio_max,Pratio_min,maxEVaTMf,maxEVaLM,minEVaTMf,pugh_max2,pugh_min2,&
                                             Ha_max2,Ha_min2, Ha_max1,Ha_min1,VVP_P_max,VVP_P_min,VVP_Sf_max,VVP_Sf_min,VVP_Ss_max,VVP_Ss_min,VVG_P_max,&
                                             VVG_P_min,VVG_Sf_max,VVG_Sf_min,VVG_Ss_max,VVG_Ss_min,VV_P_PF_max,VV_Sf_PF_max,VV_Ss_PF_max,&
                                             VV_P_PF_min,VV_Sf_PF_min,VV_Ss_PF_min,km_max,km_min, ngpos
    INTEGER                               :: h_ex0,k_ex0,l_ex0, i, ii,iii,cutmesh,n_phif, n_thetaf,j, st
    ChARACTER(LEN=6)                      :: e10,e20
    ChARACTER(LEN=2)                      :: ynveloc0
    character(len=10)                     :: val 

    OPEN(123, FILE='HKL')
     read(123,*) e10
     read(123,*) e20
     read(123,*) h_ex0
     read(123,*) k_ex0
     read(123,*) l_ex0
     read(123,*) ynveloc0
    CLOSE(123)
     !write(*,*)ynveloc0
    OPEN(69, file="MESH")
    read(69,*)n_phif,n_thetaf,cutmesh
    close(69)


    OPEN(1,file='.MaMiout')
    OPEN(5,file='.MaMiout2')
                
    read(1,*) Maxyoung,Minyoung,Maxcomp,Mincomp,G_max2,G_min2,Maxbulk,Minbulk,&
             Pratio_max,Pratio_min,maxEVaTMf,maxEVaLM,minEVaTMf,pugh_max2,pugh_min2, Ha_max2,Ha_min2, Ha_max1,Ha_min1               
     if (val == "sppoi" .or. val == "hmpoi" ) ngpos=Pratio_min
     if (val == "spcomp" .or. val == "hmcomp") ngpos=Mincomp
               
    IF (ynveloc0=='Y' .OR. ynveloc0=='y'.or. ynveloc0=='yy')THEN
     read(5,*) VVP_P_max,VVP_P_min,VVP_Sf_max,VVP_Sf_min,VVP_Ss_max,VVP_Ss_min,VVG_P_max,&
                VVG_P_min,VVG_Sf_max,VVG_Sf_min,VVG_Ss_max,VVG_Ss_min,VV_P_PF_max,VV_Sf_PF_max,VV_Ss_PF_max,&
                VV_P_PF_min,VV_Sf_PF_min,VV_Ss_PF_min,km_max,km_min
    ENDIF
    close(5)
    close(1)

end subroutine
