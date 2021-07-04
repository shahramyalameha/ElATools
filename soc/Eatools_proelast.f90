!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, calculate main properties. 
SUBROUTINE proelast()
 IMPLICIT NONE
 DOUBLE PRECISION :: av,&
                     bv,&
                     cv,&
                     ar,&
                     br,&
                     cr,&
                     kv,&
                     kr,&
                     kh,&
                     gv,&
                     gr,&
                     gh,&
                     Ev,&
                     Er,&
                     Eh,&
                     nuv,&
                     nur,&
                     nuh,&
					                Kgv,&
                     Kgr,&
                     kgh,&
					                mv,&
					                mr,&
					                mh,&
					                Au,&
					                Al,&
					                Pc,&
					                Ac,&
					                La1v,&
					                La1r,&
					                La1h,&
					                La2v,&
					                La2r,&
					                La2h,&
					                kel,&
					                H_a1_r,&   ! 
					                H_a1_v,&
					                H_a1_h,&
					                H_b1_r,&
					                H_b1_h,&
					                H_b1_v,&
					                H_2_r,&
					                H_2_h,&
					                H_2_v,&
					                H_3_r,&
					                H_3_h,&
					                H_3_v,&
					                H_4_r,&
					                H_4_h,&
					                H_4_v,&
					                H_5_r,&
					                H_5_h,&
					                H_5_v,&
					                inv_kgv,&
					                inv_kgr,&
					                inv_kgh,&
					                Pc_hex_a,&
					                Pc_hex_c,&
					                Pc_orth_a,&
					                Pc_orth_b,&
					                Pc_orth_c
 CHARACTER(LEN=10) :: bdout1,bdout2
 CHARACTER(LEN=31) :: st_ben
 CHARACTER(LEN=5)  ::	Cov_met,Cov_met_aa,Cov_met_c,Cov_met_oa,Cov_met_ob,Cov_met_oc
 DOUBLE PRECISION, DIMENSION(6,6) ::C=0D0,S=0D0				 
 INTEGER::i,j
    OPEN(44,FILE="Cij.dat",status='old')                             ! read cij data inpout
    READ(44,*) C(1,1),C(1,2),C(1,3),C(1,4),C(1,5),C(1,6)
    READ(44,*) C(2,1),C(2,2),C(2,3),C(2,4),C(2,5),C(2,6)
    READ(44,*) C(3,1),C(3,2),C(3,3),C(3,4),C(3,5),C(3,6)
    READ(44,*) C(4,1),C(4,2),C(4,3),C(4,4),C(4,5),C(4,6)
    READ(44,*) C(5,1),C(5,2),C(5,3),C(5,4),C(5,5),C(5,6)
    READ(44,*) C(6,1),C(6,2),C(6,3),C(6,4),C(6,5),C(6,6)
    CLOSE(44)

    OPEN(10,FILE="Sij.dat",status='old')                             ! read cij data inpout
    READ(10,*) S(1,1),S(1,2),S(1,3),S(1,4),S(1,5),S(1,6)
    READ(10,*) S(2,1),S(2,2),S(2,3),S(2,4),S(2,5),S(2,6)
    READ(10,*) S(3,1),S(3,2),S(3,3),S(3,4),S(3,5),S(3,6)
    READ(10,*) S(4,1),S(4,2),S(4,3),S(4,4),S(4,5),S(4,6)
    READ(10,*) S(5,1),S(5,2),S(5,3),S(5,4),S(5,5),S(5,6)
    READ(10,*) S(6,1),S(6,2),S(6,3),S(6,4),S(6,5),S(6,6)
    CLOSE(10)
    WRITE(*,*)'#########################################################################'	
    WRITE(99,*)'#########################################################################'

    WRITE(*,*)''
    WRITE(*,*)'Cij:'
    WRITE(99,*)'Cij:'
    DO i=1,6
      WRITE(*,'(2X,6( F12.6))') (C(i,j), j=1,6)
      WRITE(99,'(2X,6(1X,F12.6))') (C(i,j), j=1,6)
    END do
    call sleep(1)
    WRITE(*,*)''
    WRITE(99,*)''
    WRITE(*,*)'Sij:'
    WRITE(99,*)'Sij:'

    DO i=1,6
      WRITE(*,'(2X,7( F12.7))') (S(i,j), j=1,6)
      WRITE(99,'(2X,7(1X,F12.7))') (S(i,j), j=1,6)
    END do
    WRITE(*,*)''
    WRITE(*,*)'#########################################################################' 
    WRITE(*,*)''
    WRITE(99,*)''
    WRITE(99,*)'#########################################################################' 
    WRITE(99,*)''
    call sleep(1)
    av=(C(1,1)+C(2,2)+C(3,3))/3d0
    bv=(C(1,2)+C(2,3)+C(1,3))/3d0
    cv=(C(4,4)+C(5,5)+C(6,6))/3d0
    ar=(S(1,1)+S(2,2)+S(3,3))/3d0
    br=(S(1,2)+S(2,3)+S(1,3))/3d0
    cr=(S(4,4)+S(5,5)+S(6,6))/3d0
!
    kv = (av+2d0*bv)/3d0
    kr = 1d0/(3d0*ar+6d0*br)
    kh = 0.5d0*(kv+kr) 
    gv = (av-bv+3d0*cv)/5d0
    gr = 5d0/(4d0*ar-4d0*br+3d0*cr)
    gh = 0.5d0*(gv+gr)
    Ev = 1d0/(1d0/(3d0*gv)+1d0/(9d0*kv))
    Er = 1d0/(1d0/(3d0*gr)+1d0/(9d0*kr))
    Eh = 0.5d0*(Ev+Er)
    nuv = 0.5d0*(1d0-(3d0*gv)/(3d0*kv+gv))
    nur = 0.5d0*(1d0-(3d0*gr)/(3d0*kv+gr))
    nuh = 0.5d0*(nuv+nur)
    Kgv = kv/gv
    Kgr = kr/gr
    kgh = kh/gh
    inv_kgv = 1.d0/Kgv
    inv_kgr = 1.d0/Kgr
    inv_kgh = 1.d0/kgh
    mv = kv + (4d0*gv/3d0)
    mr = kr + (4d0*gr/3d0)
    mh = (mv + mr)/2d0
    Pc        = C(1,1) - C(4,4)
    Pc_hex_a  = C(1,3) - C(4,4)
    Pc_hex_c  = C(1,2) - C(6,6)
    Pc_orth_a = C(2,3) - C(4,4)
    Pc_orth_b = C(1,3) - C(5,5)
    Pc_orth_c = C(1,2) - C(6,6)
    La1v = ( nuv * Ev )/ ( ( 1d0 + nuv )*( 1 - 2d0 * nuv ) )
    La1r = ( nur * Er )/ ( ( 1d0 + nur )*( 1 - 2d0 * nur ) )
    La1h = ( nuh * Eh )/ ( ( 1d0 + nuh )*( 1 - 2d0 * nuh ) )
    La2v = Ev / ( 2 * ( 1d0 + nuv ) )
    La2r = Er / ( 2 * ( 1d0 + nuh ) )
    La2h = Eh / ( 2 * ( 1d0 + nuh ) )
    kel  = ( C(1,1) + 8d0 * C(1,2) ) / ( 7d0 * C(1,1) - 2 * C(1,2) )
    H_a1_r = 0.1475D0 *  gr; H_a1_h = 0.1475D0 *  gh; H_a1_v = 0.1475D0 *  gv
    H_b1_r = 0.0607D0 *  Er; H_b1_h = 0.0607D0 *  Eh; H_b1_v = 0.0607D0 *  Ev
    H_2_r  = 0.1769D0 * gr - 2.899D0; H_2_h   = 0.1769D0 * gh - 2.899D0; H_2_v   = 0.1769D0 * gv - 2.899D0
    H_3_r  = 0.0635D0 * Er; H_3_h  = 0.0635D0 * Eh; H_3_v  = 0.0635D0 * Ev
    H_4_r  = ( kr * ( 1d0 - 2d0 * nur ) ) / ( 6d0 * ( 1d0 + nur  ) )
    H_4_v  = ( kv * ( 1d0 - 2d0 * nuv ) ) / ( 6d0 * ( 1d0 + nuv  ) )
    H_4_h  = ( kh * ( 1d0 - 2d0 * nuh ) ) / ( 6d0 * ( 1d0 + nuh  ) )
    H_5_r  = ( 2d0 * (( gr * inv_kgr**2d0 )**0.585) ) - 3d0
    H_5_v  = ( 2d0 * (( gv * inv_kgv**2d0 )**0.585) ) - 3d0
    H_5_h  = ( 2d0 * (( gh * inv_kgh**2d0 )**0.585) ) - 3d0
    
    CALL ductiletester_KG(kgh,bdout1)
    CALL ductiletester_PR(nuh,bdout2)
    CALL bond_stretching_bending (kel, st_ben)
    
    WRITE(*,*)'============================================================='
    WRITE(*,*)' Elastic properties       |      Voigt     Reuss     Average      '
    WRITE(*,*)'============================================================='
    WRITE(*,'(a,3F10.3,a)')' = Bulk modulus  (GPa)     | ', kv,kr,kh    ,'  ='
    WRITE(*,'(a,3F10.3,a)')' = Shear modulus (GPa)     | ', gv,gr,gh    ,'  ='
    WRITE(*,'(a,3F10.3,a)')' = Young modulus (GPa)     | ', Ev,Er,Eh    ,'  ='
    WRITE(*,'(a,3F10.4,a)')' = P-wave modulus(GPa)     | ', mv,mr,mh    ,'  ='
    WRITE(*,'(a,3F10.4,a)')' = Lame’s first parameter  | ', La1v,La1r,La1h,'  ='
    WRITE(*,'(a,3F10.4,a)')' = Lame’s second parameter | ', La2v,La2r,La2h,'  ='
    WRITE(*,'(a,3F10.4,a,3a)')' = Poisson ratio           | ', nuv,nur,nuh ,'  = ','<--(  ',bdout1,'regime      )' !| WRITE(*,'(a,3F10.4,a,3a)')' = Pugh ratio           | ', kgv,kgr,kgh ,'   = ','<--(  ',bdout2,'regime   )' !|==>article:http://dx.doi.org/10.1080/09500839.2016.1243264
    WRITE(*,'(a,3F10.4,a,3a)')' = Pugh ratio              | ', kgv,kgr,kgh ,'  = ','<--(  ',bdout2,'regime      )' !|==> article:http://dx.doi.org/10.1080/09500839.2016.1243264
    WRITE(*,*)'============================================================='
    WRITE(*,*)''
    WRITE(*,*)'============================================================='
    WRITE(*,*)' Hardness Information      |      Voigt     Reuss     Average      '
    WRITE(*,*)'=============================================================' 
    WRITE(*,'(a,3F10.3,a)')' = Hardness H_1a  (GPa)    | ', H_a1_v,H_a1_r,H_a1_h    ,'  ='
    WRITE(*,'(a,3F10.3,a)')' = Hardness H_1b  (GPa)    | ', H_b1_v,H_b1_r,H_b1_h    ,'  ='
    WRITE(*,'(a,3F10.3,a)')' = Hardness H_2   (GPa)    | ', H_2_v,H_2_r,H_2_h    ,'  ='
    WRITE(*,'(a,3F10.3,a)')' = Hardness H_3   (GPa)    | ', H_3_v,H_3_r,H_3_h    ,'  ='
    WRITE(*,'(a,3F10.3,a)')' = Hardness H_4   (GPa)    | ', H_4_v,H_4_r,H_4_h    ,'  ='
    WRITE(*,'(a,3F10.3,a)')' = Hardness H_5   (GPa)    | ', H_5_v,H_5_r,H_5_h    ,'  ='
    WRITE(*,*)'=============================================================' 
    WRITE(*,*)'                    ***  Guide Table ***                    '    
    WRITE(*,*)'-------------------------------------------------------------'
    WRITE(*,*)" General | Cubic | Hexagonal | Orthorhombic | Rhombohedral| "
    WRITE(*,*)'-------------------------------------------------------------'    
    WRITE(*,*)'  H_2    |  H_2  |   H_1b    |    H_2       |     H_2     | <= Insulator'
    WRITE(*,*)'  H_5    |  H_5  | H_1b,H_3  |     -        |     H_2     | <= Semiconductor'
    WRITE(*,*)'  H_4    |  H_1a |   H_4     |    H_4       |     H_4     | <= Metal'
    WRITE(*,*)'-------------------------------------------------------------'   
    WRITE(*,*)''    
    WRITE(99,*)'=========================================================='
    WRITE(99,*)' Elastic properties  |     Voigt     Reuss     Average  '
    WRITE(99,*)'=========================================================='
    WRITE(99,'(a,3F10.3,a)')' = Bulk modulus  (GPa)| ', kv,kr,kh    ,'   ='
    WRITE(99,'(a,3F10.3,a)')' = Shear modulus (GPa)| ', gv,gr,gh    ,'   ='
    WRITE(99,'(a,3F10.3,a)')' = Young modulus (GPa)| ', Ev,Er,Eh    ,'   ='
    WRITE(99,'(a,3F10.4,a)')' = P-wave modulus(GPa)| ', mv,mr,mh    ,'   ='
    WRITE(99,'(a,3F10.4,a)')' = Lame’s first parameter  | ', La1v,La1r,La1h,'  ='
    WRITE(99,'(a,3F10.4,a)')' = Lame’s second parameter | ', La2v,La2r,La2h,'  ='    
    WRITE(99,'(a,3F10.4,a,3a)')' = Poisson ratio      | ', nuv,nur,nuh ,'   = ','<--(  ',bdout1,'regime      )' !|
    WRITE(99,'(a,3F10.4,a,3a)')' = Pugh ratio         | ', kgv,kgr,kgh ,'   = ','<--(  ',bdout2,'regime      )' !|==> article:http://dx.doi.org/10.1080/09500839.2016.1243264
    WRITE(99,*)'=========================================================='
    !Universal anisotropy index (Ranganathan and Ostoja-Starzewski method; PRL 101, 055504 (2008)) 
    !Log-Euclidean anisotropy parameter by Christopher M. Kube, AIP Advances 6, 095209 (2016)
    !Bond bending & Bond stretching : G. Mavko, T. Mukerji, J. Dvorkin, The rock physics handbook, Cambridge university press, 2020.
!   Kleinman parameter   : L. Kleinman, Deformation Potentials in Silicon. I. Uniaxial Strain, Phys. Rev. 128 (1962) 2614–2621. doi:10.1103/PhysRev.128.2614.
!                                                                                URL https://link.aps.org/doi/10.1103/PhysRev.128.2614
    WRITE(*,*)''
    WRITE(*,*)'============================================================='
    WRITE(*,*)' Hardness Information      |      Voigt     Reuss     Average      '
    WRITE(*,*)'=============================================================' 
    WRITE(*,'(a,3F10.3,a)')' = Hardness H_1a  (GPa)     | ', H_a1_v,H_a1_r,H_a1_h    ,'  ='
    WRITE(*,'(a,3F10.3,a)')' = Hardness H_1b  (GPa)     | ', H_b1_v,H_b1_r,H_b1_h    ,'  ='
    WRITE(*,'(a,3F10.3,a)')' = Hardness H_2   (GPa)     | ', H_2_v,H_2_r,H_2_h    ,'  ='
    WRITE(*,'(a,3F10.3,a)')' = Hardness H_3   (GPa)     | ', H_3_v,H_3_r,H_3_h    ,'  ='
    WRITE(*,'(a,3F10.3,a)')' = Hardness H_4   (GPa)     | ', H_4_v,H_4_r,H_4_h    ,'  ='
    WRITE(*,'(a,3F10.3,a)')' = Hardness H_5   (GPa)     | ', H_5_v,H_5_r,H_5_h    ,'  ='
    WRITE(*,*)'=============================================================' 
    WRITE(99,*)''
    WRITE(99,*)'============================================================='
    WRITE(99,*)' Hardness Information      |      Voigt     Reuss     Average      '
    WRITE(99,*)'=============================================================' 
    WRITE(99,'(a,3F10.3,a)')' = Hardness H_1a  (GPa)    | ', H_a1_v,H_a1_r,H_a1_h    ,'  ='
    WRITE(99,'(a,3F10.3,a)')' = Hardness H_1b  (GPa)    | ', H_b1_v,H_b1_r,H_b1_h    ,'  ='
    WRITE(99,'(a,3F10.3,a)')' = Hardness H_2   (GPa)    | ', H_2_v,H_2_r,H_2_h    ,'  ='
    WRITE(99,'(a,3F10.3,a)')' = Hardness H_3   (GPa)    | ', H_3_v,H_3_r,H_3_h    ,'  ='
    WRITE(99,'(a,3F10.3,a)')' = Hardness H_4   (GPa)    | ', H_4_v,H_4_r,H_4_h    ,'  ='
    WRITE(99,'(a,3F10.3,a)')' = Hardness H_5   (GPa)    | ', H_5_v,H_5_r,H_5_h    ,'  ='
    WRITE(99,*)'=============================================================' 
    WRITE(*,*)'                    ***  Guide Table ***                    '    
    WRITE(*,*)'-------------------------------------------------------------'
    WRITE(*,*)" General | Cubic | Hexagonal | Orthorhombic | Rhombohedral| "
    WRITE(*,*)'-------------------------------------------------------------'    
    WRITE(*,*)'  H_2    |  H_2  |   H_1b    |    H_2       |     H_2     | <= Insulator'
    WRITE(*,*)'  H_5    |  H_5  | H_1b,H_3  |     -        |     H_2     | <= Semiconductor'
    WRITE(*,*)'  H_4    |  H_1a |   H_4     |    H_4       |     H_4     | <= Metal'
    WRITE(*,*)'-------------------------------------------------------------'   
    WRITE(99,*)''     
    WRITE(99,*)'                    ***  Guide Table ***                    '    
    WRITE(99,*)'-------------------------------------------------------------'
    WRITE(99,*)" General | Cubic | Hexagonal | Orthorhombic | Rhombohedral| "
    WRITE(99,*)'-------------------------------------------------------------'    
    WRITE(99,*)'  H_2    |  H_2  |   H_1b    |    H_2       |     H_2     | <= Insulator'
    WRITE(99,*)'  H_5    |  H_5  | H_1b,H_3  |     -        |     H_2     | <= Semiconductor'
    WRITE(99,*)'  H_4    |  H_1a |   H_4     |    H_4       |     H_4     | <= Metal'
    WRITE(99,*)'-------------------------------------------------------------'   
    WRITE(99,*)'' 
    AU = (kv/kr) + 5d0*(gv/gr) - 6.0d0
    AL = sqrt(5d0)*2.303d0*log(1 + (AU/5))
    Ac = (gv-gr)/(gv+gr)
    WRITE(*,'(a,3F10.4)')' > Universal anisotropy index (AU)        :', au
    WRITE(*,'(a,3F10.4)')' > Log-Euclidean anisotropy parameter (AL):',al
    WRITE(*,'(a,3F10.4)')' > Chung-Buessem Anisotropy Index (Ac)    :',ac     
    WRITE(*,'(a,F10.4,a,a,a)')' > Kleinman parameter                     : ',kel,'       <--(  ',st_ben,')'                    
    WRITE(*,*)'----------------------------------------------------------'
    WRITE(*,'(a)')'              ---> Cauchy pressure(GPa) <---              '  
    WRITE(*,*)'----------------------------------------------------------'    
    Call coval_metal_Pc(Pc,Cov_met)     
    WRITE(*,'(a,F8.2,a,a,a)')' Cubic       : (PC_a)           =(',Pc,')                  <--( ',Cov_met,')'
    Call coval_metal_Pc(Pc_hex_a,Cov_met) 
    Cov_met_aa=Cov_met
 
    Call coval_metal_Pc(Pc_hex_c,Cov_met)
    Cov_met_c=Cov_met
    WRITE(*,'(a,F8.2,F8.2,a,a,a,a)')' Hexagonal   : (PC_a,PC_c)      =(',Pc_hex_a,Pc_hex_c,')          <--( ',Cov_met_aa ,Cov_met_c,')'
    WRITE(*,'(a,F8.2,F8.2,a,a,a,a)')' Trigonal    : (PC_a,PC_c)      =(',Pc_hex_a,Pc_hex_c,')          <--( ',Cov_met_aa ,Cov_met_c,')'
    WRITE(*,'(a,F8.2,F8.2,a,a,a,a)')' Tetragonal  : (PC_a,PC_c)      =(',Pc_hex_a,Pc_hex_c,')          <--( ',Cov_met_aa ,Cov_met_c,')'
    Call coval_metal_Pc(Pc_orth_a,Cov_met)   
    Cov_met_oa=Cov_met     
    Call coval_metal_Pc(Pc_orth_b,Cov_met)   
    Cov_met_ob=Cov_met 
    Call coval_metal_Pc(Pc_orth_c,Cov_met)   
    Cov_met_oc=Cov_met  
    WRITE(*,'(a,3F8.2,a,3a,a)')' Orthorhombic: (PC_a,PC_b,PC_c) =(',Pc_orth_a,Pc_orth_b,Pc_orth_c,')  <--( ',Cov_met_oa,Cov_met_ob,Cov_met_oc,')'
    WRITE(*,*)'=========================================================='
    WRITE(*,"(a)")'> CLB: Covalent-like bonding'
    WRITE(*,"(a)")'> MLB: Metallic-like bonding'      
    WRITE(*,*)''
    !REfs of Cauchy pressure:
    !Journal of Physics and Chemistry of Solids 138 (2020) 109253
    !Materials Today Communications 26 (2021) 101991
    WRITE(99,'(a,3F10.4)')' > Universal anisotropy index (AU)        :', au
    WRITE(99,'(a,3F10.4)')' > Log-Euclidean anisotropy parameter (AL):',al
    WRITE(99,'(a,3F10.4)')' > Chung-Buessem Anisotropy Index (Ac)    :',ac 
    WRITE(99,'(a,F10.4,a,a21,a)')' > Cauchy pressure(GPa) (Pc)              : ',Pc,'     <--(  ',Cov_met,')'
    WRITE(99,'(a,F10.4,a,a,a)')' > Kleinman parameter                     : ',kel,'       <--(  ',st_ben,')' 
    WRITE(99,*)'----------------------------------------------------------'
    WRITE(99,'(a)')'              ---> Cauchy pressure(GPa) <---              '  
    WRITE(99,*)'----------------------------------------------------------'    
    Call coval_metal_Pc(Pc,Cov_met)     
    WRITE(99,'(a,F8.2,a,a,a)')' Cubic       : (PC_a)           =(',Pc,')                  <--( ',Cov_met,')'
    Call coval_metal_Pc(Pc_hex_a,Cov_met) 
    Cov_met_aa=Cov_met
 
    Call coval_metal_Pc(Pc_hex_c,Cov_met)
    Cov_met_c=Cov_met
    WRITE(99,'(a,F8.2,F8.2,a,a,a,a)')' Hexagonal   : (PC_a,PC_c)      =(',Pc_hex_a,Pc_hex_c,')          <--( ',Cov_met_aa ,Cov_met_c,')'
    WRITE(99,'(a,F8.2,F8.2,a,a,a,a)')' Trigonal    : (PC_a,PC_c)      =(',Pc_hex_a,Pc_hex_c,')          <--( ',Cov_met_aa ,Cov_met_c,')'
    WRITE(99,'(a,F8.2,F8.2,a,a,a,a)')' Tetragonal  : (PC_a,PC_c)      =(',Pc_hex_a,Pc_hex_c,')          <--( ',Cov_met_aa ,Cov_met_c,')'
    Call coval_metal_Pc(Pc_orth_a,Cov_met)   
    Cov_met_oa=Cov_met     
    Call coval_metal_Pc(Pc_orth_b,Cov_met)   
    Cov_met_ob=Cov_met 
    Call coval_metal_Pc(Pc_orth_c,Cov_met)   
    Cov_met_oc=Cov_met  
    WRITE(99,'(a,3F8.2,a,3a,a)')' Orthorhombic: (PC_a,PC_b,PC_c) =(',Pc_orth_a,Pc_orth_b,Pc_orth_c,')  <--( ',Cov_met_oa,Cov_met_ob,Cov_met_oc,')'
    WRITE(99,*)'=========================================================='
    WRITE(99,"(a)")'> CLB: Covalent-like bonding'
    WRITE(99,"(a)")'> MLB: Metallic-like bonding'      
    WRITE(99,*)''
 
END SUBROUTINE

