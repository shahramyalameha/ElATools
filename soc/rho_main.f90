
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; calculatet rho main version.
 
      PROGRAM RHO_CAL
       REAL(8)                         :: vol0,&
                                          vatomic,&
                                          vol0_0,&
                                          ai,bi,ci,&
                                          rho,rhokgm3,&
                                          mas,massCom
      INTEGER                          :: i,j,jnoa,multis
      REAL(8),DIMENSION (100)          :: multi
      REAL(8),DIMENSION (100)          :: mass  ,mas2
      CHARACTER(len=2),DIMENSION (100) :: atomname
      CHARACTER(len=2)                 :: atomname2

      massCom = 0.d0
      !multi   = 0.d0
      multis  = 0.d0
      mas     = 0.d0
      mass    = 0.d0        
      WRITE(*,*)"LATTICE PARAMETERS AS a b c (Bohr)"
       WRITE(*,*)"NOTE: LATTICE PARAMETERS IN THE conventional cell."
         READ(*,*) ai,bi,ci
      WRITE(*,*)"TOTAL NUMBER INEQUEVALENT ATOMS:"
        READ(*,*) jnoa
      DO i=1,jnoa

      WRITE(*,*)"ATOM",i,"(ELEMENT):"
          READ(*,*)atomname(i)
          CALL find_mass(atomname(i),mass(i))
 
      WRITE(*,*)"NUMBER OF INEQUEVALENT POSITIONS of :  ",atomname(i)
         READ(*,*) multi(i)
        mas2(i)=mass(i)*multi(i)
         !WRITE(*,*)mass(i)
        END Do


        Do j=1,jnoa
          massCom=massCom+mas2(j)
          
        enddo

  vol0=ai*bi*ci
  !massCom=multis*mas
  vol0_0 = vol0*(0.529177d0)**3
  rho = massCom*10/vol0_0
  rhokgm3=rho*1000
  call system("clear")
             Write (*,*)"===================================================="
             Write (*,1060) 'Atomic Mass from Compound = ',massCom*6.022045d0,' (gr/mol)'
             Write (*,1060) 'or Atomic Mass from Compound = ',massCom,'*10^(-23) (gr)' 
             Write (*,1060) 'Volume in unit of cm^3 = ',vol0_0,'*10^(-24) (cm^3)'
             Write (*,1060) 'Mass of Compound = ',massCom,'*10^(-23) (gr)'
             Write (*,1060) 'Density of Compound = ' ,rhokgm3,' (kg/m^3)'
              Write (*,1060) 'Density of Compound = ' ,rho,' (gr/cm^3)'
             Write (*,*)"===================================================="

 
open(547,file=".rhocom")
write(547,*)rhokgm3
 close(547)
 1060 FORMAT (A30,F14.4,A9)
 
      END                                                               
   
