
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
                                          mas,massCom,ma_avrag,mtot
      INTEGER                          :: i,j,jnoa,multis,bag,tot_multi=0,argl
      REAL(8),DIMENSION (100)          :: multi
      REAL(8),DIMENSION (100)          :: mass,mass2,mas2
      CHARACTER(len=2),DIMENSION (100) :: atomname
      CHARACTER(len=2)                 :: atomname2
      ChARACTER(len=:), allocatable    :: a
      ChARACTER(len=10)                 ::  namemod=' ', val=' '
      !multi   = 0.d0
      multis  = 0.d0
      mas     = 0.d0
      mass    = 0.d0   
      
  if (command_argument_count() > 0) then
     CALL get_command_argument(1, length=argl)
     allocate(ChARACTER(argl) :: a)
     CALL get_command_argument(1, a)
     READ(a,*) namemod
     val=namemod	

  endif
If (val == "dns") then             
254   WRITE(*,*)"LATTICE PARAMETERS AS a b c"
      WRITE(*,*)"NOTE: LATTICE PARAMETERS IN THE conventional cell."
      WRITE(*,*)" Bohr: 1 / Ang.: 2"
      Read(*,*) bag

     If(bag/=2 .and. bag/=1) then
      call system("clear")
      goto 254
     endif
     WRITE(*,*)"LATTICE PARAMETERS AS a b c:"
      READ(*,*) ai,bi,ci
     If(ai==0 .or. bi==0 .or. ci==0) then
       call system("clear")
       goto 254
     Endif         
255  WRITE(*,*)"TOTAL NUMBER ATOMS IN CELL:"
     READ(*,*) jnoa
     If(jnoa==0) then
       call system("clear")
       goto 255
     Endif  
        
   DO i=1,jnoa

256  WRITE(*,*)"ATOM",i,"(ELEMENT):"
     READ(*,*)atomname(i)
     If(atomname(i)=="") then
       call system("clear")
       goto 256
     Endif           
     CALL find_mass(atomname(i),mass(i),mass2(i))
 
257  WRITE(*,*)"NUMBER OF INEQUEVALENT POSITIONS of :  ",atomname(i)
     READ(*,*) multi(i)
     tot_multi=tot_multi+multi(i)
     If(multi(i)==0) then
       call system("clear")
       goto 257
     Endif         
     mas2(i)=mass(i)*multi(i)
         !WRITE(*,*)mass(i)
   END Do


        Do j=1,jnoa
          massCom=massCom+mas2(j)          
        Enddo

      ma_avrag=(massCom/tot_multi)*10D0**(-23D0)
      !write(*,*)ma_avrag
If(bag==1)  vol0=ai*bi*ci*((0.529177d0)**3.0d0)
If(bag==2)  vol0=ai*bi*ci 
  !massCom=multis*mas
  vol0_0 = vol0
  rho = massCom*10/vol0_0
  rhokgm3=rho*1000
Call sleep(1)
             Write (*,*)"  ===================================================="
             Write (*,1060) '  Atomic Mass from Compound = ',massCom*6.022045d0,' (gr/mol)'
            ! Write (*,1060) 'or Atomic Mass from Compound = ',massCom,'*10^(-23) (gr)' 
             Write (*,1060) '  Volume in unit of cm^3 = ',vol0_0,'*10^(-24) (cm^3)'
             Write (*,1060) '  Mass of Compound = ',massCom,'*10^(-23) (gr)'
            ! Write (*,1060) '  M_avrag of Compound = ',massCom/tot_multi,'*10^(-23) (gr)'       
             Write (*,1060) '  Density of Compound = ' ,rhokgm3,' (kg/m^3)'
             Write (*,1060) '  Density of Compound = ' ,rho,' (gr/cm^3)'
             Write (*,*)"  ===================================================="
 ! call system("clear")

 open(547,file=".rhocom")
  write(547,*)rhokgm3
 close(547)

 
 1060 FORMAT (A30,F14.4,A9)
endif
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
IF (val == "mav") then
355  WRITE(*,*)"TOTAL NUMBER ATOMS IN CELL:"
     READ(*,*) jnoa
     If(jnoa==0) then
       call system("clear")
       goto 355
     Endif  
        
   DO i=1,jnoa

356  WRITE(*,*)"ATOM",i,"(ELEMENT):"
     READ(*,*)atomname(i)
     If(atomname(i)=="") then
       call system("clear")
       goto 356
     Endif           
     CALL find_mass(atomname(i),mass(i),mass2(i))
 
357  WRITE(*,*)"NUMBER OF INEQUEVALENT POSITIONS of :  ",atomname(i)
     READ(*,*) multi(i)
     tot_multi=tot_multi+multi(i)
     If(multi(i)==0) then
       call system("clear")
       goto 357
     Endif         
     mas2(i)=mass(i)*multi(i)
         !WRITE(*,*)mass(i)
   END Do


        Do j=1,jnoa
          massCom=massCom+mas2(j)          
        Enddo

      ma_avrag=(massCom/tot_multi)*10D0**(-23D0)
      !write(*,*)ma_avrag
 mtot=tot_multi
  
    open(548,file=".mavg_com")
     write(548,*)ma_avrag
     write(548,*)mtot !the number of atom per unit volume
    close(548)
ENDIF 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
END program                                                               
   
