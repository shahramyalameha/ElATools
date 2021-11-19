
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 09/08/2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; Detect the max. and main. values in the data.

SUBROUTINE  MinMax(Minimum, Maximum,nameinp)
   IMPLICIT  NONE
   ChARACTER(20)  :: nameinp
   REAL(8)  :: Minimum, Maximum                    ! max and min
   INTEGER  :: Count ,n0                           ! # of data items
   REAL(8)  :: Input,Input2                        ! the input value
   INTEGER, DIMENSION(16)  :: Numzirofile1_4,Numzirofile2_4,Numzirofile3_4,Namzirofile_4,&
                              Numzirofile1_3,Numzirofile2_3,Namzirofile_3
	call zerofile()
	CALL system('if [ ! -e checkzfile3co ]; then echo " >> Please go to DatFile folder and run again... <<"; echo "" ; fi;')
  do n0=1,3
    open(22,file='checkzfile3co')
    READ(22,*) Numzirofile1_3(n0),Numzirofile2_3(n0),Namzirofile_3(n0) 
  enddo
 CLOSE(22)
 do n0=1,4
   open(88,file='checkzfile4co')
   READ(88,*) Numzirofile1_4(n0),Numzirofile2_4(n0),Numzirofile3_4(n0),Namzirofile_4(n0) 
 enddo
CLOSE(88)
   Count = 0 
   
if (nameinp=="poisson")  then

   if (Numzirofile1_4(1)==1 .and. Numzirofile1_4(1)==0 .and. Numzirofile1_4(1)==0 ) then
      CALL I_mixer(nameinp, 1,0,0,4)
      !CALL system('cat 2dpoissonMinn.dat 2dpoissonMinp.dat > 2dpoisson')
   endif
   if (Numzirofile1_4(1)==0 .and. Numzirofile1_4(1)==0 .and. Numzirofile1_4(1)==1 ) then
      CALL I_mixer(nameinp, 0,0,1,4)
      !CALL system(' cat 2dpoissonMax.dat 2dpoissonMinp.dat > 2dpoisson ')
   endif   
   if (Numzirofile1_4(1)==0 .and. Numzirofile1_4(1)==1 .and. Numzirofile1_4(1)==0 ) then
      CALL I_mixer(nameinp, 0,1,0,4)
      !CALL system('cat 2dpoissonMax.dat 2dpoissonMinn.dat > 2dpoisson')
   endif
   if (Numzirofile1_4(1)==0 .and. Numzirofile1_4(1)==0 .and. Numzirofile1_4(1)==0 ) then
      CALL I_mixer(nameinp, 0,0,0,4)
     ! CALL system('cat 2dpoissonM* > 2dpoisson')
   endif 
   
   open(10,file='2dpoisson',ERR=99) 
   WRITE(*,*) " > Search for Poisson data: "
endif
  CALL system(' if [ -e 2dpoisson ]; then rm  2dpoisson; fi')

if (nameinp=="young")  then
   if (Numzirofile1_3(3)==1 .and. Numzirofile2_3(3)==0) then
      CALL I_mixer(nameinp, 1,0,0,3)
     ! CALL system('cat 2dyoungMin.dat > 2dyoung')
   endif
   if (Numzirofile1_3(3)==0 .and. Numzirofile2_3(3)==1) then
      CALL I_mixer(nameinp, 0,1,0,3)
      !CALL system('cat 2dyoungMax.dat > 2dyoung')
   endif
   if (Numzirofile1_3(3)==0 .and. Numzirofile2_3(3)==0) then
      CALL I_mixer(nameinp, 0,0,0,3)
      !CALL system('cat 2dyoungM* > 2dyoung')
   endif    
   open(10,file='2dyoung') 
   WRITE(*,*) " > Search for Young data: "
endif
 CALL system(' if [ -e 2dyoung ]; then rm  2dyoung; fi')

if (nameinp=="comp")  then
   if (Numzirofile1_3(2)==1 .and. Numzirofile2_3(2)==0) then
      CALL I_mixer(nameinp, 1,0,0,3)
      !CALL system('cat 2dcompMin.dat > 2dcomp')
   endif
   if (Numzirofile1_3(3)==0 .and. Numzirofile2_3(3)==1) then
      CALL I_mixer(nameinp, 0,1,0,3)
      !CALL system('cat 2dcompMax.dat > 2dcomp')
   endif
   if (Numzirofile1_3(3)==0 .and. Numzirofile2_3(3)==0) then
      CALL I_mixer(nameinp, 0,0,0,3)
     ! CALL system('cat 2dcompM* > 2dcomp')
   endif
 open(10,file='2dcomp') 
 WRITE(*,*) " > Search for comp. data: "
endif
 CALL system(' if [ -e 2dcomp ]; then rm  2dcomp; fi')

if (nameinp=="bulk")  then
   if (Numzirofile1_3(1)==0 .and. Numzirofile2_3(1)==1) then
      CALL I_mixer(nameinp, 0,1,0,3)
      !CALL system('cat 2dbulkMinp.dat > 2dbulk')
   endif
   if (Numzirofile1_3(1)==1 .and. Numzirofile2_3(1)==0) then
      CALL I_mixer(nameinp, 1,0,0,3)
      !CALL system('cat 2dbulkMaxp.dat > 2dbulk')
   endif
   if (Numzirofile1_3(1)==0 .and. Numzirofile2_3(1)==0) then
      CALL I_mixer(nameinp, 0,0,0,3)
      !CALL system('cat 2dbulkM* > 2dbulk')
   endif
 open(10,file='2dbulk') 
 WRITE(*,*) " > Search for Bulk data: "
endif
 CALL system(' if [ -e 2dbulk ]; then rm  2dbulk; fi')

if (nameinp=="shear")  then
   if (Numzirofile1_4(2)==1 .and. Numzirofile1_4(2)==0 .and. Numzirofile1_4(2)==0 ) then
      CALL I_mixer(nameinp, 1,0,0,4)
      !CALL system('cat 2dshearMinn.dat 2dshearMinp.dat > 2dshear')
   endif
   if (Numzirofile1_4(2)==0 .and. Numzirofile1_4(2)==0 .and. Numzirofile1_4(2)==1 ) then
      CALL I_mixer(nameinp, 0,0,1,4)
      !CALL system('cat 2dshearMax.dat 2dshearMinp.dat > 2dshear')
   endif   
   if (Numzirofile1_4(2)==0 .and. Numzirofile1_4(2)==1 .and. Numzirofile1_4(2)==0 ) then
      CALL I_mixer(nameinp, 0,1,0,4)
      !CALL system('cat 2dshearMax.dat 2dshearMinn.dat > 2dshear')
   endif
   if (Numzirofile1_4(2)==0 .and. Numzirofile1_4(2)==0 .and. Numzirofile1_4(2)==0 ) then
      CALL I_mixer(nameinp, 0,0,0,4)
      !CALL system('cat 2dshearM* > 2dshear')
   endif
 open(10,file='2dshear') 
 WRITE(*,*) " > Search for Shear data: "
endif
 CALL system(' if [ -e 2dshear ]; then rm  2dshear; fi')

if (nameinp=="pugh")  then
   if (Numzirofile1_4(4)==1 .and. Numzirofile1_4(4)==0 .and. Numzirofile1_4(4)==0 ) then
      CALL I_mixer(nameinp, 1,0,0,4)
      !CALL system('cat 2dpughMinn.dat 2dshearMinp.dat > 2dpugh')
   endif
   if (Numzirofile1_4(4)==0 .and. Numzirofile1_4(4)==0 .and. Numzirofile1_4(4)==1 ) then
      CALL I_mixer(nameinp, 0,0,1,4)
     ! CALL system('cat 2dpughMax.dat 2dshearMinp.dat > 2dpugh')
   endif   
   if (Numzirofile1_4(4)==0 .and. Numzirofile1_4(4)==1 .and. Numzirofile1_4(4)==0 ) then
      CALL I_mixer(nameinp, 0,1,0,4)
      !CALL system('cat 2dpughMax.dat 2dshearMinn.dat > 2dpugh')
   endif
   if (Numzirofile1_4(4)==0 .and. Numzirofile1_4(4)==0 .and. Numzirofile1_4(4)==0 ) then
      CALL I_mixer(nameinp, 0,0,0,4)
     ! CALL system('cat 2dpughM* > 2dpugh')
   endif
 open(10,file='2dpugh') 
 WRITE(*,*) " > Search for Pugh data: "
endif
 CALL system(' if [ -e 2dpugh ]; then rm  2dpugh; fi')

if (nameinp=="sound")  then

   if (Numzirofile1_4(3)==1 .and. Numzirofile1_4(3)==0 .and. Numzirofile1_4(3)==0 ) then
      CALL I_mixer(nameinp, 1,0,0,4)
     ! CALL system('cat 2dsoundTMmax.dat 2dsoundTMmin.dat > 2dsound')
   endif
   if (Numzirofile1_4(3)==0 .and. Numzirofile1_4(3)==1 .and. Numzirofile1_4(3)==0 ) then
      CALL I_mixer(nameinp, 0,1,0,4)
      !CALL system('cat 2dsoundLM.dat 2dsoundTMmin.dat > 2dsound')
   endif   
   if (Numzirofile1_4(3)==0 .and. Numzirofile1_4(3)==0 .and. Numzirofile1_4(3)==1 ) then
      CALL I_mixer(nameinp, 0,0,1,4)
     ! CALL system('cat 2dsoundLM.dat 2dsoundTMmax.dat > 2dsound')
   endif
   if (Numzirofile1_4(3)==0 .and. Numzirofile1_4(3)==0 .and. Numzirofile1_4(3)==0 ) then
      CALL I_mixer(nameinp, 0,0,0,4)
     ! CALL system('cat 2dsound* > 2dsound')
   endif
 open(10,file='2dsound') 
 WRITE(*,*) " > Search for Sound data: "
endif
 CALL system(' if [ -e 2dsound ]; then rm  2dsound; fi')

   DO              
      READ(10,*,end=100) Input2, Input                    !   READ in a new input
      IF (Input < 0)  EXIT                        !   if it is < 0, done.
      Count = Count + 1                           !   if >= 0, increase counter
  !WRITE(*,*)  'Data item #', Count, ' = ', Input
      IF (Count == 1) THEN                        !   is this the 1st data?
         Maximum = Input                          !     yes, assume it is the
         Minimum = Input                          !     min and the max
      ELSE                                        !   no, not the 1st data
         IF (Input > Maximum)  Maximum = Input    ! compare against the
         IF (Input < Minimum)  Minimum = Input    ! existing min & max
      END IF
   END DO

100   IF (Count > 0) THEN                            ! if at one data item found
     ! WRITE(*,*)  'Found ', Count, ' data items'
      WRITE(*,'(a,F11.5)')  '  Maximum = ', Maximum
      WRITE(*,'(a,F11.5)')  '  Minimum = ', Minimum
   ELSE
      WRITE(*,*)  'No data item found.'           ! no data item found
   END IF
  CLOSE(10)
 
     99 CALL system (' echo 1 > .err') 
	
END SUBROUTINE MinMax
 
!8888888888888888888888888888888888888888888888888888888888
SUBROUTINE  I_mixer(nameinp, co1,co2,co3,co4)
   IMPLICIT  NONE
   ChARACTER(20)  :: nameinp
   INTEGER        :: co1,co2,co3,co4,i,phi_meah, theta_meah, cutmesh
   DOUBLE PRECISION,DIMENSION(3601)    :: n1,n2,n3,n4
   
   
	open(875, file="MESH")
	 read(875, *) phi_meah, theta_meah, cutmesh
	close(875)
 
IF (CO4==4) THEN
         IF (nameinp=="poisson") THEN
            OPEN(85,FILE='2dcut_poisson.dat')
            OPEN(40,FILE="2dpoisson")
         ENDIF

         IF (nameinp=="shear") THEN
            OPEN(85,FILE='2dcut_shear.dat')
            OPEN(40,FILE="2dshear")
         ENDIF

         IF (nameinp=="pugh") THEN
            OPEN(85,FILE='2dcut_pugh.dat')
            OPEN(40,FILE="2dpugh")
         ENDIF

         IF (nameinp=="sound") THEN
            OPEN(85,FILE='2dcut_sound.dat')
            OPEN(40,FILE="2dsound")
         ENDIF

         DO i=1,phi_meah
            READ(85,*) n1(I),n2(I),n3(I),n4(I)
         END DO
         CLOSE(85)

         if (co1==1 .and. co2==0 .and. co3==0) THEN
             DO I=1, phi_meah
                WRITE(40,"(I3,F35.25)")IDINT(N1(I)),n4(I)
                WRITE(40,"(I3,F35.25)")IDINT(N1(I)),n3(I)
             END DO
         END IF
         if (co1==0 .and. co2==1 .and. co3==0) THEN
            DO I=1, phi_meah
               WRITE(40,"(I3,F35.25)")IDINT(N1(I)),n4(I)
               WRITE(40,"(I3,F35.25)")IDINT(N1(I)),n2(I)
            END DO
        END IF
        if (co1==0 .and. co2==0 .and. co3==1) THEN
         DO I=1, phi_meah
            WRITE(40,"(I3,F35.25)")IDINT(N1(I)),n2(I)
            WRITE(40,"(I3,F35.25)")IDINT(N1(I)),n3(I)
         END DO
        END IF
        if (co1==0 .and. co2==0 .and. co3==0) THEN
         DO I=1, phi_meah
          WRITE(40,"(I3,F35.25)")IDINT(N1(I)),n2(I)
          WRITE(40,"(I3,F35.25)")IDINT(N1(I)),n3(I)
          WRITE(40,"(I3,F35.25)")IDINT(N1(I)),n4(I)
         END DO
        END IF
   CLOSE(40)
ENDIF

IF (CO4==3) THEN
   IF (nameinp=="young") THEN
      OPEN(85,FILE='2dcut_young.dat')
      OPEN(40,FILE="2dyoung")
   ENDIF

   IF (nameinp=="bulk") THEN
      OPEN(85,FILE='2dcut_bulk.dat')
      OPEN(40,FILE="2dbulk")
   ENDIF

   IF (nameinp=="comp") THEN
      OPEN(85,FILE='2dcut_comp.dat')
      OPEN(40,FILE="2dcomp")
   ENDIF      

   DO i=1,phi_meah
      READ(85,*) n1(I),n2(I),n3(I) 
   END DO
   CLOSE(85)

   if (co1==1 .and. co2==0 ) THEN
       DO I=1, phi_meah
          WRITE(40,"(I3,F35.25)")IDINT(N1(I)),n3(I)
          
       END DO
   END IF
   if (co1==0 .and. co2==1) THEN
      DO I=1, phi_meah
         WRITE(40,"(I3,F35.25)")IDINT(N1(I)),n2(I)
      END DO
  END IF
  if (co1==0 .and. co2==0 ) THEN
   DO I=1, phi_meah
      WRITE(40,"(I3,F35.25)")IDINT(N1(I)),n2(I)
      WRITE(40,"(I3,F35.25)")IDINT(N1(I)),n3(I)
   END DO
END IF
CLOSE(40)
ENDIF
call system('rm checkzfile3co checkzfile4co')

END SUBROUTINE

