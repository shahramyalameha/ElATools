!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: FOR 3D MATERIAL, Detection of Ductile/Brittle and Covalent-like bonding/Metallic-like bonding and bond_stretching_bending in materials.

SUBROUTINE ductiletester_KG(kgh,bdout)
 IMPLICIT NONE
  CHARACTER(LEN=10) :: bdout ! Brittle and Ductile output! 
  DOUBLE PRECISION  :: kgh
	IF(abs(kgh) > 1.75d0)then
		bdout= "Ductile"
  !	WRITE(*,*)bdout
  ELSE
    bdout= "Brittle"
    !WRITE(*,*)bdout
  ENDIF
END SUBROUTINE ductiletester_KG
!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE ductiletester_PR(nuh,bdout)
  IMPLICIT NONE
  CHARACTER(LEN=10) :: bdout
  DOUBLE PRECISION :: nuh
  IF(1d0/8d0 <nuh .or. nuh<2d0/7d0 )then
    bdout= "Brittle"
    !WRITE(*,*)bdout
  ELSE
    IF(2d0/7d0<nuh .or. nuh<=1d0/2d0 )then
      bdout= "Ductile"
      !WRITE(*,*)bdout
    ENDIF
  ENDIF 
END SUBROUTINE ductiletester_PR
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE coval_metal_Pc(Pc,Cov_met)
  CHARACTER(LEN=5)  :: Cov_met
  DOUBLE PRECISION   :: Pc
 IF (Pc == 0.0D0 ) then
      Cov_met= " - "
  ELsE
  IF (Pc < 0.0D0) then
    Cov_met= "ClB"
  ELSE
    IF (Pc> 0.0D0 ) then
      Cov_met= "MlB"
    ENDIF
  ENDIF
 ENDIF
END SUBROUTINE coval_metal_Pc   
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE bond_stretching_bending (kel, st_ben)
 IMPLICIT NONE
  CHARACTER(LEN=31) :: st_ben
  DOUBLE PRECISION  :: kel
  IF (kel == 0.5D0) st_ben = "Bond bending = Bond stretching"
  IF (kel >  0.5D0) st_ben = "Bond bending < Bond stretching"
  IF (kel <  0.5D0) st_ben = "Bond bending > Bond stretching"
END SUBROUTINE bond_stretching_bending
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  


   
   
   
   
   
   
   
   
   
   
   
   
   
