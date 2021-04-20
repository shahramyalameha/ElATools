!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, creat PicFile-hkl and DatFile.

subroutine findpaln(h,k,l,e1,e2,yn_veloc)
IMPLICIT NONE
DOUBLE PRECISION                    :: h,k,l
ChARACTER(LEN=6)                    :: e1,e2
ChARACTER(LEN=2)                    :: yn_veloc
OPEN(52, FILE="HKL")
    !WRITE(*,*)E1
    !WRITE(*,*)E2
    !WRITE(*,*)h
    !WRITE(*,*)k
     WRITE(*,*)yn_veloc
if(h.eq.0.and.k.eq.0.and.l.eq.1)then
    e1="X"
    e2="Y"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,*)yn_veloc
    WRITE(*,"(1A,A1,A1,A5)") "The plane you selected: ",e1,"-",e2
else if(h.eq.0.and.k.eq.1.and.l.eq.0)then
    e1="X"
    e2="Z"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,*)yn_veloc
    WRITE(*,"(1A,A1,A1,A5)") "The plane you selected: ",e1,"-",e2
else if(h.eq.1.and.k.eq.0.and.l.eq.0)then
    e1="Z"
    e2="Y"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,*)yn_veloc
    WRITE(*,"(1A,A1,A1,A5)") "The plane you selected: ",e1,"-",e2
else if(h.eq.1.and.k.eq.1.and.l.eq.0)then
    e1="[001]"
    e2="[1-10]"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,*)yn_veloc
    WRITE(*,"(1A,A6,A1,A6)") "The plane you selected: ",e1,"-",e2
else if(h.eq.1.and.k.eq.0.and.l.eq.1)then
    e1="[010]"
    e2="[10-1]"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,*)yn_veloc
    WRITE(*,"(1A,A6,A1,A6)") "The plane you selected: ",e1,"-",e2
else if(h.eq.0.and.k.eq.1.and.l.eq.1)then
    e1="[100]"
    e2="[0-11]"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,*)yn_veloc
    WRITE(*,"(1A,A6,A1,A6)") "The plane you selected: ",e1,"-",e2
else
    e1="NON"
    e2="NON"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,*)yn_veloc
    WRITE(*,"(1A,A6,A1,A6)") "The plane you selected: ",e1,"-",e2
end if
 close(52)
END subroutine

subroutine creadfolder(CF1, CF2,h,k,l)
    implicit NONE
    DOUBLE PRECISION                             :: h,k,l
    integer                             :: CF1,CF2!> 1 for pic, 2 for data
    if(CF1==1 .and. CF2==0) then
        open(254, file=".CF")
        WRITE(254,"(A)")'#!/bin/bash'
        WRITE(254,"(2A,3I1)")'mv DatFile '," DatFile_",IDINT(h),IDINT(k),IDINT(l)
        WRITE(254,"(2A,3I1)")'cp HKL '," DatFile_",IDINT(h),IDINT(k),IDINT(l)

        close(254)
    endif
    if(CF1==0 .and. CF2==1) then
        open(254, file=".CF")
        WRITE(254,"(A)")'#!/bin/bash'
        WRITE(254,"(2A,3I1)")'mv PicFile '," PicFile_",IDINT(h),IDINT(k),IDINT(l)
        close(254)
    endif
    call system("chmod +x .CF")
    call system('./.CF; rm .CF')
    end subroutine