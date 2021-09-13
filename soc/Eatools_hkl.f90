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
    ! WRITE(*,"(A)")yn_veloc
if(h.eq.0.and.k.eq.0.and.l.eq.1)then
    e1="X"
    e2="Y"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,"(A1)")yn_veloc
    WRITE(*,"(1A,A1,A1,A5)") "The plane you selected: ",e1,"-",e2
else if(h.eq.0.and.k.eq.1.and.l.eq.0)then
    e1="X"
    e2="Z"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,"(A1)")yn_veloc
    WRITE(*,"(1A,A1,A1,A5)") "The plane you selected: ",e1,"-",e2
else if(h.eq.1.and.k.eq.0.and.l.eq.0)then
    e1="Z"
    e2="Y"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,"(A1)")yn_veloc
    WRITE(*,"(1A,A1,A1,A5)") "The plane you selected: ",e1,"-",e2
else if(h.eq.1.and.k.eq.1.and.l.eq.0)then
    e1="[001]"
    e2="[1-10]"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,"(A1)")yn_veloc
    WRITE(*,"(1A,A6,A1,A6)") "The plane you selected: ",e1,"-",e2
else if(h.eq.1.and.k.eq.0.and.l.eq.1)then
    e1="[010]"
    e2="[10-1]"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,"(A1)")yn_veloc
    WRITE(*,"(1A,A6,A1,A6)") "The plane you selected: ",e1,"-",e2
else if(h.eq.0.and.k.eq.1.and.l.eq.1)then
    e1="[100]"
    e2="[0-11]"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,"(A1)")yn_veloc
    WRITE(*,"(1A,A6,A1,A6)") "The plane you selected: ",e1,"-",e2
else
    e1="NON"
    e2="NON"
    WRITE(52,*)E1
    WRITE(52,*)E2
    WRITE(52,"(I2)")IDINT(h) 
    WRITE(52,"(I2)")IDINT(k)
    WRITE(52,"(I2)")IDINT(l)
    WRITE(52,"(A1)")yn_veloc
    WRITE(*,"(1A,A6,A1,A6)") "The plane you selected: ",e1,"-",e2
end if
 close(52)
END subroutine
!======================================================================
Subroutine creadfolder(CF1, CF2,h,k,l)
    implicit NONE
    DOUBLE PRECISION                    :: h,k,l
    INTEGER                             :: CF1,CF2!> 1 for pic, 2 for data
 

             
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
    call system('./.CF  ; rm .CF')
    end subroutine
    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Subroutine checkfolder_old(CF1,CF2,d2d3)
    DOUBLE PRECISION                 :: h,k,l
    INTEGER                          :: CF1,CF2,d2d3!> 1 for pic, 2 for data
    ChARACTER(LEN=6)                 :: e1,e2, yesno
      IF(d2d3 == 3) then
        open(625, file="HKL")
        read(625,*)e1
        read(625,*)e1
        read(625,*)h
        read(625,*)k
        read(625,*)l
        read(625,*)yesno
        close(625)
      Endif
    IF(d2d3 == 2) then
      mmx1=0
      kky1=0
      llz1=0
    Endif 
 Open(232, file=".RepF")  
    if(CF1==1 .and. CF2==0) then
       WRITE(232,"(A)")'#!/bin/csh -f'        
        WRITE(232,"(A,3I1,A)")'if (-e  DatFile_',IDINT(h),IDINT(k),IDINT(l),' ) then'
        WRITE(232,"(A,3I1,A)")'if ( ! -e  DatFile_',IDINT(h),IDINT(k),IDINT(l),'_1 ) then'
        WRITE(232,"(A,3I1,A,3I1,A)")' mv   DatFile_',IDINT(h),IDINT(k),IDINT(l),'   DatFile_',IDINT(h),IDINT(k),IDINT(l),'_1'
        WRITE(232,"(A)")" echo '======================================================='"
        WRITE(232,"(A,3I1,A,3I1,A)")' echo "> Saving pervious DatFile_',IDINT(h),IDINT(k),IDINT(l),' directory to DatFile_',IDINT(h),IDINT(k),IDINT(l),'_1" '
        WRITE(232,"(A)")" echo '======================================================='"
        WRITE(232,"(A)")' else'
        WRITE(232,"(A,3I1,A)")'set nt=`ls -n -d  DatFile_',IDINT(h),IDINT(k),IDINT(l),'_* | cut -f3 -d"_" | wc -l`'
        WRITE(232,"(A)")'set j = 1'
        WRITE(232,"(A,3I1,A)")' while ( $j <= $nt )'
        WRITE(232,"(A,3I1,A)")'set nlt=`ls -n -d  DatFile_',IDINT(h),IDINT(k),IDINT(l),'_* | cut -f3 -d"_" | head -$j | tail -1`'
        WRITE(232,"(A,3I1,A)")'echo   DatFile_',IDINT(h),IDINT(k),IDINT(l),'_$nlt is exist!'
        WRITE(232,"(A)")'echo "----------------------------"'
        WRITE(232,"(A)")'sleep 0.2'
        WRITE(232,"(A)")'   @ j++'
        WRITE(232,"(A)")'end'
        WRITE(232,"(A)")' @ n_nex= 1 + $nlt'
        WRITE(232,"(A,3I1,A,3I1,A)")'mv   DatFile_',IDINT(h),IDINT(k),IDINT(l),'   DatFile_',IDINT(h),IDINT(k),IDINT(l),'_$n_nex '
        WRITE(232,"(A)")" echo '======================================================='"
        WRITE(232,"(A,3I1,A,3I1,A)")'echo "> Saving pervious DatFile_',IDINT(h),IDINT(k),IDINT(l),' directory to DatFile_',IDINT(h),IDINT(k),IDINT(l),'_$n_nex" '
        WRITE(232,"(A)")" echo '======================================================='"
        WRITE(232,"(A)")'endif' 
        WRITE(232,"(A)")'else'
        WRITE(232,"(A,3I1,A)")'echo " The DatFile_',IDINT(h),IDINT(k),IDINT(l),' is not exist! Ok."'
        WRITE(232,"(A)")'exit'
        WRITE(232,"(A)")'endif' 
     ENDIF

     if(CF1==0 .and. CF2==1) then
        WRITE(232,"(A)")'#!/bin/csh -f'        
        WRITE(232,"(A,3I1,A)")'if (-e  PicFile_',IDINT(h),IDINT(k),IDINT(l),' ) then'
        WRITE(232,"(A,3I1,A)")'if ( ! -e  PicFile_',IDINT(h),IDINT(k),IDINT(l),'_1 ) then'
        WRITE(232,"(A,3I1,A,3I1,A)")' mv   PicFile_',IDINT(h),IDINT(k),IDINT(l),'   PicFile_',IDINT(h),IDINT(k),IDINT(l),'_1'
        WRITE(232,"(A)")" echo '======================================================='"
        WRITE(232,"(A,3I1,A,3I1,A)")' echo "> Saving pervious PicFile_',IDINT(h),IDINT(k),IDINT(l),' directory to PicFile_',IDINT(h),IDINT(k),IDINT(l),'_1" '
        WRITE(232,"(A)")" echo '======================================================='"
        WRITE(232,"(A)")' else'
        WRITE(232,"(A,3I1,A)")'set nt=`ls -n -d  PicFile_',IDINT(h),IDINT(k),IDINT(l),'_* | cut -f3 -d"_" | wc -l`'
        WRITE(232,"(A)")'set j = 1'
        WRITE(232,"(A,3I1,A)")' while ( $j <= $nt )'
        WRITE(232,"(A,3I1,A)")'set nlt=`ls -n -d  PicFile_',IDINT(h),IDINT(k),IDINT(l),'_* | cut -f3 -d"_" | head -$j | tail -1`'
        WRITE(232,"(A,3I1,A)")'echo   PicFile_',IDINT(h),IDINT(k),IDINT(l),'_$nlt is exist!'
        WRITE(232,"(A)")'echo "----------------------------"'
        WRITE(232,"(A)")'sleep 0.2'
        WRITE(232,"(A)")'   @ j++'
        WRITE(232,"(A)")'end'
        WRITE(232,"(A)")' @ n_nex= 1 + $nlt'
        WRITE(232,"(A,3I1,A,3I1,A)")'mv   PicFile_',IDINT(h),IDINT(k),IDINT(l),'   PicFile_',IDINT(h),IDINT(k),IDINT(l),'_$n_nex '
        WRITE(232,"(A)")" echo '======================================================='"
        WRITE(232,"(A,3I1,A,3I1,A)")'echo "> Saving pervious PicFile_',IDINT(h),IDINT(k),IDINT(l),' directory to PicFile_',IDINT(h),IDINT(k),IDINT(l),'_$n_nex" '
        WRITE(232,"(A)")" echo '======================================================='"
        WRITE(232,"(A)")'endif' 
        WRITE(232,"(A)")'else'
        WRITE(232,"(A,3I1,A)")'echo " The PicFile_',IDINT(h),IDINT(k),IDINT(l),' is not exist! Ok."'
        WRITE(232,"(A)")'exit'
        WRITE(232,"(A)")'endif' 
     ENDIF
 
        close(232)
    call system("chmod +x .RepF")
    call system('./.RepF #; rm .RepF')
 END  subroutine 
