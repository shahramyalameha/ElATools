!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; check zero data for creat agr file. outputs checkzfile4co, checkzfile3co

SUBROUTINE zerofile()
implicit none
DOUBLE PRECISION                             :: x=0.0,y=0.0,z=0.0,k=0.0
integer                                      :: i,j,l=0,o=0,p=0
integer, DIMENSION(18)                       :: zero=0,chek

open(20,file='checkzfile4co')
do i=1,4


    if (i==1) call system (' cat 2dcut_poisson.dat > .2din4co')
  !  if (i==6) call system (' cat 2dpoissonMinn.dat > .2din')
  ! if (i==7) call system (' cat 2dpoissonMinp.dat > .2din')


    if (i==2) call system (' cat  2dcut_shear.dat > .2din4co')
   ! if (i==9) call system (' cat 2dshearMinn.dat > .2din')
   ! if (i==10) call system (' cat 2dshearMinp.dat > .2din')


    if (i==3) call system (' cat 2dcut_sound.dat > .2din4co')
   ! if (i==12) call system (' cat 2dsoundTMmax.dat > .2din')
   ! if (i==13) call system (' cat 2dsoundTMmin.dat > .2din')

    if (i==4) call system (' cat  2dcut_pugh.dat > .2din4co')
    !if (i==17) call system (' cat 2dpughMinn.dat > .2din')
   ! if (i==18) call system (' cat 2dpughMinp.dat > .2din')

        chek=4
        open(26,file='.2din4co')
        do j=1,361
            read(26,*)x,y,z,k
            !y=y+1
            if (y==0.0) then
                l=l+1
                
            ENDIF
            if (z==0.0) then
                o=o+1
            endif
            if (k==0.0) then
                p=p+1
            endif
            
        enddo
        close(26)
        !write(*,*)l,o,p
        if (l==361 .and. o==361 .and. p==361)then
            write(20,*) 1,1,1,i
        else if (l/=361 .and. o==361 .and. p==361)then
            write(20,*) 0,1,1,i
        else if (l==361 .and. o/=361 .and. p==361)then
            write(20,*) 1,0,1,i
        else if (l==361 .and. o==361 .and. p/=361)then
            write(20,*) 1,1,0,i
        else if (l/=361 .and. o/=361 .and. p==361)then
            write(20,*) 0,0,1,i
        else if (l/=361 .and. o==361 .and. p/=361)then
            write(20,*) 0,1,0,i
        else if (l==361 .and. o/=361 .and. p/=361)then
            write(20,*) 1,0,0,i
        else 
            write(20,*) 0,0,0,i
         endif
        
        l=0
        o=0 
        p=0
    
enddo
close(20)

!==================================================================
open(21,file='checkzfile3co')
do i=1,3
    if (i==1) call system (' cat 2dcut_bulk.dat > .2din3co') 
    !if (i==2) call system (' cat 2dbulkMinp.dat > .2din')

    if (i==2) call system (' cat 2dcut_comp.dat > .2din3co')
    !  if (i==4) call system (' cat 2dcompMin.dat > .2din')

    if (i==3) call system (' cat 2dcut_young.dat > .2din3co')
    ! if (i==15) call system (' cat 2dyoungMin.dat > .2din')

    chek=3
    open(3,file='.2din3co')
    do j=1,361
        read(3,*)x,y,z
        !y=y+1
        if (y==0.0) then
            l=l+1
        ENDIF
        if (z==0.0) then
            o=o+1
        endif
    enddo
    close(3)
    if (l==361 .and. o==361)then
        write(21,*) 1,1,i
    else if (l/=361 .and. o==361)then
        write(21,*) 0,1,i
    else if (l==361 .and. o/=361)then
        write(21,*) 1,0,i
    else
        write(21,*) 0,0,i
    ENDIF
    l=0
    o=0
enddo
call system(' rm -rf .2din3co .2din4co')
close(21)
end SUBROUTINE
