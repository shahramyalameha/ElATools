Subroutine databank(id,yesno)
implicit none
character(len=20) ::id
character(len=1) ::yesno
 character(len=20)::id0 
character(len=20), DIMENSION(20000000) ::id2,id3 
!character(len=50), DIMENSION(10000) :: c1,c2,c3,c4,c5,c6
real, DIMENSION(10000000)::c1,c2,c3,c4,c5,c6,cc
integer::i,j,stat
!read(*,*)id 
     call system("clear")
 open (12,file='/home/shahram/Desktop/Cubelast/code/programMY/AAEP/soc/eatools_v1.5.2/db/All_2ID_cop.csv')
do  i=1,13122
   read(12,* )id2(i)
   if (id2(i)==id )then
     write(*,*)"Cij of ",id2(i),"compound is:  "
     write(*,*)"======================================================================="
     cc(1)=i
   endif
enddo
 close (12)

 i=0
 open (11,file='/home/shahram/Desktop/Cubelast/code/programMY/AAEP/soc/eatools_v1.5.2/db/Cijs.binery')
do  i=1,2043900
   read(11,'(Z16)',IOSTAT=stat )c1(i) ,c2(i),c3(i),c4(i),c5(i),c6(i)
 !   write(14,'(B64)')c1(j) ,c2(j),c3(j),c4(j),c5(j),c6(j)
   if (stat<0) exit
enddo
  close(11)
  
 open (14,file='Cij-id.dat')
 cc(2)=6*cc(1)-5
 cc(3)=6*cc(1)
     if (c1(cc(2))==0 )then
      WRITE(*,*)"----------------------------------------------"
      write(*,*)"Sorry! This ID was not found in our database  "

      yesno='N'
      goto 1258
      else
      yesno="Y"
   endif
 do j=cc(2), cc(3)

 write(*,"(7F12.6)")c1(j) ,c2(j),c3(j),c4(j),c5(j),c6(j)
 write(14,"(7F12.6)" )c1(j) ,c2(j),c3(j),c4(j),c5(j),c6(j)
enddo
 close(14)
      write(*,*)"======================================================================="
!1361 WRITE(*,*) "not found data-bank file!"
1258 END Subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

