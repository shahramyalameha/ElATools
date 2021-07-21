#!/bin/bash
bold=$(tput bold)
normal=$(tput sgr0)
echo " > Specify path for db folder:"
echo > Go to  ${bold}db${normal} folder and run ${bold}pwd${normal}, then copy the path here.
 read  s
echo " > Do you want to use the online database? (y/n) " 
read yn
if [ $yn == 'y' ];then
     echo " >> Enter AIP KEY:"
   read key
  else
   key=`echo 00000000`
  fi
 sleep 1.5
cat > Eatools_db.f90 << EOF
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
 open (12,file='$s/All_2ID_cop.csv')
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
 open (11,file='$s/Cijs.binery')
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

EOF
echo

cat > Eatools_api.f90 << EOF
subroutine aip_get_online(mpid)
implicit none
character (len=175), dimension(17) :: char3
character (len=16)                 :: key,mpid
INTEGER                            :: i
key='$key'
open(13, file="aip.py")
 write(13,"(A)") '#!/usr/bin/env python3'
 write(13,"(3A)")"id_com='",trim(mpid),"'"
 write(13,"(3A)") 'api_key= "',key,'"'
 write(13,"(A)") 'import os'                                                                                                                                                    
 write(13,"(A)") 'from pymatgen import MPRester, Structure'                                                                                                                                      
 write(13,"(A)") 'from pymatgen.analysis.elasticity import ElasticTensor'                                                                                                                        
 write(13,"(A)") 'with MPRester(api_key) as m:  '                                                                                                                                                
 write(13,"(A)") '     data = m.get_data(id_com)[0] '                                                                                                                                            
 write(13,"(A)") "     elastic_tensor = ElasticTensor.from_voigt(data['elasticity']['elastic_tensor'])"                                                                                        
 write(13,"(A)") "     print (elastic_tensor[0][0][0][0],elastic_tensor[0][0][1][1],elastic_tensor[0][0][2][2],elastic_tensor[0][0][1][2],elastic_tensor[0][0][2][0],elastic_tensor[0][0][0][1])"
 write(13,"(A)") "     print (elastic_tensor[1][1][0][0],elastic_tensor[1][1][1][1],elastic_tensor[1][1][2][2],elastic_tensor[1][1][1][2],elastic_tensor[1][1][2][0],elastic_tensor[1][1][0][1])"
 write(13,"(A)") "     print (elastic_tensor[2][2][0][0],elastic_tensor[2][2][1][1],elastic_tensor[2][2][2][2],elastic_tensor[2][2][1][2],elastic_tensor[2][2][2][0],elastic_tensor[2][2][0][1])"
 write(13,"(A)") "     print (elastic_tensor[1][2][0][0],elastic_tensor[1][2][1][1],elastic_tensor[1][2][2][2],elastic_tensor[1][2][1][2],elastic_tensor[1][2][2][0],elastic_tensor[1][2][0][1])"
 write(13,"(A)") "     print (elastic_tensor[2][0][0][0],elastic_tensor[2][0][1][1],elastic_tensor[2][0][2][2],elastic_tensor[2][0][1][2],elastic_tensor[2][0][2][0],elastic_tensor[2][0][0][1])"
 write(13,"(A)") "     print (elastic_tensor[0][1][0][0],elastic_tensor[0][1][1][1],elastic_tensor[0][1][2][2],elastic_tensor[0][1][1][2],elastic_tensor[0][1][2][0],elastic_tensor[0][1][0][1])"
 close(13)
End subroutine

EOF
echo
echo ${bold}The path was well documented${normal}


