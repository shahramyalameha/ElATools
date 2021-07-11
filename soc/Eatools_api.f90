subroutine aip_get_online(mpid)
implicit none
character (len=175), dimension(17) :: char3
character (len=16)                 :: key,mpid
INTEGER                            :: i
key='32O9CtVDAwGz8UZk'
open(12, file="/home/shahram/Desktop/Cubelast/code/programMY/AAEP/soc/eatools_v1.6.4/db/api.bin")
open(13, file="aip.py")
 do i=1,12
    read(12,"(Z)") char3(i)
enddo 
close(12)
write(13,"(A)") '#!/usr/bin/env python3'
write(13,"(3A)")"id_com='",trim(mpid),"'"
write(13,"(3A)") 'api_key= "',key,'"'
do i=2,12
    write(13,"(A)")char3(i)
enddo
close(13)
End subroutine


