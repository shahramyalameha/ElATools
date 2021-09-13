subroutine aip_get_online(mpid)
implicit none
character (len=175), dimension(17) :: char3
character (len=16)                 :: key,mpid
INTEGER                            :: i
key='00000000'
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

