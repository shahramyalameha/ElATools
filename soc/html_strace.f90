
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
!===========================
SUBROUTINE start_trace_web(naumbers)
  IMPLICIT NONE
  integer                :: naumbers 
  
if (naumbers==1) then
    WRITE(66,"(a)")' var trace1 ='
    WRITE(66,"(a)")'{'
endif
if (naumbers==2) then
    WRITE(66,"(a)")' var trace2 ='
    WRITE(66,"(a)")'{'
endif
if (naumbers==3) then
    WRITE(66,"(a)")' var trace3 ='
    WRITE(66,"(a)")'{'
endif
END SUBROUTINE
!!----------------
SUBROUTINE start_traceslice_web(naumbers, tag0, labtag)
  IMPLICIT NONE
  integer                :: naumbers, labtag
  ChARACTER(len=11)      :: tag0
 IF(labtag==0) THEN
    if (naumbers==1) then
     WRITE(66,"(a)")' var trace1 ='
     WRITE(66,"(a)")'{'
    endif   
    if (naumbers==2) then
     WRITE(66,"(a)")' var trace2 ='
     WRITE(66,"(a)")'{'
    endif
    if (naumbers==3) then
     WRITE(66,"(a)")' var trace3 ='
     WRITE(66,"(a)")'{'
    endif
  ENDIF 

 IF(labtag==1) THEN
    if (naumbers==1) then
     ! write(*,*)labtag,tag0

     WRITE(66,"(a)")' var trace1 ='
     WRITE(66,"(a)")'{'
     IF(tag0=="max") WRITE(66,"(a)") "name: 'Max.',"
     IF(tag0=="min") WRITE(66,"(a)") "name: 'Min.',"
     IF(tag0=="neg") WRITE(66,"(a)") "name: 'Negative',"
    endif   
    if (naumbers==2) then
     WRITE(66,"(a)")' var trace2 ='
     WRITE(66,"(a)")'{'
     IF(tag0=="max") WRITE(66,"(a)")"name: 'Max.',"
     IF(tag0=="min") WRITE(66,"(a)")"name: 'Min.',"
     IF(tag0=="neg") WRITE(66,"(a)")"name: 'Negative',"
    endif
    if (naumbers==3) then
     WRITE(66,"(a)")' var trace3 ='
     WRITE(66,"(a)")'{'
     IF(tag0=="max") WRITE(66,"(a)")"name: 'Max.',"
     IF(tag0=="min") WRITE(66,"(a)")"name: 'Min.',"
     IF(tag0=="neg") WRITE(66,"(a)")"name: 'Negative',"
    endif
  ENDIF
END SUBROUTINE
!!----------------
SUBROUTINE start_traceslice_web_veloc(naumbers, npro, tag0, labtag)
  IMPLICIT NONE
  integer                :: naumbers, labtag
  ChARACTER(len=10)      :: tag0, npro
 IF(labtag==1 ) THEN
    if (naumbers==1) then
     WRITE(66,"(a)")' var trace1 ='
     WRITE(66,"(a)")'{'
     IF(tag0=="pp" .and. npro == "pp3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pf" .and. npro == "pp3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="ps" .and. npro == "pp3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pp" .and. npro == "pf3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pf" .and. npro == "pf3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="ps" .and. npro == "pf3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pp" .and. npro == "ps3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pf" .and. npro == "ps3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="ps" .and. npro == "ps3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pp" .and. npro == "pa3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pf" .and. npro == "pa3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="ps" .and. npro == "pa3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"
!#########################
     IF(tag0=="gp".and. npro == "gp3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="gf".and. npro == "gp3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="gs".and. npro == "gp3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="gp".and. npro == "gf3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="gf".and. npro == "gf3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="gs".and. npro == "gf3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="gp".and. npro == "gs3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="gf".and. npro == "gs3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="gs".and. npro == "gs3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="gp".and. npro == "ga3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="gf".and. npro == "ga3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="gs".and. npro == "ga3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"
!#########################
     IF(tag0=="pfp".and. npro == "pfp3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pff".and. npro == "pfp3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="pfs".and. npro == "pfp3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pfp".and. npro == "pff3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pff".and. npro == "pff3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="pfs".and. npro == "pff3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pfp".and. npro == "pfs3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pff".and. npro == "pfs3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="pfs".and. npro == "pfs3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pfp".and. npro == "pfa3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pff".and. npro == "pfa3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="pfs".and. npro == "pfa3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"          

    endif   
    if (naumbers==2) then
     WRITE(66,"(a)")' var trace2 ='
     WRITE(66,"(a)")'{'
     IF(tag0=="pp" .and. npro == "pp3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pf" .and. npro == "pp3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="ps" .and. npro == "pp3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pp" .and. npro == "pf3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pf" .and. npro == "pf3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="ps" .and. npro == "pf3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pp" .and. npro == "ps3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pf" .and. npro == "ps3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="ps" .and. npro == "ps3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pp" .and. npro == "pa3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pf" .and. npro == "pa3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="ps" .and. npro == "pa3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"
!#########################
     IF(tag0=="gp".and. npro == "gp3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="gf".and. npro == "gp3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="gs".and. npro == "gp3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="gp".and. npro == "gf3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="gf".and. npro == "gf3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="gs".and. npro == "gf3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="gp".and. npro == "gs3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="gf".and. npro == "gs3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="gs".and. npro == "gs3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="gp".and. npro == "ga3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="gf".and. npro == "ga3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="gs".and. npro == "ga3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"
!#########################
     IF(tag0=="pfp".and. npro == "pfp3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pff".and. npro == "pfp3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="pfs".and. npro == "pfp3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pfp".and. npro == "pff3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pff".and. npro == "pff3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="pfs".and. npro == "pff3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pfp".and. npro == "pfs3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pff".and. npro == "pfs3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="pfs".and. npro == "pfs3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pfp".and. npro == "pfa3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pff".and. npro == "pfa3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="pfs".and. npro == "pfa3ds" ) WRITE(66,"(a)") "name: 'Fast-mode'," 
    endif
    if (naumbers==3) then
     WRITE(66,"(a)")' var trace3 ='
     WRITE(66,"(a)")'{'
     IF(tag0=="pp" .and. npro == "pp3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pf" .and. npro == "pp3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="ps" .and. npro == "pp3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pp" .and. npro == "pf3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pf" .and. npro == "pf3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="ps" .and. npro == "pf3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pp" .and. npro == "ps3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pf" .and. npro == "ps3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="ps" .and. npro == "ps3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pp" .and. npro == "pa3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pf" .and. npro == "pa3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="ps" .and. npro == "pa3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"
!#########################
     IF(tag0=="gp".and. npro == "gp3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="gf".and. npro == "gp3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="gs".and. npro == "gp3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="gp".and. npro == "gf3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="gf".and. npro == "gf3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="gs".and. npro == "gf3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="gp".and. npro == "gs3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="gf".and. npro == "gs3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="gs".and. npro == "gs3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="gp".and. npro == "ga3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="gf".and. npro == "ga3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="gs".and. npro == "ga3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"
!#########################
     IF(tag0=="pfp".and. npro == "pfp3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pff".and. npro == "pfp3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="pfs".and. npro == "pfp3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pfp".and. npro == "pff3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pff".and. npro == "pff3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="pfs".and. npro == "pff3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pfp".and. npro == "pfs3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pff".and. npro == "pfs3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="pfs".and. npro == "pfs3ds" ) WRITE(66,"(a)") "name: 'Fast-mode',"

     IF(tag0=="pfp".and. npro == "pfa3ds" ) WRITE(66,"(a)") "name: 'P-mode',"
     IF(tag0=="pff".and. npro == "pfa3ds" ) WRITE(66,"(a)") "name: 'Slow-mode',"
     IF(tag0=="pfs".and. npro == "pfa3ds" ) WRITE(66,"(a)") "name: 'Fast-mode'," 

     IF(tag0=="max".and. npro == "km3ds" ) WRITE(66,"(a)") "name: 'Min. thermal conductivity'," 

    endif
ENDIF


END SUBROUTINE