
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2021 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
!===========================
SUBROUTINE color_settings_web(namepro, naumbers, cval1, cval2, cval3)
   IMPLICIT NONE
   ChARACTER(len=10)                :: namepro, cval1, cval2, cval3
   integer                        :: naumbers

   !--------------------
   if (namepro == "young2d" .and. naumbers == 1) then
      IF (cval1 == "n") then
         WRITE (66, "(a)") "line: {color: '#1FCB00'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "bulk2d" .and. naumbers == 1) then
      IF (cval1 == "n") then
         WRITE (66, "(a)") "line: {color: 'green'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "pugh2d" .and. naumbers == 1) then
      IF (cval1 == "n") then
         WRITE (66, "(a)") "line: {color: '#4BDB24'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Max. Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
   if (namepro == "pugh2d" .and. naumbers == 2) then
      IF (cval2 == "n") then
         WRITE (66, "(a)") "line: {color: 'blue'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval2, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Min. Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "shear2d" .and. naumbers == 1) then
      IF (cval1 == "n") then
         WRITE (66, "(a)") "line: {color: '#4BDB24'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Max. Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
   if (namepro == "shear2d" .and. naumbers == 2) then
      IF (cval2 == "n") then
         WRITE (66, "(a)") "line: {color: 'blue'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval2, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Min. Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "com2d" .and. naumbers == 1) then
   IF (cval1 == "n") then
      WRITE (66, "(a)") "line: {color: '#4BDB24'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Positive',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   if (namepro == "com2d" .and. naumbers == 2) then
      IF (cval2 == "n") then
         WRITE (66, "(a)") "line: {color: 'red'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval2, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Negative',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
   !==================
   !==================
   if (namepro == "poi2d" .and. naumbers == 1) then
   IF (cval1 == "n") then
      WRITE (66, "(a)") "line: {color: '#4BDB24'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Max. Positive',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   if (namepro == "poi2d" .and. naumbers == 2) then
      IF (cval2 == "n") then
         WRITE (66, "(a)") "line: {color: '#003DFC'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval2, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Min. Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
   if (namepro == "poi2d" .and. naumbers == 3) then
   IF (cval3 == "n") then
      WRITE (66, "(a)") "line: {color: 'red'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval3, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Negative',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "hard2d" .and. naumbers == 1) then
   IF (cval1 == "n") then
      WRITE (66, "(a)") "line: {color: '#4BCB24'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Max. Positive',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   if (namepro == "hard2d" .and. naumbers == 2) then
      IF (cval2 == "n") then
         WRITE (66, "(a)") "line: {color: 'blue'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval2, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Min. Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "pp2d" .and. naumbers == 1) then
   IF (cval1 == "n") then
      WRITE (66, "(a)") "line: {color: '#660066'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Positive',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   !==================
   !==================
   if (namepro == "pf2d" .and. naumbers == 1) then
   IF (cval1 == "n") then
      WRITE (66, "(a)") "line: {color: '#AA40FC'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Positive',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "ps2d" .and. naumbers == 1) then
   IF (cval1 == "n") then
      WRITE (66, "(a)") "line: {color: '#316395'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Positive',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "gp2d" .and. naumbers == 1) then
   IF (cval1 == "n") then
      WRITE (66, "(a)") "line: {color: '#660000'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Positive',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   !==================
   !==================
   if (namepro == "gf2d" .and. naumbers == 1) then
   IF (cval1 == "n") then
      WRITE (66, "(a)") "line: {color: '#F00000'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Positive',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "gs2d" .and. naumbers == 1) then
   IF (cval1 == "n") then
      WRITE (66, "(a)") "line: {color: '#ff5f0e'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Positive',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "pfp2d" .and. naumbers == 1) then
   IF (cval1 == "n") then
      WRITE (66, "(a)") "line: {color: '#4D20B3'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Positive',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   !==================
   !==================
   if (namepro == "pff2d" .and. naumbers == 1) then
   IF (cval1 == "n") then
      WRITE (66, "(a)") "line: {color: '#B3204D'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Positive',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "pfs2d" .and. naumbers == 1) then
   IF (cval1 == "n") then
      WRITE (66, "(a)") "line: {color: '#309D0D'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Positive',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "km2d" .and. naumbers == 1) then
   IF (cval1 == "n") then
      WRITE (66, "(a)") "line: {color: '#B22471'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Positive',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
   !----------------------------2D sys. -----------------------------------------------
   if (namepro == "2dlong" .and. naumbers == 1) then
      IF (cval1 == "n") then
         WRITE (66, "(a)") "line: {color: '#4b0096'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if

   if (namepro == "2dtran" .and. naumbers == 1) then
      IF (cval1 == "n") then
         WRITE (66, "(a)") "line: {color: '#109900'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
      
   if (namepro == "2dyoung" .and. naumbers == 1) then
      IF (cval1 == "n") then
         WRITE (66, "(a)") "line: {color: '#1FCB00'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "2dshear" .and. naumbers == 1) then
      IF (cval1 == "n") then
         WRITE (66, "(a)") "line: {color: '#4BDB24'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Max. Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
   if (namepro == "2dshear" .and. naumbers == 2) then
      IF (cval2 == "n") then
         WRITE (66, "(a)") "line: {color: 'blue'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval2, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Min. Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
   !==================
   if (namepro == "2dpoi" .and. naumbers == 1) then
      IF (cval1 == "n") then
         WRITE (66, "(a)") "line: {color: '#4BDB24'},"
      ELsE
         WRITE (66, "(5a)") "line: {color: '", cval1, "'},"
      END IF
      WRITE (66, "(a)") "   mode: 'lines',"
      WRITE (66, "(a)") "  name: 'Max. Positive',"
      WRITE (66, "(a)") "  type: 'scatterpolar'"
      WRITE (66, "(a)") "};"
   end if
   if (namepro == "2dpoi" .and. naumbers == 2) then
   IF (cval2 == "n") then
      WRITE (66, "(a)") "line: {color: 'red'},"
   ELsE
      WRITE (66, "(5a)") "line: {color: '", cval2, "'},"
   END IF
   WRITE (66, "(a)") "   mode: 'lines',"
   WRITE (66, "(a)") "  name: 'Negative',"
   WRITE (66, "(a)") "  type: 'scatterpolar'"
   WRITE (66, "(a)") "};"
   end if
END SUBROUTINE
