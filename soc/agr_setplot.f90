
!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 09/08/2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: For 3D matereials; set other format of agr files

SUBROUTINE set_nplot_agr(Nploter,colora)
   IMPLICIT  NONE
        INTEGER :: Nploter
        INTEGER, DIMENSION(6)::colora
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' hidden false'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' type xy'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' symbol 0'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' symbol size 1.000000'
     WRITE(2,'(a,I1,a,I1,a)') '@  s',Nploter,' symbol color ', colora(1)," " !!!!
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' symbol pattern 1'
     WRITE(2,'(a,I1,a,I1,a)') '@  s',Nploter,' symbol fill color ', colora(2)," "!!!
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' symbol fill pattern 0'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' symbol linewidth 1.0'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' symbol linestyle 1'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' symbol char 65'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' symbol char font 0'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' symbol skip 0'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' line type 1'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' line linestyle 1'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' line linewidth 1.2'
     WRITE(2,'(a,I1,a,I1,a)') '@  s',Nploter,' line color ',colora(3)," "!!!
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' line pattern 1'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' baseline type 0'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' baseline off'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' dropline off'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' fill type 0'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' fill rule 0'
     WRITE(2,'(a,I1,a,I1,a)') '@  s',Nploter,' fill color ',colora(4)," "!!   (1)
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' fill pattern 1'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' avalue off'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' avalue type 2'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' avalue char size 1.000000'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' avalue font 0'
     WRITE(2,'(a,I1,a,I1,a)') '@  s',Nploter,' avalue color ',colora(5)," "!!   (1)
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' avalue rot 0'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' avalue format general'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' avalue prec 3'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' avalue prepend ""'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' avalue append ""'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' avalue offset 0.000000 , 0.000000'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' errorbar on'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' errorbar place both'
     WRITE(2,'(a,I1,a,I1,a)') '@  s',Nploter,' errorbar color ',colora(6)," "!!
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' errorbar pattern 1'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' errorbar size 1.000000'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' errorbar linewidth 1.0'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' errorbar linestyle 1'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' errorbar riser linewidth 1.0'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' errorbar riser linestyle 1'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' errorbar riser clip off'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' errorbar riser clip length 0.100000'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' comment "noting!!"'
     WRITE(2,'(a,I1,a)') '@  s',Nploter,' legend  ""'
END SUBROUTINE
