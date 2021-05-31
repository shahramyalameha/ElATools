!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the hardness calculation..
! H = 0.786835118497608*G.^(0.7343).*k.^1.01993441066304; see ref:https://zenodo.org/record/3376758#.YLUzVppLjjE


SUBROUTINE CHardness(B,E,hardvar)
        
    DOUBLE PRECISION, PARAMETER                    ::  const1 = 0.130548175274347D0,&
                                                       const2 = 2.2484942942017D0,  &
                                                       const3 = -1.51675853808829D0
    DOUBLE PRECISION                               ::  B, E, hardvar
    hardvar = const1*(E**const2)*(B**const3)


    end subroutine
