!```````````````````````````````````````````````````````````````````````````````````````````
! Copyright (c) 2018 Shahram Yalameha <yalameha93@gmail.com> , <sh.yalameha@sci.ui.ac.ir>, `
!               Please report bugs or suggestions to:  yalameha93@gmail.com                `
!                                                                                          `
!```````````````````````````````````````````````````````````````````````````````````````````
! SUBROUTINE: fOR 3D MATERIAL, the core of the hardness calculation..
 
!    refs:
!        Mazhnik's model: J. Appl. Phys. 126, 125109 (2019)
!        Hv[Mazhnik] = 0.096*((1-8.5*v+19.5*v^2)*E)/(1-7.5*v+12.2*v^2+19.6*v^3).

!        Chen's model: Intermetallics 19 (2011) 1275e1281
!        Hv[chen]    = 2*(k^2*G)^0.585-3, where k = G/B.

!        Tian's model: Int. Journal of Refractory Metals and Hard Materials 33 (2012) 93�C106
!        Hv[Tian]    = 0.92*k^1.137*G^0.708, where k = G/B.

!        Liao's model: ref:https://zenodo.org/record/3376758#.YLUzVppLjjE
!        H = 0.786835118497608*G.^(0.7343).*k.^1.01993441066304


SUBROUTINE CHardness(B, E, G_min, G_max, nue_min, nue_max , Max_hard, Min_hard, hard_var, method_hard)
        
    DOUBLE PRECISION, PARAMETER                    ::  const1 = 0.130548175274347D0,&
                                                       const2 = 2.2484942942017D0,  &
                                                       const3 = -1.51675853808829D0
                                                       
    DOUBLE PRECISION                               ::  B, E,G_min, G_max, nue_min, nue_max, hard_var,har_M,har_C,har_T,har_L, nue,&
                                                       m1_tem, m2_tem, m3_tem, m4_tem, m5_tem,m6_tem,m7_tem,&
                                                       c1_tem, c2_tem, c3_tem, c4_tem,c5_tem,&
                                                       t1_tem, t2_tem, t3_tem, t4_tem,t5_tem,&
                                                       Max_hard,Min_hard
    Character(Len=1) :: method_hard
    IF (method_hard .EQ. "L") THEN
    
       har_L = const1*(E**const2)*(B**const3)
       hard_var = har_L
    ENDIF
    
    IF (method_hard .EQ. "M") THEN
      m1_tem = 8.5D0*nue_min
      m2_tem = 19.5D0*(nue_min**2)
      m3_tem = 7.5D0*nue_min
      m4_tem = 12.2D0*(nue_min**2)
      m5_tem = 19.6D0*(nue_min**3)
      !-------------
      m6_tem = (1D0 - m1_tem + m2_tem)* E
      m7_tem = (1.D0 - m3_tem + m4_tem + m5_tem)
      har_M  = 0.096D0 * ( (m6_tem)/(m7_tem) )
      
      !har_M = 0.096D0*((1D0-(8.5D0*nue) + 19.5D0*(nue**2)* E)/(1.D0-7.5D0*nue + 12.2D0*(nue**2)+19.6D0*(nue**3))) 
      Max_hard = har_m
 !=========================================================
       m1_tem = 8.5D0*nue_max
      m2_tem = 19.5D0*(nue_max**2)
      m3_tem = 7.5D0*nue_max
      m4_tem = 12.2D0*(nue_max**2)
      m5_tem = 19.6D0*(nue_max**3)
      !-------------
      m6_tem = (1D0 - m1_tem + m2_tem)* E
      m7_tem = (1.D0 - m3_tem + m4_tem + m5_tem)
      har_M  = 0.096D0 * ( (m6_tem)/(m7_tem) )
      
      !har_M = 0.096D0*((1D0-(8.5D0*nue) + 19.5D0*(nue**2)* E)/(1.D0-7.5D0*nue + 12.2D0*(nue**2)+19.6D0*(nue**3))) 
      Min_hard = har_m
      
    END IF

    
    IF (method_hard .EQ. "C") THEN
      c1_tem = G_max**3
      c2_tem = B*B
      c3_tem = (c1_tem/c2_tem)**(0.585d0)
      c4_tem = 2.0d0 * c3_tem
      har_C  = c4_tem - 3.0d0
      !har_C = 2D0*((G**3/B**2)**0.585D0)-3D0
       Max_hard = har_C
!====================================================================
      c1_tem = G_min**3
      c2_tem = B*B
      c3_tem = (c1_tem/c2_tem)**(0.585d0)
      c4_tem = 2.0d0 * c3_tem
      har_C  = c4_tem - 3.0d0
      !har_C = 2D0*((G**3/B**2)**0.585D0)-3D0
       Min_hard = har_C      

        
    ENDIF
    
    IF (method_hard .EQ. "T") THEN
      t1_tem = (G_max/B)**1.137D0
      t2_tem = G_max**0.708D0
      har_T  = 0.92D0*t2_tem*t1_tem
     ! har_T = 0.92D0*( ((G/B)**1.137D0) * (G**0.708D0) )
      Max_hard = har_T
!======================================================================
      t1_tem = (G_min/B)**1.137D0
      t2_tem = G_min**0.708D0
      har_T  = 0.92D0*t2_tem*t1_tem
     ! har_T = 0.92D0*( ((G/B)**1.137D0) * (G**0.708D0) )
      Min_hard = har_T      

    ENDIF
    
    END subroutine
    
