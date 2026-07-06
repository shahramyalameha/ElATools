        !COMPILER-GENERATED INTERFACE MODULE: Wed Jun 15 02:52:18 2022
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TWOD_CALC__genmod
          INTERFACE 
            SUBROUTINE TWOD_CALC(VV11,VV12,VV13,VV22,VV23,VV33,MMX,KKY, &
                                 &LLZ,SMKL,I,PHI,THETA,THETA_POINT,VEC)
              REAL(KIND=8) :: VV11
              REAL(KIND=8) :: VV12
              REAL(KIND=8) :: VV13
              REAL(KIND=8) :: VV22
              REAL(KIND=8) :: VV23
              REAL(KIND=8) :: VV33
              REAL(KIND=8) :: MMX
              REAL(KIND=8) :: KKY
              REAL(KIND=8) :: LLZ
              REAL(KIND=8) :: SMKL
              INTEGER(KIND=4) :: I
              REAL(KIND=8) :: PHI
              REAL(KIND=8) :: THETA
              INTEGER(KIND=4) :: THETA_POINT
              REAL(KIND=8) :: VEC(3)
            END SUBROUTINE TWOD_CALC
          END INTERFACE 
        END MODULE TWOD_CALC__genmod
