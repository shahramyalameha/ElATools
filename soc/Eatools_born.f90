MODULE ElasticAnalysisHelpers
  IMPLICIT NONE
  PRIVATE  

  PUBLIC :: near_equal, is_zero
  PUBLIC :: check_cubic_conditions_met, check_hexagonal_conditions_met, &
            check_tetragonal_I_conditions_met, check_tetragonal_II_conditions_met, &
            check_rhombohedral_I_conditions_met, check_rhombohedral_II_conditions_met, &
            check_orthorhombic_conditions_met, check_monoclinic_conditions_met, &
            check_triclinic_conditions_met
  PUBLIC :: display_cubic_matrix_form, display_hexagonal_matrix_form, &
            display_tetragonal_I_matrix_form, display_tetragonal_II_matrix_form, &
            display_rhombohedral_I_matrix_form, display_rhombohedral_II_matrix_form, &
            display_orthorhombic_matrix_form, display_monoclinic_matrix_form, &
            display_triclinic_matrix_form
  PUBLIC :: check_cubic_stability, check_hexagonal_stability, &
            check_tetragonal_I_stability, check_tetragonal_II_stability, &
            check_rhombohedral_I_stability, check_rhombohedral_II_stability, &
            check_orthorhombic_stability, check_stability_by_leading_minors
  PUBLIC :: calculate_determinant
  PUBLIC :: analyze_elastic_constants
  
!// NEW 2D Procedures
  PUBLIC :: check_hexagonal_2d_conditions_met, check_square_2d_conditions_met, &
            check_rectangular_2d_conditions_met
  PUBLIC :: display_hexagonal_2d_matrix_form, display_square_2d_matrix_form, &
            display_rectangular_2d_matrix_form, display_oblique_2d_matrix_form
  PUBLIC :: check_hexagonal_2d_stability, check_square_2d_stability, &
            check_rectangular_2d_stability, check_oblique_2d_stability
  PUBLIC :: analyze_2d_elastic_constants
CONTAINS

    !-----------------------------------------------------------------------
    ! Helper logical function to check for near equality between two reals.
    !-----------------------------------------------------------------------
    LOGICAL FUNCTION near_equal(val1, val2, local_tol)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: val1, val2, local_tol
        ! Check if values are close, relative to their magnitude
        IF (ABS(val1) < local_tol .AND. ABS(val2) < local_tol) THEN
            near_equal = .TRUE. ! Both are essentially zero
        ELSE
            near_equal = (ABS(val1 - val2) < local_tol * MAX(ABS(val1), ABS(val2), 1.0_8) )
        END IF
    END FUNCTION near_equal

    !-----------------------------------------------------------------------
    ! Helper logical function to check if a real value is effectively zero.
    !-----------------------------------------------------------------------
    LOGICAL FUNCTION is_zero(val, local_tol)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: val, local_tol
        is_zero = (ABS(val) < local_tol)
    END FUNCTION is_zero

    !-----------------------------------------------------------------------
    ! Functions to check if Cij matrix conforms to a crystal system.
    ! Based on Mouhat & Coudert 2014, Table I and matrix forms.
    ! Cm: Input 6x6 Cij matrix
    ! tol: Tolerance for comparisons
    !-----------------------------------------------------------------------

    LOGICAL FUNCTION check_cubic_conditions_met(Cm, tol)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(6,6), INTENT(IN) :: Cm
        REAL(KIND=8), INTENT(IN) :: tol
        check_cubic_conditions_met = .FALSE.
        IF (near_equal(Cm(1,1), Cm(2,2), tol) .AND. near_equal(Cm(1,1), Cm(3,3), tol) .AND. &  ! C11=C22=C33
            near_equal(Cm(1,2), Cm(1,3), tol) .AND. near_equal(Cm(1,2), Cm(2,3), tol) .AND. &  ! C12=C13=C23 (Cm(2,1), Cm(3,1), Cm(3,2) by symmetry)
            near_equal(Cm(4,4), Cm(5,5), tol) .AND. near_equal(Cm(4,4), Cm(6,6), tol) .AND. &  ! C44=C55=C66
            is_zero(Cm(1,4), tol) .AND. is_zero(Cm(1,5), tol) .AND. is_zero(Cm(1,6), tol) .AND. &
            is_zero(Cm(2,4), tol) .AND. is_zero(Cm(2,5), tol) .AND. is_zero(Cm(2,6), tol) .AND. &
            is_zero(Cm(3,4), tol) .AND. is_zero(Cm(3,5), tol) .AND. is_zero(Cm(3,6), tol) .AND. &
            is_zero(Cm(4,5), tol) .AND. is_zero(Cm(4,6), tol) .AND. is_zero(Cm(5,6), tol) ) THEN
            check_cubic_conditions_met = .TRUE.
        END IF
    END FUNCTION check_cubic_conditions_met

    LOGICAL FUNCTION check_hexagonal_conditions_met(Cm, tol)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(6,6), INTENT(IN) :: Cm
        REAL(KIND=8), INTENT(IN) :: tol
        check_hexagonal_conditions_met = .FALSE.
        IF (near_equal(Cm(1,1), Cm(2,2), tol) .AND. &
            near_equal(Cm(1,3), Cm(2,3), tol) .AND. &
            near_equal(Cm(4,4), Cm(5,5), tol) .AND. &
            near_equal(Cm(6,6), (Cm(1,1)-Cm(1,2))/2.0_8, tol) .AND. &
            is_zero(Cm(1,4), tol) .AND. is_zero(Cm(1,5), tol) .AND. is_zero(Cm(1,6), tol) .AND. &
            is_zero(Cm(2,4), tol) .AND. is_zero(Cm(2,5), tol) .AND. is_zero(Cm(2,6), tol) .AND. &
            is_zero(Cm(3,4), tol) .AND. is_zero(Cm(3,5), tol) .AND. is_zero(Cm(3,6), tol) .AND. &
            is_zero(Cm(4,5), tol) .AND. is_zero(Cm(4,6), tol) .AND. is_zero(Cm(5,6), tol) ) THEN
            check_hexagonal_conditions_met = .TRUE.
        END IF
    END FUNCTION check_hexagonal_conditions_met

    LOGICAL FUNCTION check_tetragonal_I_conditions_met(Cm, tol)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(6,6), INTENT(IN) :: Cm
        REAL(KIND=8), INTENT(IN) :: tol
        check_tetragonal_I_conditions_met = .FALSE.
        IF (near_equal(Cm(1,1), Cm(2,2), tol) .AND. &
            near_equal(Cm(1,3), Cm(2,3), tol) .AND. &
            near_equal(Cm(4,4), Cm(5,5), tol) .AND. &
            .NOT. near_equal(Cm(6,6), (Cm(1,1)-Cm(1,2))/2.0_8, tol*5.0_8) .AND. & ! Ensure it's not hexagonal
            is_zero(Cm(1,4), tol) .AND. is_zero(Cm(1,5), tol) .AND. is_zero(Cm(1,6), tol) .AND. &
            is_zero(Cm(2,4), tol) .AND. is_zero(Cm(2,5), tol) .AND. is_zero(Cm(2,6), tol) .AND. &
            is_zero(Cm(3,4), tol) .AND. is_zero(Cm(3,5), tol) .AND. is_zero(Cm(3,6), tol) .AND. &
            is_zero(Cm(4,5), tol) .AND. is_zero(Cm(4,6), tol) .AND. is_zero(Cm(5,6), tol) ) THEN
            check_tetragonal_I_conditions_met = .TRUE.
        END IF
    END FUNCTION check_tetragonal_I_conditions_met

    LOGICAL FUNCTION check_tetragonal_II_conditions_met(Cm, tol)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(6,6), INTENT(IN) :: Cm
        REAL(KIND=8), INTENT(IN) :: tol
        check_tetragonal_II_conditions_met = .FALSE.
        IF (near_equal(Cm(1,1), Cm(2,2), tol) .AND. &
            near_equal(Cm(1,3), Cm(2,3), tol) .AND. &
            near_equal(Cm(4,4), Cm(5,5), tol) .AND. &
            near_equal(Cm(2,6), -Cm(1,6), tol) .AND. &
            .NOT. is_zero(Cm(1,6), tol) .AND. & ! C16 must be non-zero
            is_zero(Cm(1,4), tol) .AND. is_zero(Cm(1,5), tol) .AND. &
            is_zero(Cm(2,4), tol) .AND. is_zero(Cm(2,5), tol) .AND. &
            is_zero(Cm(3,4), tol) .AND. is_zero(Cm(3,5), tol) .AND. is_zero(Cm(3,6), tol) .AND. &
            is_zero(Cm(4,5), tol) .AND. is_zero(Cm(4,6), tol) .AND. is_zero(Cm(5,6), tol) ) THEN
            check_tetragonal_II_conditions_met = .TRUE.
        END IF
    END FUNCTION check_tetragonal_II_conditions_met

    LOGICAL FUNCTION check_rhombohedral_I_conditions_met(Cm, tol)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(6,6), INTENT(IN) :: Cm
        REAL(KIND=8), INTENT(IN) :: tol
        check_rhombohedral_I_conditions_met = .FALSE.
        IF (near_equal(Cm(1,1), Cm(2,2), tol) .AND. &                       ! C11=C22
            near_equal(Cm(1,3), Cm(2,3), tol) .AND. &                       ! C13=C23
            near_equal(Cm(4,4), Cm(5,5), tol) .AND. &                       ! C44=C55
            near_equal(Cm(2,4), -Cm(1,4), tol) .AND. &                      ! C24=-C14
            near_equal(Cm(5,6), Cm(1,4), tol) .AND. &                       ! C56=C14 (from paper matrix form C(row5,col6))
            near_equal(Cm(6,6), (Cm(1,1)-Cm(1,2))/2.0_8, tol) .AND. &       ! C66=(C11-C12)/2
            .NOT. is_zero(Cm(1,4), tol) .AND. &                             ! C14 is non-zero
            is_zero(Cm(1,5), tol) .AND. is_zero(Cm(1,6), tol) .AND. &       ! C15=0, C16=0
            is_zero(Cm(2,5), tol) .AND. is_zero(Cm(2,6), tol) .AND. &       ! C25=0, C26=0
            is_zero(Cm(3,4), tol) .AND. is_zero(Cm(3,5), tol) .AND. is_zero(Cm(3,6), tol) .AND. &
            is_zero(Cm(4,5), tol) .AND. is_zero(Cm(4,6), tol)) THEN         ! C45=0, C46=0
            check_rhombohedral_I_conditions_met = .TRUE.
        END IF
    END FUNCTION check_rhombohedral_I_conditions_met

    LOGICAL FUNCTION check_rhombohedral_II_conditions_met(Cm, tol)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(6,6), INTENT(IN) :: Cm
        REAL(KIND=8), INTENT(IN) :: tol
        check_rhombohedral_II_conditions_met = .FALSE.
        IF (near_equal(Cm(1,1), Cm(2,2), tol) .AND. &
            near_equal(Cm(1,3), Cm(2,3), tol) .AND. &
            near_equal(Cm(4,4), Cm(5,5), tol) .AND. &
            near_equal(Cm(2,4), -Cm(1,4), tol) .AND. &       ! C24=-C14
            near_equal(Cm(2,5), -Cm(1,5), tol) .AND. &       ! C25=-C15
            .NOT. is_zero(Cm(1,5), tol) .AND. &              ! C15 is non-zero
            near_equal(Cm(5,6), Cm(1,4), tol) .AND. &        ! C56=C14
            near_equal(Cm(4,6), -Cm(1,5), tol) .AND. &       ! C46=-C15
            near_equal(Cm(6,6), (Cm(1,1)-Cm(1,2))/2.0_8, tol) .AND. &
            is_zero(Cm(1,6), tol) .AND. is_zero(Cm(2,6), tol) .AND. &
            is_zero(Cm(3,4), tol) .AND. is_zero(Cm(3,5), tol) .AND. is_zero(Cm(3,6), tol) ) THEN
            check_rhombohedral_II_conditions_met = .TRUE.
        END IF
    END FUNCTION check_rhombohedral_II_conditions_met

    LOGICAL FUNCTION check_orthorhombic_conditions_met(Cm, tol)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(6,6), INTENT(IN) :: Cm
        REAL(KIND=8), INTENT(IN) :: tol
        check_orthorhombic_conditions_met = .FALSE.
        IF (is_zero(Cm(1,4), tol) .AND. is_zero(Cm(1,5), tol) .AND. is_zero(Cm(1,6), tol) .AND. &
            is_zero(Cm(2,4), tol) .AND. is_zero(Cm(2,5), tol) .AND. is_zero(Cm(2,6), tol) .AND. &
            is_zero(Cm(3,4), tol) .AND. is_zero(Cm(3,5), tol) .AND. is_zero(Cm(3,6), tol) .AND. &
            is_zero(Cm(4,5), tol) .AND. is_zero(Cm(4,6), tol) .AND. is_zero(Cm(5,6), tol) ) THEN
            IF (.NOT. check_cubic_conditions_met(Cm,tol) .AND. &
                .NOT. check_hexagonal_conditions_met(Cm,tol) .AND. &
                .NOT. check_tetragonal_I_conditions_met(Cm,tol) .AND. &
                .NOT. check_tetragonal_II_conditions_met(Cm,tol) .AND. &
                .NOT. check_rhombohedral_I_conditions_met(Cm,tol) .AND. &
                .NOT. check_rhombohedral_II_conditions_met(Cm,tol) ) THEN
                 check_orthorhombic_conditions_met = .TRUE.
            END IF
        END IF
    END FUNCTION check_orthorhombic_conditions_met

    LOGICAL FUNCTION check_monoclinic_conditions_met(Cm, tol)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(6,6), INTENT(IN) :: Cm
        REAL(KIND=8), INTENT(IN) :: tol
        LOGICAL :: y_unique, z_unique
        check_monoclinic_conditions_met = .FALSE.
        y_unique = is_zero(Cm(1,4), tol) .AND. is_zero(Cm(1,6), tol) .AND. &
                   is_zero(Cm(2,4), tol) .AND. is_zero(Cm(2,6), tol) .AND. &
                   is_zero(Cm(3,4), tol) .AND. is_zero(Cm(3,6), tol) .AND. &
                   is_zero(Cm(4,5), tol) .AND. is_zero(Cm(5,6), tol)
        z_unique = is_zero(Cm(1,5), tol) .AND. is_zero(Cm(1,4), tol) .AND. &
                   is_zero(Cm(2,5), tol) .AND. is_zero(Cm(2,4), tol) .AND. &
                   is_zero(Cm(3,5), tol) .AND. is_zero(Cm(3,4), tol) .AND. &
                   is_zero(Cm(5,6), tol) .AND. is_zero(Cm(4,6), tol)
        IF (y_unique .OR. z_unique) THEN
            IF (.NOT. check_orthorhombic_conditions_met(Cm,tol) .AND. &
                .NOT. check_cubic_conditions_met(Cm,tol) .AND. &
                .NOT. check_hexagonal_conditions_met(Cm,tol) .AND. &
                .NOT. check_tetragonal_I_conditions_met(Cm,tol) .AND. &
                .NOT. check_tetragonal_II_conditions_met(Cm,tol) .AND. &
                .NOT. check_rhombohedral_I_conditions_met(Cm,tol) .AND. &
                .NOT. check_rhombohedral_II_conditions_met(Cm,tol) ) THEN
                 check_monoclinic_conditions_met = .TRUE.
            END IF
        END IF
    END FUNCTION check_monoclinic_conditions_met

    LOGICAL FUNCTION check_triclinic_conditions_met(Cm, tol)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(6,6), INTENT(IN) :: Cm
        REAL(KIND=8), INTENT(IN) :: tol
        check_triclinic_conditions_met = .TRUE.
    END FUNCTION check_triclinic_conditions_met

    !-----------------------------------------------------------------------
    ! Subroutines to display schematic Cij matrix forms
    !-----------------------------------------------------------------------
    SUBROUTINE display_cubic_matrix_form()
        IMPLICIT NONE
        WRITE(*,*) "Schematic C_ij matrix form for Cubic system:"
        WRITE(*,'(A)') " | C11  C12  C12   0    0    0   |"
        WRITE(*,'(A)') " | C12  C11  C12   0    0    0   |"
        WRITE(*,'(A)') " | C12  C12  C11   0    0    0   |"
        WRITE(*,'(A)') " |  0    0    0   C44   0    0   |"
        WRITE(*,'(A)') " |  0    0    0    0   C44   0   |"
        WRITE(*,'(A)') " |  0    0    0    0    0   C44  |"
        WRITE(*,*) ""
        !---
        WRITE(99,*) "Schematic C_ij matrix form for Cubic system:"
        WRITE(99,'(A)') " | C11  C12  C12   0    0    0   |"
        WRITE(99,'(A)') " | C12  C11  C12   0    0    0   |"
        WRITE(99,'(A)') " | C12  C12  C11   0    0    0   |"
        WRITE(99,'(A)') " |  0    0    0   C44   0    0   |"
        WRITE(99,'(A)') " |  0    0    0    0   C44   0   |"
        WRITE(99,'(A)') " |  0    0    0    0    0   C44  |"
        WRITE(99,*) ""
    END SUBROUTINE display_cubic_matrix_form

    SUBROUTINE display_hexagonal_matrix_form()
        IMPLICIT NONE
        WRITE(*,*) "Schematic C_ij matrix form for Hexagonal system:"
        WRITE(*,'(A)') " | C11  C12  C13   0    0    0        |"
        WRITE(*,'(A)') " | C12  C11  C13   0    0    0        |"
        WRITE(*,'(A)') " | C13  C13  C33   0    0    0        |"
        WRITE(*,'(A)') " |  0    0    0   C44   0    0        |"
        WRITE(*,'(A)') " |  0    0    0    0   C44   0        |"
        WRITE(*,'(A)') " |  0    0    0    0    0   (C11-C12)/2 |"
        WRITE(*,*) ""
        !--------
        WRITE(99,*) "Schematic C_ij matrix form for Hexagonal system:"
        WRITE(99,'(A)') " | C11  C12  C13   0    0    0        |"
        WRITE(99,'(A)') " | C12  C11  C13   0    0    0        |"
        WRITE(99,'(A)') " | C13  C13  C33   0    0    0        |"
        WRITE(99,'(A)') " |  0    0    0   C44   0    0        |"
        WRITE(99,'(A)') " |  0    0    0    0   C44   0        |"
        WRITE(99,'(A)') " |  0    0    0    0    0   (C11-C12)/2 |"
        WRITE(99,*) ""
    END SUBROUTINE display_hexagonal_matrix_form

    SUBROUTINE display_tetragonal_I_matrix_form()
        IMPLICIT NONE
        WRITE(*,*) "Schematic C_ij matrix form for Tetragonal (I) system (4/mmm):"
        WRITE(*,'(A)') " | C11  C12  C13   0    0    0   |"
        WRITE(*,'(A)') " | C12  C11  C13   0    0    0   |"
        WRITE(*,'(A)') " | C13  C13  C33   0    0    0   |"
        WRITE(*,'(A)') " |  0    0    0   C44   0    0   |"
        WRITE(*,'(A)') " |  0    0    0    0   C44   0   |"
        WRITE(*,'(A)') " |  0    0    0    0    0   C66  |"
        WRITE(*,*) ""
        !---------
        WRITE(99,*) "Schematic C_ij matrix form for Tetragonal (I) system (4/mmm):"
        WRITE(99,'(A)') " | C11  C12  C13   0    0    0   |"
        WRITE(99,'(A)') " | C12  C11  C13   0    0    0   |"
        WRITE(99,'(A)') " | C13  C13  C33   0    0    0   |"
        WRITE(99,'(A)') " |  0    0    0   C44   0    0   |"
        WRITE(99,'(A)') " |  0    0    0    0   C44   0   |"
        WRITE(99,'(A)') " |  0    0    0    0    0   C66  |"
        WRITE(99,*) ""
    END SUBROUTINE display_tetragonal_I_matrix_form

    SUBROUTINE display_tetragonal_II_matrix_form()
        IMPLICIT NONE
        WRITE(*,*) "Schematic C_ij matrix form for Tetragonal (II) system (4/m):"
        WRITE(*,'(A)') " | C11  C12  C13   0    0   C16  |"
        WRITE(*,'(A)') " | C12  C11  C13   0    0  -C16  |"
        WRITE(*,'(A)') " | C13  C13  C33   0    0    0   |"
        WRITE(*,'(A)') " |  0    0    0   C44   0    0   |"
        WRITE(*,'(A)') " |  0    0    0    0   C44   0   |"
        WRITE(*,'(A)') " | C16 -C16   0    0    0   C66  |"
        WRITE(*,*) ""
        !---------
        WRITE(99,*) "Schematic C_ij matrix form for Tetragonal (II) system (4/m):"
        WRITE(99,'(A)') " | C11  C12  C13   0    0   C16  |"
        WRITE(99,'(A)') " | C12  C11  C13   0    0  -C16  |"
        WRITE(99,'(A)') " | C13  C13  C33   0    0    0   |"
        WRITE(99,'(A)') " |  0    0    0   C44   0    0   |"
        WRITE(99,'(A)') " |  0    0    0    0   C44   0   |"
        WRITE(99,'(A)') " | C16 -C16   0    0    0   C66  |"
        WRITE(99,*) ""
    END SUBROUTINE display_tetragonal_II_matrix_form

    SUBROUTINE display_rhombohedral_I_matrix_form()
        IMPLICIT NONE
        WRITE(*,*) "Schematic C_ij matrix form for Rhombohedral (I) system (3bar m):"
        WRITE(*,'(A)') " | C11  C12  C13  C14   0    0        |"
        WRITE(*,'(A)') " | C12  C11  C13 -C14   0    0        |"
        WRITE(*,'(A)') " | C13  C13  C33   0    0    0        |"
        WRITE(*,'(A)') " | C14 -C14   0   C44   0    0        |"
        WRITE(*,'(A)') " |  0    0    0    0   C44  C14       |"
        WRITE(*,'(A)') " |  0    0    0    0   C14 (C11-C12)/2 |"
        WRITE(*,*) ""
        !-----------
        WRITE(99,*) "Schematic C_ij matrix form for Rhombohedral (I) system (3bar m):"
        WRITE(99,'(A)') " | C11  C12  C13  C14   0    0        |"
        WRITE(99,'(A)') " | C12  C11  C13 -C14   0    0        |"
        WRITE(99,'(A)') " | C13  C13  C33   0    0    0        |"
        WRITE(99,'(A)') " | C14 -C14   0   C44   0    0        |"
        WRITE(99,'(A)') " |  0    0    0    0   C44  C14       |"
        WRITE(99,'(A)') " |  0    0    0    0   C14 (C11-C12)/2 |"
        WRITE(99,*) "" 
    END SUBROUTINE display_rhombohedral_I_matrix_form

    SUBROUTINE display_rhombohedral_II_matrix_form()
        IMPLICIT NONE
        WRITE(*,*) "Schematic C_ij matrix form for Rhombohedral (II) system (3bar):"
        WRITE(*,'(A)') " | C11  C12  C13  C14  C15   0        |"
        WRITE(*,'(A)') " | C12  C11  C13 -C14 -C15   0        |"
        WRITE(*,'(A)') " | C13  C13  C33   0    0    0        |"
        WRITE(*,'(A)') " | C14 -C14   0   C44   0   -C15       |"
        WRITE(*,'(A)') " | C15 -C15   0    0   C44  C14       |"
        WRITE(*,'(A)') " |  0    0    0  -C15  C14 (C11-C12)/2 |"
        WRITE(*,*) ""
        !------
        WRITE(99,*) "Schematic C_ij matrix form for Rhombohedral (II) system (3bar):"
        WRITE(99,'(A)') " | C11  C12  C13  C14  C15   0        |"
        WRITE(99,'(A)') " | C12  C11  C13 -C14 -C15   0        |"
        WRITE(99,'(A)') " | C13  C13  C33   0    0    0        |"
        WRITE(99,'(A)') " | C14 -C14   0   C44   0   -C15       |"
        WRITE(99,'(A)') " | C15 -C15   0    0   C44  C14       |"
        WRITE(99,'(A)') " |  0    0    0  -C15  C14 (C11-C12)/2 |"
        WRITE(99,*) ""
    END SUBROUTINE display_rhombohedral_II_matrix_form

    SUBROUTINE display_orthorhombic_matrix_form()
        IMPLICIT NONE
        WRITE(*,*) "Schematic C_ij matrix form for Orthorhombic system:"
        WRITE(*,'(A)') " | C11  C12  C13   0    0    0   |"
        WRITE(*,'(A)') " | C12  C22  C23   0    0    0   |"
        WRITE(*,'(A)') " | C13  C23  C33   0    0    0   |"
        WRITE(*,'(A)') " |  0    0    0   C44   0    0   |"
        WRITE(*,'(A)') " |  0    0    0    0   C55   0   |"
        WRITE(*,'(A)') " |  0    0    0    0    0   C66  |"
        WRITE(*,*) ""
        !------
        WRITE(99,*) "Schematic C_ij matrix form for Orthorhombic system:"
        WRITE(99,'(A)') " | C11  C12  C13   0    0    0   |"
        WRITE(99,'(A)') " | C12  C22  C23   0    0    0   |"
        WRITE(99,'(A)') " | C13  C23  C33   0    0    0   |"
        WRITE(99,'(A)') " |  0    0    0   C44   0    0   |"
        WRITE(99,'(A)') " |  0    0    0    0   C55   0   |"
        WRITE(99,'(A)') " |  0    0    0    0    0   C66  |"
        WRITE(99,*) ""
    END SUBROUTINE display_orthorhombic_matrix_form

    SUBROUTINE display_monoclinic_matrix_form()
        IMPLICIT NONE
        WRITE(*,*) "Schematic C_ij matrix form for Monoclinic system (y-axis unique):"
        WRITE(*,'(A)') " | C11  C12  C13   0   C15   0   |"
        WRITE(*,'(A)') " | C12  C22  C23   0   C25   0   |"
        WRITE(*,'(A)') " | C13  C23  C33   0   C35   0   |"
        WRITE(*,'(A)') " |  0    0    0   C44   0   C46  |"
        WRITE(*,'(A)') " | C15  C25  C35   0   C55   0   |"
        WRITE(*,'(A)') " |  0    0    0   C46   0   C66  |"
        WRITE(*,*) "(Note: For z-axis unique, the pattern of zeros/non-zeros shifts)"
        WRITE(*,*) ""
        !----------
        WRITE(99,*) "Schematic C_ij matrix form for Monoclinic system (y-axis unique):"
        WRITE(99,'(A)') " | C11  C12  C13   0   C15   0   |"
        WRITE(99,'(A)') " | C12  C22  C23   0   C25   0   |"
        WRITE(99,'(A)') " | C13  C23  C33   0   C35   0   |"
        WRITE(99,'(A)') " |  0    0    0   C44   0   C46  |"
        WRITE(99,'(A)') " | C15  C25  C35   0   C55   0   |"
        WRITE(99,'(A)') " |  0    0    0   C46   0   C66  |"
        WRITE(99,*) "(Note: For z-axis unique, the pattern of zeros/non-zeros shifts)"
        WRITE(99,*) ""
    END SUBROUTINE display_monoclinic_matrix_form

    SUBROUTINE display_triclinic_matrix_form()
        IMPLICIT NONE
        WRITE(*,*) "Schematic C_ij matrix form for Triclinic system (21 independent constants):"
        WRITE(*,'(A)') " | C11  C12  C13  C14  C15  C16  |"
        WRITE(*,'(A)') " | C12  C22  C23  C24  C25  C26  |"
        WRITE(*,'(A)') " | C13  C23  C33  C34  C35  C36  |"
        WRITE(*,'(A)') " | C14  C24  C34  C44  C45  C46  |"
        WRITE(*,'(A)') " | C15  C25  C35  C45  C55  C56  |"
        WRITE(*,'(A)') " | C16  C26  C36  C46  C56  C66  |"
        WRITE(*,*) ""
        !----------
                WRITE(*,*) "Schematic C_ij matrix form for Triclinic system (21 independent constants):"
        WRITE(99,'(A)') " | C11  C12  C13  C14  C15  C16  |"
        WRITE(99,'(A)') " | C12  C22  C23  C24  C25  C26  |"
        WRITE(99,'(A)') " | C13  C23  C33  C34  C35  C36  |"
        WRITE(99,'(A)') " | C14  C24  C34  C44  C45  C46  |"
        WRITE(99,'(A)') " | C15  C25  C35  C45  C55  C56  |"
        WRITE(99,'(A)') " | C16  C26  C36  C46  C56  C66  |"
        WRITE(99,*) ""
    END SUBROUTINE display_triclinic_matrix_form

    !-----------------------------------------------------------------------
    ! Subroutines to check stability conditions for each crystal system.
    ! Based on Mouhat & Coudert 2014 equations.
    ! Input: relevant independent elastic constants.
    !-----------------------------------------------------------------------

    SUBROUTINE check_cubic_stability(lc11, lc12, lc44)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: lc11, lc12, lc44
        LOGICAL :: condition_met(3)
        WRITE(*,*) "Cubic Stability Conditions (Mouhat & Coudert 2014, Eq. 6):"
        condition_met(1) = (lc11 - lc12 > 0.0_8)
        condition_met(2) = (lc11 + 2.0_8*lc12 > 0.0_8)
        condition_met(3) = (lc44 > 0.0_8)
        WRITE(*,'(A,L1,A,F10.4,A)') "1. C11 - C12 > 0        : ", condition_met(1), " (Val: ", lc11 - lc12, ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "2. C11 + 2*C12 > 0      : ", condition_met(2), " (Val: ", lc11 + 2.0_8*lc12, ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "3. C44 > 0              : ", condition_met(3), " (Val: ", lc44, ")"
        !!!!!!!!!!!!!
        WRITE(99,'(A,L1,A,F10.4,A)') "1. C11 - C12 > 0        : ", condition_met(1), " (Val: ", lc11 - lc12, ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "2. C11 + 2*C12 > 0      : ", condition_met(2), " (Val: ", lc11 + 2.0_8*lc12, ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "3. C44 > 0              : ", condition_met(3), " (Val: ", lc44, ")"
        IF (ALL(condition_met)) THEN
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0') 
            CALL system ('tput setaf 10;tput bold; echo " > Born Stability Criteria Result: STABLE  ✓";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0') 
        ELSE
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " > Born Stability Criteria Result: UNSTABLE ✗; STOP";tput sgr0')  
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
        END IF
        WRITE(*,*) ""
    END SUBROUTINE check_cubic_stability

    SUBROUTINE check_hexagonal_stability(lc11, lc12, lc13, lc33, lc44, lc66)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: lc11, lc12, lc13, lc33, lc44, lc66
        LOGICAL :: condition_met(4)
        WRITE(*,*) "Hexagonal Stability Conditions (Mouhat & Coudert 2014, Eq. 9):"
        condition_met(1) = (lc11 > ABS(lc12)) ! Equivalent to C11-C12>0 and C11+C12>0
        condition_met(2) = (2.0_8 * lc13**2 < lc33 * (lc11 + lc12))
        condition_met(3) = (lc44 > 0.0_8)
        condition_met(4) = (lc66 > 0.0_8) ! Note: C66=(C11-C12)/2, so C11>|C12| implies C66>0.
        WRITE(*,'(A,L1,A,F10.4,A,F10.4,A)') "1. C11 > |C12|            : ", condition_met(1), &
        &                                    " (C11-C12: ", lc11-lc12, ", C11+C12: ", lc11+lc12, ")"
        WRITE(*,'(A,L1,A,F12.4,A,F12.4,A)') "2. 2*C13^2<C33(C11+C12)  : ", condition_met(2), &
        &                                    " (LHS:", 2.0_8*lc13**2, ",RHS:", lc33*(lc11+lc12), ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "3. C44 > 0              : ", condition_met(3), " (Val: ", lc44, ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "4. C66 > 0              : ", condition_met(4), " (Val: ", lc66, ")"
        !!!!!!!!!!!!!
        WRITE(99,'(A,L1,A,F10.4,A,F10.4,A)') "1. C11 > |C12|            : ", condition_met(1), &
        &                                    " (C11-C12: ", lc11-lc12, ", C11+C12: ", lc11+lc12, ")"
        WRITE(99,'(A,L1,A,F12.4,A,F12.4,A)') "2. 2*C13^2<C33(C11+C12)  : ", condition_met(2), &
        &                                    " (LHS:", 2.0_8*lc13**2, ",RHS:", lc33*(lc11+lc12), ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "3. C44 > 0              : ", condition_met(3), " (Val: ", lc44, ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "4. C66 > 0              : ", condition_met(4), " (Val: ", lc66, ")"
        IF (ALL(condition_met)) THEN
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0') 
            CALL system ('tput setaf 10;tput bold; echo " > Born Stability Criteria Result: STABLE  ✓";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0') 
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: STABLE  ✓" 
            WRITE(99,'(A)') " =======================================" 
        ELSE
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " > Born Stability Criteria Result: UNSTABLE ✗; STOP";tput sgr0')  
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: UNSTABLE ✗" 
            WRITE(99,'(A)') " =======================================" 
        END IF
        WRITE(*,*) ""
    END SUBROUTINE check_hexagonal_stability

    SUBROUTINE check_tetragonal_I_stability(lc11, lc12, lc13, lc33, lc44, lc66)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: lc11, lc12, lc13, lc33, lc44, lc66
        LOGICAL :: condition_met(4)
        WRITE(*,*) "Tetragonal (I) Stability Conditions (Mouhat & Coudert 2014, Eq. 9):"
        condition_met(1) = (lc11 > ABS(lc12))
        condition_met(2) = (2.0_8 * lc13**2 < lc33 * (lc11 + lc12))
        condition_met(3) = (lc44 > 0.0_8)
        condition_met(4) = (lc66 > 0.0_8)
        WRITE(*,'(A,L1,A,F10.4,A,F10.4,A)') "1. C11 > |C12|            : ", condition_met(1), &
        &                                    " (C11-C12: ", lc11-lc12, ", C11+C12: ", lc11+lc12, ")"
        WRITE(*,'(A,L1,A,F12.4,A,F12.4,A)') "2. 2*C13^2<C33(C11+C12)  : ", condition_met(2), &
        &                                    " (LHS:", 2.0_8*lc13**2, ",RHS:", lc33*(lc11+lc12), ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "3. C44 > 0              : ", condition_met(3), " (Val: ", lc44, ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "4. C66 > 0              : ", condition_met(4), " (Val: ", lc66, ")"
        !!!!!!!!!!!!!
        WRITE(99,'(A,L1,A,F10.4,A,F10.4,A)') "1. C11 > |C12|            : ", condition_met(1), &
        &                                    " (C11-C12: ", lc11-lc12, ", C11+C12: ", lc11+lc12, ")"
        WRITE(99,'(A,L1,A,F12.4,A,F12.4,A)') "2. 2*C13^2<C33(C11+C12)  : ", condition_met(2), &
        &                                    " (LHS:", 2.0_8*lc13**2, ",RHS:", lc33*(lc11+lc12), ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "3. C44 > 0              : ", condition_met(3), " (Val: ", lc44, ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "4. C66 > 0              : ", condition_met(4), " (Val: ", lc66, ")"
        IF (ALL(condition_met)) THEN
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0') 
            CALL system ('tput setaf 10;tput bold; echo " > Born Stability Criteria Result: STABLE  ✓";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo  " =======================================";tput sgr0') 
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: STABLE  ✓" 
            WRITE(99,'(A)') " =======================================" 
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: STABLE  ✓" 
            WRITE(99,'(A)') " =======================================" 
        ELSE
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " > Born Stability Criteria Result: UNSTABLE ✗; STOP";tput sgr0')  
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: UNSTABLE ✗" 
            WRITE(99,'(A)') " =======================================" 
        END IF
        WRITE(*,*) ""
    END SUBROUTINE check_tetragonal_I_stability

    SUBROUTINE check_tetragonal_II_stability(lc11, lc12, lc13, lc16, lc33, lc44, lc66, tol_val)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: lc11, lc12, lc13, lc16, lc33, lc44, lc66, tol_val
        LOGICAL :: condition_met(4)
        WRITE(*,*) "Tetragonal (II) Stability Conditions (Mouhat & Coudert 2014, Eq. 11):"
        condition_met(1) = (lc11 > ABS(lc12))
        condition_met(2) = (2.0_8 * lc13**2 < lc33 * (lc11 + lc12))
        condition_met(3) = (lc44 > 0.0_8)
        condition_met(4) = (2.0_8 * lc16**2 < lc66 * (lc11 - lc12))
        WRITE(*,'(A,L1,A,F10.4,A,F10.4,A)') "1. C11 > |C12|            : ", condition_met(1), &
        &                                    " (C11-C12: ", lc11-lc12, ", C11+C12: ", lc11+lc12, ")"
        WRITE(*,'(A,L1,A,F12.4,A,F12.4,A)') "2. 2*C13^2<C33(C11+C12)  : ", condition_met(2), &
        &                                    " (LHS:", 2.0_8*lc13**2, ",RHS:", lc33*(lc11+lc12), ")"
        !!!!!!!!!!!!!
        WRITE(99,'(A,L1,A,F10.4,A)') "3. C44 > 0              : ", condition_met(3), " (Val: ", lc44, ")"
        WRITE(99,'(A,L1,A,F12.4,A,F12.4,A)') "4. 2*C16^2<C66(C11-C12)  : ", condition_met(4), &
        &                                    " (LHS:", 2.0_8*lc16**2, ",RHS:", lc66*(lc11-lc12), ")"
        
        WRITE(99,'(A,L1,A,F10.4,A,F10.4,A)') "1. C11 > |C12|            : ", condition_met(1), &
        &                                    " (C11-C12: ", lc11-lc12, ", C11+C12: ", lc11+lc12, ")"
        WRITE(99,'(A,L1,A,F12.4,A,F12.4,A)') "2. 2*C13^2<C33(C11+C12)  : ", condition_met(2), &
        &                                    " (LHS:", 2.0_8*lc13**2, ",RHS:", lc33*(lc11+lc12), ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "3. C44 > 0              : ", condition_met(3), " (Val: ", lc44, ")"
        WRITE(99,'(A,L1,A,F12.4,A,F12.4,A)') "4. 2*C16^2<C66(C11-C12)  : ", condition_met(4), &
        &                                    " (LHS:", 2.0_8*lc16**2, ",RHS:", lc66*(lc11-lc12), ")"
        IF (ALL(condition_met)) THEN
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0') 
            CALL system ('tput setaf 10;tput bold; echo " > Born Stability Criteria Result: STABLE  ✓";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0') 
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: STABLE  ✓" 
            WRITE(99,'(A)') " =======================================" 
        ELSE
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " > Born Stability Criteria Result: UNSTABLE ✗; STOP";tput sgr0')  
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: UNSTABLE ✗" 
            WRITE(99,'(A)') " =======================================" 
        END IF
        WRITE(*,*) ""
    END SUBROUTINE check_tetragonal_II_stability

    SUBROUTINE check_rhombohedral_I_stability(lc11, lc12, lc13, lc14, lc33, lc44, tol_val)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: lc11, lc12, lc13, lc14, lc33, lc44, tol_val
        REAL(KIND=8) :: derived_c66
        LOGICAL :: condition_met(4)
        derived_c66 = (lc11 - lc12) / 2.0_8
        WRITE(*,*) "Rhombohedral (I) Stability Conditions (Mouhat & Coudert 2014, Eq. 13):"
        WRITE(*,'(A,F10.4,A)') "(Note: C66_derived = (C11-C12)/2 = ", derived_c66, ")"
        condition_met(1) = (lc11 > ABS(lc12))
        condition_met(2) = (lc44 > 0.0_8)
        condition_met(3) = (lc13**2 < 0.5_8 * lc33 * (lc11 + lc12))
        condition_met(4) = (lc14**2 < 0.5_8 * lc44 * (lc11 - lc12)) ! which is C44*derived_c66
        WRITE(*,'(A,L1,A,F10.4,A,F10.4,A)') "1. C11 > |C12|            : ", condition_met(1), &
        &                                    " (C11-C12: ", lc11-lc12, ", C11+C12: ", lc11+lc12, ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "2. C44 > 0              : ", condition_met(2), " (Val: ", lc44, ")"
        WRITE(*,'(A,L1,A,F12.4,A,F12.4,A)') "3. C13^2<0.5C33(C11+C12) : ", condition_met(3), &
        &                                    " (LHS:", lc13**2, ",RHS:", 0.5_8*lc33*(lc11+lc12), ")"
        WRITE(*,'(A,L1,A,F12.4,A,F12.4,A)') "4. C14^2<C44C66_derived  : ", condition_met(4), &
        &                                    " (LHS:", lc14**2, ",RHS:", lc44*derived_c66, ")"
        !!!!!!!!!
        WRITE(99,'(A,L1,A,F10.4,A,F10.4,A)') "1. C11 > |C12|            : ", condition_met(1), &
        &                                    " (C11-C12: ", lc11-lc12, ", C11+C12: ", lc11+lc12, ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "2. C44 > 0              : ", condition_met(2), " (Val: ", lc44, ")"
        WRITE(99,'(A,L1,A,F12.4,A,F12.4,A)') "3. C13^2<0.5C33(C11+C12) : ", condition_met(3), &
        &                                    " (LHS:", lc13**2, ",RHS:", 0.5_8*lc33*(lc11+lc12), ")"
        WRITE(99,'(A,L1,A,F12.4,A,F12.4,A)') "4. C14^2<C44C66_derived  : ", condition_met(4), &
        &                                    " (LHS:", lc14**2, ",RHS:", lc44*derived_c66, ")"
        IF (ALL(condition_met)) THEN
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0') 
            CALL system ('tput setaf 10;tput bold; echo " > Born Stability Criteria Result: STABLE  ✓";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0') 
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: STABLE  ✓" 
            WRITE(99,'(A)') " =======================================" 
        ELSE
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " > Born Stability Criteria Result: UNSTABLE ✗; STOP";tput sgr0') 
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0') 
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: UNSTABLE ✗" 
            WRITE(99,'(A)') " =======================================" 
        END IF
        WRITE(*,*) ""
    END SUBROUTINE check_rhombohedral_I_stability

    SUBROUTINE check_rhombohedral_II_stability(lc11, lc12, lc13, lc14, lc15, lc33, lc44, tol_val)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: lc11, lc12, lc13, lc14, lc15, lc33, lc44, tol_val
        REAL(KIND=8) :: derived_c66
        LOGICAL :: condition_met(4)
        derived_c66 = (lc11 - lc12) / 2.0_8
        WRITE(*,*) "Rhombohedral (II) Stability Conditions (Mouhat & Coudert 2014, Eq. 15):"
        WRITE(*,'(A,F10.4,A)') "(Note: C66_derived = (C11-C12)/2 = ", derived_c66, ")"
        condition_met(1) = (lc11 > ABS(lc12))
        condition_met(2) = (lc44 > 0.0_8)
        condition_met(3) = (lc13**2 < 0.5_8 * lc33 * (lc11 + lc12))
        condition_met(4) = (lc14**2 + lc15**2 < 0.5_8 * lc44 * (lc11 - lc12)) ! which is C44*derived_c66
        WRITE(*,'(A,L1,A,F10.4,A,F10.4,A)') "1. C11 > |C12|                : ", condition_met(1),&
        &                                    " (C11-C12: ",lc11-lc12,", C11+C12: ",lc11+lc12,")"
        WRITE(*,'(A,L1,A,F10.4,A)') "2. C44 > 0                  : ", condition_met(2), " (Val: ", lc44, ")"
        WRITE(*,'(A,L1,A,F12.4,A,F12.4,A)') "3. C13^2<0.5C33(C11+C12)    : ", condition_met(3),&
        &                                    " (LHS:",lc13**2,",RHS:",0.5_8*lc33*(lc11+lc12),")"
        WRITE(*,'(A,L1,A,F12.4,A,F12.4,A)') "4. C14^2+C15^2<C44C66_der. : ", condition_met(4),&
        &                                    " (LHS:",lc14**2+lc15**2,",RHS:",lc44*derived_c66,")"
        !!!!!!!!
        WRITE(99,'(A,L1,A,F10.4,A,F10.4,A)') "1. C11 > |C12|                : ", condition_met(1),&
        &                                    " (C11-C12: ",lc11-lc12,", C11+C12: ",lc11+lc12,")"
        WRITE(99,'(A,L1,A,F10.4,A)') "2. C44 > 0                  : ", condition_met(2), " (Val: ", lc44, ")"
        WRITE(99,'(A,L1,A,F12.4,A,F12.4,A)') "3. C13^2<0.5C33(C11+C12)    : ", condition_met(3),&
        &                                    " (LHS:",lc13**2,",RHS:",0.5_8*lc33*(lc11+lc12),")"
        WRITE(99,'(A,L1,A,F12.4,A,F12.4,A)') "4. C14^2+C15^2<C44C66_der. : ", condition_met(4),&
        &                                    " (LHS:",lc14**2+lc15**2,",RHS:",lc44*derived_c66,")"
        IF (ALL(condition_met)) THEN
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0') 
            CALL system ('tput setaf 10;tput bold; echo " > Born Stability Criteria Result: STABLE  ✓";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0') 
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: STABLE  ✓" 
            WRITE(99,'(A)') " =======================================" 
        ELSE
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " > Born Stability Criteria Result: UNSTABLE ✗; STOP";tput sgr0')  
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: UNSTABLE ✗" 
            WRITE(99,'(A)') " =======================================" 
        END IF
        WRITE(*,*) ""
    END SUBROUTINE check_rhombohedral_II_stability

    SUBROUTINE check_orthorhombic_stability(lc11, lc12, lc13, lc22, lc23, lc33, lc44, lc55, lc66)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: lc11,lc12,lc13,lc22,lc23,lc33,lc44,lc55,lc66
        LOGICAL :: condition_met(6)
        REAL(KIND=8) :: term3_val
        WRITE(*,*) "Orthorhombic Stability Conditions (Mouhat & Coudert 2014, Eq. 18):"
        condition_met(1) = (lc11 > 0.0_8)
        condition_met(2) = (lc11*lc22 > lc12**2)
        term3_val = lc11*lc22*lc33 + 2.0_8*lc12*lc13*lc23 - lc11*lc23**2 - lc22*lc13**2 - lc33*lc12**2
        condition_met(3) = (term3_val > 0.0_8)
        condition_met(4) = (lc44 > 0.0_8)
        condition_met(5) = (lc55 > 0.0_8)
        condition_met(6) = (lc66 > 0.0_8)
        WRITE(*,'(A,L1,A,F10.4,A)') "1. C11 > 0                     : ", condition_met(1), " (Val: ", lc11, ")"
        WRITE(*,'(A,L1,A,F12.4,A,F12.4,A)') "2. C11*C22 > C12^2             : ", condition_met(2), &
        &                                    " (LHS:", lc11*lc22, ",RHS:", lc12**2, ")"
        WRITE(*,'(A,L1,A,F12.4,A)') "3. C11C22C33+2C12C13C23-...>0 : ", condition_met(3), " (Val: ", term3_val, ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "4. C44 > 0                     : ", condition_met(4), " (Val: ", lc44, ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "5. C55 > 0                     : ", condition_met(5), " (Val: ", lc55, ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "6. C66 > 0                     : ", condition_met(6), " (Val: ", lc66, ")"
        !!!!!!!!
        WRITE(99,'(A,L1,A,F10.4,A)') "1. C11 > 0                     : ", condition_met(1), " (Val: ", lc11, ")"
        WRITE(99,'(A,L1,A,F12.4,A,F12.4,A)') "2. C11*C22 > C12^2             : ", condition_met(2), &
        &                                    " (LHS:", lc11*lc22, ",RHS:", lc12**2, ")"
        WRITE(99,'(A,L1,A,F12.4,A)') "3. C11C22C33+2C12C13C23-...>0 : ", condition_met(3), " (Val: ", term3_val, ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "4. C44 > 0                     : ", condition_met(4), " (Val: ", lc44, ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "5. C55 > 0                     : ", condition_met(5), " (Val: ", lc55, ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "6. C66 > 0                     : ", condition_met(6), " (Val: ", lc66, ")"
        IF (ALL(condition_met)) THEN
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0') 
            CALL system ('tput setaf 10;tput bold; echo " > Born Stability Criteria Result: STABLE  ✓";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0') 
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: STABLE  ✓" 
            WRITE(99,'(A)') " =======================================" 
        ELSE
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " > Born Stability Criteria Result: UNSTABLE ✗; STOP";tput sgr0')        
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: UNSTABLE ✗" 
            WRITE(99,'(A)') " ======================================="    
        END IF
        WRITE(*,*) ""
    END SUBROUTINE check_orthorhombic_stability

    !-----------------------------------------------------------------------
    ! Recursive function to calculate the determinant of a square matrix A
    ! of dimension N_dim x N_dim.
    !-----------------------------------------------------------------------
    RECURSIVE FUNCTION calculate_determinant(A, N_dim) RESULT(det_val)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: N_dim
        REAL(KIND=8), DIMENSION(N_dim, N_dim), INTENT(IN) :: A
        REAL(KIND=8) :: det_val
        
        REAL(KIND=8), DIMENSION(MAX(1,N_dim-1), MAX(1,N_dim-1)) :: minor_submatrix ! Ensure allocatable or sufficiently large
        INTEGER :: j_col_expansion, r_sub, c_sub, r_orig, c_orig
        REAL(KIND=8) :: sign_term

        IF (N_dim < 1) THEN
            det_val = 0.0_8 ! Should not happen for valid inputs
            PRINT *, "Error: Determinant called for N_dim < 1"
            RETURN
        END IF

        IF (N_dim == 1) THEN
            det_val = A(1,1)
            RETURN
        END IF
        
        IF (N_dim == 2) THEN
            det_val = A(1,1)*A(2,2) - A(1,2)*A(2,1)
            RETURN
        END IF

        det_val = 0.0_8
        sign_term = 1.0_8
        
        DO j_col_expansion = 1, N_dim  ! Iterate over columns of the first row for cofactor expansion
            r_sub = 0
            DO r_orig = 2, N_dim ! Original matrix rows, skipping the first
                r_sub = r_sub + 1
                c_sub = 0
                DO c_orig = 1, N_dim ! Original matrix columns
                    IF (c_orig == j_col_expansion) CYCLE ! Skip the column used in cofactor
                    c_sub = c_sub + 1
                    minor_submatrix(r_sub, c_sub) = A(r_orig, c_orig)
                END DO
            END DO
            
            det_val = det_val + sign_term * A(1,j_col_expansion) * calculate_determinant(minor_submatrix, N_dim-1)
            sign_term = -sign_term
        END DO
    END FUNCTION calculate_determinant

    !-----------------------------------------------------------------------
    ! Subroutine to check stability for Monoclinic/Triclinic systems
    ! using Born criteria via Sylvester's criterion (leading principal minors).
    !-----------------------------------------------------------------------
    SUBROUTINE check_stability_by_leading_minors(C_main_matrix, system_name_str)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(6,6), INTENT(IN) :: C_main_matrix
        CHARACTER(LEN=*), INTENT(IN) :: system_name_str
        
        LOGICAL :: conditions_met_minors(6)
        REAL(KIND=8) :: D_minors(6)
        
        REAL(KIND=8), DIMENSION(1,1) :: mat1x1
        REAL(KIND=8), DIMENSION(2,2) :: mat2x2
        REAL(KIND=8), DIMENSION(3,3) :: mat3x3
        REAL(KIND=8), DIMENSION(4,4) :: mat4x4
        REAL(KIND=8), DIMENSION(5,5) :: mat5x5

        WRITE(*,*) TRIM(system_name_str), " Stability Conditions (Sylvester's Criterion - Leading Principal Minors):"

        mat1x1(1,1) = C_main_matrix(1,1)
        D_minors(1) = calculate_determinant(mat1x1, 1)
        conditions_met_minors(1) = (D_minors(1) > 0.0_8)
        WRITE(*,'(A,I1,A,L1,A,ES18.6E2,A)') "D", 1, " > 0 : ", conditions_met_minors(1), " (Val: ", D_minors(1), ")"

        mat2x2 = C_main_matrix(1:2, 1:2)
        D_minors(2) = calculate_determinant(mat2x2, 2)
        conditions_met_minors(2) = (D_minors(2) > 0.0_8)
        WRITE(*,'(A,I1,A,L1,A,ES18.6E2,A)') "D", 2, " > 0 : ", conditions_met_minors(2), " (Val: ", D_minors(2), ")"

        mat3x3 = C_main_matrix(1:3, 1:3)
        D_minors(3) = calculate_determinant(mat3x3, 3)
        conditions_met_minors(3) = (D_minors(3) > 0.0_8)
        WRITE(*,'(A,I1,A,L1,A,ES18.6E2,A)') "D", 3, " > 0 : ", conditions_met_minors(3), " (Val: ", D_minors(3), ")"

        mat4x4 = C_main_matrix(1:4, 1:4)
        D_minors(4) = calculate_determinant(mat4x4, 4)
        conditions_met_minors(4) = (D_minors(4) > 0.0_8)
        WRITE(*,'(A,I1,A,L1,A,ES18.6E2,A)') "D", 4, " > 0 : ", conditions_met_minors(4), " (Val: ", D_minors(4), ")"

        mat5x5 = C_main_matrix(1:5, 1:5)
        D_minors(5) = calculate_determinant(mat5x5, 5)
        conditions_met_minors(5) = (D_minors(5) > 0.0_8)
        WRITE(*,'(A,I1,A,L1,A,ES18.6E2,A)') "D", 5, " > 0 : ", conditions_met_minors(5), " (Val: ", D_minors(5), ")"

        D_minors(6) = calculate_determinant(C_main_matrix, 6) ! Pass the full 6x6 matrix
        conditions_met_minors(6) = (D_minors(6) > 0.0_8)
        WRITE(*,'(A,I1,A,L1,A,ES18.6E2,A)') "D", 6, " > 0 : ", conditions_met_minors(6), " (Val: ", D_minors(6), ")"

        IF (ALL(conditions_met_minors)) THEN
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')      
            CALL system ('tput setaf 10;tput bold; echo " > Born Stability Criteria Result (Leading Minors): STABLE  ✓";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: STABLE  ✓" 
            WRITE(99,'(A)') " =======================================" 
        ELSE
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " > Born Stability Criteria Result (Leading Minors): UNSTABLE ✗; STOP";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: UNSTABLE ✗" 
            WRITE(99,'(A)') " ======================================="  
        END IF
        WRITE(*,*) ""
    END SUBROUTINE check_stability_by_leading_minors

    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !+ Subroutine to read elastic constants, determine crystal system,     +
    !+ and check stability conditions based on Mouhat & Coudert 2014,      +
    !+ with modifications for Monoclinic/Triclinic using Born criteria     +
    !+ via leading principal minors.                                       +
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    SUBROUTINE analyze_elastic_constants(cij_filepath)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: cij_filepath
        REAL(KIND=8), DIMENSION(6,6) :: C_matrix
        REAL(KIND=8) :: C11, C12, C13, C14, C15, C16, &
                        C22, C23, C24, C25, C26, &
                        C33, C34, C35, C36, &
                        C44, C45, C46, &
                        C55, C56, &
                        C66
        REAL(KIND=8), PARAMETER :: TOLERANCE_LOCAL = 1.0E-4 ! Renamed
        INTEGER :: i, j, k, unit_num
        CHARACTER(LEN=30) :: crystal_system_name
        LOGICAL :: system_identified

        OPEN(NEWUNIT=unit_num, FILE=cij_filepath, STATUS='OLD', ACTION='READ', IOSTAT=i)
        IF (i /= 0) THEN
            PRINT *, "Error: Could not open file: ", TRIM(cij_filepath)
            PRINT *, "Please ensure 'Cij.dat' exists and contains the 6x6 elastic matrix."
            RETURN
        END IF

        DO i = 1, 6
            READ(unit_num, *, IOSTAT=j) (C_matrix(i,k), k=1,6)
            IF (j /= 0) THEN
                PRINT *, "Error reading line ", i, " from file: ", TRIM(cij_filepath)
                CLOSE(unit_num)
                RETURN
            END IF
        END DO
        CLOSE(unit_num)

        DO i = 1, 6
            DO j = i + 1, 6
                IF (ABS(C_matrix(i,j) - C_matrix(j,i)) > TOLERANCE_LOCAL * MAX(ABS(C_matrix(i,j)), ABS(C_matrix(j,i)), 1.0_8)) THEN
                    PRINT *, "Warning: Input Cij matrix is not perfectly symmetric."
                    PRINT *, "Enforcing symmetry C_matrix(j,i) = C_matrix(i,j) based on upper triangle."
                END IF
                C_matrix(j,i) = C_matrix(i,j)
            END DO
        END DO

        C11 = C_matrix(1,1); C12 = C_matrix(1,2); C13 = C_matrix(1,3); C14 = C_matrix(1,4); C15 = C_matrix(1,5); C16 = C_matrix(1,6)
        C22 = C_matrix(2,2); C23 = C_matrix(2,3); C24 = C_matrix(2,4); C25 = C_matrix(2,5); C26 = C_matrix(2,6)
        C33 = C_matrix(3,3); C34 = C_matrix(3,4); C35 = C_matrix(3,5); C36 = C_matrix(3,6)
        C44 = C_matrix(4,4); C45 = C_matrix(4,5); C46 = C_matrix(4,6)
        C55 = C_matrix(5,5); C56 = C_matrix(5,6)
        C66 = C_matrix(6,6)

        crystal_system_name = "Undetermined"
        system_identified = .FALSE.

        IF ( check_cubic_conditions_met(C_matrix, TOLERANCE_LOCAL) ) THEN
            crystal_system_name = "Cubic"
            system_identified = .TRUE.
            PRINT *, "Crystal System Identified: Cubic"
            CALL display_cubic_matrix_form()
            CALL check_cubic_stability(C11, C12, C44)
        ELSE IF ( check_hexagonal_conditions_met(C_matrix, TOLERANCE_LOCAL) ) THEN
            crystal_system_name = "Hexagonal"
            system_identified = .TRUE.
            PRINT *, "Crystal System Identified: Hexagonal (includes Laue classes 6/m and 6/mmm)"
            CALL display_hexagonal_matrix_form()
            CALL check_hexagonal_stability(C11, C12, C13, C33, C44, C66)
        ELSE IF ( check_tetragonal_I_conditions_met(C_matrix, TOLERANCE_LOCAL) ) THEN
            crystal_system_name = "Tetragonal (I)"
            system_identified = .TRUE.
            PRINT *, "Crystal System Identified: Tetragonal (I) (Laue class 4/mmm)"
            CALL display_tetragonal_I_matrix_form()
            CALL check_tetragonal_I_stability(C11, C12, C13, C33, C44, C66)
        ELSE IF ( check_tetragonal_II_conditions_met(C_matrix, TOLERANCE_LOCAL) ) THEN
            crystal_system_name = "Tetragonal (II)"
            system_identified = .TRUE.
            PRINT *, "Crystal System Identified: Tetragonal (II) (Laue class 4/m)"
            CALL display_tetragonal_II_matrix_form()
            CALL check_tetragonal_II_stability(C11, C12, C13, C16, C33, C44, C66, TOLERANCE_LOCAL)
        ELSE IF ( check_rhombohedral_I_conditions_met(C_matrix, TOLERANCE_LOCAL) ) THEN
            crystal_system_name = "Rhombohedral (I)"
            system_identified = .TRUE.
            PRINT *, "Crystal System Identified: Rhombohedral (I) (Laue class 3bar m)"
            CALL display_rhombohedral_I_matrix_form()
            CALL check_rhombohedral_I_stability(C11, C12, C13, C14, C33, C44, TOLERANCE_LOCAL)
        ELSE IF ( check_rhombohedral_II_conditions_met(C_matrix, TOLERANCE_LOCAL) ) THEN
            crystal_system_name = "Rhombohedral (II)"
            system_identified = .TRUE.
            PRINT *, "Crystal System Identified: Rhombohedral (II) (Laue class 3bar)"
            CALL display_rhombohedral_II_matrix_form()
            CALL check_rhombohedral_II_stability(C11, C12, C13, C14, C15, C33, C44, TOLERANCE_LOCAL)
        ELSE IF ( check_orthorhombic_conditions_met(C_matrix, TOLERANCE_LOCAL) ) THEN
            crystal_system_name = "Orthorhombic"
            system_identified = .TRUE.
            PRINT *, "Crystal System Identified: Orthorhombic (Laue class mmm)"
            CALL display_orthorhombic_matrix_form()
            CALL check_orthorhombic_stability(C11, C12, C13, C22, C23, C33, C44, C55, C66)
        ELSE IF ( check_monoclinic_conditions_met(C_matrix, TOLERANCE_LOCAL) ) THEN
            crystal_system_name = "Monoclinic"
            system_identified = .TRUE.
            PRINT *, "Crystal System Identified: Monoclinic (Laue class 2/m)"
            CALL display_monoclinic_matrix_form()
            PRINT *, "Stability for Monoclinic: Checking Born criteria via leading principal minors."
            CALL check_stability_by_leading_minors(C_matrix, "Monoclinic")
        ELSE IF ( check_triclinic_conditions_met(C_matrix, TOLERANCE_LOCAL) ) THEN ! Assumed if no higher symmetry matched
            crystal_system_name = "Triclinic"
            system_identified = .TRUE.
            PRINT *, "Crystal System Identified: Triclinic (Laue class 1bar)"
            CALL display_triclinic_matrix_form()
            PRINT *, "Stability for Triclinic: Checking Born criteria via leading principal minors."
            CALL check_stability_by_leading_minors(C_matrix, "Triclinic")
        ELSE
             PRINT *, "Could not determine crystal system or Cij values do not conform to any standard system defined in the paper."
        END IF
    END SUBROUTINE analyze_elastic_constants

!=======================================================================
    !
    ! START OF NEW 2D SYSTEM DEFINITIONS AND STABILITY CHECKS
    !
    !=======================================================================

    !-----------------------------------------------------------------------
    ! Functions to check if 2D Cij matrix conforms to a crystal system.
    ! Note: Cij matrix for 2D is 3x3, with indices corresponding to
    ! 1=xx, 2=yy, 6=xy (or 3 in this code's 3x3 matrix representation).
    ! C(1,3) is C16, C(2,3) is C26, C(3,3) is C66.
    !-----------------------------------------------------------------------

    LOGICAL FUNCTION check_hexagonal_2d_conditions_met(Cm, tol)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(3,3), INTENT(IN) :: Cm
        REAL(KIND=8), INTENT(IN) :: tol
        check_hexagonal_2d_conditions_met = .FALSE.
        ! Conditions: C11=C22, C16=C26=0, C66=(C11-C12)/2
        IF (near_equal(Cm(1,1), Cm(2,2), tol) .AND. &
            is_zero(Cm(1,3), tol) .AND. is_zero(Cm(2,3), tol) .AND. &
            near_equal(Cm(3,3), (Cm(1,1)-Cm(1,2))/2.0_8, tol) ) THEN
            check_hexagonal_2d_conditions_met = .TRUE.
        END IF
    END FUNCTION check_hexagonal_2d_conditions_met

    LOGICAL FUNCTION check_square_2d_conditions_met(Cm, tol)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(3,3), INTENT(IN) :: Cm
        REAL(KIND=8), INTENT(IN) :: tol
        check_square_2d_conditions_met = .FALSE.
        ! Conditions: C11=C22, C16=C26=0, C66 is independent
        IF (near_equal(Cm(1,1), Cm(2,2), tol) .AND. &
            is_zero(Cm(1,3), tol) .AND. is_zero(Cm(2,3), tol) .AND. &
            .NOT. near_equal(Cm(3,3), (Cm(1,1)-Cm(1,2))/2.0_8, tol*5.0_8) ) THEN
            check_square_2d_conditions_met = .TRUE.
        END IF
    END FUNCTION check_square_2d_conditions_met

    LOGICAL FUNCTION check_rectangular_2d_conditions_met(Cm, tol)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(3,3), INTENT(IN) :: Cm
        REAL(KIND=8), INTENT(IN) :: tol
        check_rectangular_2d_conditions_met = .FALSE.
        ! Conditions: C11!=C22, C16=C26=0
        IF (.NOT. near_equal(Cm(1,1), Cm(2,2), tol) .AND. &
            is_zero(Cm(1,3), tol) .AND. is_zero(Cm(2,3), tol) ) THEN
            check_rectangular_2d_conditions_met = .TRUE.
        END IF
    END FUNCTION check_rectangular_2d_conditions_met

    !-----------------------------------------------------------------------
    ! Subroutines to display schematic 2D Cij matrix forms
    !-----------------------------------------------------------------------
    SUBROUTINE display_hexagonal_2d_matrix_form()
        IMPLICIT NONE
        WRITE(*,*) "Schematic C_ij matrix form for 2D Hexagonal (Isotropic) system:"
        WRITE(*,'(A)') " | C11      C12        0         |"
        WRITE(*,'(A)') " | C12      C11        0         |"
        WRITE(*,'(A)') " |  0        0      (C11-C12)/2 |"
        WRITE(*,*) ""
        
        WRITE(99,*) "Schematic C_ij matrix form for 2D Hexagonal (Isotropic) system:"
        WRITE(99,'(A)') " | C11      C12        0         |"
        WRITE(99,'(A)') " | C12      C11        0         |"
        WRITE(99,'(A)') " |  0        0      (C11-C12)/2 |"
        WRITE(99,*) ""
    END SUBROUTINE display_hexagonal_2d_matrix_form

    SUBROUTINE display_square_2d_matrix_form()
        IMPLICIT NONE
        WRITE(*,*) "Schematic C_ij matrix form for 2D Square (Tetragonal) system:"
        WRITE(*,'(A)') " | C11   C12    0   |"
        WRITE(*,'(A)') " | C12   C11    0   |"
        WRITE(*,'(A)') " |  0     0    C66  |"
        WRITE(*,*) ""
        
        WRITE(99,*) "Schematic C_ij matrix form for 2D Square (Tetragonal) system:"
        WRITE(99,'(A)') " | C11   C12    0   |"
        WRITE(99,'(A)') " | C12   C11    0   |"
        WRITE(99,'(A)') " |  0     0    C66  |"
        WRITE(99,*) ""
    END SUBROUTINE display_square_2d_matrix_form

    SUBROUTINE display_rectangular_2d_matrix_form()
        IMPLICIT NONE
        WRITE(*,*) "Schematic C_ij matrix form for 2D Rectangular (Orthorhombic) system:"
        WRITE(*,'(A)') " | C11   C12    0   |"
        WRITE(*,'(A)') " | C12   C22    0   |"
        WRITE(*,'(A)') " |  0     0    C66  |"
        WRITE(*,*) ""
        
        WRITE(99,*) "Schematic C_ij matrix form for 2D Rectangular (Orthorhombic) system:"
        WRITE(99,'(A)') " | C11   C12    0   |"
        WRITE(99,'(A)') " | C12   C22    0   |"
        WRITE(99,'(A)') " |  0     0    C66  |"
        WRITE(99,*) ""
    END SUBROUTINE display_rectangular_2d_matrix_form

    SUBROUTINE display_oblique_2d_matrix_form()
        IMPLICIT NONE
        WRITE(*,*) "Schematic C_ij matrix form for 2D Oblique (Anisotropic) system:"
        WRITE(*,'(A)') " | C11   C12   C16  |"
        WRITE(*,'(A)') " | C12   C22   C26  |"
        WRITE(*,'(A)') " | C16   C26   C66  |"
        WRITE(*,*) ""
        
        WRITE(99,*) "Schematic C_ij matrix form for 2D Oblique (Anisotropic) system:"
        WRITE(99,'(A)') " | C11   C12   C16  |"
        WRITE(99,'(A)') " | C12   C22   C26  |"
        WRITE(99,'(A)') " | C16   C26   C66  |"
        WRITE(99,*) ""
    END SUBROUTINE display_oblique_2d_matrix_form

    !-----------------------------------------------------------------------
    ! Subroutines to check 2D stability conditions for each crystal system.
    !-----------------------------------------------------------------------

    SUBROUTINE check_hexagonal_2d_stability(lc11, lc12)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: lc11, lc12
        LOGICAL :: condition_met(2)
        WRITE(*,*) "2D Hexagonal Stability Conditions:"
        condition_met(1) = (lc11 > 0.0_8)
        condition_met(2) = (lc11 > ABS(lc12))
        WRITE(*,'(A,L1,A,F10.4,A)') "1. C11 > 0        : ", condition_met(1), " (Val: ", lc11, ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "2. C11 > |C12|    : ", condition_met(2), " (Val: ", lc11 - ABS(lc12), ")"
        
        WRITE(99,'(A,L1,A,F10.4,A)') "1. C11 > 0        : ", condition_met(1), " (Val: ", lc11, ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "2. C11 > |C12|    : ", condition_met(2), " (Val: ", lc11 - ABS(lc12), ")"
        IF (ALL(condition_met)) THEN
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " > Born Stability Criteria Result: STABLE  ✓";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: STABLE  ✓" 
            WRITE(99,'(A)') " =======================================" 
        ELSE
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " > Born Stability Criteria Result: UNSTABLE ✗; STOP";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: UNSTABLE ✗" 
            WRITE(99,'(A)') " ======================================="  
        END IF
        WRITE(*,*) ""
    END SUBROUTINE check_hexagonal_2d_stability

    SUBROUTINE check_square_2d_stability(lc11, lc12, lc66)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: lc11, lc12, lc66
        LOGICAL :: condition_met(3)
        WRITE(*,*) "2D Square Stability Conditions:"
        condition_met(1) = (lc11 > 0.0_8)
        condition_met(2) = (lc66 > 0.0_8)
        condition_met(3) = (lc11 > ABS(lc12))
        WRITE(*,'(A,L1,A,F10.4,A)') "1. C11 > 0        : ", condition_met(1), " (Val: ", lc11, ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "2. C66 > 0        : ", condition_met(2), " (Val: ", lc66, ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "3. C11 > |C12|    : ", condition_met(3), " (Val: ", lc11 - ABS(lc12), ")"
        
        WRITE(99,'(A,L1,A,F10.4,A)') "1. C11 > 0        : ", condition_met(1), " (Val: ", lc11, ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "2. C66 > 0        : ", condition_met(2), " (Val: ", lc66, ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "3. C11 > |C12|    : ", condition_met(3), " (Val: ", lc11 - ABS(lc12), ")"
        IF (ALL(condition_met)) THEN
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " > Born Stability Criteria Result: STABLE  ✓";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: STABLE  ✓" 
            WRITE(99,'(A)') " =======================================" 
        ELSE
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " > Born Stability Criteria Result: UNSTABLE ✗; STOP";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: UNSTABLE ✗" 
            WRITE(99,'(A)') " ======================================="  
        END IF
        WRITE(*,*) ""
    END SUBROUTINE check_square_2d_stability

    SUBROUTINE check_rectangular_2d_stability(lc11, lc12, lc22, lc66)
        IMPLICIT NONE
        REAL(KIND=8), INTENT(IN) :: lc11, lc12, lc22, lc66
        LOGICAL :: condition_met(3)
        WRITE(*,*) "2D Rectangular Stability Conditions:"
        condition_met(1) = (lc11 > 0.0_8)
        condition_met(2) = (lc66 > 0.0_8)
        condition_met(3) = (lc11*lc22 > lc12**2)
        WRITE(*,'(A,L1,A,F10.4,A)') "1. C11 > 0              : ", condition_met(1), " (Val: ", lc11, ")"
        WRITE(*,'(A,L1,A,F10.4,A)') "2. C66 > 0              : ", condition_met(2), " (Val: ", lc66, ")"
        WRITE(*,'(A,L1,A,F12.4,A,F12.4,A)') "3. C11*C22 > C12^2    : ", condition_met(3), &
        &                                    " (LHS:", lc11*lc22, ",RHS:", lc12**2, ")"
        
        WRITE(99,'(A,L1,A,F10.4,A)') "1. C11 > 0              : ", condition_met(1), " (Val: ", lc11, ")"
        WRITE(99,'(A,L1,A,F10.4,A)') "2. C66 > 0              : ", condition_met(2), " (Val: ", lc66, ")"
        WRITE(99,'(A,L1,A,F12.4,A,F12.4,A)') "3. C11*C22 > C12^2    : ", condition_met(3), &
        &                                    " (LHS:", lc11*lc22, ",RHS:", lc12**2, ")"
        IF (ALL(condition_met)) THEN
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " > Born Stability Criteria Result: STABLE  ✓";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: STABLE  ✓" 
            WRITE(99,'(A)') " =======================================" 
        ELSE
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " > Born Stability Criteria Result: UNSTABLE ✗; STOP";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: UNSTABLE ✗" 
            WRITE(99,'(A)') " ======================================="  
        END IF
        WRITE(*,*) ""
    END SUBROUTINE check_rectangular_2d_stability

    SUBROUTINE check_oblique_2d_stability(Cm)
        IMPLICIT NONE
        REAL(KIND=8), DIMENSION(3,3), INTENT(IN) :: Cm
        REAL(KIND=8) :: lc11, lc12, lc22, determinant_val
        LOGICAL :: condition_met(3)
        
        lc11 = Cm(1,1)
        lc12 = Cm(1,2)
        lc22 = Cm(2,2)
        
        determinant_val = Cm(1,1)*(Cm(2,2)*Cm(3,3) - Cm(2,3)*Cm(3,2)) &
                        - Cm(1,2)*(Cm(2,1)*Cm(3,3) - Cm(2,3)*Cm(3,1)) &
                        + Cm(1,3)*(Cm(2,1)*Cm(3,2) - Cm(2,2)*Cm(3,1))

        WRITE(*,*) "2D Oblique Stability Conditions:"
        condition_met(1) = (lc11 > 0.0_8)
        condition_met(2) = (lc11*lc22 > lc12**2)
        condition_met(3) = (determinant_val > 0.0_8)
        WRITE(*,'(A,L1,A,F10.4,A)') "1. C11 > 0              : ", condition_met(1), " (Val: ", lc11, ")"
        WRITE(*,'(A,L1,A,F12.4,A,F12.4,A)') "2. C11*C22 > C12^2    : ", condition_met(2), &
        &                                    " (LHS:", lc11*lc22, ",RHS:", lc12**2, ")"
        WRITE(*,'(A,L1,A,ES18.6E2,A)') "3. det(C_ij) > 0      : ", condition_met(3), " (Val: ", determinant_val, ")"
        
        WRITE(99,'(A,L1,A,F10.4,A)') "1. C11 > 0              : ", condition_met(1), " (Val: ", lc11, ")"
        WRITE(99,'(A,L1,A,F12.4,A,F12.4,A)') "2. C11*C22 > C12^2    : ", condition_met(2), &
        &                                    " (LHS:", lc11*lc22, ",RHS:", lc12**2, ")"
        WRITE(99,'(A,L1,A,ES18.6E2,A)') "3. det(C_ij) > 0      : ", condition_met(3), " (Val: ", determinant_val, ")"
        IF (ALL(condition_met)) THEN
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " > Born Stability Criteria Result: STABLE  ✓";tput sgr0')
            CALL system ('tput setaf 10;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: STABLE  ✓" 
            WRITE(99,'(A)') " =======================================" 
        ELSE
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " > Born Stability Criteria Result: UNSTABLE ✗; STOP";tput sgr0')
            CALL system ('tput setaf 9;tput bold; echo " =======================================";tput sgr0')
            
            WRITE(99,'(A)') " =======================================" 
            WRITE(99,'(A)') " > Born Stability Criteria Result: UNSTABLE ✗" 
            WRITE(99,'(A)') " ======================================="  
        END IF
        WRITE(*,*) ""
    END SUBROUTINE check_oblique_2d_stability

    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    !+ NEW Main driver subroutine to analyze 2D elastic constants          +
    !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    SUBROUTINE analyze_2d_elastic_constants(cij_filepath)
        IMPLICIT NONE
        CHARACTER(LEN=*), INTENT(IN) :: cij_filepath
        REAL(KIND=8), DIMENSION(3,3) :: C_matrix_2d
        REAL(KIND=8) :: C11, C12, C16, C22, C26, C66
        REAL(KIND=8), PARAMETER :: TOLERANCE_LOCAL = 1.0E-4
        INTEGER :: i, j, k, unit_num
        CHARACTER(LEN=30) :: crystal_system_name
        LOGICAL :: system_identified

        OPEN(NEWUNIT=unit_num, FILE=cij_filepath, STATUS='OLD', ACTION='READ', IOSTAT=i)
        IF (i /= 0) THEN
            PRINT *, "Error: Could not open file: ", TRIM(cij_filepath)
            PRINT *, "Please ensure 'Cij-2D.dat' exists and contains the 3x3 elastic matrix."
            RETURN
        END IF

        DO i = 1, 3
            READ(unit_num, *, IOSTAT=j) (C_matrix_2d(i,k), k=1,3)
            IF (j /= 0) THEN
                PRINT *, "Error reading line ", i, " from file: ", TRIM(cij_filepath)
                CLOSE(unit_num)
                RETURN
            END IF
        END DO
        CLOSE(unit_num)

        ! Enforce symmetry
        DO i = 1, 3
            DO j = i + 1, 3
                C_matrix_2d(j,i) = C_matrix_2d(i,j)
            END DO
        END DO

        ! Map 3x3 matrix to Voigt notation components
        C11 = C_matrix_2d(1,1); C12 = C_matrix_2d(1,2); C16 = C_matrix_2d(1,3)
        C22 = C_matrix_2d(2,2); C26 = C_matrix_2d(2,3)
        C66 = C_matrix_2d(3,3)

        crystal_system_name = "Undetermined"
        system_identified = .FALSE.

        IF ( check_hexagonal_2d_conditions_met(C_matrix_2d, TOLERANCE_LOCAL) ) THEN
            crystal_system_name = "Hexagonal (2D)"
            system_identified = .TRUE.
            PRINT *, "2D Crystal System Identified: Hexagonal / Isotropic"
            CALL display_hexagonal_2d_matrix_form()
            CALL check_hexagonal_2d_stability(C11, C12)
        ELSE IF ( check_square_2d_conditions_met(C_matrix_2d, TOLERANCE_LOCAL) ) THEN
            crystal_system_name = "Square (2D)"
            system_identified = .TRUE.
            PRINT *, "2D Crystal System Identified: Square / Tetragonal"
            CALL display_square_2d_matrix_form()
            CALL check_square_2d_stability(C11, C12, C66)
        ELSE IF ( check_rectangular_2d_conditions_met(C_matrix_2d, TOLERANCE_LOCAL) ) THEN
            crystal_system_name = "Rectangular (2D)"
            system_identified = .TRUE.
            PRINT *, "2D Crystal System Identified: Rectangular / Orthorhombic"
            CALL display_rectangular_2d_matrix_form()
            CALL check_rectangular_2d_stability(C11, C12, C22, C66)
        ELSE
            ! If none of the higher symmetries match, it's Oblique (Anisotropic)
            crystal_system_name = "Oblique (2D)"
            system_identified = .TRUE.
            PRINT *, "2D Crystal System Identified: Oblique / Anisotropic"
            CALL display_oblique_2d_matrix_form()
            CALL check_oblique_2d_stability(C_matrix_2d)
        END IF
    END SUBROUTINE analyze_2d_elastic_constants


END MODULE ElasticAnalysisHelpers

!=======================================================================
! Main Subroutine to be called from the main program
! It now directs the analysis based on dimensionality
!=======================================================================
Subroutine born_stb(dim_system)
   USE ElasticAnalysisHelpers
   implicit none
   INTEGER, INTENT(IN) :: dim_system

   IF (dim_system == 3) THEN
      write(*,*) "---------------------------------------------------------------"
      write(*,*) "--- Performing 3D Elastic Stability Analysis ---"
      write(*,*) "---------------------------------------------------------------"
      CALL analyze_elastic_constants('Cij.dat')
   ELSE IF (dim_system == 2) THEN
      write(*,*) "---------------------------------------------------------------"
      write(*,*) "--- Performing 2D Elastic Stability Analysis ---"
      write(*,*) "---------------------------------------------------------------"
      CALL analyze_2d_elastic_constants('Cij-2D.dat')
   ELSE
      write(*,*) "Error: Invalid system dimensionality provided. Must be 2 or 3."
   END IF

end Subroutine born_stb
