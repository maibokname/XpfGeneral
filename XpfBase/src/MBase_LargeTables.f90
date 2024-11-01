#define     USE_FULL_TABLE_OF_POWERS_OF_TEN

MODULE MBase_LargeTables

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains large look-up tables. <br>

!** USE STATEMENTS:
    USE MBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

    PUBLIC

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! -------------------------------------------------------------------------
    ! -----   reciprocity table for fast divisions                        -----
    ! -------------------------------------------------------------------------
#include        "Includes/Table - Reciprocity.f90"
    ! -------------------------------------------------------------------------
    ! -----   tables of digit characters                                  -----
    ! -------------------------------------------------------------------------
#include        "Includes/Table - CharDigits.f90"
    ! -------------------------------------------------------------------------
    ! -----   parameters for high-precision decimal conversion algorithm  -----
    ! -------------------------------------------------------------------------
#include        "Includes/Table - LeftShift.f90"
    ! -------------------------------------------------------------------------------
    ! -----   parameters for powers of 10 generation used by various algorithms -----
    ! -------------------------------------------------------------------------------
#ifdef          USE_FULL_TABLE_OF_POWERS_OF_TEN
#   include     "Includes/Table - PowTen 256 Bits - Full - 1.f90"
#   include     "Includes/Table - PowTen 256 Bits - Full - 2.f90"
#   include     "Includes/Table - PowTen 256 Bits - Full - 3.f90"
#   include     "Includes/Table - PowTen 256 Bits - Full - 4.f90"
#   include     "Includes/Table - PowTen 256 Bits - Full - 5.f90"
#   include     "Includes/Table - PowTen 256 Bits - Full - 6.f90"
#   include     "Includes/Table - PowTen 256 Bits - Full - 7.f90"
#   include     "Includes/Table - PowTen 256 Bits - Full - 8.f90"
#   include     "Includes/Table - PowTen 256 Bits - Full - 9.f90"
#   include     "Includes/Table - PowTen 256 Bits - Full - 10.f90"
#endif
#include     "Includes/Table - PowTen 256 Bits - Small.f90"
#include     "Includes/Table - PowTen 256 Bits - Compressed.f90"


!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

END MODULE MBase_LargeTables

!******************************************************************************
