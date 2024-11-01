
MODULE MBase_Kinds

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains kind parameters used in the library.

!** USE STATEMENTS:
    USE, INTRINSIC :: ISO_FORTRAN_ENV,  ONLY: INT8, INT16, INT32, INT64, REAL32, REAL64, REAL128

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC          ! By default, all parameters and derived types which are placed in
                    ! this data-only module should be available to other modules and routines.

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! symbolic names for kind types of 8-, 16-, 32-, and 64-bit integer numbers:
    INTEGER, PARAMETER  :: kInt8  = INT8    !! kind for  8-bit integers
    INTEGER, PARAMETER  :: kInt16 = INT16   !! kind for 16-bit integers
    INTEGER, PARAMETER  :: kInt32 = INT32   !! kind for 32-bit integers
    INTEGER, PARAMETER  :: kInt64 = INT64   !! kind for 64-bit integers
#ifdef Indx32Bits
    INTEGER, PARAMETER  :: kIndex = kInt32  !! kind for indices
#else
    INTEGER, PARAMETER  :: kIndex = kInt64  !! kind for indices
#endif
    ! symbolic names for kind types of quadruple-, double-, and single-precision real and complex numbers:
    INTEGER, PARAMETER  :: kSingle = REAL32     !! kind for  32-bit real numbers
    INTEGER, PARAMETER  :: kDouble = REAL64     !! kind for  64-bit real numbers
    INTEGER, PARAMETER  :: kQuad   = REAL128    !! kind for 128-bit real numbers
#ifdef AppQuadruple
    INTEGER, PARAMETER  :: kFloat = kQuad       !! kind for a library-default real numbers
#else
#   ifdef AppDouble
    INTEGER, PARAMETER  :: kFloat = kDouble     !! kind for a library-default real numbers
#   else
    INTEGER, PARAMETER  :: kFloat = kSingle     !! kind for a library-default real numbers
#   endif
#endif
    ! character kinds
    INTEGER, PARAMETER  :: kAscii   = SELECTED_CHAR_KIND('ASCII')       !! kind for ASCII character sets
#ifdef  __GFORTRAN__
    INTEGER, PARAMETER  :: kUnicode = SELECTED_CHAR_KIND('ISO_10646')   !! kind for Unicode character sets
#endif
    INTEGER, PARAMETER  :: kChar    = kAscii    !! kind for default character sets
#ifdef  __GFORTRAN__
    INTEGER, PARAMETER  :: kwChar   = kUnicode  !! kind for wide-character sets
#endif

END MODULE MBase_Kinds

!******************************************************************************
