
MODULE MBase_Common

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various parameters commonly used throughout the library.

!** USE STATEMENTS:
    USE, INTRINSIC :: ISO_C_BINDING,    ONLY: C_PTR, C_NULL_PTR
    USE MBase_Kinds

    IMPLICIT NONE   ! Enforce explicit typing of all variables

    PUBLIC          ! By default, all parameters and derived types which are placed in
                    ! this data-only module should be available to other modules and routines.

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! true and false values
    tLogical, PARAMETER :: TrueVal  = .TRUE.
    tLogical, PARAMETER :: FalseVal = .FALSE.
    ! angle unit flag
    tSInt32,  PARAMETER :: Degree = 1
    tSInt32,  PARAMETER :: Radian = 2
    ! commonly used numeric constants (i.e. whole real number)
    tFloat,   PARAMETER :: Zero         = 0.0_kFloat
    tFloat,   PARAMETER :: One          = 1.0_kFloat
    tFloat,   PARAMETER :: Two          = 2.0_kFloat
    tFloat,   PARAMETER :: Three        = 3.0_kFloat
    tFloat,   PARAMETER :: Four         = 4.0_kFloat
    tFloat,   PARAMETER :: Five         = 5.0_kFloat
    tFloat,   PARAMETER :: Six          = 6.0_kFloat
    tFloat,   PARAMETER :: Seven        = 7.0_kFloat
    tFloat,   PARAMETER :: Eight        = 8.0_kFloat
    tFloat,   PARAMETER :: Nine         = 9.0_kFloat
    tFloat,   PARAMETER :: Ten          = 10.0_kFloat
    tFloat,   PARAMETER :: Hundred      = 100.0_kFloat
    tFloat,   PARAMETER :: Thousand     = 1000.0_kFloat
    tFloat,   PARAMETER :: Million      = 1000000.0_kFloat
    tFloat,   PARAMETER :: Billion      = 1000000000.0_kFloat
    ! common fractions
    tFloat,   PARAMETER :: Quater       = 0.25_kFloat
    tFloat,   PARAMETER :: Half         = 0.5_kFloat
    tFloat,   PARAMETER :: ThreeQuater  = 0.75_kFloat
    tFloat,   PARAMETER :: OneThird     = One/Three
    tFloat,   PARAMETER :: TwoThird     = Two/Three
    ! frequently used mathematical constants (with precision to spare):
    tFloat,   PARAMETER :: Pi           = 3.141592653589793238462643383279502884197_kFloat
    tFloat,   PARAMETER :: PiOvr2       = Half * Pi
    tFloat,   PARAMETER :: Pi3Ovr2      = 1.5_kFloat * Pi
    tFloat,   PARAMETER :: TwoPi        = Two * Pi
    ! defined tolerance value
    tFloat,   PARAMETER :: Zero01       = 1.0E-1_kFloat
    tFloat,   PARAMETER :: Zero02       = 1.0E-2_kFloat
    tFloat,   PARAMETER :: Zero03       = 1.0E-3_kFloat
    tFloat,   PARAMETER :: Zero04       = 1.0E-4_kFloat
    tFloat,   PARAMETER :: Zero05       = 1.0E-5_kFloat
    tFloat,   PARAMETER :: Zero06       = 1.0E-6_kFloat
    tFloat,   PARAMETER :: Zero07       = 1.0E-7_kFloat
    tFloat,   PARAMETER :: Zero08       = 1.0E-8_kFloat
    tFloat,   PARAMETER :: Zero09       = 1.0E-9_kFloat
    tFloat,   PARAMETER :: Zero10       = 1.0E-10_kFloat
    tFloat,   PARAMETER :: Zero11       = 1.0E-11_kFloat
    tFloat,   PARAMETER :: Zero12       = 1.0E-12_kFloat
    tFloat,   PARAMETER :: Zero13       = 1.0E-13_kFloat
    tFloat,   PARAMETER :: Zero14       = 1.0E-14_kFloat
    tFloat,   PARAMETER :: Zero15       = 1.0E-15_kFloat
    tFloat,   PARAMETER :: Zero16       = 1.0E-16_kFloat
    tFloat,   PARAMETER :: Zero17       = 1.0E-17_kFloat
    ! machine dependent parameters
    tFloat,   PARAMETER :: MachineEps   = EPSILON(One)      !! machine epsilon
    tFloat,   PARAMETER :: Small        = TINY(One)         !! the smallest positive number
    tFloat,   PARAMETER :: Large        = HUGE(One)         !! the largest positive number
    tFloat,   PARAMETER :: SqrtEps      = SQRT(MachineEps)  !! square root of MachineEps
    ! huge (maximum) numbers for intrinsic types used to prevent overflow
    tSInt8,   PARAMETER :: Huge_I1B = HUGE(1_kInt8)         !! = 127
    tSInt16,  PARAMETER :: Huge_I2B = HUGE(1_kInt16)        !! = 32767
    tSInt32,  PARAMETER :: Huge_I4B = HUGE(1_kInt32)        !! = 2147483647
    tSInt64,  PARAMETER :: Huge_I8B = HUGE(1_kInt64)        !! = 9223372036854775807
    tRealSP,  PARAMETER :: Huge_RSP = HUGE(1.0_kSingle)     !! = 3.4028235E+38
    tRealDP,  PARAMETER :: Huge_RDP = HUGE(1.0_kDouble)     !! = 1.797693134862316E+308
    tRealQP,  PARAMETER :: Huge_RQP = HUGE(1.0_kQuad)       !! = 1.189731495357231765085759326628007E+4932
    ! tiny (positive minimum) numbers for floating point types used to prevent underflow (normal range)
    tRealSP,  PARAMETER :: Tiny_RSP = TINY(1.0_kSingle)     !! = 1.1754944E-38
    tRealDP,  PARAMETER :: Tiny_RDP = TINY(1.0_kDouble)     !! = 2.225073858507201E-308
    tRealQP,  PARAMETER :: Tiny_RQP = TINY(1.0_kQuad)       !! = 3.362103143112093506262677817321753E-4932
    ! machine epsilon numbers for floating point types used to check accuracy tolerance
    tRealSP,  PARAMETER :: Eps_RSP = EPSILON(1.0_kSingle)   !! = 1.1920929E-07
    tRealDP,  PARAMETER :: Eps_RDP = EPSILON(1.0_kDouble)   !! = 2.220446049250313E-016
    tRealQP,  PARAMETER :: Eps_RQP = EPSILON(1.0_kQuad)     !! = 1.925929944387235853055977942584927E-0034
    ! miscellaneous
    tLogical, PARAMETER :: IsLittleEndian = (1_kInt16 == TRANSFER([1_kInt8, 0_kInt8], 0_kInt16))
    tCharParam          :: LibName = "XpfLib"

!** DERIVED TYPE DEFINITIONS
    !> container type that utilize 'c' pointer (C_PTR) type
    TYPE, BIND(C) :: Container
        !> storage
        TYPE(C_PTR)     :: Store = C_NULL_PTR
    END TYPE Container

!** MODULE VARIABLE DECLARATIONS:
    tLogical    :: Verbose = FalseVal               !! global variable used for error reporting
    tLogical    :: Is_FMLib_Initialized = FalseVal  !! global variable used with FMLib library
    tLogical    :: Is_MPFun_Initialized = FalseVal  !! global variable used with MPFun library

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!**************************************************************************************

SUBROUTINE SetVerbosity(IsVerbose)

!** PURPOSE OF THIS SUBROUTINE:
    !^  To set the Verbose global variable.

!** SUBROUTINE ARGUMENT DEFINITIONS:
    tLogical, INTENT(IN)    :: IsVerbose

!** SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Verbose = IsVerbose

    RETURN

END SUBROUTINE SetVerbosity

!******************************************************************************

END MODULE MBase_Common

!******************************************************************************
