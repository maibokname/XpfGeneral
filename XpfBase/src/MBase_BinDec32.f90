
MODULE MBase_BinDec32

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various binary-to-decimal and decimal-to-binary conversion routines for
!   32-bit floating-point numbers (i.e. single-precision real numbers).  It provides these routines
!   as a building block for higher-level formatting/parsing procedures with high performance that
!   perform conversions between human-readable strings and floating-point (real) numbers. <br>
!   <br>
!  **REFERENCE TECHNICAL ARTICLES**: <br>
!   [1] <a href="https://github.com/jk-jeon/dragonbox/blob/master/other_files/Dragonbox.pdf">
!       Junekey Jeon.  "Dragonbox: A New Floating-Point Binary-to-Decimal Conversion Algorithm".</a> <br>
!   [2] <a href="https://dl.acm.org/doi/10.1145/3192366.3192369">
!       Ulf Adams.  "Ryu: Fast Float-to-String Conversion".</a> <br>
!   [3] <a href="https://drive.google.com/open?id=1luHhyQF9zKlM8yJ1nebU0OgVYhfC6CBN">
!       Raffaello Giulietti.  "The Schubfach way to render doubles".</a> <br>
!   [4] <a href="https://doi.org/10.1145/989393.989430">
!       Clinger WD. How to Read Floating Point Numbers Accurately. SIGPLAN Not 2004 Apr;
!       39(4):360-371.</a> <br>
!   [5] <a href="https://arxiv.org/abs/2101.11408">Daniel Lemire.  "Number Parsing at a
!       Gigabyte per Second", Software: Practice and Experience 51 (8), 2021.</a> <br>
!   [6] <a href="https://arxiv.org/abs/2212.06644">Noble Mushtak and Daniel Lemire.
!       "Fast  Number Parsing Without Fallback", Software: Practice and Experience
!       53 (7), 2023.</a> <br>
!   [7] <a href="https://hal.inria.fr/hal-00864293v1/document">Bouvier & Zimmermann.
!       "Division-Free Binary-to-Decimal Conversion".</a> <br>
!   [8] Hacker's Delight, 2nd Edition.
!   [9] <a href="https://nigeltao.github.io/blog/2020/eisel-lemire.html">Nigel Tao.
!       "The Eisel-Lemire ParseNumberF64 Algorithm".</a> <br>
!   [10] <a href="https://nigeltao.github.io/blog/2020/parse-number-f64-simple.html">
!       Nigel Tao.  "ParseNumberF64 by Simple Decimal Conversion".</a> <br>
!   <br>
!   **REFERENCE CODE IMPLEMENTATION**: <br>
!   [11] <a href="https://github.com/jk-jeon/dragonbox">DragonBox: C++ reference
!       implementation.</a> <br>
!   [12] <a href="https://github.com/ulfjack/ryu">Ryu: C reference implementation.</a> <br>
!   [13] <a href="https://github.com/c4f7fcce9cb06515/Schubfach">Schubfach: Java
!       reference implementation.</a> <br>
!   [14] <a href="https://github.com/abolz/Drachennest">Drachennest: Different
!       algorithms for converting binary to decimal floating-point numbers.</a> <br>
!   [15] <a href="https://github.com/ibireme/c_numconv_benchmark">Number Conversion
!       Benchmark in C.</a> <br>
!   [16] <a href="https://github.com/fastfloat/fast_float">Fast_Float Number
!       Parsing Library.</a> <br>
!   [17] <a href="https://github.com/lemire/fast_double_parser">Fast_Double_Parser.</a> <br>
!   [18] <a href="https://github.com/llvm/llvm-project/tree/main/libc/src/__support">
!       The LLVM Project (LibC).</a> <br>
!   [19] <a href="https://github.com/google/double-conversion">Double Conversion:
!       Efficient binary-decimal and decimal-binary conversion routines for IEEE doubles.</a> <br>
!   [20] <a href="https://github.com/fmtlib/fmt">fmt: A modern formatting library.</a> <br>

!** USE STATEMENTS:
    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE ISO_C_BINDING,      ONLY: C_LOC, C_F_POINTER, C_PTR, C_NULL_PTR
    USE MBase_Common
    USE MBase_FloatUtil
    USE MBase_SIntUtil
    USE MBase_UIntUtil
    USE MBase_UInt128
    USE MBase_LargeTables
    USE MBase_BinDec64,     ONLY: StringAux

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! Binary-To-Decimal
    PUBLIC :: Bin2Dec_DragonBox32
    PUBLIC :: Bin2Dec_Ryu32
    PUBLIC :: Bin2Dec_Schubfach32
    ! Decimal-To-Binary
    PUBLIC :: Dec2Bin_Clinger32
    PUBLIC :: Dec2Bin_FastFloat32
    PUBLIC :: Dec2Bin_Lemire32
    PUBLIC :: Dec2Bin_LibC32
    PUBLIC :: Dec2Bin_YY32
    ! auxiliary
    PUBLIC :: Format_Real32
    PUBLIC :: Parse_Fortran_String32
    PUBLIC :: Parse_FPlus_String32
    PUBLIC :: Parse_JSON_String32
    PUBLIC :: Handle_Invalid_String32

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#undef      tFloat
#undef      ToFloat
#define     tFloat_is_tSingle
! variable types
#define     tUInt128                        TYPE(UInt128)
#define     tFloat                          tRealSP
#define     tUIntType                       tUInt32
#define     BinRep                          BinRep32
! common parameters
#define     ZeroUInt                        0_kInt32
#define     OneUInt                         1_kInt32
#define     ZeroFloat                       0.0_kSingle
#define     OneFloat                        1.0_kSingle
! type conversions
#define     ToI32(X)                        ToInt32(X)
#define     ToUIntType(X)                   ToInt32(X)
#define     ToFloat(X)                      REAL(X, KIND=kSingle)
! characteristics of IEEE-754 & related binary floating-point numbers
#define     BinaryPrecision                 Float32%BinPrecision
#define     TotalBits                       Float32%TotBits
#define     SignBits                        Float32%SgnBits
#define     SignificandBits                 Float32%SigBits
#define     ExponentBits                    Float32%ExpBits
#define     MaxExponent                     Float32%MaxExp
#define     ExponentBias                    Float32%ExpBias
#define     DecimalPrecision                Float32%DecPrecision
#define     DecimalRange                    Float32%DecRange
#define     MaxDecimalConversionDigits      Float32%MaxDecConvDigits
! masking parameters
#define     SigHidBitMask                   FpMask32%SigHidBit
#define     SignificandMask                 FpMask32%Significand
#define     SignMask                        FpMask32%Sign
#define     ExponentMask                    FpMask32%Exponent
#define     ExpMantMask                     FpMask32%ExpMant
#define     QuietNaNMask                    FpMask32%QuietNaN
! public procedures/types
#define     Bin2Dec_DragonBox               Bin2Dec_DragonBox32
#define     Bin2Dec_Ryu                     Bin2Dec_Ryu32
#define     Bin2Dec_Schubfach               Bin2Dec_Schubfach32
#define     Dec2Bin_Clinger                 Dec2Bin_Clinger32
#define     Dec2Bin_FastFloat               Dec2Bin_FastFloat32
#define     Dec2Bin_Lemire                  Dec2Bin_Lemire32
#define     Dec2Bin_LibC                    Dec2Bin_LibC32
#define     Dec2Bin_YY                      Dec2Bin_YY32
#define     Handle_Invalid_String           Handle_Invalid_String32
#define     Format_RealSP                   Format_Real32
#define     Parse_Fortran_String            Parse_Fortran_String32
#define     Parse_FPlus_String              Parse_FPlus_String32
#define     Parse_JSON_String               Parse_JSON_String32

!** MODULE PARAMETERS:
    ! -----------------------------------------------------------------
    ! -----     options for type of number to be parsed           -----
    ! -----------------------------------------------------------------
!    tSInt32, PARAMETER, PUBLIC  :: FortNum  = 1     ! strict Fortran number
!    tSInt32, PARAMETER, PUBLIC  :: FPlusNum = 2     ! relaxed Fortran number
!    tSInt32, PARAMETER, PUBLIC  :: JsonNum  = 3     ! JSON number
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ maximum and minimum (positive) parameters +++
    tUInt32, PARAMETER  :: MinSubnormal = 1
    tUInt32, PARAMETER  :: MaxSubnormal = SHIFTL(1, SignificandBits) - 1
    tUInt32, PARAMETER  :: MinNormal    = SHIFTL(1, SignificandBits)
    tUInt32, PARAMETER  :: MaxNormal    = IOR(SHIFTL((MaxExponent - 1), SignificandBits), MaxSubnormal)
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -------------------------------------------------------------------------
    ! -----   parameters for high-precision decimal conversion algorithm  -----
    ! -------------------------------------------------------------------------
    ! 150 is an arbitrary number of digits, but should be large enough for any practical number.
    ! Important note: a number of digits large enough to represent the smallest subnormal
    ! for single-precision number is about 166 (= 54 + 112).
    tUInt32, PARAMETER  :: MAX_NUM_DIGITS = 150
    ! The maximum amount we can shift is the number of bits used in the Accumulator,
    ! minus the number of bits needed to represent the base (in this case 4).
    tUInt32, PARAMETER  :: MAX_SHIFT_AMOUNT = 4
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------------------
    ! -----   parameters for BigUInt of FastFloat algorithm  -----
    ! -----------------------------------------------------------
    ! the number of bits of 'Digit' of BigUInt.
    tSInt32, PARAMETER  :: DigitBits = 64
    ! the total number of bits of a BigUInt that needs to be at least the number of bits
    ! required to store the largest BigUInt, which is Log2(10**(MaxDigits + MaxExp10)), or
    ! Log2(10**(112 + 54))`, or ~551 bits, so we round to 576.
    tSInt32, PARAMETER  :: BigUIntBits = 576
    ! the (fixed) capacity of a BigUInt
    tSInt32, PARAMETER  :: BigCapacity = BigUIntBits / DigitBits   ! = 9
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    tUInt32, PARAMETER  :: DivBase      = 10
    tUInt32, PARAMETER  :: MaxDivbyBase = 429496729
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ number parameters +++
    tUInt32, PARAMETER  :: TwoUInt   = 2
    tUInt32, PARAMETER  :: ThreeUInt = 3
    tUInt32, PARAMETER  :: FourUInt  = 4
    tUInt32, PARAMETER  :: FiveUInt  = 5
    tUInt32, PARAMETER  :: TenUInt   = 10
    tUInt32, PARAMETER  :: FortyUInt = 40
    tUInt32, PARAMETER  :: HundredUInt     = 100
    tUInt32, PARAMETER  :: TenThousandUInt = 10000
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ----------------------------------------------------
    ! -----   Simple-Decimal-Algorithm' parameters   -----
    ! ----------------------------------------------------
    ! The nth item in Powers_Of_Two represents the greatest power of two less than
    ! 10^n. This tells us how much we can safely shift without overshooting.
    tUInt8,  PARAMETER  :: Powers_Of_Two(0:18) = [ &
            0, 3, 6, 9, 13, 16, 19, 23, 26, 29, 33, 36, 39, 43, 46, 49, 53, 56, 59]
    tSInt32, PARAMETER  :: Num_Powers_Of_Two = SIZE(Powers_Of_Two)                                  ! = 19
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! --------------------------------------------------
    ! -----   Eisel-Lemire-Algorithm' parameters   -----
    ! --------------------------------------------------
    tSInt32, PARAMETER  :: LowBits = TotalBits - SignificandBits - 3                                ! = 6
    ! The halfway constant is used to check if the bits that will be shifted away initially are all 1.
    tUInt32, PARAMETER  :: HalfWay = SHIFTL(1, LowBits) - 1                                         ! = 63
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! --------------------------------------------------
    ! -----   Clinger-Algorithm' parameters   -----
    ! --------------------------------------------------
    tSInt32             :: Idx
    tFloat,  PARAMETER  :: Powers_Of_Ten(0:10)  = [(10.0E0**Idx, Idx = 0, 10)]
    tSInt32, PARAMETER  :: Num_Exact_Pow10 = 10
    tSInt32, PARAMETER  :: Num_Mantissa_Digits = 7
    tFloat,  PARAMETER  :: Max_Exact_Integer = 16777215.0_kSingle
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    tSInt32, PARAMETER  :: Exponent_UppBound =  39      ! = 38 + 1
    tSInt32, PARAMETER  :: Exponent_LowBound = -54      ! = (-45) - 9
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Dragonbox-Algorithm' parameters   -----
    ! -----------------------------------------------
    ! parameters for main routine
    tSInt32, PARAMETER  :: Kappa = 1
    tSInt32, PARAMETER  :: Big_Divisor = 10**(Kappa+1)                  ! 100
    tSInt32, PARAMETER  :: Small_Divisor = Big_Divisor / 10             ! 10
    tSInt32, PARAMETER  :: Half_Small_Divisor = Small_Divisor / 2       ! 5
    tSInt32, PARAMETER  :: Divisibility_Check_By_5_Threshold = 39
    tSInt32, PARAMETER  :: Case_Fc_Pm_Half_Lower_Threshold = -1
    ! parameters for short interval case
    tSInt32, PARAMETER  :: Case_Shorter_Interval_Left_Endpoint_Lower_Threshold = 2
    tSInt32, PARAMETER  :: Case_Shorter_Interval_Left_Endpoint_Upper_Threshold = 3
    tSInt32, PARAMETER  :: Shorter_Interval_Tie_Lower_Threshold = -35
    tSInt32, PARAMETER  :: Shorter_Interval_Tie_Upper_Threshold = -35
    ! parameters for Is_Divisible_By_Pow10 routine
    tSInt32, PARAMETER  :: Info_Shift_Amount = 16
    tSInt32, PARAMETER  :: OneShiftL = SHIFTL(1, Info_Shift_Amount)
    tSInt32, PARAMETER  :: Comparison_Mask = OneShiftL - 1
    tSInt32, PARAMETER  :: Magic_Number = OneShiftL/Small_Divisor + 1
    ! parameters for Divide_By_10_To_Kappa_Plus_1
    tUInt64, PARAMETER  :: DivM = 1374389535_kInt64
    tSInt32, PARAMETER  :: DivS = 37
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Ryu-Algorithm' parameters   -----
    ! -----------------------------------------------
    tSInt32, PARAMETER  :: BitsPerPow5 = 64
    tSInt32, PARAMETER  :: MaxExp_ModInv5 = 13
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Schubfach-Algorithm' parameters   -----
    ! -----------------------------------------------
    tSInt32, PARAMETER  :: Pow10_Min_Exact_Exp = 0
    tSInt32, PARAMETER  :: Pow10_Max_Exact_Exp = 27
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   FastFloat-Algorithm' parameters   -----
    ! -----------------------------------------------
    ! Bias so we can get the real exponent with an invalid adjusted_mantissa
    tSInt32, PARAMETER  :: Invalid_AM_Bias = -ToInt32(Z'00008000')
    tSInt32, PARAMETER  :: Mantissa_Explicit_Bits     = SignificandBits
    tSInt32, PARAMETER  :: Minimum_Exponent           = -ExponentBias
    tSInt32, PARAMETER  :: Infinite_Power             = MaxExponent
    tSInt32, PARAMETER  :: Sign_Index                 = SignBits
    tSInt32, PARAMETER  :: MantTotalBits              = 64
    ! see section 6 in 'Number Parsing at a Gigabyte per Second' paper for
    ! how the following two numbers can be obtained
    tSInt32, PARAMETER  :: Max_Exponent_Round_To_Even = 10
    tSInt32, PARAMETER  :: Min_Exponent_Round_To_Even = -17
    tSInt32, PARAMETER  :: Largest_Power_of_Ten       = Exponent_UppBound - 1
    tSInt32, PARAMETER  :: Smallest_Power_of_Ten      = Exponent_LowBound + 1
    tSInt32, PARAMETER  :: Max_Digits                 = MaxDecimalConversionDigits + 2
    tUInt64, PARAMETER  :: OneMant                    = 1_kInt64
    tUInt64, PARAMETER  :: Max_Mantissa_Fast_Path     = SHIFTL(2_kInt64, Mantissa_Explicit_Bits)
    tUInt64, PARAMETER  :: Exponent_Mask              = ExponentMask
    tUInt64, PARAMETER  :: Mantissa_Mask              = SignificandMask
    tUInt64, PARAMETER  :: Hidden_Bit_Mask            = SigHidBitMask
    tUInt64, PARAMETER  :: MaxMant                    = MAX_U64
    tUInt64, PARAMETER  :: NotOneMant                 = NOT(1_kInt64)
    tUInt64, PARAMETER  :: NotSigHidBitMask           = NOT(SHIFTL(1_kInt64, SignificandBits))
    tUInt64, PARAMETER  :: Powers_of_Ten_Uint64(0:19) = &
        [0_kInt64, &
         10_kInt64, &
         100_kInt64, &
         1000_kInt64, &
         10000_kInt64, &
         100000_kInt64, &
         1000000_kInt64, &
         10000000_kInt64, &
         100000000_kInt64, &
         1000000000_kInt64, &
         10000000000_kInt64, &
         100000000000_kInt64, &
         1000000000000_kInt64, &
         10000000000000_kInt64, &
         100000000000000_kInt64, &
         1000000000000000_kInt64, &
         10000000000000000_kInt64, &
         100000000000000000_kInt64, &
         1000000000000000000_kInt64, &
         -8446744073709551616_kInt64]
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ------------------------------------------------
    ! -----   YY/Lemire-Algorithm's parameters   -----
    ! ------------------------------------------------
    tUInt64, PARAMETER  :: MaxU64         = MAX_U64
    tUInt32, PARAMETER  :: BitMask        = SHIFTL(1, LowBits) - 1          ! = Halfway
    tUInt32, PARAMETER  :: BitMaskMinus1  = BitMask - 1
    tUInt32, PARAMETER  :: AddRound       = SHIFTL(1, ExponentBits - 1)
    tUInt32, PARAMETER  :: MaxUInt        = ToInt32(Z'FFFFFFFF')
    tUInt32, PARAMETER  :: FpRawInf       = ToInt32(Z'7F800000')            ! = ExponentMask
    tSInt32, PARAMETER  :: MaxExpBin      = 128
    tSInt32, PARAMETER  :: MinExpBin      = -125
    tSInt32, PARAMETER  :: UIntSafeDigits = 9
    tSInt32, PARAMETER  :: MaxDecDigits   = MaxDecimalConversionDigits + 1
    tUInt32, PARAMETER  :: MaxMantissa    = SHIFTL(1_kInt64, BinaryPrecision)
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!** DERIVED TYPE DEFINITIONS
    ! ----------------------------------------------------------------------------
    ! -----   derived types for high-precision decimal conversion algorithm  -----
    ! ----------------------------------------------------------------------------
    TYPE HPDecimal
        tUInt32     :: NumDigits = 0
        tSInt32     :: DecimalPoint = 0
        tLogical    :: Truncated = FalseVal
        tUInt8      :: Digits(0:MAX_NUM_DIGITS-1)
    CONTAINS
        PROCEDURE   :: ShouldRoundUp        => HPDec_Should_Round_Up
        PROCEDURE   :: GetNumNewDigits      => HPDec_Get_Num_New_Digits
        PROCEDURE   :: TrimTrailingZeroes   => HPDec_Trim_Trailing_Zeroes
        PROCEDURE   :: RightShift           => HPDec_Right_Shift
        PROCEDURE   :: LeftShift            => HPDec_Left_Shift
        PROCEDURE   :: Construct            => HPDec_Construct
        PROCEDURE   :: Shift                => HPDec_Shift
        PROCEDURE   :: RoundToUIntType      => HPDec_Round_To_UInt
    END TYPE HPDecimal
    ! ----------------------------------------------------------------------------
    ! -----   derived types for FastFloat algorithm                          -----
    ! ----------------------------------------------------------------------------
    ! a multi-precision (fixed capacity) unsigned integer where its representation are:
    ! - Base is 2**64.
    ! - Magnitude as array in little endian order.
    ! - The 'Length' first 'Digit' count as the number.
    ! ----------------------------------------------------------------------------
    TYPE BigUInt
        tUInt64     :: Digit(0:BigCapacity-1)
        tSInt32     :: Length = 0               ! number of digit currently stored
    CONTAINS
        PROCEDURE   :: IsEmpty      => BigUInt_IsEmpty
        PROCEDURE   :: IsNonZero    => BigUInt_IsNonZero
        PROCEDURE   :: Push         => BigUInt_Push
        PROCEDURE   :: Extend       => BigUInt_Extend
        PROCEDURE   :: Normalize    => BigUInt_Normalize
        PROCEDURE   :: FromU64      => BigUInt_From_U64
        PROCEDURE   :: Hi64         => BigUInt_Get_Hi64
        PROCEDURE   :: Compare      => BigUInt_Compare
        PROCEDURE   :: ShiftL       => BigUInt_ShiftL
        PROCEDURE   :: LeadZ        => BigUInt_LeadZ
        PROCEDURE   :: BitLen       => BigUInt_BitLen
        PROCEDURE   :: SmallMul     => BigUInt_SmallMul
        PROCEDURE   :: LongMul      => BigUInt_LongMul
        PROCEDURE   :: Add          => BigUInt_Add
        PROCEDURE   :: Pow2         => BigUInt_Pow2
        PROCEDURE   :: Pow5         => BigUInt_Pow5
        PROCEDURE   :: Pow10        => BigUInt_Pow10
    END TYPE BigUInt
    ! parsed number information
    TYPE Parsed_Number_Info
        tSInt32     :: Exp              ! base-10 exponent
        tUInt32     :: Sig              ! base-10 significand
        tSInt32     :: IntegralStart    ! starting index of integral part of the significand
        tSInt32     :: IntegralEnd      ! ending index of integral part of the significand
        tSInt32     :: FractionStart    ! starting index of fractional part of the significand
        tSInt32     :: FractionEnd      ! ending index of fractional part of the significand
    END TYPE

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE/GENERIC DEFINITIONS:
    ABSTRACT INTERFACE
        SUBROUTINE CB_Round(E, M, Min)
            IMPORT
            tSInt32, INTENT(INOUT)  :: E
            tUInt64, INTENT(INOUT)  :: M
            tSInt32, INTENT(IN)     :: Min
        END SUBROUTINE
        FUNCTION CB_Round_Nearest(IsOdd, IsHalfway, IsAbove) RESULT(Flag)
            IMPORT
            tLogical, INTENT(IN)    :: IsOdd, IsHalfway, IsAbove
            tLogical                :: Flag
        END FUNCTION
    END INTERFACE

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!
!                           COMMON AND GENERIC ROUTINES
!
!------------------------------------------------------------------------------

! include common auxiliary routines
#include    "Includes/Common - Auxiliary.f90"

!------------------------------------------------------------------------------
!
!               HIGH-PRECISION DECIMAL (HPDECIMAL) ROUTINES
!
!------------------------------------------------------------------------------

! include type-bound routines for HPDecimal type
#include    "Includes/Generic - HPDecimal.f90"

!------------------------------------------------------------------------------
!
!            MULTI-PRECISION UNSIGNED INTEGER (BIGUINT) ROUTINES
!
!------------------------------------------------------------------------------

! include type-bound routines for BigUInt type and related routines
#include    "Includes/Generic - BigUInt.f90"

!------------------------------------------------------------------------------
!
!            PARSING FLOATING-POINT-NUMBER STRING ROUTINES
!
!------------------------------------------------------------------------------

! include routines for parsing floating-point-number string
#include    "Includes/Generic - Parse FP String.f90"

!------------------------------------------------------------------------------
!
!                       BINARY-TO-DECIMAL CONVERSION ROUTINES
!
!------------------------------------------------------------------------------

! include routines for binary-to-decimal algorithms
#include    "Includes/Generic - Binary To Decimal.f90"

!------------------------------------------------------------------------------
!
!                       DECIMAL-TO-BINARY CONVERSION ROUTINES
!
!------------------------------------------------------------------------------

! include routines for decimal-to-binary algorithms
#include    "Includes/Generic - Decimal To Binary.f90"

!------------------------------------------------------------------------------
!
!                           REAL32 AUXILIARY ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION DivByPow10(X, P) RESULT(Y)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute Y = X .UDIV. (10**P)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, INTENT(IN) :: X    ! X <= 10**10
    tSInt32, INTENT(IN) :: P    ! 1 <= P <= 8
    tUInt32             :: Y

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Parameters for division by power of 10 applicable for N <= 10 digits
    ! (i.e. used for division of the 'Significand')
    tUInt64, PARAMETER  :: MagicM(1:8) = [                        &
        ToInt64(Z'0000000333333334'), ToInt64(Z'000000028F5C28F6'), &
        ToInt64(Z'00000004189374BD'), ToInt64(Z'0000000346DC5D64'), &
        ToInt64(Z'000000029F16B11D'), ToInt64(Z'0000000431BDE82E'), &
        ToInt64(Z'000000035AFE5358'), ToInt64(Z'00000002AF31DC47')]
    tSInt32, PARAMETER  :: MagicS(1:8) = [37, 40, 44, 47, 50, 54, 57, 60]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Multiplier
    tSInt32     :: Shift

!** FLOW

    Multiplier = MagicM(P)
    Shift      = MagicS(P)
    Y = ToInt32(SHIFTR(ToUnsignedLong(X)*Multiplier, Shift))

    RETURN
            
END FUNCTION DivByPow10

!******************************************************************************

FUNCTION Format_RealSP(Fp, Ep, cStr, IsScientific) RESULT(sLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To format the decimal representation Fp*(10^Ep).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,            INTENT(IN)      :: Fp           !! significand in base 10
    tSInt32,            INTENT(IN)      :: Ep           !! exponent in base 10
    tCharStar,          INTENT(INOUT)   :: cStr         !! character string
    tLogical, OPTIONAL, INTENT(IN)      :: IsScientific
    !^ format flag. <br>
    !  - true  if to write the given number in scientific format. <br>
    !  - false if to write the given number in general format. <br>
    !  - default is false. <br>
    tSInt32                             :: sLen         !! length of string written

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! maximum number of significant digits (i.e. the decimal precision that guarantees
    !   an error-free write-read cycle.)
    tSInt32, PARAMETER  :: H = 9
    ! shift and multiplier parameters (i.e. magic number) for integer division
    tSInt32, PARAMETER  :: S98   = 57
    tSInt64, PARAMETER  :: M98   = 1441151881_kInt64
    tSInt32, PARAMETER  :: S178  = 20                         ! = 84-64
    tSInt64, PARAMETER  :: M178  = 193428131138340668_kInt64
    tSInt64, PARAMETER  :: DivE8 = 100000000_kInt64
    ! The first powers of 10. The last entry must be 10^H.
    tSInt32             :: I
    tSInt64, PARAMETER  :: Pow10(0:H) = [(10**I, I = 0, H)]
    ! Used for left-to-tight digit extraction.
    tSInt32, PARAMETER  :: MASK_28 = SHIFTL(1, 28) - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: F
    tSInt32     :: E, HF, LF
    tLogical    :: IsGeneral    ! true if to write the given number in general format 

!** FLOW
    
    ! check for special cases
    IF (Ep == ExceptionalExponent) THEN
        ! either NaN or Infinity
        IF (Fp /= ZeroUInt) THEN
            cStr(1:3) = 'NaN'
            sLen = 3
        ELSE
            cStr(1:8) = 'Infinity'
            sLen = 8
        END IF
        RETURN
    END IF
    
    IF (Fp == ZeroUInt) THEN
        ! zero
        cStr(1:3) = '0.0'
        sLen = 3
        RETURN
    END IF

    ! For details not discussed here see section 10 of [3].
    ! Determine sLen such that 10**(sLen-1) <= F < 10**sLen
    sLen = Floor_Log10_Pow2(32 - LEADZ(Fp))
    IF (Fp >= Pow10(sLen)) sLen = sLen + 1
    
    ! Let Fp and Ep be the original F and E, respectively.
    ! Transform F and E to ensure
    !    10**(H-1) <= F < 10**H
    !    Fp*10**Ep = F*10**(E-H) = 0.F*10**E
    F = Fp*Pow10(H - sLen)
    E = Ep + sLen
    
    ! ToChars perform digits extraction using integers,
    ! provided that the arguments are limited to 8 digits.
    ! Therefore, split the H = 9 digits of F into:
    !     HF = the most significant digit of F
    !     LF = the last 8, least significant digits of F
    !
    ! For N = 9, M = 8 the table in section 10 of [2] shows
    !     Floor(F/10**8) = Floor((1,441,151,881*F/2**57)
    !
    HF = ToInt32(SHIFTR(F*M98, S98))
    LF = ToInt32(F - DivE8*HF)
    
    ! set format flag
    IsGeneral = TrueVal
    IF (PRESENT(IsScientific)) IsGeneral = .NOT.IsScientific
    ! write output
    IF (IsGeneral) THEN
        IF ((0 < E).AND.(E <= 7)) THEN
            ! plain format without leading zeroes
            sLen = ToChar_Plain_Without_LZ(HF, LF, E, cStr)
        ELSEIF ((-3 < E).AND.(E <= 0)) THEN
            ! plain format with leading zeroes
            sLen = ToChar_Plain_With_LZ(HF, LF, E, cStr)
        ELSE
            ! scientific notation
            sLen = ToChar_Scientific(HF, LF, E, cStr)
        END IF
    ELSE
        ! scientific notation
        sLen = ToChar_Scientific(HF, LF, E, cStr)
    END IF

    RETURN
    
    CONTAINS

    FUNCTION ToChar_Plain_Without_LZ(H, L, E, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For 0 < E <= 7, plain format without leading zeroes.
        ! Left-to-right digits extraction:
        ! algorithm 1 in [7], with b = 10, k = 8, n = 28.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Y, T, I, Pos

    !** FLOW
    
        cStr(1:1) = Char1Digit(H)
        Pos = 2
        ! Algorithm 1 in [7] needs computation of floor((a + 1) 2^n / b^k) - 1
        ! with a < 10^8, b = 10, k = 8, n = 28.
        ! Noting that (a + 1) 2^n <= 10^8 2^28 < 10^17
        ! For n = 17, m = 8 the table in section 10 of [3] leads to:
        Y = ToInt32(SHIFTR(UMul128_Upper64(SHIFTL(ToInt64(L+1), 28), M178), S178)) - 1
        I = 1
        DO WHILE (I < E)
            T = 10*Y
            ! append digit
            cStr(Pos:Pos) = Char1Digit(SHIFTR(T, 28))
            Pos = Pos + 1
            Y = IAND(T, MASK_28)
            I = I + 1
        END DO
        ! append period
        cStr(Pos:Pos) = '.'
        Pos = Pos + 1
        DO WHILE (I <= 8)
            T = 10*Y
            ! append digit
            cStr(Pos:Pos) = Char1Digit(SHIFTR(T, 28))
            Pos = Pos + 1
            Y = IAND(T, MASK_28)
            I = I + 1
        END DO
        Pos = Pos - 1
        ! remove trailing zero(s)
        DO WHILE (cStr(Pos:Pos) == '0')
            Pos = Pos - 1
        END DO
        ! ... but do not remove the one directly to the right of '.'
        IF (cStr(Pos:Pos) == '.') Pos = Pos + 1
    
        ! set length
        sLen = Pos

        RETURN

    END FUNCTION ToChar_Plain_Without_LZ

    !******************************************************************************

    FUNCTION ToChar_Plain_With_LZ(H, L, E, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For -3 < E <= 0: plain format with leading zeroes.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32    :: Y, T, I, Pos

    !** FLOW
    
        ! fill the first 4 characters
        cStr(1:4) = '0.00'
        ! compute Pos
        Pos = 3 - E 
        ! append H
        cStr(Pos:Pos) = Char1Digit(H)
        Pos = Pos + 1
        ! append L
        Pos = Pos + Write_U32_8_Digits(L, cStr(Pos:)) - 1
        ! remove trailing zero(s)
        DO WHILE (cStr(Pos:Pos) == '0')
            Pos = Pos - 1
        END DO
        ! ... but do not remove the one directly to the right of '.'
        IF (cStr(Pos:Pos) == '.') Pos = Pos + 1
    
        ! set length
        sLen = Pos

        RETURN

    END FUNCTION ToChar_Plain_With_LZ

    !******************************************************************************

    FUNCTION ToChar_Scientific(H, L, E, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For E <= -3 or E > 7: computerized scientific notation.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Y, T, I, Pos

    !** FLOW
    
        ! append H
        cStr(1:1) = Char1Digit(H)
        ! append period
        cStr(2:2) = '.'
        Pos = 3
        ! append L
        Pos = Pos + Write_U32_8_Digits(L, cStr(Pos:)) - 1
        ! remove trailing zero(s)
        DO WHILE (cStr(Pos:Pos) == '0')
            Pos = Pos - 1
        END DO
        ! ... but do not remove the one directly to the right of '.'
        IF (cStr(Pos:Pos) == '.') Pos = Pos + 1
    
        ! append exponent
        Pos = Pos + 1
        cStr(Pos:Pos) = 'E'
        sLen = Pos + Write_I32_Exponent(E-1, cStr(Pos+1:))
    
        RETURN
    
    END FUNCTION ToChar_Scientific

    !******************************************************************************

    FUNCTION Write_U32_8_Digits(Number, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8 digits

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: sLen     ! length of string written

    !** SUBROUTINE PARAMETER DECLARATIONS:
        ! shift and multiplier parameters (i.e. magic number) for integer division
        tSInt32, PARAMETER  :: Shf78 = 40
        tSInt64, PARAMETER  :: Mul78 = 109951163_kInt64
        tSInt32, PARAMETER  :: Divisor = 10000

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: NxtNum, RemNum

    !** FLOW
    
        ! compute NxtNum = PosNum/10000
        NxtNum = ToInt32(SHIFTR(ToInt64(Number)*Mul78, Shf78))
    
        ! compute RemNum = MOD(PosNum, 10000)
        RemNum = Number - NxtNum*Divisor
    
        ! convert the remainder to a working string
        cStr(5:8) = Char4Digits(RemNum)
    
        ! convert the rest (NxtNum)
        cStr(1:4) = Char4Digits(NxtNum)
        
        sLen = 8
    
        RETURN

    END FUNCTION Write_U32_8_Digits

    !**************************************************************************

    FUNCTION Write_I32_Exponent(Exp, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write a signed integer in the range -45 to 38

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Exp      ! exponent number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: PosExp

    !** FLOW
        
        IF (Exp < 0) THEN
            cStr(1:1) = '-'
        ELSE
            cStr(1:1) = '+'
        END IF
        PosExp = ABS(Exp)
        IF (PosExp < 10) THEN
            ! 1 digit
            cStr(2:2) = Char1Digit(PosExp)
            sLen = 2
        ELSE
            ! 2 digits
            cStr(2:3) = Char2Digits(PosExp)
            sLen = 3
        END IF
    
        RETURN

    END FUNCTION Write_I32_Exponent

    !**************************************************************************

END FUNCTION Format_RealSP

!******************************************************************************

END MODULE MBase_BinDec32

!******************************************************************************
