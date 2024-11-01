#define     USE_FULL_TABLE_OF_POWERS_OF_TEN

MODULE MBase_BinDec128

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various binary-to-decimal and decimal-to-binary conversion routines for
!   128-bit floating-point numbers (i.e. quadruple-precision real numbers).  It provides these
!   routines as a building block for higher-level formatting/parsing procedures with high performance
!   that perform conversions between human-readable strings and floating-point (real) numbers. <br>
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
    PUBLIC :: Bin2Dec_DragonBox128
    PUBLIC :: Bin2Dec_Ryu128
    PUBLIC :: Bin2Dec_Schubfach128
    ! Decimal-To-Binary
    PUBLIC :: Dec2Bin_Clinger128
    PUBLIC :: Dec2Bin_FastFloat128
    PUBLIC :: Dec2Bin_Lemire128
    PUBLIC :: Dec2Bin_LibC128
    PUBLIC :: Dec2Bin_YY128
    ! auxiliary
    PUBLIC :: Format_Real128
    PUBLIC :: Parse_Fortran_String128
    PUBLIC :: Parse_FPlus_String128
    PUBLIC :: Parse_JSON_String128
    PUBLIC :: Handle_Invalid_String128

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#undef      tFloat
#undef      ToFloat
#define     tFloat_is_tQuad
! variable types
#define     tUInt128                        TYPE(UInt128)
#define     tFloat                          tRealQP
#define     tUIntType                       tUInt128
#define     BinRep                          BinRep128
! common parameters
#define     ZeroUInt                        ZeroU128
#define     OneUInt                         OneU128
#define     ZeroFloat                       0.0_kQuad
#define     OneFloat                        1.0_kQuad
! derived type (tUInt128) conversions
#define     ToUIntType(X)                   UInt128(X)
#define     ToFloat(X)                      ToR128(X)
! characteristics of IEEE-754 & related binary floating-point numbers
#define     BinaryPrecision                 Float128%BinPrecision
#define     TotalBits                       Float128%TotBits
#define     SignBits                        Float128%SgnBits
#define     SignificandBits                 Float128%SigBits
#define     ExponentBits                    Float128%ExpBits
#define     MaxExponent                     Float128%MaxExp
#define     ExponentBias                    Float128%ExpBias
#define     DecimalPrecision                Float128%DecPrecision
#define     DecimalRange                    Float128%DecRange
#define     MaxDecimalConversionDigits      Float128%MaxDecConvDigits
! masking parameters
#define     SigHidBitMask                   FpMask128%SigHidBit
#define     SignificandMask                 FpMask128%Significand
#define     SignMask                        FpMask128%Sign
#define     ExponentMask                    FpMask128%Exponent
#define     ExpMantMask                     FpMask128%ExpMant
#define     QuietNaNMask                    FpMask128%QuietNaN
! public procedures/types
#define     Bin2Dec_DragonBox               Bin2Dec_DragonBox128
#define     Bin2Dec_Ryu                     Bin2Dec_Ryu128
#define     Bin2Dec_Schubfach               Bin2Dec_Schubfach128
#define     Dec2Bin_Clinger                 Dec2Bin_Clinger128
#define     Dec2Bin_FastFloat               Dec2Bin_FastFloat128
#define     Dec2Bin_Lemire                  Dec2Bin_Lemire128
#define     Dec2Bin_LibC                    Dec2Bin_LibC128
#define     Dec2Bin_YY                      Dec2Bin_YY128
#define     Handle_Invalid_String           Handle_Invalid_String128
#define     Format_RealQP                   Format_Real128
#define     Parse_Fortran_String            Parse_Fortran_String128
#define     Parse_FPlus_String              Parse_FPlus_String128
#define     Parse_JSON_String               Parse_JSON_String128

!** MODULE PARAMETERS:
    ! -----------------------------------------------------------------
    ! -----     options for type of number to be parsed           -----
    ! -----------------------------------------------------------------
!    tSInt32, PARAMETER, PUBLIC  :: FortNum  = 1     ! strict Fortran number
!    tSInt32, PARAMETER, PUBLIC  :: FPlusNum = 2     ! relaxed Fortran number
!    tSInt32, PARAMETER, PUBLIC  :: JsonNum  = 3     ! JSON number
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ maximum and minimum (positive) parameters +++
    ! MinSubnormal = 1
    tUInt128, PARAMETER :: MinSubnormal = UInt128(0_kInt64, 1_kInt64)
    ! MaxSubnormal = SHIFTL(1, SignificandBits) - 1
    !              = UInt128(281474976710655_kInt64, -1_kInt64)
    tUInt128, PARAMETER :: MaxSubnormal = UInt128(SHIFTL(1_kInt64, SignificandBits-64)-1_kInt64, -1_kInt64)
    ! MinNormal = SHIFTL(1, SignificandBits) = MaxSubnormal + 1
    !           = UInt128(281474976710656_kInt64, 0_kInt64)
    tUInt128, PARAMETER :: MinNormal    = UInt128(SHIFTL(1_kInt64, SignificandBits-64), 0_kInt64)
    ! MaxNormal = IOR(SHIFTL((MaxExponent - 1), SignificandBits), MaxSubnormal)
    !           = UInt128(9223090561878065151_kInt64, -1_kInt64)
    tUInt128, PARAMETER :: MaxNormal    = UInt128(IOR(SHIFTL(MaxExponent - 1_kInt64, SignificandBits-64), &
                                                  MaxSubnormal%High), IOR(0_kInt64, MaxSubnormal%Low))
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -------------------------------------------------------------------------
    ! -----   parameters for high-precision decimal conversion algorithm  -----
    ! -------------------------------------------------------------------------
    ! 1600 is an arbitrary number of digits, but should be large enough for any practical number.
    ! Important note: a number of digits large enough to represent the smallest subnormal
    ! for quadruple-precision number is about 16564 (= 5001 + 11563).
    tUInt32,  PARAMETER :: MAX_NUM_DIGITS = 1600
    ! The maximum amount we can shift is the number of bits used in the Accumulator,
    ! minus the number of bits needed to represent the base (in this case 4).
    tUInt32,  PARAMETER :: MAX_SHIFT_AMOUNT = 4
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ------------------------------------------------------------
    ! -----   parameters for BigUInt of FastFloat algorithm  -----
    ! ------------------------------------------------------------
    ! the number of bits of 'Digit' of BigUInt.
    tSInt32,  PARAMETER :: DigitBits = 64
    ! the total number of bits of a BigUInt that needs to be at least the number of bits
    ! required to store the largest BigUInt, which is Log2(10**(MaxDigits + MaxExp10)), or
    ! Log2(10**(11563 + 5005))`, or ~55037 bits, so we round to 55040.
    tSInt32,  PARAMETER :: BigUIntBits = 55040
    ! the (fixed) capacity of a BigUInt
    tSInt32,  PARAMETER :: BigCapacity = BigUIntBits / DigitBits   ! = 860
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    tUInt128, PARAMETER :: DivBase      = UInt128(0_kInt64, 10_kInt64)
    tUInt128, PARAMETER :: MaxDivbyBase = UInt128(ToInt64(Z'1999999999999999'), ToInt64(Z'9999999999999999'))
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++ number parameters +++
    tUInt128, PARAMETER :: TwoUInt         = UInt128(0_kInt64, 2_kInt64)
    tUInt128, PARAMETER :: ThreeUInt       = UInt128(0_kInt64, 3_kInt64)
    tUInt128, PARAMETER :: FourUInt        = UInt128(0_kInt64, 4_kInt64)
    tUInt128, PARAMETER :: FiveUInt        = UInt128(0_kInt64, 5_kInt64)
    tUInt128, PARAMETER :: TenUInt         = UInt128(0_kInt64, 10_kInt64)
    tUInt128, PARAMETER :: FortyUInt       = UInt128(0_kInt64, 40_kInt64)
    tUInt128, PARAMETER :: HundredUInt     = UInt128(0_kInt64, 100_kInt64)
    tUInt128, PARAMETER :: TenThousandUInt = UInt128(0_kInt64, 10000_kInt64)
    tUInt128, PARAMETER :: Largest_Pow10   = UInt128(5421010862427522170_kInt64, 687399551400673280_kInt64)
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ----------------------------------------------------
    ! -----   Simple-Decimal-Algorithm' parameters   -----
    ! ----------------------------------------------------
    ! The nth item in Powers_Of_Two represents the greatest power of two less than
    ! 10^n. This tells us how much we can safely shift without overshooting.
    tUInt8,   PARAMETER :: Powers_Of_Two(0:18) = [ &
            0, 3, 6, 9, 13, 16, 19, 23, 26, 29, 33, 36, 39, 43, 46, 49, 53, 56, 59]
    tSInt32,  PARAMETER :: Num_Powers_Of_Two = SIZE(Powers_Of_Two)                              ! = 19
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! --------------------------------------------------
    ! -----   Eisel-Lemire-Algorithm' parameters   -----
    ! --------------------------------------------------
    tSInt32,  PARAMETER :: LowBits = TotalBits - SignificandBits - 3                            ! = 13
    ! The halfway constant is used to check if the bits that will be shifted away initially are all 1.
    tUInt128, PARAMETER :: HalfWay = UInt128(0_kInt64, SHIFTL(1_kInt64, LowBits) - 1_kInt64)    ! = 8191
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ---------------------------------------------
    ! -----   Clinger-Algorithm' parameters   -----
    ! ---------------------------------------------
    tSInt32             :: Idx
    tFloat,   PARAMETER :: Powers_Of_Ten(0:48)  = [(10.0_kQuad**Idx, Idx = 0, 48)]
    tSInt32,  PARAMETER :: Num_Exact_Pow10 = 48
    tSInt32,  PARAMETER :: Num_Mantissa_Digits = 33
    tFloat,   PARAMETER :: Max_Exact_Integer = 10384593717069655257060992658440191.0_kQuad
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    tSInt32,  PARAMETER :: Exponent_UppBound =  4933    ! = 4932 + 1
    tSInt32,  PARAMETER :: Exponent_LowBound = -5005    ! = (-4966) - 39
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Dragonbox-Algorithm' parameters   -----
    ! -----------------------------------------------
    ! parameters for main routine
    tSInt32,  PARAMETER :: Kappa = 3
    tSInt32,  PARAMETER :: Big_Divisor = 10**(Kappa+1)              ! 10000
    tSInt32,  PARAMETER :: Small_Divisor = Big_Divisor / 10         ! 1000
    tSInt32,  PARAMETER :: Half_Small_Divisor = Small_Divisor / 2   ! 500
    tSInt32,  PARAMETER :: Divisibility_Check_By_5_Threshold = 176
    tSInt32,  PARAMETER :: Case_Fc_Pm_Half_Lower_Threshold = -4
    ! parameters for short interval case
    tSInt32,  PARAMETER :: Case_Shorter_Interval_Left_Endpoint_Lower_Threshold = 2
    tSInt32,  PARAMETER :: Case_Shorter_Interval_Left_Endpoint_Upper_Threshold = 3
    tSInt32,  PARAMETER :: Shorter_Interval_Tie_Lower_Threshold = -163
    tSInt32,  PARAMETER :: Shorter_Interval_Tie_Upper_Threshold = -162
    ! parameters for Is_Divisible_By_Pow10 routine
    tSInt32,  PARAMETER :: Info_Shift_Amount = 26
    tSInt32,  PARAMETER :: OneShiftL = SHIFTL(1, Info_Shift_Amount)
    tSInt32,  PARAMETER :: Comparison_Mask = OneShiftL - 1
    tSInt32,  PARAMETER :: Magic_Number = OneShiftL/Small_Divisor + 1
    ! parameters for Divide_By_10_To_Kappa_Plus_1
    tUInt128, PARAMETER :: DivM = UInt128(ToInt64(Z'68DB8BAC710CB295'), ToInt64(Z'E9E1B089A0275255'))
    tSInt32,  PARAMETER :: DivS = 12    ! 140 - 128
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------
    ! -----   Ryu-Algorithm's parameters        -----
    ! -----------------------------------------------
    tSInt32,  PARAMETER :: BitsPerPow5 = 256
    tSInt32,  PARAMETER :: MaxExp_ModInv5 = 55
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ------------------------------------------------
    ! -----   Schubfach-Algorithm's parameters   -----
    ! ------------------------------------------------
    tSInt32,  PARAMETER :: Pow10_Min_Exact_Exp = 0
    tSInt32,  PARAMETER :: Pow10_Max_Exact_Exp = 110
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! ------------------------------------------------
    ! -----   FastFloat-Algorithm's parameters   -----
    ! ------------------------------------------------
    ! Bias so we can get the real exponent with an invalid adjusted_mantissa
    tSInt32,  PARAMETER :: Invalid_AM_Bias = -ToInt32(Z'00008000')
    tSInt32,  PARAMETER :: Mantissa_Explicit_Bits     = SignificandBits
    tSInt32,  PARAMETER :: Minimum_Exponent           = -ExponentBias
    tSInt32,  PARAMETER :: Infinite_Power             = MaxExponent
    tSInt32,  PARAMETER :: Sign_Index                 = SignBits
    tSInt32,  PARAMETER :: MantTotalBits              = 128
    ! see section 6 in 'Number Parsing at a Gigabyte per Second' paper for
    ! how the following two numbers can be obtained
    tSInt32,  PARAMETER :: Max_Exponent_Round_To_Even = 49
    tSInt32,  PARAMETER :: Min_Exponent_Round_To_Even = -6
    tSInt32,  PARAMETER :: Largest_Power_of_Ten       = Exponent_UppBound - 1
    tSInt32,  PARAMETER :: Smallest_Power_of_Ten      = Exponent_LowBound + 1
    tSInt32,  PARAMETER :: Max_Digits                 = MaxDecimalConversionDigits + 2
    tUInt128, PARAMETER :: OneMant                    = UInt128(0_kInt64, 1_kInt64)
    ! Max_Mantissa_Fast_Path = SHIFTL(2, Mantissa_Explicit_Bits)
    tUInt128, PARAMETER :: Max_Mantissa_Fast_Path     = UInt128(562949953421312_kInt64, 0_kInt64)
    ! Exponent_Mask   = ExponentMask    = UInt128(9223090561878065152_kInt64, 0_kInt64)
    tUInt128, PARAMETER :: Exponent_Mask              = UInt128(ExponentMask%High, ExponentMask%Low)
    ! Mantissa_Mask   = SignificandMask = UInt128(281474976710655_kInt64, -1_kInt64)
    tUInt128, PARAMETER :: Mantissa_Mask              = UInt128(SignificandMask%High, SignificandMask%Low)
    ! Hidden_Bit_Mask = SigHidBitMask   = UInt128(281474976710656_kInt64, 0_kInt64)
    tUInt128, PARAMETER :: Hidden_Bit_Mask            = UInt128(SigHidBitMask%High, SigHidBitMask%Low)
    ! MaxMant = UInt128(ToInt64(Z'FFFFFFFFFFFFFFFF'), ToInt64(Z'FFFFFFFFFFFFFFFF'))
    tUInt128, PARAMETER :: MaxMant                    = MaxU128
    ! NotOneMant = NOT(1) = UInt128(ToInt64(Z'FFFFFFFFFFFFFFFF'), ToInt64(Z'FFFFFFFFFFFFFFFE'))
    tUInt128, PARAMETER :: NotOneMant                 = UInt128(NOT(0_kInt64), NOT(1_kInt64))
    ! NotSigHidBitMask = NOT(SHIFTL(1, SignificandBits))
    !                  = UInt128(ToInt64(Z'FFFEFFFFFFFFFFFF'), ToInt64(Z'FFFFFFFFFFFFFFFF'))
    tUInt128, PARAMETER :: NotSigHidBitMask           = UInt128(NOT(SHIFTL(1_kInt64, SignificandBits-64)), &
                                                                NOT(0_kInt64))
    tUInt64,  PARAMETER :: Powers_of_Ten_Uint64(0:19) = &
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
    tUInt64,  PARAMETER :: MaxU64         = MAX_U64
    tUInt128, PARAMETER :: BitMask        = UInt128(0_kInt64, SHIFTL(1_kInt64, LowBits) - 1_kInt64) ! Halfway
    tUInt128, PARAMETER :: BitMaskMinus1  = UInt128(0_kInt64, SHIFTL(1_kInt64, LowBits) - 2_kInt64) ! BitMask - 1
    tUInt128, PARAMETER :: AddRound       = UInt128(0_kInt64, SHIFTL(1_kInt64, ExponentBits - 1))   ! 16384
    tUInt128, PARAMETER :: MaxUInt        = UInt128(MaxU64, MaxU64)
    tUInt128, PARAMETER :: FpRawInf       = UInt128(ToInt64(Z'7FFF000000000000'), 0_kInt64)         ! = ExponentMask
    tSInt32,  PARAMETER :: MaxExpBin      = 16384
    tSInt32,  PARAMETER :: MinExpBin      = -16381
    tSInt32,  PARAMETER :: UIntSafeDigits = 39
    tSInt32,  PARAMETER :: MaxDecDigits   = MaxDecimalConversionDigits + 1
    ! MaxMantissa = SHIFTL(1, BinaryPrecision) = UInt128(ToInt64(Z'0002000000000000'), 0_kInt64)
    tUInt128, PARAMETER :: MaxMantissa    = UInt128(SHIFTL(1_kInt64, BinaryPrecision-64), 0_kInt64)
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! -----------------------------------------------------
    ! -----   parameters for Format_RealQP functions   -----
    ! -----------------------------------------------------
    ! maximum number of significant digits (i.e. the decimal precision that guarantees
    !   an error-free write-read cycle.)
    tSInt32,  PARAMETER :: H = 36
    ! The first powers of 10. The last entry here is 10**(H-1).
    tUInt128, PARAMETER :: PowTen(0:35) = [                                     &
            UInt128(0_kInt64, 1_kInt64),      UInt128(0_kInt64, 10_kInt64),     &
            UInt128(0_kInt64, 10_kInt64**2),  UInt128(0_kInt64, 10_kInt64**3),  &
            UInt128(0_kInt64, 10_kInt64**4),  UInt128(0_kInt64, 10_kInt64**5),  &
            UInt128(0_kInt64, 10_kInt64**6),  UInt128(0_kInt64, 10_kInt64**7),  &
            UInt128(0_kInt64, 10_kInt64**8),  UInt128(0_kInt64, 10_kInt64**9),  &
            UInt128(0_kInt64, 10_kInt64**10), UInt128(0_kInt64, 10_kInt64**11), &
            UInt128(0_kInt64, 10_kInt64**12), UInt128(0_kInt64, 10_kInt64**13), &
            UInt128(0_kInt64, 10_kInt64**14), UInt128(0_kInt64, 10_kInt64**15), &
            UInt128(0_kInt64, 10_kInt64**16), UInt128(0_kInt64, 10_kInt64**17), &
            UInt128(0_kInt64, 10_kInt64**18),                                   &
            UInt128(0_kInt64, -8446744073709551616_kInt64),                     &
            UInt128(5_kInt64,  7766279631452241920_kInt64),                     &
            UInt128(54_kInt64, 3875820019684212736_kInt64),                     &
            UInt128(542_kInt64, 1864712049423024128_kInt64),                    &
            UInt128(5421_kInt64, 200376420520689664_kInt64),                    &
            UInt128(54210_kInt64, 2003764205206896640_kInt64),                  &
            UInt128(542101_kInt64, 1590897978359414784_kInt64),                 &
            UInt128(5421010_kInt64, -2537764290115403776_kInt64),               &
            UInt128(54210108_kInt64, -6930898827444486144_kInt64),              &
            UInt128(542101086_kInt64, 4477988020393345024_kInt64),              &
            UInt128(5421010862_kInt64, 7886392056514347008_kInt64),             &
            UInt128(54210108624_kInt64, 5076944270305263616_kInt64),            &
            UInt128(542101086242_kInt64, -4570789518076018688_kInt64),          &
            UInt128(5421010862427_kInt64, -8814407033341083648_kInt64),         &
            UInt128(54210108624275_kInt64, 4089650035136921600_kInt64),         &
            UInt128(542101086242752_kInt64, 4003012203950112768_kInt64),        &
            UInt128(5421010862427522_kInt64, 3136633892082024448_kInt64)]
    tUInt128, PARAMETER :: PowTen36 = UInt128(54210108624275221_kInt64, -5527149226598858752_kInt64)
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
        PROCEDURE   :: FromU128     => BigUInt_From_U128
        PROCEDURE   :: Hi128        => BigUInt_Get_Hi128
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
        tUInt128    :: Sig              ! base-10 significand
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
            tSInt32,  INTENT(INOUT) :: E
            tUInt128, INTENT(INOUT) :: M
            tSInt32,  INTENT(IN)    :: Min
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
!                           REAL128 AUXILIARY ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION DivByPow10(X, P) RESULT(Y)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute Y = X .UDIV. (10**P)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: X    ! X <= 10**40
    tSInt32,  INTENT(IN)    :: P    ! 1 <= P <= 10
    tUInt128                :: Y

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Parameters for division by power of 10 applicable for N <= 40 digits
    ! (i.e. used for division of the 'Significand')
    ! Note: elements in the row are in little-endian order
    ! (i.e. element 0 is the least significant byte and element 1 is the most one)
    tUInt64,  PARAMETER :: MagicM(0:2, 1:10) = RESHAPE([                                          &
        ToInt64(Z'3333333333333334'), ToInt64(Z'3333333333333333'), ToInt64(Z'0000000000000033'), &
        ToInt64(Z'5C28F5C28F5C28F6'), ToInt64(Z'F5C28F5C28F5C28F'), ToInt64(Z'0000000000000028'), &
        ToInt64(Z'16872B020C49BA5F'), ToInt64(Z'C49BA5E353F7CED9'), ToInt64(Z'0000000000000020'), &
        ToInt64(Z'F0D844D013A92A31'), ToInt64(Z'6DC5D63886594AF4'), ToInt64(Z'0000000000000034'), &
        ToInt64(Z'F3E0370CDC8754F4'), ToInt64(Z'F16B11C6D1E108C3'), ToInt64(Z'0000000000000029'), &
        ToInt64(Z'8FE69270B06C43F6'), ToInt64(Z'8DEF416BDB1A6D69'), ToInt64(Z'0000000000000021'), &
        ToInt64(Z'4CA41D811A46D324'), ToInt64(Z'AFE535795E90AF0F'), ToInt64(Z'0000000000000035'), &
        ToInt64(Z'70834ACDAE9F0F50'), ToInt64(Z'F31DC4611873BF3F'), ToInt64(Z'000000000000002A'), &
        ToInt64(Z'5A02A23E254C0C40'), ToInt64(Z'5C17D04DAD2965CC'), ToInt64(Z'0000000000000022'), &
        ToInt64(Z'5CD10396A21346CC'), ToInt64(Z'F9BFB3AF7B756FAD'), ToInt64(Z'0000000000000036')],[3, 10])
    tSInt32, PARAMETER :: MagicS(1:10) = [137, 140, 143, 147, 150, 153, 157, 160, 163, 167]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: MulProduct(0:4)
    tUInt64     :: Input(0:1)
    tUInt64     :: Multiplier(0:2)
    tSInt32     :: Shift

!** FLOW

    Input(0)   = X%Low
    Input(1)   = X%High
    Multiplier = MagicM(:,P)
    Shift      = MagicS(P)
    CALL Multiply_N_ShiftRight(Input, 2, Multiplier, 3, Shift, MulProduct)
    Y = UInt128(MulProduct(1), MulProduct(0))

    RETURN

END FUNCTION DivByPow10

!******************************************************************************

SUBROUTINE UMul256(X, Y, Hi, Lo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute multiplication of two 128-bit unsigned integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)   :: X, Y
    tUInt128, INTENT(OUT)  :: Hi, Lo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: X64(0:1), Y64(0:1)
    tUInt64     :: Z64(0:3)

!** FLOW

    ! get input
    X64(0) = X%Low
    X64(1) = X%High
    Y64(0) = Y%Low
    Y64(1) = Y%High

    ! perform multiplication
    CALL MultiplyBasic(X64, 2, Y64, 2, Z64)

    ! set output
    Hi = UInt128(Z64(3), Z64(2))
    Lo = UInt128(Z64(1), Z64(0))

    RETURN

END SUBROUTINE UMul256

!******************************************************************************

FUNCTION UMul256_Upper128(X, Y) RESULT(Z256Hi)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute multiplication of two 128-bit unsigned integers and
    ! return the upper 128 bits of the 256-bit unsigned result.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: X, Y
    tUInt128                :: Z256Hi

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: X64(0:1), Y64(0:1), Z64(0:3)

!** FLOW

    ! get input
    X64(0) = X%Low
    X64(1) = X%High
    Y64(0) = Y%Low
    Y64(1) = Y%High

    ! perform multiplication
    CALL MultiplyBasic(X64, 2, Y64, 2, Z64)

    ! set output
    Z256Hi = UInt128(Z64(3), Z64(2))

    RETURN

END FUNCTION UMul256_Upper128

!******************************************************************************

FUNCTION Format_RealQP(Fp, Ep, cStr, IsScientific) RESULT(sLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To format the decimal representation Fp*(10^Ep).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128,           INTENT(IN)      :: Fp           !! significand in base 10
    tSInt32,            INTENT(IN)      :: Ep           !! exponent in base 10
    tCharStar,          INTENT(INOUT)   :: cStr         !! character string
    tLogical, OPTIONAL, INTENT(IN)      :: IsScientific
    !^ format flag. <br>
    !  - true  if to write the given number in scientific format. <br>
    !  - false if to write the given number in general format. <br>
    !  - default is false. <br>
    tSInt32                             :: sLen         !! length of string written

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! shift and multiplier parameters (i.e. magic number) for integer division
    tSInt64, PARAMETER  :: TenPow9 = 1000000000_kInt64
    ! multiplier and shift for 18 digits and divisor of 10**9
    tSInt32, PARAMETER  :: S189  = 26                           ! 90 - 64
    tSInt64, PARAMETER  :: M189  = ToInt64(Z'112E0BE826D694B3')
    ! multiplier and shift for 17 digits and divisor of 10**8
    tSInt32, PARAMETER  :: S178  = 20                           ! 84-64
    tSInt64, PARAMETER  :: M178  = 193428131138340668_kInt64
    tSInt64, PARAMETER  :: DivE8 = 100000000_kInt64
    ! Used for left-to-tight digit extraction.
    tSInt32, PARAMETER  :: MASK_28 = SHIFTL(1, 28) - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128    :: F
    tUInt64     :: Hi, Lo
    tSInt32     :: E, HiHi, HiLo, LoHi, LoLo
    tLogical    :: IsGeneral        ! true if to write the given number in general format
    tLogical    :: IsPlainWOLZ      ! true if to write the number in plain format without leading zeroes

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

    ! set format flag
    IsGeneral = TrueVal
    IF (PRESENT(IsScientific)) IsGeneral = .NOT.IsScientific

    ! For details not discussed here see section 10 of [3].
    ! Determine sLen such that 10**(sLen-1) <= F < 10**sLen
    sLen = Floor_Log10_Pow2(128 - LEADZ(Fp))
    IF (Fp .UGE. PowTen(sLen)) sLen = sLen + 1

    ! Let Fp and Ep be the original F and E, respectively.
    ! Transform F and E to ensure
    !    10**(H-1) <= F < 10**H
    !    Fp*10**Ep = F*10**(E-H) = 0.F*10**E
    E = Ep + sLen
    IF ((0 < E).AND.(E <= 7).AND.(sLen < 36).AND.(IsGeneral)) THEN
        IsPlainWOLZ = TrueVal
    ELSE
        IsPlainWOLZ = FalseVal
    END IF
    IF (sLen > 0) THEN
        IF (IsPlainWOLZ) THEN
            ! 'ToChar_Plain_Without_LZ' only handles 35 digits
            F = Fp*PowTen(H - sLen - 1)
        ELSE
            ! The other two handle 36 digits
            F = Fp*PowTen(H - sLen)
        END IF
    ELSE
        ! Is this possible?  Have we handled this case already above?
        F = Fp*PowTen36
    END IF

    ! 'ToChar_...' routines perform digits extraction using 32-bit integers,
    ! provided that the arguments are limited to 9 digits.
    ! Therefore, split the H = 36 digits (or 35 digits if IsPlainWOLZ is true) of F into:
    !     HiHi = the most 9 (or 8 if IsPlainWOLZ is true) significant digit of F
    !     HiLo = the next 9 most significant digits of F
    !     LoHi = the next 9 most significant digits of F
    !     LoLo = the last 9 least significant digits of F
    CALL DivModBy10Pow18(F, Hi, Lo)
    HiHi = SHIFTR(UMul128_Upper64(Hi, M189), S189)      ! HiHi = Hi/TenPow9
    HiLo = Hi - HiHi*TenPow9                            ! HiLo = MOD(Hi, TenPow9)
    LoHi = SHIFTR(UMul128_Upper64(Lo, M189), S189)      ! HiHi = Hi/TenPow9
    LoLo = Lo - LoHi*TenPow9                            ! HiLo = MOD(Hi, TenPow9)

    ! write output
    IF (IsGeneral) THEN
        IF (IsPlainWOLZ) THEN
            ! plain format without leading zeroes
            sLen = ToChar_Plain_Without_LZ(HiHi, HiLo, LoHi, LoLo, E, cStr)
        ELSEIF ((-3 < E).AND.(E <= 0)) THEN
            ! plain format with leading zeroes
            sLen = ToChar_Plain_With_LZ(HiHi, HiLo, LoHi, LoLo, E, cStr)
        ELSE
            ! scientific notation
            sLen = ToChar_Scientific(HiHi, HiLo, LoHi, LoLo, E, cStr)
        END IF
    ELSE
        ! scientific notation
        sLen = ToChar_Scientific(HiHi, HiLo, LoHi, LoLo, E, cStr)
    END IF

    RETURN

    CONTAINS

    FUNCTION ToChar_Plain_Without_LZ(HH, HL, LH, LL, E, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For 0 < E <= 7, plain format without leading zeroes.
        ! Left-to-right digits extraction:
        ! algorithm 1 in [7], with b = 10, k = 8, n = 28.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: HH, HL, LH, LL   ! components of significand
        tSInt32,   INTENT(IN)       :: E                ! exponent
        tCharStar, INTENT(INOUT)    :: cStr             ! character string
        tSInt32                     :: sLen             ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Y, T, I, Pos

    !** FLOW

        Pos = 1
        ! Algorithm 1 in [7] needs computation of floor((a + 1) 2^n / b^k) - 1
        ! with a < 10^8, b = 10, k = 8, n = 28.
        ! Noting that (a + 1) 2^n <= 10^8 2^28 < 10^17
        ! For n = 17, m = 8 the table in section 10 of [3] leads to:
        Y = ToInt32(SHIFTR(UMul128_Upper64(SHIFTL(ToInt64(HH+1), 28), M178), S178)) - 1
        I = 0
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
        DO WHILE (I < 8)
            T = 10*Y
            ! append digit
            cStr(Pos:Pos) = Char1Digit(SHIFTR(T, 28))
            Pos = Pos + 1
            Y = IAND(T, MASK_28)
            I = I + 1
        END DO
        ! append HL
        Pos = Pos + Write_9_Digits(HL, cStr(Pos:))
        ! append LH and LL
        Pos = Pos + Write_18_Digits(LH, LL, cStr(Pos:)) - 1
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

    FUNCTION ToChar_Plain_With_LZ(HH, HL, LH, LL, E, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For -3 < E <= 0: plain format with leading zeroes.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: HH, HL, LH, LL   ! components of significand
        tSInt32,   INTENT(IN)       :: E                ! exponent
        tCharStar, INTENT(INOUT)    :: cStr             ! character string
        tSInt32                     :: sLen             ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Pos

    !** FLOW

        ! fill the first 4 characters
        cStr(1:4) = '0.00'
        ! compute Pos
        Pos = 3 - E
        ! append HH and HL
        Pos = Pos + Write_18_Digits(HH, HL, cStr(Pos:))
        ! append LH and LL
        Pos = Pos + Write_18_Digits(LH, LL, cStr(Pos:)) - 1
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

    FUNCTION ToChar_Scientific(HH, HL, LH, LL, E, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For E <= -3 or E > 7: computerized scientific notation.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: HH, HL, LH, LL   ! components of significand
        tSInt32,   INTENT(IN)       :: E                ! exponent
        tCharStar, INTENT(INOUT)    :: cStr             ! character string
        tSInt32                     :: sLen             ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Pos, HH_Hi, HH_Lo

    !** FLOW

        ! handle HH by splitting it into HH_Hi with 1 digit and HH_Lo with 8 digits
        HH_Hi = HH/100000000
        HH_Lo = HH - HH_Hi*100000000
        ! append HH_Hi
        cStr(1:1) = Char1Digit(HH_Hi)
        ! append period
        cStr(2:2) = '.'
        Pos = 3
        ! append HH_Lo and HL
        Pos = Pos + Write_17_Digits(HH_Lo, HL, cStr(Pos:))
        ! append LH and LL
        Pos = Pos + Write_18_Digits(LH, LL, cStr(Pos:)) - 1
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

    FUNCTION Write_18_Digits(Hi, Lo, cStr) RESULT(SLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write two integer numbers with a total length of 18 digits

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32,   INTENT(IN)   :: Hi, Lo
        tCharStar, INTENT(OUT)  :: cStr     ! character string
        tSInt32                 :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! NA

    !** FLOW

        sLen = Write_9_Digits(Hi, cStr(1:9)) + Write_9_Digits(Lo, cStr(10:18))

        RETURN

    END FUNCTION Write_18_Digits

    !******************************************************************************

    FUNCTION Write_17_Digits(Hi, Lo, cStr) RESULT(SLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write two integer numbers with a total length of 17 digits

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32,   INTENT(IN)   :: Hi, Lo
        tCharStar, INTENT(OUT)  :: cStr     ! character string
        tSInt32                 :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        sLen = Write_8_Digits(Hi, cStr(1:8)) + Write_9_Digits(Lo, cStr(9:17))

        RETURN

    END FUNCTION Write_17_Digits

    !******************************************************************************

    FUNCTION Write_9_Digits(Number, cStr) RESULT(SLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 9

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32,   INTENT(IN)   :: Number   ! number
        tCharStar, INTENT(OUT)  :: cStr     ! character string
        tSInt32                 :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt32     :: ABBCC, DDEE, BBCC, A

    !** FLOW

        ! ABBCC = Number/10000
        ABBCC = ToInt32(SHIFTR(ToInt64(Number)*ToInt64(Z'0000000068DB8BAD'), 44))
        ! DDEE  = MOD(Number, 10000)
        DDEE  = Number - ABBCC*10000
        ! A = ABBCC/10000
        A     = ToInt32(SHIFTR(ToInt64(ABBCC)*ToInt64(Z'000000000001A36F'), 30))
        ! BBCC  = MOD(ABBCC, 10000)
        BBCC  = ABBCC  - A*10000

        cStr(1:1) = Char1Digit(A)
        cStr(2:5) = Char4Digits(BBCC)
        cStr(6:9) = Char4Digits(DDEE)
        sLen = 9

        RETURN

    END FUNCTION Write_9_Digits

    !**************************************************************************

    FUNCTION Write_8_Digits(Number, cStr) RESULT(SLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32,   INTENT(IN)   :: Number   ! number
        tCharStar, INTENT(OUT)  :: cStr     ! character string
        tSInt32                 :: sLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt32     :: AABB, CCDD

    !** FLOW

        ! AABB = Number/10000
        AABB = ToInt32(SHIFTR(ToInt64(Number)*109951163_kInt64, 40))
        ! CCDD  = MOD(Number, 10000)
        CCDD  = Number - AABB*10000

        cStr(1:4) = Char4Digits(AABB)
        cStr(5:8) = Char4Digits(CCDD)
        sLen = 8

        RETURN

    END FUNCTION Write_8_Digits

    !**************************************************************************

    FUNCTION Write_I32_Exponent(Exp, cStr) RESULT(sLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write a signed integer in the range -4966 to 4932

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
        IF (PosExp < 1000) THEN
            IF (PosExp < 100) THEN
                IF (PosExp < 10) THEN
                    ! 1 digit
                    cStr(2:2) = Char1Digit(PosExp)
                    sLen = 2
                ELSE
                    ! 2 digits
                    cStr(2:3) = Char2Digits(PosExp)
                    sLen = 3
                END IF
            ELSE
                ! 3 digits
                cStr(2:4) = Char4Digits(PosExp)(2:4)
                sLen = 4
            END IF
        ELSE
            ! 4 digits
            cStr(2:5) = Char4Digits(PosExp)
            sLen = 5
        END IF

        RETURN

    END FUNCTION Write_I32_Exponent

    !**************************************************************************

    SUBROUTINE DivModBy10Pow18(Dividend, Quotient, Remainder)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform division Dividend / Divisor where the Divisor is equal to 10**18

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt128, INTENT(IN)    :: Dividend     ! the dividend
        tUInt64                 :: Quotient     ! the quotient
        tUInt64                 :: Remainder    ! the remainder

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32, PARAMETER  :: LSh   = 4                            ! = LEADZ(Divisor)
        tUInt64, PARAMETER  :: Denom = ToInt64(Z'DE0B6B3A76400000') ! = SHIFTL(Divisor, LSh)
        tUInt64, PARAMETER  :: V     = ToInt64(Z'2725DD1D243ABA0E') ! = Reciprocal_2By1(Denom)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32         :: RSh
        tUInt64         :: NumerHi, NumerLo, DenomHi, DenomLo
        tUInt64         :: NumerEx, RshMask, QuotHi
        tUInt64         :: R1, R2, LHS, RHS

    !** FLOW

        RSh = 64 - LSh
        RShMask = -1_kInt64
        NumerLo = SHIFTL(Dividend%Low, LSh)
        NumerHi = IOR(SHIFTL(Dividend%High, LSh), IAND(SHIFTR(Dividend%Low, RSh), RShMask))
        NumerEx = IAND(SHIFTR(Dividend%High, RSh), RShMask)

        CALL UDivRem_2By1(NumerEx, NumerHi, Denom, V, QuotHi, R1)
        CALL UDivRem_2By1(R1, NumerLo, Denom, V, Quotient, R2)
        Remainder = SHIFTR(R2, LSh)

        RETURN

    END SUBROUTINE DivModBy10Pow18

    !**************************************************************************

    SUBROUTINE UDivRem_2By1(UHi, ULo, D, V, Q, R)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform 128-bit unsigned integer division by 64-bit unsigned integer

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN)     :: UHi, ULo, D, V
        tUInt64, INTENT(OUT)    :: Q, R

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt64, PARAMETER  :: MinInt64 = ToInt64(Z'8000000000000000')   ! -9223372036854775808

   !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt64     :: QHi, QLo, NewLo

    !** FLOW

        ! Q128 = V*UHi
        CALL UMul128(V, UHi, QHi, QLo)

        ! Q128 = Q128 + U128
        NewLo = QLo + ULo
        IF (IEOR(NewLo, MinInt64) < IEOR(QLo, MinInt64)) THEN
            QHi = QHi + UHi + 1_kInt64
        ELSE
            QHi = QHi + UHi
        END IF
        QLo = NewLo

        QHi = QHi + 1_kInt64

        R = ULo - QHi*D

        IF (IEOR(R, MinInt64) > IEOR(QLo, MinInt64)) THEN
            QHi = QHi - 1_kInt64
            R = R + D
        END IF

        IF (IEOR(R, MinInt64) >= IEOR(D, MinInt64)) THEN
            QHi = QHi + 1_kInt64
            R = R - D
        END IF
        Q = QHi

        RETURN

    END SUBROUTINE UDivRem_2By1

    !**************************************************************************

END FUNCTION Format_RealQP

!******************************************************************************

END MODULE MBase_BinDec128

!******************************************************************************
