
MODULE MBase_FloatUtil

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various utility routines and parameters relating to floating point number.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_UIntUtil
    USE MBase_UInt128

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! parameters
    PUBLIC  :: Float32,  Float64,  Float128
    PUBLIC  :: FpMask32, FpMask64, FpMask128
    PUBLIC  :: ExceptionalExponent
    ! derived types
    PUBLIC  :: BinRep32, BinRep64, BinRep128
    ! procedures
    PUBLIC  :: RawFP_BiasedExponent
    PUBLIC  :: RawFP_UnbiasedExponent
    PUBLIC  :: RawFP_Significand
    PUBLIC  :: RawFP_Fraction
    PUBLIC  :: RawFP_IsZero
    PUBLIC  :: RawFP_IsNaN
    PUBLIC  :: RawFP_IsQuietNaN
    PUBLIC  :: RawFP_IsInfinite
    PUBLIC  :: RawFP_IsInfOrNaN
    PUBLIC  :: RawFP_IsMaximalFiniteMagnitude
    PUBLIC  :: RawFP_IsNegative
    PUBLIC  :: RawFP_Negate
    PUBLIC  :: RawFP_NeighborLow
    PUBLIC  :: RawFP_NeighborHigh
    PUBLIC  :: RawFP_Construct
    PUBLIC  :: RawFP_Decompose
    PUBLIC  :: RawFP_FromFloat
    PUBLIC  :: RawFP_ToFloat
    PUBLIC  :: RawFP_SetZero
    PUBLIC  :: RawFP_SetInfinity
    PUBLIC  :: RawFP_SetNaN

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#undef      tFloat
#undef      ToFloat

!** DERIVED TYPE DEFINITIONS
    ! ----------------------------------------------------------------------------
    ! Basic parameters relating to IEEE-754 floating-point number
    ! ----------------------------------------------------------------------------
    TYPE FloatInfo
        tSInt32 :: BinPrecision     !! binary precision
        tSInt32 :: TotBits          !! total bits
        tSInt32 :: SgnBits          !! sign bits
        tSInt32 :: SigBits          !! significand bits
        tSInt32 :: ExpBits          !! exponent bits
        tSInt32 :: MaxExp           !! maximum exponent
        tSInt32 :: ExpBias          !! exponent bias
        tSInt32 :: DecPrecision     !! decimal precision
        tSInt32 :: DecRange         !! decimal range
        tSInt32 :: MaxDecConvDigits !! maximum decimal conversion digits
    END TYPE FloatInfo
    ! ----------------------------------------------------------------------------
    ! Masking parameters for IEEE-754 floating-point number
    ! ----------------------------------------------------------------------------
    TYPE FP32Mask
        tUInt32 :: SigHidBit    !! significand hidden bit mask
        tUInt32 :: Significand  !! significand mask
        tUInt32 :: Sign         !! sign mask
        tUInt32 :: Exponent     !! exponent mask
        tUInt32 :: ExpMant      !! sum of exponent and significand masks
        tUInt32 :: QuietNaN     !! quiet NAN mask
    END TYPE
    TYPE FP64Mask
        tUInt64 :: SigHidBit    !! significand hidden bit mask
        tUInt64 :: Significand  !! significand mask
        tUInt64 :: Sign         !! sign mask
        tUInt64 :: Exponent     !! exponent mask
        tUInt64 :: ExpMant      !! sum of exponent and significand masks
        tUInt64 :: QuietNaN     !! quiet NAN mask
    END TYPE
    TYPE FP128Mask
        TYPE(UInt128)   :: SigHidBit    !! significand hidden bit mask
        TYPE(UInt128)   :: Significand  !! significand mask
        TYPE(UInt128)   :: Sign         !! sign mask
        TYPE(UInt128)   :: Exponent     !! exponent mask
        TYPE(UInt128)   :: ExpMant      !! sum of exponent and significand masks
        TYPE(UInt128)   :: QuietNaN     !! quiet NAN mask
    END TYPE
    ! ----------------------------------------------------------------------------
    ! binary floating-point representation in base 2
    ! --> ((-1)**S) * M * (2**E)
    ! ----------------------------------------------------------------------------
    !> *BinRep32* is a binary floating-point representation for 32-bit real number.
    TYPE BinRep32
        tUInt32         :: Significand  !! significand/mantissa (M)
        tUInt32         :: Exponent     !! exponent (E); negative value is invalid
        tLogical        :: Negative     !! negative sign flag; true if the value is negative
    END TYPE
    !> *BinRep64* is a binary floating-point representation for 64-bit real number.
    TYPE BinRep64
        tUInt64         :: Significand  !! significand/mantissa (M)
        tUInt32         :: Exponent     !! exponent (E); negative value is invalid
        tLogical        :: Negative     !! negative sign flag; true if the value is negative
    END TYPE
    !> *BinRep128* is a binary floating-point representation for 128-bit real number.
    TYPE BinRep128
        TYPE(UInt128)   :: Significand  !! significand/mantissa (M)
        tUInt32         :: Exponent     !! exponent (E); negative value is invalid
        tLogical        :: Negative     !! negative sign flag; true if the value is negative
    END TYPE

!** MODULE PARAMETERS:
    !+++++              Floating pointer number parameters                  +++++
    !% IEEE-754 information for 32-bit floating-point number
    TYPE(FloatInfo), PARAMETER  :: Float32   = FloatInfo(24, 32, 31, 23, 8, 255, 127, 6, 37, 112)
    !% IEEE-754 information for 64-bit floating-point number
    TYPE(FloatInfo), PARAMETER  :: Float64   = FloatInfo(53, 64, 63, 52, 11, 2047, 1023, 15, 307, 767)
    !% IEEE-754 information for 128-bit floating-point number
    TYPE(FloatInfo), PARAMETER  :: Float128  = FloatInfo(112, 128, 127, 112, 15, 32767, 16383, 33, 4931, 11563)
    !% masking information for 32-bit floating-point number
    TYPE(FP32Mask),  PARAMETER  :: FpMask32  = FP32Mask(8388608, 8388607, -2147483648, &
                                                        2139095040, 2147483647, 4194304)
    !% masking information for 64-bit floating-point number
    TYPE(FP64Mask),  PARAMETER  :: FpMask64  = FP64Mask(4503599627370496_kInt64,    &
                                                        4503599627370495_kInt64,    &
                                                       -9223372036854775808_kInt64, &
                                                        9218868437227405312_kInt64, &
                                                        9223372036854775807_kInt64, &
                                                        2251799813685248_kInt64)
    !% masking information for 128-bit floating-point number
    TYPE(FP128Mask), PARAMETER  :: FpMask128 = FP128Mask(UInt128(281474976710656_kInt64, 0_kInt64),      &
                                                         UInt128(281474976710655_kInt64, -1_kInt64),     &
                                                         UInt128(-9223372036854775808_kInt64, 0_kInt64), &
                                                         UInt128(9223090561878065152_kInt64, 0_kInt64),  &
                                                         UInt128(9223372036854775807_kInt64, -1_kInt64), &
                                                         UInt128(140737488355328_kInt64, 0_kInt64))
    tSInt32,         PARAMETER  :: ExceptionalExponent = ToInt32(Z'7FFFFFFF')

!** INTERFACE/GENERIC DEFINITIONS:
    INTERFACE RawFP_BiasedExponent
        !^ **Function Interface**: RawFP_BiasedExponent <br>
        !  **Purpose**:  To determine the biased exponent of the specified floating-point number. <br>
        !  **Usage**: <br>
        !   --->    BiasExp = RawFP_BiasedExponent(FPNum) <br>
        MODULE PROCEDURE RawFP32_BiasedExponent
        MODULE PROCEDURE RawFP64_BiasedExponent
        MODULE PROCEDURE RawFP128_BiasedExponent
    END INTERFACE
    INTERFACE RawFP_UnbiasedExponent
        !^ **Function Interface**: RawFP_UnbiasedExponent <br>
        !  **Purpose**:  To determine the unbiased exponent of the specified floating-point number. <br>
        !  **Usage**: <br>
        !   --->    UnbiasExp = RawFP_UnbiasedExponent(FPNum) <br>
        MODULE PROCEDURE RawFP32_UnbiasedExponent
        MODULE PROCEDURE RawFP64_UnbiasedExponent
        MODULE PROCEDURE RawFP128_UnbiasedExponent
    END INTERFACE
    INTERFACE RawFP_Significand
        !^ **Function Interface**: RawFP_Significand <br>
        !  **Purpose**:  To determine the significand of the specified floating-point number. <br>
        !  **Usage**: <br>
        !   --->    Significand = RawFP_Significand(FPNum) <br>
        MODULE PROCEDURE RawFP32_Significand
        MODULE PROCEDURE RawFP64_Significand
        MODULE PROCEDURE RawFP128_Significand
    END INTERFACE
    INTERFACE RawFP_Fraction
        !^ **Function Interface**: RawFP_Fraction <br>
        !  **Purpose**:  To determine the fractional part of the specified floating-point number. <br>
        !  **Usage**: <br>
        !   --->    Fraction = RawFP_Fraction(FPNum) <br>
        MODULE PROCEDURE RawFP32_Fraction
        MODULE PROCEDURE RawFP64_Fraction
        MODULE PROCEDURE RawFP128_Fraction
    END INTERFACE
    INTERFACE RawFP_IsZero
        !^ **Function Interface**: RawFP_IsZero <br>
        !  **Purpose**:  To check whether the specified floating-point (FP) number is zero or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = RawFP_IsZero(FPNum) <br>
        MODULE PROCEDURE RawFP32_IsZero
        MODULE PROCEDURE RawFP64_IsZero
        MODULE PROCEDURE RawFP128_IsZero
    END INTERFACE
    INTERFACE RawFP_IsNaN
        !^ **Function Interface**: RawFP_IsNaN <br>
        !  **Purpose**:  To check whether the specified floating-point (FP) number is a NAN (not a
        !                number) or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = RawFP_IsNaN(FPNum) <br>
        MODULE PROCEDURE RawFP32_IsNaN
        MODULE PROCEDURE RawFP64_IsNaN
        MODULE PROCEDURE RawFP128_IsNaN
    END INTERFACE
    INTERFACE RawFP_IsQuietNaN
        !^ **Function Interface**: RawFP_IsQuietNaN <br>
        !  **Purpose**:  To check whether the specified floating-point (FP) number is a quiet NAN
        !                or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = RawFP_IsQuietNaN(FPNum) <br>
        MODULE PROCEDURE RawFP32_IsQuietNaN
        MODULE PROCEDURE RawFP64_IsQuietNaN
        MODULE PROCEDURE RawFP128_IsQuietNaN
    END INTERFACE
    INTERFACE RawFP_IsInfinite
        !^ **Function Interface**: RawFP_IsInfinite <br>
        !  **Purpose**:  To check whether the specified floating-point (FP) number is infinite or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = RawFP_IsInfinite(FPNum) <br>
        MODULE PROCEDURE RawFP32_IsInfinite
        MODULE PROCEDURE RawFP64_IsInfinite
        MODULE PROCEDURE RawFP128_IsInfinite
    END INTERFACE
    INTERFACE RawFP_IsInfOrNaN
        !^ **Function Interface**: RawFP_IsInfOrNaN <br>
        !  **Purpose**:  To check whether the specified floating-point (FP) number is either infinite or NAN
        !                or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = RawFP_IsInfOrNaN(FPNum) <br>
        MODULE PROCEDURE RawFP32_IsInfOrNaN
        MODULE PROCEDURE RawFP64_IsInfOrNaN
        MODULE PROCEDURE RawFP128_IsInfOrNaN
    END INTERFACE
    INTERFACE RawFP_IsMaximalFiniteMagnitude
        !^ **Function Interface**: RawFP_IsMaximalFiniteMagnitude <br>
        !  **Purpose**:  To check whether the specified floating-point (FP) number is equal to its
        !                maximal finite magnitude. <br>
        !  **Usage**: <br>
        !   --->    Flag = RawFP_IsMaximalFiniteMagnitude(FPNum) <br>
        MODULE PROCEDURE RawFP32_IsMaximalFiniteMagnitude
        MODULE PROCEDURE RawFP64_IsMaximalFiniteMagnitude
        MODULE PROCEDURE RawFP128_IsMaximalFiniteMagnitude
    END INTERFACE
    INTERFACE RawFP_IsNegative
        !^ **Function Interface**: RawFP_IsNegative <br>
        !  **Purpose**:  To check whether the specified floating-point (FP) number has a negative value
        !                or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = RawFP_IsNegative(FPNum) <br>
        MODULE PROCEDURE RawFP32_IsNegative
        MODULE PROCEDURE RawFP64_IsNegative
        MODULE PROCEDURE RawFP128_IsNegative
    END INTERFACE
    INTERFACE RawFP_Negate
        !^ **Function Interface**: RawFP_Negate <br>
        !  **Purpose**:  To negate the specified floating-point (FP) number. <br>
        !  **Usage**: <br>
        !   --->    OutNum = RawFP_Negate(InNum) <br>
        MODULE PROCEDURE RawFP32_Negate
        MODULE PROCEDURE RawFP64_Negate
        MODULE PROCEDURE RawFP128_Negate
    END INTERFACE
    INTERFACE RawFP_NeighborLow
        !^ **Function Interface**: RawFP_NeighborLow <br>
        !  **Purpose**:  To determine the nearest floating point value that is smaller than the
        !                input value. <br>
        !  **Usage**: <br>
        !   --->    NearLo = RawFP_NeighborLow(InNum) <br>
        MODULE PROCEDURE RawFP32_NeighborLow
        MODULE PROCEDURE RawFP64_NeighborLow
        MODULE PROCEDURE RawFP128_NeighborLow
    END INTERFACE
    INTERFACE RawFP_NeighborHigh
        !^ **Function Interface**: RawFP_NeighborHigh <br>
        !  **Purpose**:  To determine the nearest floating point value that is greater than the
        !                input value. <br>
        !  **Usage**: <br>
        !   --->    NearHi = RawFP_NeighborHigh(InNum) <br>
        MODULE PROCEDURE RawFP32_NeighborHigh
        MODULE PROCEDURE RawFP64_NeighborHigh
        MODULE PROCEDURE RawFP128_NeighborHigh
    END INTERFACE
    INTERFACE RawFP_Construct
        !^ **Function Interface**: RawFP_Construct <br>
        !  **Purpose**:  To construct a raw binary floating point number based on its three parts. <br>
        !  **Usage**: <br>
        !   --->    RawFP = RawFP_Construct(FPBin) <br>
        MODULE PROCEDURE RawFP32_Construct
        MODULE PROCEDURE RawFP64_Construct
        MODULE PROCEDURE RawFP128_Construct
    END INTERFACE
    INTERFACE RawFP_Decompose
        !^ **Function Interface**: RawFP_Decompose <br>
        !  **Purpose**:  To decompose a raw binary floating point number into its three parts. <br>
        !  **Usage**: <br>
        !   --->    FPBin = RawFP_Decompose(RawFP) <br>
        MODULE PROCEDURE RawFP32_Decompose
        MODULE PROCEDURE RawFP64_Decompose
        MODULE PROCEDURE RawFP128_Decompose
    END INTERFACE
    INTERFACE RawFP_FromFloat
        !^ **Function Interface**: RawFP_FromFloat <br>
        !  **Purpose**:  To convert the specified real number to its equivalent unsigned integer number
        !                that represents a floating-pointer number. <br>
        !  **Usage**: <br>
        !   --->    RawFP = RawFP_FromFloat(RealNum) <br>
        MODULE PROCEDURE RawFP32_FromFloat
        MODULE PROCEDURE RawFP64_FromFloat
        MODULE PROCEDURE RawFP128_FromFloat
    END INTERFACE
    INTERFACE RawFP_ToFloat
        !^ **Function Interface**: RawFP_ToFloat <br>
        !  **Purpose**:  To convert an unsigned integer number that represents a floating-pointer number
        !                to its equivalent real number. <br>
        !  **Usage**: <br>
        !   --->    RealNum = RawFP_ToFloat(RawFP) <br>
        MODULE PROCEDURE RawFP32_ToFloat
        MODULE PROCEDURE RawFP64_ToFloat
        MODULE PROCEDURE RawFP128_ToFloat
    END INTERFACE
    INTERFACE RawFP_SetZero
        !^ **Function Interface**: RawFP_SetZero <br>
        !  **Purpose**:  To set value of the raw floating-point number to zero. <br>
        !  **Usage**: <br>
        !   --->    RawFP = RawFP_SetZero(IsNegative, RawFPMold) <br>
        MODULE PROCEDURE RawFP32_SetZero
        MODULE PROCEDURE RawFP64_SetZero
        MODULE PROCEDURE RawFP128_SetZero
    END INTERFACE
    INTERFACE RawFP_SetInfinity
        !^ **Function Interface**: RawFP_SetInfinity <br>
        !  **Purpose**:  To set value of the raw floating-point number to infinity. <br>
        !  **Usage**: <br>
        !   --->    RawFP = RawFP_SetInfinity(IsNegative, RawFPMold) <br>
        MODULE PROCEDURE RawFP32_SetInfinity
        MODULE PROCEDURE RawFP64_SetInfinity
        MODULE PROCEDURE RawFP128_SetInfinity
    END INTERFACE
    INTERFACE RawFP_SetNaN
        !^ **Function Interface**: RawFP_SetNaN <br>
        !  **Purpose**:  To set value of the raw floating-point number to not a number (NAN). <br>
        !  **Usage**: <br>
        !   --->    RawFP = RawFP_SetNaN(IsQuiet, RawFPMold) <br>
        MODULE PROCEDURE RawFP32_SetNaN
        MODULE PROCEDURE RawFP64_SetNaN
        MODULE PROCEDURE RawFP128_SetNaN
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++          macro definitions for 32-bit floating point number        +++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#define     tFloat_is_tSingle
! variable types
#define     tFloat                          tRealSP
#define     tUIntType                       tUInt32
#define     BinRep                          BinRep32
! common parameters
#define     ZeroUInt                        0_kInt32
#define     OneUInt                         1_kInt32
! type conversions
#define     To_I32(X)                       ToInt32(X)
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
! functions
#define     RawFP_BiasedExponent            RawFP32_BiasedExponent
#define     RawFP_UnbiasedExponent          RawFP32_UnbiasedExponent
#define     RawFP_Significand               RawFP32_Significand
#define     RawFP_Fraction                  RawFP32_Fraction
#define     RawFP_IsZero                    RawFP32_IsZero
#define     RawFP_IsNaN                     RawFP32_IsNaN
#define     RawFP_IsQuietNaN                RawFP32_IsQuietNaN
#define     RawFP_IsInfinite                RawFP32_IsInfinite
#define     RawFP_IsInfOrNaN                RawFP32_IsInfOrNaN
#define     RawFP_IsMaximalFiniteMagnitude  RawFP32_IsMaximalFiniteMagnitude
#define     RawFP_IsNegative                RawFP32_IsNegative
#define     RawFP_Negate                    RawFP32_Negate
#define     RawFP_NeighborLow               RawFP32_NeighborLow
#define     RawFP_NeighborHigh              RawFP32_NeighborHigh
#define     RawFP_Construct                 RawFP32_Construct
#define     RawFP_Decompose                 RawFP32_Decompose
#define     RawFP_FromFloat                 RawFP32_FromFloat
#define     RawFP_ToFloat                   RawFP32_ToFloat
#define     RawFP_SetZero                   RawFP32_SetZero
#define     RawFP_SetInfinity               RawFP32_SetInfinity
#define     RawFP_SetNaN                    RawFP32_SetNaN

! template for functions
#include    "Includes/RawBinaryFloatingPointNumber.f90"

! undefine macro definitions
#undef      tFloat_is_tSingle
#undef      tFloat
#undef      tUIntType
#undef      BinRep
#undef      ZeroUInt
#undef      OneUInt
#undef      To_I32
#undef      ToUIntType
#undef      ToFloat
#undef      BinaryPrecision
#undef      TotalBits
#undef      SignBits
#undef      SignificandBits
#undef      ExponentBits
#undef      MaxExponent
#undef      ExponentBias
#undef      DecimalPrecision
#undef      DecimalRange
#undef      MaxDecimalConversionDigits
#undef      SigHidBitMask
#undef      SignificandMask
#undef      SignMask
#undef      ExponentMask
#undef      ExpMantMask
#undef      QuietNaNMask
#undef      RawFP_BiasedExponent
#undef      RawFP_UnbiasedExponent
#undef      RawFP_Significand
#undef      RawFP_Fraction
#undef      RawFP_IsZero
#undef      RawFP_IsNaN
#undef      RawFP_IsQuietNaN
#undef      RawFP_IsInfinite
#undef      RawFP_IsInfOrNaN
#undef      RawFP_IsMaximalFiniteMagnitude
#undef      RawFP_IsNegative
#undef      RawFP_Negate
#undef      RawFP_NeighborLow
#undef      RawFP_NeighborHigh
#undef      RawFP_Construct
#undef      RawFP_Decompose
#undef      RawFP_FromFloat
#undef      RawFP_ToFloat
#undef      RawFP_SetZero
#undef      RawFP_SetInfinity
#undef      RawFP_SetNaN

!******************************************************************************

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++          macro definitions for 64-bit floating point number        +++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#define     tFloat_is_tDouble
! variable types
#define     tFloat                          tRealDP
#define     tUIntType                       tUInt64
#define     BinRep                          BinRep64
! common parameters
#define     ZeroUInt                        0_kInt64
#define     OneUInt                         1_kInt64
! type conversions
#define     To_I32(X)                       ToInt32(X)
#define     ToUIntType(X)                   ToUnsignedLong(X)
#define     ToFloat(X)                      REAL(X, KIND=kDouble)
! characteristics of IEEE-754 & related binary floating-point numbers
#define     BinaryPrecision                 Float64%BinPrecision
#define     TotalBits                       Float64%TotBits
#define     SignBits                        Float64%SgnBits
#define     SignificandBits                 Float64%SigBits
#define     ExponentBits                    Float64%ExpBits
#define     MaxExponent                     Float64%MaxExp
#define     ExponentBias                    Float64%ExpBias
#define     DecimalPrecision                Float64%DecPrecision
#define     DecimalRange                    Float64%DecRange
#define     MaxDecimalConversionDigits      Float64%MaxDecConvDigits
! masking parameters
#define     SigHidBitMask                   FpMask64%SigHidBit
#define     SignificandMask                 FpMask64%Significand
#define     SignMask                        FpMask64%Sign
#define     ExponentMask                    FpMask64%Exponent
#define     ExpMantMask                     FpMask64%ExpMant
#define     QuietNaNMask                    FpMask64%QuietNaN
! functions
#define     RawFP_BiasedExponent            RawFP64_BiasedExponent
#define     RawFP_UnbiasedExponent          RawFP64_UnbiasedExponent
#define     RawFP_Significand               RawFP64_Significand
#define     RawFP_Fraction                  RawFP64_Fraction
#define     RawFP_IsZero                    RawFP64_IsZero
#define     RawFP_IsNaN                     RawFP64_IsNaN
#define     RawFP_IsQuietNaN                RawFP64_IsQuietNaN
#define     RawFP_IsInfinite                RawFP64_IsInfinite
#define     RawFP_IsInfOrNaN                RawFP64_IsInfOrNaN
#define     RawFP_IsMaximalFiniteMagnitude  RawFP64_IsMaximalFiniteMagnitude
#define     RawFP_IsNegative                RawFP64_IsNegative
#define     RawFP_Negate                    RawFP64_Negate
#define     RawFP_NeighborLow               RawFP64_NeighborLow
#define     RawFP_NeighborHigh              RawFP64_NeighborHigh
#define     RawFP_Construct                 RawFP64_Construct
#define     RawFP_Decompose                 RawFP64_Decompose
#define     RawFP_FromFloat                 RawFP64_FromFloat
#define     RawFP_ToFloat                   RawFP64_ToFloat
#define     RawFP_SetZero                   RawFP64_SetZero
#define     RawFP_SetInfinity               RawFP64_SetInfinity
#define     RawFP_SetNaN                    RawFP64_SetNaN

! template for functions
#include    "Includes/RawBinaryFloatingPointNumber.f90"

! undefine macro definitions
#undef      tFloat_is_tDouble
#undef      tFloat
#undef      tUIntType
#undef      BinRep
#undef      ZeroUInt
#undef      OneUInt
#undef      To_I32
#undef      ToUIntType
#undef      ToFloat
#undef      BinaryPrecision
#undef      TotalBits
#undef      SignBits
#undef      SignificandBits
#undef      ExponentBits
#undef      MaxExponent
#undef      ExponentBias
#undef      DecimalPrecision
#undef      DecimalRange
#undef      MaxDecimalConversionDigits
#undef      SigHidBitMask
#undef      SignificandMask
#undef      SignMask
#undef      ExponentMask
#undef      ExpMantMask
#undef      QuietNaNMask
#undef      RawFP_BiasedExponent
#undef      RawFP_UnbiasedExponent
#undef      RawFP_Significand
#undef      RawFP_Fraction
#undef      RawFP_IsZero
#undef      RawFP_IsNaN
#undef      RawFP_IsQuietNaN
#undef      RawFP_IsInfinite
#undef      RawFP_IsInfOrNaN
#undef      RawFP_IsMaximalFiniteMagnitude
#undef      RawFP_IsNegative
#undef      RawFP_Negate
#undef      RawFP_NeighborLow
#undef      RawFP_NeighborHigh
#undef      RawFP_Construct
#undef      RawFP_Decompose
#undef      RawFP_FromFloat
#undef      RawFP_ToFloat
#undef      RawFP_SetZero
#undef      RawFP_SetInfinity
#undef      RawFP_SetNaN

!******************************************************************************

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++          macro definitions for 128-bit floating point number       +++++
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#define     tFloat_is_tQuad
! variable types
#define     tFloat                          tRealQP
#define     tUIntType                       TYPE(UInt128)
#define     BinRep                          BinRep128
! common parameters
#define     ZeroUInt                        ZeroU128
#define     OneUInt                         OneU128
! type conversions
#define     To_I32(X)                       ToI32(X)
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
! functions
#define     RawFP_BiasedExponent            RawFP128_BiasedExponent
#define     RawFP_UnbiasedExponent          RawFP128_UnbiasedExponent
#define     RawFP_Significand               RawFP128_Significand
#define     RawFP_Fraction                  RawFP128_Fraction
#define     RawFP_IsZero                    RawFP128_IsZero
#define     RawFP_IsNaN                     RawFP128_IsNaN
#define     RawFP_IsQuietNaN                RawFP128_IsQuietNaN
#define     RawFP_IsInfinite                RawFP128_IsInfinite
#define     RawFP_IsInfOrNaN                RawFP128_IsInfOrNaN
#define     RawFP_IsMaximalFiniteMagnitude  RawFP128_IsMaximalFiniteMagnitude
#define     RawFP_IsNegative                RawFP128_IsNegative
#define     RawFP_Negate                    RawFP128_Negate
#define     RawFP_NeighborLow               RawFP128_NeighborLow
#define     RawFP_NeighborHigh              RawFP128_NeighborHigh
#define     RawFP_Construct                 RawFP128_Construct
#define     RawFP_Decompose                 RawFP128_Decompose
#define     RawFP_FromFloat                 RawFP128_FromFloat
#define     RawFP_ToFloat                   RawFP128_ToFloat
#define     RawFP_SetZero                   RawFP128_SetZero
#define     RawFP_SetInfinity               RawFP128_SetInfinity
#define     RawFP_SetNaN                    RawFP128_SetNaN

! template for functions
#include    "Includes/RawBinaryFloatingPointNumber.f90"

! undefine macro definitions
#undef      tFloat_is_tQuad
#undef      tFloat
#undef      tUIntType
#undef      BinRep
#undef      ZeroUInt
#undef      OneUInt
#undef      ToI32
#undef      ToUIntType
#undef      ToFloat
#undef      BinaryPrecision
#undef      TotalBits
#undef      SignBits
#undef      SignificandBits
#undef      ExponentBits
#undef      MaxExponent
#undef      ExponentBias
#undef      DecimalPrecision
#undef      DecimalRange
#undef      MaxDecimalConversionDigits
#undef      SigHidBitMask
#undef      SignificandMask
#undef      SignMask
#undef      ExponentMask
#undef      ExpMantMask
#undef      QuietNaNMask
#undef      RawFP_BiasedExponent
#undef      RawFP_UnbiasedExponent
#undef      RawFP_Significand
#undef      RawFP_Fraction
#undef      RawFP_IsZero
#undef      RawFP_IsNaN
#undef      RawFP_IsQuietNaN
#undef      RawFP_IsInfinite
#undef      RawFP_IsInfOrNaN
#undef      RawFP_IsMaximalFiniteMagnitude
#undef      RawFP_IsNegative
#undef      RawFP_Negate
#undef      RawFP_NeighborLow
#undef      RawFP_NeighborHigh
#undef      RawFP_Construct
#undef      RawFP_Decompose
#undef      RawFP_FromFloat
#undef      RawFP_ToFloat
#undef      RawFP_SetZero
#undef      RawFP_SetInfinity
#undef      RawFP_SetNaN

!******************************************************************************

END MODULE MBase_FloatUtil

!******************************************************************************
