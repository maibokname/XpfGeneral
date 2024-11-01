
MODULE MBase_WriteUtil

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various utility routines relating to writing to character strings
!   (i.e. formatting a value).

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_LargeTables
    USE MBase_CharUtil
    USE MBase_FloatUtil
    USE MBase_UIntUtil
    USE MBase_UInt128
    USE MBase_BinDec32
    USE MBase_BinDec64
    USE MBase_BinDec128

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! procedures
    PUBLIC :: WriteNumber
    PUBLIC :: WriteFormat
    PUBLIC :: WriteUnsigned
    ! parameters
    PUBLIC :: B2D_DragonBox
    PUBLIC :: B2D_Ryu
    PUBLIC :: B2D_Schubfach

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! length for I/O message
    tSInt32, PARAMETER  :: MsgLen = 128
    ! minimum and maximum integer values
    tSInt32, PARAMETER  :: MIN_INT32 = ToInt32(Z'80000000')         ! -2147483648
    tSInt32, PARAMETER  :: MAX_INT32 = ToInt32(Z'7FFFFFF')          ! +2147483647
    tSInt64, PARAMETER  :: MIN_INT64 = ToInt64(Z'8000000000000000') ! -9223372036854775808
    tSInt64, PARAMETER  :: MAX_INT64 = ToInt64(Z'7FFFFFFFFFFFFFFF') ! +9223372036854775807
    ! Parameters for binary-to-decimal algorithm
    tSInt32, PARAMETER  :: B2D_DragonBox = 1    !! DragonBox algorithm
    tSInt32, PARAMETER  :: B2D_Ryu       = 2    !! Ryu algorithm
    tSInt32, PARAMETER  :: B2D_Schubfach = 3    !! Schubfach algorithm
    ! power-of-ten parameters
    TYPE(UInt128), PARAMETER :: PowTen(0:35) = [                                &
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
    TYPE(UInt128), PARAMETER :: PowTen36 = UInt128(54210108624275221_kInt64, -5527149226598858752_kInt64)

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE/GENERIC DEFINITIONS:
    INTERFACE WriteNumber
        !^ **Function Interface**: WriteNumber <br>
        !  **Purpose**:  To write a number as a decimal string. <br>
        !  **Usage**: <br>
        !   ! write any number and return the string buffer and its written length (using defaults) <br>
        !   --->    NumLen = WriteNumber(Number, StrBuf) <br>
        !   ! write a real or complex number and specifying optional input <br>
        !   --->    NumLen = WriteNumber(Number, StrBuf, B2DAlgo, IsScientific, MaxD, Fmt) <br>
        MODULE PROCEDURE Write_SInt8
        MODULE PROCEDURE Write_SInt16
        MODULE PROCEDURE Write_SInt32
        MODULE PROCEDURE Write_SInt64
        MODULE PROCEDURE Write_RealSP
        MODULE PROCEDURE Write_RealDP
        MODULE PROCEDURE Write_RealQP
        MODULE PROCEDURE Write_CmpxSP
        MODULE PROCEDURE Write_CmpxDP
        MODULE PROCEDURE Write_CmpxQP
    END INTERFACE
    INTERFACE WriteFormat
        !^ **Function Interface**: WriteFormat <br>
        !  **Purpose**:  To format a real or complex number. <br>
        !  **Usage**: <br>
        !   ! format a number and return the string buffer and its written length (using defaults) <br>
        !   --->    NumLen = WriteFormat(Number, StrBuf, Fmt) <br>
        !   ! format a number and specifying optional input <br>
        !   --->    NumLen = WriteFormat(Number, StrBuf, Fmt, B2DAlgo, Width, Digits) <br>
        MODULE PROCEDURE Write_RealSP_Format
        MODULE PROCEDURE Write_RealDP_Format
        MODULE PROCEDURE Write_RealQP_Format
        MODULE PROCEDURE Write_CmpxSP_Format
        MODULE PROCEDURE Write_CmpxDP_Format
        MODULE PROCEDURE Write_CmpxQP_Format
    END INTERFACE
    INTERFACE WriteUnsigned
        !^ **Function Interface**: WriteUnsigned <br>
        !  **Purpose**:  To write an integer number as a decimal string. <br>
        !  **Usage**: <br>
        !   --->    NumLen = WriteUnsigned(Number, StrBuf) <br>
        !  **Important Note**: Unlike the *WriteNumber* procedures, the *WriteUnsigned* procedures
        !           interpret the specified integer as an unsigned number.  <br>
        MODULE PROCEDURE Write_UInt8
        MODULE PROCEDURE Write_UInt16
        MODULE PROCEDURE Write_UInt32
        MODULE PROCEDURE Write_UInt64
    END INTERFACE
    INTERFACE IsSpecialCase
        MODULE PROCEDURE IsSpecialCase_RealSP
        MODULE PROCEDURE IsSpecialCase_RealDP
        MODULE PROCEDURE IsSpecialCase_RealQP
    END INTERFACE
    INTERFACE FP_Conversion
        MODULE PROCEDURE FP32_Conversion
        MODULE PROCEDURE FP64_Conversion
        MODULE PROCEDURE FP128_Conversion
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!                               PUBLIC PROCEDURES
!------------------------------------------------------------------------------

FUNCTION Write_SInt8(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a 8-bit integer number as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt8,      INTENT(IN)     :: Number   !! number
    tCharLen(4), INTENT(INOUT)  :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(11)    :: IntStr

!** FLOW

    NumLen = Write_SInt32(ToInt32(Number), IntStr)
    StrBuf(1:NumLen) = IntStr(1:NumLen)

    RETURN

END FUNCTION Write_SInt8

!******************************************************************************

FUNCTION Write_UInt8(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a 8-bit integer number interpreted as an unsigned integer
    !  as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8,      INTENT(IN)     :: Number   !! number
    tCharLen(3), INTENT(INOUT)  :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(11)    :: IntStr

!** FLOW

    NumLen = Write_SInt32(ToUnsignedInteger(Number), IntStr)
    StrBuf(1:NumLen) = IntStr(1:NumLen)

    RETURN

END FUNCTION Write_UInt8

!******************************************************************************

FUNCTION Write_SInt16(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a 16-bit integer number as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt16,     INTENT(IN)     :: Number   !! number
    tCharLen(6), INTENT(INOUT)  :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(11)    :: IntStr

!** FLOW

    NumLen = Write_SInt32(ToInt32(Number), IntStr)
    StrBuf(1:NumLen) = IntStr(1:NumLen)

    RETURN

END FUNCTION Write_SInt16

!******************************************************************************

FUNCTION Write_UInt16(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a 16-bit integer number interpreted as an unsigned integer
    !  as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt16,     INTENT(IN)     :: Number   !! number
    tCharLen(5), INTENT(INOUT)  :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(11)    :: IntStr

!** FLOW

    NumLen = Write_SInt32(ToUnsignedInteger(Number), IntStr)
    StrBuf(1:NumLen) = IntStr(1:NumLen)

    RETURN

END FUNCTION Write_UInt16

!******************************************************************************

FUNCTION Write_SInt32(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a 32-bit integer number as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,      INTENT(IN)    :: Number   !! number
    tCharLen(11), INTENT(INOUT) :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Number >= 0_kInt32) THEN
        NumLen = Write_AbsI32(Number, StrBuf)
    ELSE
        IF (Number /= MIN_INT32) THEN
            StrBuf(1:1) = '-'
            NumLen = Write_AbsI32((-Number), StrBuf(2:)) + 1
        ELSE
            StrBuf(1:11) = '-2147483648'
            NumLen = 11
        END IF
    END IF

    RETURN

END FUNCTION Write_SInt32

!******************************************************************************

FUNCTION Write_UInt32(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a 32-bit integer number interpreted as an unsigned integer
    !  as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32,      INTENT(IN)    :: Number   !! number
    tCharLen(10), INTENT(INOUT) :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt64, PARAMETER  :: MaskU32 = ToInt64(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Number >= 0_kInt32) THEN
        NumLen = Write_AbsI32(Number, StrBuf)
    ELSE
        BLOCK
            tUInt64     :: LongVal, Quotient, Remainder
            ! convert to 64-bit integer value as unsigned number
            LongVal   = IAND(ToInt64(Number), MaskU32)
            ! divide by 10
            Quotient  = SHIFTR(LongVal, 1) / 5_kInt64
            Remainder = LongVal - Quotient * 10_kInt64
            NumLen = Write_AbsI32(ToInt32(Quotient), StrBuf)
            NumLen = NumLen + Write_AbsI32(ToInt32(Remainder), StrBuf(NumLen+1:))
        END BLOCK
    END IF

    RETURN

END FUNCTION Write_UInt32

!******************************************************************************

FUNCTION Write_SInt64(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a 64-bit integer number as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64,      INTENT(IN)    :: Number   !! number
    tCharLen(20), INTENT(INOUT) :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Number >= 0_kInt64) THEN
        NumLen = Write_AbsI64(Number, StrBuf)
    ELSE
        IF (Number /= MIN_INT64) THEN
            StrBuf(1:1) = '-'
            NumLen = Write_AbsI64((-Number), StrBuf(2:)) + 1
        ELSE
            StrBuf(1:20) = '-9223372036854775808'
            NumLen = 20
        END IF
    END IF

    RETURN

END FUNCTION Write_SInt64

!******************************************************************************

FUNCTION Write_UInt64(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a 64-bit integer number interpreted as an unsigned integer
    !  as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,      INTENT(IN)    :: Number   !! number
    tCharLen(20), INTENT(INOUT) :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt64, PARAMETER  :: MaskU64 = ToInt64(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Number >= 0_kInt64) THEN
        NumLen = Write_AbsI64(Number, StrBuf)
    ELSE
        BLOCK
            tUInt64     :: Quotient, Remainder
            ! divide by 10
            Quotient  = SHIFTR(Number, 1) / 5_kInt64
            Remainder = Number - Quotient * 10_kInt64
            NumLen = Write_AbsI64(Quotient, StrBuf)
            NumLen = NumLen + Write_AbsI64(Remainder, StrBuf(NumLen+1:))
        END BLOCK
    END IF

    RETURN

END FUNCTION Write_UInt64

!******************************************************************************

FUNCTION Write_RealSP(Number, StrBuf, B2DAlgo, IsScientific, MaxD, Fmt) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To write a single-precision (32-bit) real value as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealSP,             INTENT(IN)     :: Number       !! number
    tCharLen(20),        INTENT(INOUT)  :: StrBuf       !! string buffer
    tSInt32,   OPTIONAL, INTENT(IN)     :: B2DAlgo
    !^ flag for binary-to-decimal algorithm <br>
    ! - B2D_DragonBox (or 1) if using the DragonBox algorithm. <br>
    ! - B2D_Ryu (or 2) if using the Ryu algorithm. <br>
    ! - B2D_Schubfach (or 3) if using the Schubfach algorithm. <br>
    ! - Default is B2D_DragonBox.
    tLogical,  OPTIONAL, INTENT(IN)     :: IsScientific
    !^ format flag <br>
    ! - True  if to write the given number in scientific format. <br>
    ! - False if to write the given number in general format. <br>
    ! - Default is false.
    tSInt32,   OPTIONAL, INTENT(IN)     :: MaxD
    !^ (maximum) number of digits to be written after the decimal point. <br>
    tCharStar, OPTIONAL, INTENT(IN)     :: Fmt
    !^ format to be used in the WRITE statement for the specified number. <br>
    !  If present, the procedure does not check its validity and will try to
    !  perform an internal write of the specified number based on the specified
    !  format while ignoring all other optional input.  If success, the procedure
    !  return immediately.  Otherwise, the procedure will check other optional
    !  input and write the number accordingly.
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt32, PARAMETER  :: ZeroUInt         = 0_kInt32
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

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32         :: RawBin       ! raw IEEE binary floating point representation
    tUInt32         :: SigRaw       ! raw (biased) significand in base 2
    tUInt32         :: ExpRaw       ! raw (biased) exponent in base 2
    tUInt32         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt32         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: ConvFlag     ! conversion flag (true if bin2dec conversion is needed)
    tFloat          :: FloatVal     ! working real (for conversion to binary representation)
    EQUIVALENCE(RawBin, FloatVal)
    tSInt32         :: Pos
    tSInt32         :: B2DAlgorithm
    tLogical        :: KeepTrailZ

!** FLOW

    ! check Fmt argument
    IF (PRESENT(Fmt)) THEN
        BLOCK
            tSInt32             :: IOStat   ! I/O status for error checking
            tCharLen(MsgLen)    :: IOMsg    ! I/O message for error checking
            tCharLen(20)        :: Buffer
            WRITE (Buffer, FMT=Fmt, IOSTAT=IOStat, IOMSG=IOMsg) Number
            IF (IOStat == 0) THEN
                StrBuf(1:) = TRIM(ADJUSTL(Buffer))
                NumLen = LEN_TRIM(StrBuf)
                RETURN
            END IF
        END BLOCK
    END IF

    ! set algorithm flag
    B2DAlgorithm = B2D_DragonBox
    IF (PRESENT(B2DAlgo)) THEN
        IF ((B2DAlgo > 0).AND.(B2DAlgo < 4)) B2DAlgorithm = B2DAlgo
    END IF

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion of real value to its binary representation  +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! get raw IEEE binary floating point representation (little-endian order)
    FloatVal = Number

    ! decompose the representation into its parts
    Negative = IAND(RawBin, SignMask) /= ZeroUInt
    SigRaw   = IAND(RawBin, SignificandMask)
    ExpRaw   = ToInt32(SHIFTR(IAND(RawBin, ExponentMask), SignificandBits))

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from binary to decimal representation +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ConvFlag = TrueVal
    ! check for special cases
    IF ((ExpRaw == 0).AND.(SigRaw == ZeroUInt)) THEN
        ! zero
        SigDec = ZeroUInt
        ExpDec = 0
        ConvFlag = FalseVal
    ELSEIF (ExpRaw == MaxExponent) THEN
        ! NaN or Infinity
        SigDec = SigRaw
        ExpDec = ExceptionalExponent
        ConvFlag = FalseVal
    END IF

    ! get exponent and mantissa
    IF (ExpRaw /= 0) THEN
        ! normal number
        SigBin = IOR(SigRaw, SigHidBitMask)
        ExpBin = ExpRaw - ExponentBias - SignificandBits
        IF ((-SignificandBits <= ExpBin).AND.(ExpBin <= 0)) THEN
            IF (TRAILZ(SigBin) >= -ExpBin)THEN
                ! fast path for small integer number (without fraction?)
                SigDec = SHIFTR(SigBin, -ExpBin)
                ExpDec = 0
                ConvFlag = FalseVal
            END IF
        END IF
    ELSE
        ! subnormal number
        SigBin = SigRaw
        ExpBin = 1 - ExponentBias - SignificandBits
    END IF

    IF (ConvFlag) THEN
        ! perform binary-to-decimal conversion
        SELECT CASE (B2DAlgorithm)
        CASE (B2D_DragonBox)
            CALL Bin2Dec_DragonBox32(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            KeepTrailZ = FalseVal
        CASE (B2D_Ryu)
            CALL Bin2Dec_Ryu32(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            KeepTrailZ = TrueVal
        CASE (B2D_Schubfach)
            CALL Bin2Dec_Schubfach32(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            KeepTrailZ = FalseVal
        END SELECT
    END IF

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from decimal representation to decimal string  +++++
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! write output
    IF (Negative) THEN
        StrBuf(1:1) = '-'
        Pos = 1
    ELSE
        Pos = 0
    END IF
    IF (.NOT.PRESENT(MaxD)) THEN
        NumLen = Pos + Write_RealSP_Shortest(SigDec, ExpDec, StrBuf(Pos+1:), IsScientific, KeepTrailZ)
    ELSE
        BLOCK
            tSInt32     :: DEPos(2), wLen, CutPos, I, J
            wLen = Write_RealSP_Shortest(SigDec, ExpDec, StrBuf(Pos+1:), IsScientific, KeepTrailZ, DEPos)
            IF (MaxD > (DEPos(2)-DEPos(1))) THEN
                NumLen = Pos + wLen
            ELSE
                CutPos = Pos + DEPos(1) + MaxD
                IF (StrBuf(CutPos+1:CutPos+1) >= '5') THEN
                    ! add 1
                    StrBuf(CutPos:CutPos) = ACHAR(IACHAR(StrBuf(CutPos:CutPos))+1)
                END IF
                ! need to remove some digits
                IF (DEPos(2) == wLen) THEN
                    ! plain format: do not have exponent parts
                    NumLen = CutPos
                ELSE
                    ! E format: need to move exponent parts up
                    I = DEPos(1)
                    DO J = Pos+DEPos(2)+1, Pos+wLen
                        I = I + 1
                        StrBuf(I:I) = StrBuf(J:J)
                    END DO
                    NumLen = I
                END IF
            END IF
        END BLOCK
    END IF

    RETURN

! undefine macro definitions
#undef  BinaryPrecision
#undef  TotalBits
#undef  SignBits
#undef  SignificandBits
#undef  ExponentBits
#undef  MaxExponent
#undef  ExponentBias
#undef  DecimalPrecision
#undef  DecimalRange
#undef  MaxDecimalConversionDigits
#undef  SigHidBitMask
#undef  SignificandMask
#undef  SignMask
#undef  ExponentMask
#undef  ExpMantMask
#undef  QuietNaNMask

END FUNCTION Write_RealSP

!******************************************************************************

FUNCTION Write_RealDP(Number, StrBuf, B2DAlgo, IsScientific, MaxD, Fmt) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To write a double-precision (64-bit) real value as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealDP,             INTENT(IN)     :: Number       !! number
    tCharLen(30),        INTENT(INOUT)  :: StrBuf       !! string buffer
    tSInt32,   OPTIONAL, INTENT(IN)     :: B2DAlgo
    !^ flag for binary-to-decimal algorithm <br>
    ! - B2D_DragonBox (or 1) if using the DragonBox algorithm. <br>
    ! - B2D_Ryu (or 2) if using the Ryu algorithm. <br>
    ! - B2D_Schubfach (or 3) if using the Schubfach algorithm. <br>
    ! - Default is B2D_DragonBox.
    tLogical,  OPTIONAL, INTENT(IN)     :: IsScientific
    !^ format flag <br>
    ! - True  if to write the given number in scientific format. <br>
    ! - False if to write the given number in general format. <br>
    ! - Default is false.
    tSInt32,   OPTIONAL, INTENT(IN)     :: MaxD
    !^ (maximum) number of digits to be written after the decimal point. <br>
    tCharStar, OPTIONAL, INTENT(IN)     :: Fmt
    !^ format to be used in the WRITE statement for the specified number. <br>
    !  If present, the procedure does not check its validity and will try to
    !  perform an internal write of the specified number based on the specified
    !  format while ignoring all other optional input.  If success, the procedure
    !  return immediately.  Otherwise, the procedure will check other optional
    !  input and write the number accordingly.
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt64, PARAMETER  :: ZeroUInt         = 0_kInt64
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

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: RawBin       ! raw IEEE binary floating point representation
    tUInt64         :: SigRaw       ! raw (biased) significand in base 2
    tUInt32         :: ExpRaw       ! raw (biased) exponent in base 2
    tUInt64         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt64         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: ConvFlag     ! conversion flag (true if bin2dec conversion is needed)
    tFloat          :: FloatVal     ! working real (for conversion to binary representation)
    EQUIVALENCE(RawBin, FloatVal)
    tSInt32         :: Pos
    tSInt32         :: B2DAlgorithm
    tLogical        :: KeepTrailZ

!** FLOW

    ! check Fmt argument
    IF (PRESENT(Fmt)) THEN
        BLOCK
            tSInt32             :: IOStat   ! I/O status for error checking
            tCharLen(MsgLen)    :: IOMsg    ! I/O message for error checking
            tCharLen(30)        :: Buffer
            WRITE (Buffer, FMT=Fmt, IOSTAT=IOStat, IOMSG=IOMsg) Number
            IF (IOStat == 0) THEN
                StrBuf(1:) = TRIM(ADJUSTL(Buffer))
                NumLen = LEN_TRIM(StrBuf)
                RETURN
            END IF
        END BLOCK
    END IF

    ! set algorithm flag
    B2DAlgorithm = B2D_DragonBox
    IF (PRESENT(B2DAlgo)) THEN
        IF ((B2DAlgo > 0).AND.(B2DAlgo < 4)) B2DAlgorithm = B2DAlgo
    END IF

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion of real value to its binary representation  +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! get raw IEEE binary floating point representation (little-endian order)
    FloatVal = Number

    ! decompose the representation into its parts
    Negative = IAND(RawBin, SignMask) /= ZeroUInt
    SigRaw   = IAND(RawBin, SignificandMask)
    ExpRaw   = ToInt32(SHIFTR(IAND(RawBin, ExponentMask), SignificandBits))

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from binary to decimal representation +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ConvFlag = TrueVal
    ! check for special cases
    IF ((ExpRaw == 0).AND.(SigRaw == ZeroUInt)) THEN
        ! zero
        SigDec = ZeroUInt
        ExpDec = 0
        ConvFlag = FalseVal
    ELSEIF (ExpRaw == MaxExponent) THEN
        ! NaN or Infinity
        SigDec = SigRaw
        ExpDec = ExceptionalExponent
        ConvFlag = FalseVal
    END IF

    ! get exponent and mantissa
    IF (ExpRaw /= 0) THEN
        ! normal number
        SigBin = IOR(SigRaw, SigHidBitMask)
        ExpBin = ExpRaw - ExponentBias - SignificandBits
        IF ((-SignificandBits <= ExpBin).AND.(ExpBin <= 0)) THEN
            IF (TRAILZ(SigBin) >= -ExpBin)THEN
                ! fast path for small integer number (without fraction?)
                SigDec = SHIFTR(SigBin, -ExpBin)
                ExpDec = 0
                ConvFlag = FalseVal
            END IF
        END IF
    ELSE
        ! subnormal number
        SigBin = SigRaw
        ExpBin = 1 - ExponentBias - SignificandBits
    END IF

    IF (ConvFlag) THEN
        ! perform binary-to-decimal conversion
        SELECT CASE (B2DAlgorithm)
        CASE (B2D_DragonBox)
            CALL Bin2Dec_DragonBox64(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            KeepTrailZ = FalseVal
        CASE (B2D_Ryu)
            CALL Bin2Dec_Ryu64(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            KeepTrailZ = TrueVal
        CASE (B2D_Schubfach)
            CALL Bin2Dec_Schubfach64(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            KeepTrailZ = FalseVal
        END SELECT
    END IF

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from decimal representation to decimal string  +++++
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! write output
    IF (Negative) THEN
        StrBuf(1:1) = '-'
        Pos = 1
    ELSE
        Pos = 0
    END IF
    IF (.NOT.PRESENT(MaxD)) THEN
        NumLen = Pos + Write_RealDP_Shortest(SigDec, ExpDec, StrBuf(Pos+1:), IsScientific, KeepTrailZ)
    ELSE
        BLOCK
            tSInt32     :: DEPos(2), wLen, CutPos, I, J
            wLen = Write_RealDP_Shortest(SigDec, ExpDec, StrBuf(Pos+1:), IsScientific, KeepTrailZ, DEPos)
            IF (MaxD > (DEPos(2)-DEPos(1))) THEN
                NumLen = Pos + wLen
            ELSE
                CutPos = Pos + DEPos(1) + MaxD
                IF (StrBuf(CutPos+1:CutPos+1) >= '5') THEN
                    ! add 1
                    StrBuf(CutPos:CutPos) = ACHAR(IACHAR(StrBuf(CutPos:CutPos))+1)
                END IF
                ! need to remove some digits
                IF (DEPos(2) == wLen) THEN
                    ! plain format: do not have exponent parts
                    NumLen = CutPos
                ELSE
                    ! E format: need to move exponent parts up
                    I = DEPos(1)
                    DO J = Pos+DEPos(2)+1, Pos+wLen
                        I = I + 1
                        StrBuf(I:I) = StrBuf(J:J)
                    END DO
                    NumLen = I
                END IF
            END IF
        END BLOCK
    END IF

    RETURN

! undefine macro definitions
#undef  BinaryPrecision
#undef  TotalBits
#undef  SignBits
#undef  SignificandBits
#undef  ExponentBits
#undef  MaxExponent
#undef  ExponentBias
#undef  DecimalPrecision
#undef  DecimalRange
#undef  MaxDecimalConversionDigits
#undef  SigHidBitMask
#undef  SignificandMask
#undef  SignMask
#undef  ExponentMask
#undef  ExpMantMask
#undef  QuietNaNMask

END FUNCTION Write_RealDP

!******************************************************************************

FUNCTION Write_RealQP(Number, StrBuf, B2DAlgo, IsScientific, MaxD, Fmt) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To write a quadruple-precision (128-bit) real value as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealQP,             INTENT(IN)     :: Number       !! number
    tCharLen(50),        INTENT(INOUT)  :: StrBuf       !! string buffer
    tSInt32,   OPTIONAL, INTENT(IN)     :: B2DAlgo
    !^ flag for binary-to-decimal algorithm <br>
    ! - B2D_DragonBox (or 1) if using the DragonBox algorithm. <br>
    ! - B2D_Ryu (or 2) if using the Ryu algorithm. <br>
    ! - B2D_Schubfach (or 3) if using the Schubfach algorithm. <br>
    ! - Default is B2D_DragonBox.
    tLogical,  OPTIONAL, INTENT(IN)     :: IsScientific
    !^ format flag <br>
    ! - True  if to write the given number in scientific format. <br>
    ! - False if to write the given number in general format. <br>
    ! - Default is false.
    tSInt32,   OPTIONAL, INTENT(IN)     :: MaxD
    !^ (maximum) number of digits to be written after the decimal point. <br>
    tCharStar, OPTIONAL, INTENT(IN)     :: Fmt
    !^ format to be used in the WRITE statement for the specified number. <br>
    !  If present, the procedure does not check its validity and will try to
    !  perform an internal write of the specified number based on the specified
    !  format while ignoring all other optional input.  If success, the procedure
    !  return immediately.  Otherwise, the procedure will check other optional
    !  input and write the number accordingly.
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    TYPE(UInt128), PARAMETER    :: ZeroUInt         = ZeroU128
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

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)   :: RawBin       ! raw IEEE binary floating point representation
    TYPE(UInt128)   :: SigRaw       ! raw (biased) significand in base 2
    tUInt32         :: ExpRaw       ! raw (biased) exponent in base 2
    TYPE(UInt128)   :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    TYPE(UInt128)   :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: ConvFlag     ! conversion flag (true if bin2dec conversion is needed)
    tUInt64         :: IntVal(2)    ! working integers (for conversion to binary representation)
    tFloat          :: FloatVal     ! working real (for conversion to binary representation)
    EQUIVALENCE(IntVal, FloatVal)
    tSInt32         :: Pos
    tSInt32         :: B2DAlgorithm
    tLogical        :: KeepTrailZ

!** FLOW

    ! check Fmt argument
    IF (PRESENT(Fmt)) THEN
        BLOCK
            tSInt32             :: IOStat   ! I/O status for error checking
            tCharLen(MsgLen)    :: IOMsg    ! I/O message for error checking
            tCharLen(50)        :: Buffer
            WRITE (Buffer, FMT=Fmt, IOSTAT=IOStat, IOMSG=IOMsg) Number
            IF (IOStat == 0) THEN
                StrBuf(1:) = TRIM(ADJUSTL(Buffer))
                NumLen = LEN_TRIM(StrBuf)
                RETURN
            END IF
        END BLOCK
    END IF

    ! set algorithm flag
    B2DAlgorithm = B2D_DragonBox
    IF (PRESENT(B2DAlgo)) THEN
        IF ((B2DAlgo > 0).AND.(B2DAlgo < 4)) B2DAlgorithm = B2DAlgo
    END IF

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion of real value to its binary representation  +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! get raw IEEE binary floating point representation (little-endian order)
    FloatVal = Number
    RawBin   = UInt128(IntVal(2), IntVal(1))

    ! decompose the representation into its parts
    Negative = IAND(RawBin, SignMask) /= ZeroUInt
    SigRaw   = IAND(RawBin, SignificandMask)
    ExpRaw   = ToI32(SHIFTR(IAND(RawBin, ExponentMask), SignificandBits))

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from binary to decimal representation +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ConvFlag = TrueVal
    ! check for special cases
    IF ((ExpRaw == 0).AND.(SigRaw == ZeroUInt)) THEN
        ! zero
        SigDec = ZeroUInt
        ExpDec = 0
        ConvFlag = FalseVal
    ELSEIF (ExpRaw == MaxExponent) THEN
        ! NaN or Infinity
        SigDec = SigRaw
        ExpDec = ExceptionalExponent
        ConvFlag = FalseVal
    END IF

    ! get exponent and mantissa
    IF (ExpRaw /= 0) THEN
        ! normal number
        SigBin = IOR(SigRaw, SigHidBitMask)
        ExpBin = ExpRaw - ExponentBias - SignificandBits
        IF ((-SignificandBits <= ExpBin).AND.(ExpBin <= 0)) THEN
            IF (TRAILZ(SigBin) >= -ExpBin)THEN
                ! fast path for small integer number (without fraction?)
                SigDec = SHIFTR(SigBin, -ExpBin)
                ExpDec = 0
                ConvFlag = FalseVal
            END IF
        END IF
    ELSE
        ! subnormal number
        SigBin = SigRaw
        ExpBin = 1 - ExponentBias - SignificandBits
    END IF

    IF (ConvFlag) THEN
        ! perform binary-to-decimal conversion
        SELECT CASE (B2DAlgorithm)
        CASE (B2D_DragonBox)
            CALL Bin2Dec_DragonBox128(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            KeepTrailZ = FalseVal
        CASE (B2D_Ryu)
            CALL Bin2Dec_Ryu128(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            KeepTrailZ = TrueVal
        CASE (B2D_Schubfach)
            CALL Bin2Dec_Schubfach128(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            KeepTrailZ = FalseVal
        END SELECT
    END IF

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from decimal representation to decimal string  +++++
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! write output
    IF (Negative) THEN
        StrBuf(1:1) = '-'
        Pos = 1
    ELSE
        Pos = 0
    END IF
    IF (.NOT.PRESENT(MaxD)) THEN
        NumLen = Pos + Write_RealQP_Shortest(SigDec, ExpDec, StrBuf(Pos+1:), IsScientific, KeepTrailZ)
    ELSE
        BLOCK
            tSInt32     :: DEPos(2), wLen, CutPos, I, J
            wLen = Write_RealQP_Shortest(SigDec, ExpDec, StrBuf(Pos+1:), IsScientific, KeepTrailZ, DEPos)
            IF (MaxD > (DEPos(2)-DEPos(1))) THEN
                NumLen = Pos + wLen
            ELSE
                CutPos = Pos + DEPos(1) + MaxD
                IF (StrBuf(CutPos+1:CutPos+1) >= '5') THEN
                    ! add 1
                    StrBuf(CutPos:CutPos) = ACHAR(IACHAR(StrBuf(CutPos:CutPos))+1)
                END IF
                ! need to remove some digits
                IF (DEPos(2) == wLen) THEN
                    ! plain format: do not have exponent parts
                    NumLen = CutPos
                ELSE
                    ! E format: need to move exponent parts up
                    I = DEPos(1)
                    DO J = Pos+DEPos(2)+1, Pos+wLen
                        I = I + 1
                        StrBuf(I:I) = StrBuf(J:J)
                    END DO
                    NumLen = I
                END IF
            END IF
        END BLOCK
    END IF

    RETURN

! undefine macro definitions
#undef  BinaryPrecision
#undef  TotalBits
#undef  SignBits
#undef  SignificandBits
#undef  ExponentBits
#undef  MaxExponent
#undef  ExponentBias
#undef  DecimalPrecision
#undef  DecimalRange
#undef  MaxDecimalConversionDigits
#undef  SigHidBitMask
#undef  SignificandMask
#undef  SignMask
#undef  ExponentMask
#undef  ExpMantMask
#undef  QuietNaNMask

END FUNCTION Write_RealQP

!******************************************************************************

FUNCTION Write_RealSP_Format(Number, StrBuf, Fmt, B2DAlgo, Width, Digits) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To write a single-precision (32-bit) real value as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealSP,           INTENT(IN)       :: Number       !! number
    tCharLen(20),      INTENT(INOUT)    :: StrBuf       !! string buffer
    tChar,             INTENT(IN)       :: Fmt
    !^ format (F, E or G)
    tSInt32, OPTIONAL, INTENT(IN)       :: B2DAlgo
    !^ flag for binary-to-decimal algorithm <br>
    ! - B2D_DragonBox (or 1) if using the DragonBox algorithm. <br>
    ! - B2D_Ryu (or 2) if using the Ryu algorithm. <br>
    ! - B2D_Schubfach (or 3) if using the Schubfach algorithm. <br>
    ! - Default is B2D_DragonBox.
    tSInt32, OPTIONAL, INTENT(IN)       :: Width
    !^ (maximum) length of string to be written to; if not present, Width = LEN(StrBuf)
    tSInt32, OPTIONAL, INTENT(IN)       :: Digits
    !^ (maximum) number of digits after the decimal point; if not present, use shortest representation
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, PARAMETER  :: ZeroUInt         = 0_kInt32
    tSInt32, PARAMETER  :: BinaryPrecision  = 24
    tSInt32, PARAMETER  :: TotalBits        = 32
    tSInt32, PARAMETER  :: SignBits         = TotalBits - 1                     ! 31
    tSInt32, PARAMETER  :: SignificandBits  = BinaryPrecision - 1               ! 23
    tSInt32, PARAMETER  :: ExponentBits     = TotalBits - BinaryPrecision       ! 8
    tSInt32, PARAMETER  :: MaxExponent      = SHIFTL(1, ExponentBits) - 1       ! 255
    tSInt32, PARAMETER  :: ExponentBias     = SHIFTL(1, ExponentBits - 1) - 1   ! 127
    tUInt32, PARAMETER  :: SigHidBitMask    = SHIFTL(1, SignificandBits)
    tUInt32, PARAMETER  :: SignificandMask  = SigHidBitMask - 1
    tUInt32, PARAMETER  :: SignMask         = SHIFTL(1, SignBits)
    tUInt32, PARAMETER  :: ExponentMask     = NOT(IOR(SignMask, SignificandMask))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32         :: RawBin       ! raw IEEE binary floating point representation
    tUInt32         :: SigRaw       ! raw (biased) significand in base 2
    tUInt32         :: ExpRaw       ! raw (biased) exponent in base 2
    tUInt32         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt32         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: ConvFlag     ! conversion flag (true if bin2dec conversion is needed)
    tFloat          :: FloatVal     ! working real (for conversion to binary representation)
    EQUIVALENCE(RawBin, FloatVal)
    tSInt32         :: Pos
    tSInt32         :: B2DAlgorithm
    tSInt32         :: SigLen
    tCharLen(10)    :: SigStr
    tLogical        :: RemoveZeros
    tUInt32         :: Sig10
    tSInt32         :: Exp10

!** FLOW

    ! set algorithm flag
    B2DAlgorithm = B2D_DragonBox
    IF (PRESENT(B2DAlgo)) THEN
        IF ((B2DAlgo > 0).AND.(B2DAlgo < 4)) B2DAlgorithm = B2DAlgo
    END IF

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion of real value to its binary representation  +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! get raw IEEE binary floating point representation (little-endian order)
    FloatVal = Number

    ! decompose the representation into its parts
    Negative = IAND(RawBin, SignMask) /= ZeroUInt
    SigRaw   = IAND(RawBin, SignificandMask)
    ExpRaw   = ToInt32(SHIFTR(IAND(RawBin, ExponentMask), SignificandBits))

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from binary to decimal representation +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ConvFlag = TrueVal
    ! check for special cases
    IF ((ExpRaw == 0).AND.(SigRaw == ZeroUInt)) THEN
        ! zero
        SigDec = ZeroUInt
        ExpDec = 0
        ConvFlag = FalseVal
    ELSEIF (ExpRaw == MaxExponent) THEN
        ! NaN or Infinity
        SigDec = SigRaw
        ExpDec = ExceptionalExponent
        ConvFlag = FalseVal
    END IF

    ! get exponent and mantissa
    IF (ExpRaw /= 0) THEN
        ! normal number
        SigBin = IOR(SigRaw, SigHidBitMask)
        ExpBin = ExpRaw - ExponentBias - SignificandBits
        IF ((-SignificandBits <= ExpBin).AND.(ExpBin <= 0)) THEN
            IF (TRAILZ(SigBin) >= -ExpBin)THEN
                ! fast path for small integer number (without fraction?)
                SigDec = SHIFTR(SigBin, -ExpBin)
                ExpDec = 0
                ConvFlag = FalseVal
            END IF
        END IF
    ELSE
        ! subnormal number
        SigBin = SigRaw
        ExpBin = 1 - ExponentBias - SignificandBits
    END IF

    IF (ConvFlag) THEN
        ! perform binary-to-decimal conversion
        SELECT CASE (B2DAlgorithm)
        CASE (B2D_DragonBox)
            CALL Bin2Dec_DragonBox32(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            RemoveZeros = TrueVal
        CASE (B2D_Ryu)
            CALL Bin2Dec_Ryu32(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            RemoveZeros = FalseVal
        CASE (B2D_Schubfach)
            CALL Bin2Dec_Schubfach32(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            RemoveZeros = TrueVal
        END SELECT
    END IF

    ! check for special case
    IF (IsSpecialCase(SigDec, ExpDec, Negative, StrBuf, NumLen)) RETURN

    ! convert the decimal significand and exponent
    CALL FP_Conversion(SigDec, ExpDec, Sig10, Exp10)

    ! write the decimal significand
    SigLen = Write_AbsI32_9Digits(Sig10, SigStr)
    IF (RemoveZeros) CALL RemoveTrailingZeros(SigStr, SigLen)

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from decimal representation to decimal string  +++++
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! write output
    IF (Negative) THEN
        StrBuf(1:1) = '-'
        Pos = 1
    ELSE
        Pos = 0
    END IF
    NumLen = Pos + Write_Real_Generic(SigStr, SigLen, Exp10, StrBuf(Pos+1:), Fmt, Width, Digits)

    RETURN

END FUNCTION Write_RealSP_Format

!******************************************************************************

FUNCTION Write_RealDP_Format(Number, StrBuf, Fmt, B2DAlgo, Width, Digits) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To write a double-precision (64-bit) real value as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealDP,           INTENT(IN)       :: Number       !! number
    tCharLen(30),      INTENT(INOUT)    :: StrBuf       !! string buffer
    tChar,             INTENT(IN)       :: Fmt
    !^ format (F, E or G)
    tSInt32, OPTIONAL, INTENT(IN)       :: B2DAlgo
    !^ flag for binary-to-decimal algorithm <br>
    ! - B2D_DragonBox (or 1) if using the DragonBox algorithm. <br>
    ! - B2D_Ryu (or 2) if using the Ryu algorithm. <br>
    ! - B2D_Schubfach (or 3) if using the Schubfach algorithm. <br>
    ! - Default is B2D_DragonBox.
    tSInt32, OPTIONAL, INTENT(IN)       :: Width
    !^ (maximum) length of string to be written to; if not present, Width = LEN(StrBuf)
    tSInt32, OPTIONAL, INTENT(IN)       :: Digits
    !^ (maximum) number of digits after the decimal point; if not present, use shortest representation
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, PARAMETER  :: ZeroUInt         = 0_kInt64
    tSInt32, PARAMETER  :: BinaryPrecision  = 53
    tSInt32, PARAMETER  :: TotalBits        = 64
    tSInt32, PARAMETER  :: SignBits         = TotalBits - 1                     ! 63
    tSInt32, PARAMETER  :: SignificandBits  = BinaryPrecision - 1               ! 52
    tSInt32, PARAMETER  :: ExponentBits     = TotalBits - BinaryPrecision       ! 11
    tSInt32, PARAMETER  :: MaxExponent      = SHIFTL(1, ExponentBits) - 1       ! 2047
    tSInt32, PARAMETER  :: ExponentBias     = SHIFTL(1, ExponentBits - 1) - 1   ! 1023
    tUInt64, PARAMETER  :: SigHidBitMask    = SHIFTL(1_kInt64, SignificandBits)
    tUInt64, PARAMETER  :: SignificandMask  = SigHidBitMask - 1_kInt64
    tUInt64, PARAMETER  :: SignMask         = SHIFTL(1_kInt64, SignBits)
    tUInt64, PARAMETER  :: ExponentMask     = NOT(IOR(SignMask, SignificandMask))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: RawBin       ! raw IEEE binary floating point representation
    tUInt64         :: SigRaw       ! raw (biased) significand in base 2
    tUInt32         :: ExpRaw       ! raw (biased) exponent in base 2
    tUInt64         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    tUInt64         :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: ConvFlag     ! conversion flag (true if bin2dec conversion is needed)
    tFloat          :: FloatVal     ! working real (for conversion to binary representation)
    EQUIVALENCE(RawBin, FloatVal)
    tSInt32         :: Pos
    tSInt32         :: B2DAlgorithm
    tSInt32         :: SigLen
    tCharLen(20)    :: SigStr
    tLogical        :: RemoveZeros
    tUInt64         :: Sig10
    tSInt32         :: Exp10

!** FLOW

    ! set algorithm flag
    B2DAlgorithm = B2D_DragonBox
    IF (PRESENT(B2DAlgo)) THEN
        IF ((B2DAlgo > 0).AND.(B2DAlgo < 4)) B2DAlgorithm = B2DAlgo
    END IF

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion of real value to its binary representation  +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! get raw IEEE binary floating point representation (little-endian order)
    FloatVal = Number

    ! decompose the representation into its parts
    Negative = IAND(RawBin, SignMask) /= ZeroUInt
    SigRaw   = IAND(RawBin, SignificandMask)
    ExpRaw   = ToInt32(SHIFTR(IAND(RawBin, ExponentMask), SignificandBits))

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from binary to decimal representation +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ConvFlag = TrueVal
    ! check for special cases
    IF ((ExpRaw == 0).AND.(SigRaw == ZeroUInt)) THEN
        ! zero
        SigDec = ZeroUInt
        ExpDec = 0
        ConvFlag = FalseVal
    ELSEIF (ExpRaw == MaxExponent) THEN
        ! NaN or Infinity
        SigDec = SigRaw
        ExpDec = ExceptionalExponent
        ConvFlag = FalseVal
    END IF

    ! get exponent and mantissa
    IF (ExpRaw /= 0) THEN
        ! normal number
        SigBin = IOR(SigRaw, SigHidBitMask)
        ExpBin = ExpRaw - ExponentBias - SignificandBits
        IF ((-SignificandBits <= ExpBin).AND.(ExpBin <= 0)) THEN
            IF (TRAILZ(SigBin) >= -ExpBin)THEN
                ! fast path for small integer number (without fraction?)
                SigDec = SHIFTR(SigBin, -ExpBin)
                ExpDec = 0
                ConvFlag = FalseVal
            END IF
        END IF
    ELSE
        ! subnormal number
        SigBin = SigRaw
        ExpBin = 1 - ExponentBias - SignificandBits
    END IF

    IF (ConvFlag) THEN
        ! perform binary-to-decimal conversion
        SELECT CASE (B2DAlgorithm)
        CASE (B2D_DragonBox)
            CALL Bin2Dec_DragonBox64(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            RemoveZeros = TrueVal
        CASE (B2D_Ryu)
            CALL Bin2Dec_Ryu64(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            RemoveZeros = FalseVal
        CASE (B2D_Schubfach)
            CALL Bin2Dec_Schubfach64(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            RemoveZeros = TrueVal
        END SELECT
    END IF

    ! check for special case
    IF (IsSpecialCase(SigDec, ExpDec, Negative, StrBuf, NumLen)) RETURN

    ! convert the decimal significand and exponent
    CALL FP_Conversion(SigDec, ExpDec, Sig10, Exp10)

    ! write the decimal significand
    SigLen = Write_AbsI64(Sig10, SigStr)
    IF (RemoveZeros) CALL RemoveTrailingZeros(SigStr, SigLen)

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from decimal representation to decimal string  +++++
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! write output
    IF (Negative) THEN
        StrBuf(1:1) = '-'
        Pos = 1
    ELSE
        Pos = 0
    END IF
    NumLen = Pos + Write_Real_Generic(SigStr, SigLen, Exp10, StrBuf(Pos+1:), Fmt, Width, Digits)

    RETURN

END FUNCTION Write_RealDP_Format

!******************************************************************************

FUNCTION Write_RealQP_Format(Number, StrBuf, Fmt, B2DAlgo, Width, Digits) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To write a quadruple-precision (128-bit) real value as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealQP,           INTENT(IN)       :: Number       !! number
    tCharLen(50),      INTENT(INOUT)    :: StrBuf       !! string buffer
    tChar,             INTENT(IN)       :: Fmt
    !^ format (F, E or G)
    tSInt32, OPTIONAL, INTENT(IN)       :: B2DAlgo
    !^ flag for binary-to-decimal algorithm <br>
    ! - B2D_DragonBox (or 1) if using the DragonBox algorithm. <br>
    ! - B2D_Ryu (or 2) if using the Ryu algorithm. <br>
    ! - B2D_Schubfach (or 3) if using the Schubfach algorithm. <br>
    ! - Default is B2D_DragonBox.
    tSInt32, OPTIONAL, INTENT(IN)       :: Width
    !^ (maximum) length of string to be written to; if not present, Width = LEN(StrBuf)
    tSInt32, OPTIONAL, INTENT(IN)       :: Digits
    !^ (maximum) number of digits after the decimal point; if not present, use shortest representation
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), PARAMETER    :: ZeroUInt         = ZeroU128
    tSInt32,       PARAMETER    :: BinaryPrecision  = 113
    tSInt32,       PARAMETER    :: TotalBits        = 128
    tSInt32,       PARAMETER    :: SignBits         = TotalBits - 1                     ! 127
    tSInt32,       PARAMETER    :: SignificandBits  = BinaryPrecision - 1               ! 112
    tSInt32,       PARAMETER    :: ExponentBits     = TotalBits - BinaryPrecision       ! 15
    tSInt32,       PARAMETER    :: MaxExponent      = SHIFTL(1, ExponentBits) - 1       ! 32767
    tSInt32,       PARAMETER    :: ExponentBias     = SHIFTL(1, ExponentBits - 1) - 1   ! 16383
    TYPE(UInt128), PARAMETER    :: SigHidBitMask    = UInt128(SHIFTL(1_kInt64, SignificandBits-64),          &
                                                              0_kInt64)
    TYPE(UInt128), PARAMETER    :: SignificandMask  = UInt128(SHIFTL(1_kInt64, SignificandBits-64)-1_kInt64, &
                                                              -1_kInt64)
    TYPE(UInt128), PARAMETER    :: SignMask         = UInt128(SHIFTL(1_kInt64, SignBits-64),                 &
                                                              0_kInt64)
    TYPE(UInt128), PARAMETER    :: ExponentMask     = UInt128(NOT(IOR(SignMask%High, SignificandMask%High)), &
                                                              NOT(IOR(SignMask%Low,  SignificandMask%Low)))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)   :: RawBin       ! raw IEEE binary floating point representation
    TYPE(UInt128)   :: SigRaw       ! raw (biased) significand in base 2
    tUInt32         :: ExpRaw       ! raw (biased) exponent in base 2
    TYPE(UInt128)   :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: Negative     ! sign flag (true if real value is negative)
    TYPE(UInt128)   :: SigDec       ! significand in base 10
    tSInt32         :: ExpDec       ! exponent in base 10
    tLogical        :: ConvFlag     ! conversion flag (true if bin2dec conversion is needed)
    tUInt64         :: IntVal(2)    ! working integers (for conversion to binary representation)
    tFloat          :: FloatVal     ! working real (for conversion to binary representation)
    EQUIVALENCE(IntVal, FloatVal)
    tSInt32         :: Pos
    tSInt32         :: B2DAlgorithm
    tSInt32         :: SigLen
    tCharLen(40)    :: SigStr
    tLogical        :: RemoveZeros
    TYPE(UInt128)   :: Sig10
    tSInt32         :: Exp10

!** FLOW

    ! set algorithm flag
    B2DAlgorithm = B2D_DragonBox
    IF (PRESENT(B2DAlgo)) THEN
        IF ((B2DAlgo > 0).AND.(B2DAlgo < 4)) B2DAlgorithm = B2DAlgo
    END IF

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion of real value to its binary representation  +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! get raw IEEE binary floating point representation (little-endian order)
    FloatVal = Number
    RawBin   = UInt128(IntVal(2), IntVal(1))

    ! decompose the representation into its parts
    Negative = IAND(RawBin, SignMask) /= ZeroUInt
    SigRaw   = IAND(RawBin, SignificandMask)
    ExpRaw   = ToI32(SHIFTR(IAND(RawBin, ExponentMask), SignificandBits))

    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from binary to decimal representation +++++
    ! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ConvFlag = TrueVal
    ! check for special cases
    IF ((ExpRaw == 0).AND.(SigRaw == ZeroUInt)) THEN
        ! zero
        SigDec = ZeroUInt
        ExpDec = 0
        ConvFlag = FalseVal
    ELSEIF (ExpRaw == MaxExponent) THEN
        ! NaN or Infinity
        SigDec = SigRaw
        ExpDec = ExceptionalExponent
        ConvFlag = FalseVal
    END IF

    ! get exponent and mantissa
    IF (ExpRaw /= 0) THEN
        ! normal number
        SigBin = IOR(SigRaw, SigHidBitMask)
        ExpBin = ExpRaw - ExponentBias - SignificandBits
        IF ((-SignificandBits <= ExpBin).AND.(ExpBin <= 0)) THEN
            IF (TRAILZ(SigBin) >= -ExpBin)THEN
                ! fast path for small integer number (without fraction?)
                SigDec = SHIFTR(SigBin, -ExpBin)
                ExpDec = 0
                ConvFlag = FalseVal
            END IF
        END IF
    ELSE
        ! subnormal number
        SigBin = SigRaw
        ExpBin = 1 - ExponentBias - SignificandBits
    END IF

    IF (ConvFlag) THEN
        ! perform binary-to-decimal conversion
        SELECT CASE (B2DAlgorithm)
        CASE (B2D_DragonBox)
            CALL Bin2Dec_DragonBox128(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            RemoveZeros = TrueVal
        CASE (B2D_Ryu)
            CALL Bin2Dec_Ryu128(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            RemoveZeros = FalseVal
        CASE (B2D_Schubfach)
            CALL Bin2Dec_Schubfach128(SigRaw, ExpRaw, SigBin, ExpBin, SigDec, ExpDec)
            RemoveZeros = TrueVal
        END SELECT
    END IF

    ! check for special case
    IF (IsSpecialCase(SigDec, ExpDec, Negative, StrBuf, NumLen)) RETURN

    ! convert the decimal significand and exponent
    CALL FP_Conversion(SigDec, ExpDec, Sig10, Exp10)

    ! write the decimal significand
    SigLen = Write_Sig128(Sig10, SigStr)
    IF (RemoveZeros) CALL RemoveTrailingZeros(SigStr, SigLen)

    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! +++++ conversion from decimal representation to decimal string  +++++
    ! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    ! write output
    IF (Negative) THEN
        StrBuf(1:1) = '-'
        Pos = 1
    ELSE
        Pos = 0
    END IF
    NumLen = Pos + Write_Real_Generic(SigStr, SigLen, Exp10, StrBuf(Pos+1:), Fmt, Width, Digits)

    RETURN

END FUNCTION Write_RealQP_Format

!******************************************************************************

FUNCTION Write_CmpxSP(Number, StrBuf, B2DAlgo, IsScientific, MaxD, Fmt) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To write a single-precision complex value as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCmpxSP,             INTENT(IN)     :: Number       !! number
    tCharLen(45),        INTENT(INOUT)  :: StrBuf       !! string buffer
    tSInt32,   OPTIONAL, INTENT(IN)     :: B2DAlgo
    !^ flag for binary-to-decimal algorithm <br>
    ! - B2D_DragonBox (or 1) if using the DragonBox algorithm. <br>
    ! - B2D_Ryu (or 2) if using the Ryu algorithm. <br>
    ! - B2D_Schubfach (or 3) if using the Schubfach algorithm. <br>
    ! - Default is B2D_DragonBox.
    tLogical,  OPTIONAL, INTENT(IN)     :: IsScientific
    !^ format flag <br>
    ! - True  if to write the given number in scientific format. <br>
    ! - False if to write the given number in general format. <br>
    ! - Default is false.
    tSInt32,   OPTIONAL, INTENT(IN)     :: MaxD
    !^ (maximum) number of digits to be written after the decimal point. <br>
    tCharStar, OPTIONAL, INTENT(IN)     :: Fmt
    !^ format to be used in the WRITE statement for the specified number. <br>
    !  If present, the procedure does not check its validity and will try to
    !  perform an internal write of the specified number based on the specified
    !  format while ignoring all other optional input.  If success, the procedure
    !  return immediately.  Otherwise, the procedure will check other optional
    !  input and write the number accordingly.
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check Fmt argument
    IF (PRESENT(Fmt)) THEN
        BLOCK
            tSInt32             :: IOStat   ! I/O status for error checking
            tCharLen(MsgLen)    :: IOMsg    ! I/O message for error checking
            tCharLen(45)        :: Buffer
            WRITE (Buffer, FMT=Fmt, IOSTAT=IOStat, IOMSG=IOMsg) Number
            IF (IOStat == 0) THEN
                StrBuf(1:) = TRIM(ADJUSTL(Buffer))
                NumLen = LEN_TRIM(StrBuf)
                RETURN
            END IF
        END BLOCK
    END IF

    StrBuf(1:1) = '('
    NumLen = Write_RealSP(Number%RE, StrBuf(2:), B2DAlgo, IsScientific, MaxD) + 1
    StrBuf(NumLen+1:NumLen+2) = ', '
    NumLen = Write_RealSP(Number%IM, StrBuf(NumLen+3:), B2DAlgo, IsScientific, MaxD) + (NumLen + 3)
    StrBuf(NumLen:NumLen) = ')'

    RETURN

END FUNCTION Write_CmpxSP

!******************************************************************************

FUNCTION Write_CmpxDP(Number, StrBuf, B2DAlgo, IsScientific, MaxD, Fmt) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To write a single-precision complex value as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCmpxDP,             INTENT(IN)     :: Number       !! number
    tCharLen(65),        INTENT(INOUT)  :: StrBuf       !! string buffer
    tSInt32,   OPTIONAL, INTENT(IN)     :: B2DAlgo
    !^ flag for binary-to-decimal algorithm <br>
    ! - B2D_DragonBox (or 1) if using the DragonBox algorithm. <br>
    ! - B2D_Ryu (or 2) if using the Ryu algorithm. <br>
    ! - B2D_Schubfach (or 3) if using the Schubfach algorithm. <br>
    ! - Default is B2D_DragonBox.
    tLogical,  OPTIONAL, INTENT(IN)     :: IsScientific
    !^ format flag <br>
    ! - True  if to write the given number in scientific format. <br>
    ! - False if to write the given number in general format. <br>
    ! - Default is false.
    tSInt32,   OPTIONAL, INTENT(IN)     :: MaxD
    !^ (maximum) number of digits to be written after the decimal point. <br>
    tCharStar, OPTIONAL, INTENT(IN)     :: Fmt
    !^ format to be used in the WRITE statement for the specified number. <br>
    !  If present, the procedure does not check its validity and will try to
    !  perform an internal write of the specified number based on the specified
    !  format while ignoring all other optional input.  If success, the procedure
    !  return immediately.  Otherwise, the procedure will check other optional
    !  input and write the number accordingly.
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check Fmt argument
    IF (PRESENT(Fmt)) THEN
        BLOCK
            tSInt32             :: IOStat   ! I/O status for error checking
            tCharLen(MsgLen)    :: IOMsg    ! I/O message for error checking
            tCharLen(65)        :: Buffer
            WRITE (Buffer, FMT=Fmt, IOSTAT=IOStat, IOMSG=IOMsg) Number
            IF (IOStat == 0) THEN
                StrBuf(1:) = TRIM(ADJUSTL(Buffer))
                NumLen = LEN_TRIM(StrBuf)
                RETURN
            END IF
        END BLOCK
    END IF

    StrBuf(1:1) = '('
    NumLen = Write_RealDP(Number%RE, StrBuf(2:), B2DAlgo, IsScientific, MaxD) + 1
    StrBuf(NumLen+1:NumLen+2) = ', '
    NumLen = Write_RealDP(Number%IM, StrBuf(NumLen+3:), B2DAlgo, IsScientific, MaxD) + (NumLen + 3)
    StrBuf(NumLen:NumLen) = ')'

    RETURN

END FUNCTION Write_CmpxDP

!******************************************************************************

FUNCTION Write_CmpxQP(Number, StrBuf, B2DAlgo, IsScientific, MaxD, Fmt) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To write a single-precision complex value as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCmpxQP,             INTENT(IN)     :: Number       !! number
    tCharLen(105),       INTENT(INOUT)  :: StrBuf       !! string buffer
    tSInt32,   OPTIONAL, INTENT(IN)     :: B2DAlgo
    !^ flag for binary-to-decimal algorithm <br>
    ! - B2D_DragonBox (or 1) if using the DragonBox algorithm. <br>
    ! - B2D_Ryu (or 2) if using the Ryu algorithm. <br>
    ! - B2D_Schubfach (or 3) if using the Schubfach algorithm. <br>
    ! - Default is B2D_DragonBox.
    tLogical,  OPTIONAL, INTENT(IN)     :: IsScientific
    !^ format flag <br>
    ! - True  if to write the given number in scientific format. <br>
    ! - False if to write the given number in general format. <br>
    ! - Default is false.
    tSInt32,   OPTIONAL, INTENT(IN)     :: MaxD
    !^ (maximum) number of digits to be written after the decimal point. <br>
    tCharStar, OPTIONAL, INTENT(IN)     :: Fmt
    !^ format to be used in the WRITE statement for the specified number. <br>
    !  If present, the procedure does not check its validity and will try to
    !  perform an internal write of the specified number based on the specified
    !  format while ignoring all other optional input.  If success, the procedure
    !  return immediately.  Otherwise, the procedure will check other optional
    !  input and write the number accordingly.
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! check Fmt argument
    IF (PRESENT(Fmt)) THEN
        BLOCK
            tSInt32             :: IOStat   ! I/O status for error checking
            tCharLen(MsgLen)    :: IOMsg    ! I/O message for error checking
            tCharLen(105)       :: Buffer
            WRITE (Buffer, FMT=Fmt, IOSTAT=IOStat, IOMSG=IOMsg) Number
            IF (IOStat == 0) THEN
                StrBuf(1:) = TRIM(ADJUSTL(Buffer))
                NumLen = LEN_TRIM(StrBuf)
                RETURN
            END IF
        END BLOCK
    END IF

    StrBuf(1:1) = '('
    NumLen = Write_RealQP(Number%RE, StrBuf(2:), B2DAlgo, IsScientific, MaxD) + 1
    StrBuf(NumLen+1:NumLen+2) = ', '
    NumLen = Write_RealQP(Number%IM, StrBuf(NumLen+3:), B2DAlgo, IsScientific, MaxD) + (NumLen + 3)
    StrBuf(NumLen:NumLen) = ')'

    RETURN

END FUNCTION Write_CmpxQP

!******************************************************************************

FUNCTION Write_CmpxSP_Format(Number, StrBuf, Fmt, B2DAlgo, Width, Digits) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To write a single-precision complex value as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCmpxSP,           INTENT(IN)       :: Number       !! number
    tCharLen(45),      INTENT(INOUT)    :: StrBuf       !! string buffer
    tChar,             INTENT(IN)       :: Fmt
    !^ format (F, E or G)
    tSInt32, OPTIONAL, INTENT(IN)       :: B2DAlgo
    !^ flag for binary-to-decimal algorithm <br>
    ! - B2D_DragonBox (or 1) if using the DragonBox algorithm. <br>
    ! - B2D_Ryu (or 2) if using the Ryu algorithm. <br>
    ! - B2D_Schubfach (or 3) if using the Schubfach algorithm. <br>
    ! - Default is B2D_DragonBox.
    tSInt32, OPTIONAL, INTENT(IN)       :: Width
    !^ (maximum) length of string to be written to; if not present, Width = LEN(StrBuf)
    tSInt32, OPTIONAL, INTENT(IN)       :: Digits
    !^ (maximum) number of digits after the decimal point; if not present, use shortest representation
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    StrBuf(1:1) = '('
    NumLen = Write_RealSP_Format(Number%RE, StrBuf(2:), Fmt, B2DAlgo, Width, Digits) + 1
    StrBuf(NumLen+1:NumLen+2) = ', '
    NumLen = Write_RealSP_Format(Number%IM, StrBuf(NumLen+3:), Fmt, B2DAlgo, Width, Digits) + (NumLen + 3)
    StrBuf(NumLen:NumLen) = ')'

    RETURN

END FUNCTION Write_CmpxSP_Format

!******************************************************************************

FUNCTION Write_CmpxDP_Format(Number, StrBuf, Fmt, B2DAlgo, Width, Digits) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To write a single-precision complex value as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCmpxDP,           INTENT(IN)       :: Number       !! number
    tCharLen(65),      INTENT(INOUT)    :: StrBuf       !! string buffer
    tChar,             INTENT(IN)       :: Fmt
    !^ format (F, E or G)
    tSInt32, OPTIONAL, INTENT(IN)       :: B2DAlgo
    !^ flag for binary-to-decimal algorithm <br>
    ! - B2D_DragonBox (or 1) if using the DragonBox algorithm. <br>
    ! - B2D_Ryu (or 2) if using the Ryu algorithm. <br>
    ! - B2D_Schubfach (or 3) if using the Schubfach algorithm. <br>
    ! - Default is B2D_DragonBox.
    tSInt32, OPTIONAL, INTENT(IN)       :: Width
    !^ (maximum) length of string to be written to; if not present, Width = LEN(StrBuf)
    tSInt32, OPTIONAL, INTENT(IN)       :: Digits
    !^ (maximum) number of digits after the decimal point; if not present, use shortest representation
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    StrBuf(1:1) = '('
    NumLen = Write_RealDP_Format(Number%RE, StrBuf(2:), Fmt, B2DAlgo, Width, Digits) + 1
    StrBuf(NumLen+1:NumLen+2) = ', '
    NumLen = Write_RealDP_Format(Number%IM, StrBuf(NumLen+3:), Fmt, B2DAlgo, Width, Digits) + (NumLen + 3)
    StrBuf(NumLen:NumLen) = ')'

    RETURN

END FUNCTION Write_CmpxDP_Format

!******************************************************************************

FUNCTION Write_CmpxQP_Format(Number, StrBuf, Fmt, B2DAlgo, Width, Digits) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    ! To write a single-precision complex value as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCmpxQP,           INTENT(IN)       :: Number       !! number
    tCharLen(105),     INTENT(INOUT)    :: StrBuf       !! string buffer
    tChar,             INTENT(IN)       :: Fmt
    !^ format (F, E or G)
    tSInt32, OPTIONAL, INTENT(IN)       :: B2DAlgo
    !^ flag for binary-to-decimal algorithm <br>
    ! - B2D_DragonBox (or 1) if using the DragonBox algorithm. <br>
    ! - B2D_Ryu (or 2) if using the Ryu algorithm. <br>
    ! - B2D_Schubfach (or 3) if using the Schubfach algorithm. <br>
    ! - Default is B2D_DragonBox.
    tSInt32, OPTIONAL, INTENT(IN)       :: Width
    !^ (maximum) length of string to be written to; if not present, Width = LEN(StrBuf)
    tSInt32, OPTIONAL, INTENT(IN)       :: Digits
    !^ (maximum) number of digits after the decimal point; if not present, use shortest representation
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    StrBuf(1:1) = '('
    NumLen = Write_RealQP_Format(Number%RE, StrBuf(2:), Fmt, B2DAlgo, Width, Digits) + 1
    StrBuf(NumLen+1:NumLen+2) = ', '
    NumLen = Write_RealQP_Format(Number%IM, StrBuf(NumLen+3:), Fmt, B2DAlgo, Width, Digits) + (NumLen + 3)
    StrBuf(NumLen:NumLen) = ')'

    RETURN

END FUNCTION Write_CmpxQP_Format

!------------------------------------------------------------------------------
!                              PRIVATE PROCEDURES
!------------------------------------------------------------------------------

FUNCTION Write_AbsI32(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a positive 32-bit integer number as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,      INTENT(IN)    :: Number   !! number
    tCharLen(10), INTENT(INOUT) :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER :: ShiftPos = 45
    tSInt64, PARAMETER :: Multiplier = ToInt64(Z'00000000D1B71759')
    tSInt32, PARAMETER :: Divisor = 10000

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: NxtNum, RemNum
    tSInt32     :: NxtNxtNum, NxtRemNum

!** FLOW

    ! start the conversion
    IF (Number < 10000) THEN
        NumLen = Write_AbsI32_1To4Digits(Number, StrBuf)
    ELSEIF (Number < 100000000) THEN
        NumLen = Write_AbsI32_5To8Digits(Number, StrBuf)
    ELSE
        ! compute the next round of working number
        NxtNum = ToInt32(SHIFTR(Number*Multiplier, ShiftPos))       ! NxtNum = Number/10000
        ! compute the remainder
        RemNum = Number - NxtNum*Divisor                            ! RemNum = MOD(Number, 10000)
        ! compute the next-next round of working number
        NxtNxtNum = ToInt32(SHIFTR(NxtNum*Multiplier, ShiftPos))    ! NxtNxtNum = NxtNum/10000
        ! compute the remainder
        NxtRemNum = NxtNum - NxtNxtNum*Divisor                      ! NxtRemNum = MOD(NxtNum, 10000)
        IF (NxtNxtNum < 10) THEN
            StrBuf(1:1) = Char1Digit(NxtNxtNum)
            StrBuf(2:5) = Char4Digits(NxtRemNum)
            StrBuf(6:9) = Char4Digits(RemNum)
            NumLen = 9
        ELSE
            StrBuf(1:2)  = Char2Digits(NxtNxtNum)
            StrBuf(3:6)  = Char4Digits(NxtRemNum)
            StrBuf(7:10) = Char4Digits(RemNum)
            NumLen = 10
        END IF
    END IF

    RETURN

END FUNCTION Write_AbsI32

!******************************************************************************

FUNCTION Write_AbsI32_1To4Digits(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a positive 32-bit integer number as a decimal string to the specified string buffer
    !  for Number < 10000.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,     INTENT(IN)     :: Number   !! number
    tCharLen(4), INTENT(INOUT)  :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Number < 100) THEN
        IF (Number < 10) THEN
            StrBuf(1:1) = Char1Digit(Number)
            NumLen = 1
        ELSE
            StrBuf(1:2) = Char2Digits(Number)
            NumLen = 2
        END IF
    ELSE
        IF (Number < 1000) THEN
            StrBuf(1:3) = Char4Digits(Number)(2:4)
            NumLen = 3
        ELSE
            StrBuf(1:4) = Char4Digits(Number)
            NumLen = 4
        END IF
    END IF

    RETURN

END FUNCTION Write_AbsI32_1To4Digits

!******************************************************************************

FUNCTION Write_AbsI32_5To8Digits(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a positive 32-bit integer number as a decimal string to the specified string buffer
    !  for 10000 <= Number < 100000000.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,     INTENT(IN)     :: Number   !! number
    tCharLen(8), INTENT(INOUT)  :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER :: ShiftPos = 40
    tSInt64, PARAMETER :: Multiplier = 109951163_kInt64
    tSInt32, PARAMETER :: Divisor = 10000
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: QtnNum, RemNum

!** FLOW

    QtnNum = ToInt32(SHIFTR(Number*Multiplier, ShiftPos))   ! QtnNum = Number/10000
    RemNum = Number - QtnNum*Divisor                        ! RemNum = MOD(Number, 10000)
    IF (Number < 1000000) THEN
        IF (Number < 100000) THEN
            StrBuf(1:1) = Char1Digit(QtnNum)
            StrBuf(2:5) = Char4Digits(RemNum)
            NumLen = 5
        ELSE
            StrBuf(1:2) = Char2Digits(QtnNum)
            StrBuf(3:6) = Char4Digits(RemNum)
            NumLen = 6
        END IF
    ELSE
        IF (Number < 10000000) THEN
            StrBuf(1:3) = Char4Digits(QtnNum)(2:4)
            StrBuf(4:7) = Char4Digits(RemNum)
            NumLen = 7
        ELSE
        StrBuf(1:4) = Char4Digits(QtnNum)
        StrBuf(5:8) = Char4Digits(RemNum)
        NumLen = 8
        END IF
    END IF

    RETURN

END FUNCTION Write_AbsI32_5To8Digits

!******************************************************************************

FUNCTION Write_AbsI32_8Digits(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a positive 32-bit integer number as a decimal string to the specified string buffer
    !  for 10000000 <= Number < 100000000.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,     INTENT(IN)     :: Number   !! number
    tCharLen(8), INTENT(INOUT)  :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER :: ShiftPos = 40
    tSInt64, PARAMETER :: Multiplier = 109951163_kInt64
    tSInt32, PARAMETER :: Divisor = 10000
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: QtnNum, RemNum

!** FLOW

    QtnNum = ToInt32(SHIFTR(Number*Multiplier, ShiftPos))   ! QtnNum = Number/10000
    RemNum = Number - QtnNum*Divisor                        ! RemNum = MOD(Number, 10000)
    StrBuf(1:4) = Char4Digits(QtnNum)
    StrBuf(5:8) = Char4Digits(RemNum)
    NumLen = 8

    RETURN

END FUNCTION Write_AbsI32_8Digits

!******************************************************************************

FUNCTION Write_AbsI32_9Digits(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a positive 32-bit integer number as a decimal string to the specified string buffer
    !  for 100000000 <= Number < 1000000000.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,     INTENT(IN)     :: Number   !! number
    tCharLen(9), INTENT(INOUT)  :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER :: S9 = 44
    tSInt64, PARAMETER :: M9 = ToInt64(Z'0000000068DB8BAD')
    tSInt32, PARAMETER :: S5 = 30
    tSInt64, PARAMETER :: M5 = ToInt64(Z'000000000001A36F')
    tSInt32, PARAMETER :: Divisor = 10000
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: ABBBB, CCCC, BBBB, A

!** FLOW

    ABBBB = ToInt32(SHIFTR(ToInt64(Number)*M9, S9))     ! ABBBB = Number/10000
    CCCC  = Number - ABBBB*10000                        ! CCCC  = MOD(Number, 10000)
    A     = ToInt32(SHIFTR(ToInt64(ABBBB)*M5, S5))      ! A = ABBBB/10000
    BBBB  = ABBBB  - A*10000                            ! BBBB  = MOD(ABBBB, 10000)
    ! write 9-digit number as ABBBBCCCC
    StrBuf(1:1) = Char1Digit(A)
    StrBuf(2:5) = Char4Digits(BBBB)
    StrBuf(6:9) = Char4Digits(CCCC)
    NumLen = 9

    RETURN

END FUNCTION Write_AbsI32_9Digits

!******************************************************************************

FUNCTION Write_AbsI64(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a positive 64-bit integer number as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64,      INTENT(IN)    :: Number   !! number
    tCharLen(19), INTENT(INOUT) :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! the divisor of 1.0E8
    tSInt64, PARAMETER  :: Div1E8 = 100000000_kInt64
    ! multiplier and shift for 19 digits and divisor of 1.0E8
    tSInt64, PARAMETER  :: M90 = ToInt64(Z'ABCC77118461CEFD')
    tSInt32, PARAMETER  :: S90 = 90 - 64
    ! multiplier for 11 digits and divisor of 1.0E8
    tSInt64, PARAMETER  :: M64 = ToInt64(Z'0000002AF31DC462')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: HiNum, LoNum, MidNum, HMNum

!** FLOW

    ! start the conversion
    IF (Number < ToInt64(MAX_INT32)) THEN
        ! +++ Number between 1 and 10 +++
        NumLen = Write_AbsI32(ToInt32(Number), StrBuf)
    ELSEIF (Number < 10000000000000000_kInt64) THEN
        ! +++ Number between 10 and 16 +++
        HiNum = SHIFTR(UMul128_Upper64(Number, M90), S90)   ! HiNum = Number/100000000
        LoNum = Number - HiNum*Div1E8                       ! LoNum = MOD(Number, 100000000)
        IF (HiNum < 10000_kInt64) THEN
            NumLen = Write_AbsI32_1To4Digits(ToInt32(HiNum), StrBuf)
        ELSE
            NumLen = Write_AbsI32_5To8Digits(ToInt32(HiNum), StrBuf)
        END IF
        NumLen = NumLen + Write_AbsI32_8Digits(ToInt32(LoNum), StrBuf(NumLen+1:))
    ELSE
        ! +++ Number between 17 and 19 +++
        HMNum  = SHIFTR(UMul128_Upper64(Number, M90), S90)  ! HMNum = Number/100000000
        LoNum  = Number - HMNum*Div1E8                      ! LoNum = MOD(Number, 100000000)
        HiNum  = UMul128_Upper64(HMNum, M64)                ! HiNum = HMNum/100000000
        MidNum = HMNum - HiNum*Div1E8                       ! MidNum = MOD(HMNum, 100000000)
        NumLen = Write_AbsI32_1To4Digits(ToInt32(HiNum), StrBuf)
        NumLen = NumLen + Write_AbsI32_8Digits(ToInt32(MidNum), StrBuf(NumLen+1:))
        NumLen = NumLen + Write_AbsI32_8Digits(ToInt32(LoNum), StrBuf(NumLen+1:))
    END IF

    RETURN

END FUNCTION Write_AbsI64

!******************************************************************************

FUNCTION Write_Sig128(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a (positive) 128-bit significand as a decimal string to the specified string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)       :: Number   !! number
    tCharLen(40),  INTENT(INOUT)    :: StrBuf   !! string buffer
    tSInt32                         :: NumLen   !! length of the written decimal string

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
    tUInt64         :: Hi, Lo
    tSInt32         :: HiHi, HiLo, LoHi, LoLo
    tSInt32         :: Pos

!** FLOW

    ! 'ToChar_...' routines perform digits extraction using 32-bit integers,
    ! provided that the arguments are limited to 9 digits.
    ! Therefore, split the H = 36 digits (or 35 digits if IsPlainWOLZ is true) of F into:
    !     HiHi = the most 8 or 9 significant digit of F
    !     HiLo = the next 9 most significant digits of F
    !     LoHi = the next 9 most significant digits of F
    !     LoLo = the last 9 least significant digits of F
    CALL DivModBy10Pow18(Number, Hi, Lo)
    HiHi = SHIFTR(UMul128_Upper64(Hi, M189), S189)  ! HiHi = Hi/TenPow9
    HiLo = Hi - HiHi*TenPow9                        ! HiLo = MOD(Hi, TenPow9)
    LoHi = SHIFTR(UMul128_Upper64(Lo, M189), S189)  ! HiHi = Hi/TenPow9
    LoLo = Lo - LoHi*TenPow9                        ! HiLo = MOD(Hi, TenPow9)

    ! write 35 or 36 digits
    IF (HiHi < 100000000) THEN
        Pos = Write_AbsI32_8Digits(HiHi, StrBuf)
    ELSE
        Pos = Write_AbsI32_9Digits(HiHi, StrBuf)
    END IF
    Pos = Pos + Write_AbsI32_9Digits(HiLo, StrBuf(Pos+1:))
    Pos = Pos + Write_AbsI32_9Digits(LoHi, StrBuf(Pos+1:))
    NumLen = Pos + Write_AbsI32_9Digits(LoLo, StrBuf(Pos+1:))

    RETURN

END FUNCTION Write_Sig128

!******************************************************************************

FUNCTION Write_FP_Exponent(Number, StrBuf) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a 32-bit integer number representing a decimal exponent of a floating
    !  point (real) number as a decimal string to the specified string buffer.  Its
    !  range is between -4966 and 4932 (i.e. for quadruple precision).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,     INTENT(IN)     :: Number   !! number
    tCharLen(6), INTENT(INOUT)  :: StrBuf   !! string buffer
    tSInt32                     :: NumLen   !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (Number >= 0_kInt32) THEN
        StrBuf(1:2) = 'E+'
        NumLen = Write_AbsI32_1To4Digits(Number, StrBuf(3:))
    ELSE
        StrBuf(1:2) = 'E-'
        NumLen = Write_AbsI32_1To4Digits((-Number), StrBuf(3:))
    END IF
    NumLen = NumLen + 2

    RETURN

END FUNCTION Write_FP_Exponent

!******************************************************************************

FUNCTION Write_RealSP_Shortest(Fp, Ep, StrBuf, IsScientific, KeepTrailZ, DEPos) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To format the decimal Fp*10**Ep for single-precision floating point number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,            INTENT(IN)      :: Fp           !! decimal significand
    tSInt32,            INTENT(IN)      :: Ep           !! decimal exponent
    tCharStar,          INTENT(INOUT)   :: StrBuf       !! character string
    tLogical, OPTIONAL, INTENT(IN)      :: IsScientific
    !^ format flag <br>
    ! - True  if to write the given number in scientific format. <br>
    ! - False if to write the given number in general format. <br>
    ! - Default is false.
    tLogical, OPTIONAL, INTENT(IN)      :: KeepTrailZ
    !^ If present and true, do not remove trailing zero(s).
    tSInt32,  OPTIONAL, INTENT(OUT)     :: DEPos(2)
    !^ positions where decimal point and exponent characters are written
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! shift and multiplier parameters (i.e. magic number) for integer division
    tSInt32, PARAMETER  :: S98   = 57
    tSInt64, PARAMETER  :: M98   = 1441151881_kInt64
    tSInt32, PARAMETER  :: S178  = 20                         ! = 84-64
    tSInt64, PARAMETER  :: M178  = 193428131138340668_kInt64
    tSInt64, PARAMETER  :: DivE8 = 100000000_kInt64
    ! Used for left-to-tight digit extraction.
    tSInt32, PARAMETER  :: MASK_28 = SHIFTL(1, 28) - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: F
    tSInt32     :: E, HF, LF
    tLogical    :: IsGeneral    ! true if to write the given number in general format 
    tLogical    :: RemoveTZ     ! true if to remove trailing zero(s)

!** FLOW

    ! check for special case
    IF (IsSpecialCase(Fp, Ep, FalseVal, StrBuf, NumLen)) RETURN

    ! convert the decimal significand and exponent
    CALL FP_Conversion(Fp, Ep, F, E)

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
    
    ! set variables relating optional input
    IsGeneral = TrueVal
    IF (PRESENT(IsScientific)) IsGeneral = .NOT.IsScientific
    RemoveTZ = TrueVal
    IF (PRESENT(KeepTrailZ)) RemoveTZ = .NOT.KeepTrailZ
    ! write output
    IF (IsGeneral) THEN
        IF ((0 < E).AND.(E <= 7)) THEN
            ! plain format without leading zeroes
            NumLen = ToChar_Plain_Without_LZ(HF, LF, E, RemoveTZ, StrBuf, DEPos)
        ELSEIF ((-3 < E).AND.(E <= 0)) THEN
            ! plain format with leading zeroes
            NumLen = ToChar_Plain_With_LZ(HF, LF, E, RemoveTZ, StrBuf, DEPos)
        ELSE
            ! scientific notation
            NumLen = ToChar_Scientific(HF, LF, E, RemoveTZ, StrBuf, DEPos)
        END IF
    ELSE
        ! scientific notation
        NumLen = ToChar_Scientific(HF, LF, E, RemoveTZ, StrBuf, DEPos)
    END IF

    RETURN
    
    CONTAINS

    FUNCTION ToChar_Plain_Without_LZ(H, L, E, NoTrailZ, StrBuf, DEPos) RESULT(NumLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For 0 < E <= 7, plain format without leading zeroes.
        ! Left-to-right digits extraction:
        ! algorithm 1 in [7], with b = 10, k = 8, n = 28.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tLogical,  INTENT(IN)       :: NoTrailZ ! true if to remove trailing zero(s)
        tCharStar, INTENT(INOUT)    :: StrBuf   ! character string
        tSInt32,   INTENT(OUT)      :: DEPos(2)
        OPTIONAL                    :: DEPos
        !^ positions where decimal point and exponent characters are written
        tSInt32                     :: NumLen   ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Y, T, I, Pos

    !** FLOW
    
        StrBuf(1:1) = Char1Digit(H)
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
            StrBuf(Pos:Pos) = Char1Digit(SHIFTR(T, 28))
            Pos = Pos + 1
            Y = IAND(T, MASK_28)
            I = I + 1
        END DO
        ! append period
        StrBuf(Pos:Pos) = '.'
        IF (PRESENT(DEPos)) DEPos(1) = Pos
        Pos = Pos + 1
        DO WHILE (I <= 8)
            T = 10*Y
            ! append digit
            StrBuf(Pos:Pos) = Char1Digit(SHIFTR(T, 28))
            Pos = Pos + 1
            Y = IAND(T, MASK_28)
            I = I + 1
        END DO
        ! set length
        NumLen = Pos - 1
        ! remove trailing zero(s) if needed
        IF (NoTrailZ) CALL RemoveTrailingZeros(StrBuf, NumLen)
        IF (PRESENT(DEPos)) DEPos(2) = NumLen

        RETURN

    END FUNCTION ToChar_Plain_Without_LZ

    !******************************************************************************

    FUNCTION ToChar_Plain_With_LZ(H, L, E, NoTrailZ, StrBuf, DEPos) RESULT(NumLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For -3 < E <= 0: plain format with leading zeroes.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tLogical,  INTENT(IN)       :: NoTrailZ ! true if to remove trailing zero(s)
        tCharStar, INTENT(INOUT)    :: StrBuf   ! character string
        tSInt32,   INTENT(OUT)      :: DEPos(2)
        OPTIONAL                    :: DEPos
        !^ positions where decimal point and exponent characters are written
        tSInt32                     :: NumLen   ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32    :: Y, T, I, Pos, wLen

    !** FLOW
    
        ! fill the first 4 characters
        StrBuf(1:4) = '0.00'
        IF (PRESENT(DEPos)) DEPos(1) = 2
        ! compute Pos
        Pos = 3 - E 
        ! append H
        StrBuf(Pos:Pos) = Char1Digit(H)
        Pos = Pos + 1
        ! append L and set length
        NumLen = Pos + Write_AbsI32_8Digits(L, StrBuf(Pos:)) - 1
        ! remove trailing zero(s) if needed
        IF (NoTrailZ) CALL RemoveTrailingZeros(StrBuf, NumLen)
        IF (PRESENT(DEPos)) DEPos(2) = NumLen

        RETURN

    END FUNCTION ToChar_Plain_With_LZ

    !******************************************************************************

    FUNCTION ToChar_Scientific(H, L, E, NoTrailZ, StrBuf, DEPos) RESULT(NumLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For E <= -3 or E > 7: computerized scientific notation.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tLogical,  INTENT(IN)       :: NoTrailZ ! true if to remove trailing zero(s)
        tCharStar, INTENT(INOUT)    :: StrBuf   ! character string
        tSInt32,   INTENT(OUT)      :: DEPos(2)
        OPTIONAL                    :: DEPos
        !^ positions where decimal point and exponent characters are written
        tSInt32                     :: NumLen   ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Y, T, I, Pos

    !** FLOW
    
        ! append H
        StrBuf(1:1) = Char1Digit(H)
        ! append period
        StrBuf(2:2) = '.'
        IF (PRESENT(DEPos)) DEPos(1) = 2
        ! append L
        Pos = Write_AbsI32_8Digits(L, StrBuf(3:)) + 2
        ! remove trailing zero(s) if needed
        IF (NoTrailZ) CALL RemoveTrailingZeros(StrBuf, Pos)
        IF (PRESENT(DEPos)) DEPos(2) = Pos
        ! append exponent
        NumLen = Pos + Write_FP_Exponent(E-1, StrBuf(Pos+1:)) - 1
    
        RETURN
    
    END FUNCTION ToChar_Scientific

    !******************************************************************************

END FUNCTION Write_RealSP_Shortest

!******************************************************************************

FUNCTION Write_RealDP_Shortest(Fp, Ep, StrBuf, IsScientific, KeepTrailZ, DEPos) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To format the decimal Fp*10**Ep for double-precision floating point number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64,            INTENT(IN)      :: Fp           !! decimal significand
    tSInt32,            INTENT(IN)      :: Ep           !! decimal exponent
    tCharStar,          INTENT(INOUT)   :: StrBuf       !! character string
    tLogical, OPTIONAL, INTENT(IN)      :: IsScientific
    !^ format flag <br>
    ! - True  if to write the given number in scientific format. <br>
    ! - False if to write the given number in general format. <br>
    ! - Default is false.
    tLogical, OPTIONAL, INTENT(IN)      :: KeepTrailZ
    !^ If present and true, do not remove trailing zero(s).
    tSInt32,  OPTIONAL, INTENT(OUT)     :: DEPos(2)
    !^ positions where decimal point and exponent characters are written
    tSInt32                             :: NumLen       !! length of the written decimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! shift and multiplier parameters (i.e. magic number) for integer division
    tSInt32, PARAMETER  :: S98   = 57
    tSInt64, PARAMETER  :: M98   = 1441151881_kInt64
    tSInt32, PARAMETER  :: S178  = 20                         ! = 84-64
    tSInt64, PARAMETER  :: M178  = 193428131138340668_kInt64
    tSInt64, PARAMETER  :: DivE8 = 100000000_kInt64
    ! Used for left-to-tight digit extraction.
    tSInt32, PARAMETER  :: MASK_28 = SHIFTL(1, 28) - 1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: F, HM
    tSInt32     :: E, HF, MF, LF
    tLogical    :: IsGeneral    ! true if to write the given number in general format 
    tLogical    :: RemoveTZ     ! true if to remove trailing zero(s)

!** FLOW

    ! check for special case
    IF (IsSpecialCase(Fp, Ep, FalseVal, StrBuf, NumLen)) RETURN

    ! convert the decimal significand and exponent
    CALL FP_Conversion(Fp, Ep, F, E)

    ! ToChars perform digits extraction using integers,
    ! provided that the arguments are limited to 8 digits.
    ! Therefore, split the H = 17 digits of F into:
    !     HF = the most significant digit of F
    !     MF = the next 8 most significant digits of F
    !     LF = the last 8, least significant digits of F
    !
    ! For N = 17, M = 8 the table in section 10 of [2] shows
    !     Floor(F/10**8) = Floor(193,428,131,138,340,668*F/2**84) =
    !     Floor(Floor(193,428,131,138,340,668*F/2**64) / 2**20)
    ! and for N = 9, M = 8
    !     Floor(HM/10**8) = Floor(1,441,151,881*HM/2**57)
    !
    HM = SHIFTR(UMul128_Upper64(F, M178), S178)
    LF = ToInt32(F - DivE8*HM)
    HF = ToInt32(SHIFTR(HM*M98, S98))
    MF = ToInt32(HM - DivE8*ToInt64(HF))

    ! set variables relating optional input
    IsGeneral = TrueVal
    IF (PRESENT(IsScientific)) IsGeneral = .NOT.IsScientific
    RemoveTZ = TrueVal
    IF (PRESENT(KeepTrailZ)) RemoveTZ = .NOT.KeepTrailZ
    ! write output
    IF (IsGeneral) THEN
        IF ((0 < E).AND.(E <= 7)) THEN
            ! plain format without leading zeroes
            NumLen = ToChar_Plain_Without_LZ(HF, MF, LF, E, RemoveTZ, StrBuf, DEPos)
        ELSEIF ((-3 < E).AND.(E <= 0)) THEN
            ! plain format with leading zeroes
            NumLen = ToChar_Plain_With_LZ(HF, MF, LF, E, RemoveTZ, StrBuf, DEPos)
        ELSE
            ! scientific notation
            NumLen = ToChar_Scientific(HF, MF, LF, E, RemoveTZ, StrBuf, DEPos)
        END IF
    ELSE
        ! scientific notation
        NumLen = ToChar_Scientific(HF, MF, LF, E, RemoveTZ, StrBuf, DEPos)
    END IF

    RETURN

    CONTAINS

    FUNCTION ToChar_Plain_Without_LZ(H, M, L, E, NoTrailZ, StrBuf, DEPos) RESULT(NumLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For 0 < E <= 7, plain format without leading zeroes.
        ! Left-to-right digits extraction:
        ! algorithm 1 in [7], with b = 10, k = 8, n = 28.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: M        ! middle digits
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tLogical,  INTENT(IN)       :: NoTrailZ ! true if to remove trailing zero(s)
        tCharStar, INTENT(INOUT)    :: StrBuf   ! character string
        tSInt32,   INTENT(OUT)      :: DEPos(2)
        OPTIONAL                    :: DEPos
        !^ positions where decimal point and exponent characters are written
        tSInt32                     :: NumLen   ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Y, T, I, Pos

    !** FLOW

        StrBuf(1:1) = Char1Digit(H)
        Pos = 2
        ! Algorithm 1 in [7] needs computation of floor((a + 1) 2^n / b^k) - 1
        ! with a < 10^8, b = 10, k = 8, n = 28.
        ! Noting that (a + 1) 2^n <= 10^8 2^28 < 10^17
        ! For n = 17, m = 8 the table in section 10 of [3] leads to:
        Y = ToInt32(SHIFTR(UMul128_Upper64(SHIFTL(ToInt64(M+1), 28), M178), S178)) - 1
        I = 1
        DO WHILE (I < E)
            T = 10*Y
            ! append digit
            StrBuf(Pos:Pos) = Char1Digit(SHIFTR(T, 28))
            Pos = Pos + 1
            Y = IAND(T, MASK_28)
            I = I + 1
        END DO
        ! append period
        StrBuf(Pos:Pos) = '.'
        IF (PRESENT(DEPos)) DEPos(1) = Pos
        Pos = Pos + 1
        DO WHILE (I <= 8)
            T = 10*Y
            ! append digit
            StrBuf(Pos:Pos) = Char1Digit(SHIFTR(T, 28))
            Pos = Pos + 1
            Y = IAND(T, MASK_28)
            I = I + 1
        END DO
        ! append L and set length
        NumLen = Pos + Write_AbsI32_8Digits(L, StrBuf(Pos:)) - 1
        ! remove trailing zero(s) if needed
        IF (NoTrailZ) CALL RemoveTrailingZeros(StrBuf, NumLen)
        IF (PRESENT(DEPos)) DEPos(2) = NumLen

        RETURN

    END FUNCTION ToChar_Plain_Without_LZ

    !**************************************************************************

    FUNCTION ToChar_Plain_With_LZ(H, M, L, E, NoTrailZ, StrBuf, DEPos) RESULT(NumLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For -3 < E <= 0: plain format with leading zeroes.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: M        ! middle digits
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tLogical,  INTENT(IN)       :: NoTrailZ ! true if to remove trailing zero(s)
        tCharStar, INTENT(INOUT)    :: StrBuf   ! character string
        tSInt32,   INTENT(OUT)      :: DEPos(2)
        OPTIONAL                    :: DEPos
        !^ positions where decimal point and exponent characters are written
        tSInt32                     :: NumLen   ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Y, T, I, Pos

    !** FLOW

        ! fill the first 4 characters
        StrBuf(1:4) = '0.00'
        IF (PRESENT(DEPos)) DEPos(1) = 2
        ! compute Pos
        Pos = 3 - E
        ! append H
        StrBuf(Pos:Pos) = Char1Digit(H)
        Pos = Pos + 1
        ! append M and L, and set length
        NumLen = Pos + Write_2I32_16Digits(M, L, StrBuf(Pos:)) - 1
        ! remove trailing zero(s) if needed
        IF (NoTrailZ) CALL RemoveTrailingZeros(StrBuf, NumLen)
        IF (PRESENT(DEPos)) DEPos(2) = NumLen

        RETURN

    END FUNCTION ToChar_Plain_With_LZ

    !**************************************************************************

    FUNCTION ToChar_Scientific(H, M, L, E, NoTrailZ, StrBuf, DEPos) RESULT(NumLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For E <= -3 or E > 7: computerized scientific notation.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: H        ! high digit
        tSInt32,   INTENT(IN)       :: M        ! middle digits
        tSInt32,   INTENT(IN)       :: L        ! low digits
        tSInt32,   INTENT(IN)       :: E        ! exponent
        tLogical,  INTENT(IN)       :: NoTrailZ ! true if to remove trailing zero(s)
        tCharStar, INTENT(INOUT)    :: StrBuf   ! character string
        tSInt32,   INTENT(OUT)      :: DEPos(2)
        OPTIONAL                    :: DEPos
        !^ positions where decimal point and exponent characters are written
        tSInt32                     :: NumLen   ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32    :: Y, T, I, Pos

    !** FLOW

        ! append H
        StrBuf(1:1) = Char1Digit(H)
        ! append period
        StrBuf(2:2) = '.'
        IF (PRESENT(DEPos)) DEPos(1) = 2
        ! append M and L
        Pos = 2 + Write_2I32_16Digits(M, L, StrBuf(3:))
        ! remove trailing zero(s) if needed
        IF (NoTrailZ) CALL RemoveTrailingZeros(StrBuf, Pos)
        IF (PRESENT(DEPos)) DEPos(2) = Pos
        ! append exponent and set length
        NumLen = Pos + Write_FP_Exponent(E-1, StrBuf(Pos+1:)) - 1

        RETURN

    END FUNCTION ToChar_Scientific

    !**************************************************************************

    FUNCTION Write_2I32_16Digits(FirstNum, SecondNum, StrBuf) RESULT(NumLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write two (unsigned) integer numbers with a length of 16 digits

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: FirstNum     ! first number
        tSInt32,   INTENT(IN)       :: SecondNum    ! first number
        tCharStar, INTENT(INOUT)    :: StrBuf       ! character string
        tSInt32                     :: NumLen       ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        NumLen = Write_AbsI32_8Digits(FirstNum, StrBuf(1:8)) + &
                 Write_AbsI32_8Digits(SecondNum, StrBuf(9:16))

        RETURN

    END FUNCTION Write_2I32_16Digits

    !**************************************************************************

END FUNCTION Write_RealDP_Shortest

!******************************************************************************

FUNCTION Write_RealQP_Shortest(Fp, Ep, StrBuf, IsScientific, KeepTrailZ, DEPos) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To format the decimal Fp*10**Ep for quadruple-precision floating point number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128),      INTENT(IN)      :: Fp           !! decimal significand
    tSInt32,            INTENT(IN)      :: Ep           !! decimal exponent
    tCharStar,          INTENT(INOUT)   :: StrBuf       !! character string
    tLogical, OPTIONAL, INTENT(IN)      :: IsScientific
    !^ format flag <br>
    ! - True  if to write the given number in scientific format. <br>
    ! - False if to write the given number in general format. <br>
    ! - Default is false.
    tLogical, OPTIONAL, INTENT(IN)      :: KeepTrailZ
    !^ If present and true, do not remove trailing zero(s).
    tSInt32,  OPTIONAL, INTENT(OUT)     :: DEPos(2)
    !^ positions where decimal point and exponent characters are written
    tSInt32                             :: NumLen       !! length of the written decimal string

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
    ! maximum number of significant digits (i.e. the decimal precision that guarantees
    !   an error-free write-read cycle.)
    tSInt32,  PARAMETER :: H = 36

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)   :: F
    tUInt64         :: Hi, Lo
    tSInt32         :: E, HiHi, HiLo, LoHi, LoLo
    tLogical        :: IsGeneral    ! true if to write the given number in general format 
    tLogical        :: RemoveTZ     ! true if to remove trailing zero(s)
    tLogical        :: IsPlainWOLZ  ! true if to write the number in plain format without leading zeroes

!** FLOW

    ! check for special case
    IF (IsSpecialCase(Fp, Ep, FalseVal, StrBuf, NumLen)) RETURN

    ! convert the decimal significand and exponent
    CALL FP_Conversion(Fp, Ep, F, E)
    NumLen = E - Ep

    ! set variables relating optional input
    IsGeneral = TrueVal
    IF (PRESENT(IsScientific)) IsGeneral = .NOT.IsScientific
    RemoveTZ = TrueVal
    IF (PRESENT(KeepTrailZ)) RemoveTZ = .NOT.KeepTrailZ

    ! set Format flag
    IF ((0 < E).AND.(E <= 7).AND.(NumLen < 36).AND.(IsGeneral)) THEN
        IsPlainWOLZ = TrueVal
    ELSE
        IsPlainWOLZ = FalseVal
    END IF

    ! check if need to compute new F
    IF (NumLen > 0) THEN
        ! 'ToChar_Plain_Without_LZ' only handles 35 digits
        IF (IsPlainWOLZ) F = Fp*PowTen(H - NumLen - 1)
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
    HiHi = SHIFTR(UMul128_Upper64(Hi, M189), S189)  ! HiHi = Hi/TenPow9
    HiLo = Hi - HiHi*TenPow9                        ! HiLo = MOD(Hi, TenPow9)
    LoHi = SHIFTR(UMul128_Upper64(Lo, M189), S189)  ! HiHi = Hi/TenPow9
    LoLo = Lo - LoHi*TenPow9                        ! HiLo = MOD(Hi, TenPow9)

    ! write output
    IF (IsGeneral) THEN
        IF (IsPlainWOLZ) THEN
            ! plain format without leading zeroes
            NumLen = ToChar_Plain_Without_LZ(HiHi, HiLo, LoHi, LoLo, E, RemoveTZ, StrBuf, DEPos)
        ELSEIF ((-3 < E).AND.(E <= 0)) THEN
            ! plain format with leading zeroes
            NumLen = ToChar_Plain_With_LZ(HiHi, HiLo, LoHi, LoLo, E, RemoveTZ, StrBuf, DEPos)
        ELSE
            ! scientific notation
            NumLen = ToChar_Scientific(HiHi, HiLo, LoHi, LoLo, E, RemoveTZ, StrBuf, DEPos)
        END IF
    ELSE
        ! scientific notation
        NumLen = ToChar_Scientific(HiHi, HiLo, LoHi, LoLo, E, RemoveTZ, StrBuf, DEPos)
    END IF

    RETURN

    CONTAINS

    FUNCTION ToChar_Plain_Without_LZ(HH, HL, LH, LL, E, NoTrailZ, StrBuf, DEPos) RESULT(NumLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For 0 < E <= 7, plain format without leading zeroes.
        ! Left-to-right digits extraction:
        ! algorithm 1 in [7], with b = 10, k = 8, n = 28.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: HH, HL, LH, LL   ! components of significand
        tSInt32,   INTENT(IN)       :: E                ! exponent
        tLogical,  INTENT(IN)       :: NoTrailZ         ! true if to remove trailing zero(s)
        tCharStar, INTENT(INOUT)    :: StrBuf           ! character string
        tSInt32,   INTENT(OUT)      :: DEPos(2)
        OPTIONAL                    :: DEPos
        !^ positions where decimal point and exponent characters are written
        tSInt32                     :: NumLen           ! length of string written

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
            StrBuf(Pos:Pos) = Char1Digit(SHIFTR(T, 28))
            Pos = Pos + 1
            Y = IAND(T, MASK_28)
            I = I + 1
        END DO
        ! append period
        StrBuf(Pos:Pos) = '.'
        IF (PRESENT(DEPos)) DEPos(1) = Pos
        Pos = Pos + 1
        DO WHILE (I < 8)
            T = 10*Y
            ! append digit
            StrBuf(Pos:Pos) = Char1Digit(SHIFTR(T, 28))
            Pos = Pos + 1
            Y = IAND(T, MASK_28)
            I = I + 1
        END DO
        ! append HL
        Pos = Pos + Write_AbsI32_9Digits(HL, StrBuf(Pos:))
        ! append LH and LL, and set length
        NumLen = Pos + Write_2I32_18Digits(LH, LL, StrBuf(Pos:)) - 1
        ! remove trailing zero(s) if needed
        IF (NoTrailZ) CALL RemoveTrailingZeros(StrBuf, NumLen)
        IF (PRESENT(DEPos)) DEPos(2) = NumLen

        RETURN

    END FUNCTION ToChar_Plain_Without_LZ

    !******************************************************************************

    FUNCTION ToChar_Plain_With_LZ(HH, HL, LH, LL, E, NoTrailZ, StrBuf, DEPos) RESULT(NumLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For -3 < E <= 0: plain format with leading zeroes.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: HH, HL, LH, LL   ! components of significand
        tSInt32,   INTENT(IN)       :: E                ! exponent
        tLogical,  INTENT(IN)       :: NoTrailZ         ! true if to remove trailing zero(s)
        tCharStar, INTENT(INOUT)    :: StrBuf           ! character string
        tSInt32,   INTENT(OUT)      :: DEPos(2)
        OPTIONAL                    :: DEPos
        !^ positions where decimal point and exponent characters are written
        tSInt32                     :: NumLen           ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Pos

    !** FLOW

        ! fill the first 4 characters
        StrBuf(1:4) = '0.00'
        IF (PRESENT(DEPos)) DEPos(1) = 2
        ! compute Pos
        Pos = 3 - E
        ! append HH and HL
        Pos = Pos + Write_2I32_18Digits(HH, HL, StrBuf(Pos:))
        ! append LH and LL, and set length
        NumLen = Pos + Write_2I32_18Digits(LH, LL, StrBuf(Pos:)) - 1
        ! remove trailing zero(s) if needed
        IF (NoTrailZ) CALL RemoveTrailingZeros(StrBuf, NumLen)
        IF (PRESENT(DEPos)) DEPos(2) = NumLen

        RETURN

    END FUNCTION ToChar_Plain_With_LZ

    !******************************************************************************

    FUNCTION ToChar_Scientific(HH, HL, LH, LL, E, NoTrailZ, StrBuf, DEPos) RESULT(NumLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! For E <= -3 or E > 7: computerized scientific notation.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: HH, HL, LH, LL   ! components of significand
        tSInt32,   INTENT(IN)       :: E                ! exponent
        tLogical,  INTENT(IN)       :: NoTrailZ         ! true if to remove trailing zero(s)
        tCharStar, INTENT(INOUT)    :: StrBuf           ! character string
        tSInt32,   INTENT(OUT)      :: DEPos(2)
        OPTIONAL                    :: DEPos
        !^ positions where decimal point and exponent characters are written
        tSInt32                     :: NumLen           ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Pos, HH_Hi, HH_Lo

    !** FLOW

        ! handle HH by splitting it into HH_Hi with 1 digit and HH_Lo with 8 digits
        HH_Hi = HH/100000000
        HH_Lo = HH - HH_Hi*100000000
        ! append HH_Hi
        StrBuf(1:1) = Char1Digit(HH_Hi)
        ! append period
        StrBuf(2:2) = '.'
        IF (PRESENT(DEPos)) DEPos(1) = 2
        ! append HH_Lo and HL
        Pos = 3 + Write_2I32_17Digits(HH_Lo, HL, StrBuf(3:))
        ! append LH and LL
        Pos = Pos + Write_2I32_18Digits(LH, LL, StrBuf(Pos:)) - 1
        ! remove trailing zero(s) if needed
        IF (NoTrailZ) CALL RemoveTrailingZeros(StrBuf, Pos)
        IF (PRESENT(DEPos)) DEPos(2) = Pos
        ! append exponent and set length
        NumLen = Pos + Write_FP_Exponent(E-1, StrBuf(Pos+1:)) - 1

        RETURN

    END FUNCTION ToChar_Scientific

    !******************************************************************************

    FUNCTION Write_2I32_18Digits(Hi, Lo, StrBuf) RESULT(NumLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write two integer numbers with a total length of 18 digits

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32,   INTENT(IN)   :: Hi, Lo
        tCharStar, INTENT(OUT)  :: StrBuf     ! character string
        tSInt32                 :: NumLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! NA

    !** FLOW

        NumLen = Write_AbsI32_9Digits(Hi, StrBuf(1:9)) + &
                 Write_AbsI32_9Digits(Lo, StrBuf(10:18))

        RETURN

    END FUNCTION Write_2I32_18Digits

    !******************************************************************************

    FUNCTION Write_2I32_17Digits(Hi, Lo, StrBuf) RESULT(NumLen)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write two integer numbers with a total length of 17 digits

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32,   INTENT(IN)   :: Hi, Lo
        tCharStar, INTENT(OUT)  :: StrBuf     ! character string
        tSInt32                 :: NumLen     ! length of string written

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        NumLen = Write_AbsI32_8Digits(Hi, StrBuf(1:8)) + &
                 Write_AbsI32_9Digits(Lo, StrBuf(9:17))

        RETURN

    END FUNCTION Write_2I32_17Digits

    !******************************************************************************

END FUNCTION Write_RealQP_Shortest

!******************************************************************************

FUNCTION Write_Real_Generic(SigStr, SigLen, Exp, StrBuf, Fmt, Width, Digits) RESULT(NumLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write a real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,         INTENT(IN)       :: SigStr   !! string representing decimal significand
    tSInt32,           INTENT(IN)       :: SigLen   !! length of SigStr
    tSInt32,           INTENT(IN)       :: Exp      !! decimal exponent
    tCharStar,         INTENT(INOUT)    :: StrBuf   !! character string
    tChar,   OPTIONAL, INTENT(IN)       :: Fmt
    !^ format (F, E or G); if not present, format = 'G'
    tSInt32, OPTIONAL, INTENT(IN)       :: Width
    !^ (maximum) length of string to be written to; if not present, Width = LEN(StrBuf)
    tSInt32, OPTIONAL, INTENT(IN)       :: Digits
    !^ (maximum) number of digits after the decimal point; if not present, use shortest representation
    tSInt32                             :: NumLen   !! length of the written decimal string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: AbsExp, DOld
    tSInt32     :: W, D, Pos, Indx
    tLogical    :: IsFixed

!** FLOW

    ! initialize
    W = LEN(StrBuf)
    D = 0
    IsFixed = TrueVal
    AbsExp = ABS(Exp)
    
    ! handle optional input
    IF (PRESENT(Fmt)) THEN
        IF ((Fmt == 'E').OR.(Fmt == 'e')) IsFixed = FalseVal
    END IF
    IF (PRESENT(Width)) THEN
        IF ((Width > 0).AND.(Width < W)) W = Width
    END IF
    IF (PRESENT(Digits)) THEN
        IF (IsFixed) THEN
            IF ((Digits > 0).AND.((Digits+2) < W)) D = Digits
        ELSE
            IF (AbsExp < 100) THEN
                IF ((Digits > 0).AND.((Digits+6) < W)) D = Digits
            ELSEIF (AbsExp < 1000) THEN
                IF ((Digits > 0).AND.((Digits+7) < W)) D = Digits
            ELSE
                IF ((Digits > 0).AND.((Digits+8) < W)) D = Digits
            END IF
        END IF
    END IF

    ! check whether if Fixed format is really appropriate
    IF (IsFixed) THEN
        IF (Exp > W-2) THEN
            IsFixed = FalseVal
        ELSEIF (Exp < 0) THEN
            DOld = D
            IF (D == 0) D = SigLen
            D = MIN(D, W-2)
            IF (AbsExp > D-1) THEN
                IsFixed = FalseVal
                D = DOld
            END IF
        END IF
    END IF

    ! now handle the suitable format
    IF (IsFixed) THEN
        !+++ fixed format +++
        IF (Exp <= 0) THEN
            !+++ write plain format with leading zeros +++
            StrBuf(1:2) = '0.'
            Pos = 3
            ! write leading zeros
            DO WHILE (AbsExp > 0)
                StrBuf(Pos:Pos) = '0'
                Pos = Pos + 1
                AbsExp = AbsExp - 1
            END DO
            ! get the number of digits left to be written
            D = D - (Pos-3)
            IF (D > SigLen) D = SigLen
            ! write significant digits excluding the last one
            Indx = 1
            DO WHILE ((Pos < W).AND.(Indx < D))
                StrBuf(Pos:Pos) = SigStr(Indx:Indx)
                Pos = Pos + 1
                Indx = Indx + 1
            END DO
            ! write the last digit while handling rounding problem
            NumLen = Pos
            CALL WriteLastDigit(SigStr, StrBuf, SigLen, NumLen, Indx)
        ELSEIF (Exp >= SigLen) THEN
            ! +++ write plain format without leading zeros +++
            StrBuf(1:SigLen) = SigStr(1:SigLen)
            AbsExp = Exp - SigLen
            Pos = SigLen + 1
            DO WHILE (AbsExp > 0)
                StrBuf(Pos:Pos) = '0'
                Pos = Pos + 1
                AbsExp = AbsExp - 1
            END DO
            StrBuf(Pos:Pos+1) = '.0'
            NumLen = Pos + 1
        ELSE
            ! +++ write plain format without leading zeros +++
            IF (Exp < SigLen-1) THEN
                IF (D == 0) D = SigLen - Exp
                StrBuf(1:Exp) = SigStr(1:)
                Pos = Exp + 1
                StrBuf(Pos:Pos) = '.'
                IF (D > (SigLen-Exp)) D = SigLen - Exp
                NumLen = Pos + D
                Indx = Exp + D - 1
                StrBuf(Pos+1:NumLen-1) = SigStr(Pos:Indx)
                ! write the last digit while handling rounding problem
                CALL WriteLastDigit(SigStr, StrBuf, SigLen, NumLen, Indx)
            ELSE
                ! Exp == SigLen-1 exactly
                StrBuf(1:Exp) = SigStr(1:)
                Pos = Exp + 1
                StrBuf(Pos:Pos) = '.'
                NumLen = Pos + 1
                StrBuf(NumLen:NumLen) = SigStr(SigLen:SigLen)
            END IF
        END IF
    ELSE
        !+++ scientific format +++
        ! limit D to appropriate value
        IF ((D == 0).OR.(D > SigLen-1)) D = SigLen - 1
        IF (AbsExp < 100) THEN
            D = MIN(D, W-6)
        ELSEIF (AbsExp < 1000) THEN
            D = MIN(D, W-7)
        ELSE
            D = MIN(D, W-8)
        END IF
        StrBuf(1:1) = SigStr(1:1)
        StrBuf(2:2) = '.'
        ! write significant digits excluding the last one
        Pos  = 3
        Indx = 1
        DO WHILE (Indx < D)
            StrBuf(Pos:Pos) = SigStr(Indx:Indx)
            Pos  = Pos + 1
            Indx = Indx + 1
        END DO
        ! write the last digit (of significand) while handling rounding problem
        CALL WriteLastDigit(SigStr, StrBuf, SigLen, Pos, Indx)
        ! append exponent and set length
        NumLen = Pos + Write_FP_Exponent(Exp-1, StrBuf(Pos+1:)) - 1
    END IF

    RETURN

CONTAINS

    SUBROUTINE WriteLastDigit(SigStr, StrBuf, SigLen, bPos, sPos)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To write the last digit while handling rounding problem.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, INTENT(IN)       :: SigStr
        tCharStar, INTENT(INOUT)    :: StrBuf
        tSInt32,   INTENT(IN)       :: SigLen, bPos, sPos

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tChar       :: Last, Next
        tLogical    :: RoundUp

    !** FLOW

        ! write the last digit
        IF (sPos == SigLen) THEN
            ! write the last one without rounding because
            ! this is the shortest representation
            StrBuf(bPos:bPos) = SigStr(sPos:sPos)
        ELSE
            ! need to check whether to round up the last digit or not
            Last = SigStr(sPos:sPos)
            Next = SigStr(sPos+1:sPos+1)
            IF (Next < '5') THEN
                ! do not round up
                RoundUp = FalseVal
            ELSEIF (Next > '5') THEN
                ! round up
                RoundUp = TrueVal
            ELSE
                IF (sPos+2 > SigLen) THEN
                    ! round nearest ties to even
                    IF (IAND(IACHAR(Last), 1) == 0) THEN
                        ! Last is even number
                        RoundUp = FalseVal
                    ELSE
                        ! Last is odd number
                        RoundUp = TrueVal
                    END IF
                ELSE
                    IF (SigStr(sPos+2:sPos+2) > '0') THEN
                        ! round up
                        RoundUp = TrueVal
                    ELSE
                        ! round nearest ties to even
                        IF (IAND(IACHAR(Last), 1) == 0) THEN
                            ! Last is even number
                            RoundUp = FalseVal
                        ELSE
                            ! Last is odd number
                            RoundUp = TrueVal
                        END IF
                    END IF
                END IF
            END IF
            ! write the last digit
            IF (RoundUp) THEN
                StrBuf(bPos:bPos) = ACHAR(IACHAR(Last)+1)
            ELSE
                StrBuf(bPos:bPos) = Last
            END IF
        END IF

        RETURN

    END SUBROUTINE WriteLastDigit

    !**************************************************************************

END FUNCTION Write_Real_Generic

!******************************************************************************

FUNCTION IsSpecialCase_RealSP(Fp, Ep, Negative, StrBuf, NumLen) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified floating point number is zero, NaN or Infinity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32,   INTENT(IN)       :: Fp       !! decimal significand
    tSInt32,   INTENT(IN)       :: Ep       !! decimal exponent
    tLogical,  INTENT(IN)       :: Negative !! true if the number is negative
    tCharStar, INTENT(INOUT)    :: StrBuf   !! character string
    tSInt32,   INTENT(INOUT)    :: NumLen   !! number of characters written
    tLogical                    :: Flag     !! true if the given number represents a special case

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Pos

!** FLOW

    Flag = FalseVal
    IF (Ep == ExceptionalExponent) THEN
        ! either NAN or Infinity
        Pos  = 0
        IF (Negative) THEN
            Pos = 1
            StrBuf(Pos:Pos) = '-'
        END IF
        IF (Fp /= 0_kInt32) THEN
            StrBuf(Pos+1:Pos+3) = 'NAN'
            NumLen = Pos + 3
        ELSE
            StrBuf(Pos+1:Pos+8) = 'INFINITY'
            NumLen = Pos + 8
        END IF
        Flag = TrueVal
    ELSEIF (Fp == 0_kInt32) THEN
        ! zero
        Pos  = 0
        IF (Negative) THEN
            Pos = 1
            StrBuf(Pos:Pos) = '-'
        END IF
        StrBuf(Pos+1:Pos+3) = '0.0'
        NumLen = Pos + 3
        Flag = TrueVal
    END IF

    RETURN

END FUNCTION IsSpecialCase_RealSP

!******************************************************************************

FUNCTION IsSpecialCase_RealDP(Fp, Ep, Negative, StrBuf, NumLen) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified floating point number is zero, NaN or Infinity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64,   INTENT(IN)       :: Fp       !! decimal significand
    tSInt32,   INTENT(IN)       :: Ep       !! decimal exponent
    tLogical,  INTENT(IN)       :: Negative !! true if the number is negative
    tCharStar, INTENT(INOUT)    :: StrBuf   !! character string
    tSInt32,   INTENT(INOUT)    :: NumLen   !! number of characters written
    tLogical                    :: Flag     !! true if the given number represents a special case

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Pos

!** FLOW

    Flag = FalseVal
    IF (Ep == ExceptionalExponent) THEN
        ! either NAN or Infinity
        Pos  = 0
        IF (Negative) THEN
            Pos = 1
            StrBuf(Pos:Pos) = '-'
        END IF
        IF (Fp /= 0_kInt64) THEN
            StrBuf(Pos+1:Pos+3) = 'NAN'
            NumLen = Pos + 3
        ELSE
            StrBuf(Pos+1:Pos+8) = 'INFINITY'
            NumLen = Pos + 8
        END IF
        Flag = TrueVal
    ELSEIF (Fp == 0_kInt64) THEN
        ! zero
        Pos  = 0
        IF (Negative) THEN
            Pos = 1
            StrBuf(Pos:Pos) = '-'
        END IF
        StrBuf(Pos+1:Pos+3) = '0.0'
        NumLen = Pos + 3
        Flag = TrueVal
    END IF

    RETURN

END FUNCTION IsSpecialCase_RealDP

!******************************************************************************

FUNCTION IsSpecialCase_RealQP(Fp, Ep, Negative, StrBuf, NumLen) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified floating point number is zero, NaN or Infinity.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)       :: Fp       !! decimal significand
    tSInt32,       INTENT(IN)       :: Ep       !! decimal exponent
    tLogical,      INTENT(IN)       :: Negative !! true if the number is negative
    tCharStar,     INTENT(INOUT)    :: StrBuf   !! character string
    tSInt32,       INTENT(INOUT)    :: NumLen   !! number of characters written
    tLogical                        :: Flag     !! true if the given number represents a special case

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Pos

!** FLOW

    Flag = FalseVal
    IF (Ep == ExceptionalExponent) THEN
        ! either NAN or Infinity
        Pos  = 0
        IF (Negative) THEN
            Pos = 1
            StrBuf(Pos:Pos) = '-'
        END IF
        IF (Fp /= ZeroU128) THEN
            StrBuf(Pos+1:Pos+3) = 'NAN'
            NumLen = Pos + 3
        ELSE
            StrBuf(Pos+1:Pos+8) = 'INFINITY'
            NumLen = Pos + 8
        END IF
        Flag = TrueVal
    ELSEIF (Fp == ZeroU128) THEN
        ! zero
        Pos  = 0
        IF (Negative) THEN
            Pos = 1
            StrBuf(Pos:Pos) = '-'
        END IF
        StrBuf(Pos+1:Pos+3) = '0.0'
        NumLen = Pos + 3
        Flag = TrueVal
    END IF

    RETURN

END FUNCTION IsSpecialCase_RealQP

!******************************************************************************

SUBROUTINE FP32_Conversion(Fp, Ep, F, E)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the specified significand and exponent to the form
    !  suitable for writing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN)     :: Fp   !! decimal significand
    tSInt32, INTENT(IN)     :: Ep   !! decimal exponent
    tSInt32, INTENT(OUT)    :: F
    tSInt32, INTENT(OUT)    :: E

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! maximum number of significant digits (i.e. the decimal precision that guarantees
    !   an error-free write-read cycle.)
    tSInt32, PARAMETER  :: H = 9
    ! The first powers of 10. The last entry must be 10^H.
    tSInt32             :: I
    tSInt32, PARAMETER  :: Pow10(0:H) = [(10**I, I = 0, H)]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: sLen

!** FLOW

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

    RETURN

END SUBROUTINE FP32_Conversion

!******************************************************************************

SUBROUTINE FP64_Conversion(Fp, Ep, F, E)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the specified significand and exponent to the form
    !  suitable for writing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN)     :: Fp   !! decimal significand
    tSInt32, INTENT(IN)     :: Ep   !! decimal exponent
    tSInt64, INTENT(OUT)    :: F
    tSInt32, INTENT(OUT)    :: E

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! maximum number of significant digits (i.e. the decimal precision that guarantees
    !   an error-free write-read cycle.)
    tSInt32, PARAMETER  :: H = 17
    ! The first powers of 10. The last entry must be 10^H.
    tSInt32             :: I
    tSInt64, PARAMETER  :: Pow10(0:H) = [(10_kInt64**I, I = 0, H)]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: sLen

!** FLOW

    ! For details not discussed here see section 10 of [3].
    ! Determine sLen such that 10**(sLen-1) <= F < 10**sLen
    sLen = Floor_Log10_Pow2(64 - LEADZ(Fp))
    IF (Fp >= Pow10(sLen)) sLen = sLen + 1
    
    ! Let Fp and Ep be the original F and E, respectively.
    ! Transform F and E to ensure
    !    10**(H-1) <= F < 10**H
    !    Fp*10**Ep = F*10**(E-H) = 0.F*10**E
    F = Fp*Pow10(H - sLen)
    E = Ep + sLen

    RETURN

END SUBROUTINE FP64_Conversion

!******************************************************************************

SUBROUTINE FP128_Conversion(Fp, Ep, F, E)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the specified significand and exponent to the form
    !  suitable for writing.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)   :: Fp   !! decimal significand
    tSInt32,       INTENT(IN)   :: Ep   !! decimal exponent
    TYPE(UInt128), INTENT(OUT)  :: F
    tSInt32,       INTENT(OUT)  :: E

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! maximum number of significant digits (i.e. the decimal precision that guarantees
    !   an error-free write-read cycle.)
    tSInt32,  PARAMETER :: H = 36

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: sLen

!** FLOW

    ! For details not discussed here see section 10 of [3].
    ! Determine sLen such that 10**(sLen-1) <= F < 10**sLen
    sLen = Floor_Log10_Pow2(128 - LEADZ(Fp))
    IF (Fp .UGE. PowTen(sLen)) sLen = sLen + 1

    ! Let Fp and Ep be the original F and E, respectively.
    ! Transform F and E to ensure
    !    10**(H-1) <= F < 10**H
    !    Fp*10**Ep = F*10**(E-H) = 0.F*10**E
    F = Fp*PowTen(H - sLen)
    E = Ep + sLen

    RETURN

END SUBROUTINE FP128_Conversion

!******************************************************************************

SUBROUTINE DivModBy10Pow18(Dividend, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform division Dividend / Divisor where the Divisor is equal to 10**18

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(UInt128), INTENT(IN)   :: Dividend     ! the dividend
    tUInt64                     :: Quotient     ! the quotient
    tUInt64                     :: Remainder    ! the remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32, PARAMETER  :: LSh   = 4                            ! = LEADZ(Divisor)
    tUInt64, PARAMETER  :: Denom = ToInt64(Z'DE0B6B3A76400000') ! = SHIFTL(Divisor, LSh)
    tUInt64, PARAMETER  :: V     = ToInt64(Z'2725DD1D243ABA0E') ! = Reciprocal_2By1(Denom)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32         :: RSh
    tUInt64         :: NumerHi, NumerLo
    tUInt64         :: NumerEx, RshMask, QuotHi
    tUInt64         :: R1, R2

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

!******************************************************************************

FUNCTION Floor_Log10_Pow2(E) RESULT(K)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute K = FLOOR(LOG10(2**E)) where -5456721 <= E <= 5456721

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: E    ! base-2 exponent
    tSInt32             :: K    ! base-10 exponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! Multiplier = FLOOR(LOG10(2) * 2**Shift)
    tSInt32, PARAMETER  :: Shift      = 41
    tSInt64, PARAMETER  :: Multiplier = 661971961083_kInt64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    K = ToInt32(SHIFTA(ToInt64(E)*Multiplier, Shift))
    
    RETURN
    
END FUNCTION Floor_Log10_Pow2

!******************************************************************************

SUBROUTINE RemoveTrailingZeros(StrBuf, Pos)

!** PURPOSE OF THIS SUBROUTINE:
    ! To remove the trailing zero(s) from the string buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(INOUT)    :: StrBuf   ! character string
    tSInt32,   INTENT(INOUT)    :: Pos
    !^ On entry, the last index into the buffer to be check for zero(s).
    !^ On exit, the last index into the buffer where trailing zero(s) removed.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW


    ! remove trailing zero(s)
    DO WHILE (StrBuf(Pos:Pos) == '0')
        Pos = Pos - 1
    END DO
    ! ... but do not remove the one directly to the right of '.'
    IF (StrBuf(Pos:Pos) == '.') Pos = Pos + 1

    RETURN

END SUBROUTINE RemoveTrailingZeros

!******************************************************************************

END MODULE MBase_WriteUtil

!******************************************************************************
