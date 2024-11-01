
MODULE MBase_ReadUtil

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various utility routines relating to reading from character strings
!   (i.e. parsing the string and return appropriate value).

!** USE STATEMENTS:
    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE ISO_C_BINDING,  ONLY: C_LOC, C_F_POINTER, C_PTR, C_NULL_PTR
    USE MBase_Common
    USE MBase_CharUtil
    USE MBase_FloatUtil
    USE MBase_UIntUtil
    USE MBase_SInt128
    USE MBase_UInt128
    USE MBase_BinDec32
    USE MBase_BinDec64
    USE MBase_BinDec128

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! main procedures
    PUBLIC :: ParseNumber
    ! auxiliary procedures
    PUBLIC :: SkipChars
    PUBLIC :: SkipSpaces
    PUBLIC :: Skip2NextLine
    PUBLIC :: SkipAllWhiteSpaces
    PUBLIC :: SkipSeparators
    PUBLIC :: SkipDecimalDigits
    ! parameters
    PUBLIC :: D2B_FastFloat
    PUBLIC :: D2B_Lemire
    PUBLIC :: D2B_LibC
    PUBLIC :: D2B_YY

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! length for I/O message
    tSInt32, PARAMETER  :: MsgLen = 128
    ! minimum and maximum integer values
    tSInt32, PARAMETER  :: MIN_INT32 = ToInt32(Z'80000000')         ! -2147483648
    tSInt32, PARAMETER  :: MAX_INT32 = ToInt32(Z'7FFFFFF')          ! +2147483647
    tSInt64, PARAMETER  :: MIN_INT64 = ToInt64(Z'8000000000000000') ! -9223372036854775808
    tSInt64, PARAMETER  :: MAX_INT64 = ToInt64(Z'7FFFFFFFFFFFFFFF') ! +9223372036854775807
    ! Parameters for decimal-to-binary algorithm
    tSInt32, PARAMETER  :: D2B_FastFloat = 1    !! FastFloat algorithm
    tSInt32, PARAMETER  :: D2B_Lemire    = 2    !! Lemire algorithm
    tSInt32, PARAMETER  :: D2B_LibC      = 3    !! LibC algorithm
    tSInt32, PARAMETER  :: D2B_YY        = 4    !! YY algorithm
    ! types of number
    tSInt32, PARAMETER  :: TYPE_INF_NEGATIVE         = -3
    tSInt32, PARAMETER  :: TYPE_INF_POSITIVE         = -2
    tSInt32, PARAMETER  :: TYPE_NAN_EXACT            = -1
    tSInt32, PARAMETER  :: TYPE_NAN_INVALID          = 0
    tSInt32, PARAMETER  :: TYPE_INTEGER_ZERO         = 1
    tSInt32, PARAMETER  :: TYPE_REAL_ZERO            = 2
    tSInt32, PARAMETER  :: TYPE_CMPX_ZERO            = 3
    tSInt32, PARAMETER  :: TYPE_INT8_LIKELY          = 4
    tSInt32, PARAMETER  :: TYPE_INT16_LIKELY         = 5
    tSInt32, PARAMETER  :: TYPE_INT32_LIKELY         = 6
    tSInt32, PARAMETER  :: TYPE_INT64_LIKELY         = 7
    tSInt32, PARAMETER  :: TYPE_INT128_LIKELY        = 8
    tSInt32, PARAMETER  :: TYPE_APINT_LIKELY         = 9
    tSInt32, PARAMETER  :: TYPE_REAL32_LIKELY        = 10
    tSInt32, PARAMETER  :: TYPE_REAL64_LIKELY        = 11
    tSInt32, PARAMETER  :: TYPE_REAL128_LIKELY       = 12
    tSInt32, PARAMETER  :: TYPE_CMPX32_LIKELY        = 13
    tSInt32, PARAMETER  :: TYPE_CMPX64_LIKELY        = 14
    tSInt32, PARAMETER  :: TYPE_CMPX128_LIKELY       = 15
    tSInt32, PARAMETER  :: TYPE_REAL_OVERFLOW_LIKELY = 16
    tSInt32, PARAMETER  :: TYPE_CMPX_OVERFLOW_LIKELY = 17
    ! parameters relating to conversion of real number to integer number
    tSInt32, PARAMETER  :: Relaxed_Integer    = 1
    tSInt32, PARAMETER  :: Strict_Integer     = 2
    tSInt32, PARAMETER  :: VeryStrict_Integer = 3

!** DERIVED TYPE DEFINITIONS
    TYPE NumStrInfo
        tLogical    :: NegSign          ! true if the number is negative
        tSInt32     :: Start            ! starting position that exclude the leading spaces and sign
        tSInt32     :: IntegralStart    ! starting position of the integral part of the significand digits
        tSInt32     :: IntegralEnd      ! ending position of the integral part of the significand digits
        tSInt32     :: FractionStart    ! starting position of the fraction part of the significand digits
        tSInt32     :: FractionEnd      ! ending position of the fraction part of the significand digits
        tSInt32     :: SigCount         ! number of significand digits
        tSInt32     :: NFrac            ! number of significand digits after decimal points
        tSInt32     :: ExponentStart    ! starting position of the exponent digits
        tSInt32     :: ExponentEnd      ! ending position of the exponent digits
        tSInt32     :: ESign            ! +1 if exponent is positive; -1 if negative
        tLogical    :: DotFound         ! true if the decimal point found
    END TYPE NumStrInfo

!** INTERFACE/GENERIC DEFINITIONS:
    INTERFACE ParseNumber
        !^ **Function Interface**: ParseNumber <br>
        !  **Purpose**:  To parse a string and return a requested number. <br>
        !  **Usage**: <br>
        !   ! parse a string and return an integer (or real/complex) number (using defaults) <br>
        !   --->    Valid = ParseNumber(Str, Number, EndPos) <br>
        !   ! parse a string and return an integer number (specifying optional input) <br>
        !   --->    Valid = ParseNumber(Str, Number, EndPos, AllowReal, Strict, ErrMsg) <br>
        !   ! parse a string and return a real number (specifying optional input) <br>
        !   --->    Valid = ParseNumber(Str, Number, EndPos, D2BAlgo, Strict, ErrMsg) <br>
        !   ! parse a string and return a complex number (specifying optional input) <br>
        !   --->    Valid = ParseNumber(Str, Number, EndPos, As2Real, D2BAlgo, Strict, ErrMsg) <br>
        MODULE PROCEDURE Parse_Int32
        MODULE PROCEDURE Parse_Int64
        MODULE PROCEDURE Parse_Int128
        MODULE PROCEDURE Parse_RealSP
        MODULE PROCEDURE Parse_RealDP
        MODULE PROCEDURE Parse_RealQP
        MODULE PROCEDURE Parse_CmpxSP
        MODULE PROCEDURE Parse_CmpxDP
        MODULE PROCEDURE Parse_CmpxQP
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!                               PUBLIC PROCEDURES
!------------------------------------------------------------------------------

FUNCTION Parse_Int32(cStr, Number, EndPos, AllowReal, Strict, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To parse a string and return the 32-bit integer number. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     !! string representing a number
    tSInt32,              INTENT(OUT)   :: Number   !! output number
    tSInt32,              INTENT(OUT)   :: EndPos   !! position next to the last valid character
    tLogical,   OPTIONAL, INTENT(IN)    :: AllowReal
    !^ If present and true and the string representing a real number encountered, first convert
    !  the string as a real number first and then convert the real number to an integer one. <br>
    !  Otherwise, if the string representing a real number encountered, only the integral part of 
    !  the significand is converted as an integer number while the fractional part of the significand
    !  and the exponent are ignored. <br>
    !  If Strict is present and true and AllowReal is present and false and the string representing
    !  a real number encountered, the valid flag is set to false. <br>
    tLogical,   OPTIONAL, INTENT(IN)    :: Strict
    !^ If present and true, return true only if the character at EndPos is a separator or EndPos
    !  is equal to the length of the string plus one. <br>
    !  Otherwise, do not check the character at EndPos. <br>
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tLogical                            :: Valid
    !^ true if the string (or a part of it) can represent the output number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(NumStrInfo)    :: StrInfo
    tSInt32             :: NumType
    tSInt32             :: Real2IntFlag

!** FLOW

    ! set default flag and check optional input for real-to-integer conversion
    Real2IntFlag = Strict_Integer
    IF (PRESENT(Strict).AND.PRESENT(AllowReal)) THEN
        ! both input are present
        IF ((Strict).AND.(.NOT.AllowReal)) THEN
            Real2IntFlag = VeryStrict_Integer
        ELSE
            IF (AllowReal) Real2IntFlag = Relaxed_Integer
        END IF
    ELSEIF (PRESENT(AllowReal)) THEN
        ! only AllowReal is present
        IF (AllowReal) Real2IntFlag = Relaxed_Integer
    END IF

    ! set default flag
    Valid = FalseVal
    ! check the number type
    NumType = Check_NumType(cStr, EndPos, StrInfo, Strict, ErrMsg)
    SELECT CASE (NumType)
    CASE (TYPE_INT8_LIKELY, TYPE_INT16_LIKELY, TYPE_INT32_LIKELY)
        ! set flag and ending position
        Valid = TrueVal
        EndPos = StrInfo%IntegralEnd + 1
        ! convert string to number
        Number = Convert_To_Int32(cStr, StrInfo%IntegralStart, StrInfo%IntegralEnd)
        ! handle overflow if needed
        CALL Handle_Overflow_Int32(cStr, StrInfo, Number, ErrMsg)
    CASE (TYPE_REAL32_LIKELY, TYPE_REAL64_LIKELY, TYPE_REAL128_LIKELY)
        SELECT CASE (Real2IntFlag)
        CASE (Strict_Integer)
            ! +++ only consider the integral part of the significand as an integer number +++
            ! set flag and ending position
            Valid = TrueVal
            EndPos = StrInfo%IntegralEnd + 1
            IF ((StrInfo%IntegralEnd - StrInfo%IntegralStart + 1) < 11) THEN
                ! +++ in the range of 32-bit integer +++
                ! convert string to number
                Number = Convert_To_Int32(cStr, StrInfo%IntegralStart, StrInfo%IntegralEnd)
                ! handle overflow if needed
                CALL Handle_Overflow_Int32(cStr, StrInfo, Number, ErrMsg)
            ELSE
                ! +++ overflow for sure +++
                IF (StrInfo%NegSign) THEN
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
                    Number = MIN_INT32
                ELSE
                    IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
                    Number = MAX_INT32
                END IF
            END IF
        CASE (Relaxed_Integer)
            ! +++ consider the string as a real number +++
            ! set flag (ending position has already been set)
            Valid = TrueVal
            SELECT CASE (NumType)
            CASE (TYPE_REAL32_LIKELY)
                BLOCK
                    tUInt32         :: SigDec   ! significand in base 10
                    tSInt32         :: ExpDec   ! exponent in base 10
                    tLogical        :: NegSign  ! true if negative sign
                    TYPE(StringAux) :: Aux      ! auxiliary information
                    CALL Convert_To_FP32(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
                    Number = ToInt32(Convert_To_RealSP(cStr, SigDec, ExpDec, NegSign, Aux, D2B_FastFloat))
                END BLOCK
            CASE (TYPE_REAL64_LIKELY)
                BLOCK
                    tUInt64         :: SigDec   ! significand in base 10
                    tSInt32         :: ExpDec   ! exponent in base 10
                    tLogical        :: NegSign  ! true if negative sign
                    TYPE(StringAux) :: Aux      ! auxiliary information
                    CALL Convert_To_FP64(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
                    Number = ToInt32(Convert_To_RealDP(cStr, SigDec, ExpDec, NegSign, Aux, D2B_FastFloat))
                END BLOCK
            CASE (TYPE_REAL128_LIKELY)
                BLOCK
                    TYPE(UInt128)   :: SigDec   ! significand in base 10
                    tSInt32         :: ExpDec   ! exponent in base 10
                    tLogical        :: NegSign  ! true if negative sign
                    TYPE(StringAux) :: Aux      ! auxiliary information
                    CALL Convert_To_FP128(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
                    Number = ToInt32(Convert_To_RealQP(cStr, SigDec, ExpDec, NegSign, Aux, D2B_FastFloat))
                END BLOCK
            END SELECT
        CASE (VeryStrict_Integer)
            ! +++ For very strict integer, this is considered as invalid string. +++ 
            ! set flag and ending position
            Valid  = FalseVal
            EndPos = StrInfo%IntegralEnd + 1
        END SELECT
    CASE (TYPE_INTEGER_ZERO)
        ! set flag and number
        Valid = TrueVal
        Number = 0_kInt32
    CASE (TYPE_REAL_ZERO)
        Valid = TrueVal
        Number = 0_kInt32
        SELECT CASE (Real2IntFlag)
        CASE (Strict_Integer)
            ! +++ only consider the integral part of the significand as an integer number +++
            ! should return the position of the decimal point
            EndPos = EndPos - 1
            DO
                IF (cStr(EndPos:EndPos) == CHR_PERIOD) EXIT
                EndPos = EndPos - 1
            END DO
        CASE (Relaxed_Integer)
            ! +++ consider the string as a real number +++
            ! do nothing because ending position has already been set
        CASE (VeryStrict_Integer)
            ! +++ For very strict integer, this is considered as invalid string. +++ 
            ! set flag and ending position
            Valid  = FalseVal
            ! should return the position of the decimal point
            EndPos = EndPos - 1
            DO
                IF (cStr(EndPos:EndPos) == CHR_PERIOD) EXIT
                EndPos = EndPos - 1
            END DO
        END SELECT
    CASE (TYPE_INT64_LIKELY, TYPE_INT128_LIKELY, TYPE_APINT_LIKELY)
        ! +++ overflow for sure +++
        ! set flag
        Valid = TrueVal
        IF (StrInfo%NegSign) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MIN_INT32
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MAX_INT32
        END IF
    CASE DEFAULT
        IF (PRESENT(ErrMsg))  ErrMsg  = 'The input string is NOT a valid integer number.'
        Number = MIN_INT32
    END SELECT

    RETURN

END FUNCTION Parse_Int32

!******************************************************************************

FUNCTION Parse_Int64(cStr, Number, EndPos, AllowReal, Strict, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To parse a string and return the 64-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     !! string representing a number
    tSInt64,              INTENT(OUT)   :: Number   !! output number
    tSInt32,              INTENT(OUT)   :: EndPos   !! position next to the last valid character
    tLogical,   OPTIONAL, INTENT(IN)    :: AllowReal
    !^ If present and true and the string representing a real number encountered, first convert
    !  the string as a real number first and then convert the real number to an integer one. <br>
    !  Otherwise, if the string representing a real number encountered, only the integral part of 
    !  the significand is converted as an integer number while the fractional part of the significand
    !  and the exponent are ignored. <br>
    !  If Strict is present and true and AllowReal is present and false and the string representing
    !  a real number encountered, the valid flag is set to false. <br>
    tLogical,   OPTIONAL, INTENT(IN)    :: Strict
    !^ If present and true, return true only if the character at EndPos is a separator or EndPos
    !  is equal to the length of the string plus one. <br>
    !  Otherwise, do not check the character at EndPos. <br>
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tLogical                            :: Valid
    !^ true if the string (or a part of it) can represent the output number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(NumStrInfo)    :: StrInfo
    tSInt32             :: NumType
    tSInt32             :: Real2IntFlag

!** FLOW

    ! set default flag and check optional input for real-to-integer conversion
    Real2IntFlag = Strict_Integer
    IF (PRESENT(Strict).AND.PRESENT(AllowReal)) THEN
        ! both input are present
        IF ((Strict).AND.(.NOT.AllowReal)) THEN
            Real2IntFlag = VeryStrict_Integer
        ELSE
            IF (AllowReal) Real2IntFlag = Relaxed_Integer
        END IF
    ELSEIF (PRESENT(AllowReal)) THEN
        ! only AllowReal is present
        IF (AllowReal) Real2IntFlag = Relaxed_Integer
    END IF

    ! set default flag
    Valid = FalseVal
    ! check the number type
    NumType = Check_NumType(cStr, EndPos, StrInfo, Strict, ErrMsg)
    SELECT CASE (NumType)
    CASE (TYPE_INT8_LIKELY, TYPE_INT16_LIKELY, TYPE_INT32_LIKELY)
        ! set flag and ending position
        Valid = TrueVal
        EndPos = StrInfo%IntegralEnd + 1
        ! convert string to number
        Number = ToUnsignedLong(Convert_To_Int32(cStr, StrInfo%IntegralStart, StrInfo%IntegralEnd))
        IF (StrInfo%NegSign) Number = -Number
        ! Note: we do not need to worry about the overflow of 32-bit integer because we convert it
        !       to an unsigned integer.
    CASE (TYPE_INT64_LIKELY)
        ! set flag and ending position
        Valid = TrueVal
        EndPos = StrInfo%IntegralEnd + 1
        ! convert string to number
        Number = Convert_To_Int64(cStr, StrInfo%IntegralStart, StrInfo%IntegralEnd)
        ! handle overflow if needed
        CALL Handle_Overflow_Int64(cStr, StrInfo, Number, ErrMsg)
    CASE (TYPE_REAL32_LIKELY, TYPE_REAL64_LIKELY, TYPE_REAL128_LIKELY)
        SELECT CASE (Real2IntFlag)
        CASE (Strict_Integer)
            ! +++ only consider the integral part of the significand as an integer number +++
            ! set flag and ending position
            Valid = TrueVal
            EndPos = StrInfo%IntegralEnd + 1
            BLOCK
                tSInt32     :: NumDigits
                NumDigits = StrInfo%IntegralEnd - StrInfo%IntegralStart + 1
                IF (NumDigits < 11) THEN
                    ! +++ in the range of 32-bit integer +++
                    ! convert string to number
                    Number = ToUnsignedLong(Convert_To_Int32(cStr, StrInfo%IntegralStart, StrInfo%IntegralEnd))
                    IF (StrInfo%NegSign) Number = -Number
                    ! Note: we do not need to worry about the overflow of 32-bit integer because we convert it
                    !       to an unsigned integer.
                ELSEIF (NumDigits < 20) THEN
                    ! +++ in the range of 64-bit integer +++
                    ! convert string to number
                    Number = Convert_To_Int64(cStr, StrInfo%IntegralStart, StrInfo%IntegralEnd)
                    ! handle overflow if needed
                    CALL Handle_Overflow_Int64(cStr, StrInfo, Number, ErrMsg)
                ELSE
                    ! +++ overflow for sure +++
                    IF (StrInfo%NegSign) THEN
                        IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
                        Number = MIN_INT64
                    ELSE
                        IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
                        Number = MAX_INT64
                    END IF
                END IF
            END BLOCK
        CASE (Relaxed_Integer)
            ! +++ consider the string as a real number +++
            ! set flag (ending position has already been set)
            Valid = TrueVal
            SELECT CASE (NumType)
            CASE (TYPE_REAL32_LIKELY)
                BLOCK
                    tUInt32         :: SigDec   ! significand in base 10
                    tSInt32         :: ExpDec   ! exponent in base 10
                    tLogical        :: NegSign  ! true if negative sign
                    TYPE(StringAux) :: Aux      ! auxiliary information
                    CALL Convert_To_FP32(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
                    Number = ToInt64(Convert_To_RealSP(cStr, SigDec, ExpDec, NegSign, Aux, D2B_FastFloat))
                END BLOCK
            CASE (TYPE_REAL64_LIKELY)
                BLOCK
                    tUInt64         :: SigDec   ! significand in base 10
                    tSInt32         :: ExpDec   ! exponent in base 10
                    tLogical        :: NegSign  ! true if negative sign
                    TYPE(StringAux) :: Aux      ! auxiliary information
                    CALL Convert_To_FP64(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
                    Number = ToInt64(Convert_To_RealDP(cStr, SigDec, ExpDec, NegSign, Aux, D2B_FastFloat))
                END BLOCK
            CASE (TYPE_REAL128_LIKELY)
                BLOCK
                    TYPE(UInt128)   :: SigDec   ! significand in base 10
                    tSInt32         :: ExpDec   ! exponent in base 10
                    tLogical        :: NegSign  ! true if negative sign
                    TYPE(StringAux) :: Aux      ! auxiliary information
                    CALL Convert_To_FP128(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
                    Number = ToInt64(Convert_To_RealQP(cStr, SigDec, ExpDec, NegSign, Aux, D2B_FastFloat))
                END BLOCK
            END SELECT
        CASE (VeryStrict_Integer)
            ! +++ For very strict integer, this is considered as invalid string. +++ 
            ! set flag and ending position
            Valid  = FalseVal
            EndPos = StrInfo%IntegralEnd + 1
        END SELECT
    CASE (TYPE_INTEGER_ZERO)
        ! set flag and number
        Valid = TrueVal
        Number = 0_kInt32
    CASE (TYPE_REAL_ZERO)
        Valid = TrueVal
        Number = 0_kInt32
        SELECT CASE (Real2IntFlag)
        CASE (Strict_Integer)
            ! +++ only consider the integral part of the significand as an integer number +++
            ! should return the position of the decimal point
            EndPos = EndPos - 1
            DO
                IF (cStr(EndPos:EndPos) == CHR_PERIOD) EXIT
                EndPos = EndPos - 1
            END DO
        CASE (Relaxed_Integer)
            ! +++ consider the string as a real number +++
            ! do nothing because ending position has already been set
        CASE (VeryStrict_Integer)
            ! +++ For very strict integer, this is considered as invalid string. +++ 
            ! set flag and ending position
            Valid  = FalseVal
            ! should return the position of the decimal point
            EndPos = EndPos - 1
            DO
                IF (cStr(EndPos:EndPos) == CHR_PERIOD) EXIT
                EndPos = EndPos - 1
            END DO
        END SELECT
    CASE (TYPE_INT128_LIKELY, TYPE_APINT_LIKELY)
        ! +++ overflow for sure +++
        ! set flag
        Valid = TrueVal
        IF (StrInfo%NegSign) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MIN_INT64
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MAX_INT64
        END IF
    CASE DEFAULT
        IF (PRESENT(ErrMsg))  ErrMsg  = 'The input string is NOT a valid integer number.'
        Number = MIN_INT64
    END SELECT

    RETURN

END FUNCTION Parse_Int64

!******************************************************************************

FUNCTION Parse_Int128(cStr, Number, EndPos, AllowReal, Strict, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To parse a string and return the 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     !! string representing a number
    TYPE(SInt128),        INTENT(OUT)   :: Number   !! output number
    tSInt32,              INTENT(OUT)   :: EndPos   !! position next to the last valid character
    tLogical,   OPTIONAL, INTENT(IN)    :: AllowReal
    !^ If present and true and the string representing a real number encountered, first convert
    !  the string as a real number first and then convert the real number to an integer one. <br>
    !  Otherwise, if the string representing a real number encountered, only the integral part of 
    !  the significand is converted as an integer number while the fractional part of the significand
    !  and the exponent are ignored. <br>
    !  If Strict is present and true and AllowReal is present and false and the string representing
    !  a real number encountered, the valid flag is set to false. <br>
    tLogical,   OPTIONAL, INTENT(IN)    :: Strict
    !^ If present and true, return true only if the character at EndPos is a separator or EndPos
    !  is equal to the length of the string plus one. <br>
    !  Otherwise, do not check the character at EndPos. <br>
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tLogical                            :: Valid
    !^ true if the string (or a part of it) can represent the output number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(NumStrInfo)    :: StrInfo
    tSInt32             :: NumType
    tSInt32             :: Real2IntFlag

!** FLOW

    ! set default flag and check optional input for real-to-integer conversion
    Real2IntFlag = Strict_Integer
    IF (PRESENT(Strict).AND.PRESENT(AllowReal)) THEN
        ! both input are present
        IF ((Strict).AND.(.NOT.AllowReal)) THEN
            Real2IntFlag = VeryStrict_Integer
        ELSE
            IF (AllowReal) Real2IntFlag = Relaxed_Integer
        END IF
    ELSEIF (PRESENT(AllowReal)) THEN
        ! only AllowReal is present
        IF (AllowReal) Real2IntFlag = Relaxed_Integer
    END IF

    ! set default flag
    Valid = FalseVal
    ! check the number type
    NumType = Check_NumType(cStr, EndPos, StrInfo, Strict, ErrMsg)
    SELECT CASE (NumType)
    CASE (TYPE_INT8_LIKELY, TYPE_INT16_LIKELY, TYPE_INT32_LIKELY)
        ! set flag and ending position
        Valid = TrueVal
        EndPos = StrInfo%IntegralEnd + 1
        ! convert string to number
        Number = SInt128(0_kInt64, ToUnsignedLong(Convert_To_Int32(cStr, StrInfo%IntegralStart, &
                                                                   StrInfo%IntegralEnd)))
        IF (StrInfo%NegSign) Number = -Number
        ! Note: we do not need to worry about the overflow of 32-bit integer because we convert it
        !       to an unsigned integer.
    CASE (TYPE_INT64_LIKELY)
        ! set flag and ending position
        Valid = TrueVal
        EndPos = StrInfo%IntegralEnd + 1
        ! convert string to number
        Number = SInt128(0_kInt64, Convert_To_Int64(cStr, StrInfo%IntegralStart, StrInfo%IntegralEnd))
        IF (StrInfo%NegSign) Number = -Number
        ! Note: we do not need to worry about the overflow of 64-bit integer because the lower 64-bits
        !       of 128-bit integer is treated as an unsigned one so if its value is negative, that just
        !       means that its value is higher than MAX_I64.
    CASE (TYPE_INT128_LIKELY)
        ! set flag and ending position
        Valid = TrueVal
        EndPos = StrInfo%IntegralEnd + 1
        ! convert string to number
        Number = Convert_To_Int128(cStr, StrInfo%IntegralStart, StrInfo%IntegralEnd)
        ! handle overflow if needed
        CALL Handle_Overflow_Int128(cStr, StrInfo, Number, ErrMsg)
    CASE (TYPE_REAL32_LIKELY, TYPE_REAL64_LIKELY, TYPE_REAL128_LIKELY)
        SELECT CASE (Real2IntFlag)
        CASE (Strict_Integer)
            ! +++ only consider the integral part of the significand as an integer number +++
            ! set flag and ending position
            Valid = TrueVal
            EndPos = StrInfo%IntegralEnd + 1
            BLOCK
                tSInt32     :: NumDigits
                NumDigits = StrInfo%IntegralEnd - StrInfo%IntegralStart + 1
                IF (NumDigits < 11) THEN
                    ! +++ in the range of 32-bit integer +++
                    ! convert string to number
                    Number = SInt128(0_kInt64, ToUnsignedLong(Convert_To_Int32(cStr, StrInfo%IntegralStart, &
                                                                               StrInfo%IntegralEnd)))
                    IF (StrInfo%NegSign) Number = -Number
                    ! Note: we do not need to worry about the overflow of 32-bit integer because we convert it
                    !       to an unsigned integer.
                ELSEIF (NumDigits < 20) THEN
                    ! +++ in the range of 64-bit integer +++
                    ! convert string to number
                    Number = SInt128(0_kInt64, Convert_To_Int64(cStr, StrInfo%IntegralStart, StrInfo%IntegralEnd))
                    IF (StrInfo%NegSign) Number = -Number
                    ! Note: we do not need to worry about the overflow of 64-bit integer because the lower 64-bits
                    !       of 128-bit integer is treated as an unsigned one so if its value is negative, that just
                    !       means that its value is higher than MAX_I64.
                ELSEIF (NumDigits < 40) THEN
                    ! +++ in the range of 128-bit integer +++
                    ! convert string to number
                    Number = Convert_To_Int128(cStr, StrInfo%IntegralStart, StrInfo%IntegralEnd)
                    ! handle overflow if needed
                    CALL Handle_Overflow_Int128(cStr, StrInfo, Number, ErrMsg)
                ELSE
                    ! +++ overflow for sure +++
                    IF (StrInfo%NegSign) THEN
                        IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
                        Number = MinI128
                    ELSE
                        IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
                        Number = MaxI128
                    END IF
                END IF
            END BLOCK
        CASE (Relaxed_Integer)
            ! +++ consider the string as a real number +++
            ! set flag (ending position has already been set)
            Valid = TrueVal
            SELECT CASE (NumType)
            CASE (TYPE_REAL32_LIKELY)
                BLOCK
                    tUInt32         :: SigDec   ! significand in base 10
                    tSInt32         :: ExpDec   ! exponent in base 10
                    tLogical        :: NegSign  ! true if negative sign
                    TYPE(StringAux) :: Aux      ! auxiliary information
                    CALL Convert_To_FP32(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
                    Number = SInt128(Convert_To_RealSP(cStr, SigDec, ExpDec, NegSign, Aux, D2B_FastFloat))
                END BLOCK
            CASE (TYPE_REAL64_LIKELY)
                BLOCK
                    tUInt64         :: SigDec   ! significand in base 10
                    tSInt32         :: ExpDec   ! exponent in base 10
                    tLogical        :: NegSign  ! true if negative sign
                    TYPE(StringAux) :: Aux      ! auxiliary information
                    CALL Convert_To_FP64(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
                    Number = SInt128(Convert_To_RealDP(cStr, SigDec, ExpDec, NegSign, Aux, D2B_FastFloat))
                END BLOCK
            CASE (TYPE_REAL128_LIKELY)
                BLOCK
                    TYPE(UInt128)   :: SigDec   ! significand in base 10
                    tSInt32         :: ExpDec   ! exponent in base 10
                    tLogical        :: NegSign  ! true if negative sign
                    TYPE(StringAux) :: Aux      ! auxiliary information
                    CALL Convert_To_FP128(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
                    Number = SInt128(Convert_To_RealQP(cStr, SigDec, ExpDec, NegSign, Aux, D2B_FastFloat))
                END BLOCK
            END SELECT
        CASE (VeryStrict_Integer)
            ! +++ For very strict integer, this is considered as invalid string. +++ 
            ! set flag and ending position
            Valid  = FalseVal
            EndPos = StrInfo%IntegralEnd + 1
        END SELECT
    CASE (TYPE_INTEGER_ZERO)
        ! set flag and number
        Valid = TrueVal
        Number = 0_kInt32
    CASE (TYPE_REAL_ZERO)
        Valid = TrueVal
        Number = 0_kInt32
        SELECT CASE (Real2IntFlag)
        CASE (Strict_Integer)
            ! +++ only consider the integral part of the significand as an integer number +++
            ! should return the position of the decimal point
            EndPos = EndPos - 1
            DO
                IF (cStr(EndPos:EndPos) == CHR_PERIOD) EXIT
                EndPos = EndPos - 1
            END DO
        CASE (Relaxed_Integer)
            ! +++ consider the string as a real number +++
            ! do nothing because ending position has already been set
        CASE (VeryStrict_Integer)
            ! +++ For very strict integer, this is considered as invalid string. +++ 
            ! set flag and ending position
            Valid  = FalseVal
            ! should return the position of the decimal point
            EndPos = EndPos - 1
            DO
                IF (cStr(EndPos:EndPos) == CHR_PERIOD) EXIT
                EndPos = EndPos - 1
            END DO
        END SELECT
    CASE (TYPE_APINT_LIKELY)
        ! +++ overflow for sure +++
        ! set flag
        Valid = TrueVal
        IF (StrInfo%NegSign) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI128
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI128
        END IF
    CASE DEFAULT
        IF (PRESENT(ErrMsg))  ErrMsg  = 'The input string is NOT a valid integer number.'
        Number = MinI128
    END SELECT

    RETURN

END FUNCTION Parse_Int128

!******************************************************************************

FUNCTION Parse_RealSP(cStr, Number, EndPos, D2BAlgo, Strict, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To parse a string and return the single-precision real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     !! string representing a number
    tRealSP,              INTENT(OUT)   :: Number   !! output number
    tSInt32,              INTENT(OUT)   :: EndPos   !! position next to the last valid character
    tSInt32,    OPTIONAL, INTENT(IN)    :: D2BAlgo
    !^ flag for decimal-to-binary algorithm <br>
    ! - D2B_FastFloat (or 1) if using the FastFloat algorithm. <br>
    ! - D2B_Lemire (or 2) if using the Lemire algorithm. <br>
    ! - D2B_LibC (or 3) if using the LibC algorithm. <br>
    ! - D2B_YY (or 4) if using the YY algorithm. <br>
    tLogical,   OPTIONAL, INTENT(IN)    :: Strict
    !^ If present and true, return true only if the character at EndPos is a separator or EndPos
    !  is equal to the length of the string plus one. <br>
    !  Otherwise, do not check the character at EndPos. <br>
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tLogical                            :: Valid
    !^ true if the string (or a part of it) can represent the output number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(NumStrInfo)    :: StrInfo
    tSInt32             :: NumType
    tSInt32             :: D2BAlgorithm
    tLogical            :: Handle32Bits
    tLogical            :: Handle64Bits
    tLogical            :: Handle128Bits

!** FLOW

    ! set decimal-to-binary algorithm flag
    D2BAlgorithm = D2B_FastFloat
    IF (PRESENT(D2BAlgo)) THEN
        SELECT CASE (D2BAlgo)
        CASE (1:4)
            D2BAlgorithm = D2BAlgo
        END SELECT
    END IF

    ! set default values
    Handle32Bits  = FalseVal
    Handle64Bits  = FalseVal
    Handle128Bits = FalseVal

    ! check number type and reset flags
    NumType = Check_NumType(cStr, EndPos, StrInfo, Strict, ErrMsg)
    SELECT CASE (NumType)
    CASE (TYPE_INT8_LIKELY, TYPE_INT16_LIKELY, TYPE_INT32_LIKELY, TYPE_INT64_LIKELY)
        ! +++ only integral part exists +++
        ! set flag and ending position
        Valid = TrueVal
        EndPos = StrInfo%IntegralEnd + 1
        ! handle using 32-bit numbers
        Handle32Bits = TrueVal
    CASE (TYPE_INT128_LIKELY, TYPE_APINT_LIKELY)
        ! +++ only integral part exists +++
        ! set flag and ending position
        Valid = TrueVal
        EndPos = StrInfo%IntegralEnd + 1
        IF (StrInfo%SigCount < 39) THEN
            ! handle using 32-bit numbers
            Handle32Bits = TrueVal
        ELSE
            ! handle using 64-bit numbers
            Handle64Bits = TrueVal
        END IF
    CASE (TYPE_REAL32_LIKELY)
        ! +++ fraction part and/or exponent part exist(s) +++
        ! set flag (ending position is already set accurately)
        Valid = TrueVal
        ! handle using 32-bit numbers
        Handle32Bits = TrueVal
    CASE (TYPE_REAL64_LIKELY)
        ! +++ fraction part and/or exponent part exist(s) +++
        ! set flag (ending position is already set accurately)
        Valid = TrueVal
        ! handle using 64-bit numbers
        Handle64Bits = TrueVal
    CASE (TYPE_REAL128_LIKELY)
        ! +++ fraction part and/or exponent part exist(s) +++
        ! set flag (ending position is already set accurately)
        Valid = TrueVal
        ! handle using 128-bit numbers
        Handle128Bits = TrueVal
    CASE (TYPE_INTEGER_ZERO, TYPE_REAL_ZERO)
        ! set flag (ending position is already set accurately) and number
        Valid = TrueVal
        Number = 0.0_kSingle
    CASE (TYPE_REAL_OVERFLOW_LIKELY)
        ! set flag (ending position is already set accurately)
        Valid = TrueVal
        ! handle using 128-bit numbers
        Handle128Bits = TrueVal
    CASE (TYPE_INF_NEGATIVE)
        ! set flag (ending position is already set accurately) and number
        Valid = TrueVal
        Number = IEEE_VALUE(1.0_kSingle, IEEE_NEGATIVE_INF)
    CASE (TYPE_INF_POSITIVE)
        ! set flag (ending position is already set accurately) and number
        Valid = TrueVal
        Number = IEEE_VALUE(1.0_kSingle, IEEE_POSITIVE_INF)
    CASE (TYPE_NAN_EXACT)
        ! set flag (ending position is already set accurately) and number
        Valid = TrueVal
        Number = IEEE_VALUE(0.0_kSingle, IEEE_QUIET_NAN)
    CASE (TYPE_NAN_INVALID)
        ! set flag (ending position is already set accurately) and number
        Valid = FalseVal
        Number = IEEE_VALUE(0.0_kSingle, IEEE_SIGNALING_NAN)
    CASE DEFAULT
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Unknown number type.'
        Valid = FalseVal
        ! set flag (ending position is already set accurately) and number
        Number = IEEE_VALUE(0.0_kSingle, IEEE_SIGNALING_NAN)
    END SELECT

    ! convert to the real number if needed
    IF (Handle32Bits) THEN
        BLOCK
            tUInt32         :: SigDec   ! significand in base 10
            tSInt32         :: ExpDec   ! exponent in base 10
            tLogical        :: NegSign  ! true if negative sign
            TYPE(StringAux) :: Aux      ! auxiliary information
            CALL Convert_To_FP32(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
            Number = Convert_To_RealSP(cStr, SigDec, ExpDec, NegSign, Aux, D2BAlgorithm)
        END BLOCK
    ELSEIF (Handle64Bits) THEN
        BLOCK
            tUInt64         :: SigDec   ! significand in base 10
            tSInt32         :: ExpDec   ! exponent in base 10
            tLogical        :: NegSign  ! true if negative sign
            TYPE(StringAux) :: Aux      ! auxiliary information
            CALL Convert_To_FP64(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
            Number = REAL(Convert_To_RealDP(cStr, SigDec, ExpDec, NegSign, Aux, D2BAlgorithm), KIND=kSingle)
        END BLOCK
    ELSEIF (Handle128Bits) THEN
        BLOCK
            TYPE(UInt128)   :: SigDec   ! significand in base 10
            tSInt32         :: ExpDec   ! exponent in base 10
            tLogical        :: NegSign  ! true if negative sign
            TYPE(StringAux) :: Aux      ! auxiliary information
            CALL Convert_To_FP128(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
            Number = REAL(Convert_To_RealQP(cStr, SigDec, ExpDec, NegSign, Aux, D2BAlgorithm), KIND=kSingle)
        END BLOCK
    END IF

    RETURN

END FUNCTION Parse_RealSP

!******************************************************************************

FUNCTION Parse_RealDP(cStr, Number, EndPos, D2BAlgo, Strict, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To parse a string and return the double-precision real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     !! string representing a number
    tRealDP,              INTENT(OUT)   :: Number   !! output number
    tSInt32,              INTENT(OUT)   :: EndPos   !! position next to the last valid character
    tSInt32,    OPTIONAL, INTENT(IN)    :: D2BAlgo
    !^ flag for decimal-to-binary algorithm <br>
    ! - D2B_FastFloat (or 1) if using the FastFloat algorithm. <br>
    ! - D2B_Lemire (or 2) if using the Lemire algorithm. <br>
    ! - D2B_LibC (or 3) if using the LibC algorithm. <br>
    ! - D2B_YY (or 4) if using the YY algorithm. <br>
    tLogical,   OPTIONAL, INTENT(IN)    :: Strict
    !^ If present and true, return true only if the character at EndPos is a separator or EndPos
    !  is equal to the length of the string plus one. <br>
    !  Otherwise, do not check the character at EndPos. <br>
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tLogical                            :: Valid
    !^ true if the string (or a part of it) can represent the output number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(NumStrInfo)    :: StrInfo
    tSInt32             :: NumType
    tSInt32             :: D2BAlgorithm
    tLogical            :: Handle32Bits
    tLogical            :: Handle64Bits
    tLogical            :: Handle128Bits

!** FLOW

    ! set decimal-to-binary algorithm flag
    D2BAlgorithm = D2B_FastFloat
    IF (PRESENT(D2BAlgo)) THEN
        SELECT CASE (D2BAlgo)
        CASE (1:4)
            D2BAlgorithm = D2BAlgo
        END SELECT
    END IF

    ! set default values
    Handle32Bits  = FalseVal
    Handle64Bits  = FalseVal
    Handle128Bits = FalseVal

    ! check number type and reset flags
    NumType = Check_NumType(cStr, EndPos, StrInfo, Strict, ErrMsg)
    SELECT CASE (NumType)
    CASE (TYPE_INT8_LIKELY, TYPE_INT16_LIKELY, TYPE_INT32_LIKELY, TYPE_INT64_LIKELY)
        ! +++ only integral part exists +++
        ! set flag and ending position
        Valid = TrueVal
        EndPos = StrInfo%IntegralEnd + 1
        ! handle using 32-bit numbers
        Handle32Bits = TrueVal
    CASE (TYPE_INT128_LIKELY, TYPE_APINT_LIKELY)
        ! +++ only integral part exists +++
        ! set flag and ending position
        Valid = TrueVal
        EndPos = StrInfo%IntegralEnd + 1
        IF (StrInfo%SigCount < 39) THEN
            ! handle using 32-bit numbers
            Handle32Bits = TrueVal
        ELSE
            ! handle using 64-bit numbers
            Handle64Bits = TrueVal
        END IF
    CASE (TYPE_REAL32_LIKELY)
        ! +++ fraction part and/or exponent part exist(s) +++
        ! set flag (ending position is already set accurately)
        Valid = TrueVal
        ! handle using 32-bit numbers
        Handle32Bits = TrueVal
    CASE (TYPE_REAL64_LIKELY)
        ! +++ fraction part and/or exponent part exist(s) +++
        ! set flag (ending position is already set accurately)
        Valid = TrueVal
        ! handle using 64-bit numbers
        Handle64Bits = TrueVal
    CASE (TYPE_REAL128_LIKELY)
        ! +++ fraction part and/or exponent part exist(s) +++
        ! set flag (ending position is already set accurately)
        Valid = TrueVal
        ! handle using 128-bit numbers
        Handle128Bits = TrueVal
    CASE (TYPE_INTEGER_ZERO, TYPE_REAL_ZERO)
        ! set flag (ending position is already set accurately) and number
        Valid = TrueVal
        Number = 0.0_kDouble
    CASE (TYPE_REAL_OVERFLOW_LIKELY)
        ! set flag (ending position is already set accurately)
        Valid = TrueVal
        ! handle using 128-bit numbers
        Handle128Bits = TrueVal
    CASE (TYPE_INF_NEGATIVE)
        ! set flag (ending position is already set accurately) and number
        Valid = TrueVal
        Number = IEEE_VALUE(1.0_kDouble, IEEE_NEGATIVE_INF)
    CASE (TYPE_INF_POSITIVE)
        ! set flag (ending position is already set accurately) and number
        Valid = TrueVal
        Number = IEEE_VALUE(1.0_kDouble, IEEE_POSITIVE_INF)
    CASE (TYPE_NAN_EXACT)
        ! set flag (ending position is already set accurately) and number
        Valid = TrueVal
        Number = IEEE_VALUE(0.0_kDouble, IEEE_QUIET_NAN)
    CASE (TYPE_NAN_INVALID)
        ! set flag (ending position is already set accurately) and number
        Valid = FalseVal
        Number = IEEE_VALUE(0.0_kDouble, IEEE_SIGNALING_NAN)
    CASE DEFAULT
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Unknown number type.'
        Valid = FalseVal
        ! set flag (ending position is already set accurately) and number
        Number = IEEE_VALUE(0.0_kDouble, IEEE_SIGNALING_NAN)
    END SELECT

    ! convert to the real number if needed
    IF (Handle32Bits) THEN
        BLOCK
            tUInt32         :: SigDec   ! significand in base 10
            tSInt32         :: ExpDec   ! exponent in base 10
            tLogical        :: NegSign  ! true if negative sign
            TYPE(StringAux) :: Aux      ! auxiliary information
            CALL Convert_To_FP32(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
            Number = REAL(Convert_To_RealSP(cStr, SigDec, ExpDec, NegSign, Aux, D2BAlgorithm), KIND=kDouble)
        END BLOCK
    ELSEIF (Handle64Bits) THEN
        BLOCK
            tUInt64         :: SigDec   ! significand in base 10
            tSInt32         :: ExpDec   ! exponent in base 10
            tLogical        :: NegSign  ! true if negative sign
            TYPE(StringAux) :: Aux      ! auxiliary information
            CALL Convert_To_FP64(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
            Number = Convert_To_RealDP(cStr, SigDec, ExpDec, NegSign, Aux, D2BAlgorithm)
        END BLOCK
    ELSEIF (Handle128Bits) THEN
        BLOCK
            TYPE(UInt128)   :: SigDec   ! significand in base 10
            tSInt32         :: ExpDec   ! exponent in base 10
            tLogical        :: NegSign  ! true if negative sign
            TYPE(StringAux) :: Aux      ! auxiliary information
            CALL Convert_To_FP128(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
            Number = REAL(Convert_To_RealQP(cStr, SigDec, ExpDec, NegSign, Aux, D2BAlgorithm), KIND=kDouble)
        END BLOCK
    END IF

    RETURN

END FUNCTION Parse_RealDP

!******************************************************************************

FUNCTION Parse_RealQP(cStr, Number, EndPos, D2BAlgo, Strict, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To parse a string and return the quadruple-precision real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     !! string representing a number
    tRealQP,              INTENT(OUT)   :: Number   !! output number
    tSInt32,              INTENT(OUT)   :: EndPos   !! position next to the last valid character
    tSInt32,    OPTIONAL, INTENT(IN)    :: D2BAlgo
    !^ flag for decimal-to-binary algorithm <br>
    ! - D2B_FastFloat (or 1) if using the FastFloat algorithm. <br>
    ! - D2B_Lemire (or 2) if using the Lemire algorithm. <br>
    ! - D2B_LibC (or 3) if using the LibC algorithm. <br>
    ! - D2B_YY (or 4) if using the YY algorithm. <br>
    tLogical,   OPTIONAL, INTENT(IN)    :: Strict
    !^ If present and true, return true only if the character at EndPos is a separator or EndPos
    !  is equal to the length of the string plus one. <br>
    !  Otherwise, do not check the character at EndPos. <br>
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tLogical                            :: Valid
    !^ true if the string (or a part of it) can represent the output number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(NumStrInfo)    :: StrInfo
    tSInt32             :: NumType
    tSInt32             :: D2BAlgorithm
    tLogical            :: Handle32Bits
    tLogical            :: Handle64Bits
    tLogical            :: Handle128Bits

!** FLOW

    ! set decimal-to-binary algorithm flag
    D2BAlgorithm = D2B_FastFloat
    IF (PRESENT(D2BAlgo)) THEN
        SELECT CASE (D2BAlgo)
        CASE (1:4)
            D2BAlgorithm = D2BAlgo
        END SELECT
    END IF

    ! set default values
    Handle32Bits  = FalseVal
    Handle64Bits  = FalseVal
    Handle128Bits = FalseVal

    ! check number type and reset flags
    NumType = Check_NumType(cStr, EndPos, StrInfo, Strict, ErrMsg)
    SELECT CASE (NumType)
    CASE (TYPE_INT8_LIKELY, TYPE_INT16_LIKELY, TYPE_INT32_LIKELY, TYPE_INT64_LIKELY)
        ! +++ only integral part exists +++
        ! set flag and ending position
        Valid = TrueVal
        EndPos = StrInfo%IntegralEnd + 1
        ! handle using 32-bit numbers
        Handle32Bits = TrueVal
    CASE (TYPE_INT128_LIKELY, TYPE_APINT_LIKELY)
        ! +++ only integral part exists +++
        ! set flag and ending position
        Valid = TrueVal
        EndPos = StrInfo%IntegralEnd + 1
        IF (StrInfo%SigCount < 39) THEN
            ! handle using 32-bit numbers
            Handle32Bits = TrueVal
        ELSE
            ! handle using 64-bit numbers
            Handle64Bits = TrueVal
        END IF
    CASE (TYPE_REAL32_LIKELY)
        ! +++ fraction part and/or exponent part exist(s) +++
        ! set flag (ending position is already set accurately)
        Valid = TrueVal
        ! handle using 32-bit numbers
        Handle32Bits = TrueVal
    CASE (TYPE_REAL64_LIKELY)
        ! +++ fraction part and/or exponent part exist(s) +++
        ! set flag (ending position is already set accurately)
        Valid = TrueVal
        ! handle using 64-bit numbers
        Handle64Bits = TrueVal
    CASE (TYPE_REAL128_LIKELY)
        ! +++ fraction part and/or exponent part exist(s) +++
        ! set flag (ending position is already set accurately)
        Valid = TrueVal
        ! handle using 128-bit numbers
        Handle128Bits = TrueVal
    CASE (TYPE_INTEGER_ZERO, TYPE_REAL_ZERO)
        ! set flag (ending position is already set accurately) and number
        Valid = TrueVal
        Number = 0.0_kQuad
    CASE (TYPE_REAL_OVERFLOW_LIKELY)
        ! set flag (ending position is already set accurately)
        Valid = TrueVal
        ! handle using 128-bit numbers
        Handle128Bits = TrueVal
    CASE (TYPE_INF_NEGATIVE)
        ! set flag (ending position is already set accurately) and number
        Valid = TrueVal
        Number = IEEE_VALUE(1.0_kQuad, IEEE_NEGATIVE_INF)
    CASE (TYPE_INF_POSITIVE)
        ! set flag (ending position is already set accurately) and number
        Valid = TrueVal
        Number = IEEE_VALUE(1.0_kQuad, IEEE_POSITIVE_INF)
    CASE (TYPE_NAN_EXACT)
        ! set flag (ending position is already set accurately) and number
        Valid = TrueVal
        Number = IEEE_VALUE(0.0_kQuad, IEEE_QUIET_NAN)
    CASE (TYPE_NAN_INVALID)
        ! set flag (ending position is already set accurately) and number
        Valid = FalseVal
        Number = IEEE_VALUE(0.0_kQuad, IEEE_SIGNALING_NAN)
    CASE DEFAULT
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Unknown number type.'
        Valid = FalseVal
        ! set flag (ending position is already set accurately) and number
        Number = IEEE_VALUE(0.0_kQuad, IEEE_SIGNALING_NAN)
    END SELECT

    ! convert to the real number if needed
    IF (Handle32Bits) THEN
        BLOCK
            tUInt32         :: SigDec   ! significand in base 10
            tSInt32         :: ExpDec   ! exponent in base 10
            tLogical        :: NegSign  ! true if negative sign
            TYPE(StringAux) :: Aux      ! auxiliary information
            CALL Convert_To_FP32(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
            Number = REAL(Convert_To_RealSP(cStr, SigDec, ExpDec, NegSign, Aux, D2BAlgorithm), KIND=kQuad)
        END BLOCK
    ELSEIF (Handle64Bits) THEN
        BLOCK
            tUInt64         :: SigDec   ! significand in base 10
            tSInt32         :: ExpDec   ! exponent in base 10
            tLogical        :: NegSign  ! true if negative sign
            TYPE(StringAux) :: Aux      ! auxiliary information
            CALL Convert_To_FP64(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
            Number = REAL(Convert_To_RealDP(cStr, SigDec, ExpDec, NegSign, Aux, D2BAlgorithm), KIND=kQuad)
        END BLOCK
    ELSEIF (Handle128Bits) THEN
        BLOCK
            TYPE(UInt128)   :: SigDec   ! significand in base 10
            tSInt32         :: ExpDec   ! exponent in base 10
            tLogical        :: NegSign  ! true if negative sign
            TYPE(StringAux) :: Aux      ! auxiliary information
            CALL Convert_To_FP128(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)
            Number = Convert_To_RealQP(cStr, SigDec, ExpDec, NegSign, Aux, D2BAlgorithm)
        END BLOCK
    END IF

    RETURN

END FUNCTION Parse_RealQP

!******************************************************************************

FUNCTION Parse_CmpxSP(cStr, Number, EndPos, As2Real, D2BAlgo, Strict, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To parse a string and return the single-precision real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     !! string representing a number
    tCmpxSP,              INTENT(OUT)   :: Number   !! output number
    tSInt32,              INTENT(OUT)   :: EndPos   !! position next to the last valid character
    tLogical,   OPTIONAL, INTENT(IN)    :: As2Real
    !^ If present and true, parse the string as two real numbers separated by space(s). <br>
    !  Otherwise, parse the string in a common form of '(RealNum, ImagNum)' . <br>
    tSInt32,    OPTIONAL, INTENT(IN)    :: D2BAlgo
    !^ flag for decimal-to-binary algorithm <br>
    ! - D2B_FastFloat (or 1) if using the FastFloat algorithm. <br>
    ! - D2B_Lemire (or 2) if using the Lemire algorithm. <br>
    ! - D2B_LibC (or 3) if using the LibC algorithm. <br>
    ! - D2B_YY (or 4) if using the YY algorithm. <br>
    tLogical,   OPTIONAL, INTENT(IN)    :: Strict
    !^ If present and true, return true only if the character at EndPos is a separator or EndPos
    !  is equal to the length of the string plus one. <br>
    !  Otherwise, do not check the character at EndPos. <br>
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tLogical                            :: Valid
    !^ true if the string (or a part of it) can represent the output number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Indx, NextPos
    tSInt32     :: StrLen
    tLogical    :: ParseAs2Real
    tRealSP     :: REPart, IMPart

!** FLOW

    ! initialize
    Valid = FalseVal
    EndPos = StrLen + 1
    SET_OPTION(ParseAs2Real, FalseVal, As2Real)

    ! skip over space(s)
    Indx = 1
    StrLen = LEN(cStr)
    CALL SkipChars(cStr, CHR_SPACE, Indx, StrLen)
    IF (Indx > StrLen) THEN
        Number%RE = IEEE_VALUE(0.0_kSingle, IEEE_SIGNALING_NAN)
        Number%IM = Number%RE
        RETURN
    END IF

    MainIfBlock: IF (ParseAs2Real) THEN
        ! parse as 2 consecutive real numbers separated by space(s)
        IF (Parse_RealSP(cStr(Indx:), REPart, NextPos, D2BAlgo, Strict, ErrMsg)) THEN
            Indx = Indx + NextPos
            CALL SkipChars(cStr, CHR_SPACE, Indx, StrLen)
            IF (Indx > StrLen) EXIT MainIfBlock
            Valid = Parse_RealSP(cStr(Indx:), IMPart, NextPos, D2BAlgo, Strict, ErrMsg)
        END IF
        EndPos = Indx + NextPos
    ELSE
        ! parse as 2 consecutive real numbers separated by a comma where
        ! the two numbers are enclosed by parentheses.
        ! therefore, first check for opening parenthesis
        IF (cStr(Indx:Indx) == CHR_PARENTHESES_LEFT) THEN
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT MainIfBlock
            IF (Parse_RealSP(cStr(Indx:), REPart, NextPos, D2BAlgo, Strict, ErrMsg)) THEN
                Indx = Indx + NextPos
                CALL SkipChars(cStr, CHR_SPACE, Indx, StrLen)
                IF (Indx > StrLen) EXIT MainIfBlock
                ! now check for comma
                IF (cStr(Indx:Indx) == CHR_COMMA) THEN
                    Indx = Indx + 1
                    IF (Indx > StrLen) EXIT MainIfBlock
                    IF (Parse_RealSP(cStr(Indx:), IMPart, NextPos, D2BAlgo, Strict, ErrMsg)) THEN
                        Indx = Indx + NextPos
                        CALL SkipChars(cStr, CHR_SPACE, Indx, StrLen)
                        IF (Indx > StrLen) EXIT MainIfBlock
                        ! finally check for closing parenthesis
                        IF (cStr(Indx:Indx) == CHR_PARENTHESES_RIGHT) THEN
                            Valid = TrueVal
                            EndPos = Indx + 1
                        ELSE
                            EndPos = Indx
                        END IF
                    ELSE
                        EndPos = Indx + NextPos
                    END IF
                ELSE
                    EndPos = Indx
                END IF
            ELSE
                EndPos = Indx + NextPos
            END IF
        ELSE
            EndPos = Indx
        END IF
    END IF MainIfBlock
    IF (Valid) THEN
        Number%RE = REPart
        Number%IM = IMPart
    ELSE
        Number%RE = IEEE_VALUE(0.0_kSingle, IEEE_SIGNALING_NAN)
        Number%IM = Number%RE
    END IF

    RETURN

END FUNCTION Parse_CmpxSP

!******************************************************************************

FUNCTION Parse_CmpxDP(cStr, Number, EndPos, As2Real, D2BAlgo, Strict, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To parse a string and return the single-precision real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     !! string representing a number
    tCmpxDP,              INTENT(OUT)   :: Number   !! output number
    tSInt32,              INTENT(OUT)   :: EndPos   !! position next to the last valid character
    tLogical,   OPTIONAL, INTENT(IN)    :: As2Real
    !^ If present and true, parse the string as two real numbers separated by space(s). <br>
    !  Otherwise, parse the string in a common form of '(RealNum, ImagNum)' . <br>
    tLogical,   OPTIONAL, INTENT(IN)    :: Strict
    !^ If present and true, return true only if the character at EndPos is a separator or EndPos
    !  is equal to the length of the string plus one. <br>
    !  Otherwise, do not check the character at EndPos. <br>
    tSInt32,    OPTIONAL, INTENT(IN)    :: D2BAlgo
    !^ flag for decimal-to-binary algorithm <br>
    ! - D2B_FastFloat (or 1) if using the FastFloat algorithm. <br>
    ! - D2B_Lemire (or 2) if using the Lemire algorithm. <br>
    ! - D2B_LibC (or 3) if using the LibC algorithm. <br>
    ! - D2B_YY (or 4) if using the YY algorithm. <br>
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tLogical                            :: Valid
    !^ true if the string (or a part of it) can represent the output number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Indx, NextPos
    tSInt32     :: StrLen
    tLogical    :: ParseAs2Real
    tRealDP     :: REPart, IMPart

!** FLOW

    ! initialize
    Valid  = FalseVal
    EndPos = StrLen + 1
    SET_OPTION(ParseAs2Real, FalseVal, As2Real)

    ! skip over space(s)
    Indx = 1
    StrLen = LEN(cStr)
    CALL SkipChars(cStr, CHR_SPACE, Indx, StrLen)
    IF (Indx > StrLen) THEN
        Number%RE = IEEE_VALUE(0.0_kDouble, IEEE_SIGNALING_NAN)
        Number%IM = Number%RE
        RETURN
    END IF

    MainIfBlock: IF (ParseAs2Real) THEN
        ! parse as 2 consecutive real numbers separated by space(s)
        IF (Parse_RealDP(cStr(Indx:), REPart, NextPos, D2BAlgo, Strict, ErrMsg)) THEN
            Indx = Indx + NextPos
            CALL SkipChars(cStr, CHR_SPACE, Indx, StrLen)
            IF (Indx > StrLen) EXIT MainIfBlock
            Valid = Parse_RealDP(cStr(Indx:), IMPart, NextPos, D2BAlgo, Strict, ErrMsg)
        END IF
        EndPos = Indx + NextPos
    ELSE
        ! parse as 2 consecutive real numbers separated by a comma where
        ! the two numbers are enclosed by parentheses.
        ! therefore, first check for opening parenthesis
        IF (cStr(Indx:Indx) == CHR_PARENTHESES_LEFT) THEN
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT MainIfBlock
            IF (Parse_RealDP(cStr(Indx:), REPart, NextPos, D2BAlgo, Strict, ErrMsg)) THEN
                Indx = Indx + NextPos
                CALL SkipChars(cStr, CHR_SPACE, Indx, StrLen)
                IF (Indx > StrLen) EXIT MainIfBlock
                ! now check for comma
                IF (cStr(Indx:Indx) == CHR_COMMA) THEN
                    Indx = Indx + 1
                    IF (Indx > StrLen) EXIT MainIfBlock
                    IF (Parse_RealDP(cStr(Indx:), IMPart, NextPos, D2BAlgo, Strict, ErrMsg)) THEN
                        Indx = Indx + NextPos
                        CALL SkipChars(cStr, CHR_SPACE, Indx, StrLen)
                        IF (Indx > StrLen) EXIT MainIfBlock
                        ! finally check for closing parenthesis
                        IF (cStr(Indx:Indx) == CHR_PARENTHESES_RIGHT) THEN
                            Valid = TrueVal
                            EndPos = Indx + 1
                        ELSE
                            EndPos = Indx
                        END IF
                    ELSE
                        EndPos = Indx + NextPos
                    END IF
                ELSE
                    EndPos = Indx
                END IF
            ELSE
                EndPos = Indx + NextPos
            END IF
        ELSE
            EndPos = Indx
        END IF
    END IF MainIfBlock
    IF (Valid) THEN
        Number%RE = REPart
        Number%IM = IMPart
    ELSE
        Number%RE = IEEE_VALUE(0.0_kDouble, IEEE_SIGNALING_NAN)
        Number%IM = Number%RE
    END IF

    RETURN

END FUNCTION Parse_CmpxDP

!******************************************************************************

FUNCTION Parse_CmpxQP(cStr, Number, EndPos, As2Real, D2BAlgo, Strict, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To parse a string and return the single-precision real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,            INTENT(IN)    :: cStr     !! string representing a number
    tCmpxQP,              INTENT(OUT)   :: Number   !! output number
    tSInt32,              INTENT(OUT)   :: EndPos   !! position next to the last valid character
    tLogical,   OPTIONAL, INTENT(IN)    :: As2Real
    !^ If present and true, parse the string as two real numbers separated by space(s). <br>
    !  Otherwise, parse the string in a common form of '(RealNum, ImagNum)' . <br>
    tSInt32,    OPTIONAL, INTENT(IN)    :: D2BAlgo
    !^ flag for decimal-to-binary algorithm <br>
    ! - D2B_FastFloat (or 1) if using the FastFloat algorithm. <br>
    ! - D2B_Lemire (or 2) if using the Lemire algorithm. <br>
    ! - D2B_LibC (or 3) if using the LibC algorithm. <br>
    ! - D2B_YY (or 4) if using the YY algorithm. <br>
    tLogical,   OPTIONAL, INTENT(IN)    :: Strict
    !^ If present and true, return true only if the character at EndPos is a separator or EndPos
    !  is equal to the length of the string plus one. <br>
    !  Otherwise, do not check the character at EndPos. <br>
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tLogical                            :: Valid
    !^ true if the string (or a part of it) can represent the output number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Indx, NextPos
    tSInt32     :: StrLen
    tLogical    :: ParseAs2Real
    tRealQP     :: REPart, IMPart

!** FLOW

    ! initialize
    Valid  = FalseVal
    EndPos = StrLen + 1
    SET_OPTION(ParseAs2Real, FalseVal, As2Real)

    ! skip over space(s)
    Indx = 1
    StrLen = LEN(cStr)
    CALL SkipChars(cStr, CHR_SPACE, Indx, StrLen)
    IF (Indx > StrLen) THEN
        Number%RE = IEEE_VALUE(0.0_kQuad, IEEE_SIGNALING_NAN)
        Number%IM = Number%RE
        RETURN
    END IF

    MainIfBlock: IF (ParseAs2Real) THEN
        ! parse as 2 consecutive real numbers separated by space(s)
        IF (Parse_RealQP(cStr(Indx:), REPart, NextPos, D2BAlgo, Strict, ErrMsg)) THEN
            Indx = Indx + NextPos
            CALL SkipChars(cStr, CHR_SPACE, Indx, StrLen)
            IF (Indx > StrLen) EXIT MainIfBlock
            Valid = Parse_RealQP(cStr(Indx:), IMPart, NextPos, D2BAlgo, Strict, ErrMsg)
        END IF
        EndPos = Indx + NextPos
    ELSE
        ! parse as 2 consecutive real numbers separated by a comma where
        ! the two numbers are enclosed by parentheses.
        ! therefore, first check for opening parenthesis
        IF (cStr(Indx:Indx) == CHR_PARENTHESES_LEFT) THEN
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT MainIfBlock
            IF (Parse_RealQP(cStr(Indx:), REPart, NextPos, D2BAlgo, Strict, ErrMsg)) THEN
                Indx = Indx + NextPos
                CALL SkipChars(cStr, CHR_SPACE, Indx, StrLen)
                IF (Indx > StrLen) EXIT MainIfBlock
                ! now check for comma
                IF (cStr(Indx:Indx) == CHR_COMMA) THEN
                    Indx = Indx + 1
                    IF (Indx > StrLen) EXIT MainIfBlock
                    IF (Parse_RealQP(cStr(Indx:), IMPart, NextPos, D2BAlgo, Strict, ErrMsg)) THEN
                        Indx = Indx + NextPos
                        CALL SkipChars(cStr, CHR_SPACE, Indx, StrLen)
                        IF (Indx > StrLen) EXIT MainIfBlock
                        ! finally check for closing parenthesis
                        IF (cStr(Indx:Indx) == CHR_PARENTHESES_RIGHT) THEN
                            Valid = TrueVal
                            EndPos = Indx + 1
                        ELSE
                            EndPos = Indx
                        END IF
                    ELSE
                        EndPos = Indx + NextPos
                    END IF
                ELSE
                    EndPos = Indx
                END IF
            ELSE
                EndPos = Indx + NextPos
            END IF
        ELSE
            EndPos = Indx
        END IF
    END IF MainIfBlock
    IF (Valid) THEN
        Number%RE = REPart
        Number%IM = IMPart
    ELSE
        Number%RE = IEEE_VALUE(0.0_kQuad, IEEE_SIGNALING_NAN)
        Number%IM = Number%RE
    END IF

    RETURN

END FUNCTION Parse_CmpxQP

!******************************************************************************

SUBROUTINE SkipChars(Str, Char, Pos, sLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To skip over the specified character.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)       :: Str
    tChar,     INTENT(IN)       :: Char
    tSInt32,   INTENT(INOUT)    :: Pos
    !^ on entry, the starting position to check for the specified character. <br>
    !  on exit, the starting position of a character that is NOT the specified character. <br>
    tSInt32,   INTENT(IN)       :: sLen   !! length of the string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    DO WHILE (Pos <= sLen)
        IF (Str(Pos:Pos) /= Char) EXIT
        Pos = Pos + 1
    END DO

    RETURN

END SUBROUTINE SkipChars

!******************************************************************************

SUBROUTINE SkipSpaces(Str, Pos, sLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To skip over blank(s) (including space, (horizontal) tab and carriage return).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)       :: Str
    tSInt32,   INTENT(INOUT)    :: Pos
    !^ on entry, the starting position to check for blank(s). <br>
    !  on exit, the starting position of non-blank characters. <br>
    tSInt32,   INTENT(IN)       :: sLen   !! length of the string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    DO WHILE (Pos <= sLen)
        IF ((Str(Pos:Pos) /= CHR_SPACE).AND.(Str(Pos:Pos) /= CHR_TAB).AND. &
            (Str(Pos:Pos) /= CHR_CARRIAGE_RETURN)) EXIT
        Pos = Pos + 1
    END DO

    RETURN

END SUBROUTINE SkipSpaces

!******************************************************************************

SUBROUTINE SkipAllWhiteSpaces(Str, Pos, sLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To skip over all white space(s) (including space, horizontal tab,
    !  vertical tab, new line (line feed), form feed and carriage return).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)       :: Str
    tSInt32,   INTENT(INOUT)    :: Pos
    !^ on entry, the starting position to check for blank(s). <br>
    !  on exit, the starting position of non-blank characters. <br>
    tSInt32,   INTENT(IN)       :: sLen   !! length of the string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    DO WHILE (Pos <= sLen)
        IF (.NOT.Is_Character_WhiteSpace(Str(Pos:Pos))) EXIT
        Pos = Pos + 1
    END DO

    RETURN

END SUBROUTINE SkipAllWhiteSpaces

!******************************************************************************

SUBROUTINE SkipSeparators(Str, Pos, sLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To skip over separator characters.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)       :: Str
    tSInt32,   INTENT(INOUT)    :: Pos
    !^ on entry, the starting position to check for separator(s). <br>
    !  on exit, the starting position of non-separator characters. <br>
    tSInt32,   INTENT(IN)       :: sLen   !! length of the string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! first, skip over space characters
    CALL SkipSpaces(Str, Pos, sLen)

    ! check for a separator
    IF (Pos <= sLen) THEN
        SELECT CASE (Str(Pos:Pos))
        CASE (CHR_COMMA, CHR_SEMICOLON)
            ! word separators
            CALL SkipSpaces(Str, Pos, sLen)
        CASE (CHR_SLASH, CHR_NEWLINE)
            ! line separators
            Pos = Pos + 1
        CASE (CHR_CARRIAGE_RETURN)
            ! line separators
            Pos = Pos + 1
            IF (Pos <= sLen) THEN
                IF (Str(Pos:Pos)== CHR_NEWLINE) Pos = Pos + 1
            END IF
        END SELECT
    END IF

    RETURN

END SUBROUTINE SkipSeparators

!******************************************************************************

SUBROUTINE Skip2NextLine(Str, Pos, sLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To skip over characters until the new-line character found.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)       :: Str
    tSInt32,   INTENT(INOUT)    :: Pos
    !^ on entry, the starting position to check for character(s). <br>
    !  on exit, position of the first character after the new-line character. <br>
    tSInt32,   INTENT(IN)       :: sLen   !! length of the string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    DO WHILE (Pos <= sLen)
        IF ((Str(Pos:Pos) == CHR_NEWLINE)) EXIT
        Pos = Pos + 1
    END DO

    RETURN

END SUBROUTINE Skip2NextLine

!******************************************************************************

SUBROUTINE SkipDecimalDigits(Str, Pos, sLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To skip over decimal digits.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)       :: Str
    tSInt32,   INTENT(INOUT)    :: Pos
    !^ on entry, the starting position to check for decimal digit(s). <br>
    !  on exit, the starting position of non-digit characters. <br>
    tSInt32,   INTENT(IN)       :: sLen   !! length of the string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    DO WHILE (Pos <= sLen)
        IF ((Str(Pos:Pos) < '0').OR.(Str(Pos:Pos) > '9')) EXIT
        Pos = Pos + 1
    END DO

    RETURN

END SUBROUTINE SkipDecimalDigits

!------------------------------------------------------------------------------
!                              PRIVATE PROCEDURES
!------------------------------------------------------------------------------

FUNCTION Check_NumType(cStr, EndPos, StrInfo, Strict, ErrMsg) RESULT(NumType)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a possible type of a string that possibly represents a number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,                  TARGET, INTENT(IN)  :: cStr     !! string representing a number
    tSInt32,                            INTENT(OUT) :: EndPos   !! position next to the last valid character
    TYPE(NumStrInfo), OPTIONAL, TARGET, INTENT(OUT) :: StrInfo  !! information of the specified string
    tLogical,         OPTIONAL,         INTENT(IN)  :: Strict
    !^ If present and true, return true only if the character at EndPos is a separator or EndPos
    !  is equal to the length of the string plus one. <br>
    !  Otherwise, do not check the character at EndPos. <br>
    tCharAlloc,       OPTIONAL,         INTENT(OUT) :: ErrMsg   !! message if input is not invalid
    tSInt32                                         :: NumType  !! possible number type

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(NumStrInfo), TARGET    :: InfoTarget
    TYPE(NumStrInfo), POINTER   :: InfoPtr
    tSInt32                     :: ExpDigits
    tSInt32                     :: IntDigits

!** FLOW

    IF (PRESENT(StrInfo)) THEN
        InfoPtr => StrInfo
    ELSE
        InfoPtr => InfoTarget
    END IF
    IF (Scan_NumStr(cStr, InfoPtr, EndPos, Strict, ErrMsg)) THEN
        ! either integer or real
        IF (InfoPtr%SigCount == 0) THEN
            ! only zero digits encountered
            IF (InfoPtr%DotFound) THEN
                ! real zero
                NumType = TYPE_REAL_ZERO
            ELSE
                ! integer zero
                NumType = TYPE_INTEGER_ZERO
            END IF
        ELSE
            IF (InfoPtr%DotFound) THEN
                ! real number
                IF (InfoPtr%ExponentStart > 0) THEN
                    ! in scientific format so use exponent to conservatively estimate the number type
                    ExpDigits = InfoPtr%ExponentEnd - InfoPtr%ExponentStart + 1
                    IF (ExpDigits < 2) THEN
                        NumType = TYPE_REAL32_LIKELY
                    ELSEIF (ExpDigits < 3) THEN
                        NumType = TYPE_REAL64_LIKELY
                    ELSEIF (ExpDigits < 4) THEN
                        NumType = TYPE_REAL128_LIKELY
                    ELSE
                        NumType = TYPE_REAL_OVERFLOW_LIKELY
                    END IF
                ELSE
                    ! in plain format so use integral part of the significand to estimate the number type
                    IntDigits = InfoPtr%IntegralEnd - InfoPtr%IntegralStart + 1
                    IF (IntDigits < 38) THEN
                        NumType = TYPE_REAL32_LIKELY
                    ELSEIF (IntDigits < 308) THEN
                        NumType = TYPE_REAL64_LIKELY
                    ELSEIF (IntDigits < 4932) THEN
                        NumType = TYPE_REAL128_LIKELY
                    ELSE
                        NumType = TYPE_REAL_OVERFLOW_LIKELY
                    END IF
                END IF
            ELSEIF (InfoPtr%ExponentStart > 0) THEN
                ! real number without decimal point but in the scientific format
                ! so use exponent to estimate the number type
                ExpDigits = InfoPtr%ExponentEnd - InfoPtr%ExponentStart + 1
                IF (ExpDigits < 2) THEN
                    NumType = TYPE_REAL32_LIKELY
                ELSEIF (ExpDigits < 3) THEN
                    NumType = TYPE_REAL64_LIKELY
                ELSEIF (ExpDigits < 4) THEN
                    NumType = TYPE_REAL128_LIKELY
                ELSE
                    NumType = TYPE_REAL_OVERFLOW_LIKELY
                END IF
            ELSE
                ! integer number
                IF (InfoPtr%SigCount < 3) THEN
                    NumType = TYPE_INT8_LIKELY
                ELSEIF (InfoPtr%SigCount < 5) THEN
                    NumType = TYPE_INT16_LIKELY
                ELSEIF (InfoPtr%SigCount < 10) THEN
                    NumType = TYPE_INT32_LIKELY
                ELSEIF (InfoPtr%SigCount < 19) THEN
                    NumType = TYPE_INT64_LIKELY
                ELSEIF (InfoPtr%SigCount < 39) THEN
                    NumType = TYPE_INT128_LIKELY
                ELSE
                    NumType = TYPE_APINT_LIKELY
                END IF
            END IF
        END IF
    ELSE
        ! invalid string number
        NumType = Handle_Invalid_NumStr(cStr, InfoPtr%Start, InfoPtr%NegSign, EndPos)
    END IF
    NULLIFY(InfoPtr)

    RETURN

END FUNCTION Check_NumType

!******************************************************************************

FUNCTION Scan_NumStr(cStr, StrInfo, EndPos, Strict, ErrMsg) RESULT(Valid)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To scan the specified string and collect information relating to the number
    !  that the string may represent.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! string representing a number
    TYPE(NumStrInfo),     INTENT(OUT)   :: StrInfo
    !^ Information that can be used to determine whether the number is strictly an integer
    !  or a real number.  It also provide information that can be used (in conjunction with
    !  the specified string) to determine the value of the integer number or the significand
    !  and the exponent values of the real number.
    tSInt32,              INTENT(OUT)   :: EndPos   !! position next to the last valid character
    tLogical,   OPTIONAL, INTENT(IN)    :: Strict
    !^ If present and true, return true only if the character at EndPos is a separator or EndPos
    !  is equal to the length of the string plus one. <br>
    !  Otherwise, do not check the character at EndPos. <br>
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
    tLogical                            :: Valid
    !^ true if the string (or a part of it) can represent either an integer or a real number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(1), POINTER    :: CurChr
    tSInt32                 :: Indx, StrLen
    tLogical                :: AtLeastOneDigit, Truncated
    tLogical                :: LeadZeroExpDigit

!** FLOW

    ! initialize
    Valid = FalseVal
    AtLeastOneDigit  = FalseVal
    LeadZeroExpDigit = FalseVal
    Truncated = FalseVal
    StrLen = LEN(cStr)
    StrInfo%Start         = 0
    StrInfo%NegSign       = FalseVal
    StrInfo%IntegralStart = 0
    StrInfo%IntegralEnd   = 0
    StrInfo%FractionStart = 0
    StrInfo%FractionEnd   = 0
    StrInfo%SigCount      = 0
    StrInfo%ESign         = 1
    StrInfo%ExponentStart = 0
    StrInfo%ExponentEnd   = 0
    StrInfo%DotFound      = FalseVal
    IF (StrLen == 0) THEN
        IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this is an empty string.'
        EndPos = StrLen + 1
        RETURN
    END IF

    ! check whether there are spaces in front of the number
    ! (only allow space(s) in front of the number but no spaces inside it)
    Indx = 1
    CALL SkipChars(cStr, CHR_SPACE, Indx, StrLen)
    IF (Indx > StrLen) THEN
        StrInfo%Start = Indx
        IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this is an empty string.'
        EndPos = Indx
        RETURN
    END IF

    ! check for sign of the significand
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') StrInfo%NegSign = TrueVal
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            StrInfo%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: this string only contains a sign without a digit.'
            EndPos = Indx
            RETURN
        END IF
        ! check whether the following character is a digit or a dot
        CurChr => cStr(Indx:Indx)
        IF (((CurChr < '0').OR.(CurChr > '9')).AND.(CurChr /= '.')) THEN
            ! current character is neither a digit nor a dot
            StrInfo%Start = Indx
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid Input: a sign must be followed by a digit or the dot.'
            EndPos = Indx
            RETURN
        END IF
    END IF

    StrInfo%Start = Indx

    ! check for leading zero(s)
    IF (cStr(Indx:Indx) == '0') THEN
        AtLeastOneDigit = TrueVal
        ! the current digit is zero so loop through the following characters
        !  until a non-zero character is found
        Indx = Indx + 1
        CALL SkipChars(cStr, '0', Indx, StrLen)
        IF (Indx > StrLen) THEN
            ! only zero digits encountered
            Valid = TrueVal
            EndPos = Indx
            RETURN
        END IF
    END IF

    ! determine indices for the significand in the integral part
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        AtLeastOneDigit = TrueVal
        StrInfo%IntegralStart = Indx
        Indx = Indx + 1
        CALL SkipDecimalDigits(cStr, Indx, StrLen)
        StrInfo%SigCount = Indx - StrInfo%IntegralStart
        StrInfo%IntegralEnd = Indx - 1
    END IF

    ! check whether the current character is a dot
    StrInfo%NFrac = 0
    IF (Indx <= StrLen) THEN
        IF (cStr(Indx:Indx) == '.') THEN
            StrInfo%DotFound = TrueVal
            Indx = Indx + 1
            IF (Indx <= StrLen) THEN
                CurChr => cStr(Indx:Indx)
                IF ((CurChr >= '0').AND.(CurChr <= '9')) THEN
                    AtLeastOneDigit = TrueVal
                    StrInfo%NFrac = Indx
                    IF (StrInfo%SigCount > 0) THEN
                        StrInfo%FractionStart = Indx
                        ! continue checking for the significand
                        Indx = Indx + 1
                        CALL SkipDecimalDigits(cStr, Indx, StrLen)
                        StrInfo%NFrac = Indx - StrInfo%NFrac
                        StrInfo%SigCount = StrInfo%SigCount + StrInfo%NFrac
                        StrInfo%FractionEnd = Indx - 1
                    ELSE
                        ! check for leading zero(s)
                        IF (cStr(Indx:Indx) == '0') THEN
                            Indx = Indx + 1
                            CALL SkipChars(cStr, '0', Indx, StrLen)
                            IF (Indx > StrLen) THEN
                                ! only zero digits encountered
                                Valid = TrueVal
                                EndPos = Indx
                                RETURN
                            END IF
                        END IF
                        CurChr => cStr(Indx:Indx)
                        IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
                            ! start determining indices for the significand
                            StrInfo%FractionStart = Indx
                            Indx = Indx + 1
                            CALL SkipDecimalDigits(cStr, Indx, StrLen)
                            StrInfo%SigCount = Indx - StrInfo%FractionStart
                            StrInfo%NFrac = Indx - StrInfo%NFrac
                            StrInfo%FractionEnd = Indx - 1
                        END IF
                    END IF
                END IF
            END IF
        END IF
    END IF

    ! done for the significand part so check the number of significant digits
    ! (there must be at least one significant digit)
    IF (StrInfo%SigCount == 0) THEN
        IF (AtLeastOneDigit) THEN
            ! this case is unlikely because it should have been already handled.
            Valid = TrueVal
        ELSE
            ! this case can happen when not a number is encountered (i.e. the first non-blank character
            ! is not a sign, a digit or a period)
            ! this can be the case when the string is 'NAN' or 'INFINITY' with/without sign
            ! or '.', '+.' or '-.'
            IF (PRESENT(ErrMsg)) ErrMsg = 'Invalid input: the first non-blank character is not a sign, a digit or a period.'
        END IF
        EndPos = Indx
        IF (PRESENT(Strict)) THEN
            IF (Strict.AND.Valid.AND.(EndPos <= StrLen)) THEN
                IF (.NOT.IsSeparator(cStr(EndPos:EndPos))) THEN
                    Valid = FalseVal
                    IF (PRESENT(ErrMsg)) ErrMsg = 'Input is NOT strictly valid.'
                END IF
            END IF
        END IF
        RETURN
    END IF

    ! check whether the current character is an exponent indicator
    IF (Indx <= StrLen) THEN
        DO
            SELECT CASE (cStr(Indx:Indx))
            CASE ('e', 'E', 'd', 'D', 'q', 'Q')
                Indx = Indx + 1
                ! check for a sign of the exponent
                IF (Indx <= StrLen) THEN
                    CurChr => cStr(Indx:Indx)
                    IF (CurChr == '-') THEN
                        StrInfo%ESign = -1
                        Indx = Indx + 1
                    ELSEIF (CurChr == '+') THEN
                        Indx = Indx + 1
                    END IF
                ELSE
                    Indx = Indx - 1
                    EXIT
                END IF
            CASE ('-')
                IF (StrInfo%DotFound) THEN
                    ! possibly real number with ABS(exponent) > 99
                    StrInfo%ESign = -1
                    Indx = Indx + 1
                ELSE
                    ! integer number - maybe integer number
                    EXIT
                END IF
            CASE ('+')
                IF (StrInfo%DotFound) THEN
                    ! possibly real number with ABS(exponent) > 99
                    Indx = Indx + 1
                ELSE
                    ! integer number + maybe integer number
                    EXIT
                END IF
            CASE DEFAULT
                EXIT
            END SELECT
            IF (Indx <= StrLen) THEN
                ! skip leading zero(s)
                IF (cStr(Indx:Indx) == '0') THEN
                    LeadZeroExpDigit = TrueVal
                    CALL SkipChars(cStr, '0', Indx, StrLen)
                END IF
                StrInfo%ExponentStart = Indx
                CALL SkipDecimalDigits(cStr, Indx, StrLen)
                StrInfo%ExponentEnd = Indx - 1
                IF (StrInfo%ExponentEnd < StrInfo%ExponentStart) THEN
                    IF (LeadZeroExpDigit) THEN
                        ! a zero exponent value
                        StrInfo%ExponentStart = StrInfo%ExponentEnd
                    ELSE
                        ! no exponent digits found so reset
                        StrInfo%ExponentStart = 0
                        StrInfo%ExponentEnd = 0
                    END IF
                END IF
            END IF
            EXIT
        END DO
    END IF

    ! set output
    Valid = TrueVal
    EndPos = Indx
    IF (PRESENT(ErrMsg)) ErrMsg = 'Valid Input: no error encountered.'
    IF (PRESENT(Strict)) THEN
        IF (Strict.AND.(EndPos <= StrLen)) THEN
            IF (.NOT.IsSeparator(cStr(EndPos:EndPos))) THEN
                Valid = FalseVal
                IF (PRESENT(ErrMsg)) ErrMsg = 'Input is NOT strictly valid.'
            END IF
        END IF
    END IF

    RETURN

END FUNCTION Scan_NumStr

!******************************************************************************

FUNCTION Handle_Invalid_NumStr(cStr, StartPos, Negative, EndPos) RESULT(NumType)

!** PURPOSE OF THIS SUBROUTINE:
    ! To handle invalid input string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, INTENT(IN)   :: cStr
    tSInt32,   INTENT(IN)   :: StartPos
    tLogical,  INTENT(IN)   :: Negative
    tSInt32,   INTENT(OUT)  :: EndPos
    tSInt32                 :: NumType

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32    :: FinishPos
    tSInt32    :: Ptr, Q

!** FLOW

    ! Could not parse a decimal floating-point number.  Start has been
    ! advanced over any leading spaces.
    Ptr = StartPos
    FinishPos = LEN(cStr)
    EndPos = StartPos
    IF (StartPos > FinishPos) THEN
        NumType = TYPE_NAN_INVALID
    ELSEIF (FinishPos == Ptr + 2) THEN
        IF ((ToUpperCase(cStr(Ptr:Ptr)) == 'N').AND.(ToUpperCase(cStr(Ptr+1:Ptr+1)) == 'A').AND. &
            (ToUpperCase(cStr(Ptr+2:Ptr+2)) == 'N')) THEN
            NumType = TYPE_NAN_EXACT
            EndPos = FinishPos + 1
        ELSE
            NumType = TYPE_NAN_INVALID
        END IF
    ELSE
        ! Try to parse Inf, maybe with a sign
        Q = Ptr
        IF (Q <= FinishPos) THEN
            ! this is actually already handled?
            IF (Is_Character_Sign(cStr(Q:Q))) Q = Q + 1
        END IF
        IF (FinishPos == Q + 2) THEN
            IF ((ToUpperCase(cStr(Q:Q)) == 'I').AND.(ToUpperCase(cStr(Q+1:Q+1)) == 'N').AND. &
                (ToUpperCase(cStr(Q+2:Q+2)) == 'F')) THEN
                IF (Negative) THEN
                    NumType = TYPE_INF_NEGATIVE
                    EndPos = FinishPos + 1
                ELSE
                    NumType = TYPE_INF_POSITIVE
                    EndPos = FinishPos + 1
                END IF
            ELSE
                NumType = TYPE_NAN_INVALID
            END IF
        ELSEIF (FinishPos == Q + 7) THEN
            IF ((ToUpperCase(cStr(Q:Q)) == 'I').AND.(ToUpperCase(cStr(Q+1:Q+1)) == 'N').AND. &
                (ToUpperCase(cStr(Q+2:Q+2)) == 'F').AND.(ToUpperCase(cStr(Q+3:Q+3)) == 'I').AND. &
                (ToUpperCase(cStr(Q+4:Q+4)) == 'N').AND.(ToUpperCase(cStr(Q+5:Q+5)) == 'I').AND. &
                (ToUpperCase(cStr(Q+6:Q+6)) == 'T').AND.(ToUpperCase(cStr(Q+7:Q+7)) == 'Y')) THEN
                IF (Negative) THEN
                    NumType = TYPE_INF_NEGATIVE
                    EndPos = FinishPos + 1
                ELSE
                    NumType = TYPE_INF_POSITIVE
                    EndPos = FinishPos + 1
                END IF
            ELSE
                NumType = TYPE_NAN_INVALID
            END IF
        ELSE
            NumType = TYPE_NAN_INVALID
        END IF
    END IF
    IF ((NumType == TYPE_NAN_INVALID).AND.(EndPos > 1)) THEN
        IF (Is_Character_Sign(cStr(EndPos-1:EndPos-1))) EndPos = EndPos - 1
    END IF

    RETURN

END FUNCTION Handle_Invalid_NumStr

!******************************************************************************

FUNCTION Convert_To_Int32(cStr, StartPos, EndPos) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the specified string to the 32-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, TARGET, INTENT(IN)   :: cStr
    tSInt32,           INTENT(IN)   :: StartPos
    tSInt32,           INTENT(IN)   :: EndPos
    tSInt32                         :: Number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: A0 = IACHAR('0')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Indx

!** FLOW
    
    Indx = StartPos
    DO WHILE (Indx <= EndPos)
        ! compute the value without checking if it will overflow
        ! we will check it after we process all the characters
        Number = Number*10 + (IACHAR(cStr(Indx:Indx))-A0)
        Indx = Indx + 1
    END DO

    RETURN

END FUNCTION Convert_To_Int32

!******************************************************************************

SUBROUTINE Handle_Overflow_Int32(cStr, StrInfo, Number, ErrMsg)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check and handle the overflow case of the 32-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! string representing a number
    TYPE(NumStrInfo),     INTENT(INOUT) :: StrInfo  !! string information
    tSInt32,              INTENT(INOUT) :: Number   !! output number
    tCharAlloc, OPTIONAL, INTENT(INOUT) :: ErrMsg   !! error message

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxDigit = 10
    tSInt32, PARAMETER  :: A4       = IACHAR('4')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: NumDigit
    tLogical    :: Overflow

!** FLOW

    ! get the number of digits used to convert the string to the number
    NumDigit = StrInfo%IntegralEnd - StrInfo%IntegralStart + 1
    IF (NumDigit < MaxDigit) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigit) THEN
        ! value might be in the applicable range
        IF (Number < 0) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((StrInfo%NegSign).AND.(Number == MIN_INT32)) THEN
                ! actually not overflow
                Overflow = FalseVal
                StrInfo%NegSign = TrueVal
            END IF
        ELSE
            IF (IACHAR(cStr(StrInfo%IntegralStart:StrInfo%IntegralStart)) < A4) THEN
                ! no overflow
                Overflow = FalseVal
            ELSE
                ! overflow
                Overflow = TrueVal
            END IF
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (StrInfo%NegSign) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MIN_INT32
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MAX_INT32
        END IF
    ELSE
        IF (StrInfo%NegSign) Number = -Number
    END IF

    RETURN

END SUBROUTINE Handle_Overflow_Int32

!******************************************************************************

FUNCTION Convert_To_Int64(cStr, StartPos, EndPos) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the specified string to the 64-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, TARGET, INTENT(IN)   :: cStr
    tSInt32,           INTENT(IN)   :: StartPos
    tSInt32,           INTENT(IN)   :: EndPos
    tSInt64                         :: Number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: A0 = IACHAR('0')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(C_PTR)             :: cPtr     ! C pointer to wStr
    tUInt64,     POINTER    :: wPtr     ! Fortran pointer to wStr
    tCharLen(8), TARGET     :: wStr     ! working string
    tSInt32                 :: Indx
    tSInt32                 :: IndxP7

!** FLOW

    ! get a C pointer to IBits
    cPtr = C_LOC(wStr)

    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, wPtr)

    ! initialize indices and the number
    Indx = StartPos
    IndxP7 = Indx + 7
    Number = 0_kInt64

    ! compute the number based on 8 digits at once
    DO WHILE (IndxP7 <= EndPos)
        ! set the working string
        wStr = cStr(Indx:IndxP7)
        ! process 8 digits at once
        Number = Number*100000000_kInt64 + Parse8Digits(wPtr)
        Indx   = Indx + 8
        IndxP7 = Indx + 7
    END DO

    ! nullify pointers
    NULLIFY(wPtr)
    cPtr = C_NULL_PTR

    ! compute the number based on 1 digit at a time
    DO WHILE (Indx <= EndPos)
        ! compute the value without checking if it will overflow
        ! we will check it after we process all the characters
        Number = Number*10_kInt64 + ToInt64(IACHAR(cStr(Indx:Indx))-A0)
        Indx = Indx + 1
    END DO

    RETURN

END FUNCTION Convert_To_Int64

!******************************************************************************

SUBROUTINE Handle_Overflow_Int64(cStr, StrInfo, Number, ErrMsg)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check and handle the overflow case of the 64-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! string representing a number
    TYPE(NumStrInfo),     INTENT(INOUT) :: StrInfo  !! string information
    tSInt64,              INTENT(INOUT) :: Number   !! output number
    tCharAlloc, OPTIONAL, INTENT(INOUT) :: ErrMsg   !! error message

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxDigit = 19

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: NumDigit
    tLogical    :: Overflow

!** FLOW

    ! get the number of digits used to convert the string to the number
    NumDigit = StrInfo%IntegralEnd - StrInfo%IntegralStart + 1
    IF (NumDigit < MaxDigit) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigit) THEN
        ! value might be in the applicable range
        IF (Number < 0_kInt64) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((StrInfo%NegSign).AND.(Number == MIN_INT64)) THEN
                ! actually not overflow
                Overflow = FalseVal
                StrInfo%NegSign = FalseVal
            END IF
        ELSE
            ! no overflow
            Overflow = FalseVal
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    IF (Overflow) THEN
        IF (StrInfo%NegSign) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MIN_INT64
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MAX_INT64
        END IF
    ELSE
        IF (StrInfo%NegSign) Number = -Number
    END IF

    RETURN

END SUBROUTINE Handle_Overflow_Int64

!******************************************************************************

FUNCTION Convert_To_Int128(cStr, StartPos, EndPos) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the specified string to the 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, TARGET, INTENT(IN)   :: cStr
    tSInt32,           INTENT(IN)   :: StartPos
    tSInt32,           INTENT(IN)   :: EndPos
    TYPE(SInt128)                   :: Number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: A0 = IACHAR('0')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(C_PTR)             :: cPtr     ! C pointer to wStr
    tUInt64,     POINTER    :: wPtr     ! Fortran pointer to wStr
    tCharLen(8), TARGET     :: wStr     ! working string
    tSInt32                 :: Indx
    tSInt32                 :: IndxP7

!** FLOW

    ! get a C pointer to IBits
    cPtr = C_LOC(wStr)

    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, wPtr)

    ! initialize indices and the number
    Indx = StartPos
    IndxP7 = Indx + 7
    Number = ZeroI128

    ! compute the number based on 8 digits at once
    DO WHILE (IndxP7 <= EndPos)
        ! set the working string
        wStr = cStr(Indx:IndxP7)
        ! process 8 digits at once where Number = Number*100000000 + Parse8Digits(wPtr)
        CALL Multiply(Number, 100000000)
        CALL Add(Number, Parse8Digits(wPtr))
        Indx   = Indx + 8
        IndxP7 = Indx + 7
    END DO

    ! nullify pointers
    NULLIFY(wPtr)
    cPtr = C_NULL_PTR

    ! compute the number based on 1 digit at a time
    DO WHILE (Indx <= EndPos)
        ! compute the value without checking if it will overflow
        ! we will check it after we process all the characters
        ! Number = Number*10 + (IACHAR(Chr)-A0)
        CALL Multiply(Number, 10)
        CALL Add(Number, (IACHAR(cStr(Indx:Indx))-A0))
        Indx = Indx + 1
    END DO

    RETURN

END FUNCTION Convert_To_Int128

!******************************************************************************

SUBROUTINE Handle_Overflow_Int128(cStr, StrInfo, Number, ErrMsg)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check and handle the overflow case of the 128-bit integer number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! string representing a number
    TYPE(NumStrInfo),     INTENT(INOUT) :: StrInfo  !! string information
    TYPE(SInt128),        INTENT(INOUT) :: Number   !! output number
    tCharAlloc, OPTIONAL, INTENT(INOUT) :: ErrMsg   !! error message

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxDigitI128 = 39
    tCharParam          :: MaxStr       = '170141183460469231731687303715884105727'

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(:), POINTER    :: CurStr
    tChar,       POINTER    :: CurChr
    tSInt32                 :: Indx
    tSInt32                 :: NumDigit
    tLogical                :: Overflow

!** FLOW

    ! get the number of digits used to convert the string to the number
    NumDigit = StrInfo%IntegralEnd - StrInfo%IntegralStart + 1
    IF (NumDigit < MaxDigitI128) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigitI128) THEN
        ! value might be in the applicable range
        IF (IsNegative(Number)) THEN
            ! overflow likely occurs
            Overflow = TrueVal
            IF ((StrInfo%NegSign).AND.(Number == MinI128)) THEN
                ! actually not overflow
                IF (ToDecString(MinI128) == cStr(StrInfo%IntegralStart-1:StrInfo%IntegralEnd)) THEN
                    Overflow = FalseVal
                    StrInfo%NegSign = FalseVal
                END IF
            END IF
        ELSE
            ! positive value so check overflow
            CurStr => cStr(StrInfo%IntegralStart:StrInfo%IntegralEnd)
            Overflow = FalseVal
            DO Indx = 1, MaxDigitI128
                CurChr => CurStr(Indx:Indx)
                IF (IACHAR(CurChr) < IACHAR(MaxStr(Indx:Indx))) THEN
                    EXIT
                ELSEIF (IACHAR(CurChr) > IACHAR(MaxStr(Indx:Indx))) THEN
                    Overflow = TrueVal
                    EXIT
                END IF
            END DO
        END IF
    ELSE
        ! value is out of the applicable range
        Overflow = TrueVal
    END IF
    ! nullify pointers
    NULLIFY(CurStr, CurChr)
    IF (Overflow) THEN
        IF (StrInfo%NegSign) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MinI128
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MaxI128
        END IF
    ELSE
        IF (StrInfo%NegSign) Number = -Number
    END IF

    RETURN

END SUBROUTINE Handle_Overflow_Int128

!******************************************************************************

SUBROUTINE Convert_To_FP32(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the specified string to the 32-bit floating-point number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, TARGET, INTENT(IN)       :: cStr
    TYPE(NumStrInfo),  INTENT(INOUT)    :: StrInfo  !! string information
    tUInt32,           INTENT(OUT)      :: SigDec   !! significand in base 10
    tSInt32,           INTENT(OUT)      :: ExpDec   !! exponent in base 10
    tLogical,          INTENT(OUT)      :: NegSign  !! true if negative sign
    TYPE(StringAux),   INTENT(OUT)      :: Aux      !! auxiliary information

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: A0            = IACHAR('0')
    tSInt32, PARAMETER  :: IBase         = 10
    tSInt32, PARAMETER  :: ExpLimit      = ToInt32(Z'10000000')
    tSInt32, PARAMETER  :: FP_Max_Digits = 9
    tUInt32, PARAMETER  :: MaxDivbyBase  = 429496729_kInt32

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32     :: SigLimit
    tSInt32     :: Indx

!** FLOW

    ! initialize
    SigDec         = 0_kInt32
    ExpDec         = 0
    NegSign        = StrInfo%NegSign
    Aux%SigCut     = 0
    Aux%Truncated  = FalseVal
    Aux%Start      = StrInfo%Start
    Aux%Indices(1) = StrInfo%IntegralStart
    Aux%Indices(2) = StrInfo%IntegralEnd
    Aux%Indices(3) = StrInfo%FractionStart
    Aux%Indices(4) = StrInfo%FractionEnd

    ! compute the exponent
    IF (StrInfo%ExponentStart > 0) THEN
        Indx = StrInfo%ExponentStart
        DO WHILE (Indx <= StrInfo%ExponentEnd)
            ExpDec = ExpDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
            IF (ExpDec > ExpLimit) EXIT
            Indx = Indx + 1
        END DO
    END IF

    ! check number of significant digits
    IF (StrInfo%SigCount <= FP_Max_Digits) THEN
        ! compute for the significand in the integral part without worrying about overflow
        CALL Add_Significand_Part(cStr, StrInfo%IntegralStart, StrInfo%IntegralStart, SigDec)
        ! compute for the significand in the fractional part
        CALL Add_Significand_Part(cStr, StrInfo%FractionStart, StrInfo%FractionStart, SigDec)
        ! determine exponent
        ExpDec = StrInfo%ESign*ExpDec - StrInfo%NFrac    
    ELSE
        ! must avoid overflow
        SigDec = 0_kInt32
        SigLimit = MaxDivbyBase - 10
        IF (StrInfo%IntegralStart > 0) THEN
            Indx = StrInfo%IntegralStart
            DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= StrInfo%IntegralEnd))
                SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                Indx = Indx + 1
            END DO
        END IF
        IF (SigDec .UGE. SigLimit) THEN    
            ! We have a big integer (but we can handle it) so determine exponent
            ExpDec = StrInfo%IntegralEnd + 1 - Indx + StrInfo%ESign*ExpDec
        ELSE                            
            ! We may have a value with a fractional component.
            IF (StrInfo%FractionStart > 0) THEN
                Indx = StrInfo%FractionStart
                DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= StrInfo%FractionEnd))
                    SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                    Indx = Indx + 1
                END DO
            END IF
            ! determine exponent
            ExpDec = StrInfo%FractionStart - Indx + StrInfo%ESign*ExpDec
        END IF
        ! We have now corrected both exponent and significand, to a truncated value
        Aux%Truncated  = TrueVal
        Aux%SigCut = Indx
    END IF

    RETURN

CONTAINS

    SUBROUTINE Add_Significand_Part(cStr, StartPos, EndPos, SigDec)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To add the specified significant part to the decimal significand.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, TARGET, INTENT(IN)       :: cStr
        tSInt32,           INTENT(IN)       :: StartPos
        tSInt32,           INTENT(IN)       :: EndPos
        tUInt32,           INTENT(INOUT)    :: SigDec

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Indx

    !** FLOW

        IF (StartPos > 0) THEN
            Indx = StartPos
            DO WHILE (Indx <= EndPos)
                ! add a digit to SigDec
                SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                Indx = Indx + 1
            END DO
        END IF

        RETURN

    END SUBROUTINE Add_Significand_Part

    !**************************************************************************

END SUBROUTINE Convert_To_FP32

!******************************************************************************

SUBROUTINE Convert_To_FP64(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the specified string to the 64-bit floating-point number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, TARGET, INTENT(IN)       :: cStr
    TYPE(NumStrInfo),  INTENT(INOUT)    :: StrInfo  !! string information
    tUInt64,           INTENT(OUT)      :: SigDec   !! significand in base 10
    tSInt32,           INTENT(OUT)      :: ExpDec   !! exponent in base 10
    tLogical,          INTENT(OUT)      :: NegSign  !! true if negative sign
    TYPE(StringAux),   INTENT(OUT)      :: Aux      !! auxiliary information

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: A0            = IACHAR('0')
    tSInt32, PARAMETER  :: IBase         = 10
    tSInt32, PARAMETER  :: ExpLimit      = ToInt32(Z'10000000')
    tSInt32, PARAMETER  :: FP_Max_Digits = 19
    tUInt64, PARAMETER  :: MaxDivbyBase  = 1844674407370955161_kInt64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: SigLimit
    tSInt32     :: Indx

!** FLOW

    ! initialize
    SigDec         = 0_kInt64
    ExpDec         = 0
    NegSign        = StrInfo%NegSign
    Aux%SigCut     = 0
    Aux%Truncated  = FalseVal
    Aux%Start      = StrInfo%Start
    Aux%Indices(1) = StrInfo%IntegralStart
    Aux%Indices(2) = StrInfo%IntegralEnd
    Aux%Indices(3) = StrInfo%FractionStart
    Aux%Indices(4) = StrInfo%FractionEnd

    ! compute the exponent
    IF (StrInfo%ExponentStart > 0) THEN
        Indx = StrInfo%ExponentStart
        DO WHILE (Indx <= StrInfo%ExponentEnd)
            ExpDec = ExpDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
            IF (ExpDec > ExpLimit) EXIT
            Indx = Indx + 1
        END DO
    END IF

    ! check number of significant digits
    IF (StrInfo%SigCount <= FP_Max_Digits) THEN
        ! compute for the significand in the integral part without worrying about overflow
        CALL Add_Significand_Part(cStr, StrInfo%IntegralStart, StrInfo%IntegralStart, SigDec)
        ! compute for the significand in the fractional part
        CALL Add_Significand_Part(cStr, StrInfo%FractionStart, StrInfo%FractionStart, SigDec)
        ! determine exponent
        ExpDec = StrInfo%ESign*ExpDec - StrInfo%NFrac    
    ELSE
        ! must avoid overflow
        SigDec = 0_kInt64
        SigLimit = MaxDivbyBase - 10
        IF (StrInfo%IntegralStart > 0) THEN
            Indx = StrInfo%IntegralStart
            DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= StrInfo%IntegralEnd))
                SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                Indx = Indx + 1
            END DO
        END IF
        IF (SigDec .UGE. SigLimit) THEN    
            ! We have a big integer (but we can handle it) so determine exponent
            ExpDec = StrInfo%IntegralEnd + 1 - Indx + StrInfo%ESign*ExpDec
        ELSE                            
            ! We may have a value with a fractional component.
            IF (StrInfo%FractionStart > 0) THEN
                Indx = StrInfo%FractionStart
                DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= StrInfo%FractionEnd))
                    SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                    Indx = Indx + 1
                END DO
            END IF
            ! determine exponent
            ExpDec = StrInfo%FractionStart - Indx + StrInfo%ESign*ExpDec
        END IF
        ! We have now corrected both exponent and significand, to a truncated value
        Aux%Truncated  = TrueVal
        Aux%SigCut = Indx
    END IF

    RETURN

CONTAINS

    SUBROUTINE Add_Significand_Part(cStr, StartPos, EndPos, SigDec)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To add the specified significant part to the decimal significand.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, TARGET, INTENT(IN)       :: cStr
        tSInt32,           INTENT(IN)       :: StartPos
        tSInt32,           INTENT(IN)       :: EndPos
        tUInt64,           INTENT(INOUT)    :: SigDec

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Indx

    !** FLOW

        IF (StartPos > 0) THEN
            Indx = StartPos
            DO WHILE (Indx <= EndPos)
                ! add a digit to SigDec
                SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                Indx = Indx + 1
            END DO
        END IF

        RETURN

    END SUBROUTINE Add_Significand_Part

    !**************************************************************************

END SUBROUTINE Convert_To_FP64

!******************************************************************************

SUBROUTINE Convert_To_FP128(cStr, StrInfo, SigDec, ExpDec, NegSign, Aux)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the specified string to the 128-bit floating-point number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar, TARGET, INTENT(IN)       :: cStr
    TYPE(NumStrInfo),  INTENT(INOUT)    :: StrInfo  !! string information
    TYPE(UInt128),     INTENT(OUT)      :: SigDec   !! significand in base 10
    tSInt32,           INTENT(OUT)      :: ExpDec   !! exponent in base 10
    tLogical,          INTENT(OUT)      :: NegSign  !! true if negative sign
    TYPE(StringAux),   INTENT(OUT)      :: Aux      !! auxiliary information

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,       PARAMETER    :: A0            = IACHAR('0')
    tSInt32,       PARAMETER    :: IBase         = 10
    tSInt32,       PARAMETER    :: ExpLimit      = ToInt32(Z'10000000')
    tSInt32,       PARAMETER    :: I64SafeDigits = 18
    tSInt64,       PARAMETER    :: TenPow18      = 10_kInt64**I64SafeDigits
    tSInt32,       PARAMETER    :: FP_Max_Digits = 39
    TYPE(UInt128), PARAMETER    :: MaxDivbyBase  = UInt128(ToInt64(Z'1999999999999999'), &
                                                           ToInt64(Z'9999999999999999'))

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)   :: SigLimit
    tSInt32         :: AddCount
    tSInt32         :: Indx

!** FLOW

    ! initialize
    SigDec         = ZeroU128
    ExpDec         = 0
    NegSign        = StrInfo%NegSign
    Aux%SigCut     = 0
    Aux%Truncated  = FalseVal
    Aux%Start      = StrInfo%Start
    Aux%Indices(1) = StrInfo%IntegralStart
    Aux%Indices(2) = StrInfo%IntegralEnd
    Aux%Indices(3) = StrInfo%FractionStart
    Aux%Indices(4) = StrInfo%FractionEnd

    ! compute the exponent
    IF (StrInfo%ExponentStart > 0) THEN
        Indx = StrInfo%ExponentStart
        DO WHILE (Indx <= StrInfo%ExponentEnd)
            ExpDec = ExpDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
            IF (ExpDec > ExpLimit) EXIT
            Indx = Indx + 1
        END DO
    END IF

    ! check number of significant digits
    IF (StrInfo%SigCount <= FP_Max_Digits) THEN
        ! initialize
        AddCount = 0
        ! compute for the significand in the integral part without worrying about overflow
        CALL Add_Significand_Part(cStr, StrInfo%IntegralStart, StrInfo%IntegralStart, SigDec, AddCount)
        ! compute for the significand in the fractional part
        CALL Add_Significand_Part(cStr, StrInfo%FractionStart, StrInfo%FractionStart, SigDec, AddCount)
        ! determine exponent
        ExpDec = StrInfo%ESign*ExpDec - StrInfo%NFrac    
    ELSE
        ! must avoid overflow
        SigDec = ZeroU128
        SigLimit = MaxDivbyBase - 10
        IF (StrInfo%IntegralStart > 0) THEN
            Indx = StrInfo%IntegralStart
            DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= StrInfo%IntegralEnd))
                SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                Indx = Indx + 1
            END DO
        END IF
        IF (SigDec .UGE. SigLimit) THEN    
            ! We have a big integer (but we can handle it) so determine exponent
            ExpDec = StrInfo%IntegralEnd + 1 - Indx + StrInfo%ESign*ExpDec
        ELSE                            
            ! We may have a value with a fractional component.
            IF (StrInfo%FractionStart > 0) THEN
                Indx = StrInfo%FractionStart
                DO WHILE ((SigDec .ULT. SigLimit).AND.(Indx <= StrInfo%FractionEnd))
                    SigDec = SigDec*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                    Indx = Indx + 1
                END DO
            END IF
            ! determine exponent
            ExpDec = StrInfo%FractionStart - Indx + StrInfo%ESign*ExpDec
        END IF
        ! We have now corrected both exponent and significand, to a truncated value
        Aux%Truncated  = TrueVal
        Aux%SigCut = Indx
    END IF

    RETURN

CONTAINS

    SUBROUTINE Add_Significand_Part(cStr, StartPos, EndPos, SigDec, AddCount)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To add the specified significant part to the decimal significand.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tCharStar, TARGET, INTENT(IN)       :: cStr
        tSInt32,           INTENT(IN)       :: StartPos
        tSInt32,           INTENT(IN)       :: EndPos
        TYPE(UInt128),     INTENT(INOUT)    :: SigDec
        tSInt32,           INTENT(INOUT)    :: AddCount

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Indx
        tSInt64     :: CurVal
        tSInt32     :: CurLen

    !** FLOW

        IF (StartPos > 0) THEN
            CurVal = 0_kInt64
            CurLen = 0
            Indx = StartPos
            DO WHILE (Indx <= EndPos)
                ! add a digit to CurVal
                CurVal = CurVal*IBase + (IACHAR(cStr(Indx:Indx))-A0)
                CurLen = CurLen +  1
                IF (CurLen == I64SafeDigits) THEN
                    ! add CurVal to SigDec
                    IF (AddCount > 0) THEN
                        SigDec = SigDec*TenPow18 + CurVal
                    ELSE
                        SigDec%Low = CurVal
                    END IF
                    ! reset
                    CurVal = 0_kInt64
                    CurLen = 0
                    ! update
                    AddCount = AddCount + 1
                END IF
                Indx = Indx + 1
            END DO
            IF (CurLen /= 0) THEN
                ! add CurVal to SigDec
                IF (AddCount > 0) THEN
                    SigDec = SigDec*(IBase**CurLen) + CurVal
                ELSE
                    SigDec%Low = CurVal
                END IF
                ! update
                AddCount = AddCount + 1
            END IF
        END IF

        RETURN

    END SUBROUTINE Add_Significand_Part

    !**************************************************************************

END SUBROUTINE Convert_To_FP128

!******************************************************************************

FUNCTION Convert_To_RealSP(cStr, SigDec, ExpDec, NegSign, Aux, D2BAlgo) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the specified input to the 32-bit real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,       INTENT(IN) :: cStr
    tUInt32,         INTENT(IN) :: SigDec   !! significand in base 10
    tSInt32,         INTENT(IN) :: ExpDec   !! exponent in base 10
    tLogical,        INTENT(IN) :: NegSign  !! true if negative sign
    TYPE(StringAux), INTENT(IN) :: Aux      !! auxiliary information
    tSInt32,         INTENT(IN) :: D2BAlgo
    !^ flag for decimal-to-binary algorithm <br>
    ! - D2B_FastFloat (or 1) if using the FastFloat algorithm. <br>
    ! - D2B_Lemire (or 2) if using the Lemire algorithm. <br>
    ! - D2B_LibC (or 3) if using the LibC algorithm. <br>
    ! - D2B_YY (or 4) if using the YY algorithm. <br>
    tRealSP                     :: Number   !! real number

!** SUBROUTINE PARAMETER DECLARATIONS:
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
    tSInt32, PARAMETER  :: Exponent_UppBound =  39      ! = 38 + 1
    tSInt32, PARAMETER  :: Exponent_LowBound = -54      ! = (-45) - 9

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32         :: RawVal
    tUInt32         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: SlowPath
    tRealSP         :: FloatVal
    EQUIVALENCE(RawVal, FloatVal)

!** FLOW

    ! set flag
    SlowPath = TrueVal

    ! If the exponent is too large and can't be represented in this size of
    ! float, return inf. These bounds are relatively loose, but are mostly
    ! serving as a first pass. Some close numbers getting through is okay.
    IF (ExpDec > Exponent_UppBound) THEN
        ! infinity
        SigBin = 0_kInt32
        ExpBin = MaxExponent
        SlowPath = FalseVal
    ! If the exponent is too small even for a subnormal, return 0.
    ELSEIF (ExpDec < Exponent_LowBound) THEN
        ! zero
        SigBin = 0_kInt32
        ExpBin = 0
        SlowPath = FalseVal
    ELSEIF (.NOT.Aux%Truncated) THEN
        IF (Dec2Bin_Clinger32(SigDec, ExpDec, SigBin, ExpBin)) THEN
            ! clinger's fast path is valid
            SlowPath = FalseVal
        END IF
    END IF

    ! perform decimal to binary conversion using the specified D2B algorithm if SlowPath is true
    IF (SlowPath) THEN
        SELECT CASE (D2BAlgo)
        CASE (D2B_FastFloat)
            BLOCK
                tUInt64     :: SigBin64
                CALL Dec2Bin_FastFloat32(ToInt64(SigDec), ExpDec, cStr, Aux%Truncated, Aux%Indices, &
                                         SigBin64, ExpBin)
                SigBin = ToInt32(SigBin64)
            END BLOCK
        CASE (D2B_Lemire)
            CALL Dec2Bin_LibC32(SigDec, ExpDec, cStr, Aux%Start, Aux%Truncated, SigBin, ExpBin)
        CASE (D2B_LibC)
            RawVal = Dec2Bin_YY32(SigDec, ExpDec, NegSign, cStr, Aux)
        CASE (D2B_YY)
            RawVal = Dec2Bin_Lemire32(SigDec, ExpDec, NegSign, cStr, Aux)
        END SELECT
    END IF

    ! construct raw binary representation of floating point number if needed
    IF ((.NOT.SlowPath).OR.(D2BAlgo == D2B_FastFloat).OR.(D2BAlgo == D2B_LibC)) THEN
        ! set sign bit
        IF (NegSign) THEN
            RawVal = SignMask
        ELSE
            RawVal = 0_kInt32
        END IF
        ! add exponent bits
        RawVal = IOR(RawVal, SHIFTL(ToInt32(ExpBin), SignificandBits))
        ! add (both implicit and explicit) significand bits
        RawVal = IOR(RawVal, IAND(SigBin, SignificandMask))
    END IF
    
    ! convert raw binary representation to floating point number
    Number = FloatVal

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

END FUNCTION Convert_To_RealSP

!******************************************************************************

FUNCTION Convert_To_RealDP(cStr, SigDec, ExpDec, NegSign, Aux, D2BAlgo) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the specified input to the 64-bit real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,       INTENT(IN) :: cStr
    tUInt64,         INTENT(IN) :: SigDec   !! significand in base 10
    tSInt32,         INTENT(IN) :: ExpDec   !! exponent in base 10
    tLogical,        INTENT(IN) :: NegSign  !! true if negative sign
    TYPE(StringAux), INTENT(IN) :: Aux      !! auxiliary information
    tSInt32,         INTENT(IN) :: D2BAlgo
    !^ flag for decimal-to-binary algorithm <br>
    ! - D2B_FastFloat (or 1) if using the FastFloat algorithm. <br>
    ! - D2B_Lemire (or 2) if using the Lemire algorithm. <br>
    ! - D2B_LibC (or 3) if using the LibC algorithm. <br>
    ! - D2B_YY (or 4) if using the YY algorithm. <br>
    tRealDP                     :: Number   !! real number

!** SUBROUTINE PARAMETER DECLARATIONS:
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
    tSInt32, PARAMETER  :: Exponent_UppBound =  309     ! = 308 + 1
    tSInt32, PARAMETER  :: Exponent_LowBound = -343     ! = (-324) - 19

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: RawVal
    tUInt64         :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: SlowPath
    tRealDP         :: FloatVal
    EQUIVALENCE(RawVal, FloatVal)

!** FLOW

    ! set flag
    SlowPath = TrueVal

    ! If the exponent is too large and can't be represented in this size of
    ! float, return inf. These bounds are relatively loose, but are mostly
    ! serving as a first pass. Some close numbers getting through is okay.
    IF (ExpDec > Exponent_UppBound) THEN
        ! infinity
        SigBin = 0_kInt64
        ExpBin = MaxExponent
        SlowPath = FalseVal
    ! If the exponent is too small even for a subnormal, return 0.
    ELSEIF (ExpDec < Exponent_LowBound) THEN
        ! zero
        SigBin = 0_kInt64
        ExpBin = 0
        SlowPath = FalseVal
    ELSEIF (.NOT.Aux%Truncated) THEN
        IF (Dec2Bin_Clinger64(SigDec, ExpDec, SigBin, ExpBin)) THEN
            ! clinger's fast path is valid
            SlowPath = FalseVal
        END IF
    END IF

    ! perform decimal to binary conversion using the specified D2B algorithm if SlowPath is true
    IF (SlowPath) THEN
        SELECT CASE (D2BAlgo)
        CASE (D2B_FastFloat)
            CALL Dec2Bin_FastFloat64(SigDec, ExpDec, cStr, Aux%Truncated, Aux%Indices, SigBin, ExpBin)
        CASE (D2B_Lemire)
            CALL Dec2Bin_LibC64(SigDec, ExpDec, cStr, Aux%Start, Aux%Truncated, SigBin, ExpBin)
        CASE (D2B_LibC)
            RawVal = Dec2Bin_YY64(SigDec, ExpDec, NegSign, cStr, Aux)
        CASE (D2B_YY)
            RawVal = Dec2Bin_Lemire64(SigDec, ExpDec, NegSign, cStr, Aux)
        END SELECT
    END IF

    ! construct raw binary representation of floating point number if needed
    IF ((.NOT.SlowPath).OR.(D2BAlgo == D2B_FastFloat).OR.(D2BAlgo == D2B_LibC)) THEN
        ! set sign bit
        IF (NegSign) THEN
            RawVal = SignMask
        ELSE
            RawVal = 0_kInt64
        END IF
        ! add exponent bits
        RawVal = IOR(RawVal, SHIFTL(ToUnsignedLong(ExpBin), SignificandBits))
        ! add (both implicit and explicit) significand bits
        RawVal = IOR(RawVal, IAND(SigBin, SignificandMask))
    END IF
    
    ! convert raw binary representation to floating point number
    Number = FloatVal

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

END FUNCTION Convert_To_RealDP

!******************************************************************************

FUNCTION Convert_To_RealQP(cStr, SigDec, ExpDec, NegSign, Aux, D2BAlgo) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the specified input to the 128-bit real number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,       INTENT(IN) :: cStr
    TYPE(UInt128),   INTENT(IN) :: SigDec   !! significand in base 10
    tSInt32,         INTENT(IN) :: ExpDec   !! exponent in base 10
    tLogical,        INTENT(IN) :: NegSign  !! true if negative sign
    TYPE(StringAux), INTENT(IN) :: Aux      !! auxiliary information
    tSInt32,         INTENT(IN) :: D2BAlgo
    !^ flag for decimal-to-binary algorithm <br>
    ! - D2B_FastFloat (or 1) if using the FastFloat algorithm. <br>
    ! - D2B_Lemire (or 2) if using the Lemire algorithm. <br>
    ! - D2B_LibC (or 3) if using the LibC algorithm. <br>
    ! - D2B_YY (or 4) if using the YY algorithm. <br>
    tRealQP                     :: Number   !! real number

!** SUBROUTINE PARAMETER DECLARATIONS:
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
    tSInt32,  PARAMETER :: Exponent_UppBound =  4933    ! = 4932 + 1
    tSInt32,  PARAMETER :: Exponent_LowBound = -5005    ! = (-4966) - 39

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(UInt128)   :: RawVal
    TYPE(UInt128)   :: SigBin       ! (unbiased) significand in base 2
    tUInt32         :: ExpBin       ! (unbiased) exponent in base 2
    tLogical        :: SlowPath
    tUInt64         :: IntVal(2)
    tRealQP         :: FloatVal
    EQUIVALENCE(IntVal, FloatVal)

!** FLOW

    ! set flag
    SlowPath = TrueVal

    ! If the exponent is too large and can't be represented in this size of
    ! float, return inf. These bounds are relatively loose, but are mostly
    ! serving as a first pass. Some close numbers getting through is okay.
    IF (ExpDec > Exponent_UppBound) THEN
        ! infinity
        SigBin = ZeroU128
        ExpBin = MaxExponent
        SlowPath = FalseVal
    ! If the exponent is too small even for a subnormal, return 0.
    ELSEIF (ExpDec < Exponent_LowBound) THEN
        ! zero
        SigBin = ZeroU128
        ExpBin = 0
        SlowPath = FalseVal
    ELSEIF (.NOT.Aux%Truncated) THEN
        IF (Dec2Bin_Clinger128(SigDec, ExpDec, SigBin, ExpBin)) THEN
            ! clinger's fast path is valid
            SlowPath = FalseVal
        END IF
    END IF

    ! perform decimal to binary conversion using the specified D2B algorithm if SlowPath is true
    IF (SlowPath) THEN
        SELECT CASE (D2BAlgo)
        CASE (D2B_FastFloat)
            CALL Dec2Bin_FastFloat128(SigDec, ExpDec, cStr, Aux%Truncated, Aux%Indices, SigBin, ExpBin)
        CASE (D2B_Lemire)
            CALL Dec2Bin_LibC128(SigDec, ExpDec, cStr, Aux%Start, Aux%Truncated, SigBin, ExpBin)
        CASE (D2B_LibC)
            RawVal = Dec2Bin_YY128(SigDec, ExpDec, NegSign, cStr, Aux)
        CASE (D2B_YY)
            RawVal = Dec2Bin_Lemire128(SigDec, ExpDec, NegSign, cStr, Aux)
        END SELECT
    END IF

    ! construct raw binary representation of floating point number if needed
    IF ((.NOT.SlowPath).OR.(D2BAlgo == D2B_FastFloat).OR.(D2BAlgo == D2B_LibC)) THEN
        ! set sign bit
        IF (NegSign) THEN
            RawVal = SignMask
        ELSE
            RawVal = ZeroU128
        END IF
        ! add exponent bits
        RawVal = IOR(RawVal, SHIFTL(UInt128(ExpBin), SignificandBits))
        ! add (both implicit and explicit) significand bits
        RawVal = IOR(RawVal, IAND(SigBin, SignificandMask))
    END IF
    
    ! convert raw binary representation to floating point number
    IF (IsLittleEndian) THEN
        IntVal(1) = RawVal%Low
        IntVal(2) = RawVal%High
    ELSE
        IntVal(1) = RawVal%High
        IntVal(2) = RawVal%Low
    END IF
    Number = FloatVal

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

END FUNCTION Convert_To_RealQP

!******************************************************************************

FUNCTION Parse8Digits(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To parse eight digits immediately.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: InVal
    tUInt64             :: OutVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt64, PARAMETER  :: K1 = ToInt64(Z'0F0F0F0F0F0F0F0F')
    tUInt64, PARAMETER  :: K2 = ToInt64(Z'00FF00FF00FF00FF')
    tUInt64, PARAMETER  :: K3 = ToInt64(Z'0000FFFF0000FFFF')
    tUInt64, PARAMETER  :: M1 = 2561_kInt64
    tUInt64, PARAMETER  :: M2 = 6553601_kInt64
    tUInt64, PARAMETER  :: M3 = 42949672960001_kInt64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = SHIFTR(IAND(SHIFTR(IAND(SHIFTR(IAND(InVal, K1)*M1, 8), K2)*M2, 16), K3)*M3, 32)

    RETURN

END FUNCTION Parse8Digits

!******************************************************************************

FUNCTION IsSeparator(C) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the specified character is a separator or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tChar, INTENT(IN)   :: C
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ((C == CHR_SPACE).OR.(C == CHR_COMMA).OR. &
            (C == CHR_NEWLINE).OR.(C == CHR_SLASH).OR. &
            (C == CHR_TAB).OR.(C == CHR_CARRIAGE_RETURN).OR. &
            (C == CHR_SEMICOLON))

    RETURN

END FUNCTION IsSeparator

!******************************************************************************

END MODULE MBase_ReadUtil

!******************************************************************************
