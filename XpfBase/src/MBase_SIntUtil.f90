
MODULE MBase_SIntUtil

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains various utility routines relating to (signed) integers.

!** USE STATEMENTS:
    USE MBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! parameters
    PUBLIC :: MAX_I8, MAX_I16, MAX_I32, MAX_I64
    PUBLIC :: MIN_I8, MIN_I16, MIN_I32, MIN_I64
    ! procedures
    PUBLIC :: Compare
    PUBLIC :: HighestOneBit
    PUBLIC :: LowestOneBit
    PUBLIC :: ReverseBits
    PUBLIC :: ReverseBytes
    PUBLIC :: SigNum
    PUBLIC :: ToHexStrSigned
    PUBLIC :: ToDecStrSigned
    PUBLIC :: I32_FromChar
    PUBLIC :: I64_FromChar

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! maximum values
    tSInt8,      PARAMETER  :: MAX_I8  = ToInt8(Z'7F')                 !! 127
    tSInt16,     PARAMETER  :: MAX_I16 = ToInt16(Z'7FFF')              !! 32767
    tSInt32,     PARAMETER  :: MAX_I32 = ToInt32(Z'7FFFFFFF')          !! 2147483647
    tSInt64,     PARAMETER  :: MAX_I64 = ToInt64(Z'7FFFFFFFFFFFFFFF')  !! 9223372036854775807
    ! minimum values
    tSInt8,      PARAMETER  :: MIN_I8  = ToInt8(Z'80')                 !! -128
    tSInt16,     PARAMETER  :: MIN_I16 = ToInt16(Z'8000')              !! -32768
    tSInt32,     PARAMETER  :: MIN_I32 = ToInt32(Z'80000000')          !! -2147483648
    tSInt64,     PARAMETER  :: MIN_I64 = ToInt64(Z'8000000000000000')  !! -9223372036854775808
    ! tables of digit characters
    tCharLen(2), PARAMETER  :: Char2Digits(0:99) = [                &
        '00', '01', '02', '03', '04', '05', '06', '07', '08', '09', &
        '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', &
        '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', &
        '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', &
        '40', '41', '42', '43', '44', '45', '46', '47', '48', '49', &
        '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', &
        '60', '61', '62', '63', '64', '65', '66', '67', '68', '69', &
        '70', '71', '72', '73', '74', '75', '76', '77', '78', '79', &
        '80', '81', '82', '83', '84', '85', '86', '87', '88', '89', &
        '90', '91', '92', '93', '94', '95', '96', '97', '98', '99']

!** DERIVED TYPE DEFINITIONS
    ! na

!** INTERFACE/GENERIC DEFINITIONS:
    INTERFACE Compare
        !^ **Function Interface**: Compare <br>
        !  **Purpose**:  To compare two signed integers of the same kind and return <br>
        !   -1 if LHS < RHS <br>
        !    0 if LHS == RHS <br>
        !    1 if LHS > RHS <br>
        !  **Usage**: <br>
        !   --->    Flag = Compare(LHS, RHS) <br>
        !   --->    IF (Compare(LHS, RHS) /= 0) DoSomething
        MODULE PROCEDURE    I8_Compare
        MODULE PROCEDURE    I16_Compare
        MODULE PROCEDURE    I32_Compare
        MODULE PROCEDURE    I64_Compare
    END INTERFACE
    INTERFACE HighestOneBit
        !^ **Function Interface**: HighestOneBit <br>
        !  **Purpose**:  To return an integer value (same kind as the specified integer value) with at most
        !   a single one-bit, in the position of the highest-order (leftmost) one-bit in the input. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = HighestOneBit(INPUT)
        MODULE PROCEDURE    I32_HighestOneBit
        MODULE PROCEDURE    I64_HighestOneBit
    END INTERFACE
    INTERFACE LowestOneBit
        !^ **Function Interface**: LowestOneBit <br>
        !  **Purpose**:  To return an integer value (same kind as the specified integer value) with at most
        !   a single one-bit, in the position of the lowest-order (rightmost) one-bit in the input. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = LowestOneBit(INPUT)
        MODULE PROCEDURE    I32_LowestOneBit
        MODULE PROCEDURE    I64_LowestOneBit
    END INTERFACE
    INTERFACE ReverseBits
        !^ **Function Interface**: ReverseBits <br>
        !  **Purpose**:  To return an integer value (same kind as the specified integer value) obtained by
        !    reversing the order of the bits in the two's complement binary representation of the input. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = ReverseBits(INPUT)
        MODULE PROCEDURE    I32_ReverseBits
        MODULE PROCEDURE    I64_ReverseBits
    END INTERFACE
    INTERFACE ReverseBytes
        !^ **Function Interface**: ReverseBits <br>
        !  **Purpose**:  To return an integer value (same kind as the specified integer value) obtained by
        !    reversing the order of the bytes in the two's complement binary representation of the input. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = ReverseBytes(INPUT)
        MODULE PROCEDURE    I32_ReverseBytes
        MODULE PROCEDURE    I64_ReverseBytes
    END INTERFACE
    INTERFACE SigNum
        !^ **Function Interface**: SigNum <br>
        !  **Purpose**:  To return the sign of the specified input where the return value is <br>
        !   -1 if the specified value is negative <br>
        !    0 if the specified value is zero <br>
        !    1 if the specified value is positive <br>
        !  **Usage**: <br>
        !   --->    SignFlag = SigNum(INPUT)
        MODULE PROCEDURE    I32_SigNum
        MODULE PROCEDURE    I64_SigNum
    END INTERFACE
    INTERFACE ToHexStrSigned
        !^ **Function Interface**: ToHexStrSigned <br>
        !  **Purpose**:  To convert a signed integer to a hexadecimal string <br>
        !  **Usage**: <br>
        !   --->    Str = ToHexStrSigned(I32)
        MODULE PROCEDURE    I32_ToHexString
        MODULE PROCEDURE    I64_ToHexString
    END INTERFACE
    INTERFACE ToDecStrSigned
        !^ **Function Interface**: ToDecStrSigned <br>
        !  **Purpose**:  To convert a signed integer to a decimal string <br>
        !  **Usage**: <br>
        !   --->    Str = ToDecStrSigned(I64)
        MODULE PROCEDURE    I32_ToDecString
        MODULE PROCEDURE    I64_ToDecString
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                               comparison
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION I8_Compare(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS. <br>
    ! - return -1 if LHS < RHS <br>
    ! - return  0 if LHS == RHS <br>
    ! - return +1 if LHS > RHS

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt8, INTENT(IN)  :: LHS, RHS
    tSInt32             :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS < RHS) THEN
        Flag = -1
    ELSEIF (LHS > RHS) THEN
        Flag = +1
    ELSE
        Flag = 0
    END IF

    RETURN

END FUNCTION I8_Compare

!******************************************************************************

PURE FUNCTION I16_Compare(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS. <br>
    ! - return -1 if LHS < RHS <br>
    ! - return  0 if LHS == RHS <br>
    ! - return +1 if LHS > RHS

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt16, INTENT(IN) :: LHS, RHS
    tSInt32             :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS < RHS) THEN
        Flag = -1
    ELSEIF (LHS > RHS) THEN
        Flag = +1
    ELSE
        Flag = 0
    END IF

    RETURN

END FUNCTION I16_Compare

!******************************************************************************

PURE FUNCTION I32_Compare(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS. <br>
    ! - return -1 if LHS < RHS <br>
    ! - return  0 if LHS == RHS <br>
    ! - return +1 if LHS > RHS

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: LHS, RHS
    tSInt32             :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS < RHS) THEN
        Flag = -1
    ELSEIF (LHS > RHS) THEN
        Flag = +1
    ELSE
        Flag = 0
    END IF

    RETURN

END FUNCTION I32_Compare

!******************************************************************************

PURE FUNCTION I64_Compare(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS. <br>
    ! - return -1 if LHS < RHS <br>
    ! - return  0 if LHS == RHS <br>
    ! - return +1 if LHS > RHS

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: LHS, RHS
    tSInt32             :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (LHS < RHS) THEN
        Flag = -1
    ELSEIF (LHS > RHS) THEN
        Flag = +1
    ELSE
        Flag = 0
    END IF

    RETURN

END FUNCTION I64_Compare

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                      Bitwise-Related Procedures
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION I64_HighestOneBit(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an 64-bit integer value with at most a single one-bit, in the
    ! position of the highest-order ("leftmost") one-bit in the specified
    ! 64-bit integer value.  To return zero if the specified value has no
    ! one-bits in its two's complement binary representation, that is, if it
    ! is equal to zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: InVal
    tSInt64             :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(InVal, SHIFTR(MIN_I64, LEADZ(InVal)))

    RETURN

END FUNCTION I64_HighestOneBit

!******************************************************************************

PURE FUNCTION I32_HighestOneBit(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an 32-bit integer value with at most a single one-bit, in the
    ! position of the highest-order ("leftmost") one-bit in the specified
    ! 32-bit integer value.  To return zero if the specified value has no
    ! one-bits in its two's complement binary representation, that is, if it
    ! is equal to zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: InVal
    tSInt32             :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(InVal, SHIFTR(MIN_I32, LEADZ(InVal)))

    RETURN

END FUNCTION I32_HighestOneBit

!******************************************************************************

PURE FUNCTION I64_LowestOneBit(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an 64-bit integer value with at most a single one-bit, in the
    ! position of the lowest-order ("rightmost") one-bit in the specified
    ! 64-bit integer value.  To return zero if the specified value has no
    ! one-bits in its two's complement binary representation, that is, if it
    ! is equal to zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: InVal
    tSInt64             :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(InVal, -InVal)

    RETURN

END FUNCTION I64_LowestOneBit

!******************************************************************************

PURE FUNCTION I32_LowestOneBit(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an 32-bit integer value with at most a single one-bit, in the
    ! position of the lowest-order ("rightmost") one-bit in the specified
    ! 32-bit integer value.  To return zero if the specified value has no
    ! one-bits in its two's complement binary representation, that is, if it
    ! is equal to zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: InVal
    tSInt32             :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(InVal, -InVal)

    RETURN

END FUNCTION I32_LowestOneBit

!******************************************************************************

PURE FUNCTION I64_ReverseBits(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an 64-bit integer value obtained by reversing the order of the
    ! bits in the two's complement binary representation of the specified value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: InVal
    tSInt64             :: OutVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER  :: C1 = ToInt64(Z'5555555555555555')
    tSInt64, PARAMETER  :: C2 = ToInt64(Z'3333333333333333')
    tSInt64, PARAMETER  :: C3 = ToInt64(Z'0F0F0F0F0F0F0F0F')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IOR(SHIFTL(IAND(InVal,  C1), 1), IAND(SHIFTR(InVal,  1), C1))
    OutVal = IOR(SHIFTL(IAND(OutVal, C2), 2), IAND(SHIFTR(OutVal, 2), C2))
    OutVal = IOR(SHIFTL(IAND(OutVal, C3), 4), IAND(SHIFTR(OutVal, 4), C3))

    OutVal = ReverseBytes(OutVal)

    RETURN

END FUNCTION I64_ReverseBits

!******************************************************************************
PURE FUNCTION I32_ReverseBits(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an 32-bit integer value obtained by reversing the order of the
    ! bits in the two's complement binary representation of the specified value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: InVal
    tSInt32             :: OutVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: C1 = ToInt32(Z'55555555')
    tSInt32, PARAMETER  :: C2 = ToInt32(Z'33333333')
    tSInt32, PARAMETER  :: C3 = ToInt32(Z'0F0F0F0F')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IOR(SHIFTL(IAND(InVal,  C1), 1), IAND(SHIFTR(InVal,  1), C1))
    OutVal = IOR(SHIFTL(IAND(OutVal, C2), 2), IAND(SHIFTR(OutVal, 2), C2))
    OutVal = IOR(SHIFTL(IAND(OutVal, C3), 4), IAND(SHIFTR(OutVal, 4), C3))

    OutVal = ReverseBytes(OutVal)

    RETURN

END FUNCTION I32_ReverseBits

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                      Miscellaneous Procedures
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION I64_ReverseBytes(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an 64-bit integer value obtained by reversing the order of the
    ! bytes in the two's complement representation of the specified value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: InVal
    tSInt64             :: OutVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER  :: C1 = ToInt64(Z'00FF00FF00FF00FF')
    tSInt64, PARAMETER  :: C2 = ToInt64(Z'00000000FFFF0000')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IOR(SHIFTL(IAND(InVal, C1), 8), IAND(SHIFTR(InVal, 8), C1))
    OutVal = IOR(IOR(IOR(SHIFTL(OutVal, 48), SHIFTL(IAND(OutVal, C2), 16)), &
                     IAND(SHIFTR(OutVal, 16), C2)), SHIFTR(OutVal, 48))

    ! alternative implementation
!    OutVal = 0
!    CALL MVBITS(InVal, 56, 8, OutVal,  0)
!    CALL MVBITS(InVal, 48, 8, OutVal,  8)
!    CALL MVBITS(InVal, 40, 8, OutVal, 16)
!    CALL MVBITS(InVal, 32, 8, OutVal, 24)
!    CALL MVBITS(InVal, 24, 8, OutVal, 32)
!    CALL MVBITS(InVal, 16, 8, OutVal, 40)
!    CALL MVBITS(InVal,  8, 8, OutVal, 48)
!    CALL MVBITS(InVal,  0, 8, OutVal, 56)

    RETURN

END FUNCTION I64_ReverseBytes

!******************************************************************************

PURE FUNCTION I32_ReverseBytes(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return an 32-bit integer value obtained by reversing the order of the
    ! bytes in the two's complement representation of the specified value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: InVal
    tSInt32             :: OutVal

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: C = ToInt32(Z'0000FF00')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IOR(IOR(IOR(SHIFTL(InVal, 24), SHIFTL(IAND(InVal, C), 8)), &
                     IAND(SHIFTR(InVal, 8), C)), SHIFTR(InVal, 24))

    ! alternative implementation
!    OutVal = 0
!    CALL MVBITS(InVal, 24, 8, OutVal,  0)
!    CALL MVBITS(InVal, 16, 8, OutVal,  8)
!    CALL MVBITS(InVal,  8, 8, OutVal, 16)
!    CALL MVBITS(InVal,  0, 8, OutVal, 24)

    RETURN

END FUNCTION I32_ReverseBytes

!******************************************************************************

PURE FUNCTION I64_SigNum(Val) RESULT(Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the signum function of the specified value where the
    ! return value is -1 if the specified value is negative; 0 if the
    ! specified value is zero; and 1 if the specified value is positive.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Val
    tSInt32             :: Sign

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Sign = ToInt32(IOR(SHIFTA(Val, 63), SHIFTR(-Val, 63)))

    RETURN

END FUNCTION I64_SigNum

!******************************************************************************
PURE FUNCTION I32_SigNum(Val) RESULT(Sign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the signum function of the specified value where the
    ! return value is -1 if the specified value is negative; 0 if the
    ! specified value is zero; and 1 if the specified value is positive.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: Val
    tSInt32             :: Sign

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Sign = IOR(SHIFTA(Val, 31), SHIFTR(-Val, 31))

    RETURN

END FUNCTION I32_SigNum

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   Conversion To Hexadecimal String
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION I64_ToHexString(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 64-bit integer number to a hexadecimal string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! hexadecimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxLen = 16
    tSInt64, PARAMETER  :: Base   = 16_kInt64
    tSInt32, PARAMETER  :: Shift  = 4
    tCharParam          :: NumStr(0:15) = ['0', '1', '2', '3', '4', '5', '6', '7', &
                                           '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr     ! working string
    tSInt64             :: PosNum   ! positive number (working number)
    tSInt64             :: CurNum   ! current (saved) working number
    tSInt64             :: RemNum   ! remainder number
    tSInt32             :: Indx

!** FLOW

    ! check whether the number is zero
    IF (Number == 0_kInt64) THEN
        cStr = '0'
        RETURN
    END IF
    IF (Number < 0_kInt64) THEN
        IF (Number == MIN_I64) THEN
            cStr = '-8000000000000000'
            RETURN
        END IF
        PosNum = ABS(Number)
    ELSE
        PosNum = Number
    END IF
    Indx = MaxLen

    ! start the conversion
    DO
        ! save current number
        CurNum = PosNum
        ! compute the next round of working number
        PosNum = SHIFTR(PosNum, Shift)
        ! compute the remainder
        RemNum = CurNum - SHIFTL(PosNum, Shift)
        ! convert the remainder to a working string
        wStr(Indx:Indx) = NumStr(RemNum)
        Indx = Indx - 1
        IF (PosNum == 0_kInt64) EXIT
    END DO

    ! allocate the resulting string and transfer
    ! characters from the working string
    Indx = Indx + 1
    IF (Number < 0_kInt64) THEN
        cStr = '-' // wStr(Indx:MaxLen)
    ELSE
        cStr = wStr(Indx:MaxLen)
    END IF

    RETURN

END FUNCTION I64_ToHexString

!******************************************************************************

PURE FUNCTION I32_ToHexString(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a signed 32-bit integer number to a hexadecimal string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! hexadecimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxLen = 8
    tSInt32, PARAMETER  :: Base   = 16
    tSInt32, PARAMETER  :: Shift  = 4
    tCharParam          :: NumStr(0:15) = ['0', '1', '2', '3', '4', '5', '6', '7', &
                                           '8', '9', 'A', 'B', 'C', 'D', 'E', 'F']

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr     ! working string
    tSInt32             :: PosNum   ! positive number (working number)
    tSInt32             :: CurNum   ! current (saved) working number
    tSInt32             :: RemNum   ! remainder number
    tSInt32             :: Indx

!** FLOW

    ! check whether the number is zero
    IF (Number == 0) THEN
        cStr = '0'
        RETURN
    END IF
    IF (Number < 0) THEN
        IF (Number == MIN_I32) THEN
            cStr = '-80000000'
            RETURN
        END IF
        PosNum = ABS(Number)
    ELSE
        PosNum = Number
    END IF
    Indx = MaxLen

    ! start the conversion
    DO
        ! save current number
        CurNum = PosNum
        ! compute the next round of working number
        PosNum = SHIFTR(PosNum, Shift)
        ! compute the remainder
        RemNum = CurNum - SHIFTL(PosNum, Shift)
        ! convert the remainder to a working string
        wStr(Indx:Indx) = NumStr(RemNum)
        Indx = Indx - 1
        IF (PosNum == 0) EXIT
    END DO

    ! allocate the resulting string and transfer
    ! characters from the working string
    Indx = Indx + 1
    IF (Number < 0) THEN
        cStr = '-' // wStr(Indx:MaxLen)
    ELSE
        cStr = wStr(Indx:MaxLen)
    END IF

    RETURN

END FUNCTION I32_ToHexString

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   Conversion To Decimal String
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION I32_ToDecString(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an integer number to character string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxLen = 10

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr
    tSInt32             :: PosNum
    tSInt32             :: Finish, Start
    tSInt32             :: AA, BB, CC, DD
    tSInt32             :: AABB, BBCC, CCDD
    tSInt32             :: AABBCC, DDEE, EE

!** FLOW

    ! set positive number
    PosNum = ABS(Number)

    ! start the conversion
    IF (PosNum < 100) THEN              ! 1-2 digits
        wStr(1:2) = Char2Digits(PosNum)
        Finish = 2
    ELSEIF (PosNum < 10000) THEN        ! 3-4 digits
        AA = ToInt32(SHIFTR(PosNum*5243, 19))                   ! PosNum / 100
        BB = PosNum - AA*100                                    ! MOD(PosNum, 100)
        wStr(1:2) = Char2Digits(AA)
        wStr(3:4) = Char2Digits(BB)
        Finish = 4
    ELSEIF (PosNum < 1000000) THEN      ! 5-6 digits
        AA = ToInt32(SHIFTR(PosNum*429497_kInt64, 32))          ! PosNum / 10000
        BBCC = PosNum - AA*10000                                ! MOD(PosNum, 10000)
        BB = SHIFTR(BBCC*5243, 19)                              ! BBCC / 100
        CC = BBCC - BB*100                                      ! MOD(BBCC, 100)
        wStr(1:2) = Char2Digits(AA)
        wStr(3:4) = Char2Digits(BB)
        wStr(5:6) = Char2Digits(CC)
        Finish = 6
    ELSEIF (PosNum < 100000000) THEN    ! 7-8 digits
        AABB = ToInt32(SHIFTR(PosNum*109951163_kInt64, 40))     ! PosNum / 10000
        CCDD = PosNum - AABB*10000                              ! MOD(PosNum, 10000)
        AA = SHIFTR(AABB*5243, 19)                              ! AABB / 100
        CC = SHIFTR(CCDD*5243, 19)                              ! CCDD / 100
        BB = AABB - AA*100                                      ! MOD(AABB, 100)
        DD = CCDD - CC*100                                      ! MOD(CCDD, 100)
        wStr(1:2) = Char2Digits(AA)
        wStr(3:4) = Char2Digits(BB)
        wStr(5:6) = Char2Digits(CC)
        wStr(7:8) = Char2Digits(DD)
        Finish = 8
    ELSE                                ! 9-10 digits
        AABBCC = ToInt32(SHIFTR(PosNum*3518437209_kInt64, 45))  ! PosNum / 10000
        AA   = ToInt32(SHIFTR(AABBCC*429497_kInt64, 32))        ! aabbcc / 10000
        DDEE = PosNum - AABBCC*10000                            ! MOD(PosNum, 10000)
        BBCC = AABBCC - AA*10000                                ! MOD(aabbcc, 10000)
        BB = SHIFTR(BBCC*5243, 19)                              ! bbcc / 100
        DD = SHIFTR(DDEE*5243, 19)                              ! ddee / 100
        CC = BBCC - BB*100                                      ! MOD(bbcc, 100)
        EE = DDEE - DD*100                                      ! MOD(ddee, 100)
        wStr(1:2)  = Char2Digits(AA)
        wStr(3:4)  = Char2Digits(BB)
        wStr(5:6)  = Char2Digits(CC)
        wStr(7:8)  = Char2Digits(DD)
        wStr(9:10) = Char2Digits(EE)
        Finish = 10
    END IF

    Start = 1
    IF (wStr(1:1) == '0') Start = 2

    IF (Number < 0) THEN
        IF (Number == MIN_I32) THEN
            cStr = '-2147483648'
            RETURN
        END IF
        cStr = '-' // wStr(Start:Finish)
    ELSE
        cStr = wStr(Start:Finish)
    END IF

    RETURN

END FUNCTION I32_ToDecString

!******************************************************************************

FUNCTION I64_ToDecString(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an integer number to character string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! character string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxLen = 20

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tCharLen(MaxLen)    :: wStr
    tSInt64             :: PosNum, TmpNum, HiNum, LoNum, MidNum
    tSInt32             :: Start

!** FLOW

    ! set positive number
    PosNum = ABS(Number)

    ! start conversion and store digits in working string
    IF (PosNum < 100000000_kInt64) THEN                                 ! 1-8 digits
        Start = 12 + Write_1_to_8_Digits(ToInt32(PosNum), wStr(13:20))
    ELSEIF (PosNum < 10000000000000000_kInt64) THEN                     ! 9-16 digits
        HiNum = PosNum / 100000000_kInt64
        LoNum = PosNum - HiNum * 100000000_kInt64                       ! MOD(PosNum, 100000000)
        CALL Write_8_Digits(ToInt32(LoNum), wStr(13:20))
        Start = 4 + Write_1_to_8_Digits(ToInt32(HiNum), wStr(5:12))
    ELSE                                                                ! 17-20 digits
        TmpNum = PosNum / 100000000_kInt64
        LoNum  = PosNum - TmpNum * 100000000_kInt64                     ! MOD(PosNum, 100000000)
        HiNum  = TmpNum / 10000_kInt64
        MidNum = TmpNum - HiNum * 10000_kInt64                          ! MOD(TmpNum, 10000)
        CALL Write_8_Digits(ToInt32(LoNum), wStr(13:20))
        CALL Write_4_Digits(ToInt32(MidNum), wStr(9:12))
        Start  = Write_5_to_8_Digits(ToInt32(HiNum), wStr(1:8))
    END IF

    ! transfer to output string
    IF (Number < 0_kInt64) THEN
        IF (Number == MIN_I64) THEN
            cStr = '-9223372036854775808'
            RETURN
        END IF
        cStr = '-' // wStr(Start:MaxLen)
    ELSE
        cStr = wStr(Start:MaxLen)
    END IF

    RETURN

    CONTAINS

    SUBROUTINE Write_8_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: AA, BB, CC, DD   ! working indices
        tSInt32     :: AABB, CCDD       ! working variables

    !** FLOW

        AABB = ToInt32(SHIFTR(ToInt64(Number)*109951163_kInt64, 40))    ! Number / 10000
        CCDD = Number - AABB*10000                                      ! MOD(Number, 10000)
        AA = SHIFTR(AABB*5243, 19)                                      ! AABB / 100
        CC = SHIFTR(CCDD*5243, 19)                                      ! CCDD / 100
        BB = AABB - AA*100                                              ! MOD(AABB, 100)
        DD = CCDD - CC*100                                              ! MOD(CCDD, 100)
        cStr(1:2) = Char2Digits(AA)
        cStr(3:4) = Char2Digits(BB)
        cStr(5:6) = Char2Digits(CC)
        cStr(7:8) = Char2Digits(DD)

        RETURN

    END SUBROUTINE Write_8_Digits

    !**************************************************************************

    SUBROUTINE Write_4_Digits(Number, cStr)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: AA, BB   ! working indices

    !** FLOW

        AA = SHIFTR(Number*5243, 19)            ! Number / 100
        BB = Number - AA*100                    ! MOD(Number, 100)
        cStr(1:2) = Char2Digits(AA)
        cStr(3:4) = Char2Digits(BB)

        RETURN

    END SUBROUTINE Write_4_Digits

    !**************************************************************************

    FUNCTION Write_1_to_8_Digits(Number, cStr) RESULT(Start)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 1 to 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                     :: Start

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: AA, BB, CC, DD   ! working indices
        tSInt32     :: AABB, BBCC, CCDD ! working variables

    !** FLOW

        IF (Number < 100) THEN                                      ! 1-2 digits
            AA = Number
            IF (AA < 10) THEN
                cStr(8:8) = Char2Digits(AA)(2:2)
                Start = 8
            ELSE
                cStr(7:8) = Char2Digits(AA)
                Start = 7
            END IF
        ELSEIF (Number < 10000) THEN                                ! 3-4 digits
            AA = ToInt32(SHIFTR(ToInt64(Number)*5243_kInt64, 19))   ! Number / 100
            BB = Number - AA*100                                    ! MOD(Number, 100)
            IF (AA < 10) THEN
                cStr(6:6) = Char2Digits(AA)(2:2)
                cStr(7:8) = Char2Digits(BB)
                Start = 6
            ELSE
                cStr(5:6) = Char2Digits(AA)
                cStr(7:8) = Char2Digits(BB)
                Start = 5
            END IF
        ELSEIF (Number < 1000000) THEN                              ! 5-6 digits
            AA = ToInt32(SHIFTR(ToInt64(Number)*429497_kInt64, 32)) ! Number / 10000
            BBCC = Number - AA*10000                                ! MOD(Number, 10000)
            BB = SHIFTR(BBCC*5243, 19)                              ! BBCC / 100
            CC = BBCC - BB*100                                      ! MOD(BBCC, 100)
            IF (AA < 10) THEN
                cStr(4:4) = Char2Digits(AA)(2:2)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 4
            ELSE
                cStr(3:4) = Char2Digits(AA)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 3
            END IF
        ELSEIF (Number < 100000000) THEN                                    ! 7-8 digits
            AABB = ToInt32(SHIFTR(ToInt64(Number)*109951163_kInt64, 40))    ! Number / 10000
            CCDD = Number - AABB*10000                                      ! MOD(Number, 10000)
            AA = SHIFTR(AABB*5243, 19)                                      ! AABB / 100
            CC = SHIFTR(CCDD*5243, 19)                                      ! CCDD / 100
            BB = AABB - AA*100                                              ! MOD(AABB, 100)
            DD = CCDD - CC*100                                              ! MOD(CCDD, 100)
            IF (AA < 10) THEN
                cStr(2:2) = Char2Digits(AA)(2:2)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 2
            ELSE
                cStr(1:2) = Char2Digits(AA)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 1
            END IF
        END IF

        RETURN

    END FUNCTION Write_1_to_8_Digits

    !**************************************************************************

    FUNCTION Write_5_to_8_Digits(Number, cStr) RESULT(Start)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To write an (unsigned) integer number with a length of 5 to 8

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,   INTENT(IN)       :: Number   ! number
        tCharStar, INTENT(INOUT)    :: cStr     ! character string
        tSInt32                    :: Start

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: AA, BB, CC, DD   ! working indices
        tSInt32     :: AABB, BBCC, CCDD ! working variables

    !** FLOW

        IF (Number < 1000000) THEN                                  ! 5-6 digits
            AA = ToInt32(SHIFTR(ToInt64(Number)*429497_kInt64, 32)) ! Number / 10000
            BBCC = Number - AA*10000                                ! MOD(Number, 10000)
            BB = SHIFTR(BBCC*5243, 19)                              ! BBCC / 100
            CC = BBCC - BB*100                                      ! MOD(BBCC, 100)
            IF (AA < 10) THEN
                cStr(4:4) = Char2Digits(AA)(2:2)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 4
            ELSE
                cStr(3:4) = Char2Digits(AA)
                cStr(5:6) = Char2Digits(BB)
                cStr(7:8) = Char2Digits(CC)
                Start = 3
            END IF
        ELSEIF (Number < 100000000) THEN                                    ! 7-8 digits
            AABB = ToInt32(SHIFTR(ToInt64(Number)*109951163_kInt64, 40))    ! Number / 10000
            CCDD = Number - AABB*10000                                      ! MOD(Number, 10000)
            AA = SHIFTR(AABB*5243, 19)                                      ! AABB / 100
            CC = SHIFTR(CCDD*5243, 19)                                      ! CCDD / 100
            BB = AABB - AA*100                                              ! MOD(AABB, 100)
            DD = CCDD - CC*100                                              ! MOD(CCDD, 100)
            IF (AA < 10) THEN
                cStr(2:2) = Char2Digits(AA)(2:2)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 2
            ELSE
                cStr(1:2) = Char2Digits(AA)
                cStr(3:4) = Char2Digits(BB)
                cStr(5:6) = Char2Digits(CC)
                cStr(7:8) = Char2Digits(DD)
                Start = 1
            END IF
        END IF

        RETURN

    END FUNCTION Write_5_to_8_Digits

    !**************************************************************************

END FUNCTION I64_ToDecString

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   Conversion From Decimal String
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION I32_FromChar(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a decimal string to an integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSInt32                             :: Number   ! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxDigit = 10
    tSInt32, PARAMETER  :: A0       = IACHAR('0')
    tSInt32, PARAMETER  :: A4       = IACHAR('4')
    tSInt32, PARAMETER  :: IBase    = 10

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32                 :: Indx, StrLen
    tSInt32                 :: Sign
    tSInt32                 :: NumDigit
    tSInt32                 :: IStart
    tCharLen(1), POINTER    :: CurChr
    tLogical                :: Overflow

!** FLOW

    ! get valid string length by removing the trailing space(s)
    StrLen = LEN_TRIM(cStr)
    IF (PRESENT(ErrFlag)) ErrFlag = FalseVal

    ! check whether there are spaces in front of the number
    ! (only allow space(s) in front of the number but no spaces inside it)
    Indx = 1
    IF (cStr(Indx:Indx) == ' ') THEN
        Indx = Indx + 1
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= ' ') EXIT
            Indx = Indx + 1
        END DO
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this is an empty string.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MIN_I32
            RETURN
        END IF
    END IF

    ! check for sign
    Sign  = 1
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MIN_I32
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MIN_I32
            RETURN
        END IF
    END IF

    ! check for leading zero(s)
    Number = 0
    IF (cStr(Indx:Indx) == '0') THEN
        ! the first digit is zero so loop through the following
        ! characters until a non-zero character is found
        Indx = Indx + 1
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= '0') EXIT
            Indx = Indx + 1
        END DO
        IF (Indx > StrLen) THEN
            ! only zero digits encountered
            Number = 0
            RETURN
        END IF
    END IF

    ! compute value of the input string
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        DO
            ! compute the value without checking if it will overflow
            ! we will check it after we process all the characters if valid
            Number = Number*IBase + (IACHAR(CurChr)-A0)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MIN_I32
                RETURN
            END IF
        END DO
        NumDigit = Indx - IStart
    ELSE
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MIN_I32
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigit) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigit) THEN
        ! value might be in the applicable range
        IF (Number < 0) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1).AND.(Number == MIN_I32)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1
            END IF
        ELSE
            IF (IACHAR(cStr(IStart:IStart)) < A4) THEN
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
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MAX_I32
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MIN_I32
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I32_FromChar

!******************************************************************************

FUNCTION I64_FromChar(cStr, ErrFlag, ErrMsg) RESULT(Number)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a decimal string to an integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    tSInt64                             :: Number   ! number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32, PARAMETER  :: MaxDigit = 19
    tSInt32, PARAMETER  :: A0       = IACHAR('0')
    tSInt64, PARAMETER  :: LBase    = 10_kInt64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32                 :: Indx, StrLen
    tSInt64                 :: Sign
    tSInt32                 :: NumDigit
    tSInt32                 :: IStart
    tCharLen(1), POINTER    :: CurChr
    tLogical                :: Overflow

!** FLOW

    ! get valid string length by removing the trailing space(s)
    StrLen = LEN_TRIM(cStr)
    IF (PRESENT(ErrFlag)) ErrFlag = FalseVal

    ! check whether there are spaces in front of the number
    ! (only allow space(s) in front of the number but no spaces inside it)
    Indx = 1
    IF (cStr(Indx:Indx) == ' ') THEN
        Indx = Indx + 1
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= ' ') EXIT
            Indx = Indx + 1
        END DO
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this is an empty string.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MIN_I64
            RETURN
        END IF
    END IF

    ! check for sign
    Sign  = 1_kInt64
    CurChr => cStr(Indx:Indx)
    IF ((CurChr == '-').OR.(CurChr == '+')) THEN
        IF (CurChr == '-') Sign = -1_kInt64
        Indx = Indx + 1
        IF (Indx > StrLen) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: this string only contains a sign without a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MIN_I64
            RETURN
        END IF
        ! check whether the following character is a digit or not
        CurChr => cStr(Indx:Indx)
        IF ((CurChr < '0').OR.(CurChr > '9')) THEN
            ! current character is not a digit
            IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a sign must be followed by a digit.'
            IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
            Number = MIN_I64
            RETURN
        END IF
    END IF

    ! check for leading zero(s)
    Number = 0_kInt64
    IF (cStr(Indx:Indx) == '0') THEN
        ! the first digit is zero so loop through the following
        ! characters until a non-zero character is found
        Indx = Indx + 1
        DO WHILE (Indx <= StrLen)
            IF (cStr(Indx:Indx) /= '0') EXIT
            Indx = Indx + 1
        END DO
        IF (Indx > StrLen) THEN
            ! only zero digits encountered
            Number = 0_kInt64
            RETURN
        END IF
    END IF

    ! compute value of the input string
    IStart   = 0
    NumDigit = 0
    CurChr => cStr(Indx:Indx)
    IF ((CurChr > '0').AND.(CurChr <= '9')) THEN
        IStart = Indx
        DO
            ! compute the value without checking if it will overflow
            ! we will check it after we process all the characters if valid
            Number = Number*LBase + ToInt64(IACHAR(CurChr)-A0)
            Indx = Indx + 1
            IF (Indx > StrLen) EXIT
            CurChr => cStr(Indx:Indx)
            IF ((CurChr < '0').OR.(CurChr > '9')) THEN
                IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
                IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
                Number = MIN_I64
                RETURN
            END IF
        END DO
        NumDigit = Indx - IStart
    ELSE
        IF (PRESENT(ErrMsg))  ErrMsg  = 'Invalid Input: a non-digit character encountered.'
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        Number = MIN_I64
        RETURN
    END IF

    ! now, we have a valid string so check if the value is in the applicable range
    IF (NumDigit < MaxDigit) THEN
        ! value is in the applicable range
        Overflow = FalseVal
    ELSEIF (NumDigit == MaxDigit) THEN
        ! value might be in the applicable range
        IF (Number < 0_kInt64) THEN
            ! overflow occurs
            Overflow = TrueVal
            IF ((Sign /= 1_kInt64).AND.(Number == MIN_I64)) THEN
                ! actually not overflow
                Overflow = FalseVal
                Sign = 1_kInt64
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
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        IF (Sign == 1_kInt64) THEN
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is positively too large.'
            Number = MAX_I64
        ELSE
            IF (PRESENT(ErrMsg))  ErrMsg  = 'The input number is negatively too large.'
            Number = MIN_I64
        END IF
    ELSE
        Number = Number*Sign
    END IF

    RETURN

END FUNCTION I64_FromChar

!******************************************************************************

END MODULE MBase_SIntUtil

!******************************************************************************
