    
MODULE MBase_UIntUtil

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains routines that perform comparisons and arithmetic
!   operations for unsigned integers.  The module also provide various
!   utility routines including conversions from an unsigned integer to
!   a string as well as unsigned multiplications (to higher precision). <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://github.com/google/guava">Guava: Google Core Libraries
!       for Java</a> <br>
!   [2] <a href="https://github.com/openjdk/jdk">Java's OpenJDK</a> <br>
!   [3] <a href="https://flibs.sourceforge.net/">FLIBS - A collection of Fortran
!       modules</a> <br>
!   <br>
!^ **TECHNICAL NOTES**: <br>
!   According to [3], Fortran does not natively support unsigned integers but
!   intrinsic (signed) integer types in Fortran behave the same as unsigned
!   one for positive values.  For negative values, however, they behave differently
!   for the following operations: <br>
!    - *conversion*:   to and from string <br>
!    - *comparison*:   less than (<), less than or equal to(<=),
!                      greater than (>), greater than or equal to (>=) <br>
!    - *arithmetic*:   division (/) and modulation (MOD) <br>
!   (Note that in two's complement arithmetic, the three other basic
!    arithmetic operations of add, subtract, and multiply are bit-wise
!    identical if the two operands are regarded as both being signed
!    or both being unsigned.) <br>
!   Therefore, the module only provides routines for those operations that differ.
!   For those operations (the one that not yet mentioned including bitwise one)
!   that have the same behaviors, normal Fortran expressions can be used. <br>
!   It is important to note that routines in this module cannot differentiate between
!   signed and unsigned integers (i.e. they assume all integers (input and/or output)
!   to be unsigned with the exception of output of 'CompareUnsigned' routines); thus,
!   users of this module must be extremely careful not to mix up signed and unsigned
!   integers in the same expressions. <br>
!   The application interfaces (APIs) used in this module follows closely those used in
!   reference [3].  However, the implementations used here are mostly based on those
!   in references [1] and [2] because it appears that algorithms used in [1] and [2]
!   are more efficient than those used in [3].

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil
    USE MBase_LargeTables,    ONLY: RecTable

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! parameters
    PUBLIC :: MAX_U8, MAX_U16, MAX_U32, MAX_U64
    PUBLIC :: MIN_U8, MIN_U16, MIN_U32, MIN_U64
    ! comparison operators
    PUBLIC :: OPERATOR (.ULT.)
    PUBLIC :: OPERATOR (.ULE.)
    PUBLIC :: OPERATOR (.UGT.)
    PUBLIC :: OPERATOR (.UGE.)
    ! arithmetic operator and procedure
    PUBLIC :: OPERATOR (.UDIV.)
    PUBLIC :: UMOD, UDivMod
    ! comparison procedures
    PUBLIC :: CompareUnsigned
    ! conversion procedures
    PUBLIC :: ToUnsignedInteger
    PUBLIC :: ToUnsignedLong
    PUBLIC :: ToDecStrUnsigned
    PUBLIC :: ToHexStrUnsigned
    ! unsigned addition and subtraction
    PUBLIC :: AddU64
    PUBLIC :: SubU64
    ! unsigned multiplications
    PUBLIC :: UMul96_Upper64
    PUBLIC :: UMul96_Lower64
    PUBLIC :: UMul128           ! U128_Multiply
    PUBLIC :: UMul128_Upper64   ! U128_Multiply_High
    PUBLIC :: UMul128_N_Add     ! U128_Multiply_N_Add
    PUBLIC :: UMul128_N_Shift   ! U128_Multiply_N_Shift
    PUBLIC :: UMul192_Upper128
    PUBLIC :: UMul192_Lower128
    PUBLIC :: UMulBasic
    ! unsigned divisions
    PUBLIC :: Reciprocal_2By1
    PUBLIC :: Reciprocal_3By2
    PUBLIC :: UDivRem_2By1
    PUBLIC :: UDivRem_3By2

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! bit and byte sizes
    tSInt32, PARAMETER  :: Byte_BitSize     = BIT_SIZE(1_kInt8)     !  8
    tSInt32, PARAMETER  :: Byte_ByteSize    = kInt8                 !  1
    tSInt32, PARAMETER  :: Short_BitSize    = BIT_SIZE(1_kInt16)    ! 16
    tSInt32, PARAMETER  :: Short_ByteSize   = kInt16                !  2
    tSInt32, PARAMETER  :: Integer_BitSize  = BIT_SIZE(1_kInt32)    ! 32
    tSInt32, PARAMETER  :: Integer_ByteSize = kInt32                !  4
    tSInt32, PARAMETER  :: Long_BitSize     = BIT_SIZE(1_kInt64)    ! 64
    tSInt32, PARAMETER  :: Long_ByteSize    = kInt64                !  8
    ! maximum values
    tUInt8,  PARAMETER  :: MAX_U8  = ToInt8(Z'FF')                  !! 255
    tUInt16, PARAMETER  :: MAX_U16 = ToInt16(Z'FFFF')               !! 65535
    tUInt32, PARAMETER  :: MAX_U32 = ToInt32(Z'FFFFFFFF')           !! 4294967295
    tUInt64, PARAMETER  :: MAX_U64 = ToInt64(Z'FFFFFFFFFFFFFFFF')   !! 18446744073709551615
    ! minimum values
    tUInt8,  PARAMETER  :: MIN_U8  = 0_kInt8
    tUInt16, PARAMETER  :: MIN_U16 = 0_kInt16
    tUInt32, PARAMETER  :: MIN_U32 = 0_kInt32
    tUInt64, PARAMETER  :: MIN_U64 = 0_kInt64
    ! other parameters
    tUInt64, PARAMETER  :: MaskU32 = ToInt64(Z'00000000FFFFFFFF')
    tSInt64, PARAMETER  :: MinI64  = ToInt64(Z'8000000000000000')   ! min signed 64-bit

!** DERIVED TYPE DEFINITIONS
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

!** INTERFACE DEFINITIONS:
    INTERFACE OPERATOR (.ULT.)
        !^ **Operator Overload**: OPERATOR(.ULT.) <br>
        !  **Purpose**:  To check if the LHS value is less than the RHS value <br>
        !   return .TRUE. if LHS < RHS; otherwise return .FALSE. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS .ULT. RHS <br>
        !   --->    IF (LHS .ULT. RHS) DoSomething
        MODULE PROCEDURE    UInt8_LT
        MODULE PROCEDURE    UInt16_LT
        MODULE PROCEDURE    UInt32_LT
        MODULE PROCEDURE    UInt64_LT
    END INTERFACE
    INTERFACE OPERATOR (.ULE.)
        !^ **Operator Overload**: OPERATOR(.ULE.) <br>
        !  **Purpose**:  To check if the LHS value is less than or equal to the RHS value <br>
        !   return .TRUE. if LHS <= RHS; otherwise return .FALSE. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS .ULE. RHS <br>
        !   --->    IF (LHS .ULE. RHS) DoSomething
        MODULE PROCEDURE    UInt8_LE
        MODULE PROCEDURE    UInt16_LE
        MODULE PROCEDURE    UInt32_LE
        MODULE PROCEDURE    UInt64_LE
    END INTERFACE
    INTERFACE OPERATOR (.UGT.)
        !^ **Operator Overload**: OPERATOR(.UGT.) <br>
        !  **Purpose**:  To check if the LHS value is greater than the RHS value <br>
        !   return .TRUE. if LHS > RHS; otherwise return .FALSE. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS .UGT. RHS <br>
        !   --->    IF (LHS .UGT. RHS) DoSomething
        MODULE PROCEDURE    UInt8_GT
        MODULE PROCEDURE    UInt16_GT
        MODULE PROCEDURE    UInt32_GT
        MODULE PROCEDURE    UInt64_GT
    END INTERFACE
    INTERFACE OPERATOR (.UGE.)
        !^ **Operator Overload**: OPERATOR(.UGE.) <br>
        !  **Purpose**:  To check if the LHS value is greater than or equal to the RHS value <br>
        !   return .TRUE. if LHS >= RHS; otherwise return .FALSE. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS .UGE. RHS <br>
        !   --->    IF (LHS .UGE. RHS) DoSomething
        MODULE PROCEDURE    UInt8_GE
        MODULE PROCEDURE    UInt16_GE
        MODULE PROCEDURE    UInt32_GE
        MODULE PROCEDURE    UInt64_GE
    END INTERFACE
    INTERFACE OPERATOR (.UDIV.)
        !^ **Operator Overload**: OPERATOR(.UDIV.) <br>
        !  **Purpose**:  To return the quotient of a division of two unsigned integers,
        !   where both input and an output have the same kind <br>
        !  **Usage**: <br>
        !   --->    QUOT = NUMER .UDIV. DENOM
        MODULE PROCEDURE    UInt8_Divide
        MODULE PROCEDURE    UInt16_Divide
        MODULE PROCEDURE    UInt32_Divide
        MODULE PROCEDURE    UInt64_Divide
    END INTERFACE
    INTERFACE UMOD
        !^ **Function Interface**: UMOD <br>
        !  **Purpose**:  To return the remainder of a division of two unsigned integers,
        !   where both input and an output have the same kind <br>
        !  **Usage**: <br>
        !   --->    REM = UMOD(NUMER, DENOM)
        MODULE PROCEDURE    UInt8_Remainder
        MODULE PROCEDURE    UInt16_Remainder
        MODULE PROCEDURE    UInt32_Remainder
        MODULE PROCEDURE    UInt64_Remainder
    END INTERFACE
    INTERFACE UDivMod
        !^ **Subroutine Interface**: UDivMod <br>
        !  **Purpose**:  To perform a division of two unsigned integers and then return both
        !   the quotient and the remainder where both input and output have the same kind <br>
        !  **Usage**: <br>
        !   --->    CALL UDivMod(NUMER, DENOM, QUOT, REM)
        MODULE PROCEDURE    UInt32_DivMod
        MODULE PROCEDURE    UInt64_DivMod
    END INTERFACE
    INTERFACE CompareUnsigned
        !^ **Function Interface**: CompareUnsigned <br>
        !  **Purpose**:  To compare two unsigned integers (of the same kind) and return <br>
        !   -1 if LHS < RHS <br>
        !    0 if LHS == RHS <br>
        !    1 if LHS > RHS <br>
        !  **Usage**: <br>
        !   --->    Flag = CompareUnsigned(LHS, RHS) <br>
        !   --->    IF (CompareUnsigned(LHS, RHS) /= 0) DoSomething
        MODULE PROCEDURE    Compare_UInt8
        MODULE PROCEDURE    Compare_UInt16
        MODULE PROCEDURE    Compare_UInt32
        MODULE PROCEDURE    Compare_UInt64
    END INTERFACE
    INTERFACE ToUnsignedInteger
        !^ **Function Interface**: ToUnsignedInteger <br>
        !  **Purpose**:  To perform unsigned conversion from lower-precision unsigned integer
        !   to 32-bit unsigned integer <br>
        !  **Usage**: <br>
        !   --->    U32 = ToUnsignedInteger(U8)
        MODULE PROCEDURE    ByteToUnsignedInteger
        MODULE PROCEDURE    ShortToUnsignedInteger
    END INTERFACE
    INTERFACE ToUnsignedLong
        !^ **Function Interface**: ToUnsignedLong <br>
        !  **Purpose**:  To perform unsigned conversion from lower-precision unsigned integer
        !   to 64-bit unsigned integer <br>
        !  **Usage**: <br>
        !   --->    U64 = ToUnsignedLong(U32)
        MODULE PROCEDURE    ByteToUnsignedLong
        MODULE PROCEDURE    ShortToUnsignedLong
        MODULE PROCEDURE    IntegerToUnsignedLong
    END INTERFACE
    INTERFACE ToDecStrUnsigned
        !^ **Function Interface**: ToDecStrUnsigned <br>
        !  **Purpose**:  To convert an unsigned integer to a decimal string <br>
        !  **Usage**: <br>
        !   --->    Str = ToDecStrUnsigned(U32)
        MODULE PROCEDURE    U32_ToDecString
        MODULE PROCEDURE    U64_ToDecString
    END INTERFACE
    INTERFACE ToHexStrUnsigned
        !^ **Function Interface**: ToHexStrUnsigned <br>
        !  **Purpose**:  To convert an unsigned integer to a hexadecimal string <br>
        !  **Usage**: <br>
        !   --->    Str = ToHexStrUnsigned(U32)
        MODULE PROCEDURE    U32_ToHexString
        MODULE PROCEDURE    U64_ToHexString
    END INTERFACE

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   ROUTINES FOR BYTE INTEGER NUMBER
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION UInt8_LT(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS < RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) < ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt8_LT

!******************************************************************************

PURE FUNCTION UInt8_LE(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS <= RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) <= ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt8_LE

!******************************************************************************

PURE FUNCTION UInt8_GT(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS > RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) > ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt8_GT

!******************************************************************************

PURE FUNCTION UInt8_GE(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS >= RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) >= ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt8_GE

!******************************************************************************

PURE FUNCTION UInt8_Divide(Dividend, Divisor) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned quotient of dividing the first argument by
    ! the second where each argument and the result is interpreted as
    ! an unsigned value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Dividend, Divisor
    tUInt8              :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ResVal = ToInt8(ToUnsignedInteger(Dividend)/ToUnsignedInteger(Divisor))

    RETURN

END FUNCTION UInt8_Divide

!******************************************************************************

PURE FUNCTION UInt8_Remainder(Dividend, Divisor) RESULT(Rem)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned remainder from dividing the first argument
    ! by the second where each argument and the result is interpreted
    ! as an unsigned value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Dividend, Divisor
    tUInt8              :: Rem

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Rem = ToInt8(MOD(ToUnsignedInteger(Dividend), ToUnsignedInteger(Divisor)))

    RETURN

END FUNCTION UInt8_Remainder

!******************************************************************************

PURE FUNCTION Compare_UInt8(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS where both numbers are treated as unsigned. <br>
    ! - return -1 if LHS < RHS <br>
    ! - return  0 if LHS == RHS <br>
    ! - return +1 if LHS > RHS

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: LHS, RHS
    tSInt32             :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = Compare(ToUnsignedInteger(LHS), ToUnsignedInteger(RHS))

    RETURN

END FUNCTION Compare_UInt8

!******************************************************************************

PURE FUNCTION ByteToUnsignedInteger(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the argument to an integer by an unsigned
    ! conversion.  In an unsigned conversion to an integer, the
    ! high-order 24 bits of the integer are zero and the low-order
    ! 8 bits are equal to the bits of the byte argument.
    ! Consequently, zero and positive byte values are mapped
    ! to a numerically equal integer value and negative byte values
    ! are mapped to a integer value equal to the input plus 2**8. <br>
    ! InVal:   the value to convert to an unsigned integer <br>
    ! OutVal:  the result converted to integer by an unsigned conversion

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt8, INTENT(IN)  :: InVal
    tUInt32             :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(ToInt32(InVal), Z'000000FF')

    RETURN

END FUNCTION ByteToUnsignedInteger

!******************************************************************************

PURE FUNCTION ByteToUnsignedLong(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the argument to a long integer by an unsigned
    ! conversion.  In an unsigned conversion to a long, the
    ! high-order 56 bits of the long are zero and the low-order
    ! 8 bits are equal to the bits of the byte argument.
    ! Consequently, zero and positive byte values are mapped
    ! to a numerically equal long value and negative byte values
    ! are mapped to a long value equal to the input plus 2**8. <br>
    ! InVal:   the value to convert to an unsigned long <br>
    ! OutVal:  the result converted to long by an unsigned conversion

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt8, INTENT(IN)  :: InVal
    tUInt64             :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(ToInt64(InVal), Z'00000000000000FF')

    RETURN

END FUNCTION ByteToUnsignedLong

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   ROUTINES FOR SHORT INTEGER NUMBER
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION UInt16_LT(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS < RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt16, INTENT(IN) :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) < ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt16_LT

!******************************************************************************

PURE FUNCTION UInt16_LE(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS <= RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt16, INTENT(IN) :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) <= ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt16_LE

!******************************************************************************

PURE FUNCTION UInt16_GT(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS > RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt16, INTENT(IN) :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) > ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt16_GT

!******************************************************************************

PURE FUNCTION UInt16_GE(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS >= RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt16, INTENT(IN) :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = ToUnsignedInteger(LHS) >= ToUnsignedInteger(RHS)

    RETURN

END FUNCTION UInt16_GE

!******************************************************************************

PURE FUNCTION UInt16_Divide(Dividend, Divisor) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned quotient of dividing the first argument by
    ! the second where each argument and the result is interpreted as
    ! an unsigned value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt16, INTENT(IN) :: Dividend, Divisor
    tUInt16             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ResVal = ToInt16(ToUnsignedInteger(Dividend)/ToUnsignedInteger(Divisor))

    RETURN

END FUNCTION UInt16_Divide

!******************************************************************************

PURE FUNCTION UInt16_Remainder(Dividend, Divisor) RESULT(Rem)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned remainder from dividing the first argument
    ! by the second where each argument and the result is interpreted
    ! as an unsigned value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt16, INTENT(IN) :: Dividend, Divisor
    tUInt16             :: Rem

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Rem = ToInt16(MOD(ToUnsignedInteger(Dividend), ToUnsignedInteger(Divisor)))

    RETURN

END FUNCTION UInt16_Remainder

!******************************************************************************

PURE FUNCTION Compare_UInt16(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS where both numbers are treated as unsigned. <br>
    ! - return -1 if LHS < RHS <br>
    ! - return  0 if LHS == RHS <br>
    ! - return +1 if LHS > RHS

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt16, INTENT(IN) :: LHS, RHS
    tSInt32             :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = Compare(ToUnsignedInteger(LHS), ToUnsignedInteger(RHS))

    RETURN

END FUNCTION Compare_UInt16

!******************************************************************************

PURE FUNCTION ShortToUnsignedInteger(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the argument to an integer by an unsigned
    ! conversion.  In an unsigned conversion to an integer, the
    ! high-order 16 bits of the integer are zero and the low-order
    ! 16 bits are equal to the bits of the short argument.
    ! Consequently, zero and positive short values are mapped
    ! to a numerically equal integer value and negative short values
    ! are mapped to a integer value equal to the input plus 2**16. <br>
    ! InVal:   the value to convert to an unsigned integer <br>
    ! OutVal:  the result converted to integer by an unsigned conversion

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt16, INTENT(IN) :: InVal
    tUInt32             :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(ToInt32(InVal), Z'0000FFFF')

    RETURN

END FUNCTION ShortToUnsignedInteger

!******************************************************************************

PURE FUNCTION ShortToUnsignedLong(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the argument to a long integer by an unsigned
    ! conversion.  In an unsigned conversion to a long, the
    ! high-order 48 bits of the long are zero and the low-order
    ! 16 bits are equal to the bits of the short argument.
    ! Consequently, zero and positive short values are mapped
    ! to a numerically equal long value and negative short values
    ! are mapped to a long value equal to the input plus 2**16. <br>
    ! InVal:   the value to convert to an unsigned long <br>
    ! OutVal:  the result converted to long by an unsigned conversion

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt16, INTENT(IN) :: InVal
    tUInt64             :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(ToInt64(InVal), Z'000000000000FFFF')

    RETURN

END FUNCTION ShortToUnsignedLong

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   ROUTINES FOR DEFAULT INTEGER NUMBER
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION UInt32_LT(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS < RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, INTENT(IN) :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I32) < IEOR(RHS, MIN_I32)

    RETURN

END FUNCTION UInt32_LT

!******************************************************************************

PURE FUNCTION UInt32_LE(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS <= RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, INTENT(IN) :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I32) <= IEOR(RHS, MIN_I32)

    RETURN

END FUNCTION UInt32_LE

!******************************************************************************

PURE FUNCTION UInt32_GT(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS > RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, INTENT(IN) :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I32) > IEOR(RHS, MIN_I32)

    RETURN

END FUNCTION UInt32_GT

!******************************************************************************

PURE FUNCTION UInt32_GE(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS >= RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, INTENT(IN) :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I32) >= IEOR(RHS, MIN_I32)

    RETURN

END FUNCTION UInt32_GE

!******************************************************************************

PURE FUNCTION UInt32_Divide(Dividend, Divisor) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned quotient of dividing the first argument by
    ! the second where each argument and the result is interpreted as
    ! an unsigned value. <br>
    !  <br>
    ! Note that in two's complement arithmetic, the three other
    ! basic arithmetic operations of add, subtract, and multiply are
    ! bit-wise identical if the two operands are regarded as both
    ! being signed or both being unsigned.  Therefore separate routines
    ! for the three other operations are not provided. <br>
    !  <br>
    ! Dividend - the value to be divided <br>
    ! Divisor  - the value doing the dividing <br>
    ! ResVal   - the unsigned quotient of the first argument divided by the second argument

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, INTENT(IN) :: Dividend, Divisor
    tUInt32             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! In lieu of tricky code, for now just use long arithmetic.
    ResVal = ToInt32(ToUnsignedLong(Dividend)/ToUnsignedLong(Divisor))

    RETURN

END FUNCTION UInt32_Divide

!******************************************************************************

PURE FUNCTION UInt32_Remainder(Dividend, Divisor) RESULT(Rem)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned remainder from dividing the first argument
    ! by the second where each argument and the result is interpreted
    ! as an unsigned value. <br>
    !  <br>
    ! Dividend - the value to be divided <br>
    ! Divisor  - the value doing the dividing <br>
    ! Rem      - the unsigned remainder of the first argument divided by the second argument

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, INTENT(IN) :: Dividend, Divisor
    tUInt32             :: Rem

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! In lieu of tricky code, for now just use long arithmetic.
    Rem = ToInt32(MOD(ToUnsignedLong(Dividend), ToUnsignedLong(Divisor)))

    RETURN

END FUNCTION UInt32_Remainder

!******************************************************************************

PURE SUBROUTINE UInt32_DivMod(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned quotient of dividing the first argument by
    ! the second where each argument and the result is interpreted as
    ! an unsigned value. <br>
    !  <br>
    ! Note that in two's complement arithmetic, the three other
    ! basic arithmetic operations of add, subtract, and multiply are
    ! bit-wise identical if the two operands are regarded as both
    ! being signed or both being unsigned.  Therefore separate routines
    ! for the three other operations are not provided. <br>
    !  <br>
    ! Dividend  - the value to be divided <br>
    ! Divisor   - the value doing the dividing <br>
    ! Quotient  - the unsigned quotient of the first argument divided by the second argument <br>
    ! Remainder - the unsigned remainder of the first argument divided by the second argument

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, INTENT(IN)     :: Dividend, Divisor
    tUInt32, INTENT(OUT)    :: Quotient, Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Q, R

!** FLOW

    ! In lieu of tricky code, for now just use long arithmetic.
    CALL UDivMod(ToUnsignedLong(Dividend), ToUnsignedLong(Divisor), Q, R)
    Quotient  = ToInt32(Q)
    Remainder = ToInt32(R)

    RETURN

END SUBROUTINE UInt32_DivMod

!******************************************************************************

PURE FUNCTION Compare_UInt32(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS where both numbers are treated as unsigned. <br>
    ! - return -1 if LHS < RHS <br>
    ! - return  0 if LHS == RHS <br>
    ! - return +1 if LHS > RHS

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, INTENT(IN) :: LHS, RHS
    tSInt32             :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = Compare(IEOR(LHS, MIN_I32), IEOR(RHS, MIN_I32))

    RETURN

END FUNCTION Compare_UInt32

!******************************************************************************

PURE FUNCTION IntegerToUnsignedLong(InVal) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert the argument to a long integer by an unsigned
    ! conversion.  In an unsigned conversion to a long, the
    ! high-order 32 bits of the long are zero and the low-order
    ! 32 bits are equal to the bits of the integer argument.
    ! Consequently, zero and positive integer values are mapped
    ! to a numerically equal long value and negative integer values
    ! are mapped to a long value equal to the input plus 2**32. <br>
    ! InVal:   the value to convert to an unsigned long <br>
    ! OutVal:  the result converted to long by an unsigned conversion

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32, INTENT(IN) :: InVal
    tUInt64             :: OutVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    OutVal = IAND(ToInt64(InVal), Z'00000000FFFFFFFF')

    RETURN

END FUNCTION IntegerToUnsignedLong

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                   ROUTINES FOR LONG INTEGER NUMBER
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION UInt64_LT(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS < RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I64) < IEOR(RHS, MIN_I64)

    RETURN

END FUNCTION UInt64_LT

!******************************************************************************

PURE FUNCTION UInt64_LE(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS <= RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I64) <= IEOR(RHS, MIN_I64)

    RETURN

END FUNCTION UInt64_LE

!******************************************************************************

PURE FUNCTION UInt64_GT(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS > RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I64) > IEOR(RHS, MIN_I64)

    RETURN

END FUNCTION UInt64_GT

!******************************************************************************

PURE FUNCTION UInt64_GE(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compare whether LHS >= RHS where both numbers are treated as unsigned.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: LHS, RHS
    tLogical            :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = IEOR(LHS, MIN_I64) >= IEOR(RHS, MIN_I64)

    RETURN

END FUNCTION UInt64_GE

!******************************************************************************

PURE FUNCTION UInt64_Divide(Dividend, Divisor) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned quotient of dividing the first argument by
    ! the second where each argument and the result is interpreted as
    ! an unsigned value. <br>
    !  <br>
    ! Note that in two's complement arithmetic, the three other
    ! basic arithmetic operations of add, subtract, and multiply are
    ! bit-wise identical if the two operands are regarded as both
    ! being signed or both being unsigned.  Therefore separate routines
    ! for the three other operations are not provided. <br>
    !  <br>
    ! Dividend - the value to be divided <br>
    ! Divisor  - the value doing the dividing <br>
    ! ResVal   - the unsigned quotient of the first argument divided by the second argument

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: Dividend, Divisor
    tUInt64             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Quotient, Remainder

!** FLOW

    ! See Hacker's Delight (2nd ed), section 9.3
    IF (Divisor >= 0_kInt64) THEN
        Quotient  = SHIFTL(SHIFTR(Dividend, 1)/Divisor, 1)
        Remainder = Dividend - Quotient*Divisor
        ResVal = Quotient + SHIFTR(IOR(Remainder, NOT(Remainder-Divisor)), (Long_BitSize-1))
    ELSE
        ResVal = SHIFTR(IAND(Dividend, NOT(Dividend-Divisor)), (Long_BitSize-1))
    END IF

    RETURN

END FUNCTION UInt64_Divide

!******************************************************************************

PURE FUNCTION UInt64_Remainder(Dividend, Divisor) RESULT(Rem)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned remainder from dividing the first argument
    ! by the second where each argument and the result is interpreted
    ! as an unsigned value. <br>
    !  <br>
    ! Dividend - the value to be divided <br>
    ! Divisor  - the value doing the dividing <br>
    ! Rem      - the unsigned remainder of the first argument divided by the second argument

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: Dividend, Divisor
    tUInt64             :: Rem

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Q, R

!** FLOW

    ! See Hacker's Delight (2nd ed), section 9.3
    IF (Divisor >= 0_kInt64) THEN
        Q = SHIFTL(SHIFTR(Dividend, 1)/Divisor, 1)
        R = Dividend - Q*Divisor
        ! Here, 0 <= r < 2 * divisor
        ! (1) When 0 <= r < divisor, the remainder is simply r.
        ! (2) Otherwise the remainder is r - divisor.
        !
        ! In case (1), r - divisor < 0. Applying ~ produces a long with
        ! sign bit 0, so >> produces 0. The returned value is thus r.
        !
        ! In case (2), a similar reasoning shows that >> produces -1,
        ! so the returned value is r - divisor.
        Rem = R - IAND(SHIFTA(NOT(R-Divisor), (Long_BitSize-1)), Divisor)
    ELSE
        ! (1) When dividend >= 0, the remainder is dividend.
        ! (2) Otherwise
        !      (2.1) When dividend < divisor, the remainder is dividend.
        !      (2.2) Otherwise the remainder is dividend - divisor
        !
        ! A reasoning similar to the above shows that the returned value
        ! is as expected.
        Rem = Dividend - IAND(SHIFTA(IAND(Dividend, NOT(Dividend-Divisor)), &
                                     (Long_BitSize-1)), Divisor)
    END IF

    RETURN

END FUNCTION UInt64_Remainder

!******************************************************************************

PURE SUBROUTINE UInt64_DivMod(Dividend, Divisor, Quotient, Remainder)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned quotient of dividing the first argument by
    ! the second where each argument and the result is interpreted as
    ! an unsigned value. <br>
    !  <br>
    ! Note that in two's complement arithmetic, the three other
    ! basic arithmetic operations of add, subtract, and multiply are
    ! bit-wise identical if the two operands are regarded as both
    ! being signed or both being unsigned.  Therefore separate routines
    ! for the three other operations are not provided. <br>
    !  <br>
    ! Dividend  - the value to be divided <br>
    ! Divisor   - the value doing the dividing <br>
    ! Quotient  - the unsigned quotient of the first argument divided by the second argument <br>
    ! Remainder - the unsigned remainder of the first argument divided by the second argument

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: Dividend, Divisor
    tUInt64, INTENT(OUT)    :: Quotient, Remainder

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Q, R

!** FLOW

    ! See Hacker's Delight (2nd ed), section 9.3
    IF (Divisor >= 0_kInt64) THEN
        Q = SHIFTL(SHIFTR(Dividend, 1)/Divisor, 1)
        R = Dividend - Q*Divisor
        Quotient  = Q + SHIFTR(IOR(R, NOT(R-Divisor)), (Long_BitSize-1))
        ! Here, 0 <= r < 2 * divisor
        ! (1) When 0 <= r < divisor, the remainder is simply r.
        ! (2) Otherwise the remainder is r - divisor.
        !
        ! In case (1), r - divisor < 0. Applying ~ produces a long with
        ! sign bit 0, so >> produces 0. The returned value is thus r.
        !
        ! In case (2), a similar reasoning shows that >> produces -1,
        ! so the returned value is r - divisor.
        Remainder = R - IAND(SHIFTA(NOT(R-Divisor), (Long_BitSize-1)), Divisor)
    ELSE
        Quotient = SHIFTR(IAND(Dividend, NOT(Dividend-Divisor)), (Long_BitSize-1))
        ! (1) When dividend >= 0, the remainder is dividend.
        ! (2) Otherwise
        !      (2.1) When dividend < divisor, the remainder is dividend.
        !      (2.2) Otherwise the remainder is dividend - divisor
        !
        ! A reasoning similar to the above shows that the returned value
        ! is as expected.
        Remainder = Dividend - IAND(SHIFTA(IAND(Dividend, NOT(Dividend-Divisor)), &
                                           (Long_BitSize-1)), Divisor)
    END IF

    RETURN

END SUBROUTINE UInt64_DivMod

!******************************************************************************

PURE FUNCTION Compare_UInt64(LHS, RHS) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compare LHS and RHS where both numbers are treated as unsigned. <br>
    ! - return -1 if LHS < RHS <br>
    ! - return  0 if LHS == RHS <br>
    ! - return +1 if LHS > RHS

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: LHS, RHS
    tSInt32             :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = Compare(IEOR(LHS, MIN_I64), IEOR(RHS, MIN_I64))

    RETURN

END FUNCTION Compare_UInt64

!******************************************************************************

FUNCTION U32_ToDecString(Val) RESULT(RetStr)

    ! PURPOSE OF THIS FUNCTION:
    !! To convert a 32-bit integer treated as an unsigned number into a string.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tUInt32, INTENT(IN) :: Val
    tCharAlloc          :: RetStr

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

    ! FLOW:

    IF (Val >= 0) THEN
        RetStr = ToDecStrSigned(Val)
    ELSE
        BLOCK
            tUInt64     :: LongVal
            tUInt64     :: Quotient, Remainder
            tCharAlloc  :: QuotStr, RemStr
            ! execution
            LongVal   = IAND(ToInt64(Val), Z'00000000FFFFFFFF')
            Quotient  = SHIFTR(LongVal, 1) / 5
            Remainder = LongVal - Quotient * 10
            QuotStr = ToDecStrSigned(ToInt32(Quotient))
            RemStr  = ToDecStrSigned(ToInt32(Remainder))
            RetStr  = QuotStr // RemStr
        END BLOCK
    END IF

    RETURN

END FUNCTION

!******************************************************************************

FUNCTION U64_ToDecString(Val) RESULT(RetStr)

    ! PURPOSE OF THIS FUNCTION:
    !! To convert a 64-bit integer treated as an unsigned number into a string.

    ! SUBROUTINE ARGUMENT DEFINITIONS:
    tUInt64, INTENT(IN) :: Val
    tCharAlloc          :: RetStr

    ! SUBROUTINE LOCAL VARIABLE DECLARATIONS:
    ! na

    ! FLOW:

    IF (Val >= 0) THEN
        RetStr = ToDecStrSigned(Val)
    ELSE
        BLOCK
            tUInt64     :: Quotient, Remainder
            tCharAlloc  :: QuotStr, RemStr
            ! execution
            Quotient  = SHIFTR(Val, 1) / 5
            Remainder = Val - Quotient * 10
            QuotStr = ToDecStrSigned(Quotient)
            RemStr  = ToDecStrSigned(Remainder)
            RetStr  = QuotStr // RemStr
        END BLOCK
    END IF

    RETURN

END FUNCTION

!******************************************************************************

FUNCTION U64_ToHexString(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an unsigned 64-bit integer number to a hexadecimal string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: Number   ! number treated as unsigned one
    tCharAlloc          :: cStr     ! hexadecimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt32, PARAMETER :: Shift = 4

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64         :: Quotient, Remainder
    tCharAlloc      :: QuotStr,  RemStr

!** FLOW

    IF (Number >= 0_kInt64) THEN
        cStr = ToHexStrSigned(Number)
    ELSE
        Quotient  = SHIFTR(Number, Shift)
        Remainder = Number - SHIFTL(Quotient, Shift)
        QuotStr = ToHexStrSigned(Quotient)
        RemStr  = ToHexStrSigned(Remainder)
        cStr    = QuotStr // RemStr
    END IF

    RETURN

END FUNCTION U64_ToHexString

!******************************************************************************

FUNCTION U32_ToHexString(Number) RESULT(cStr)

!** PURPOSE OF THIS SUBROUTINE:
    !! To convert an unsigned 32-bit integer number to a hexadecimal string

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, INTENT(IN) :: Number   ! number
    tCharAlloc          :: cStr     ! hexadecimal string

!** SUBROUTINE PARAMETER DECLARATIONS:
    tUInt32, PARAMETER :: Shift = 4

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32         :: Quotient, Remainder
    tCharAlloc      :: QuotStr,  RemStr

!** FLOW

    IF (Number >= 0) THEN
        cStr = ToHexStrSigned(Number)
    ELSE
        Quotient  = SHIFTR(Number, Shift)
        Remainder = Number - SHIFTL(Quotient, Shift)
        QuotStr = ToHexStrSigned(Quotient)
        RemStr  = ToHexStrSigned(Remainder)
        cStr    = QuotStr // RemStr
    END IF

    RETURN

END FUNCTION U32_ToHexString

!******************************************************************************

PURE SUBROUTINE UMul128(X, Y, U128Hi, U128Lo)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute 128-bit result of multiplication of two 64-bit unsigned integers.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: X, Y
    tUInt64, INTENT(OUT)    :: U128Hi, U128Lo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: X_Lo, X_Hi, Y_Lo, Y_Hi
    tUInt64     :: Lo_Lo, Hi_Lo, Cross

!** FLOW

    X_Lo = IAND(X, MaskU32)
    X_Hi = SHIFTR(X, 32)
    Y_Lo = IAND(Y, MaskU32)
    Y_Hi = SHIFTR(Y, 32)
    Lo_Lo = X_Lo*Y_Lo
    Hi_Lo = X_Hi*Y_Lo
    Cross = SHIFTR(Lo_Lo, 32) + IAND(Hi_Lo, MaskU32) + X_Lo*Y_Hi
    U128Hi = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi
    U128Lo = IOR(SHIFTL(Cross, 32), IAND(Lo_Lo, MaskU32))

    RETURN

END SUBROUTINE UMul128

!******************************************************************************

PURE FUNCTION UMul128_Upper64(X, Y) RESULT(U128Hi)

!** PURPOSE OF THIS SUBROUTINE:
    !! To compute upper 64 bits of multiplication of two 64-bit unsigned integers

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: X, Y
    tUInt64             :: U128Hi

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: X_Lo, X_Hi, Y_Lo, Y_Hi
    tUInt64     :: Hi_Lo, Cross

!** FLOW

    X_Lo = IAND(X, MaskU32)
    X_Hi = SHIFTR(X, 32)
    Y_Lo = IAND(Y, MaskU32)
    Y_Hi = SHIFTR(Y, 32)
    Hi_Lo  = X_Hi*Y_Lo
    Cross  = SHIFTR(X_Lo*Y_Lo, 32) + IAND(Hi_Lo, MaskU32) + X_Lo*Y_Hi
    U128Hi = SHIFTR(Hi_Lo, 32) + SHIFTR(Cross, 32) + X_Hi*Y_Hi

    RETURN

END FUNCTION UMul128_Upper64

!******************************************************************************

PURE SUBROUTINE UMul192_Upper128(X, YHi, YLo, U128Hi, U128Lo)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute upper 128 bits of multiplication of a 64-bit unsigned integer and
    ! a 128-bit unsigned integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: X, YHi, YLo
    tUInt64, INTENT(OUT)    :: U128Hi, U128Lo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Add

!** FLOW

    CALL UMul128(X, YHi, U128Hi, U128Lo)
    Add = UMul128_Upper64(X, YLo)
    U128Lo = U128Lo + Add
    IF (U128Lo .ULT. Add) U128Hi = U128Hi + 1_kInt64

    RETURN

END SUBROUTINE UMul192_Upper128

!******************************************************************************

PURE FUNCTION UMul96_Upper64(X, Y) RESULT(U128Hi)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute upper 64 bits of multiplication of a 32-bit unsigned integer and
    ! a 64-bit unsigned integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, INTENT(IN) :: X
    tUInt64, INTENT(IN) :: Y
    tUInt64             :: U128Hi

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U128Hi = UMul128_Upper64(SHIFTL(ToUnsignedLong(X), 32), Y)

    RETURN

END FUNCTION UMul96_Upper64

!******************************************************************************

PURE SUBROUTINE UMul192_Lower128(X, YHi, YLo, U128Hi, U128Lo)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute lower 128 bits of multiplication of a 64-bit unsigned integer and
    ! a 128-bit unsigned integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: X, YHi, YLo
    tUInt64, INTENT(OUT)    :: U128Hi, U128Lo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL UMul128(X, YLo, U128Hi, U128Lo)
    U128Hi = U128Hi + X*YHi

    RETURN

END SUBROUTINE UMul192_Lower128

!******************************************************************************

PURE FUNCTION UMul96_Lower64(X, Y) RESULT(U128Lo)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute lower 64 bits of multiplication of a 32-bit unsigned integer and
    ! a 64-bit unsigned integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt32, INTENT(IN) :: X
    tUInt64, INTENT(IN) :: Y
    tUInt64             :: U128Lo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    U128Lo = X*Y

    RETURN

END FUNCTION UMul96_Lower64

!******************************************************************************

PURE SUBROUTINE UMul128_N_Add(A, B, C, U128Hi, U128Lo)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To multiply two 64-bit unsigned integers and add a value (A*B + C), and
    ! return the 128-bit result as U128Hi, U128Lo.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: A, B, C
    tUInt64, INTENT(OUT)    :: U128Hi, U128Lo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: H, L, T

!** FLOW

    ! multiply A and B
    CALL UMul128(A, B, H, L)

    ! add carry
    T = L + C

    ! check whether to add 1 to high bit
    IF (T .ULT. L) THEN
        H = H + 1_kInt64
    ELSE
        IF (T .ULT. C) H = H + 1_kInt64
    END IF

    U128Hi = H
    U128Lo = T

    RETURN

END SUBROUTINE UMul128_N_Add

!******************************************************************************

PURE FUNCTION UMul128_N_Shift(A, B_Hi, B_Lo, ShrPos) RESULT(ResVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To multiply two 64-bit unsigned integers, and then shift
    ! the 128-bit result by ShrPos => SHIFTR(A*B, ShrPos). <br>
    ! Note: ShrPos should be in the range [64, 128].

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: A, B_Hi, B_Lo
    tSInt32, INTENT(IN) :: ShrPos
    tUInt64             :: ResVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: R0_Hi, R1_Lo, R1_Hi

!** FLOW

    ! multiply A and B_Lo and return the high bit R0_Hi
    R0_Hi = UMul128_Upper64(A, B_Lo)

    ! multiply A and B_Hi and add R0_Hi to the result
    CALL UMul128_N_Add(A, B_Hi, R0_Hi, R1_Hi, R1_Lo)

    ! shift the result by ShrPos position => SHIFTR(A*B, ShrPos)
    ResVal = IOR(SHIFTL(R1_Hi, (128 - ShrPos)), SHIFTR(R1_Lo, (ShrPos - 64)))

    RETURN

END FUNCTION UMul128_N_Shift

!******************************************************************************

SUBROUTINE UMulBasic(X, XLen, Y, YLen, Z)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To multiply 64-bit unsigned integer arrays and return
    !  the result using grade-school algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,  INTENT(IN)     :: XLen             !! The length of the first array
    tUInt64, INTENT(IN)     :: X(0:XLen-1)      !! The first magnitude array
    tIndex,  INTENT(IN)     :: YLen             !! The length of the second array
    tUInt64, INTENT(IN)     :: Y(0:YLen-1)      !! The second magnitude array
    tUInt64, INTENT(OUT)    :: Z(0:XLen+YLen-1) !! The result array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Carry64, ProductHi, ProductLo, Sum
    tIndex      :: I, J

!** FLOW

    Carry64 = 0_kInt64
    DO J = 0, YLen-1
        CALL UMul128(X(0), Y(J), ProductHi, ProductLo)
        Z(J) = ProductLo + Carry64
        IF (IEOR(Z(J), MinI64) < IEOR(ProductLo, MinI64)) THEN
            Carry64 = ProductHi + 1_kInt64
        ELSE
            Carry64 = ProductHi
        END IF
    END DO
    Z(YLen) = Carry64
    DO I = 1, XLen-1
        Carry64 = 0_kInt64
        DO J = 0, YLen-1
            CALL UMul128(X(I), Y(J), ProductHi, ProductLo)
            Sum = ProductLo + Z(I+J)
            IF (IEOR(Sum, MinI64) < IEOR(ProductLo, MinI64)) ProductHi = ProductHi + 1_kInt64
            Z(I+J) = Sum + Carry64
            IF (IEOR(Z(I+J), MinI64) < IEOR(Sum, MinI64)) THEN
                Carry64 = ProductHi + 1_kInt64
            ELSE
                Carry64 = ProductHi
            END IF
        END DO
        Z(I+YLen) = Carry64
    END DO

    RETURN

END SUBROUTINE UMulBasic

!******************************************************************************

SUBROUTINE AddU64(X, Y, CarryIn, Sum, CarryOut)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the sum with carry of X, Y and CarryIn: Sum = X + Y + CarryIn.
    !  The carry input must be 0 or 1; otherwise the behavior is undefined.
    !  The carry output is guaranteed to be 0 or 1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: X, Y, CarryIn
    tUInt64, INTENT(OUT)    :: Sum, CarryOut
    OPTIONAL                :: CarryOut

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Sum = X + Y + CarryIn
    ! The sum will overflow if both top bits are set (x & y) or if one of them
    ! is (x | y), and a carry from the lower place happened. If such a carry
    ! happens, the top bit will be 1 + 0 + 1 = 0 (&^ sum).
    IF (PRESENT(CarryOut)) THEN
        CarryOut = SHIFTR(IOR(IAND(X, Y), IAND(IOR(X, Y), NOT(Sum))), 63)
    END IF

    RETURN

END SUBROUTINE AddU64

!******************************************************************************

SUBROUTINE SubU64(X, Y, BorrowIn, Diff, BorrowOut)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the difference of X, Y and BorrowIn: Diff = X - Y - BorrowIn.
    !  The borrow input must be 0 or 1; otherwise the behavior is undefined.
    !  The borrow output is guaranteed to be 0 or 1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: X, Y, BorrowIn
    tUInt64, INTENT(OUT)    :: Diff, BorrowOut
    OPTIONAL                :: BorrowOut

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Diff = X - Y - BorrowIn
    ! The difference will underflow if the top bit of x is not set and the top
    ! bit of y is set (^x & y) or if they are the same (^(x ^ y)) and a Borrow
    ! from the lower place happens. If that Borrow happens, the result will be
    ! 1 - 1 - 1 = 0 - 0 - 1 = 1 (& diff).
    IF (PRESENT(BorrowOut)) THEN
        BorrowOut = SHIFTR(IOR(IAND(NOT(X), Y), IAND(NOT(IEOR(X, Y)), Diff)), 63)
    END IF

    RETURN

END SUBROUTINE SubU64

!******************************************************************************

FUNCTION Reciprocal_2By1(D) RESULT(R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute the reciprocal (2**128 - 1) / D - 2**64 for normalized D
    ! based on Algorithm 2 from "Improved division by invariant integers".

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: D
    tUInt64             :: R

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: D0, D9, D40, D63, E, T
    tUInt64     :: PHi, PLo, V0, V1, V2, V3

!** FLOW

    D9  = SHIFTR(D, 55)
    V0  = ToInt64(RecTable(ToInt32(D9) - 256))

    D40 = SHIFTR(D, 24) + 1
    V1  = SHIFTL(V0, 11) - SHIFTR((V0 * V0) * D40, 40) - 1_kInt64

    V2  = SHIFTL(V1, 13) + SHIFTR(V1 * (ToInt64(Z'1000000000000000') - V1 * D40), 47)

    D0  = IAND(D, 1_kInt64)
    D63 = SHIFTR(D, 1) + D0     ! ceil(D/2)
    E   = IAND(SHIFTR(V2, 1), (0_kInt64 - D0)) - V2 * D63
    CALL UMul128(V2, E, PHi, PLo)
    V3  = SHIFTR(PHi, 1) + SHIFTL(V2, 31)

    CALL UMul128(V3, D, PHi, PLo)
    T = PLo + D
    IF (IEOR(T, MinI64) < IEOR(PLo, MinI64)) PHi = PHi + 1
    R = V3 - PHi - D

    RETURN

END FUNCTION Reciprocal_2By1

!******************************************************************************

FUNCTION Reciprocal_3By2(DHi, DLo) RESULT(R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute the reciprocal (2**128 - 1) / D - 2**64 for normalized D
    ! based on Algorithm 2 from "Improved division by invariant integers".

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)   :: DHi, DLo
    tUInt64               :: R

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: V, P, THi, TLo

!** FLOW

    V = Reciprocal_2By1(DHi)
    P = DHi * V
    P = P + DLo
    IF (IEOR(P, MinI64) < IEOR(DLo, MinI64)) THEN
        V = V - 1_kInt64
        IF (IEOR(P, MinI64) >= IEOR(DHi, MinI64)) THEN
            V = V - 1_kInt64
            P = P - DHi
        END IF
        P = P - DHi
    END IF

    CALL UMul128(V, DLo, THi, TLo)

    P = P + THi
    IF (IEOR(P, MinI64) < IEOR(THi, MinI64)) THEN
        V = V - 1_kInt64
        IF (IEOR(P, MinI64) >= IEOR(DHi, MinI64)) THEN
            IF ((IEOR(P,   MinI64) >  IEOR(DHi, MinI64)).OR.&
                (IEOR(TLo, MinI64) >= IEOR(DLo, MinI64))) V = V - 1_kInt64
        END IF
    END IF
    R = V

    RETURN

END FUNCTION Reciprocal_3By2

!******************************************************************************

SUBROUTINE UDivRem_2By1(UHi, ULo, D, V, Q, R)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform 128-bit unsigned integer division by 64-bit unsigned integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)     :: UHi, ULo, D, V
    tUInt64, INTENT(OUT)    :: Q, R

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: QHi, QLo, NewLo

!** FLOW

    ! Q128 = V*UHi
    CALL UMul128(V, UHi, QHi, QLo)

    ! Q128 = Q128 + U128
    NewLo = QLo + ULo
    IF (IEOR(NewLo, MinI64) < IEOR(QLo, MinI64)) THEN
        QHi = QHi + UHi + 1_kInt64
    ELSE
        QHi = QHi + UHi
    END IF
    QLo = NewLo

    QHi = QHi + 1_kInt64

    R = ULo - QHi*D

    IF (IEOR(R, MinI64) > IEOR(QLo, MinI64)) THEN
        QHi = QHi - 1_kInt64
        R = R + D
    END IF

    IF (IEOR(R, MinI64) >= IEOR(D, MinI64)) THEN
        QHi = QHi + 1_kInt64
        R = R - D
    END IF
    Q = QHi

    RETURN

END SUBROUTINE UDivRem_2By1

!******************************************************************************

SUBROUTINE UDivRem_3By2(U2, U1, U0, DHi, DLo, V, Q, RHi, RLo)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform 128-bit integer division by 64-bit integer

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN)   :: U2, U1, U0, DHi, DLo, V
    tUInt64, INTENT(OUT)  :: Q, RHi, RLo

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: QHi, QLo, NewLo, R1
    tUInt64     :: THi, TLo, SHi, SLo
    tLogical    :: Flag

!** FLOW

    ! Q128 = V*U2
    CALL UMul128(V, U2, QHi, QLo)

    ! Q128 = Q128 + UInt128(U2, U1)
    NewLo = QLo + U1
    IF (IEOR(NewLo, MinI64) < IEOR(QLo, MinI64)) THEN
        QHi = QHi + U2 + 1_kInt64
    ELSE
        QHi = QHi + U2
    END IF
    QLo = NewLo

    R1 = U1 - QHi * DHi

    ! T128 = DLo*QHi
    CALL UMul128(DLo, QHi, THi, TLo)

    ! R128 = UInt128(R1, U0) - T128 - D128
    SLo  = U0 - TLo
    IF (IEOR(U0, MinI64) < IEOR(TLo, MinI64)) THEN
        SHi = R1 - THi - 1_kInt64
    ELSE
        SHi = R1 - THi
    END IF
    RLo  = SLo - DLo
    IF (IEOR(SLo, MinI64) < IEOR(DLo, MinI64)) THEN
        RHi = SHi - DHi - 1_kInt64
    ELSE
        RHi = SHi - DHi
    END IF

    R1 = RHi

    QHi = QHi + 1_kInt64

    IF (R1 .UGE. QLo) THEN
        QHi = QHi - 1_kInt64
        ! R128 = R128 + D128
        NewLo = RLo + DLo
        IF (IEOR(NewLo, MinI64) < IEOR(RLo, MinI64)) THEN
            RHi = RHi + DHi + 1_kInt64
        ELSE
            RHi = RHi + DHi
        END IF
        RLo = NewLo
    END IF

    IF (RHi == DHi) THEN
        Flag = (IEOR(RLo, MinI64) >= IEOR(DLo, MinI64))
    ELSE
        Flag = (IEOR(RHi, MinI64) >= IEOR(DHi, MinI64))
    END IF
    IF (Flag) THEN
        QHi = QHi + 1_kInt64
        ! R128 = R128 - D128
        NewLo = RLo - DLo
        IF (IEOR(RLo, MinI64) < IEOR(DLo, MinI64)) THEN
            RHi = RHi - DHi - 1_kInt64
        ELSE
            RHi = RHi - DHi
        END IF
        RLo = NewLo
    END IF

    Q = QHi

    RETURN

END SUBROUTINE UDivRem_3By2

!******************************************************************************

END MODULE MBase_UIntUtil

!******************************************************************************
