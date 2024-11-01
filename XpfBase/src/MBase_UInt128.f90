
MODULE MBase_UInt128

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *UInt128* type, its related routines and useful *UInt128*
!   parameters.  The *UInt128* type is a derived type representing a **128-bit unsigned**
!   integer.   Various common operations usually available for integer types are provided
!   including arithmetic, bitwise, comparison, and conversion/construction operations.
!   The application programming interface (API) follows Fortran intrinsic integer types
!   as close as practical.  However, since Fortran does not have an unsigned integer type,
!   those operations that differ between signed and unsigned integers use the interfaces
!   similar to those operations used in the <a href="../module/mbase_uintutil.html">
!   MBase_UIntUtil</a> module.  For a 128-bit signed integer, use the
!   <a href="../module/mbase_sint128.html#type-sint128">SInt128</a> type.  For an unsigned
!   integer with precision higher than that provided by the *UInt128* type, a user can use
!   either the *ApInt32* type or *ApInt64* type available in the ***XpfHighPrec*** package. <br>
!   <br>
!  **Important Notes**: <br>
!   (1) For arithmetic operations, various types of unsigned integer types (32-, 64-
!   and 128-bit) are allowed.  However, the use of signed and unsigned integers in
!   the same operation is NOT allowed.  Signed integer types must be explicitly
!   converted to unsigned types before using in the arithmetic operations. <br>
!   (2) For comparison and bitwise operations that require two input arguments,
!   both arguments must only be the 128-bit unsigned integer type.  The operations
!   on mixed types are not provided.  Therefore, all other types must be explicitly
!   converted to the *UInt128* type before using in the comparison and bitwise
!   operations. <br>
!   (3) It is impossible to differentiate between signed and unsigned integers for
!   Fortran intrinsic types.  Therefore, it is a user responsibility to make sure
!   and be extremely careful not to mix up signed and unsigned integers in the same
!   expressions.  It should be noted that, in this module, the signed integer types
!   are mostly used in conversion operations.  In all other operations, all Fortran
!   intrinsic integer types are assumed to be unsigned. <br>
!  <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://github.com/abseil/abseil-cpp/tree/master/absl/numeric">
!       Absl's Numeric Library</a> <br>
!   [2] <a href="https://github.com/martint/int128/">Fast 128-bit math library
!       for Java</a> <br>
!   [3] <a href="https://github.com/chfast/intx">IntX: Extended precision integer C++
!       library</a>

!** USE STATEMENTS:
    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE, INTRINSIC :: ISO_FORTRAN_ENV,  ONLY: OUTPUT_UNIT
    USE ISO_C_BINDING,                  ONLY: C_LOC, C_F_POINTER, C_PTR, C_NULL_PTR
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_SIntUtil
    USE MBase_UIntUtil
    USE MBase_LargeTables,              ONLY: RecTable, Char2Digits, Char4Digits

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type + constructor
    PUBLIC :: UInt128
    ! assignment (only applicable for unsigned integers)
    PUBLIC :: ASSIGNMENT(=)
    ! conversion
    PUBLIC :: ToI32, ToI64, ToR32, ToR64, ToR128
    PUBLIC :: ToDecString, ToHexString
    ! comparison
    PUBLIC :: OPERATOR(==), OPERATOR(/=)
    PUBLIC :: OPERATOR(.ULT.), OPERATOR(.ULE.)
    PUBLIC :: OPERATOR(.UGT.), OPERATOR(.UGE.)
    PUBLIC :: CompareUnsigned
    ! arithmetic
    PUBLIC :: OPERATOR(+), OPERATOR(-)
    PUBLIC :: OPERATOR(*), OPERATOR(.UDIV.)
    PUBLIC :: Increment, Decrement, Add, Subtract
    PUBLIC :: Multiply, Divide, UMOD, UDivMod
    ! bitwise (general)
    PUBLIC :: SHIFTL, SHIFTR, ISHFT, ISHFTC
    PUBLIC :: IOR, IEOR, IAND, NOT, LEADZ, TRAILZ
    PUBLIC :: POPCNT, POPPAR, IBSET, IBCLR
    PUBLIC :: IBCHNG, BTEST, IBITS
    PUBLIC :: MoveBits  ! == MVBITS
    ! bitwise (specialized)
    PUBLIC :: ShiftLOnce, ShiftROnce
    PUBLIC :: ShiftL64, ShiftR64
    PUBLIC :: ShiftL63Down, ShiftR63Down
    PUBLIC :: ShiftL64Up, ShiftR64Up
    ! auxiliary
    PUBLIC :: MIN, MAX
    PUBLIC :: Display
    ! experimental
    PUBLIC :: UAddXp, USubXp, UMulXp, UDivModXp
    PUBLIC :: ToDecStrXp, ParseU128Xp
    PUBLIC :: ToU128Xp, ToR32Xp, ToR64Xp, ToR128Xp

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define     tUInt128        TYPE(UInt128)

!** MODULE PARAMETERS:
    ! module name
    tCharParam          :: ModName = 'MBase_UInt128'
    ! unsigned limit parameters
    tUInt64,  PARAMETER :: MaxU64 = ToInt64(Z'FFFFFFFFFFFFFFFF')    ! max unsigned 64-bit
    tUInt64,  PARAMETER :: MinU64 = ToInt64(Z'0100000000000000')    ! min unsigned 64-bit
    tUInt64,  PARAMETER :: MaxU32 = ToInt64(Z'01000000FFFFFFFF')    ! max unsigned 32-bit
    tUInt64,  PARAMETER :: MinU32 = ToInt64(Z'0100000000000000')    ! min unsigned 32-bit
    ! signed limit parameters
    tSInt64,  PARAMETER :: MaxI64 = ToInt64(Z'7FFFFFFFFFFFFFFF')    ! max signed 64-bit
    tSInt64,  PARAMETER :: MinI64 = ToInt64(Z'8000000000000000')    ! min signed 64-bit
    tSInt64,  PARAMETER :: MaxI32 = ToInt64(Z'010000007FFFFFFF')    ! max signed 32-bit
    tSInt64,  PARAMETER :: MinI32 = ToInt64(Z'0100000080000000')    ! min signed 32-bit
    ! miscellaneous
    tUInt64,  PARAMETER :: Mask32 = MaxU32
    tUInt64,  PARAMETER :: TopBit = SHIFTL(1_kInt64, 63)
    tLogical, PARAMETER :: AsUnsigned = TrueVal

!** DERIVED TYPE DEFINITIONS
    !> *UInt128* is a 128-bit unsigned integer type where the base
    !   of its components is 2<sup>64</sup>.  
    TYPE UInt128
        !% number representing upper 64 bits treated as unsigned
        tUInt64 :: High
        !% number representing lower 64 bits treated as unsigned
        tUInt64 :: Low
    END TYPE UInt128

!** MODULE PARAMETERS (PART 2):
    !% 128-bit unsigned parameter with maximum value
    tUInt128, PARAMETER, PUBLIC :: MaxU128   = UInt128(MaxU64, MaxU64)
    !% 128-bit unsigned parameter with minimum value
    tUInt128, PARAMETER, PUBLIC :: MinU128   = UInt128(MinU64, MinU64)
    !% 128-bit unsigned parameter with value of one
    tUInt128, PARAMETER, PUBLIC :: OneU128   = UInt128(MinU64, 1_kInt64)
    !% 128-bit unsigned parameter with value of zero
    tUInt128, PARAMETER, PUBLIC :: ZeroU128  = MinU128
    !% Used to cast a 64-bit integer to a 128bit integer without getting unwanted sign extension
    tUInt128, PARAMETER, PUBLIC :: MASKI64   = UInt128(MinU64, MaxU64)
    ! private parameters
    tUInt128, PARAMETER         :: TenU128   = UInt128(MinU64, 10_kInt64)
    tUInt128, PARAMETER         :: DivBase10 = UInt128(0_kInt64, ToInt64(Z'8AC7230489E80000'))  ! 10**19
    tUInt128, PARAMETER         :: DivBase16 = UInt128(0_kInt64, ToInt64(Z'1000000000000000'))  ! 16**15

!** INTERFACE DEFINITIONS:
    INTERFACE
        MODULE SUBROUTINE U128_DivMod_Java(Dividend, Divisor, Quotient, Remainder)
            !^ To perform division of two UInt128 objects (Dividend / Divisor)
            !  and return both the quotient and the remainder. <br>
            !  This routine is based on reference #2.
            tUInt128, INTENT(IN)    :: Dividend
            tUInt128, INTENT(IN)    :: Divisor
            tUInt128, INTENT(OUT)   :: Quotient
            tUInt128, INTENT(OUT)   :: Remainder
        END SUBROUTINE
        MODULE SUBROUTINE U128_DivMod_IntX(Dividend, Divisor, Quotient, Remainder)
            !^ To perform division of two UInt128 objects (Dividend / Divisor)
            !  and return both the quotient and the remainder. <br>
            !  This routine is based on reference #3.
            tUInt128, INTENT(IN)    :: Dividend
            tUInt128, INTENT(IN)    :: Divisor
            tUInt128, INTENT(OUT)   :: Quotient
            tUInt128, INTENT(OUT)   :: Remainder
        END SUBROUTINE
    END INTERFACE
    !-----------------------------------------------
    !-----      conversion operations          -----
    !-----------------------------------------------
    INTERFACE ASSIGNMENT(=)
        !^ **Operator Overload**: ASSIGNMENT(=) <br>
        !  **Purpose**:  To convert between a 128-bit unsigned integer and
        !   other unsigned integers (32- and 64-bit integers). <br>
        !  **Usage**: <br>
        !   ! convert 32-bit unsigned integer to 128-bit unsigned integer <br>
        !   --->    U128 = U32 <br>
        !   ! convert 128-bit unsigned integer to 64-bit unsigned integer <br>
        !   --->    U64 = U128
        MODULE SUBROUTINE U128_From_U32(U128, U32)
            !^ To convert an unsigned 32-bit integer number to an unsigned 128-bit integer number. <br>
            !  *Usage*: U128 = U32
            tUInt128, INTENT(OUT)   :: U128
            tUInt32,  INTENT(IN)    :: U32      !! number treated as unsigned
        END SUBROUTINE U128_From_U32
        !----------------------------------------------------------------------
        MODULE SUBROUTINE U128_From_U64(U128, U64)
            !^ To convert an unsigned 64-bit integer number to an unsigned 128-bit integer number. <br>
            !  *Usage*: U128 = U64
            tUInt128, INTENT(OUT)   :: U128
            tUInt64,  INTENT(IN)    :: U64      !! number treated as unsigned
        END SUBROUTINE U128_From_U64
        !----------------------------------------------------------------------
        MODULE SUBROUTINE U128_To_U32(U32, U128)
            !^ To convert an unsigned 128-bit integer number to an unsigned 32-bit integer number. <br>
            !  *Usage*: U32 = U128
            tUInt32,  INTENT(OUT)   :: U32      !! number treated as unsigned
            tUInt128, INTENT(IN)    :: U128
        END SUBROUTINE U128_To_U32
        !----------------------------------------------------------------------
        MODULE SUBROUTINE U128_To_U64(U64, U128)
            !^ To convert an unsigned 128-bit integer number to an unsigned 64-bit integer number. <br>
            !  *Usage*: U64 = U128
            tUInt64,  INTENT(OUT)   :: U64      !! number treated as unsigned
            tUInt128, INTENT(IN)    :: U128
        END SUBROUTINE U128_To_U64
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE UInt128
        !^ **Constructor Interface**: UInt128 <br>
        !  **Purpose**:  To construct a 128-bit unsigned integer. <br>
        !  **Usage**: <br>
        !   ! construct U128 from 32-bit signed integer <br>
        !   --->    U128 = UInt128(I32) <br>
        !   ! construct U128 from 64-bit intrinsic integer treated as unsigned <br>
        !   --->    U128 = UInt128(I64, AsUnsigned=.TRUE.) <br>
        !   ! construct U128 from 128-bit real number <br>
        !   --->    U128 = UInt128(R128) <br>
        !   ! construct U128 from a decimal string <br>
        !   --->    U128 = UInt128('1234567890987654321011223344')
        MODULE FUNCTION I32_To_U128(I32, AsUnsigned) RESULT(U128)
            !^ To convert a signed 32-bit integer number to an unsigned 128-bit integer number
            !  or to convert an unsigned 32-bit integer number to an unsigned 128-bit integer
            !  number if the specified flag is present and true. <br>
            !  *Usage*:  <br>
            !  ---> U128 = UInt128(I32) <br>
            !  ---> U128 = UInt128(I32, AsUnsigned=.TRUE.)
            tSInt32,            INTENT(IN)  :: I32          !! number treated as signed (default)
            tLogical, OPTIONAL, INTENT(IN)  :: AsUnsigned   !! if present and true, number treated as unsigned
            tUInt128                        :: U128
        END FUNCTION I32_To_U128
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_To_U128(I64, AsUnsigned) RESULT(U128)
            !^ To convert a signed 64-bit integer number to an unsigned 128-bit integer number
            !  or to convert an unsigned 64-bit integer number to an unsigned 128-bit integer
            !  number if the specified flag is present and true. <br>
            !  *Usage*:  <br>
            !  ---> U128 = UInt128(I64) <br>
            !  ---> U128 = UInt128(I64, AsUnsigned=.TRUE.)
            tSInt64,            INTENT(IN)  :: I64          !! number treated as signed (default)
            tLogical, OPTIONAL, INTENT(IN)  :: AsUnsigned   !! if present and true, number treated as unsigned
            tUInt128                        :: U128
        END FUNCTION I64_To_U128
        !----------------------------------------------------------------------
        MODULE FUNCTION R32_To_U128(R32) RESULT(U128)
            !^ To convert a 32-bit floating point number to an unsigned 128-bit integer number. <br>
            !  *Usage*: U128 = UInt128(R32)
            tRealSP, INTENT(IN) :: R32
            tUInt128            :: U128
        END FUNCTION R32_To_U128
        !----------------------------------------------------------------------
        MODULE FUNCTION R64_To_U128(R64) RESULT(U128)
            !^ To convert a 64-bit floating point number to an unsigned 128-bit integer number. <br>
            !  *Usage*: U128 = UInt128(R64)
            tRealDP, INTENT(IN) :: R64
            tUInt128            :: U128
        END FUNCTION R64_To_U128
        !----------------------------------------------------------------------
        MODULE FUNCTION R128_To_U128(R128) RESULT(U128)
            !^ To convert a 128-bit floating point number to an unsigned 128-bit integer number. <br>
            !  *Usage*: U128 = UInt128(R128)
            tRealQP, INTENT(IN)   :: R128
            tUInt128            :: U128
        END FUNCTION R128_To_U128
        !----------------------------------------------------------------------
        MODULE FUNCTION DecString_To_U128(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to an unsigned 128-bit integer value. <br>
            !  *Usage*:  <br>
            !  ---> U128 = UInt128('1234567890987654321011223344') <br>
            !  ---> U128 = UInt128(NumStr, ErrFlag) <br>
            !  ---> U128 = UInt128(NumStr, ErrMsg=Message) <br>
            !  ---> U128 = UInt128(NumStr, ErrFlag, ErrMsg)
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tUInt128                            :: Number   !! unsigned 128-bit integer value
        END FUNCTION DecString_To_U128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToI32
        !^ **Function Interface**: ToI32 <br>
        !  **Purpose**:  To convert a 128-bit unsigned integer to a
        !   32-bit signed integer. <br>
        !  **Usage**: <br>
        !   --->    I32 = ToI32(U128)
        MODULE FUNCTION I32_From_U128(U128) RESULT(I32)
            !^ To convert an unsigned 128-bit integer number to a signed 32-bit integer number.
            tUInt128, INTENT(IN)    :: U128
            tSInt32                 :: I32      !! number treated as signed
        END FUNCTION I32_From_U128
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToI64
        !^ **Function Interface**: ToI64 <br>
        !  **Purpose**:  To convert a 128-bit unsigned integer to
        !   a 64-bit signed integer. <br>
        !  **Usage**: <br>
        !   --->    I64 = ToI64(U128)
        MODULE FUNCTION I64_From_U128(U128) RESULT(I64)
            !^ To convert an unsigned 128-bit integer number to a signed 64-bit integer number.
            tUInt128, INTENT(IN)    :: U128
            tSInt64                 :: I64      !! number treated as signed
        END FUNCTION I64_From_U128
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToR32
        !^ **Function Interface**: ToR32 <br>
        !  **Purpose**:  To convert a 128-bit unsigned integer to
        !   a 32-bit floating point (real) number. <br>
        !  **Usage**: <br>
        !   --->    R32 = ToR32(U128)
        MODULE FUNCTION R32_From_U128(U128) RESULT(R32)
            !^ To convert an unsigned 128-bit integer number to a 32-bit floating point number.
            tUInt128, INTENT(IN)    :: U128
            tRealSP                 :: R32
        END FUNCTION R32_From_U128
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToR64
        !^ **Function Interface**: ToR64 <br>
        !  **Purpose**:  To convert a 128-bit unsigned integer to
        !   a 64-bit floating point (real) number. <br>
        !  **Usage**: <br>
        !   --->    R64 = ToR64(U128)
        MODULE FUNCTION R64_From_U128(U128) RESULT(R64)
            !^ To convert an unsigned 128-bit integer number to a 64-bit floating point number.
            tUInt128, INTENT(IN)    :: U128
            tRealDP                 :: R64
        END FUNCTION R64_From_U128
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToR128
        !^ **Function Interface**: ToR128 <br>
        !  **Purpose**:  To convert a 128-bit unsigned integer to
        !   a 128-bit floating point (real) number. <br>
        !  **Usage**: <br>
        !   --->    R128 = ToR128(U128)
        MODULE FUNCTION R128_From_U128(U128) RESULT(R128)
            !^ To convert an unsigned 128-bit integer number to a 128-bit floating point number.
            tUInt128, INTENT(IN)    :: U128
            tRealQP                   :: R128
        END FUNCTION R128_From_U128
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToDecString
        !^ **Function Interface**: ToDecString <br>
        !  **Purpose**:  To convert a 128-bit unsigned integer to
        !   a decimal string. <br>
        !  **Usage**: <br>
        !   --->    Str = ToDecString(U128)
        MODULE FUNCTION DecString_From_U128(U128) RESULT(Str)
            !^ To convert an unsigned 128-bit integer number to a decimal string.
            tUInt128, INTENT(IN)    :: U128
            tCharAlloc              :: Str
        END FUNCTION DecString_From_U128
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToHexString
        !^ **Function Interface**: ToHexString <br>
        !  **Purpose**:  To convert a 128-bit unsigned integer to
        !   a hexadecimal string. <br>
        !  **Usage**: <br>
        !   --->    Str = ToHexString(U128)
        MODULE FUNCTION HexString_From_U128(U128) RESULT(Str)
            !^ To convert an unsigned 128-bit integer number to a hexadecimal string.
            tUInt128, INTENT(IN)    :: U128
            tCharAlloc              :: Str
        END FUNCTION HexString_From_U128
    END INTERFACE
    !-----------------------------------------------
    !-----      comparison operations          -----
    !-----------------------------------------------
    INTERFACE OPERATOR(==)
        !^ **Operator Overload**: OPERATOR(==) <br>
        !  **Purpose**:  To check if values of two 128-bit unsigned integers are equal.
        !   Return .TRUE. if both values are equal; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS == RHS <br>
        !   --->    IF (LHS .EQ. RHS) DoSomething
        MODULE FUNCTION U128_Equal(LHS, RHS) RESULT(Flag)
            !^ To check whether two Uint128 objects are equal.
            tUInt128, INTENT(IN)    :: LHS
            tUInt128, INTENT(IN)    :: RHS
            tLogical                :: Flag
        END FUNCTION U128_Equal
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(/=)
        !^ **Operator Overload**: OPERATOR(/=) <br>
        !  **Purpose**:  To check if values of two 128-bit unsigned integers are not equal.
        !   Return .TRUE. if both values are NOT equal; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS /= RHS <br>
        !   --->    IF (LHS .NE. RHS) DoSomething
        MODULE FUNCTION U128_NotEqual(LHS, RHS) RESULT(Flag)
            !^ To check whether two Uint128 objects are NOT equal.
            tUInt128, INTENT(IN)    :: LHS
            tUInt128, INTENT(IN)    :: RHS
            tLogical                :: Flag
        END FUNCTION U128_NotEqual
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(.ULT.)
        !^ **Operator Overload**: OPERATOR(.ULT.) <br>
        !  **Purpose**:  To check if the LHS value is less than the RHS value.
        !   Return .TRUE. if LHS < RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS .ULT. RHS <br>
        !   --->    IF (LHS .ULT. RHS) DoSomething
        MODULE FUNCTION U128_LessThan(LHS, RHS) RESULT(Flag)
            !^ To check whether the LHS Uint128 object is less than the RHS Uint128 object.
            tUInt128, INTENT(IN)    :: LHS
            tUInt128, INTENT(IN)    :: RHS
            tLogical                :: Flag
        END FUNCTION U128_LessThan
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(.ULE.)
        !^ **Operator Overload**: OPERATOR(.ULE.) <br>
        !  **Purpose**:  To check if the LHS value is less than or equal to the RHS value.
        !   Return .TRUE. if LHS <= RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS .ULE. RHS <br>
        !   --->    IF (LHS .ULE. RHS) DoSomething
        MODULE FUNCTION U128_LessEqual(LHS, RHS) RESULT(Flag)
            !^ To check whether the LHS Uint128 object is less than or equal to the RHS Uint128 object.
            tUInt128, INTENT(IN)    :: LHS
            tUInt128, INTENT(IN)    :: RHS
            tLogical                :: Flag
        END FUNCTION U128_LessEqual
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(.UGT.)
        !^ **Operator Overload**: OPERATOR(.UGT.) <br>
        !  **Purpose**:  To check if the LHS value is greater than the RHS value.
        !   Return .TRUE. if LHS > RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS .UGT. RHS <br>
        !   --->    IF (LHS .UGT. RHS) DoSomething
        MODULE FUNCTION U128_GreaterThan(LHS, RHS) RESULT(Flag)
            !^ To check whether the LHS Uint128 object is greater than the RHS Uint128 object.
            tUInt128, INTENT(IN)    :: LHS
            tUInt128, INTENT(IN)    :: RHS
            tLogical                :: Flag
        END FUNCTION U128_GreaterThan
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(.UGE.)
        !^ **Operator Overload**: OPERATOR(.UGE.) <br>
        !  **Purpose**:  To check if the LHS value is greater than or equal to the RHS value.
        !   Return .TRUE. if LHS >= RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS .UGE. RHS <br>
        !   --->    IF (LHS .UGE. RHS) DoSomething
        MODULE FUNCTION U128_GreaterEqual(LHS, RHS) RESULT(Flag)
            !^ To check whether the LHS Uint128 object is greater than or equal to the RHS Uint128 object.
            tUInt128, INTENT(IN)    :: LHS
            tUInt128, INTENT(IN)    :: RHS
            tLogical                :: Flag
        END FUNCTION U128_GreaterEqual
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE CompareUnsigned
        !^ **Function Interface**: CompareUnsigned <br>
        !  **Purpose**:  To compare two 128-bit unsigned integers and return <br>
        !   -1 if LHS < RHS, <br>
        !    0 if LHS == RHS, or <br>
        !    1 if LHS > RHS. <br>
        !  **Usage**: <br>
        !   --->    Flag = CompareUnsigned(LHS, RHS) <br>
        !   --->    IF (CompareUnsigned(LHS, RHS) /= 0) DoSomething
        MODULE FUNCTION U128_Compare(LHS, RHS) RESULT(Flag)
            !^ To compare LHS and RHS objects. <br>
            ! - return -1 if LHS < RHS <br>
            ! - return  0 if LHS == RHS <br>
            ! - return +1 if LHS > RHS
            tUInt128, INTENT(IN)    :: LHS
            tUInt128, INTENT(IN)    :: RHS
            tSInt32                 :: Flag
        END FUNCTION U128_Compare
    END INTERFACE
    !-----------------------------------------------
    !-----      arithmetic operations          -----
    !-----------------------------------------------
    INTERFACE OPERATOR(+)
        !^ **Operator Overload**: OPERATOR(+) <br>
        !  **Purpose**:  To perform a summation of two unsigned integers
        !   (at least one of which is a 128-bit unsigned integer) or
        !   to add a unary plus sign to a 128-bit unsigned integer
        !   (which has no effect on the unsigned integer). <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = +INPUT <br>
        !   --->    OUTPUT = FIRST_IN + SECOND_IN
        MODULE FUNCTION U128_UnaryPlus(InVal) RESULT(OutVal)
            !^ To return result of the unary plus sign of the Uint128 object.
            tUInt128, INTENT(IN)    :: InVal
            tUInt128                :: OutVal
        END FUNCTION U128_UnaryPlus
        !----------------------------------------------------------------------
        MODULE FUNCTION U128_Plus_U32(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform addition:  OutVal = LhsVal + RhsVal.
            tUInt128, INTENT(IN)    :: LhsVal
            tUInt32,  INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U128_Plus_U32
        !----------------------------------------------------------------------
        MODULE FUNCTION U32_Plus_U128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform addition:  OutVal = LhsVal + RhsVal.
            tUInt32,  INTENT(IN)    :: LhsVal
            tUInt128, INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U32_Plus_U128
        !----------------------------------------------------------------------
        MODULE FUNCTION U128_Plus_U64(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform addition:  OutVal = LhsVal + RhsVal.
            tUInt128, INTENT(IN)    :: LhsVal
            tUInt64,  INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U128_Plus_U64
        !----------------------------------------------------------------------
        MODULE FUNCTION U64_Plus_U128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform addition:  OutVal = LhsVal + RhsVal.
            tUInt64,  INTENT(IN)    :: LhsVal
            tUInt128, INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U64_Plus_U128
        !----------------------------------------------------------------------
        MODULE FUNCTION U128_Plus_U128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform addition:  OutVal = LhsVal + RhsVal.
            tUInt128, INTENT(IN)    :: LhsVal
            tUInt128, INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U128_Plus_U128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(-)
        !^ **Operator Overload**: OPERATOR(-) <br>
        !  **Purpose**:  To perform a subtraction of two unsigned integers
        !   (at least one of which is a 128-bit unsigned integer) or
        !   to perform a negation of a 128-bit unsigned integer. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = -INPUT <br>
        !   --->    OUTPUT = FIRST_IN - SECOND_IN <br>
        !  **Important Note**:  For subtraction of unsigned integers, value of
        !   FIRST_IN must always be greater than SECOND_IN.  Otherwise, value of
        !   OUTPUT is NOT valid.
        MODULE FUNCTION U128_Negate(InVal) RESULT(OutVal)
            !^ To negate the Uint128 object.
            tUInt128, INTENT(IN)    :: InVal
            tUInt128                :: OutVal
        END FUNCTION U128_Negate
        !----------------------------------------------------------------------
        MODULE FUNCTION U128_Minus_U32(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform subtraction:  OutVal = LhsVal - RhsVal.
            tUInt128, INTENT(IN)    :: LhsVal
            tUInt32,  INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U128_Minus_U32
        !----------------------------------------------------------------------
        MODULE FUNCTION U32_Minus_U128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform subtraction:  OutVal = LhsVal - RhsVal.
            tUInt32,  INTENT(IN)    :: LhsVal
            tUInt128, INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U32_Minus_U128
        !----------------------------------------------------------------------
        MODULE FUNCTION U128_Minus_U64(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform subtraction:  OutVal = LhsVal - RhsVal.
            tUInt128, INTENT(IN)    :: LhsVal
            tUInt64,  INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U128_Minus_U64
        !----------------------------------------------------------------------
        MODULE FUNCTION U64_Minus_U128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform subtraction:  OutVal = LhsVal - RhsVal.
            tUInt64,  INTENT(IN)    :: LhsVal
            tUInt128, INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U64_Minus_U128
        !----------------------------------------------------------------------
        MODULE FUNCTION U128_Minus_U128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform subtraction:  OutVal = LhsVal - RhsVal.
            tUInt128, INTENT(IN)    :: LhsVal
            tUInt128, INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U128_Minus_U128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(*)
        !^ **Operator Overload**: OPERATOR( * ) <br>
        !  **Purpose**:  To perform a multiplication of two unsigned integers
        !   (at least one of which is a 128-bit unsigned integer). <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = FIRST_IN * SECOND_IN
        MODULE FUNCTION U32_Multiply_U128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform multiplication:  OutVal = LhsVal * RhsVal.
            tUInt32,  INTENT(IN)    :: LhsVal
            tUInt128, INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U32_Multiply_U128
        !----------------------------------------------------------------------
        MODULE FUNCTION U128_Multiply_U32(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform multiplication:  OutVal = LhsVal * RhsVal.
            tUInt128, INTENT(IN)    :: LhsVal
            tUInt32,  INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U128_Multiply_U32
        !----------------------------------------------------------------------
        MODULE FUNCTION U64_Multiply_U128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform multiplication:  OutVal = LhsVal * RhsVal.
            tUInt64,  INTENT(IN)    :: LhsVal
            tUInt128, INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U64_Multiply_U128
        !----------------------------------------------------------------------
        MODULE FUNCTION U128_Multiply_U64(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform multiplication:  OutVal = LhsVal * RhsVal.
            tUInt128, INTENT(IN)    :: LhsVal
            tUInt64,  INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U128_Multiply_U64
        !----------------------------------------------------------------------
        MODULE FUNCTION U128_Multiply_U128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform multiplication:  OutVal = LhsVal * RhsVal.
            tUInt128, INTENT(IN)    :: LhsVal
            tUInt128, INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U128_Multiply_U128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(.UDIV.)
        !^ **Operator Overload**: OPERATOR(.UDIV.) <br>
        !  **Purpose**:  To return the quotient of a division of two unsigned integers,
        !   where the dividend (numerator) is a 128-bit unsigned integer and the
        !   divisor (denominator) can be 32-, 64- or 128-bit unsigned integer. <br>
        !  **Usage**: <br>
        !   --->    QUOT = NUMER .UDIV. DENOM
        MODULE FUNCTION U128_Divide_U32(Dividend, Divisor) RESULT(Quotient)
            !^ To perform division:  Quotient = Dividend / Divisor.
            tUInt128, INTENT(IN)    :: Dividend
            tUInt32,  INTENT(IN)    :: Divisor
            tUInt128                :: Quotient
        END FUNCTION U128_Divide_U32
        !----------------------------------------------------------------------
        MODULE FUNCTION U128_Divide_U64(Dividend, Divisor) RESULT(Quotient)
            !^ To perform division:  Quotient = Dividend / Divisor.
            tUInt128, INTENT(IN)    :: Dividend
            tUInt64,  INTENT(IN)    :: Divisor
            tUInt128                :: Quotient
        END FUNCTION U128_Divide_U64
        !----------------------------------------------------------------------
        MODULE FUNCTION U128_Divide_U128(Dividend, Divisor) RESULT(Quotient)
            !^ To perform division:  Quotient = Dividend / Divisor.
            tUInt128, INTENT(IN)    :: Dividend
            tUInt128, INTENT(IN)    :: Divisor
            tUInt128                :: Quotient
        END FUNCTION U128_Divide_U128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE UMOD
        !^ **Function Interface**: UMOD <br>
        !  **Purpose**:  To return the remainder of a division of two unsigned integers,
        !   where the dividend (numerator) is a 128-bit unsigned integer and the
        !   divisor (denominator) can be 32-, 64- or 128-bit unsigned integer. <br>
        !  **Usage**: <br>
        !   --->    REM = UMOD(NUMER, DENOM)
        MODULE FUNCTION U128_Mod_U32(Dividend, Divisor) RESULT(Remainder)
            !^ To perform modulation:  Remainder = Dividend MOD Divisor.
            tUInt128, INTENT(IN)    :: Dividend
            tUInt32,  INTENT(IN)    :: Divisor
            tUInt128                :: Remainder
        END FUNCTION U128_Mod_U32
        !----------------------------------------------------------------------
        MODULE FUNCTION U128_Mod_U64(Dividend, Divisor) RESULT(Remainder)
            !^ To perform modulation:  Remainder = Dividend MOD Divisor.
            tUInt128, INTENT(IN)    :: Dividend
            tUInt64,  INTENT(IN)    :: Divisor
            tUInt128                :: Remainder
        END FUNCTION U128_Mod_U64
        !----------------------------------------------------------------------
        MODULE FUNCTION U128_Mod_U128(Dividend, Divisor) RESULT(Remainder)
            !^ To perform modulation:  Remainder = Dividend MOD Divisor.
            tUInt128, INTENT(IN)    :: Dividend
            tUInt128, INTENT(IN)    :: Divisor
            tUInt128                :: Remainder
        END FUNCTION U128_Mod_U128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE UDivMod
        !^ **Subroutine Interface**: UDivMod <br>
        !  **Purpose**:  To perform a division of two unsigned integers (where the
        !   dividend (numerator) is a 128-bit unsigned integer and the divisor
        !   (denominator) can be 32-, 64- or 128-bit unsigned integer) and
        !   to return both the quotient and the remainder. <br>
        !  **Usage**: <br>
        !   --->    CALL UDivMod(NUMER, DENOM, QUOT, REM)
        MODULE SUBROUTINE U128_DivMod_U32(Dividend, Divisor, Quotient, Remainder)
            !^ To perform division:  Quotient = Dividend / Divisor. <br>
            !  Return both quotient and remainder.
            tUInt128, INTENT(IN)    :: Dividend
            tUInt32,  INTENT(IN)    :: Divisor
            tUInt128, INTENT(OUT)   :: Quotient
            tUInt128, INTENT(OUT)   :: Remainder
        END SUBROUTINE U128_DivMod_U32
        !----------------------------------------------------------------------
        MODULE SUBROUTINE U128_DivMod_U64(Dividend, Divisor, Quotient, Remainder)
            !^ To perform division:  Quotient = Dividend / Divisor. <br>
            !  Return both quotient and remainder.
            tUInt128, INTENT(IN)    :: Dividend
            tUInt64,  INTENT(IN)    :: Divisor
            tUInt128, INTENT(OUT)   :: Quotient
            tUInt128, INTENT(OUT)   :: Remainder
        END SUBROUTINE U128_DivMod_U64
        !----------------------------------------------------------------------
        MODULE SUBROUTINE U128_DivMod_U128(Dividend, Divisor, Quotient, Remainder)
            !^ To perform division:  Quotient = Dividend / Divisor. <br>
            !  Return both quotient and remainder.
            tUInt128, INTENT(IN)    :: Dividend
            tUInt128, INTENT(IN)    :: Divisor
            tUInt128, INTENT(OUT)   :: Quotient
            tUInt128, INTENT(OUT)   :: Remainder
        END SUBROUTINE U128_DivMod_U128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Increment
        !^ **Subroutine Interface**: Increment <br>
        !  **Purpose**:  To increase value of a 128-bit unsigned integer by one. <br>
        !  **Usage**: <br>
        !   --->    CALL Increment(U128)
        MODULE SUBROUTINE U128_Increment(Val)
            !^ To increase value of the input by 1.
            tUInt128, INTENT(INOUT) :: Val
        END SUBROUTINE U128_Increment
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Decrement
        !^ **Subroutine Interface**: Decrement <br>
        !  **Purpose**:  To decrease value of a 128-bit unsigned integer by one. <br>
        !  **Usage**: <br>
        !   --->    CALL Decrement(U128)
        MODULE SUBROUTINE U128_Decrement(Val)
            !^ To decrease value of the input by 1.
            tUInt128, INTENT(INOUT) :: Val
        END SUBROUTINE U128_Decrement
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Add
        !^ **Subroutine Interface**: Add <br>
        !  **Purpose**:  To perform addition: This = This + Other. <br>
        !  **Usage**: <br>
        !   --->    CALL Add(This, Other)
        MODULE SUBROUTINE U128_Add_U32(This, Other)
            !^ To perform addition:  This = This + Other.
            tUInt128, INTENT(INOUT) :: This
            tUInt32,  INTENT(IN)    :: Other
        END SUBROUTINE U128_Add_U32
        !----------------------------------------------------------------------
        MODULE SUBROUTINE U128_Add_U64(This, Other)
            !^ To perform addition:  This = This + Other.
            tUInt128, INTENT(INOUT) :: This
            tUInt64,  INTENT(IN)    :: Other
        END SUBROUTINE U128_Add_U64
        !----------------------------------------------------------------------
        MODULE SUBROUTINE U128_Add_U128(This, Other)
            !^ To perform addition:  This = This + Other.
            tUInt128, INTENT(INOUT) :: This
            tUInt128, INTENT(IN)    :: Other
        END SUBROUTINE U128_Add_U128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Subtract
        !^ **Subroutine Interface**: Subtract <br>
        !  **Purpose**:  To perform subtraction: This = This - Other. <br>
        !  **Usage**: <br>
        !   --->    CALL Subtract(This, Other) <br>
        !  **Important Note**:  For subtraction of unsigned integers, value of This
        !   must always be greater than Other.  Otherwise, value of the returned
        !   This is NOT valid.
        MODULE SUBROUTINE U128_Subtract_U32(This, Other)
            !^ To perform subtraction:  This = This - Other.
            tUInt128, INTENT(INOUT) :: This
            tUInt32,  INTENT(IN)    :: Other
        END SUBROUTINE U128_Subtract_U32
        !----------------------------------------------------------------------
        MODULE SUBROUTINE U128_Subtract_U64(This, Other)
            !^ To perform subtraction:  This = This - Other.
            tUInt128, INTENT(INOUT) :: This
            tUInt64,  INTENT(IN)    :: Other
        END SUBROUTINE U128_Subtract_U64
        !----------------------------------------------------------------------
        MODULE SUBROUTINE U128_Subtract_U128(This, Other)
            !^ To perform subtraction:  This = This - Other.
            tUInt128, INTENT(INOUT) :: This
            tUInt128, INTENT(IN)    :: Other
        END SUBROUTINE U128_Subtract_U128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Multiply
        !^ **Subroutine Interface**: Multiply <br>
        !  **Purpose**:  To perform multiplication: This = This * Other. <br>
        !  **Usage**: <br>
        !   --->    CALL Multiply(This, Other)
        MODULE SUBROUTINE U128_Times_U32(This, Other)
            !^ To perform multiplication:  This = This * Other.
            tUInt128, INTENT(INOUT) :: This
            tUInt32,  INTENT(IN)    :: Other
        END SUBROUTINE U128_Times_U32
        !----------------------------------------------------------------------
        MODULE SUBROUTINE U128_Times_U64(This, Other)
            !^ To perform multiplication:  This = This * Other.
            tUInt128, INTENT(INOUT) :: This
            tUInt64,  INTENT(IN)    :: Other
        END SUBROUTINE U128_Times_U64
        !----------------------------------------------------------------------
        MODULE SUBROUTINE U128_Times_U128(This, Other)
            !^ To perform multiplication:  This = This * Other.
            tUInt128, INTENT(INOUT) :: This
            tUInt128, INTENT(IN)    :: Other
        END SUBROUTINE U128_Times_U128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Divide
        !^ **Subroutine Interface**: Divide <br>
        !  **Purpose**:  To perform a division: This = This / Other. <br>
        !  **Usage**: <br>
        !   --->    CALL Divide(This, Other)
        MODULE SUBROUTINE U128_Over_U32(This, Other, Remainder)
            !^ To perform division:  This = This / Other.
            tUInt128,          INTENT(INOUT)    :: This
            tUInt32,           INTENT(IN)       :: Other
            tUInt32, OPTIONAL, INTENT(OUT)      :: Remainder
        END SUBROUTINE U128_Over_U32
        !----------------------------------------------------------------------
        MODULE SUBROUTINE U128_Over_U64(This, Other, Remainder)
            !^ To perform division:  This = This / Other.
            tUInt128,          INTENT(INOUT)    :: This
            tUInt64,           INTENT(IN)       :: Other
            tUInt64, OPTIONAL, INTENT(OUT)      :: Remainder
        END SUBROUTINE U128_Over_U64
        !----------------------------------------------------------------------
        MODULE SUBROUTINE U128_Over_U128(This, Other, Remainder)
            !^ To perform division:  This = This / Other.
            tUInt128,           INTENT(INOUT)   :: This
            tUInt128,           INTENT(IN)      :: Other
            tUInt128, OPTIONAL, INTENT(OUT)     :: Remainder
        END SUBROUTINE U128_Over_U128
        !----------------------------------------------------------------------
    END INTERFACE
    !-----------------------------------------------
    !-----       bitwise operations            -----
    !-----------------------------------------------
    INTERFACE ShiftLOnce
        !^ **Function Interface**: ShiftLOnce <br>
        !  **Purpose**:  To perform logical left shift by 1. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftLOnce(IN)
        MODULE FUNCTION U128_ShiftLeftOnce(InVal) RESULT(OutVal)
            !^ To perform logical left shift by 1.
            tUInt128, INTENT(IN)    :: InVal
            tUInt128                :: OutVal
        END FUNCTION U128_ShiftLeftOnce
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftROnce
        !^ **Function Interface**: ShiftROnce <br>
        !  **Purpose**:  To perform logical right shift by 1. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftROnce(IN)
        MODULE FUNCTION U128_ShiftRightOnce(InVal) RESULT(OutVal)
            !^ To perform logical right shift by 1.
            tUInt128, INTENT(IN)    :: InVal
            tUInt128                :: OutVal
        END FUNCTION U128_ShiftRightOnce
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftL64
        !^ **Function Interface**: ShiftL64 <br>
        !  **Purpose**:  To perform logical left shift by 64. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftL64(IN)
        MODULE FUNCTION U128_ShiftLeft64(InVal) RESULT(OutVal)
            !^ To perform logical left shift by 64.
            tUInt128, INTENT(IN)    :: InVal
            tUInt128                :: OutVal
        END FUNCTION U128_ShiftLeft64
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftR64
        !^ **Function Interface**: ShiftR64 <br>
        !  **Purpose**:  To perform logical right shift by 64. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftR64(IN)
        MODULE FUNCTION U128_ShiftRight64(InVal) RESULT(OutVal)
            !^ To perform logical right shift by 64.
            tUInt128, INTENT(IN)    :: InVal
            tUInt128                :: OutVal
        END FUNCTION U128_ShiftRight64
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftL63Down
        !^ **Function Interface**: ShiftL63Down <br>
        !  **Purpose**:  To perform logical left shift by 63 or less. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftL63Down(IN, 11)
        MODULE FUNCTION U128_ShiftLeft63Down(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical left shift by 63 or less.
            tUInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 63
            tUInt128                :: OutVal
        END FUNCTION U128_ShiftLeft63Down
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftR63Down
        !^ **Function Interface**: ShiftR63Down <br>
        !  **Purpose**:  To perform logical right shift by 63 or less. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftR63Down(IN, 53)
        MODULE FUNCTION U128_ShiftRight63Down(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical right shift by 63 or less.
            tUInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 63
            tUInt128                :: OutVal
        END FUNCTION U128_ShiftRight63Down
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftL64Up
        !^ **Function Interface**: ShiftL64Up <br>
        !  **Purpose**:  To perform logical left shift by 64 or more (<= 128). <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftL64Up(IN, 111)
        MODULE FUNCTION U128_ShiftLeft64Up(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical left shift by 64 or more.
            tUInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 64 <= ShiftPos <= 128
            tUInt128                :: OutVal
        END FUNCTION U128_ShiftLeft64Up
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftR64Up
        !^ **Function Interface**: ShiftR64Up <br>
        !  **Purpose**:  To perform logical right shift by 64 or more (<= 128). <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftR64Up(IN, 84)
        MODULE FUNCTION U128_ShiftRight64Up(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical right shift by 64 or more.
            tUInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 64 <= ShiftPos <= 128
            tUInt128                :: OutVal
        END FUNCTION U128_ShiftRight64Up
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE SHIFTL
        !^ **Function Interface**: SHIFTL <br>
        !  **Purpose**:  To perform logical left shift with 0 <= ShiftPos <= 128.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = SHIFTL(IN, 127)
        MODULE FUNCTION U128_ShiftLeft(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical left shift of the UInt128 object.
            tUInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 128
            tUInt128                :: OutVal
        END FUNCTION U128_ShiftLeft
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE SHIFTR
        !^ **Function Interface**: SHIFTR <br>
        !  **Purpose**:  To perform logical right shift with 0 <= ShiftPos <= 128.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = SHIFTR(IN, 33)
        MODULE FUNCTION U128_ShiftRight(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical right shift of the UInt128 object.
            tUInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 128
            tUInt128                :: OutVal
        END FUNCTION U128_ShiftRight
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ISHFT
        !^ **Function Interface**: ISHFT <br>
        !  **Purpose**:  To perform logical shift with -128 <= ShiftPos <= 128.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = ISHFT(IN, 53)    ! a logical left shift by 53 <br>
        !   --->    OUT = ISHFT(IN, -24)   ! a logical right shift by 24
        MODULE FUNCTION U128_ShiftLogical(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical (left or right) shift of the UInt128 object.
            tUInt128, INTENT(IN)    :: InVal
            !> Value must be between -128 and 128. <br>
            ! - Positive, the shift is to the left. <br>
            ! - Negative, the shift is to the right.
            tSInt32,  INTENT(IN)    :: ShiftPos
            tUInt128                :: OutVal
        END FUNCTION U128_ShiftLogical
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ISHFTC
        !^ **Function Interface**: ISHFTC <br>
        !  **Purpose**:  To perform circular shift with -128 <= ShiftPos <= 128.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = ISHFTC(IN, 53)    ! a circular left shift by 53 <br>
        !   --->    OUT = ISHFTC(IN, -24)   ! a circular right shift by 24
        MODULE FUNCTION U128_Rotate(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform a circular shift of the rightmost bits.
            tUInt128, INTENT(IN)    :: InVal
            !> Value must be between -128 and 128. <br>
            ! - Positive, the shift is to the left. <br>
            ! - Negative, the shift is to the right.
            tSInt32,  INTENT(IN)    :: ShiftPos
            tUInt128                :: OutVal
        END FUNCTION U128_Rotate
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE NOT
        !^ **Function Interface**: NOT <br>
        !  **Purpose**:  To return the bitwise logical complement of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = NOT(IN)
        MODULE FUNCTION U128_Not(InVal) RESULT(OutVal)
            !^ To return the bitwise logical complement of the UInt128 object.
            tUInt128, INTENT(IN)    :: InVal
            tUInt128                :: OutVal
        END FUNCTION U128_Not
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IOR
        !^ **Function Interface**: IOR <br>
        !  **Purpose**:  To perform an inclusive OR on corresponding bits of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IOR(LHSIN, RHSIN)
        MODULE FUNCTION U128_Ior(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform an inclusive OR on corresponding bits of the UInt128 objects.
            tUInt128, INTENT(IN)    :: LhsVal
            tUInt128, INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U128_Ior
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IEOR
        !^ **Function Interface**: IEOR <br>
        !  **Purpose**:  To perform an exclusive OR on corresponding bits of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IEOR(LHSIN, RHSIN)
        MODULE FUNCTION U128_Ieor(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform an exclusive OR on corresponding bits of the UInt128 objects.
            tUInt128, INTENT(IN)    :: LhsVal
            tUInt128, INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U128_Ieor
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IAND
        !^ **Function Interface**: IAND <br>
        !  **Purpose**:  To perform a logical AND on corresponding bits of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IAND(LHSIN, RHSIN)
        MODULE FUNCTION U128_Iand(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform a logical AND on corresponding bits of the UInt128 objects.
            tUInt128, INTENT(IN)    :: LhsVal
            tUInt128, INTENT(IN)    :: RhsVal
            tUInt128                :: OutVal
        END FUNCTION U128_Iand
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE LEADZ
        !^ **Function Interface**: LEADZ <br>
        !  **Purpose**:  To count the number of leading zero bits of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    NumLZ = LEADZ(INPUT)
        MODULE FUNCTION U128_LeadingZeros(U128) RESULT(NumLZ)
            !^ To count the number of leading zero bits.
            tUInt128, INTENT(IN)    :: U128
            tSInt32                 :: NumLZ
        END FUNCTION U128_LeadingZeros
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE TRAILZ
        !^ **Function Interface**: TRAILZ <br>
        !  **Purpose**:  To count the number of trailing zero bits of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    NumTZ = TRAILZ(INPUT)
        MODULE FUNCTION U128_TrailingZeros(U128) RESULT(NumTZ)
            !^ To count the number of trailing zero bits.
            tUInt128, INTENT(IN)    :: U128
            tSInt32                 :: NumTZ
        END FUNCTION U128_TrailingZeros
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE POPCNT
        !^ **Function Interface**: POPCNT <br>
        !  **Purpose**:  To count the number of 1 bits in the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    NumBits = POPCNT(INPUT)
        MODULE FUNCTION U128_Count1Bits(U128) RESULT(NumBits)
            !^ To count the number of 1 bits in the specified input.
            tUInt128, INTENT(IN)    :: U128
            tSInt32                 :: NumBits
        END FUNCTION U128_Count1Bits
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE POPPAR
        !^ **Function Interface**: POPPAR <br>
        !  **Purpose**:  To determine the parity of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    NumPar = POPPAR(INPUT)
        MODULE FUNCTION U128_Parity(U128) RESULT(ParNum)
            !^ To determine the parity of the specified input.
            tUInt128, INTENT(IN)    :: U128
            tSInt32                 :: ParNum
        END FUNCTION U128_Parity
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IBSET
        !^ **Function Interface**: IBSET <br>
        !  **Purpose**:  To set the bit at the specified position to 1.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IBSET(IN, Pos)
        MODULE FUNCTION U128_SetBit(InVal, Pos) RESULT(OutVal)
            !^ To set the bit at the specified position to 1.
            tUInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: Pos
            tUInt128                :: OutVal
        END FUNCTION U128_SetBit
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IBCLR
        !^ **Function Interface**: IBCLR <br>
        !  **Purpose**:  To set the bit at the specified position to 0.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IBCLR(IN, Pos)
        MODULE FUNCTION U128_ClearBit(InVal, Pos) RESULT(OutVal)
            !^ To set the bit at the specified position to 0.
            tUInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: Pos
            tUInt128                :: OutVal
        END FUNCTION U128_ClearBit
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IBCHNG
        !^ **Function Interface**: IBCHNG <br>
        !  **Purpose**:  To reverse the bit at the specified position.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IBCHNG(IN, Pos)
        MODULE FUNCTION U128_FlipBit(InVal, Pos) RESULT(OutVal)
            !^ To reverse the bit at the specified position.
            tUInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: Pos
            tUInt128                :: OutVal
        END FUNCTION U128_FlipBit
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE BTEST
        !^ **Function Interface**: BTEST <br>
        !  **Purpose**:  To check whether the bit at the specified position is 0 (False) or 1 (True).
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    Flag = BTEST(IN, Pos)
        MODULE FUNCTION U128_TestBit(U128, Pos) RESULT(Flag)
            !^ To check whether the bit at the specified position is 0 (False) or 1 (True).
            tUInt128, INTENT(IN)    :: U128
            tSInt32,  INTENT(IN)    :: Pos
            tLogical                :: Flag
        END FUNCTION U128_TestBit
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IBITS
        !^ **Function Interface**: IBITS <br>
        !  **Purpose**:  To extract a sequence of bits according to the specified input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IBITS(IN, Pos, Len)
        MODULE FUNCTION U128_ExtractBits(InVal, Pos, Len) RESULT(OutVal)
            !^ To extract a sequence of bits according to the specified input.
            tUInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: Pos
            tSInt32,  INTENT(IN)    :: Len
            tUInt128                :: OutVal
        END FUNCTION U128_ExtractBits
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE MoveBits
        !^ **Subroutine Interface**: MoveBits <br>
        !  **Purpose**:  To copy a sequence of bits (a bit field) from one location to another.
        !   (For more information, see detailed explanation of the intrinsic subroutine 'MVBITS'.) <br>
        !  **Usage**: <br>
        !   --->    CALL MoveBits(InVal, InPos, Len, OutVal, OutPos)
        MODULE SUBROUTINE U128_MoveBits(InVal, InPos, Len, OutVal, OutPos)
            !^ To copy a sequence of bits (a bit field) from one location to another.
            tUInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: InPos
            tSInt32,  INTENT(IN)    :: Len
            tUInt128, INTENT(INOUT) :: OutVal
            tSInt32,  INTENT(IN)    :: OutPos
        END SUBROUTINE U128_MoveBits
    END INTERFACE
    !-----------------------------------------------
    !-----        Auxiliary Routines           -----
    !-----------------------------------------------
    INTERFACE MIN
        !^ **Function Interface**: MIN <br>
        !  **Purpose**:  To return the minimum value of the specified input. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = MIN(INP1, INP2) <br>
        !   --->    OUTPUT = MIN(INPARR) <br>
        MODULE PROCEDURE U128_Minimum
        MODULE PROCEDURE U128Array_Minimum
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE MAX
        !^ **Function Interface**: MAX <br>
        !  **Purpose**:  To return the maximum value of the specified input. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = MAX(INP1, INP2) <br>
        !   --->    OUTPUT = MAX(INPARR) <br>
        MODULE PROCEDURE U128_Maximum
        MODULE PROCEDURE U128Array_Maximum
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Display
        !^ **Subroutine Interface**: Display <br>
        !  **Purpose**:  To write/display the 'UInt128' object to the screen (or the specified unit). <br>
        !  **Usage**: <br>
        !   ! To display (unsigned) value of U128 as a decimal string to the screen <br>
        !   --->    CALL Display(U128) <br>
        !   ! To display (unsigned) value of U128 as a decimal string to the output logical unit <br>
        !   --->    CALL Display(U128, 11) <br>
        !   ! To display (unsigned) value of U128 as a decimal string to the output logical unit <br>
        !   ! with input/output status and message <br>
        !   --->    CALL Display(U128, 11, IOStat, IOMsg) <br>
        !   ! To display (signed) values of components of U128 as a decimal string to the screen <br>
        !   --->    CALL Display(U128, ShowComponent=.TRUE.) <br>
        !   ! To display (unsigned) value of U128 as a decimal string to the screen with a prefix string <br>
        !   --->    CALL Display(U128, Prefix='Unsigned value of U128')
        MODULE PROCEDURE U128_Write
    END INTERFACE
    !-----------------------------------------------
    !-----       Experimental Routines         -----
    !-----------------------------------------------
    INTERFACE UAddXp
        MODULE FUNCTION U128_Addition_Xp(LhsVal, RhsVal, Algo) RESULT(OutVal)
            tUInt128, INTENT(IN)   :: LhsVal
            tUInt128, INTENT(IN)   :: RhsVal
            tSInt32,  INTENT(IN)   :: Algo
            tUInt128               :: OutVal
        END FUNCTION U128_Addition_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE USubXp
        MODULE FUNCTION U128_Subtraction_Xp(LhsVal, RhsVal, Algo) RESULT(OutVal)
            tUInt128, INTENT(IN)   :: LhsVal
            tUInt128, INTENT(IN)   :: RhsVal
            tSInt32,  INTENT(IN)   :: Algo
            tUInt128               :: OutVal
        END FUNCTION U128_Subtraction_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE UMulXp
        MODULE FUNCTION U128_Multiplication_Xp(LhsVal, RhsVal, Algo) RESULT(OutVal)
            tUInt128, INTENT(IN)   :: LhsVal
            tUInt128, INTENT(IN)   :: RhsVal
            tSInt32,  INTENT(IN)   :: Algo
            tUInt128               :: OutVal
        END FUNCTION U128_Multiplication_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE UDivModXp
        MODULE SUBROUTINE U128_DivMod_Xp(Dividend, Divisor, Algo, Quotient, Remainder)
            tUInt128, INTENT(IN)   :: Dividend
            tUInt128, INTENT(IN)   :: Divisor
            tSInt32,  INTENT(IN)   :: Algo
            tUInt128, INTENT(OUT)  :: Quotient
            tUInt128, INTENT(OUT)  :: Remainder
        END SUBROUTINE U128_DivMod_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToDecStrXp
        MODULE FUNCTION U128_ToDecString_Xp(U128, Algo) RESULT(Str)
            tUInt128, INTENT(IN)   :: U128
            tSInt32,  INTENT(IN)   :: Algo
            tCharAlloc             :: Str
        END FUNCTION U128_ToDecString_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ParseU128Xp
        MODULE FUNCTION U128_FromDecString_Xp(cStr, Algo, ErrFlag, ErrMsg) RESULT(Number)
            tCharStar,            INTENT(IN)    :: cStr     ! character string
            tSInt32,              INTENT(IN)    :: Algo     ! algorithm
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
            tUInt128                            :: Number   ! number
        END FUNCTION U128_FromDecString_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToU128Xp
        MODULE FUNCTION R32_To_U128_Xp(R32, Algo) RESULT(U128)
            tRealSP, INTENT(IN)    :: R32
            tSInt32,  INTENT(IN)    :: Algo
            tUInt128                :: U128
        END FUNCTION R32_To_U128_Xp
        !------------------------------------------------------------
        MODULE FUNCTION R64_To_U128_Xp(R64, Algo) RESULT(U128)
            tRealDP, INTENT(IN)    :: R64
            tSInt32,  INTENT(IN)    :: Algo
            tUInt128                :: U128
        END FUNCTION R64_To_U128_Xp
        !------------------------------------------------------------
        MODULE FUNCTION R128_To_U128_Xp(R128, Algo) RESULT(U128)
            tRealQP,  INTENT(IN) :: R128
            tSInt32, INTENT(IN) :: Algo
            tUInt128            :: U128
        END FUNCTION R128_To_U128_Xp
        !------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToR32Xp
        MODULE FUNCTION R32_From_U128_Xp(U128, Algo) RESULT(R32)
            tUInt128, INTENT(IN)    :: U128
            tSInt32,  INTENT(IN)    :: Algo
            tRealSP                :: R32
        END FUNCTION R32_From_U128_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToR64Xp
        MODULE FUNCTION R64_From_U128_Xp(U128, Algo) RESULT(R64)
            tUInt128, INTENT(IN)    :: U128
            tSInt32,  INTENT(IN)    :: Algo
            tRealDP                :: R64
        END FUNCTION R64_From_U128_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToR128Xp
        MODULE FUNCTION R128_From_U128_Xp(U128, Algo) RESULT(R128)
            tUInt128, INTENT(IN)    :: U128
            tSInt32,  INTENT(IN)    :: Algo
            tRealQP                  :: R128
        END FUNCTION R128_From_U128_Xp
    END INTERFACE
    !--------------------------------------------------------------------------

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!
!                           AUXILIARY ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION U128_Minimum(A, B) RESULT(MIN)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the minimum value of the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: A, B
    tUInt128                :: MIN

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (A .ULT. B) THEN
        MIN = A
    ELSE
        MIN = B
    END IF

    RETURN

END FUNCTION U128_Minimum

!******************************************************************************

FUNCTION U128Array_Minimum(Arr) RESULT(MIN)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the minimum value of the specified input array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: Arr(:)
    tUInt128                :: MIN

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I

!** FLOW

    MIN = Arr(1)
    DO I = 2_kIndex, SIZE(Arr, KIND=kIndex)
        IF (MIN .ULT. Arr(I)) MIN = Arr(I)
    END DO

    RETURN

END FUNCTION U128Array_Minimum

!******************************************************************************

FUNCTION U128_Maximum(A, B) RESULT(MAX)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the maximum value of the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: A, B
    tUInt128                :: MAX

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (A .UGT. B) THEN
        MAX = A
    ELSE
        MAX = B
    END IF

    RETURN

END FUNCTION U128_Maximum

!******************************************************************************

FUNCTION U128Array_Maximum(Arr) RESULT(MAX)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the maximum value of the specified input array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128, INTENT(IN)    :: Arr(:)
    tUInt128                :: MAX

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I

!** FLOW

    MAX = Arr(1)
    DO I = 2_kIndex, SIZE(Arr, KIND=kIndex)
        IF (MAX .UGT. Arr(I)) MAX = Arr(I)
    END DO

    RETURN

END FUNCTION U128Array_Maximum

!******************************************************************************

SUBROUTINE U128_Write(U128, Unit, IOStat, IOMsg, ShowComponent, Prefix)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write 'UInt128' object to the screen (or the specified unit).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt128,            INTENT(IN)     :: U128
    tSInt32,   OPTIONAL, INTENT(IN)     :: Unit             !! output logical unit
    tSInt32,   OPTIONAL, INTENT(OUT)    :: IOStat           !! io stat
    tCharStar, OPTIONAL, INTENT(OUT)    :: IOMsg            !! io message
    tLogical,  OPTIONAL, INTENT(IN)     :: ShowComponent
    !^ flag indicating whether to write the upper and lower components. <br>
    ! - If flag is present and true, write components of the object. <br>
    ! - Otherwise, write the object as a decimal string.
    tCharStar, OPTIONAL, INTENT(IN)     :: Prefix           !! prefix string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical        :: AsString
    tSInt32         :: OutUnit
    tSInt32         :: IO_Stat
    tCharLen(128)   :: IO_Msg
    tCharAlloc      :: DispStr

!** FLOW

    ! set defaults
    OutUnit  = OUTPUT_UNIT
    AsString = TrueVal

    ! check optional input
    IF (PRESENT(ShowComponent)) THEN
        IF (ShowComponent) AsString = FalseVal
    END IF
    IF (PRESENT(Unit)) OutUnit = Unit

    ! write the object
    IF (AsString) THEN
        IF (PRESENT(Prefix)) THEN
            DispStr = Prefix // ToDecString(U128)
        ELSE
            DispStr = ' U128 = ' // ToDecString(U128)
        END IF
        WRITE(UNIT=OutUnit, FMT='(A)', IOSTAT=IO_Stat, IOMSG=IO_Msg) DispStr
    ELSE
        DispStr = '-: '
        IF (PRESENT(Prefix)) DispStr = Prefix
        WRITE(UNIT=OutUnit, FMT='(A, I0)', IOSTAT=IO_Stat, IOMSG=IO_Msg) &
              DispStr // 'High value = ', U128%High
        WRITE(UNIT=OutUnit, FMT='(A, I0)', IOSTAT=IO_Stat, IOMSG=IO_Msg) &
              DispStr // 'Low value = ', U128%Low
    END IF

    ! return output if requested
    IF (PRESENT(IOStat)) IOStat = IO_Stat
    IF (PRESENT(IOMsg))  IOMsg  = IO_Msg

    RETURN

END SUBROUTINE U128_Write

!******************************************************************************

END MODULE MBase_UInt128

!******************************************************************************
