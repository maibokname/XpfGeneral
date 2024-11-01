
MODULE MBase_SInt128

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SInt128* type, its related routines and useful *SInt128*
!   parameters.  The *SInt128* type is a derived type representing a **128-bit signed**
!   integer.   Various common operations usually available for integer types are provided
!   including arithmetic, bitwise, comparison, and conversion/construction operations.
!   The application programming interface (API) follows Fortran intrinsic integer types
!   with additional functions/methods provided. <br>
!   Similar to the <a href="../module/mbase_uint128.html#type-uint128">UInt128</a>
!   type, the *SInt128* type consists of two 64-bit integer components where the base
!   of its components is 2<sup>64</sup>.  Like the *UInt128* type, the *SInt128* type
!   treats its lower 64-bit component as unsigned integer.  Unlike the *UInt128* type,
!   however, the *SInt128* type treats its upper 64-bit component as a signed one.
!   The *SInt128* type relies on the *UInt128* type for some of its operations (the
!   division/modulation operations in particular) where the same algorithms can be
!   applied for both types.  If an unsigned integer is needed, user can use the
!   <a href="../module/mbase_uint128.html#type-uint128">UInt128</a> type instead.
!   Also, for a signed integer with precision higher than that provided by the *SInt128*
!   type, a user can use either the *ApInt32* type or *ApInt64* type available in the
!   ***XpfHighPrec*** package. <br>
!   <br>
!  **Important Notes**: <br>
!   (1) For arithmetic operations, various types of signed integer types (32-, 64- and
!   128-bit) are allowed.  Like the *UInt128* type, the use of signed and unsigned
!   integers in the same operation is NOT allowed.  Unsigned integer types must be
!   explicitly converted to signed types before using in the arithmetic operations. <br>
!   (2) For comparison and bitwise operations that require two input arguments,
!   both arguments must only be the 128-bit signed integer type.  The operations
!   on mixed types are not provided.  Therefore, all other types must be explicitly
!   converted to the *SInt128* type before using in the comparison and bitwise
!   operations. <br>
!   (3) It is impossible to differentiate between signed and unsigned integers for
!   Fortran intrinsic types.  Therefore, it is a user responsibility to make sure
!   and be extremely careful not to mix up signed and unsigned integers in the same
!   expressions.  It should be noted that, in this module, the unsigned integer types
!   are mostly used in conversion operations.  In all other operations, all Fortran
!   intrinsic integer types are assumed to be signed. <br>
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
    USE MBase_UInt128
    USE MBase_LargeTables,            ONLY: Char1Digit

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type + constructor
    PUBLIC :: SInt128
    ! assignment + conversion
    PUBLIC :: ASSIGNMENT(=)
    PUBLIC :: ToR32, ToR64, ToR128
    PUBLIC :: ToU32, ToU64, ToU128
    PUBLIC :: ToDecString, ToHexString
    ! comparison
    PUBLIC :: OPERATOR(==), OPERATOR(/=)
    PUBLIC :: OPERATOR(<), OPERATOR(<=)
    PUBLIC :: OPERATOR(>), OPERATOR(>=)
    PUBLIC :: Compare
    ! arithmetic
    PUBLIC :: OPERATOR(+), OPERATOR(-)
    PUBLIC :: OPERATOR(*), OPERATOR(/)
    PUBLIC :: Increment, Decrement, Add, Subtract
    PUBLIC :: Multiply, Divide, MOD, DivMod
    ! bitwise
    PUBLIC :: SHIFTL, SHIFTR, SHIFTA, ISHFT, ISHFTC
    PUBLIC :: IOR, IEOR, IAND, NOT, LEADZ, TRAILZ
    PUBLIC :: POPCNT, POPPAR, IBSET, IBCLR
    PUBLIC :: IBCHNG, BTEST, IBITS
    PUBLIC :: MoveBits ! == MVBITS
    ! bitwise (specialized)
    PUBLIC :: ShiftLOnce, ShiftROnce, ShiftAOnce
    PUBLIC :: ShiftL64, ShiftR64, ShiftA64
    PUBLIC :: ShiftL63Down, ShiftR63Down, ShiftA63Down
    PUBLIC :: ShiftL64Up, ShiftR64Up, ShiftA64Up
    ! inquiry
    PUBLIC :: IsPositive, IsNegative, IsZero
    ! auxiliary
    PUBLIC :: ABS, MIN, MAX
    PUBLIC :: Display
    ! experimental
    PUBLIC :: AddXp, SubXp, MulXp, DivModXp
    PUBLIC :: ToDecStrXp, ParseI128Xp
    PUBLIC :: ToI128Xp, ToR32Xp, ToR64Xp, ToR128Xp

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"
#define     tSInt128        TYPE(SInt128)
#define     tUInt128        TYPE(UInt128)

!** MODULE PARAMETERS:
    ! module name
    tCharParam          :: ModName = 'MBase_SInt128'
    ! unsigned limit parameters
    tUInt64,  PARAMETER :: MaxU64 = ToInt64(Z'FFFFFFFFFFFFFFFF')    ! max unsigned 64-bit
    tUInt64,  PARAMETER :: MinU64 = ToInt64(Z'0000000000000000')    ! min unsigned 64-bit
    tUInt64,  PARAMETER :: MaxU32 = ToInt64(Z'00000000FFFFFFFF')    ! max unsigned 32-bit
    tUInt64,  PARAMETER :: MinU32 = ToInt64(Z'0000000000000000')    ! min unsigned 32-bit
    ! signed limit parameters
    tSInt64,  PARAMETER :: MaxI64 = ToInt64(Z'7FFFFFFFFFFFFFFF')    ! max signed 64-bit
    tSInt64,  PARAMETER :: MinI64 = ToInt64(Z'8000000000000000')    ! min signed 64-bit
    tSInt64,  PARAMETER :: MaxI32 = ToInt64(Z'000000007FFFFFFF')    ! max signed 32-bit
    tSInt64,  PARAMETER :: MinI32 = ToInt64(Z'0000000080000000')    ! min signed 32-bit
    ! miscellaneous
    tUInt64,  PARAMETER :: Mask32 = MaxU32
    tUInt64,  PARAMETER :: TopBit = SHIFTL(1_kInt64, 63)
    tLogical, PARAMETER :: Positive = FalseVal

!** DERIVED TYPE DEFINITIONS
    !> *SInt128* is a 128-bit signed integer type where the base
    !   of its components is 2<sup>64</sup>.  
    TYPE SInt128
        !% number representing upper 64 bits treated as signed
        tSInt64 :: High
        !% number representing lower 64 bits treated as unsigned
        tUInt64 :: Low
    END TYPE SInt128

!** MODULE PARAMETERS (PART 2):
    !% 128-bit signed integer parameter with maximum value
    tSInt128, PARAMETER, PUBLIC    :: MaxI128  = SInt128(MaxI64, MaxU64)
    !% 128-bit signed integer parameter with minimum value
    tSInt128, PARAMETER, PUBLIC    :: MinI128  = SInt128(MinI64, MinU64)
    !% 128-bit signed integer parameter with value of one
    tSInt128, PARAMETER, PUBLIC    :: OneI128  = SInt128(0_kInt64, 1_kInt64)
    !% 128-bit signed integer parameter with value of zero
    tSInt128, PARAMETER, PUBLIC    :: ZeroI128 = SInt128(0_kInt64, 0_kInt64)
    tSInt128, PARAMETER            :: TenI128  = SInt128(0_kInt64, 10_kInt64)

!** INTERFACE DEFINITIONS:
    !-----------------------------------------------
    !-----      conversion operations          -----
    !-----------------------------------------------
    INTERFACE ASSIGNMENT(=)
        !^ **Operator Overload**: ASSIGNMENT(=) <br>
        !  **Purpose**:  To convert between a 128-bit signed integer and
        !   other signed integers (32- and 64-bit integers). <br>
        !  **Usage**: <br>
        !   ! convert 32-bit signed integer to 128-bit signed integer <br>
        !   --->    I128 = I32 <br>
        !   ! convert 128-bit signed integer to 64-bit signed integer <br>
        !   --->    I64 = I128
        MODULE SUBROUTINE I128_From_I32(I128, I32)
            !^ To convert a signed 32-bit integer number to a signed 128-bit integer number. <br>
            !  *Usage*: I128 = I32
            tSInt128, INTENT(OUT)   :: I128
            tSInt32,  INTENT(IN)    :: I32      !! number treated as signed
        END SUBROUTINE I128_From_I32
        !----------------------------------------------------------------------
        MODULE SUBROUTINE I128_From_I64(I128, I64)
            !^ To convert a signed 64-bit integer number to a signed 128-bit integer number. <br>
            !  *Usage*: I128 = I64
            tSInt128, INTENT(OUT)   :: I128
            tSInt64,  INTENT(IN)    :: I64      !! number treated as signed
        END SUBROUTINE I128_From_I64
        !----------------------------------------------------------------------
        MODULE SUBROUTINE I128_To_I32(I32, I128)
            !^ To convert a signed 128-bit integer number to a signed 32-bit integer number. <br>
            !  *Usage*: I32 = I128
            tSInt32,  INTENT(OUT)   :: I32      !! number treated as signed
            tSInt128, INTENT(IN)    :: I128
        END SUBROUTINE I128_To_I32
        !----------------------------------------------------------------------
        MODULE SUBROUTINE I128_To_I64(I64, I128)
            !^ To convert a signed 128-bit integer number to a signed 64-bit integer number. <br>
            !  *Usage*: I64 = I128
            tSInt64,  INTENT(OUT)   :: I64      !! number treated as signed
            tSInt128, INTENT(IN)    :: I128
        END SUBROUTINE I128_To_I64
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE SInt128
        !^ **Constructor Interface**: SInt128 <br>
        !  **Purpose**:  To construct a 128-bit signed integer. <br>
        !  **Usage**: <br>
        !   ! construct I128 (with negative value) from 32-bit intrinsic integer treated as unsigned <br>
        !   --->    I128 = SInt128(U32, Negative=.TRUE.) <br>
        !   ! construct I128 from 64-bit intrinsic integer treated as signed <br>
        !   --->    I128 = SInt128(I64) <br>
        !   ! construct I128 from 128-bit real number <br>
        !   --->    I128 = SInt128(R128) <br>
        !   ! construct I128 from a decimal string <br>
        !   --->    I128 = SInt128('-1234567890987654321011223344')
        MODULE FUNCTION I32_To_I128(I32) RESULT(I128)
            !^ To convert a signed 32-bit integer number to a signed 128-bit integer number. <br>
            !  *Usage*: I128 = SInt128(I32)
            tSInt32, INTENT(IN) :: I32      !! number treated as signed
            tSInt128            :: I128
        END FUNCTION I32_To_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_To_I128(I64) RESULT(I128)
            !^ To convert a signed 64-bit integer number to a signed 128-bit integer number. <br>
            !  *Usage*: I128 = SInt128(I64)
            tSInt64, INTENT(IN) :: I64      !! number treated as signed
            tSInt128            :: I128
        END FUNCTION I64_To_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION U32_To_I128(U32, Negative) RESULT(I128)
            !^ To convert an unsigned 32-bit integer number to a signed 128-bit integer number
            !  where the sign flag is used to indicate whether the 128-bit integer value is
            !  positive or negative. <br>
            !  *Usage*:  <br>
            !  ---> I128 = SInt128(U32, .TRUE.) <br>
            !  ---> I128 = SInt128(U32, .FALSE.)
            tUInt32,  INTENT(IN)    :: U32      !! number treated as unsigned
            tLogical, INTENT(IN)    :: Negative
            !^ true if the 128-bit integer value is negative. <br>
            ! otherwise, the 128-bit integer value is positive
            tSInt128                :: I128
        END FUNCTION U32_To_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION U64_To_I128(U64, Negative) RESULT(I128)
            !^ To convert an unsigned 64-bit integer number to a signed 128-bit integer number
            !  where the sign flag is used to indicate whether the 128-bit integer value is
            !  positive or negative. <br>
            !  *Usage*:  <br>
            !  ---> I128 = SInt128(U64, .TRUE.) <br>
            !  ---> I128 = SInt128(U64, .FALSE.)
            tUInt64,  INTENT(IN)    :: U64      !! number treated as unsigned
            tLogical, INTENT(IN)    :: Negative
            !^ True if the 128-bit integer value is negative. <br>
            !  Otherwise, the 128-bit integer value is positive.
            tSInt128                :: I128
        END FUNCTION U64_To_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION R32_To_I128(R32) RESULT(I128)
            !^ To convert a 32-bit floating point number to a signed 128-bit integer number. <br>
            !  *Usage*: I128 = SInt128(R32)
            tRealSP, INTENT(IN) :: R32
            tSInt128            :: I128
        END FUNCTION R32_To_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION R64_To_I128(R64) RESULT(I128)
            !^ To convert a 64-bit floating point number to a signed 128-bit integer number. <br>
            !  *Usage*: I128 = SInt128(R64)
            tRealDP, INTENT(IN) :: R64
            tSInt128            :: I128
        END FUNCTION R64_To_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION R128_To_I128(R128) RESULT(I128)
            !^ To convert a 128-bit floating point number to an unsigned 128-bit integer number. <br>
            !  *Usage*: I128 = SInt128(R128)
            tRealQP, INTENT(IN) :: R128
            tSInt128            :: I128
        END FUNCTION R128_To_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION DecString_To_I128(cStr, ErrFlag, ErrMsg) RESULT(Number)
            !^ To convert a decimal string to a signed 128-bit integer value. <br>
            !  *Usage*:  <br>
            !  ---> I128 = SInt128('1234567890987654321011223344') <br>
            !  ---> I128 = SInt128('-987654321012345678900123123', ErrFlag) <br>
            !  ---> I128 = SInt128(NumStr, ErrMsg=Message) <br>
            !  ---> I128 = SInt128(NumStr, ErrFlag, ErrMsg)
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            tSInt128                            :: Number   !! number
        END FUNCTION DecString_To_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToU32
        !^ **Function Interface**: ToU32 <br>
        !  **Purpose**:  To convert a 128-bit signed integer to a
        !   32-bit unsigned integer. <br>
        !  **Usage**: <br>
        !   --->    U32 = ToU32(I128)
        MODULE FUNCTION U32_From_I128(I128) RESULT(U32)
            !^ To convert a signed 128-bit integer number to an unsigned 32-bit integer number.
            tSInt128, INTENT(IN)    :: I128
            tUInt32                 :: U32      !! number treated as unsigned
        END FUNCTION U32_From_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToU64
        !^ **Function Interface**: ToU64 <br>
        !  **Purpose**:  To convert a 128-bit signed integer to a
        !   64-bit unsigned integer. <br>
        !  **Usage**: <br>
        !   --->    U64 = ToU64(I128)
        MODULE FUNCTION U64_From_I128(I128) RESULT(U64)
            !^ To convert a signed 128-bit integer number to an unsigned 64-bit integer number.
            tSInt128, INTENT(IN)    :: I128
            tUInt64                 :: U64      !! number treated as unsigned
        END FUNCTION U64_From_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToU128
        !^ **Function Interface**: ToU128 <br>
        !  **Purpose**:  To convert a 128-bit signed integer to a
        !   128-bit unsigned integer. <br>
        !  **Usage**: <br>
        !   --->    U128 = ToU128(I128)
        MODULE FUNCTION U128_From_I128(I128) RESULT(U128)
            !^ To convert a signed 128-bit integer to an unsigned 128-bit integer.
            tSInt128, INTENT(IN)    :: I128
            TYPE(UInt128)           :: U128
        END FUNCTION U128_From_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToR32
        !^ **Function Interface**: ToR32 <br>
        !  **Purpose**:  To convert a 128-bit signed integer to
        !   a 32-bit floating point (real) number. <br>
        !  **Usage**: <br>
        !   --->    R32 = ToR32(I128)
        MODULE FUNCTION R32_From_I128(I128) RESULT(R32)
            !^ To convert a signed 128-bit integer number to a 32-bit floating point number.
            tSInt128, INTENT(IN)    :: I128
            tRealSP                 :: R32
        END FUNCTION R32_From_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToR64
        !^ **Function Interface**: ToR64 <br>
        !  **Purpose**:  To convert a 128-bit signed integer to
        !   a 64-bit floating point (real) number. <br>
        !  **Usage**: <br>
        !   --->    R64 = ToR64(I128)
        MODULE FUNCTION R64_From_I128(I128) RESULT(R64)
            !^ To convert a signed 128-bit integer number to a 64-bit floating point number.
            tSInt128, INTENT(IN)    :: I128
            tRealDP                 :: R64
        END FUNCTION R64_From_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToR128
        !^ **Function Interface**: ToR128 <br>
        !  **Purpose**:  To convert a 128-bit signed integer to
        !   a 128-bit floating point (real) number. <br>
        !  **Usage**: <br>
        !   --->    R128 = ToR128(I128)
        MODULE FUNCTION R128_From_I128(I128) RESULT(R128)
            !^ To convert a signed 128-bit integer number to a 128-bit floating point number.
            tSInt128, INTENT(IN)    :: I128
            tRealQP                   :: R128
        END FUNCTION R128_From_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToDecString
        !^ **Function Interface**: ToDecString <br>
        !  **Purpose**:  To convert a 128-bit signed integer to
        !   a decimal string. <br>
        !  **Usage**: <br>
        !   --->    Str = ToDecString(I128)
        MODULE FUNCTION DecString_From_I128(I128) RESULT(Str)
            !^ To convert a signed 128-bit integer number to a decimal string.
            tSInt128, INTENT(IN)    :: I128
            tCharAlloc              :: Str
        END FUNCTION DecString_From_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToHexString
        !^ **Function Interface**: ToHexString <br>
        !  **Purpose**:  To convert a 128-bit signed integer to
        !   a hexadecimal string. <br>
        !  **Usage**: <br>
        !   --->    Str = ToHexString(I128)
        MODULE FUNCTION HexString_From_I128(I128) RESULT(Str)
            !^ To convert a signed 128-bit integer number to a hexadecimal string.
            tSInt128, INTENT(IN)    :: I128
            tCharAlloc              :: Str
        END FUNCTION HexString_From_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !-----------------------------------------------
    !-----      comparison operations          -----
    !-----------------------------------------------
    INTERFACE OPERATOR(==)
        !^ **Operator Overload**: OPERATOR(==) <br>
        !  **Purpose**:  To check if values of two 128-bit signed integers are equal.
        !   Return .TRUE. if both values are equal; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS == RHS <br>
        !   --->    IF (LHS .EQ. RHS) DoSomething
        MODULE FUNCTION I128_Equal(LHS, RHS) RESULT(Flag)
            !^ To check whether two SInt128 objects are equal.
            tSInt128, INTENT(IN)    :: LHS
            tSInt128, INTENT(IN)    :: RHS
            tLogical                :: Flag
        END FUNCTION I128_Equal
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(/=)
        !^ **Operator Overload**: OPERATOR(/=) <br>
        !  **Purpose**:  To check if values of two 128-bit signed integers are not equal.
        !   Return .TRUE. if both values are NOT equal; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS /= RHS <br>
        !   --->    IF (LHS .NE. RHS) DoSomething
        MODULE FUNCTION I128_NotEqual(LHS, RHS) RESULT(Flag)
            !^ To check whether two SInt128 objects are NOT equal.
            tSInt128, INTENT(IN)    :: LHS
            tSInt128, INTENT(IN)    :: RHS
            tLogical                :: Flag
        END FUNCTION I128_NotEqual
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(<)
        !^ **Operator Overload**: OPERATOR(<) <br>
        !  **Purpose**:  To check if the LHS value is less than the RHS value.
        !   Return .TRUE. if LHS < RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS < RHS <br>
        !   --->    IF (LHS .LT. RHS) DoSomething
        MODULE FUNCTION I128_LessThan(LHS, RHS) RESULT(Flag)
            !^ To check whether the LHS SInt128 object is less than the RHS SInt128 object.
            tSInt128, INTENT(IN)    :: LHS
            tSInt128, INTENT(IN)    :: RHS
            tLogical                :: Flag
        END FUNCTION I128_LessThan
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(<=)
        !^ **Operator Overload**: OPERATOR(<=) <br>
        !  **Purpose**:  To check if the LHS value is less than or equal to the RHS value.
        !   Return .TRUE. if LHS <= RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS <= RHS <br>
        !   --->    IF (LHS .LE. RHS) DoSomething
        MODULE FUNCTION I128_LessEqual(LHS, RHS) RESULT(Flag)
            !^ To check whether the LHS SInt128 object is less than or equal to the RHS SInt128 object.
            tSInt128, INTENT(IN)    :: LHS
            tSInt128, INTENT(IN)    :: RHS
            tLogical                :: Flag
        END FUNCTION I128_LessEqual
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(>)
        !^ **Operator Overload**: OPERATOR(>) <br>
        !  **Purpose**:  To check if the LHS value is greater than the RHS value.
        !   Return .TRUE. if LHS > RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS > RHS <br>
        !   --->    IF (LHS .GT. RHS) DoSomething
        MODULE FUNCTION I128_GreaterThan(LHS, RHS) RESULT(Flag)
            !^ To check whether the LHS SInt128 object is greater than the RHS SInt128 object.
            tSInt128, INTENT(IN)    :: LHS
            tSInt128, INTENT(IN)    :: RHS
            tLogical                :: Flag
        END FUNCTION I128_GreaterThan
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(>=)
        !^ **Operator Overload**: OPERATOR(>=) <br>
        !  **Purpose**:  To check if the LHS value is greater than or equal to the RHS value.
        !   Return .TRUE. if LHS >= RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS >= RHS <br>
        !   --->    IF (LHS .GE. RHS) DoSomething
        MODULE FUNCTION I128_GreaterEqual(LHS, RHS) RESULT(Flag)
            !^ To check whether the LHS SInt128 object is greater than or equal to the RHS SInt128 object.
            tSInt128, INTENT(IN)    :: LHS
            tSInt128, INTENT(IN)    :: RHS
            tLogical                :: Flag
        END FUNCTION I128_GreaterEqual
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Compare
        !^ **Function Interface**: Compare <br>
        !  **Purpose**:  To compare two 128-bit signed integers and return <br>
        !   -1 if LHS < RHS, <br>
        !    0 if LHS == RHS, or <br>
        !    1 if LHS > RHS. <br>
        !  **Usage**: <br>
        !   --->    Flag = Compare(LHS, RHS) <br>
        !   --->    IF (Compare(LHS, RHS) /= 0) DoSomething
        MODULE FUNCTION I128_Compare(LHS, RHS) RESULT(Flag)
            !^ To compare LHS and RHS objects. <br>
            ! - Return -1 if LHS < RHS. <br>
            ! - Return  0 if LHS == RHS. <br>
            ! - Return +1 if LHS > RHS.
            tSInt128, INTENT(IN)    :: LHS
            tSInt128, INTENT(IN)    :: RHS
            tSInt32                 :: Flag
        END FUNCTION I128_Compare
        !----------------------------------------------------------------------
    END INTERFACE
    !-----------------------------------------------
    !-----          arithmetic operations      -----
    !-----------------------------------------------
    INTERFACE OPERATOR(+)
        !^ **Operator Overload**: OPERATOR(+) <br>
        !  **Purpose**:  To perform a summation of two signed integers
        !   (at least one of which is a 128-bit signed integer) or
        !   to add a unary plus sign to a 128-bit signed integer
        !   (which has no effect on the signed integer). <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = +INPUT <br>
        !   --->    OUTPUT = FIRST_IN + SECOND_IN
        MODULE FUNCTION I128_UnaryPlus(InVal) RESULT(OutVal)
            !^ To return result of the unary plus sign of the Sint128 object.
            tSInt128, INTENT(IN)    :: InVal
            tSInt128                :: OutVal
        END FUNCTION I128_UnaryPlus
        !----------------------------------------------------------------------
        MODULE FUNCTION I128_Plus_I32(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform addition:  OutVal = LhsVal + RhsVal.
            tSInt128, INTENT(IN)    :: LhsVal
            tSInt32,  INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I128_Plus_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_Plus_I128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform addition:  OutVal = LhsVal + RhsVal.
            tSInt32,  INTENT(IN)    :: LhsVal
            tSInt128, INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I32_Plus_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION I128_Plus_I64(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform addition:  OutVal = LhsVal + RhsVal.
            tSInt128, INTENT(IN)    :: LhsVal
            tSInt64,  INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I128_Plus_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_Plus_I128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform addition:  OutVal = LhsVal + RhsVal.
            tSInt64,  INTENT(IN)    :: LhsVal
            tSInt128, INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I64_Plus_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION I128_Plus_I128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform addition of two SInt128 objects (Lhs + Rhs).
            tSInt128, INTENT(IN)    :: LhsVal
            tSInt128, INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I128_Plus_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(-)
        !^ **Operator Overload**: OPERATOR(-) <br>
        !  **Purpose**:  To perform a subtraction of two signed integers
        !   (at least one of which is a 128-bit signed integer) or
        !   to perform a negation of a 128-bit signed integer. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = -INPUT <br>
        !   --->    OUTPUT = FIRST_IN - SECOND_IN <br>
        !  **Important Note**:  For subtraction of signed integers (unlike unsigned one),
        !   value of FIRST_IN can be less than SECOND_IN.
        MODULE FUNCTION I128_Negate(InVal) RESULT(OutVal)
            !^ To negate the Uint128 object.
            tSInt128, INTENT(IN)    :: InVal
            tSInt128                :: OutVal
        END FUNCTION I128_Negate
        !----------------------------------------------------------------------
        MODULE FUNCTION I128_Minus_I32(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform subtraction:  OutVal = LhsVal - RhsVal.
            tSInt128, INTENT(IN)    :: LhsVal
            tSInt32,  INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I128_Minus_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION I32_Minus_I128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform subtraction:  OutVal = LhsVal - RhsVal.
            tSInt32,  INTENT(IN)    :: LhsVal
            tSInt128, INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I32_Minus_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION I128_Minus_I64(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform subtraction:  OutVal = LhsVal - RhsVal.
            tSInt128, INTENT(IN)    :: LhsVal
            tSInt64,  INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I128_Minus_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_Minus_I128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform subtraction:  OutVal = LhsVal - RhsVal.
            tSInt64,  INTENT(IN)    :: LhsVal
            tSInt128, INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I64_Minus_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION I128_Minus_I128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform subtraction of two SInt128 objects (Lhs - Rhs).
            tSInt128, INTENT(IN)    :: LhsVal
            tSInt128, INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I128_Minus_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(*)
        !^ **Operator Overload**: OPERATOR( * ) <br>
        !  **Purpose**:  To perform a multiplication of two signed integers
        !   (at least one of which is a 128-bit signed integer). <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = FIRST_IN * SECOND_IN
        MODULE FUNCTION I32_Multiply_I128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform multiplication:  OutVal = LhsVal * RhsVal.
            tSInt32,  INTENT(IN)    :: LhsVal
            tSInt128, INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I32_Multiply_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION I128_Multiply_I32(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform multiplication:  OutVal = LhsVal * RhsVal.
            tSInt128, INTENT(IN)    :: LhsVal
            tSInt32,  INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I128_Multiply_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION I64_Multiply_I128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform multiplication:  OutVal = LhsVal * RhsVal.
            tSInt64,  INTENT(IN)    :: LhsVal
            tSInt128, INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I64_Multiply_I128
        !----------------------------------------------------------------------
        MODULE FUNCTION I128_Multiply_I64(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform multiplication:  OutVal = LhsVal * RhsVal.
            tSInt128, INTENT(IN)    :: LhsVal
            tSInt64,  INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I128_Multiply_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION I128_Multiply_I128(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform multiplication of two SInt128 objects (Lhs * Rhs).
            tSInt128, INTENT(IN)    :: LhsVal
            tSInt128, INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I128_Multiply_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE OPERATOR(/)
        !^ **Operator Overload**: OPERATOR(/) <br>
        !  **Purpose**:  To return the quotient of a division of two signed integers,
        !   where the dividend (numerator) is a 128-bit signed integer and the
        !   divisor (denominator) can be 32-, 64- or 128-bit signed integer. <br>
        !  **Usage**: <br>
        !   --->    QUOT = NUMER / DENOM
        MODULE FUNCTION I128_Divide_I32(Dividend, Divisor) RESULT(Quotient)
            !^ To perform division:  Quotient = Dividend / Divisor.
            tSInt128, INTENT(IN)    :: Dividend
            tSInt32,  INTENT(IN)    :: Divisor
            tSInt128                :: Quotient
        END FUNCTION I128_Divide_I32
        !----------------------------------------------------------------------
        MODULE FUNCTION I128_Divide_I64(Dividend, Divisor) RESULT(Quotient)
            !^ To perform division:  Quotient = Dividend / Divisor.
            tSInt128, INTENT(IN)    :: Dividend
            tSInt64,  INTENT(IN)    :: Divisor
            tSInt128                :: Quotient
        END FUNCTION I128_Divide_I64
        !----------------------------------------------------------------------
        MODULE FUNCTION I128_Divide_I128(Dividend, Divisor) RESULT(Quotient)
            !^ To perform division of two SInt128 objects (Dividend / Divisor)
            !  and return the quotient.
            tSInt128, INTENT(IN)    :: Dividend
            tSInt128, INTENT(IN)    :: Divisor
            tSInt128                :: Quotient
        END FUNCTION I128_Divide_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE MOD
        !^ **Function Interface**: MOD <br>
        !  **Purpose**:  To return the remainder of a division of two signed integers,
        !   where the dividend (numerator) is a 128-bit signed integer and the
        !   divisor (denominator) can be 32-, 64- or 128-bit signed integer. <br>
        !  **Usage**: <br>
        !   --->    REM = MOD(NUMER, DENOM)
        MODULE FUNCTION I128_Mod_I32(Dividend, Divisor) RESULT(Remainder)
            !^ To perform modulation:  Remainder = Dividend MOD Divisor.
            tSInt128, INTENT(IN)    :: Dividend
            tSInt32,  INTENT(IN)    :: Divisor
            tSInt128                :: Remainder
        END FUNCTION I128_Mod_I32
        !******************************************************************************
        MODULE FUNCTION I128_Mod_I64(Dividend, Divisor) RESULT(Remainder)
            !^ To perform modulation:  Remainder = Dividend MOD Divisor.
            tSInt128, INTENT(IN)    :: Dividend
            tSInt64,  INTENT(IN)    :: Divisor
            tSInt128                :: Remainder
        END FUNCTION I128_Mod_I64
        !******************************************************************************
        MODULE FUNCTION I128_Mod_I128(Dividend, Divisor) RESULT(Remainder)
            !^ To perform division of two SInt128 objects (Dividend / Divisor)
            !  and return the remainder.
            tSInt128, INTENT(IN)    :: Dividend
            tSInt128, INTENT(IN)    :: Divisor
            tSInt128                :: Remainder
        END FUNCTION I128_Mod_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE DivMod
        !^ **Subroutine Interface**: DivMod <br>
        !  **Purpose**:  To perform a division of two signed integers (where the
        !   dividend (numerator) is a 128-bit signed integer and the divisor
        !   (denominator) can be 32-, 64- or 128-bit signed integer) and
        !   to return both the quotient and the remainder. <br>
        !  **Usage**: <br>
        !   --->    CALL DivMod(NUMER, DENOM, QUOT, REM)
        MODULE SUBROUTINE I128_DivMod_I32(Dividend, Divisor, Quotient, Remainder)
            !^ To perform division:  Quotient = Dividend / Divisor.  <br>
            !  Return both quotient and remainder.
            tSInt128, INTENT(IN)    :: Dividend
            tSInt32,  INTENT(IN)    :: Divisor
            tSInt128, INTENT(OUT)   :: Quotient
            tSInt128, INTENT(OUT)   :: Remainder
        END SUBROUTINE I128_DivMod_I32
        !----------------------------------------------------------------------
        MODULE SUBROUTINE I128_DivMod_I64(Dividend, Divisor, Quotient, Remainder)
            !^ To perform division:  Quotient = Dividend / Divisor.  <br>
            !  Return both quotient and remainder.
            tSInt128, INTENT(IN)    :: Dividend
            tSInt64,  INTENT(IN)    :: Divisor
            tSInt128, INTENT(OUT)   :: Quotient
            tSInt128, INTENT(OUT)   :: Remainder
        END SUBROUTINE I128_DivMod_I64
        !----------------------------------------------------------------------
        MODULE SUBROUTINE I128_DivMod_I128(Dividend, Divisor, Quotient, Remainder)
            !^ To perform division:  Quotient = Dividend / Divisor.  <br>
            !  Return both quotient and remainder.
            tSInt128, INTENT(IN)    :: Dividend
            tSInt128, INTENT(IN)    :: Divisor
            tSInt128, INTENT(OUT)   :: Quotient
            tSInt128, INTENT(OUT)   :: Remainder
        END SUBROUTINE I128_DivMod_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Increment
        !^ **Subroutine Interface**: Increment <br>
        !  **Purpose**:  To increase value of a 128-bit signed integer by one. <br>
        !  **Usage**: <br>
        !   --->    CALL Increment(I128)
        MODULE SUBROUTINE I128_Increment(Val)
            !^ To increase value of the input by 1.
            tSInt128, INTENT(INOUT) :: Val
        END SUBROUTINE I128_Increment
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Decrement
        !^ **Subroutine Interface**: Decrement <br>
        !  **Purpose**:  To decrease value of a 128-bit signed integer by one. <br>
        !  **Usage**: <br>
        !   --->    CALL Decrement(I128)
        MODULE SUBROUTINE I128_Decrement(Val)
            !^ To decrease value of the input by 1.
            tSInt128, INTENT(INOUT) :: Val
        END SUBROUTINE I128_Decrement
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Add
        !^ **Subroutine Interface**: Add <br>
        !  **Purpose**:  To perform addition: This = This + Other. <br>
        !  **Usage**: <br>
        !   --->    CALL Add(This, Other)
        MODULE SUBROUTINE I128_Add_I32(This, Other)
            !^ To perform addition:  This = This + Other.
            tSInt128, INTENT(INOUT) :: This
            tSInt32,  INTENT(IN)    :: Other
        END SUBROUTINE I128_Add_I32
        !----------------------------------------------------------------------
        MODULE SUBROUTINE I128_Add_I64(This, Other)
            !^ To perform addition:  This = This + Other.
            tSInt128, INTENT(INOUT) :: This
            tSInt64,  INTENT(IN)    :: Other
        END SUBROUTINE I128_Add_I64
        !----------------------------------------------------------------------
        MODULE SUBROUTINE I128_Add_I128(This, Other)
            !^ To perform addition:  This = This + Other.
            tSInt128, INTENT(INOUT) :: This
            tSInt128, INTENT(IN)    :: Other
        END SUBROUTINE I128_Add_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Subtract
        !^ **Subroutine Interface**: Subtract <br>
        !  **Purpose**:  To perform subtraction: This = This - Other. <br>
        !  **Usage**: <br>
        !   --->    CALL Subtract(This, Other) <br>
        !  **Important Note**:  For subtraction of signed integers (unlike unsigned one),
        !   value of This can be less than Other.
        MODULE SUBROUTINE I128_Subtract_I32(This, Other)
            !^ To perform subtraction:  This = This - Other.
            tSInt128, INTENT(INOUT) :: This
            tSInt32,  INTENT(IN)    :: Other
        END SUBROUTINE I128_Subtract_I32
        !----------------------------------------------------------------------
        MODULE SUBROUTINE I128_Subtract_I64(This, Other)
            !^ To perform subtraction:  This = This - Other.
            tSInt128, INTENT(INOUT) :: This
            tSInt64,  INTENT(IN)    :: Other
        END SUBROUTINE I128_Subtract_I64
        !----------------------------------------------------------------------
        MODULE SUBROUTINE I128_Subtract_I128(This, Other)
            !^ To perform subtraction:  This = This - Other.
            tSInt128, INTENT(INOUT) :: This
            tSInt128, INTENT(IN)    :: Other
        END SUBROUTINE I128_Subtract_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Multiply
        !^ **Subroutine Interface**: Multiply <br>
        !  **Purpose**:  To perform multiplication: This = This * Other. <br>
        !  **Usage**: <br>
        !   --->    CALL Multiply(This, Other)
        MODULE SUBROUTINE I128_Times_I32(This, Other)
            !^ To perform multiplication:  This = This * Other.
            tSInt128, INTENT(INOUT) :: This
            tSInt32,  INTENT(IN)    :: Other
        END SUBROUTINE I128_Times_I32
        !----------------------------------------------------------------------
        MODULE SUBROUTINE I128_Times_I64(This, Other)
            !^ To perform multiplication:  This = This * Other.
            tSInt128, INTENT(INOUT) :: This
            tSInt64,  INTENT(IN)    :: Other
        END SUBROUTINE I128_Times_I64
        !----------------------------------------------------------------------
        MODULE SUBROUTINE I128_Times_I128(This, Other)
            !^ To perform multiplication:  This = This * Other.
            tSInt128, INTENT(INOUT) :: This
            tSInt128, INTENT(IN)    :: Other
        END SUBROUTINE I128_Times_I128
        !----------------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Divide
        !^ **Subroutine Interface**: Divide <br>
        !  **Purpose**:  To perform a division: This = This / Other. <br>
        !  **Usage**: <br>
        !   --->    CALL Divide(This, Other)
        MODULE SUBROUTINE I128_Over_I32(This, Other, Remainder)
            !^ To perform division:  This = This / Other.
            tSInt128,          INTENT(INOUT)    :: This
            tSInt32,           INTENT(IN)       :: Other
            tSInt32, OPTIONAL, INTENT(OUT)      :: Remainder
        END SUBROUTINE I128_Over_I32
        !******************************************************************************
        MODULE SUBROUTINE I128_Over_I64(This, Other, Remainder)
            !^ To perform division:  This = This / Other.
            tSInt128,          INTENT(INOUT)    :: This
            tSInt64,           INTENT(IN)       :: Other
            tSInt64, OPTIONAL, INTENT(OUT)      :: Remainder
        END SUBROUTINE I128_Over_I64
        !******************************************************************************
        MODULE SUBROUTINE I128_Over_I128(This, Other, Remainder)
            !^ To perform division:  This = This / Other.
            tSInt128,           INTENT(INOUT)   :: This
            tSInt128,           INTENT(IN)      :: Other
            tSInt128, OPTIONAL, INTENT(OUT)     :: Remainder
        END SUBROUTINE I128_Over_I128
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
        MODULE FUNCTION I128_ShiftL_Once(InVal) RESULT(OutVal)
            !^ To perform logical left shift by 1.
            tSInt128, INTENT(IN)    :: InVal
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftL_Once
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftROnce
        !^ **Function Interface**: ShiftROnce <br>
        !  **Purpose**:  To perform logical right shift by 1. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftROnce(IN)
        MODULE FUNCTION I128_ShiftR_Once(InVal) RESULT(OutVal)
            !^ To perform logical right shift by 1.
            tSInt128, INTENT(IN)    :: InVal
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftR_Once
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftAOnce
        !^ **Function Interface**: ShiftAOnce <br>
        !  **Purpose**:  To perform arithmetic right shift by 1. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftAOnce(IN)
        MODULE FUNCTION I128_ShiftA_Once(InVal) RESULT(OutVal)
            !^ To perform arithmetic right shift by 1.
            tSInt128, INTENT(IN)    :: InVal
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftA_Once
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftL64
        !^ **Function Interface**: ShiftL64 <br>
        !  **Purpose**:  To perform logical left shift by 64. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftL64(IN)
        MODULE FUNCTION I128_ShiftL_64(InVal) RESULT(OutVal)
            !^ To perform logical left shift by 64.
            tSInt128, INTENT(IN)    :: InVal
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftL_64
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftR64
        !^ **Function Interface**: ShiftR64 <br>
        !  **Purpose**:  To perform logical right shift by 64. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftR64(IN)
        MODULE FUNCTION I128_ShiftR_64(InVal) RESULT(OutVal)
            !^ To perform logical right shift by 64.
            tSInt128, INTENT(IN)    :: InVal
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftR_64
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftA64
        !^ **Function Interface**: ShiftA64 <br>
        !  **Purpose**:  To perform arithmetic right shift by 64. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftA64(IN)
        MODULE FUNCTION I128_ShiftA_64(InVal) RESULT(OutVal)
            !^ To perform arithmetic right shift by 64.
            tSInt128, INTENT(IN)    :: InVal
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftA_64
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftL63Down
        !^ **Function Interface**: ShiftL63Down <br>
        !  **Purpose**:  To perform logical left shift by 63 or less. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftL63Down(IN, 11)
        MODULE FUNCTION I128_ShiftL_63Down(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical left shift by 63 or less.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 63
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftL_63Down
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftR63Down
        !^ **Function Interface**: ShiftR63Down <br>
        !  **Purpose**:  To perform logical right shift by 63 or less. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftR63Down(IN, 53)
        MODULE FUNCTION I128_ShiftR_63Down(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical right shift by 63 or less.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 63
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftR_63Down
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftA63Down
        !^ **Function Interface**: ShiftA63Down <br>
        !  **Purpose**:  To perform arithmetic right shift by 63 or less. <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftA63Down(IN, 53)
        MODULE FUNCTION I128_ShiftA_63Down(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform arithmetic right shift by 63 or less.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 63
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftA_63Down
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftL64Up
        !^ **Function Interface**: ShiftL64Up <br>
        !  **Purpose**:  To perform logical left shift by 64 or more (<= 128). <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftL64Up(IN, 111)
        MODULE FUNCTION I128_ShiftL_64Up(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical left shift by 64 or more.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 64 <= ShiftPos <= 128
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftL_64Up
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftR64Up
        !^ **Function Interface**: ShiftR64Up <br>
        !  **Purpose**:  To perform logical right shift by 64 or more (<= 128). <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftR64Up(IN, 84)
        MODULE FUNCTION I128_ShiftR_64Up(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical right shift by 64 or more.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 64 <= ShiftPos <= 128
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftR_64Up
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ShiftA64Up
        !^ **Function Interface**: ShiftA64Up <br>
        !  **Purpose**:  To perform arithmetic right shift by 64 or more (<= 128) <br>
        !  **Usage**: <br>
        !   --->    OUT = ShiftA64Up(IN, 84)
        MODULE FUNCTION I128_ShiftA_64Up(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform arithmetic right shift by 64 or more.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 64 <= ShiftPos <= 128
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftA_64Up
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE SHIFTL
        !^ **Function Interface**: SHIFTL <br>
        !  **Purpose**:  To perform logical left shift with 0 <= ShiftPos <= 128.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = SHIFTL(IN, 127)
        MODULE FUNCTION I128_ShiftLeft(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical/arithmetic left shift of the SInt128 object.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 128
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftLeft
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE SHIFTR
        !^ **Function Interface**: SHIFTR <br>
        !  **Purpose**:  To perform logical right shift with 0 <= ShiftPos <= 128.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = SHIFTR(IN, 33)
        MODULE FUNCTION I128_ShiftRightLogical(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical right shift of the SInt128 object.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 128
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftRightLogical
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE SHIFTA
        !^ **Function Interface**: SHIFTA <br>
        !  **Purpose**:  To perform arithmetic right shift with 0 <= ShiftPos <= 128.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = SHIFTA(IN, 33)
        MODULE FUNCTION I128_ShiftRightArithmetic(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform arithmetic right shift of the SInt128 object.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos !! 0 <= ShiftPos <= 128
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftRightArithmetic
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ISHFT
        !^ **Function Interface**: ISHFT <br>
        !  **Purpose**:  To perform logical shift with -128 <= ShiftPos <= 128.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = ISHFT(IN, 53)    ! a logical left shift by 53 <br>
        !   --->    OUT = ISHFT(IN, -24)   ! a logical right shift by 24
        MODULE FUNCTION I128_ShiftLogical(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical (left or right) shift of the SInt128 object.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos
            ! -128 <= ShiftPos <= 128 <br>
            ! Positive, the shift is to the left. <br>
            ! Negative, the shift is to the right.
            tSInt128                :: OutVal
        END FUNCTION I128_ShiftLogical
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ISHFTC
        !^ **Function Interface**: ISHFTC <br>
        !  **Purpose**:  To perform circular shift with -128 <= ShiftPos <= 128.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = ISHFTC(IN, 53)    ! a circular left shift by 53 <br>
        !   --->    OUT = ISHFTC(IN, -24)   ! a circular right shift by 24
        MODULE FUNCTION I128_Rotate(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform a circular shift of the rightmost bits.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: ShiftPos
            ! -128 <= ShiftPos <= 128 <br>
            ! Positive, the shift is to the left. <br>
            ! Negative, the shift is to the right.
            tSInt128               :: OutVal
        END FUNCTION I128_Rotate
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE NOT
        !^ **Function Interface**: NOT <br>
        !  **Purpose**:  To return the bitwise logical complement of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = NOT(IN)
        MODULE FUNCTION I128_Not(InVal) RESULT(OutVal)
            !^ To return the bitwise logical complement of the SInt128 object.
            tSInt128, INTENT(IN)    :: InVal
            tSInt128                :: OutVal
        END FUNCTION I128_Not
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IOR
        !^ **Function Interface**: IOR <br>
        !  **Purpose**:  To perform an inclusive OR on corresponding bits of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IOR(LHSIN, RHSIN)
        MODULE FUNCTION I128_Ior(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform an inclusive OR on corresponding bits of the SInt128 objects.
            tSInt128, INTENT(IN)    :: LhsVal
            tSInt128, INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I128_Ior
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IEOR
        !^ **Function Interface**: IEOR <br>
        !  **Purpose**:  To perform an exclusive OR on corresponding bits of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IEOR(LHSIN, RHSIN)
        MODULE FUNCTION I128_Ieor(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform an exclusive OR on corresponding bits of the SInt128 objects.
            tSInt128, INTENT(IN)    :: LhsVal
            tSInt128, INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I128_Ieor
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IAND
        !^ **Function Interface**: IAND <br>
        !  **Purpose**:  To perform a logical AND on corresponding bits of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IAND(LHSIN, RHSIN)
        MODULE FUNCTION I128_Iand(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform a logical AND on corresponding bits of the SInt128 objects.
            tSInt128, INTENT(IN)    :: LhsVal
            tSInt128, INTENT(IN)    :: RhsVal
            tSInt128                :: OutVal
        END FUNCTION I128_Iand
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE LEADZ
        !^ **Function Interface**: LEADZ <br>
        !  **Purpose**:  To count the number of leading zero bits of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    NumLZ = LEADZ(INPUT)
        MODULE FUNCTION I128_LeadingZeros(I128) RESULT(NumLZ)
            !^ To count the number of leading zero bits.
            tSInt128, INTENT(IN)    :: I128
            tSInt32                 :: NumLZ
        END FUNCTION I128_LeadingZeros
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE TRAILZ
        !^ **Function Interface**: TRAILZ <br>
        !  **Purpose**:  To count the number of trailing zero bits of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    NumTZ = TRAILZ(INPUT)
        MODULE FUNCTION I128_TrailingZeros(I128) RESULT(NumTZ)
            !^ To count the number of trailing zero bits.
            tSInt128, INTENT(IN)    :: I128
            tSInt32                 :: NumTZ
        END FUNCTION I128_TrailingZeros
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE POPCNT
        !^ **Function Interface**: POPCNT <br>
        !  **Purpose**:  To count the number of 1 bits in the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    NumBits = POPCNT(INPUT)
        MODULE FUNCTION I128_Count1Bits(I128) RESULT(NumBits)
            !^ To count the number of 1 bits in the specified input.
            tSInt128, INTENT(IN)    :: I128
            tSInt32                 :: NumBits
        END FUNCTION I128_Count1Bits
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE POPPAR
        !^ **Function Interface**: POPPAR <br>
        !  **Purpose**:  To determine the parity of the input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    NumPar = POPPAR(INPUT)
        MODULE FUNCTION I128_Parity(I128) RESULT(ParNum)
            !^ To determine the parity of the specified input.
            tSInt128, INTENT(IN)    :: I128
            tSInt32                 :: ParNum
        END FUNCTION I128_Parity
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IBSET
        !^ **Function Interface**: IBSET <br>
        !  **Purpose**:  To set the bit at the specified position to 1.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IBSET(IN, Pos)
        MODULE FUNCTION I128_SetBit(InVal, Pos) RESULT(OutVal)
            !^ To set the bit at the specified position to 1.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: Pos
            tSInt128                :: OutVal
        END FUNCTION I128_SetBit
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IBCLR
        !^ **Function Interface**: IBCLR <br>
        !  **Purpose**:  To set the bit at the specified position to 0.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IBCLR(IN, Pos)
        MODULE FUNCTION I128_ClearBit(InVal, Pos) RESULT(OutVal)
            !^ To set the bit at the specified position to 0.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: Pos
            tSInt128                :: OutVal
        END FUNCTION I128_ClearBit
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IBCHNG
        !^ **Function Interface**: IBCHNG <br>
        !  **Purpose**:  To reverse the bit at the specified position.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IBCHNG(IN, Pos)
        MODULE FUNCTION I128_FlipBit(InVal, Pos) RESULT(OutVal)
            !^ To reverse the bit at the specified position.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: Pos
            tSInt128                :: OutVal
        END FUNCTION I128_FlipBit
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE BTEST
        !^ **Function Interface**: BTEST <br>
        !  **Purpose**:  To check whether the bit at the specified position is 0 (False) or 1 (True).
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    Flag = BTEST(IN, Pos)
        MODULE FUNCTION I128_TestBit(I128, Pos) RESULT(Flag)
            !^ To check whether the bit at the specified position is 0 (False) or 1 (True).
            tSInt128, INTENT(IN)    :: I128
            tSInt32,  INTENT(IN)    :: Pos
            tLogical                :: Flag
        END FUNCTION I128_TestBit
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IBITS
        !^ **Function Interface**: IBITS <br>
        !  **Purpose**:  To extract a sequence of bits according to the specified input.
        !   (For more information, see detailed explanation of the intrinsic function.) <br>
        !  **Usage**: <br>
        !   --->    OUT = IBITS(IN, Pos, Len)
        MODULE FUNCTION I128_ExtractBits(InVal, Pos, Len) RESULT(OutVal)
            !^ To extract a sequence of bits according to the specified input.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: Pos
            tSInt32,  INTENT(IN)    :: Len
            tSInt128                :: OutVal
        END FUNCTION I128_ExtractBits
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE MoveBits
        !^ **Subroutine Interface**: MoveBits <br>
        !  **Purpose**:  To copy a sequence of bits (a bit field) from one location to another.
        !   (For more information, see detailed explanation of the intrinsic subroutine 'MVBITS'.) <br>
        !  **Usage**: <br>
        !   --->    CALL MoveBits(InVal, InPos, Len, OutVal, OutPos)
        MODULE SUBROUTINE I128_MoveBits(InVal, InPos, Len, OutVal, OutPos)
            !^ To copy a sequence of bits (a bit field) from one location to another.
            tSInt128, INTENT(IN)    :: InVal
            tSInt32,  INTENT(IN)    :: InPos
            tSInt32,  INTENT(IN)    :: Len
            tSInt128, INTENT(INOUT) :: OutVal
            tSInt32,  INTENT(IN)    :: OutPos
        END SUBROUTINE I128_MoveBits
    END INTERFACE
    !-----------------------------------------------
    !-----             Inquiry Routines        -----
    !-----------------------------------------------
    INTERFACE IsPositive
        !^ **Function Interface**: IsPositive <br>
        !  **Purpose**:  To check whether the input value is positive or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsPositive(INPUT) <br>
        !   --->    IF (.NOT.IsPositive(INPUT)) DoSomeThing
        MODULE PROCEDURE I128_Is_Positive
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IsNegative
        !^ **Function Interface**: IsNegative <br>
        !  **Purpose**:  To check whether the input value is negative or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsNegative(INPUT) <br>
        !   --->    IF (.NOT.IsNegative(INPUT)) DoSomeThing
        MODULE PROCEDURE I128_Is_Negative
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE IsZero
        !^ **Function Interface**: IsZero <br>
        !  **Purpose**:  To check whether the input value is zero or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsZero(INPUT) <br>
        !   --->    IF (.NOT.IsZero(INPUT)) DoSomeThing
        MODULE PROCEDURE I128_Is_Zero
    END INTERFACE
    !-----------------------------------------------
    !-----          Auxiliary Routines         -----
    !-----------------------------------------------
    INTERFACE BitCastToSigned
        ! private function interface
        MODULE PROCEDURE U64_To_I64
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE UABS
        ! private function interface
        MODULE PROCEDURE I128_UnsignedAbsolute
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ABS
        !^ **Function Interface**: ABS <br>
        !  **Purpose**:  To return the absolute value of the input. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = ABS(INPUT)
        MODULE PROCEDURE I128_Absolute
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE MIN
        !^ **Function Interface**: MIN <br>
        !  **Purpose**:  To return the minimum value of the specified input. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = MIN(INP1, INP2) <br>
        !   --->    OUTPUT = MIN(INPARR) <br>
        MODULE PROCEDURE I128_Minimum
        MODULE PROCEDURE I128Array_Minimum
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE MAX
        !^ **Function Interface**: MAX <br>
        !  **Purpose**:  To return the maximum value of the specified input. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = MAX(INP1, INP2) <br>
        !   --->    OUTPUT = MAX(INPARR) <br>
        MODULE PROCEDURE I128_Maximum
        MODULE PROCEDURE I128Array_Maximum
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE Display
        !^ **Subroutine Interface**: Display <br>
        !  **Purpose**:  To write/display the 'SInt128' object to the screen (or the specified unit). <br>
        !  **Usage**: <br>
        !   ! To display (signed) value of I128 as a decimal string to the screen <br>
        !   --->    CALL Display(I128) <br>
        !   ! To display (signed) value of I128 as a decimal string to the output logical unit <br>
        !   --->    CALL Display(I128, 11) <br>
        !   ! To display (signed) value of I128 as a decimal string to the output logical unit <br>
        !   with input/output status and message <br>
        !   --->    CALL Display(I128, 11, IOStat, IOMsg) <br>
        !   ! To display (signed) values of components of I128 as a decimal string to the screen <br>
        !   --->    CALL Display(I128, ShowComponent=.TRUE.) <br>
        !   ! To display (signed) value of I128 as a decimal string to the screen with a prefix string <br>
        !   --->    CALL Display(I128, Prefix='Signed value of I128')
        MODULE PROCEDURE I128_Write
    END INTERFACE
    !-----------------------------------------------
    !-----       Experimental Routines         -----
    !-----------------------------------------------
    INTERFACE AddXp
        MODULE FUNCTION I128_Addition_Xp(LhsVal, RhsVal, Algo) RESULT(OutVal)
            tSInt128, INTENT(IN)   :: LhsVal
            tSInt128, INTENT(IN)   :: RhsVal
            tSInt32,  INTENT(IN)   :: Algo
            tSInt128               :: OutVal
        END FUNCTION I128_Addition_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE SubXp
        MODULE FUNCTION I128_Subtraction_Xp(LhsVal, RhsVal, Algo) RESULT(OutVal)
            tSInt128, INTENT(IN)   :: LhsVal
            tSInt128, INTENT(IN)   :: RhsVal
            tSInt32,  INTENT(IN)   :: Algo
            tSInt128               :: OutVal
        END FUNCTION I128_Subtraction_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE MulXp
        MODULE FUNCTION I128_Multiplication_Xp(LhsVal, RhsVal, Algo) RESULT(OutVal)
            tSInt128, INTENT(IN)   :: LhsVal
            tSInt128, INTENT(IN)   :: RhsVal
            tSInt32,  INTENT(IN)   :: Algo
            tSInt128               :: OutVal
        END FUNCTION I128_Multiplication_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE DivModXp
        MODULE SUBROUTINE I128_DivMod_Xp(Dividend, Divisor, Algo, Quotient, Remainder)
            tSInt128, INTENT(IN)   :: Dividend
            tSInt128, INTENT(IN)   :: Divisor
            tSInt32,  INTENT(IN)   :: Algo
            tSInt128, INTENT(OUT)  :: Quotient
            tSInt128, INTENT(OUT)  :: Remainder
        END SUBROUTINE I128_DivMod_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToDecStrXp
        MODULE FUNCTION I128_ToDecString_Xp(I128, Algo) RESULT(Str)
            tSInt128, INTENT(IN)   :: I128
            tSInt32,  INTENT(IN)   :: Algo
            tCharAlloc             :: Str
        END FUNCTION I128_ToDecString_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ParseI128Xp
        MODULE FUNCTION I128_FromDecString_Xp(cStr, Algo, ErrFlag, ErrMsg) RESULT(Number)
            tCharStar,            INTENT(IN)    :: cStr     ! character string
            tSInt32,              INTENT(IN)    :: Algo
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
            tSInt128                            :: Number   ! number
        END FUNCTION I128_FromDecString_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToI128Xp
        MODULE FUNCTION R32_To_I128_Xp(R32, Algo) RESULT(I128)
            tRealSP, INTENT(IN)    :: R32
            tSInt32,  INTENT(IN)    :: Algo
            tSInt128                :: I128
        END FUNCTION R32_To_I128_Xp
        !------------------------------------------------------------
        MODULE FUNCTION R64_To_I128_Xp(R64, Algo) RESULT(I128)
            tRealDP, INTENT(IN)    :: R64
            tSInt32,  INTENT(IN)    :: Algo
            tSInt128            :: I128
        END FUNCTION R64_To_I128_Xp
        !------------------------------------------------------------
        MODULE FUNCTION R128_To_I128_Xp(R128, Algo) RESULT(I128)
            tRealQP,  INTENT(IN) :: R128
            tSInt32, INTENT(IN) :: Algo
            tSInt128            :: I128
        END FUNCTION R128_To_I128_Xp
        !------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToR32Xp
        MODULE FUNCTION R32_From_I128_Xp(I128, Algo) RESULT(R32)
            tSInt128, INTENT(IN)    :: I128
            tSInt32,  INTENT(IN)    :: Algo
            tRealSP                :: R32
        END FUNCTION R32_From_I128_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToR64Xp
        MODULE FUNCTION R64_From_I128_Xp(I128, Algo) RESULT(R64)
            tSInt128, INTENT(IN)    :: I128
            tSInt32,  INTENT(IN)    :: Algo
            tRealDP                :: R64
        END FUNCTION R64_From_I128_Xp
    END INTERFACE
    !--------------------------------------------------------------------------
    INTERFACE ToR128Xp
        MODULE FUNCTION R128_From_I128_Xp(I128, Algo) RESULT(R128)
            tSInt128, INTENT(IN)    :: I128
            tSInt32,  INTENT(IN)    :: Algo
            tRealQP                  :: R128
        END FUNCTION R128_From_I128_Xp
    END INTERFACE
    !--------------------------------------------------------------------------

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!------------------------------------------------------------------------------
!
!                           INQUIRY ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION I128_Is_Zero(I128) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the number is zero or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: I128
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = I128 == ZeroI128

    RETURN

END FUNCTION I128_Is_Zero

!******************************************************************************

FUNCTION I128_Is_Negative(I128) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the number is negative or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: I128
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = I128%High < 0_kInt64

    RETURN

END FUNCTION I128_Is_Negative

!******************************************************************************

FUNCTION I128_Is_Positive(I128) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the number is positive or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: I128
    tLogical                :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (I128%High > 0_kInt64).OR.((I128%High == 0_kInt64).AND.(I128%Low /= 0_kInt64))

    RETURN

END FUNCTION I128_Is_Positive

!------------------------------------------------------------------------------
!
!                           AUXILIARY ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION U64_To_I64(U64) RESULT(I64)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert unsigned 64-bit integer to signed 64-bit integer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: U64      ! number treated as unsigned
    tSInt64             :: I64      ! number treated as signed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! Casting an unsigned integer to a signed integer of the same
    ! width is implementation defined behavior if the source value would not fit
    ! in the destination type. We step around it with a round-trip bitwise not
    ! operation to make sure this function remains constant expression.
    IF (IAND(U64, SHIFTL(1_kInt64, 63)) /= 0_kInt64) THEN
        I64 = NOT(NOT(U64))
    ELSE
        I64 = U64
    END IF

    RETURN

END FUNCTION U64_To_I64

!******************************************************************************

FUNCTION I128_UnsignedAbsolute(I128) RESULT(U128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned absolute value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: I128
    tUInt128                :: U128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! Cast to uint128 before possibly negating because -Int128Min() is undefined.
    IF (I128%High < 0_kInt64) THEN
        U128 = -ToU128(I128)
    ELSE
        U128 = ToU128(I128)
    END IF

    RETURN

END FUNCTION I128_UnsignedAbsolute

!******************************************************************************

FUNCTION I128_Absolute(I128) RESULT(ABS)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the absolute value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: I128
    tSInt128                :: ABS

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (IsNegative(I128)) THEN
        ABS = -I128
    ELSE
        ABS = I128
    END IF

    RETURN

END FUNCTION I128_Absolute

!******************************************************************************

FUNCTION I128_Minimum(A, B) RESULT(MIN)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the minimum value of the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: A, B
    tSInt128                :: MIN

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (A < B) THEN
        MIN = A
    ELSE
        MIN = B
    END IF

    RETURN

END FUNCTION I128_Minimum

!******************************************************************************

FUNCTION I128Array_Minimum(Arr) RESULT(MIN)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the minimum value of the specified input array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: Arr(:)
    tSInt128                :: MIN

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I

!** FLOW

    MIN = Arr(1)
    DO I = 2_kIndex, SIZE(Arr, KIND=kIndex)
        IF (MIN < Arr(I)) MIN = Arr(I)
    END DO

    RETURN

END FUNCTION I128Array_Minimum

!******************************************************************************

FUNCTION I128_Maximum(A, B) RESULT(MAX)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the maximum value of the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: A, B
    tSInt128                :: MAX

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (A > B) THEN
        MAX = A
    ELSE
        MAX = B
    END IF

    RETURN

END FUNCTION I128_Maximum

!******************************************************************************

FUNCTION I128Array_Maximum(Arr) RESULT(MAX)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the maximum value of the specified input array.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128, INTENT(IN)    :: Arr(:)
    tSInt128                :: MAX

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I

!** FLOW

    MAX = Arr(1)
    DO I = 2_kIndex, SIZE(Arr, KIND=kIndex)
        IF (MAX > Arr(I)) MAX = Arr(I)
    END DO

    RETURN

END FUNCTION I128Array_Maximum

!******************************************************************************

SUBROUTINE I128_Write(I128, Unit, IOStat, IOMsg, ShowComponent, Prefix)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To write 'SInt128' object to the screen (or the specified unit).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt128,            INTENT(IN)     :: I128
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
            DispStr = Prefix // ToDecString(I128)
        ELSE
            DispStr = ' I128 = ' // ToDecString(I128)
        END IF
        WRITE(UNIT=OutUnit, FMT='(A)', IOSTAT=IO_Stat, IOMSG=IO_Msg) DispStr
    ELSE
        DispStr = '-: '
        IF (PRESENT(Prefix)) DispStr = Prefix
        WRITE(UNIT=OutUnit, FMT='(A, I0)', IOSTAT=IO_Stat, IOMSG=IO_Msg) &
              DispStr // 'High value = ', I128%High
        WRITE(UNIT=OutUnit, FMT='(A, I0)', IOSTAT=IO_Stat, IOMSG=IO_Msg) &
              DispStr // 'Low value = ', I128%Low
    END IF

    ! return output if requested
    IF (PRESENT(IOStat)) IOStat = IO_Stat
    IF (PRESENT(IOMsg))  IOMsg  = IO_Msg

    RETURN

END SUBROUTINE I128_Write

!******************************************************************************

END MODULE MBase_SInt128

!******************************************************************************
