
MODULE MClass_ApInt32

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *ApInt32* type, its related routines and some useful
!   parameter-like functions.  The *ApInt32* type is a derived type representing
!   a **mutable, arbitrary-precision signed** integer.  Various common operations
!   usually available for integer types are provided including arithmetic, bitwise,
!   comparison, and conversion/construction operations.  The application programming
!   interface (API) follows Fortran intrinsic integer types with additional functions/
!   methods provided. <br>
!   Due to the fact that the *ApInt32* type has an allocatable component, unlike
!   other integer types available, the *ApInt32* type normally requires an explicit
!   construction either via a constructor method or an assignment expression before
!   being used as an input argument in other operations.  Otherwise, the number
!   would be interpreted as having zero value. <br>
!   If a number is always in a range provided by a 128-bit signed integer, a user can
!   use the <a href="../../xpfbase/module/mbase_sint128.html#type-sint128">SInt128</a>
!   type instead for efficiency reason.  Likewise, if a number is always in a range
!   provided by a 128-bit unsigned integer, the
!   <a href="../../xpfbase/module/mbase_uint128.html#type-uint128">UInt128</a> type
!   can be used with better performance. <br>
!   Similar to the *SInt128* type, mixed types of signed integer types (32-bit, 64-bit,
!   128-bit or arbitrary-precision) are allowed in arithmetic operations.  Likewise,
!   arguments with Fortran intrinsic integer types in all public methods are considered
!   to be signed, except those in some of conversion methods.  Also, for comparison and
!   bitwise operations that require two input arguments, both arguments must only be the
!   *ApInt32* type.  The operations on mixed types are not provided.  Therefore, all other
!   types must be explicitly converted to the *ApInt32* type before using in the comparison
!   and bitwise operations. <br>
!   It should be noted that unlike both the *SInt128* and *UInt128* types where a conventional
!   API (procedural programming) following Fortran integer types is used, the *ApInt32* type
!   uses a mixed programming style.  For basic integer operations, a conventional API is used.
!   For additional functionalities provided, both a conventional API and an object-oriented
!   API (via type-bound procedures) are used.  Therefore, for additional functions/methods,
!   the usage of the *ApInt32* type may differ from the usage of the *SInt128* and *UInt128*
!   types. <br>
!   See the <a href="../module/mclass_apint64.html">ApInt64</a> type for a **mutable,
!   arbitrary-precision signed** integer that has mostly identical functionalities.
!   The *ApInt64* type is similar to the *ApInt32* type, but employs the base of
!   2<sup>64</sup> instead of 2<sup>32</sup>.  Therefore, the *ApInt64* type uses
!   many similar algorithms but requires different implementations.  Also, the
!   *ApInt64* type provides experimental routines for various operations while
!   the *ApInt32* type does not. <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://github.com/bwakell/Huldra/blob/master/src/main/java/org/huldra/math/BigInt.java">
!       The Huldra Project: BigInt</a> <br>
!   [2] <a href="https://docs.oracle.com/javase/8/docs/api/?java/math/BigInteger.html">
!       Java's Class BigInteger</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_UIntUtil
    USE MBase_SInt128
    USE MBase_UInt128
    USE MClass_BaseRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type & constructor
    PUBLIC :: ApInt32
    ! assignment & conversion
    PUBLIC :: ASSIGNMENT(=)
    PUBLIC :: ToU32, ToU64, ToU128
    PUBLIC :: ToI32, ToI64, ToI128
    PUBLIC :: ToR32, ToR64, ToR128
    PUBLIC :: ToBytes, ToDecString
    ! comparison
    PUBLIC :: OPERATOR(==), OPERATOR(/=)
    PUBLIC :: OPERATOR(<),  OPERATOR(<=)
    PUBLIC :: OPERATOR(>),  OPERATOR(>=)
    PUBLIC :: Compare
    ! arithmetic
    PUBLIC :: OPERATOR(+), OPERATOR(-)
    PUBLIC :: OPERATOR(*), OPERATOR(/)
    PUBLIC :: OPERATOR(**)
    PUBLIC :: MOD, MODULO, SQR
    ! bitwise
    PUBLIC :: SHIFTL, SHIFTR, ISHFT
    PUBLIC :: NOT, IOR, IAND, IAND_NOT
    PUBLIC :: IEOR, LEADZ, TRAILZ
    PUBLIC :: POPCNT, POPPAR, IBSET, IBCLR
    PUBLIC :: IBCHNG, BTEST
    ! parameter-like
    PUBLIC :: ZeroApInt32, OneApInt32
    ! inquiry
    PUBLIC :: IsPositive, IsNegative, IsZero, IsOne
    ! miscellaneous
    PUBLIC :: ABS, RandNumApInt32
    ! auxiliary
    PUBLIC :: MakeCopy, GetLength, Display

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of the module
    tCharStar, PARAMETER    :: ModName = 'MClass_ApInt32'
    ! Used to cast a 32-bit integer to a 64-bit integer without getting unwanted sign extension
    tSInt64,   PARAMETER    :: MASK = ToInt64(Z'00000000FFFFFFFF')  ! SHIFTL(1_kInt64, 32) - 1_kInt64

!** DERIVED TYPE DEFINITIONS
    !> *ApInt32* is a mutable arbitrary-precision signed integer type where
    !   its representations are as follows. <br>
    ! - Base is 2<sup>32</sup>. <br>
    ! - Magnitude is represented by the 'Digit' array in little-endian order. <br>
    ! - The 'Length' first 'Digit' count as the *ApInt32* number (i.e. it is always
    !   less than or equal to the size/capacity of the 'Digit' array). <br>
    ! - Sign is represented by a sign integer (-1 or 1). <br>
    ! - Internally zero is allowed to have either sign.
    !   (Otherwise one would have to remember to check for sign-swap for division,
    !   multiplication etc...) <br>
    ! - Zero can have many forms: <br>
    !   -> The most common form of zero has 'Length' = 1 and Digit(0) = 0 (set through
    !      ZeroApInt32()). <br>
    !   -> If 'Digit' has not yet been allocated or 'Length' is less than 1, the number
    !      is considered to be zero. <br>
    TYPE ApInt32
        PRIVATE
        !> The sign of this number. <br>
        ! -> +1 for positive numbers and -1 for negative numbers. <br>
        ! -> Zero can have either sign.
        tSInt32                 :: Sign = 1
        !% The number of digits of the number (in base 2<sup>64</sup>).
        tIndex                  :: Length = 0_kIndex
        !> The digits of the number, i.e., the magnitude array. <br>
        !  Values are treated as unsigned integer
        tUInt32, ALLOCATABLE    :: Digit(:)
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: ApInt32_Add_I32
        PROCEDURE, PRIVATE  :: ApInt32_Add_I64
        PROCEDURE, PRIVATE  :: ApInt32_Add_I128
        PROCEDURE, PRIVATE  :: ApInt32_Add_ApInt32
        PROCEDURE, PRIVATE  :: ApInt32_Subtract_I32
        PROCEDURE, PRIVATE  :: ApInt32_Subtract_I64
        PROCEDURE, PRIVATE  :: ApInt32_Subtract_I128
        PROCEDURE, PRIVATE  :: ApInt32_Subtract_ApInt32
        PROCEDURE, PRIVATE  :: ApInt32_Times_I32
        PROCEDURE, PRIVATE  :: ApInt32_Times_I64
        PROCEDURE, PRIVATE  :: ApInt32_Times_I128
        PROCEDURE, PRIVATE  :: ApInt32_Times_ApInt32
        PROCEDURE, PRIVATE  :: ApInt32_Over_I32
        PROCEDURE, PRIVATE  :: ApInt32_Over_I64
        PROCEDURE, PRIVATE  :: ApInt32_Over_I128
        PROCEDURE, PRIVATE  :: ApInt32_Over_ApInt32
        PROCEDURE, PRIVATE  :: ApInt32_DivMod_I32
        PROCEDURE, PRIVATE  :: ApInt32_DivMod_I64
        PROCEDURE, PRIVATE  :: ApInt32_DivMod_I128
        PROCEDURE, PRIVATE  :: ApInt32_DivMod_ApInt32
        ! ---------------------------------------------------------------------
        ! -----                 Public Procedures                         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Increment <br>
        !  **Purpose**:  To increase value of the ApInt32 number by one. <br>
        !  **Usage**: <br>
        !   --->    CALL ApNum%Increment()
        PROCEDURE   :: Increment    => ApInt32_Increment
        !> **Type-Bound Subroutine**: Decrement <br>
        !  **Purpose**:  To decrease value of the ApInt32 number by one. <br>
        !  **Usage**: <br>
        !   --->    CALL ApNum%Decrement()
        PROCEDURE   :: Decrement    => ApInt32_Decrement
        !> **Type-Bound Subroutine**: Square <br>
        !  **Purpose**:  To return the ApInt32 number (ApInt32) where its value is
        !                computed by: ApNum = ApNum * ApNum. <br>
        !  **Usage**: <br>
        !   --->    CALL ApNum%Square()
        PROCEDURE   :: Square       => ApInt32_SquareSub
        !> **Type-Bound Function**: Compare <br>
        !  **Purpose**:  To compare two ApInt32 numbers (LHS and RHS) and return <br>
        !   -1 if LHS < RHS, <br>
        !    0 if LHS == RHS, or <br>
        !    1 if LHS > RHS. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS%Compare(RHS) <br>
        !   --->    IF (LHS%Compare(RHS) /= 0) DoSomething
        PROCEDURE   :: Compare      => ApInt32_Compare
        !> **Type-Bound Subroutine**: LShift <br>
        !  **Purpose**:  To perform logical left shift where ShiftPos is non-negative. <br>
        !  **Usage**: <br>
        !   --->    CALL ApNum%LShift(112)
        PROCEDURE   :: LShift       => ApInt32_LeftShift
        !> **Type-Bound Subroutine**: RShift <br>
        !  **Purpose**:  To perform logical right shift where ShiftPos is non-negative. <br>
        !  **Usage**: <br>
        !   --->    CALL ApNum%RShift(112)
        PROCEDURE   :: RShift       => ApInt32_RightShift
        !> **Type-Bound Function**: IsZero <br>
        !  **Purpose**:  To check whether the ApInt32 number has value of zero or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ApNum%IsZero() <br>
        !   --->    IF (.NOT.ApNum%IsZero()) DoSomething
        PROCEDURE   :: IsZero       => ApInt32_Is_Zero_II
        !> **Type-Bound Function**: IsOne <br>
        !  **Purpose**:  To check whether the ApInt32 number has value of one or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ApNum%IsOne() <br>
        !   --->    IF (.NOT.ApNum%IsOne()) DoSomething
        PROCEDURE   :: IsOne        => ApInt32_Is_One
        !> **Type-Bound Function**: IsPositive <br>
        !  **Purpose**:  To check whether the ApInt32 number has positive value or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ApNum%IsPositive() <br>
        !   --->    IF (.NOT.ApNum%IsPositive()) DoSomething
        PROCEDURE   :: IsPositive   => ApInt32_Is_Positive
        !> **Type-Bound Function**: IsNegative <br>
        !  **Purpose**:  To check whether the ApInt32 number has negative value or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = ApNum%IsNegative() <br>
        !   --->    IF (.NOT.ApNum%IsNegative()) DoSomething
        PROCEDURE   :: IsNegative   => ApInt32_Is_Negative
        !> **Type-Bound Subroutine**: RandNum <br>
        !  **Purpose**:  To generate the ApIn64 number with random value. <br>
        !  **Usage**: <br>
        !   ! generate random number with default settings <br>
        !   --->    CALL ApNum%RandNum() <br>
        !   ! generate random number with specified PRNG <br>
        !   --->    CALL ApNum%RandNum(PRNG) <br>
        !   ! generate random number with negative value <br>
        !   --->    CALL ApNum%RandNum(Positive=.FALSE.) <br>
        !   ! generate random number with specified length of magnitude array <br>
        !   --->    CALL ApNum%RandNum(Length=MagLen)
        PROCEDURE   :: RandNum      => ApInt32_RandNumSub
        !> **Type-Bound Function**: Copy <br>
        !  **Purpose**:  To make a copy of the ApInt32 number. <br>
        !  **Usage**: <br>
        !   ! make a copy of the ApInt32 number <br>
        !   --->    DstApNum = SrcApNum%Copy() <br>
        !   ! make a copy of the ApInt32 number with the specified capacity
        !   (size of magnitude array) of the destination number <br>
        !   --->    DstApNum = SrcApNum%Copy(DstCap)
        PROCEDURE   :: Copy         => ApInt32_Copy
        !> **Type-Bound Subroutine**: Display <br>
        !  **Purpose**:  To write/display the 'ApInt32' number to the screen (or the specified unit). <br>
        !  **Usage**: <br>
        !   ! To display (signed) value of ApNum as a decimal string to the screen <br>
        !   --->    CALL ApNum%Display() <br>
        !   ! To display (signed) value of ApNum as a decimal string to the output logical unit <br>
        !   --->    CALL ApNum%Display(11) <br>
        !   ! To display (signed) value of ApNum as a decimal string to the output logical unit <br>
        !   with input/output status and message <br>
        !   --->    CALL ApNum%Display(11, IOStat, IOMsg) <br>
        !   ! To display (signed) values of components of ApNum as a decimal string to the screen <br>
        !   --->    CALL ApNum%Display(ShowComponent=.TRUE.) <br>
        !   ! To display (signed) value of ApNum as a decimal string to the screen with a prefix string <br>
        !   --->    CALL ApNum%Display(Prefix='Signed value of ApNum')
        PROCEDURE   :: Display      => ApInt32_Write
        !> **Type-Bound Function**: GetLength <br>
        !  **Purpose**:  To return the length of the magnitude array (the number of
        !                digits counted as the ApNum number). <br>
        !  **Usage**: <br>
        !   --->    MagLen = ApNum%GetLength()
        PROCEDURE   :: GetLength    => ApInt32_GetLength
        ! ---------------------------------------------------------------------
        ! -----                 Generic Interfaces                         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Add <br>
        !  **Purpose**:  To perform an addition: This = This + Other. <br>
        !  **Usage**: <br>
        !   --->    CALL This%Add(Other)
        GENERIC     :: Add          => ApInt32_Add_I32,       ApInt32_Add_I64,      &
                                       ApInt32_Add_I128,      ApInt32_Add_ApInt32
        !> **Type-Bound Subroutine**: Subtract <br>
        !  **Purpose**:  To perform a subtraction: This = This - Other. <br>
        !  **Usage**: <br>
        !   --->    CALL This%Subtract(Other)
        GENERIC     :: Subtract     => ApInt32_Subtract_I32,  ApInt32_Subtract_I64, &
                                       ApInt32_Subtract_I128, ApInt32_Subtract_ApInt32
        !> **Type-Bound Subroutine**: Multiply <br>
        !  **Purpose**:  To perform a multiplication: This = This * Other. <br>
        !  **Usage**: <br>
        !   --->    CALL This%Multiply(Other)
        GENERIC     :: Multiply     => ApInt32_Times_I32,     ApInt32_Times_I64,    &
                                       ApInt32_Times_I128,    ApInt32_Times_ApInt32
        !> **Type-Bound Subroutine**: Divide <br>
        !  **Purpose**:  To perform a division: This = This / Other.  Optionally,
        !                return the remainder if present. <br>
        !  **Usage**: <br>
        !   --->    CALL This%Divide(Other) <br>
        !   --->    CALL This%Divide(Other, Remainder)
        GENERIC     :: Divide       => ApInt32_Over_I32,      ApInt32_Over_I64,     &
                                       ApInt32_Over_I128,     ApInt32_Over_ApInt32
        !> **Type-Bound Subroutine**: DivMod <br>
        !  **Purpose**:  To perform a division and return both quotient and remainder. <br>
        !  **Usage**: <br>
        !   --->    CALL Numerator%DivMod(Denominator, Quotient, Remainder)
        GENERIC     :: DivMod       => ApInt32_DivMod_I32,    ApInt32_DivMod_I64,   &
                                       ApInt32_DivMod_I128,   ApInt32_DivMod_ApInt32
        ! ---------------------------------------------------------------------
    END TYPE ApInt32

!** INTERFACE DEFINITIONS:
    !------------------------------------------------------------
    ! type-bound procedure interfaces
    !------------------------------------------------------------
    INTERFACE
        !------------------------------------------------------------
        ! comparison
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Compare(LHS, RHS) RESULT(Flag)
            !^ To compare two ApInt32 numbers (LHS and RHS) and return <br>
            !   -1 if LHS < RHS, <br>
            !    0 if LHS == RHS, or <br>
            !    1 if LHS > RHS.
            CLASS(ApInt32), INTENT(IN)  :: LHS
            TYPE(ApInt32),  INTENT(IN)  :: RHS
            tSInt32                     :: Flag
        END FUNCTION ApInt32_Compare
        !------------------------------------------------------------
        ! bitwise
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_LeftShift(Val, ShiftPos)
            !^ To perform logical left shift with 0 <= ShiftPos <= 128.
            CLASS(ApInt32), INTENT(INOUT)   :: Val
            tSInt32,        INTENT(IN)      :: ShiftPos
        END SUBROUTINE ApInt32_LeftShift
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_RightShift(Val, ShiftPos)
            !^ To perform logical right shift with 0 <= ShiftPos <= 128.
            CLASS(ApInt32), INTENT(INOUT)   :: Val
            tSInt32,        INTENT(IN)      :: ShiftPos
        END SUBROUTINE ApInt32_RightShift
        !------------------------------------------------------------
        ! arithmetic
        !------------------------------------------------------------
        !------------------------------------------------------------
        ! 'Plus' operations
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Increment(Val)
            !^ To perform addition: Val = Val + 1.
            CLASS(ApInt32), INTENT(INOUT)   :: Val
        END SUBROUTINE ApInt32_Increment
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Add_ApInt32(This, Other)
            !^ To perform addition: This = This + Other.
            CLASS(ApInt32), INTENT(INOUT)   :: This
            TYPE(ApInt32),  INTENT(IN)      :: Other
        END SUBROUTINE ApInt32_Add_ApInt32
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Add_I32(This, Other)
            !^ To perform addition: This = This + Other.
            CLASS(ApInt32), INTENT(INOUT)   :: This
            tSInt32,        INTENT(IN)      :: Other
        END SUBROUTINE ApInt32_Add_I32
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Add_I64(This, Other)
            !^ To perform addition: This = This + Other.
            CLASS(ApInt32), INTENT(INOUT)   :: This
            tSInt64,        INTENT(IN)      :: Other
        END SUBROUTINE ApInt32_Add_I64
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Add_I128(This, Other)
            !^ To perform addition: This = This + Other.
            CLASS(ApInt32), INTENT(INOUT)   :: This
            TYPE(SInt128),  INTENT(IN)      :: Other
        END SUBROUTINE ApInt32_Add_I128
        !------------------------------------------------------------
        ! 'Minus' operations
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Decrement(Val)
            !^ To perform subtraction: Val = Val - 1.
            CLASS(ApInt32), INTENT(INOUT)   :: Val
        END SUBROUTINE ApInt32_Decrement
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Subtract_ApInt32(This, Other)
            !^ To perform subtraction: This = This - Other.
            CLASS(ApInt32), INTENT(INOUT)   :: This
            TYPE(ApInt32),  INTENT(IN)      :: Other
        END SUBROUTINE ApInt32_Subtract_ApInt32
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Subtract_I32(This, Other)
            !^ To perform subtraction: This = This - Other.
            CLASS(ApInt32), INTENT(INOUT)   :: This
            tSInt32,        INTENT(IN)      :: Other
        END SUBROUTINE ApInt32_Subtract_I32
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Subtract_I64(This, Other)
            !^ To perform subtraction: This = This - Other.
            CLASS(ApInt32), INTENT(INOUT)   :: This
            tSInt64,        INTENT(IN)      :: Other
        END SUBROUTINE ApInt32_Subtract_I64
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Subtract_I128(This, Other)
            !^ To perform subtraction: This = This - Other.
            CLASS(ApInt32), INTENT(INOUT)   :: This
            TYPE(SInt128),  INTENT(IN)      :: Other
        END SUBROUTINE ApInt32_Subtract_I128
        !------------------------------------------------------------
        ! 'Multiply' operations
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Times_ApInt32(This, Other)
            !^ To perform multiplication: This = This * Other.
            CLASS(ApInt32), INTENT(INOUT)   :: This
            TYPE(ApInt32),  INTENT(IN)      :: Other
        END SUBROUTINE ApInt32_Times_ApInt32
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Times_I32(This, Other)
            !^ To perform multiplication: This = This * Other.
            CLASS(ApInt32), INTENT(INOUT)   :: This
            tSInt32,        INTENT(IN)      :: Other
        END SUBROUTINE ApInt32_Times_I32
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Times_I64(This, Other)
            !^ To perform multiplication: This = This * Other.
            CLASS(ApInt32), INTENT(INOUT)   :: This
            tSInt64,        INTENT(IN)      :: Other
        END SUBROUTINE ApInt32_Times_I64
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Times_I128(This, Other)
            !^ To perform multiplication: This = This * Other.
            CLASS(ApInt32), INTENT(INOUT)   :: This
            TYPE(SInt128),  INTENT(IN)      :: Other
        END SUBROUTINE ApInt32_Times_I128
        !------------------------------------------------------------
        ! 'Divide' operations
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_DivMod_ApInt32(Dividend, Divisor, Quotient, Remainder)
            !^ To perform division (Dividend / Divisor) and return both
            !  the quotient and the remainder.
            CLASS(ApInt32), INTENT(IN)  :: Dividend
            TYPE(ApInt32),  INTENT(IN)  :: Divisor
            TYPE(ApInt32),  INTENT(OUT) :: Quotient
            TYPE(ApInt32),  INTENT(OUT) :: Remainder
        END SUBROUTINE ApInt32_DivMod_ApInt32
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_DivMod_I32(Dividend, Divisor, Quotient, Remainder)
            !^ To perform division (Dividend / Divisor) and return both
            !  the quotient and the remainder.
            CLASS(ApInt32), INTENT(IN)  :: Dividend
            tSInt32,        INTENT(IN)  :: Divisor
            TYPE(ApInt32),  INTENT(OUT) :: Quotient
            TYPE(ApInt32),  INTENT(OUT) :: Remainder
        END SUBROUTINE ApInt32_DivMod_I32
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_DivMod_I64(Dividend, Divisor, Quotient, Remainder)
            !^ To perform division (Dividend / Divisor) and return both
            !  the quotient and the remainder.
            CLASS(ApInt32), INTENT(IN)  :: Dividend
            tSInt64,        INTENT(IN)  :: Divisor
            TYPE(ApInt32),  INTENT(OUT) :: Quotient
            TYPE(ApInt32),  INTENT(OUT) :: Remainder
        END SUBROUTINE ApInt32_DivMod_I64
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_DivMod_I128(Dividend, Divisor, Quotient, Remainder)
            !^ To perform division (Dividend / Divisor) and return both
            !  the quotient and the remainder.
            CLASS(ApInt32), INTENT(IN)  :: Dividend
            TYPE(SInt128),  INTENT(IN)  :: Divisor
            TYPE(ApInt32),  INTENT(OUT) :: Quotient
            TYPE(ApInt32),  INTENT(OUT) :: Remainder
        END SUBROUTINE ApInt32_DivMod_I128
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Over_ApInt32(This, Other, Remainder)
            !^ To perform division: This = This / Other.
            CLASS(ApInt32),          INTENT(INOUT)  :: This
            TYPE(ApInt32),           INTENT(IN)     :: Other
            TYPE(ApInt32), OPTIONAL, INTENT(OUT)    :: Remainder
        END SUBROUTINE ApInt32_Over_ApInt32
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Over_I32(This, Other, Remainder)
            !^ To perform division: This = This / Other.
            CLASS(ApInt32),     INTENT(INOUT)   :: This
            tSInt32,            INTENT(IN)      :: Other
            tSInt32,  OPTIONAL, INTENT(OUT)     :: Remainder
        END SUBROUTINE ApInt32_Over_I32
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Over_I64(This, Other, Remainder)
            !^ To perform division: This = This / Other.
            CLASS(ApInt32),    INTENT(INOUT)    :: This
            tSInt64,           INTENT(IN)       :: Other
            tSInt64, OPTIONAL, INTENT(OUT)      :: Remainder
        END SUBROUTINE ApInt32_Over_I64
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Over_I128(This, Other, Remainder)
            !^ To perform division: This = This / Other.
            CLASS(ApInt32),          INTENT(INOUT)  :: This
            TYPE(SInt128),           INTENT(IN)     :: Other
            TYPE(SInt128), OPTIONAL, INTENT(OUT)    :: Remainder
        END SUBROUTINE ApInt32_Over_I128
        !------------------------------------------------------------
        ! 'Other' operations
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_SquareSub(This)
            !^ To perform self-multiplication: This = This * This.
            CLASS(ApInt32), INTENT(INOUT)   :: This
        END SUBROUTINE ApInt32_SquareSub
        !------------------------------------------------------------
        ! auxiliary
        !------------------------------------------------------------------------------
        MODULE FUNCTION ApInt32_Copy(Source, Capacity) RESULT(Destination)
            !^ To make a copy of the ApInt32 number.
            CLASS(ApInt32),   INTENT(IN)    :: Source       !! the source number
            !> capacity (size of magnitude array) of the destination number <br>
            !  if specified, must be greater than capacity of the source number
            tIndex, OPTIONAL, INTENT(IN)    :: Capacity
            TYPE(ApInt32)                   :: Destination  !! the destination number
        END FUNCTION ApInt32_Copy
        !------------------------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_Write(Big, Unit, IOStat, IOMsg, ShowComponent, Prefix)
            !^ To write the ApInt32 number to the screen (or the specified unit).
            CLASS(ApInt32),      INTENT(IN)     :: Big
            tSInt32,   OPTIONAL, INTENT(IN)     :: Unit             !! output logical unit
            tSInt32,   OPTIONAL, INTENT(OUT)    :: IOStat           !! io stat
            tCharStar, OPTIONAL, INTENT(OUT)    :: IOMsg            !! io message
            tLogical,  OPTIONAL, INTENT(IN)     :: ShowComponent
            !^ flag indicating whether to write the upper and lower components. <br>
            ! - If flag is present and true, write components of the object. <br>
            ! - Otherwise, write the object as a decimal string.
            tCharStar, OPTIONAL, INTENT(IN)     :: Prefix           !! prefix string
        END SUBROUTINE ApInt32_Write
        !------------------------------------------------------------------------------
        MODULE FUNCTION ZeroApInt32() RESULT(Big)
            !^ To return the ApInt32 number with value of zero.
            TYPE(ApInt32)       :: Big
        END FUNCTION ZeroApInt32
        !------------------------------------------------------------------------------
        MODULE FUNCTION OneApInt32() RESULT(Big)
            !^ To return the ApInt32 number with value of one.
            TYPE(ApInt32)       :: Big
        END FUNCTION OneApInt32
        !------------------------------------------------------------------------------
        MODULE FUNCTION ApInt32_Is_Zero_II(Big) RESULT(Flag)
            !^ To check whether the ApInt32 number has value of zero or not.
            CLASS(ApInt32), INTENT(INOUT)   :: Big
            tLogical                        :: Flag
        END FUNCTION ApInt32_Is_Zero_II
        !------------------------------------------------------------------------------
        MODULE FUNCTION ApInt32_Is_One(Big) RESULT(Flag)
            !^ To check whether the ApInt32 number has value of one or not.
            CLASS(ApInt32), INTENT(IN)  :: Big
            tLogical                    :: Flag
        END FUNCTION ApInt32_Is_One
        !------------------------------------------------------------------------------
        MODULE FUNCTION ApInt32_Is_Positive(Big) RESULT(Flag)
            !^ To check whether the ApInt32 number has positive value or not.
            CLASS(ApInt32), INTENT(IN)  :: Big
            tLogical                    :: Flag
        END FUNCTION ApInt32_Is_Positive
        !------------------------------------------------------------------------------
        MODULE FUNCTION ApInt32_Is_Negative(Big) RESULT(Flag)
            !^ To check whether the ApInt32 number has negative value or not.
            CLASS(ApInt32), INTENT(IN)  :: Big
            tLogical                    :: Flag
        END FUNCTION ApInt32_Is_Negative
        !------------------------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_RandNumSub(BigRnd, Prng, Positive, Length)
            !^ To generate the ApIn64 number with random value.
            CLASS(ApInt32),                   INTENT(OUT)   :: BigRnd
            !! the ApInt32 number with random value
            CLASS(BaseRNG), OPTIONAL, TARGET, INTENT(INOUT) :: Prng
            !! pseudo-random number generator
            tLogical,       OPTIONAL,         INTENT(IN)    :: Positive
            !! flag indicating whether the number has positive value or not
            tIndex,         OPTIONAL,         INTENT(IN)    :: Length
            !! number indicating the length of magnitude array
        END SUBROUTINE ApInt32_RandNumSub
        !------------------------------------------------------------------------------
        MODULE FUNCTION ApInt32_GetLength(Num) RESULT(Length)
            !^ To return the number of digits counted as the ApNum number.
            CLASS(ApInt32), INTENT(IN)  :: Num
            tIndex                      :: Length   !! the number of digits counted
        END FUNCTION ApInt32_GetLength
        !------------------------------------------------------------
    END INTERFACE

!** GENERIC DEFINITIONS:
    !--------------------------------------------------------------------------
    !   assignment
    !--------------------------------------------------------------------------
    INTERFACE  ASSIGNMENT(=)
        !^ **Operator Overload**: ASSIGNMENT(=) <br>
        !  **Purpose**:  To convert between the ApInt32 type and a signed integer
        !       (32-bit, 64-bit, 128-bit or arbitrary-precision integer). <br>
        !  **Usage**: <br>
        !   ! convert 64-bit signed integer to the ApInt32 type <br>
        !   --->    ApNum = I64 <br>
        !   ! convert the ApInt32 type to 128-bit signed integer <br>
        !   --->    I128 = ApNum
        MODULE SUBROUTINE ApInt32_Assign(This, Other)
            !^ To make a copy of the *ApInt32* number via an assignment expression. <br>
            !  *Usage*: This = Other
            TYPE(ApInt32), INTENT(OUT)  :: This
            TYPE(ApInt32), INTENT(IN)   :: Other
        END SUBROUTINE ApInt32_Assign
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_From_I32(Big, I32)
            !^ To convert from a 32-bit signed integer to an ApInt32 number
            !  via an assignment expression. <br>
            !  *Usage*: Big = I32
            TYPE(ApInt32), INTENT(OUT)  :: Big
            tSInt32,       INTENT(IN)   :: I32
        END SUBROUTINE ApInt32_From_I32
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_From_I64(Big, I64)
            !^ To convert from a 64-bit signed integer to an ApInt32 number
            !  via an assignment expression. <br>
            !  *Usage*: Big = I64
            TYPE(ApInt32), INTENT(OUT)  :: Big
            tSInt64,       INTENT(IN)   :: I64
        END SUBROUTINE ApInt32_From_I64
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_From_I128(Big, I128)
            !^ To convert from a 128-bit signed integer to an ApInt32 number
            !  via an assignment expression. <br>
            !  *Usage*: Big = I128
            TYPE(ApInt32), INTENT(OUT)  :: Big
            TYPE(SInt128), INTENT(IN)   :: I128
        END SUBROUTINE ApInt32_From_I128
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_To_I32(I32, Big)
            !^ To convert from an ApInt32 number to a 32-bit signed integer
            !  via an assignment expression. <br>
            !  *Usage*: I32 = Big
            tSInt32,       INTENT(OUT)  :: I32
            TYPE(ApInt32), INTENT(IN)   :: Big
        END SUBROUTINE ApInt32_To_I32
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_To_I64(I64, Big)
            !^ To convert from an ApInt32 number to a 64-bit signed integer
            !  via an assignment expression. <br>
            !  *Usage*: I64 = Big
            tSInt64,       INTENT(OUT)  :: I64
            TYPE(ApInt32), INTENT(IN)   :: Big
        END SUBROUTINE ApInt32_To_I64
        !------------------------------------------------------------
        MODULE SUBROUTINE ApInt32_To_I128(I128, Big)
            !^ To convert from an ApInt32 number to a 128-bit signed integer
            !  via an assignment expression. <br>
            !  *Usage*: I128 = Big
            TYPE(SInt128), INTENT(OUT)  :: I128
            TYPE(ApInt32), INTENT(IN)   :: Big
        END SUBROUTINE ApInt32_To_I128
        !------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    !   constructor
    !--------------------------------------------------------------------------
    INTERFACE  ApInt32
        !^ **Constructor Interface**: ApInt32 <br>
        !  **Purpose**:  To construct an ApInt32 number. <br>
        !  **Usage**: <br>
        !   ! construct ApNum from a magnitude array <br>
        !   --->    ApNum = ApInt32(Sign, MagLen, MagArray) <br>
        !   ! construct ApNum from 64-bit intrinsic integer treated as signed <br>
        !   --->    ApNum = ApInt32(I64) <br>
        !   ! construct ApNum from 32-bit intrinsic integer treated as unsigned <br>
        !   --->    ApNum = ApInt32(Sign, U32) <br>
        !   ! construct ApNum from 128-bit real number <br>
        !   --->    ApNum = ApInt32(R128) <br>
        !   ! construct ApNum from a decimal string <br>
        !   --->    ApNum = ApInt32('-123456789098765432101122334455667788990012345')
        MODULE FUNCTION Construct_ApInt32(Sign, Length, Digit) RESULT(Big)
            !^ To construct the *ApInt32* number based on the specified 32-bit integer
            !  magnitude array and related data. <br>
            !  *Usage*: ApNum = ApInt32(Sign, MagLen, MagArray)
            tSInt32,  INTENT(IN)    :: Sign     !! sign of the number
            tIndex,   INTENT(IN)    :: Length   !! length of the magnitude array
            !> the magnitude of the number given as a 32-bit integer array where
            !  the first element gives the least significant 32 bits (i.e. little
            !  endian order)
            tUInt32,  INTENT(IN)    :: Digit(0:Length-1)
            TYPE(ApInt32)           :: Big      !! the arbitrary-precision integer number
        END FUNCTION Construct_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION Bytes_To_ApInt32(Sign, Length, Digit) RESULT(Big)
            !^ To construct the *ApInt32* number based on the specified 8-bit integer
            !  magnitude array and related data. <br>
            !  *Usage*: ApNum = ApInt32(Sign, MagLen, MagArray)
            tSInt32,  INTENT(IN)    :: Sign     !! sign of the number
            tIndex,   INTENT(IN)    :: Length   !! length of the magnitude array
            !> the magnitude of the number given as a 8-bit integer array where
            !  the first element gives the least significant 8 bits (i.e. little
            !  endian order)
            tSInt8,   INTENT(IN)    :: Digit(0:Length-1)
            TYPE(ApInt32)           :: Big      !! the arbitrary-precision integer number
        END FUNCTION Bytes_To_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION DecString_To_ApInt32(cStr, ErrFlag, ErrMsg) RESULT(Big)
            !^ To construct the *ApInt32* number based on the specified decimal string. <br>
            !  *Usage*:  <br>
            !  ---> ApNum = ApInt32('1234567890987654321011223344') <br>
            !  ---> ApNum = ApInt32('-987654321012345678900123123', ErrFlag) <br>
            !  ---> ApNum = ApInt32(NumStr, ErrMsg=Message) <br>
            !  ---> ApNum = ApInt32(NumStr, ErrFlag, ErrMsg)
            tCharStar,  TARGET,   INTENT(IN)    :: cStr     !! character string
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  !! true if input is not invalid
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   !! message if input is not invalid
            TYPE(ApInt32)                       :: Big      !! number
        END FUNCTION DecString_To_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION U32_To_ApInt32(Sign, U32) RESULT(Big)
            !^ To construct the *ApInt32* number based on the specified 32-bit unsigned integer. <br>
            !  *Usage*: ApNum = ApInt32(Sign, U32)
            tSInt32,  INTENT(IN)    :: Sign     !! the sign of the number
            tUInt32,  INTENT(IN)    :: U32      !! the magnitude of the number treated as unsigned
            TYPE(ApInt32)           :: Big
        END FUNCTION U32_To_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION U64_To_ApInt32(Sign, U64) RESULT(Big)
            !^ To construct the *ApInt32* number based on the specified 64-bit unsigned integer. <br>
            !  *Usage*: ApNum = ApInt32(Sign, U64)
            tSInt32,  INTENT(IN)    :: Sign     !! the sign of the number
            tUInt64,  INTENT(IN)    :: U64      !! the magnitude of the number treated as unsigned
            TYPE(ApInt32)           :: Big
        END FUNCTION U64_To_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION U128_To_ApInt32(Sign, U128) RESULT(Big)
            !^ To construct the *ApInt32* number based on the specified 128-bit unsigned integer. <br>
            !  *Usage*: ApNum = ApInt32(Sign, U128)
            tSInt32,       INTENT(IN)   :: Sign !! the sign of the number
            TYPE(UInt128), INTENT(IN)   :: U128 !! the magnitude of the number treated as unsigned
            TYPE(ApInt32)               :: Big
        END FUNCTION U128_To_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION I32_To_ApInt32(I32) RESULT(Big)
            !^ To construct the *ApInt32* number based on the specified 32-bit signed integer. <br>
            !  *Usage*: ApNum = ApInt32(I32)
            tSInt32,  INTENT(IN)    :: I32  !! the 32-bit signed integer
            TYPE(ApInt32)           :: Big
        END FUNCTION I32_To_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION I64_To_ApInt32(I64) RESULT(Big)
            !^ To construct the *ApInt32* number based on the specified 64-bit signed integer. <br>
            !  *Usage*: ApNum = ApInt32(I64)
            tSInt64, INTENT(IN) :: I64  !! the 64-bit signed integer
            TYPE(ApInt32)       :: Big
        END FUNCTION I64_To_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION I128_To_ApInt32(I128) RESULT(Big)
            !^ To construct the *ApInt32* number based on the specified 128-bit signed integer. <br>
            !  *Usage*: ApNum = ApInt32(I128)
            TYPE(SInt128), INTENT(IN)   :: I128 !! the 128-bit signed integer
            TYPE(ApInt32)               :: Big
        END FUNCTION I128_To_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION R32_To_ApInt32(R32) RESULT(Big)
            !^ To construct the *ApInt32* number based on the specified 32-bit real number. <br>
            !  *Usage*: ApNum = ApInt32(R32)
            tRealSP, INTENT(IN) :: R32  !! the 32-bit real number
            TYPE(ApInt32)       :: Big
        END FUNCTION R32_To_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION R64_To_ApInt32(R64) RESULT(Big)
            !^ To construct the *ApInt32* number based on the specified 64-bit real number. <br>
            !  *Usage*: ApNum = ApInt32(R64)
            tRealDP, INTENT(IN) :: R64  !! the 64-bit real number
            TYPE(ApInt32)       :: Big
        END FUNCTION R64_To_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION R128_To_ApInt32(R128) RESULT(Big)
            !^ To construct the *ApInt32* number based on the specified 128-bit real number. <br>
            !  *Usage*: ApNum = ApInt32(R128)
            tRealQP, INTENT(IN) :: R128 !! the 128-bit real number
            TYPE(ApInt32)       :: Big
        END FUNCTION R128_To_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    !   conversion
    !--------------------------------------------------------------------------
    INTERFACE  ToBytes
        !^ **Subroutine Interface**: ToBytes <br>
        !  **Purpose**:  To convert an ApInt32 number to a 8-bit integer magnitude
        !   array and its sign. <br>
        !  **Usage**: <br>
        !   ---> CALL ToBytes(ApNum, MagArray, Sign)
       MODULE SUBROUTINE Bytes_From_ApInt32(Big, Digit, Sign)
            !^ To convert an ApInt32 number to a 8-bit integer magnitude array and its sign.
            TYPE(ApInt32),       INTENT(IN)     :: Big
            !> the magnitude of the number given as a 8-bit integer array where
            !  the first element gives the least significant 8 bits (i.e. little
            !  endian order)
            tSInt8, ALLOCATABLE, INTENT(OUT)    :: Digit(:)
            tSInt32,   OPTIONAL, INTENT(OUT)    :: Sign     !! sign of the number
        END SUBROUTINE Bytes_From_ApInt32
    END INTERFACE
    INTERFACE  ToU32
        !^ **Function Interface**: ToU32 <br>
        !  **Purpose**:  To convert an arbitrary-precision signed integer to a
        !   32-bit unsigned integer. <br>
        !  **Usage**: <br>
        !   --->    U32 = ToU32(ApNum)
        MODULE FUNCTION U32_From_ApInt32(Big) RESULT(U32)
            !^ To convert the ApInt32 number to a 32-bit unsigned integer.
            TYPE(ApInt32), INTENT(IN)   :: Big
            tUInt32                     :: U32  !! a 32-bit integer treated as unsigned
        END FUNCTION U32_From_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  ToU64
        !^ **Function Interface**: ToU64 <br>
        !  **Purpose**:  To convert an arbitrary-precision signed integer to a
        !   64-bit unsigned integer. <br>
        !  **Usage**: <br>
        !   --->    U64 = ToU64(ApNum)
        MODULE FUNCTION U64_From_ApInt32(Big) RESULT(U64)
            !^ To convert the ApInt32 number to a 64-bit unsigned integer.
            TYPE(ApInt32), INTENT(IN)   :: Big
            tUInt64                     :: U64  !! a 64-bit integer treated as unsigned
        END FUNCTION U64_From_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  ToU128
        !^ **Function Interface**: ToU128 <br>
        !  **Purpose**:  To convert an arbitrary-precision signed integer to a
        !   128-bit unsigned integer. <br>
        !  **Usage**: <br>
        !   --->    U128 = ToU128(ApNum)
        MODULE FUNCTION U128_From_ApInt32(Big) RESULT(U128)
            !^ To convert the ApInt32 number to a 128-bit unsigned integer.
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(UInt128)               :: U128 !! a 128-bit unsigned integer
        END FUNCTION U128_From_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  ToI32
        !^ **Function Interface**: ToI32 <br>
        !  **Purpose**:  To convert an arbitrary-precision signed integer to a
        !   32-bit signed integer. <br>
        !  **Usage**: <br>
        !   --->    I32 = ToI32(ApNum)
        MODULE FUNCTION I32_From_ApInt32(Big) RESULT(I32)
            !^ To convert the ApInt32 number to a 32-bit signed integer.
            TYPE(ApInt32), INTENT(IN)   :: Big
            tSInt32                     :: I32  !! a 32-bit integer treated as signed
        END FUNCTION I32_From_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  ToI64
        !^ **Function Interface**: ToI64 <br>
        !  **Purpose**:  To convert an arbitrary-precision signed integer to a
        !   64-bit signed integer. <br>
        !  **Usage**: <br>
        !   --->    I64 = ToI64(ApNum)
        MODULE FUNCTION I64_From_ApInt32(Big) RESULT(I64)
            !^ To convert the ApInt32 number to a 64-bit signed integer.
            TYPE(ApInt32), INTENT(IN)   :: Big
            tSInt64                     :: I64  !! a 64-bit integer treated as signed
        END FUNCTION I64_From_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  ToI128
        !^ **Function Interface**: ToI128 <br>
        !  **Purpose**:  To convert an arbitrary-precision signed integer to a
        !   128-bit signed integer. <br>
        !  **Usage**: <br>
        !   --->    I128 = ToI128(ApNum)
        MODULE FUNCTION I128_From_ApInt32(Big) RESULT(I128)
            !^ To convert the ApInt32 number to a 128-bit signed integer.
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(SInt128)               :: I128 !! a 128-bit signed integer
        END FUNCTION I128_From_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  ToR32
        !^ **Function Interface**: ToR32 <br>
        !  **Purpose**:  To convert an arbitrary-precision signed integer to a
        !   32-bit real number. <br>
        !  **Usage**: <br>
        !   --->    R32 = ToR32(ApNum)
        MODULE FUNCTION R32_From_ApInt32(Big) RESULT(R32)
            !^ To convert the ApInt32 number to a 32-bit real number.
            TYPE(ApInt32), INTENT(IN)   :: Big
            tRealSP                     :: R32
        END FUNCTION R32_From_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  ToR64
        !^ **Function Interface**: ToR64 <br>
        !  **Purpose**:  To convert an arbitrary-precision signed integer to a
        !   64-bit real number. <br>
        !  **Usage**: <br>
        !   --->    R64 = ToR64(ApNum)
        MODULE FUNCTION R64_From_ApInt32(Big) RESULT(R64)
            !^ To convert the ApInt32 number to a 64-bit real number.
            TYPE(ApInt32), INTENT(IN)   :: Big
            tRealDP                     :: R64
        END FUNCTION R64_From_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  ToR128
        !^ **Function Interface**: ToR128 <br>
        !  **Purpose**:  To convert an arbitrary-precision signed integer to a
        !   128-bit real number. <br>
        !  **Usage**: <br>
        !   --->    R128 = ToR128(ApNum)
        MODULE FUNCTION R128_From_ApInt32(Big) RESULT(R128)
            !^ To convert the ApInt32 number to a 128-bit real number.
            TYPE(ApInt32), INTENT(IN)   :: Big
            tRealQP                     :: R128
        END FUNCTION R128_From_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  ToDecString
        !^ **Function Interface**: ToDecString <br>
        !  **Purpose**:  To convert an arbitrary-precision signed integer to
        !   a decimal string. <br>
        !  **Usage**: <br>
        !   --->    Str = ToDecString(ApNum)
        MODULE FUNCTION DecString_From_ApInt32(Big) RESULT(Str)
            !^ To convert an arbitrary-precision signed integer to a decimal string.
            TYPE(ApInt32), INTENT(IN)   :: Big
            tCharAlloc                  :: Str
        END FUNCTION DecString_From_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    !   comparison
    !--------------------------------------------------------------------------
    INTERFACE  OPERATOR(==)
        !^ **Operator Overload**: OPERATOR(==) <br>
        !  **Purpose**:  To check if values of two ApInt32 numbers are equal.
        !   Return .TRUE. if both values are equal; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS == RHS <br>
        !   --->    IF (LHS .EQ. RHS) DoSomething
        MODULE FUNCTION ApInt32_Equal(LHS, RHS) RESULT(Flag)
            !^ To check whether two ApInt32 numbers are equal.
            TYPE(ApInt32), INTENT(IN)   :: LHS
            TYPE(ApInt32), INTENT(IN)   :: RHS
            tLogical                    :: Flag
        END FUNCTION ApInt32_Equal
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  OPERATOR(/=)
        !^ **Operator Overload**: OPERATOR(/=) <br>
        !  **Purpose**:  To check if values of two ApInt32 numbers are NOT equal.
        !   Return .TRUE. if both values are NOT equal; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS /= RHS <br>
        !   --->    IF (LHS .NE. RHS) DoSomething
        MODULE FUNCTION ApInt32_NotEqual(LHS, RHS) RESULT(Flag)
            !^ To check whether two ApInt32 numbers NOT are equal.
            TYPE(ApInt32), INTENT(IN)   :: LHS
            TYPE(ApInt32), INTENT(IN)   :: RHS
            tLogical                    :: Flag
        END FUNCTION ApInt32_NotEqual
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  OPERATOR(<)
        !^ **Operator Overload**: OPERATOR(<) <br>
        !  **Purpose**:  To check if the LHS value is less than the RHS value.
        !   Return .TRUE. if LHS < RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS < RHS <br>
        !   --->    IF (LHS .LT. RHS) DoSomething
        MODULE FUNCTION ApInt32_LessThan(LHS, RHS) RESULT(Flag)
            !^ To check whether the LHS number is less than the RHS number.
            TYPE(ApInt32), INTENT(IN)   :: LHS
            TYPE(ApInt32), INTENT(IN)   :: RHS
            tLogical                    :: Flag
        END FUNCTION ApInt32_LessThan
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  OPERATOR(<=)
        !^ **Operator Overload**: OPERATOR(<=) <br>
        !  **Purpose**:  To check if the LHS value is less than or equal to the RHS value.
        !   Return .TRUE. if LHS <= RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS <= RHS <br>
        !   --->    IF (LHS .LE. RHS) DoSomething
        MODULE FUNCTION ApInt32_LessEqual(LHS, RHS) RESULT(Flag)
            !^ To check whether the LHS number is less than or equal to the RHS number.
            TYPE(ApInt32), INTENT(IN)   :: LHS
            TYPE(ApInt32), INTENT(IN)   :: RHS
            tLogical                    :: Flag
        END FUNCTION ApInt32_LessEqual
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  OPERATOR(>)
        !^ **Operator Overload**: OPERATOR(>) <br>
        !  **Purpose**:  To check if the LHS value is greater than the RHS value.
        !   Return .TRUE. if LHS > RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS > RHS <br>
        !   --->    IF (LHS .GT. RHS) DoSomething
        MODULE FUNCTION ApInt32_GreaterThan(LHS, RHS) RESULT(Flag)
            !^ To check whether the LHS number is greater than the RHS number.
            TYPE(ApInt32), INTENT(IN)   :: LHS
            TYPE(ApInt32), INTENT(IN)   :: RHS
            tLogical                    :: Flag
        END FUNCTION ApInt32_GreaterThan
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  OPERATOR(>=)
        !^ **Operator Overload**: OPERATOR(>=) <br>
        !  **Purpose**:  To check if the LHS value is greater than or equal to the RHS value.
        !   Return .TRUE. if LHS >= RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS >= RHS <br>
        !   --->    IF (LHS .GE. RHS) DoSomething
        MODULE FUNCTION ApInt32_GreaterEqual(LHS, RHS) RESULT(Flag)
            !^ To check whether the LHS number is greater than or equal to the RHS number.
            TYPE(ApInt32), INTENT(IN)   :: LHS
            TYPE(ApInt32), INTENT(IN)   :: RHS
            tLogical                    :: Flag
        END FUNCTION ApInt32_GreaterEqual
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  Compare
        !^ **Function Interface**: Compare <br>
        !  **Purpose**:  To compare two ApInt32 numbers and return <br>
        !   -1 if LHS < RHS, <br>
        !    0 if LHS == RHS, or <br>
        !    1 if LHS > RHS. <br>
        !  **Usage**: <br>
        !   --->    Flag = Compare(LHS, RHS) <br>
        !   --->    IF (Compare(LHS, RHS) /= 0) DoSomething
        MODULE PROCEDURE ApInt32_Compare
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  CompareAbs
        !^ **Function Interface**: CompareAbs <br>
        !  **Purpose**:  To compare the absolute values of two ApInt32 numbers and return <br>
        !   -1 if ABS(LHS) <  ABS(RHS), <br>
        !    0 if ABS(LHS) == ABS(RHS), or <br>
        !    1 if ABS(LHS) >  ABS(RHS). <br>
        !  **Usage**: <br>
        !   --->    Flag = CompareAbs(LHS, RHS) <br>
        !   --->    IF (CompareAbs(LHS, RHS) /= 0) DoSomething
        MODULE FUNCTION ApInt32_CompareAbs(LHS, RHS) RESULT(Flag)
            !^ To compare the absolute value of LHS and RHS. <br>
            ! - Return -1 if ABS(LHS) <  ABS(RHS). <br>
            ! - Return  0 if ABS(LHS) == ABS(RHS). <br>
            ! - Return +1 if ABS(LHS) >  ABS(RHS).
            TYPE(ApInt32), INTENT(IN)   :: LHS
            TYPE(ApInt32), INTENT(IN)   :: RHS
            tSInt32                     :: Flag
        END FUNCTION ApInt32_CompareAbs
        !------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    !   arithmetic
    !--------------------------------------------------------------------------
    INTERFACE  OPERATOR(+)
        !^ **Operator Overload**: OPERATOR(+) <br>
        !  **Purpose**:  To perform a summation of two signed integers (at least one
        !                of which is a ApInt32 number) or to add a unary plus sign
        !                to a ApInt32 number. <br>
        !  **Usage**: <br>
        !   --->    OutNum = +InNum <br>
        !   --->    OutNum = InNum1 + InNum2
        MODULE FUNCTION ApInt32_UnaryPlus(InVal) RESULT(OutVal)
            !^ To return result of the unary plus sign of the ApInt32 number.
            TYPE(ApInt32), INTENT(IN)   :: InVal
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_UnaryPlus
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Plus_ApInt32(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform addition: OutVal = LhsVal + RhsVal.
            TYPE(ApInt32), INTENT(IN)   :: LhsVal
            TYPE(ApInt32), INTENT(IN)   :: RhsVal
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Plus_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Plus_I32(Big, I32) RESULT(OutVal)
            !^ To perform addition: OutVal = Big + I32.
            TYPE(ApInt32), INTENT(IN)   :: Big
            tSInt32,       INTENT(IN)   :: I32
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Plus_I32
        !------------------------------------------------------------
        MODULE FUNCTION I32_Plus_ApInt32(I32, Big) RESULT(OutVal)
            !^ To perform addition: OutVal = I32 + Big.
            tSInt32,       INTENT(IN)   :: I32
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(ApInt32)               :: OutVal
        END FUNCTION I32_Plus_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Plus_I64(Big, I64) RESULT(OutVal)
            !^ To perform addition: OutVal = Big + I64.
            TYPE(ApInt32), INTENT(IN)   :: Big
            tSInt64,       INTENT(IN)   :: I64
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Plus_I64
        !------------------------------------------------------------
        MODULE FUNCTION I64_Plus_ApInt32(I64, Big) RESULT(OutVal)
            !^ To perform addition: OutVal = I64 + Big.
            tSInt64,       INTENT(IN)   :: I64
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(ApInt32)               :: OutVal
        END FUNCTION I64_Plus_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Plus_I128(Big, I128) RESULT(OutVal)
            !^ To perform addition: OutVal = Big + I128.
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(SInt128), INTENT(IN)   :: I128
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Plus_I128
        !------------------------------------------------------------
        MODULE FUNCTION I128_Plus_ApInt32(I128, Big) RESULT(OutVal)
            !^ To perform addition: OutVal = I128 + Big.
            TYPE(SInt128), INTENT(IN)   :: I128
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(ApInt32)               :: OutVal
        END FUNCTION I128_Plus_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  OPERATOR(-)
        !^ **Operator Overload**: OPERATOR(-) <br>
        !  **Purpose**:  To perform a subtraction of two signed integers (at least one
        !                of which is a ApInt32 number) or to negate a ApInt32 number. <br>
        !  **Usage**: <br>
        !   --->    OutNum = -InNum <br>
        !   --->    OutNum = InNum1 - InNum2
        MODULE FUNCTION ApInt32_Negate(InVal) RESULT(OutVal)
            !^ To negate a ApInt32 number.
            TYPE(ApInt32), INTENT(IN)   :: InVal
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Negate
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Minus_ApInt32(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform subtraction: OutVal = LhsVal - RhsVal
            TYPE(ApInt32), INTENT(IN)   :: LhsVal
            TYPE(ApInt32), INTENT(IN)   :: RhsVal
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Minus_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Minus_I32(Big, I32) RESULT(OutVal)
            !^ To perform subtraction: OutVal = Big - I32
            TYPE(ApInt32), INTENT(IN)   :: Big
            tSInt32,       INTENT(IN)   :: I32
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Minus_I32
        !------------------------------------------------------------
        MODULE FUNCTION I32_Minus_ApInt32(I32, Big) RESULT(OutVal)
            !^ To perform subtraction: OutVal = I32 - Big
            tSInt32,       INTENT(IN)   :: I32
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(ApInt32)               :: OutVal
        END FUNCTION I32_Minus_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Minus_I64(Big, I64) RESULT(OutVal)
            !^ To perform subtraction: OutVal = Big - I64
            TYPE(ApInt32), INTENT(IN)   :: Big
            tSInt64,       INTENT(IN)   :: I64
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Minus_I64
        !------------------------------------------------------------
        MODULE FUNCTION I64_Minus_ApInt32(I64, Big) RESULT(OutVal)
            !^ To perform subtraction: OutVal = I64 - Big
            tSInt64,       INTENT(IN)   :: I64
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(ApInt32)               :: OutVal
        END FUNCTION I64_Minus_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Minus_I128(Big, I128) RESULT(OutVal)
            !^ To perform subtraction: OutVal = Big - I128
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(SInt128), INTENT(IN)   :: I128
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Minus_I128
        !------------------------------------------------------------
        MODULE FUNCTION I128_Minus_ApInt32(I128, Big) RESULT(OutVal)
            !^ To perform subtraction: OutVal = I128 - Big
            TYPE(SInt128), INTENT(IN)   :: I128
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(ApInt32)               :: OutVal
        END FUNCTION I128_Minus_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  OPERATOR(*)
        !^ **Operator Overload**: OPERATOR( * ) <br>
        !  **Purpose**:  To perform a multiplication of two signed integers (at least one
        !                of which is a ApInt32 number). <br>
        !  **Usage**: <br>
        !   --->    OutNum = InNum1 * InNum2
        MODULE FUNCTION ApInt32_Multiply_ApInt32(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform multiplication: OutVal = LhsVal * RhsVal
            TYPE(ApInt32), INTENT(IN)   :: LhsVal
            TYPE(ApInt32), INTENT(IN)   :: RhsVal
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Multiply_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Multiply_I32(Big, I32) RESULT(OutVal)
            !^ To perform multiplication: OutVal = Big * I32
            TYPE(ApInt32), INTENT(IN)   :: Big
            tSInt32,       INTENT(IN)   :: I32
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Multiply_I32
        !------------------------------------------------------------
        MODULE FUNCTION I32_Multiply_ApInt32(I32, Big) RESULT(OutVal)
            !^ To perform multiplication: OutVal = I32 * Big
            tSInt32,       INTENT(IN)   :: I32
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(ApInt32)               :: OutVal
        END FUNCTION I32_Multiply_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Multiply_I64(Big, I64) RESULT(OutVal)
            !^ To perform multiplication: OutVal = Big * I64
            TYPE(ApInt32), INTENT(IN)   :: Big
            tSInt64,       INTENT(IN)   :: I64
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Multiply_I64
        !------------------------------------------------------------
        MODULE FUNCTION I64_Multiply_ApInt32(I64, Big) RESULT(OutVal)
            !^ To perform multiplication: OutVal = I64 * Big
            tSInt64,       INTENT(IN)   :: I64
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(ApInt32)               :: OutVal
        END FUNCTION I64_Multiply_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Multiply_I128(Big, I128) RESULT(OutVal)
            !^ To perform multiplication: OutVal = Big * I128
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(SInt128), INTENT(IN)   :: I128
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Multiply_I128
        !------------------------------------------------------------
        MODULE FUNCTION I128_Multiply_ApInt32(I128, Big) RESULT(OutVal)
            !^ To perform multiplication: OutVal = I128 * Big
            TYPE(SInt128), INTENT(IN)   :: I128
            TYPE(ApInt32), INTENT(IN)   :: Big
            TYPE(ApInt32)               :: OutVal
        END FUNCTION I128_Multiply_ApInt32
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  OPERATOR(/)
        !^ **Operator Overload**: OPERATOR(/) <br>
        !  **Purpose**:  To return the quotient of a division of two signed integers,
        !                where the dividend (numerator) is a ApInt32 number and the
        !                divisor (denominator) can be any signed integer. <br>
        !  **Usage**: <br>
        !   --->    Quotient = Dividend / Divisor
        MODULE FUNCTION ApInt32_Divide_ApInt32(Dividend, Divisor) RESULT(Quotient)
            !^ To perform division: Quotient = Dividend / Divisor.
            TYPE(ApInt32), INTENT(IN)   :: Dividend
            TYPE(ApInt32), INTENT(IN)   :: Divisor
            TYPE(ApInt32)               :: Quotient
        END FUNCTION ApInt32_Divide_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Divide_I32(Dividend, Divisor) RESULT(Quotient)
            !^ To perform division: Quotient = Dividend / Divisor.
            TYPE(ApInt32), INTENT(IN)   :: Dividend
            tSInt32,       INTENT(IN)   :: Divisor
            TYPE(ApInt32)               :: Quotient
        END FUNCTION ApInt32_Divide_I32
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Divide_I64(Dividend, Divisor) RESULT(Quotient)
            !^ To perform division: Quotient = Dividend / Divisor.
            TYPE(ApInt32), INTENT(IN)   :: Dividend
            tSInt64,       INTENT(IN)   :: Divisor
            TYPE(ApInt32)               :: Quotient
        END FUNCTION ApInt32_Divide_I64
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Divide_I128(Dividend, Divisor) RESULT(Quotient)
            !^ To perform division: Quotient = Dividend / Divisor.
            TYPE(ApInt32), INTENT(IN)   :: Dividend
            TYPE(SInt128), INTENT(IN)   :: Divisor
            TYPE(ApInt32)               :: Quotient
        END FUNCTION ApInt32_Divide_I128
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  MOD
        !^ **Function Interface**: MOD <br>
        !  **Purpose**:  To return the remainder of a division of two signed integers,
        !                where the dividend (numerator) is a ApInt32 number and the
        !                divisor (denominator) can be any signed integer. <br>
        !  **Usage**: <br>
        !   --->    Remainder = MOD(Dividend, Divisor)
        MODULE FUNCTION ApInt32_Mod_ApInt32(Dividend, Divisor) RESULT(Remainder)
            !^ To perform modulation:  Remainder = Dividend Mod Divisor
            TYPE(ApInt32), INTENT(IN)   :: Dividend
            TYPE(ApInt32), INTENT(IN)   :: Divisor
            TYPE(ApInt32)               :: Remainder
        END FUNCTION ApInt32_Mod_ApInt32
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Mod_I32(Dividend, Divisor) RESULT(Remainder)
            !^ To perform modulation:  Remainder = Dividend Mod Divisor
            TYPE(ApInt32), INTENT(IN)   :: Dividend
            tSInt32,       INTENT(IN)   :: Divisor
            TYPE(ApInt32)               :: Remainder
        END FUNCTION ApInt32_Mod_I32
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Mod_I64(Dividend, Divisor) RESULT(Remainder)
            !^ To perform modulation:  Remainder = Dividend Mod Divisor
            TYPE(ApInt32), INTENT(IN)   :: Dividend
            tSInt64,       INTENT(IN)   :: Divisor
            TYPE(ApInt32)               :: Remainder
        END FUNCTION ApInt32_Mod_I64
        !------------------------------------------------------------
        MODULE FUNCTION ApInt32_Mod_I128(Dividend, Divisor) RESULT(Remainder)
            !^ To perform modulation:  Remainder = Dividend Mod Divisor
            TYPE(ApInt32), INTENT(IN)   :: Dividend
            TYPE(SInt128), INTENT(IN)   :: Divisor
            TYPE(ApInt32)               :: Remainder
        END FUNCTION ApInt32_Mod_I128
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  MODULO
        !^ **Function Interface**: MODULO <br>
        !  **Purpose**:  To compute the modulo of two ApInt32 numbers. <br>
        !  **Usage**: <br>
        !   --->    Modulo = MODULO(Dividend, Divisor)
        MODULE FUNCTION ApInt32_Modulo(Dividend, Divisor) RESULT(Modulo)
            !^ To compute the modulo of two ApInt32 numbers.
            TYPE(ApInt32), INTENT(IN)   :: Dividend
            TYPE(ApInt32), INTENT(IN)   :: Divisor
            TYPE(ApInt32)               :: Modulo
        END FUNCTION ApInt32_Modulo
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  OPERATOR(**)
        !^ **Operator Overload**: OPERATOR( ** ) <br>
        !  **Purpose**:  To perform an exponentiation of the ApInt32 number. <br>
        !  **Usage**: <br>
        !   --->    NumOut = NumIn**Exp
        MODULE FUNCTION ApInt32_Power(BigIn, Exp) RESULT(BigOut)
            !^ To perform an exponentiation: BigOut = BigIn**Exp
            TYPE(ApInt32), INTENT(IN)   :: BigIn
            tSInt32,       INTENT(IN)   :: Exp
            TYPE(ApInt32)               :: BigOut
        END FUNCTION ApInt32_Power
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  SQR
        !^ **Function Interface**: SQR <br>
        !  **Purpose**:  To compute the square of the ApInt32 number. <br>
        !  **Usage**: <br>
        !   --->    NumSqr = SQR(ApNum)
        MODULE FUNCTION ApInt32_Square(BigIn) RESULT(BigOut)
            !^ To perform squaring: BigOut = BigIn * BigIn.
            TYPE(ApInt32), INTENT(IN)   :: BigIn
            TYPE(ApInt32)               :: BigOut
        END FUNCTION ApInt32_Square
        !------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    !   bitwise
    !--------------------------------------------------------------------------
    INTERFACE  SHIFTL
        !^ **Function Interface**: SHIFTL <br>
        !  **Purpose**:  To perform logical left shift where ShiftPos is non-negative. <br>
        !  **Usage**: <br>
        !   --->    NumOut = SHIFTL(NumIn, 157)
        MODULE FUNCTION ApInt32_ShiftLeft(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical left shift where ShiftPos is non-negative.
            TYPE(ApInt32), INTENT(IN)   :: InVal
            tSInt32,       INTENT(IN)   :: ShiftPos
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_ShiftLeft
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  SHIFTR
        !^ **Function Interface**: SHIFTR <br>
        !  **Purpose**:  To perform logical right shift where ShiftPos is non-negative. <br>
        !  **Usage**: <br>
        !   --->    NumOut = SHIFTR(NumIn, 141)
        MODULE FUNCTION ApInt32_ShiftRight(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical right shift where ShiftPos is non-negative.
            TYPE(ApInt32), INTENT(IN)   :: InVal
            tSInt32,       INTENT(IN)   :: ShiftPos
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_ShiftRight
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  ISHFT
        !^ **Function Interface**: ISHFT <br>
        !  **Purpose**:  To perform logical shift of an ApInt32 number. <br>
        !  **Usage**: <br>
        !   --->    NumOut = ISHFT(NumIn, 153)    ! a logical left shift by 153 <br>
        !   --->    NumOut = ISHFT(NumIn, -224)   ! a logical right shift by 224
        MODULE FUNCTION ApInt32_LogicalShift(InVal, ShiftPos) RESULT(OutVal)
            !^ To perform logical shift of an ApInt32 number.
            TYPE(ApInt32), INTENT(IN)   :: InVal
            tSInt32,       INTENT(IN)   :: ShiftPos
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_LogicalShift
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  NOT
        !^ **Function Interface**: NOT <br>
        !  **Purpose**:  To return the bitwise logical complement an ApInt32 number. <br>
        !  **Usage**: <br>
        !   --->    NumOut = NOT(NumIn)
        MODULE FUNCTION ApInt32_Not(InVal) RESULT(OutVal)
            !^ To return the bitwise logical complement an ApInt32 number.
            TYPE(ApInt32), INTENT(IN)   :: InVal
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Not
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  IOR
        !^ **Function Interface**: IOR <br>
        !  **Purpose**:  To perform an inclusive OR on corresponding bits of two
        !                ApInt32 numbers. <br>
        !  **Usage**: <br>
        !   --->    NumOut = IOR(NumIn1, NumIn2)
        MODULE FUNCTION ApInt32_Ior(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform an inclusive OR on corresponding bits of two ApInt32 numbers.
            TYPE(ApInt32), INTENT(IN)   :: LhsVal
            TYPE(ApInt32), INTENT(IN)   :: RhsVal
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Ior
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  IEOR
        !^ **Function Interface**: IEOR <br>
        !  **Purpose**:  To perform an exclusive OR on corresponding bits of two
        !                ApInt32 numbers. <br>
        !  **Usage**: <br>
        !   --->    NumOut = IEOR(NumIn1, NumIn2)
        MODULE FUNCTION ApInt32_Ieor(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform an exclusive OR on corresponding bits of two ApInt32 numbers.
            TYPE(ApInt32), INTENT(IN)   :: LhsVal
            TYPE(ApInt32), INTENT(IN)   :: RhsVal
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Ieor
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  IAND
        !^ **Function Interface**: IAND <br>
        !  **Purpose**:  To perform a logical AND on corresponding bits of two
        !                ApInt32 numbers. <br>
        !  **Usage**: <br>
        !   --->    NumOut = IAND(NumIn1, NumIn2)
        MODULE FUNCTION ApInt32_Iand(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform a logical AND on corresponding bits of two ApInt32 numbers.
            TYPE(ApInt32), INTENT(IN)   :: LhsVal
            TYPE(ApInt32), INTENT(IN)   :: RhsVal
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_Iand
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  IAND_NOT
        !^ **Function Interface**: IAND_NOT <br>
        !  **Purpose**:  To perform a bitwise AND and NOT on corresponding bits of two
        !                ApInt32 numbers. <br>
        !  **Usage**: <br>
        !   --->    NumOut = IAND_NOT(NumIn1, NumIn2) <br>
        !  **Note**: IAND_NOT(NumIn1, NumIn2) is equal to IAND(NumIn1, NOT(NumIn2)).
        MODULE FUNCTION ApInt32_IandNot(LhsVal, RhsVal) RESULT(OutVal)
            !^ To perform a bitwise AND and NOT on corresponding bits of two ApInt32 numbers.
            TYPE(ApInt32), INTENT(IN)   :: LhsVal
            TYPE(ApInt32), INTENT(IN)   :: RhsVal
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_IandNot
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  LEADZ
        !^ **Function Interface**: LEADZ <br>
        !  **Purpose**:  To count the number of leading zero bits of the ApInt32 number. <br>
        !  **Usage**: <br>
        !   --->    NumLZ = LEADZ(ApNum)
        MODULE FUNCTION ApInt32_LeadingZeros(Big) RESULT(NumLZ)
            !^ To count the number of leading zero bits of the ApInt32 number.
            TYPE(ApInt32), INTENT(IN)   :: Big
            tSInt32                     :: NumLZ
        END FUNCTION ApInt32_LeadingZeros
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  TRAILZ
        !^ **Function Interface**: TRAILZ <br>
        !  **Purpose**:  To count the number of trailing zero bits of the ApInt32 number. <br>
        !  **Usage**: <br>
        !   --->    NumTZ = TRAILZ(ApNum)
        MODULE FUNCTION ApInt32_TrailingZeros(Big) RESULT(NumTZ)
            !^ To count the number of trailing zero bits of the ApInt32 number.
            TYPE(ApInt32), INTENT(IN)   :: Big
            tSInt32                     :: NumTZ
        END FUNCTION ApInt32_TrailingZeros
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  POPCNT
        !^ **Function Interface**: POPCNT <br>
        !  **Purpose**:  To count the number of 1 bits in the ApInt32 number. <br>
        !  **Usage**: <br>
        !   --->    NumBits = POPCNT(ApNum)
        MODULE FUNCTION ApInt32_Count1Bits(Big) RESULT(NumBits)
            !^ To count the number of 1 bits in the ApInt32 number.  (For more information,
            !  see detailed explanation of the intrinsic function.)
            TYPE(ApInt32), INTENT(IN)   :: Big
            tSInt32                     :: NumBits
        END FUNCTION ApInt32_Count1Bits
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  POPPAR
        !^ **Function Interface**: POPPAR <br>
        !  **Purpose**:  To determine the parity of the ApInt32 number. <br>
        !  **Usage**: <br>
        !   --->    NumPar = POPPAR(ApNum)
        MODULE FUNCTION ApInt32_Parity(Big) RESULT(ParNum)
            !^ To determine the parity of the ApInt32 number.  (For more information,
            !  see detailed explanation of the intrinsic function.)
            TYPE(ApInt32), INTENT(IN)   :: Big
            tSInt32                     :: ParNum
        END FUNCTION ApInt32_Parity
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  IBSET
        !^ **Function Interface**: IBSET <br>
        !  **Purpose**:  To set the bit at the specified position to 1. <br>
        !  **Usage**: <br>
        !   --->    NumOut = IBSET(NumIn, Pos)
        MODULE FUNCTION ApInt32_SetBit(InVal, Pos) RESULT(OutVal)
            !^ To set the bit at the specified position to 1.  (For more information,
            !  see detailed explanation of the intrinsic function.)
            TYPE(ApInt32), INTENT(IN)   :: InVal
            tSInt32,       INTENT(IN)   :: Pos
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_SetBit
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  IBCLR
        !^ **Function Interface**: IBCLR <br>
        !  **Purpose**:  To set the bit at the specified position to 0. <br>
        !  **Usage**: <br>
        !   --->    NumOut = IBCLR(NumIn, Pos)
        MODULE FUNCTION ApInt32_ClearBit(InVal, Pos) RESULT(OutVal)
            !^ To set the bit at the specified position to 0.  (For more information,
            !  see detailed explanation of the intrinsic function.)
            TYPE(ApInt32), INTENT(IN)   :: InVal
            tSInt32,       INTENT(IN)   :: Pos
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_ClearBit
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  IBCHNG
        !^ **Function Interface**: IBCHNG <br>
        !  **Purpose**:  To reverse the bit at the specified position. <br>
        !  **Usage**: <br>
        !   --->    NumOut = IBCHNG(NumIn, Pos)
        MODULE FUNCTION ApInt32_FlipBit(InVal, Pos) RESULT(OutVal)
            !^ To reverse the bit at the specified position.  (For more information,
            !  see detailed explanation of the intrinsic function.)
            TYPE(ApInt32), INTENT(IN)   :: InVal
            tSInt32,       INTENT(IN)   :: Pos
            TYPE(ApInt32)               :: OutVal
        END FUNCTION ApInt32_FlipBit
        !------------------------------------------------------------
    END INTERFACE
    INTERFACE  BTEST
        !^ **Function Interface**: BTEST <br>
        !  **Purpose**:  To check whether the bit at the specified position
        !                is 0 (False) or 1 (True). <br>
        !  **Usage**: <br>
        !   --->    Flag = BTEST(ApNum, Pos)
        MODULE FUNCTION ApInt32_TestBit(Big, Pos) RESULT(Flag)
            !^ To check whether the bit at the specified position is 0 (False) or 1 (True).
            !  (For more information, see detailed explanation of the intrinsic function.)
            TYPE(ApInt32), INTENT(IN)   :: Big
            tSInt32,       INTENT(IN)   :: Pos
            tLogical                    :: Flag
        END FUNCTION ApInt32_TestBit
        !------------------------------------------------------------
    END INTERFACE
    !--------------------------------------------------------------------------
    !   miscellaneous
    !--------------------------------------------------------------------------
    INTERFACE  ABS
        !^ **Function Interface**: ABS <br>
        !  **Purpose**:  To return the absolute value of the ApInt32 number. <br>
        !  **Usage**: <br>
        !   --->    AbsNum = ABS(ApNum)
        MODULE PROCEDURE ApInt32_Absolute
    END INTERFACE
    INTERFACE  IsZero
        !^ **Function Interface**: IsZero <br>
        !  **Purpose**:  To check whether the ApInt32 number has value of zero or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsZero(ApNum) <br>
        !   --->    IF (IsZero(ApNum)) DoSomeThing
        MODULE PROCEDURE ApInt32_Is_Zero
    END INTERFACE
    INTERFACE  IsOne
        !^ **Function Interface**: IsOne <br>
        !  **Purpose**:  To check whether the ApInt32 number has value of one or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsOne(ApNum) <br>
        !   --->    IF (IsOne(ApNum)) DoSomeThing
        MODULE PROCEDURE ApInt32_Is_One
    END INTERFACE
    INTERFACE  IsPositive
        !^ **Function Interface**: IsPositive <br>
        !  **Purpose**:  To check whether the ApInt32 number has positive value or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsPositive(ApNum) <br>
        !   --->    IF (IsPositive(ApNum)) DoSomeThing
        MODULE PROCEDURE ApInt32_Is_Positive
    END INTERFACE
    INTERFACE  IsNegative
        !^ **Function Interface**: IsNegative <br>
        !  **Purpose**:  To check whether the ApInt32 number has negative value or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsNegative(ApNum) <br>
        !   --->    IF (IsNegative(ApNum)) DoSomeThing
        MODULE PROCEDURE ApInt32_Is_Negative
    END INTERFACE
    INTERFACE  RandNumApInt32
        !> **Function Interface**: RandNumApInt32 <br>
        !  **Purpose**:  To generate and return the ApIn64 number with random value. <br>
        !  **Usage**: <br>
        !   ! generate random number with default settings <br>
        !   --->    ApNum = RandNumApInt32() <br>
        !   ! generate random number with specified PRNG <br>
        !   --->    ApNum = RandNumApInt32(PRNG) <br>
        !   ! generate random number with negative value <br>
        !   --->    ApNum = RandNumApInt32(Positive=.FALSE.) <br>
        !   ! generate random number with specified length of magnitude array <br>
        !   --->    ApNum = RandNumApInt32(Length=MagLen)
        MODULE FUNCTION ApInt32_Random_Number(Prng, Positive, Length) RESULT(BigRnd)
            !^ To generate the ApIn64 number with random value.
            CLASS(BaseRNG), OPTIONAL, TARGET, INTENT(INOUT) :: Prng
            !! pseudo-random number generator
            tLogical,       OPTIONAL,         INTENT(IN)    :: Positive
            !! flag indicating whether the number has positive value or not
            tIndex,         OPTIONAL,         INTENT(IN)    :: Length
            !! number indicating the length of magnitude array
            TYPE(ApInt32)                                   :: BigRnd
            !! the ApInt32 number with random value
        END FUNCTION ApInt32_Random_Number
        !------------------------------------------------------------------------------
    END INTERFACE
    INTERFACE  Display
        !^ **Subroutine Interface**: Display <br>
        !  **Purpose**:  To write/display the 'ApInt32' number to the screen (or the specified unit). <br>
        !  **Usage**: <br>
        !   ! To display (signed) value of ApNum as a decimal string to the screen <br>
        !   --->    CALL Display(ApNum) <br>
        !   ! To display (signed) value of ApNum as a decimal string to the output logical unit <br>
        !   --->    CALL Display(ApNum, 11) <br>
        !   ! To display (signed) value of ApNum as a decimal string to the output logical unit <br>
        !   with input/output status and message <br>
        !   --->    CALL Display(ApNum, 11, IOStat, IOMsg) <br>
        !   ! To display (signed) values of components of ApNum as a decimal string to the screen <br>
        !   --->    CALL Display(ApNum, ShowComponent=.TRUE.) <br>
        !   ! To display (signed) value of ApNum as a decimal string to the screen with a prefix string <br>
        !   --->    CALL Display(ApNum, Prefix='Signed value of ApNum')
        MODULE PROCEDURE ApInt32_Write
    END INTERFACE
    INTERFACE  MakeCopy
        !^ **Function Interface**: Copy <br>
        !  **Purpose**:  To make a copy of the ApInt32 number. <br>
        !  **Usage**: <br>
        !   ! make a copy of the ApInt32 number <br>
        !   --->    DstApNum = MakeCopy(SrcApNum) <br>
        !   ! make a copy of the ApInt32 number with the specified capacity
        !   (size of magnitude array) of the destination number <br>
        !   --->    DstApNum = MakeCopy(SrcApNum, DstCap)
        MODULE PROCEDURE ApInt32_Copy
    END INTERFACE
    INTERFACE  GetLength
        !^ **Function Interface**: GetLength <br>
        !  **Purpose**:  To return the length of the magnitude array (the number of
        !                digits counted as the ApNum number). <br>
        !  **Usage**: <br>
        !   --->    MagLen = GetLength(ApNum)
        MODULE PROCEDURE ApInt32_GetLength
    END INTERFACE
    !--------------------------------------------------------------------------

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

FUNCTION ApInt32_Is_Zero(Big) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To check whether the number is zero or not.

! ** Technical notes**:
!   Zero can have many forms:
!   - The most common form is set through ZeroApInt32() where Digit(0) = 0 and Length = 1.
!   - If Digit has not yet been allocated or Length is less than 1, the number is also
!     considered to be zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Big
    tLogical                    :: Flag     !! true if value is zero.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (ALLOCATED(Big%Digit)) THEN
        IF (Big%Length >= 1_kIndex) THEN
            Flag = (Big%Length == 1_kIndex).AND.(Big%Digit(0) == 0)
        ELSE
            ! length is less than 1
            Flag = TrueVal
        END IF
    ELSE
        ! digit not yet allocated
        Flag = TrueVal
    END IF

    RETURN

END FUNCTION ApInt32_Is_Zero

!******************************************************************************

FUNCTION ApInt32_Absolute(Num) RESULT(Abs)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the absolute value of the specified number.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ApInt32), INTENT(IN)   :: Num
    TYPE(ApInt32)               :: Abs

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Abs = MakeCopy(Num)
    IF (Abs%Sign < 0) Abs%Sign = -Abs%Sign

    RETURN

END FUNCTION ApInt32_Absolute

!******************************************************************************

END MODULE MClass_ApInt32

!******************************************************************************
