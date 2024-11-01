
MODULE MBase_Float128

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Float128* type, its related routines and useful *Float128* parameters.
!   The *Float128* type is a derived type representing a quadruple-precision floating point number.
!   Internally, it consists of a logical value representing a sign component, two 64-bit integers
!   representing a significand component and a 32-bit integer representing an exponent component.
!   Therefore, it is slightly more precise and has a wider range than the standard IEEE-754 quadruple-
!   precision floating point number. <br>
!   Various common operations typically available for real types are provided including arithmetic,
!   comparison and conversion/construction operations.  Currently, only basic arithmetic operations
!   including addition, subtraction, multiplication and division are implemented. <br>
!  <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://github.com/m-vokhm/Quadruple">Quadruple: A 128-bit floating-point arithmetic
!       for Java</a> <br>

!** USE STATEMENTS:
    USE MBase_Common

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type + constructor
    PUBLIC :: Float128
    ! assignment + conversion
    PUBLIC :: ASSIGNMENT(=)
    PUBLIC :: ToI32, ToI64, ToR32, ToR64, ToR128
    PUBLIC :: ToDecString !, ToHexString
!    PUBLIC :: ToF128
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
    ! inquiry / getter
    PUBLIC :: Is_NaN, Is_Infinite, Is_Negative, Is_Zero
!    PUBLIC :: GetExponent, GetExpUnbiased, GetMantHi, GetMantLo
!    PUBLIC :: FloorNCeiling
    ! auxiliary
!    PUBLIC :: ABS
!    PUBLIC :: Display

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tCharStar, PARAMETER    :: ModName = 'MBase_Float128'
    ! The value of the exponent (biased) corresponding to subnormal values
    tSInt32,   PARAMETER    :: EXPONENT_OF_SUBNORMAL  = 0
    ! The value of the exponent (biased) corresponding to MIN_NORMAL
    tSInt32,   PARAMETER    :: EXPONENT_OF_MIN_NORMAL = 1
    ! The value of the exponent (biased) corresponding to 1.0 (== 2**0)
    ! equals to 2147483647(Z'7FFFFFFF').
    ! The same as EXPONENT_BIAS
    tSInt32,   PARAMETER    :: EXPONENT_OF_ONE = ToInt32(Z'7FFFFFFF')
    ! The value of the exponent (biased) corresponding to 1.0 (== 2**0)
    ! equals to 2147483647(Z'7FFFFFFF').
    ! The same as EXPONENT_OF_ONE
    tSInt32,   PARAMETER    :: EXPONENT_BIAS   = ToInt32(Z'7FFFFFFF')
    ! The value of the exponent (biased), corresponding to MAX_VALUE
    ! equals to 4294967294 (Z'FFFFFFFE)
    tSInt64,   PARAMETER    :: EXPONENT_OF_MAX_VALUE = ToInt64(Z'00000000FFFFFFFE')
    ! The value of the exponent (biased), corresponding to Infinity and NaN
    ! equals to -1 (Z'FFFFFFFF')
!    tSInt32,   PARAMETER    :: EXPONENT_OF_INFINITY  = ToInt32(Z'FFFFFFFF')
    tSInt32,   PARAMETER    :: EXPONENT_OF_INFINITY  = -1
    ! character parameter used in string conversion
    tCharStar, PARAMETER    :: ZEROS = '0000000000000000000000000000000000000000'
    ! Just for convenience: Z'8000_0000_0000_0000' (== Long.MIN_VALUE)
    tSInt64,   PARAMETER    :: HIGH_BIT           = ToInt64(Z'8000000000000000')
    ! Just for convenience: Z'8000_0000_0000_0000'
    tSInt64,   PARAMETER    :: BIT_63             = HIGH_BIT

    ! Just for convenience: Z'0000_0000_0000_7FFF'
    tSInt64,   PARAMETER    :: LOWER_15_BITS      = ToInt64(Z'0000000000007FFF')
    ! Just for convenience: Z'0000_0000_FFFF_FFFF'
    tSInt64,   PARAMETER    :: LOWER_32_BITS      = ToInt64(Z'00000000FFFFFFFF')
    ! Just for convenience: Z'0000_FFFF_FFFF_FFFF'
    tSInt64,   PARAMETER    :: LOWER_48_BITS      = ToInt64(Z'0000FFFFFFFFFFFF')
    ! Just for convenience: Z'FFFF_FFFF_0000_0000'
    tSInt64,   PARAMETER    :: HIGHER_32_BITS     = ToInt64(Z'FFFFFFFF00000000')
    ! Just for convenience: Z'8000_0000' // 2^31
    tSInt64,   PARAMETER    :: POW_2_31_L         = ToInt64(Z'0000000080000000') ! 2^31
    ! Inner structure of double: where it holds its sign
    tSInt64,   PARAMETER    :: DOUBLE_SIGN_MASK   = HIGH_BIT
    ! Inner structure of double: where it holds its exponent
    tSInt64,   PARAMETER    :: DOUBLE_EXP_MASK    = ToInt64(Z'7FF0000000000000')
    ! Inner structure of double: where it holds its mantissa
    tSInt64,   PARAMETER    :: DOUBLE_MANT_MASK   = ToInt64(Z'000FFFFFFFFFFFFF')

    ! double's exponent value corresponding to 2^0 = 1, shifted to lower bits
    tSInt32,   PARAMETER    :: EXP_0D             = ToInt32(Z'000003FF')

    ! The highest bit of Quad's mantissa that doesn't fit in double's mantissa (is lower than the lowest)
    tSInt64,   PARAMETER    :: HALF_DOUBLES_LSB   = ToInt64(Z'0000000000000800')
    ! The implied position of the implied unity in double
    tSInt64,   PARAMETER    :: DOUBLE_IMPLIED_MSB = ToInt64(Z'0010000000000000')

    ! Max value of the decimal exponent, corresponds to EXPONENT_OF_MAX_VALUE
    tSInt32,   PARAMETER    :: MAX_EXP10 = 646456993
    ! Min value of the decimal exponent, corresponds to EXPONENT_OF_MIN_NORMAL
    tSInt32,   PARAMETER    :: MIN_EXP10 = -646457032   ! corresponds

    tSInt32,   PARAMETER    :: IEEE754_EXPONENT_BIAS       = 16383  ! Z'3FFF'
    tSInt32,   PARAMETER    :: IEEE754_MAX_EXPONENT        = IEEE754_EXPONENT_BIAS
    tSInt32,   PARAMETER    :: IEEE754_MIN_NORMAL_EXPONENT = -16382     ! Z'FFFF_C002'
    tSInt32,   PARAMETER    :: IEEE754_MIN_EXPONENT        = IEEE754_MIN_NORMAL_EXPONENT - 112

    tSInt64,   PARAMETER    :: IEEE754_MINUS_ZERO_LEAD     = ToInt64(Z'8000000000000000')
    tSInt64,   PARAMETER    :: IEEE754_NAN_LEAD            = ToInt64(Z'7FFF800000000000')
    tSInt64,   PARAMETER    :: IEEE754_MINUS_INFINITY_LEAD = ToInt64(Z'FFFF000000000000')
    tSInt64,   PARAMETER    :: IEEE754_INFINITY_LEAD       = ToInt64(Z'7FFF000000000000')
    tSInt64,   PARAMETER    :: IEEE754_EXPONENT_MASK       = ToInt64(Z'7FFF000000000000')

    ! Approximate value of log2(10)
    tRealDP,   PARAMETER    :: LOG2_10 = LOG(10.0_kDouble) / LOG(2.0_kDouble)
    ! Approximate value of log2(e)
    tRealDP,   PARAMETER    :: LOG2_E  = 1.0_kDouble/LOG(2.0_kDouble)

    ! The maximum number of digits in the mantissa that are taken into account
    ! 2^192 = 6.277e57, so the 58-th digit after point may affect the result
    tSInt32,   PARAMETER    :: MAX_MANTISSA_LENGTH = 59

!** DERIVED TYPE DEFINITIONS
    !> *Float128* is a derived type representing a signed quadruple-precision floating-point
    !   number.  Internally, it consists of a logical variable representing its sign, two
    !   64-bit integers representing its 128-bit significand, and a 32-bit integer representing
    !   its (biased) exponent.  Its value ranges from approximately 6.67283E-646457032 to
    !   1.76161E+646456993.
    TYPE Float128
        PRIVATE
        tLogical    :: Negative !! flag indicating whether the number is negative or not
        tSInt32     :: Exponent !! number representing the biased exponent
        tSInt64     :: MantHi   !! number representing upper 64 bits of the significand
        tSInt64     :: MantLo   !! number representing lower 64 bits of the significand
    END TYPE Float128

!** MODULE PARAMETERS (PART 2):
    !% Minimum possible positive value, 6.672829482607474308148353774991346115977e-646457032
    TYPE(Float128), PUBLIC, PARAMETER   :: MIN_VALUE  = Float128(FalseVal, 0, 0_kInt64, 1_kInt64)
    !% Maximum possible value, 1.761613051683963353207493149791840285665e+646456993
    TYPE(Float128), PUBLIC, PARAMETER   :: MAX_VALUE  = Float128(FalseVal, ToInt32(EXPONENT_OF_MAX_VALUE), &
                                                                 -1_kInt64, -1_kInt64)
    !% Minimum possible positive normal value, 2.270646210401492537526567265179587581247e-646456993
    TYPE(Float128), PUBLIC, PARAMETER   :: MIN_NORMAL = Float128(FalseVal, 1, 0_kInt64, 0_kInt64)
    !% Float128 with value of 0.0
    TYPE(Float128), PUBLIC, PARAMETER   :: ZERO_F128  = Float128(FalseVal, 0, 0_kInt64, 0_kInt64)
    !% Float128 with value of 1.0
    TYPE(Float128), PUBLIC, PARAMETER   :: ONE_F128   = Float128(FalseVal, EXPONENT_OF_ONE, 0_kInt64, 0_kInt64)
    !% Float128 with value of 2.0
    TYPE(Float128), PUBLIC, PARAMETER   :: TWO_F128   = Float128(FalseVal, -2147483648, 0_kInt64, 0_kInt64)
    !% Float128 with value of 2.0
    TYPE(Float128), PUBLIC, PARAMETER   :: TEN_F128   = Float128(FalseVal, -2147483646, 4611686018427387904_kInt64, 0_kInt64)
    !% Float128 with value of -1.0
    TYPE(Float128), PUBLIC, PARAMETER   :: MINUS_ONE  = Float128(TrueVal, EXPONENT_OF_ONE, 0_kInt64, 0_kInt64)
    !% Not a number
    TYPE(Float128), PUBLIC, PARAMETER   :: NOT_A_NUMBER = Float128(FalseVal, EXPONENT_OF_INFINITY, &
                                                                   HIGH_BIT, 0_kInt64)
    !% Negative infinity
    TYPE(Float128), PUBLIC, PARAMETER   :: NEGATIVE_INFINITY  = Float128(TrueVal, EXPONENT_OF_INFINITY, &
                                                                         0_kInt64, 0_kInt64)
    !% Positive infinity
    TYPE(Float128), PUBLIC, PARAMETER   :: POSITIVE_INFINITY  = Float128(FalseVal, EXPONENT_OF_INFINITY, &
                                                                         0_kInt64, 0_kInt64)
    !% Float128 with value of Pi
    TYPE(Float128), PUBLIC, PARAMETER   :: PI_F128 = Float128(FalseVal, ToInt32(Z'80000000'), &
                                                              ToInt64(Z'921FB54442D18469'),   &
                                                              ToInt64(Z'898CC51701B839A2'))

!** INTERFACE DEFINITIONS:
    ! procedures from SBase_F128_Conversion
    INTERFACE
        ! assignment routines
        MODULE SUBROUTINE Float128_Assign(This, Other)
            TYPE(Float128), INTENT(OUT) :: This
            TYPE(Float128), INTENT(IN)  :: Other
        END SUBROUTINE Float128_Assign
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_From_R32(F128, R32)
            TYPE(Float128), INTENT(OUT) :: F128
            tRealSP,        INTENT(IN)  :: R32
        END SUBROUTINE Float128_From_R32
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_From_R64(F128, R64)
            TYPE(Float128), INTENT(OUT)  :: F128
            tRealDP,        INTENT(IN)   :: R64
        END SUBROUTINE Float128_From_R64
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_From_R128(F128, R128)
            TYPE(Float128), INTENT(OUT) :: F128
            tRealQP,          INTENT(IN)  :: R128
        END SUBROUTINE Float128_From_R128
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_To_R32(R32, F128)
            tRealSP,        INTENT(OUT) :: R32
            TYPE(Float128), INTENT(IN)  :: F128
        END SUBROUTINE Float128_To_R32
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_To_R64(R64, F128)
            tRealDP,        INTENT(OUT) :: R64
            TYPE(Float128), INTENT(IN)  :: F128
        END SUBROUTINE Float128_To_R64
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_To_R128(R128, F128)
            tRealQP,          INTENT(OUT) :: R128
            TYPE(Float128), INTENT(IN)  :: F128
        END SUBROUTINE Float128_To_R128
        !------------------------------------------------------------
        ! constructor routines
        MODULE FUNCTION Construct_Float128(Negative, Exponent, MantHi, MantLo) RESULT(F128)
            tLogical, INTENT(IN)    :: Negative
            tSInt32,  INTENT(IN)    :: Exponent
            tSInt64,  INTENT(IN)    :: MantHi
            tSInt64,  INTENT(IN)    :: MantLo
            TYPE(Float128)          :: F128
        END FUNCTION Construct_Float128
        !------------------------------------------------------------
        MODULE FUNCTION Construct_Positive_Float128(Exponent, MantHi, MantLo) RESULT(F128)
            tSInt32,  INTENT(IN)    :: Exponent
            tSInt64,  INTENT(IN)    :: MantHi
            tSInt64,  INTENT(IN)    :: MantLo
            TYPE(Float128)          :: F128
        END FUNCTION Construct_Positive_Float128
        !------------------------------------------------------------
        MODULE FUNCTION DecString_To_Float128(cStr, ErrFlag, ErrMsg) RESULT(F128)
            tCharStar,  TARGET,   INTENT(IN)    :: cStr
            tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg
            TYPE(Float128)                      :: F128
        END FUNCTION DecString_To_Float128
        !------------------------------------------------------------
        MODULE FUNCTION I32_To_Float128(I32) RESULT(F128)
            tSInt32,  INTENT(IN)    :: I32
            TYPE(Float128)          :: F128
        END FUNCTION I32_To_Float128
        !------------------------------------------------------------
        MODULE FUNCTION I64_To_Float128(I64) RESULT(F128)
            tSInt64, INTENT(IN) :: I64
            TYPE(Float128)      :: F128
        END FUNCTION I64_To_Float128
        !------------------------------------------------------------
        MODULE FUNCTION R32_To_Float128(R32) RESULT(F128)
            tRealSP, INTENT(IN) :: R32
            TYPE(Float128)      :: F128
        END FUNCTION R32_To_Float128
        !------------------------------------------------------------
        MODULE FUNCTION R64_To_Float128(R64) RESULT(F128)
            tRealDP, INTENT(IN) :: R64
            TYPE(Float128)      :: F128
        END FUNCTION R64_To_Float128
        !------------------------------------------------------------
        MODULE FUNCTION R128_To_Float128(R128) RESULT(F128)
            tRealQP, INTENT(IN)   :: R128
            TYPE(Float128)      :: F128
        END FUNCTION R128_To_Float128
        !------------------------------------------------------------
        ! conversion routines
        MODULE FUNCTION DecString_From_Float128(F128, SigDigit) RESULT(Str)
            TYPE(Float128),   INTENT(IN)    :: F128
            tIndex, OPTIONAL, INTENT(IN)    :: SigDigit
            tCharAlloc                      :: Str
        END FUNCTION DecString_From_Float128
        !------------------------------------------------------------
        MODULE FUNCTION I32_From_Float128(F128) RESULT(I32)
            TYPE(Float128), INTENT(IN)  :: F128
            tSInt32                     :: I32
        END FUNCTION I32_From_Float128
        !------------------------------------------------------------
        MODULE FUNCTION I64_From_Float128(F128) RESULT(I64)
            TYPE(Float128), INTENT(IN)  :: F128
            tSInt64                     :: I64
        END FUNCTION I64_From_Float128
        !------------------------------------------------------------
        MODULE FUNCTION R32_From_Float128(F128) RESULT(R32)
            TYPE(Float128), INTENT(IN)  :: F128
            tRealSP                     :: R32
        END FUNCTION R32_From_Float128
        !------------------------------------------------------------
        MODULE FUNCTION R64_From_Float128(F128) RESULT(R64)
            TYPE(Float128), INTENT(IN)  :: F128
            tRealDP                     :: R64
        END FUNCTION R64_From_Float128
        !------------------------------------------------------------
        MODULE FUNCTION R128_From_Float128(F128) RESULT(R128)
            TYPE(Float128), INTENT(IN)    :: F128
            tRealQP                       :: R128
        END FUNCTION R128_From_Float128
        !------------------------------------------------------------
        ! auxiliary routines
        MODULE SUBROUTINE Assign_I32_To_Float128(F128, I32)
            TYPE(Float128), INTENT(INOUT)   :: F128
            tSInt32,        INTENT(IN)      :: I32
        END SUBROUTINE Assign_I32_To_Float128
        !------------------------------------------------------------
        MODULE SUBROUTINE Assign_I64_To_Float128(F128, I64)
            TYPE(Float128), INTENT(INOUT)   :: F128
            tSInt64,        INTENT(IN)      :: I64
        END SUBROUTINE Assign_I64_To_Float128
        !------------------------------------------------------------
        MODULE SUBROUTINE Assign_R32_To_Float128(F128, R32)
            TYPE(Float128), INTENT(INOUT)   :: F128
            tRealSP,        INTENT(IN)      :: R32
        END SUBROUTINE Assign_R32_To_Float128
        !------------------------------------------------------------
        MODULE SUBROUTINE Assign_R64_To_Float128(F128, R64)
            TYPE(Float128), INTENT(INOUT)   :: F128
            tRealDP,        INTENT(IN)      :: R64
        END SUBROUTINE Assign_R64_To_Float128
        !------------------------------------------------------------
        MODULE SUBROUTINE Assign_R128_To_Float128(F128, R128)
            TYPE(Float128), INTENT(INOUT)   :: F128
            tRealQP,          INTENT(IN)      :: R128
        END SUBROUTINE Assign_R128_To_Float128
        !------------------------------------------------------------
    END INTERFACE
    ! procedures from SBase_F128_ToString
    INTERFACE
        MODULE SUBROUTINE MultMantByMinNormal(F128, Product_4x64)
            TYPE(Float128), INTENT(IN)      :: F128
            tSInt64,        INTENT(INOUT)   :: Product_4x64(0:3)
        END SUBROUTINE MultMantByMinNormal
        !------------------------------------------------------------
        MODULE SUBROUTINE MultMantByPowerOfTwo(F128, Pow2, Product_4x64)
            TYPE(Float128), INTENT(IN)      :: F128
            tSInt64,        INTENT(IN)      :: Pow2(0:3)
            tSInt64,        INTENT(INOUT)   :: Product_4x64(0:3)
        END SUBROUTINE MultMantByPowerOfTwo
        !------------------------------------------------------------
        MODULE SUBROUTINE DecimalMantToString(QDNumber, StrLen, MantStr, DigitLen)
            tSInt64,   INTENT(INOUT)    :: QDNumber(0:3)
            tIndex,    INTENT(IN)       :: StrLen
            tCharStar, INTENT(OUT)      :: MantStr
            tIndex,    INTENT(OUT)      :: DigitLen
        END SUBROUTINE DecimalMantToString
        !------------------------------------------------------------
        MODULE FUNCTION PowerOfTwo(Exp) RESULT(Power)
            tSInt64, INTENT(IN)   :: Exp
            tSInt64             :: Power(0:3)
        END FUNCTION PowerOfTwo
        !------------------------------------------------------------
        MODULE SUBROUTINE Unpack_3x64_to_6x32(PackedVal, UnpackedVal)
            tSInt64, INTENT(IN)       :: PackedVal(0:3)
            tSInt64, INTENT(INOUT)    :: UnpackedVal(0:5)
        END SUBROUTINE Unpack_3x64_to_6x32
        !------------------------------------------------------------
        MODULE SUBROUTINE MultBuffBy10(Buffer)
            tSInt64, INTENT(INOUT)    :: Buffer(0:)
        END SUBROUTINE MultBuffBy10
        !------------------------------------------------------------
        MODULE SUBROUTINE DivBuffBy10(Buffer)
            tSInt64, INTENT(INOUT)    :: Buffer(0:)
        END SUBROUTINE DivBuffBy10
        !------------------------------------------------------------
        MODULE FUNCTION AddCarry(DigitBuf, DigitLen) RESULT(Flag)
            tCharStar, INTENT(INOUT)    :: DigitBuf
            tIndex,    INTENT(IN)       :: DigitLen
            tSInt32                     :: Flag
        END FUNCTION AddCarry
        !------------------------------------------------------------
        MODULE FUNCTION IsEmpty(Buffer) RESULT(Flag)
            tSInt64, INTENT(INOUT)    :: Buffer(0:)
            tLogical                :: Flag
        END FUNCTION IsEmpty
        !------------------------------------------------------------
    END INTERFACE
    ! procedures from SBase_F128_FromString
    INTERFACE
        !------------------------------------------------------------
        MODULE FUNCTION Parse_Fortran_Number(cStr, Negative, SigStr, Exp10, ExpCor, ErrMsg) RESULT(Valid)
            tCharStar,  TARGET,   INTENT(IN)    :: cStr
            tLogical,             INTENT(OUT)   :: Negative
            tCharAlloc,           INTENT(OUT)   :: SigStr
            tSInt64,              INTENT(OUT)   :: Exp10
            tSInt32,              INTENT(OUT)   :: ExpCor
            tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg
            tLogical                            :: Valid
        END FUNCTION Parse_Fortran_Number
        !------------------------------------------------------------
        MODULE FUNCTION BuildFloat128(Negative, SigStr, ExpDec, ExpCor) RESULT(F128)
            tLogical,  INTENT(IN)       :: Negative
            tCharStar, INTENT(INOUT)    :: SigStr
            tSInt64,   INTENT(IN)       :: ExpDec
            tSInt32,   INTENT(IN)       :: ExpCor
            TYPE(Float128)              :: F128
        END FUNCTION BuildFloat128
        !------------------------------------------------------------
        MODULE FUNCTION Parse_F128_Number(cStr, Negative, SigDig, SigCount, Exp10, ExpCor, ErrMsg) RESULT(Valid)
            tCharStar,  INTENT(IN)  :: cStr
            TARGET                  :: cStr
            tLogical,   INTENT(OUT) :: Negative
            tSInt8,     INTENT(OUT) :: SigDig(1:MAX_MANTISSA_LENGTH+1)
            tSInt32,    INTENT(OUT) :: SigCount
            tSInt64,    INTENT(OUT) :: Exp10
            tSInt32,    INTENT(OUT) :: ExpCor
            tCharAlloc, INTENT(OUT) :: ErrMsg
            OPTIONAL                :: ErrMsg
            tLogical                :: Valid
        END FUNCTION Parse_F128_Number
        !------------------------------------------------------------
        MODULE FUNCTION AssembleFloat128(Negative, SigDig, SigCount, ExpDec, ExpCor) RESULT(F128)
            tLogical, INTENT(IN)    :: Negative
            tSInt8,   INTENT(INOUT) :: SigDig(1:MAX_MANTISSA_LENGTH+1)
            tSInt32,  INTENT(INOUT) :: SigCount
            tSInt64,  INTENT(IN)    :: ExpDec
            tSInt32,  INTENT(IN)    :: ExpCor
            TYPE(Float128)          :: F128
        END FUNCTION AssembleFloat128
        !------------------------------------------------------------
    END INTERFACE
    ! procedures from SBase_F128_Comparison
    INTERFACE
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Equal(LHS, RHS) RESULT(Flag)
            TYPE(Float128), INTENT(IN)  :: LHS
            TYPE(Float128), INTENT(IN)  :: RHS
            tLogical                    :: Flag
        END FUNCTION Float128_Equal
        !------------------------------------------------------------
        MODULE FUNCTION Float128_NotEqual(LHS, RHS) RESULT(Flag)
            TYPE(Float128), INTENT(IN)  :: LHS
            TYPE(Float128), INTENT(IN)  :: RHS
            tLogical                    :: Flag
        END FUNCTION Float128_NotEqual
        !------------------------------------------------------------
        MODULE FUNCTION Float128_LessThan(LHS, RHS) RESULT(Flag)
            TYPE(Float128), INTENT(IN)  :: LHS
            TYPE(Float128), INTENT(IN)  :: RHS
            tLogical                    :: Flag
        END FUNCTION Float128_LessThan
        !------------------------------------------------------------
        MODULE FUNCTION Float128_LessEqual(LHS, RHS) RESULT(Flag)
            TYPE(Float128), INTENT(IN)  :: LHS
            TYPE(Float128), INTENT(IN)  :: RHS
            tLogical                    :: Flag
        END FUNCTION Float128_LessEqual
        !------------------------------------------------------------
        MODULE FUNCTION Float128_GreaterThan(LHS, RHS) RESULT(Flag)
            TYPE(Float128), INTENT(IN)  :: LHS
            TYPE(Float128), INTENT(IN)  :: RHS
            tLogical                    :: Flag
        END FUNCTION Float128_GreaterThan
        !------------------------------------------------------------
        MODULE FUNCTION Float128_GreaterEqual(LHS, RHS) RESULT(Flag)
            TYPE(Float128), INTENT(IN)  :: LHS
            TYPE(Float128), INTENT(IN)  :: RHS
            tLogical                    :: Flag
        END FUNCTION Float128_GreaterEqual
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Compare(LHS, RHS) RESULT(Flag)
            TYPE(Float128), INTENT(IN)  :: LHS
            TYPE(Float128), INTENT(IN)  :: RHS
            tSInt32                     :: Flag
        END FUNCTION Float128_Compare
        !------------------------------------------------------------
    END INTERFACE
    ! procedures from SBase_F128_Arithmetic
    INTERFACE
        !------------------------------------------------------------
        ! 'Plus' operations
        !------------------------------------------------------------
        MODULE FUNCTION Float128_UnaryPlus(InVal) RESULT(OutVal)
            !^ OutVal = +InVal
            TYPE(Float128), INTENT(IN)  :: InVal
            TYPE(Float128)              :: OutVal
        END FUNCTION Float128_UnaryPlus
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Increment(Val)
            !^ Val = Val + 1
            TYPE(Float128), INTENT(INOUT)   :: Val
        END SUBROUTINE Float128_Increment
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Add_R32(This, Other)
            !^ This = This + Other
            TYPE(Float128), INTENT(INOUT)   :: This
            tRealSP,        INTENT(IN)      :: Other
        END SUBROUTINE Float128_Add_R32
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Add_R64(This, Other)
            !^ This = This + Other
            TYPE(Float128), INTENT(INOUT)   :: This
            tRealDP,        INTENT(IN)      :: Other
        END SUBROUTINE Float128_Add_R64
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Add_Float128(This, Other)
            !^ This = This + Other
            TYPE(Float128), INTENT(INOUT)   :: This
            TYPE(Float128), INTENT(IN)      :: Other
        END SUBROUTINE Float128_Add_Float128
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Plus_R32(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal + RhsVal
            TYPE(Float128), INTENT(IN)  :: LhsVal
            tRealSP,        INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION Float128_Plus_R32
        !------------------------------------------------------------
        MODULE FUNCTION R32_Plus_Float128(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal + RhsVal
            tRealSP,        INTENT(IN)  :: LhsVal
            TYPE(Float128), INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION R32_Plus_Float128
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Plus_R64(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal + RhsVal
            TYPE(Float128), INTENT(IN)  :: LhsVal
            tRealDP,        INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION Float128_Plus_R64
        !------------------------------------------------------------
        MODULE FUNCTION R64_Plus_Float128(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal + RhsVal
            tRealDP,        INTENT(IN)  :: LhsVal
            TYPE(Float128), INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION R64_Plus_Float128
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Plus_Float128(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal + RhsVal
            TYPE(Float128), INTENT(IN)  :: LhsVal
            TYPE(Float128), INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION Float128_Plus_Float128
        !------------------------------------------------------------
        ! 'Minus' operations
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Negate(InVal) RESULT(OutVal)
            !^ OutVal = -InVal
            TYPE(Float128), INTENT(IN)   :: InVal
            TYPE(Float128)               :: OutVal
        END FUNCTION Float128_Negate
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Decrement(Val)
            !^ Val = Val - 1
            TYPE(Float128), INTENT(INOUT)   :: Val
        END SUBROUTINE Float128_Decrement
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Subtract_R32(This, Other)
            !^ This = This - Other
            TYPE(Float128), INTENT(INOUT)   :: This
            tRealSP,        INTENT(IN)      :: Other
        END SUBROUTINE Float128_Subtract_R32
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Subtract_R64(This, Other)
            !^ This = This - Other
            TYPE(Float128), INTENT(INOUT)   :: This
            tRealDP,        INTENT(IN)      :: Other
        END SUBROUTINE Float128_Subtract_R64
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Subtract_Float128(This, Other)
            !^ This = This - Other
            TYPE(Float128), INTENT(INOUT)   :: This
            TYPE(Float128), INTENT(IN)      :: Other
        END SUBROUTINE Float128_Subtract_Float128
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Minus_R32(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal - RhsVal
            TYPE(Float128), INTENT(IN)  :: LhsVal
            tRealSP,        INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION Float128_Minus_R32
        !------------------------------------------------------------
        MODULE FUNCTION R32_Minus_Float128(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal - RhsVal
            tRealSP,        INTENT(IN)  :: LhsVal
            TYPE(Float128), INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION R32_Minus_Float128
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Minus_R64(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal - RhsVal
            TYPE(Float128), INTENT(IN)  :: LhsVal
            tRealDP,        INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION Float128_Minus_R64
        !------------------------------------------------------------
        MODULE FUNCTION R64_Minus_Float128(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal - RhsVal
            tRealDP,        INTENT(IN)  :: LhsVal
            TYPE(Float128), INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION R64_Minus_Float128
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Minus_Float128(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal - RhsVal
            TYPE(Float128), INTENT(IN)  :: LhsVal
            TYPE(Float128), INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION Float128_Minus_Float128
        !------------------------------------------------------------
        ! 'Multiply' operations
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Times_R32(This, Other)
            !^ This = This * Other
            TYPE(Float128), INTENT(INOUT)   :: This
            tRealSP,        INTENT(IN)      :: Other
        END SUBROUTINE Float128_Times_R32
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Times_R64(This, Other)
            !^ This = This * Other
            TYPE(Float128), INTENT(INOUT)   :: This
            tRealDP,        INTENT(IN)      :: Other
        END SUBROUTINE Float128_Times_R64
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Times_Float128(This, Other)
            !^ This = This * Other
            TYPE(Float128), INTENT(INOUT)   :: This
            TYPE(Float128), INTENT(IN)      :: Other
        END SUBROUTINE Float128_Times_Float128
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Multiply_R32(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal * RhsVal
            TYPE(Float128), INTENT(IN)  :: LhsVal
            tRealSP,        INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION Float128_Multiply_R32
        !------------------------------------------------------------
        MODULE FUNCTION R32_Multiply_Float128(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal * RhsVal
            tRealSP,        INTENT(IN)  :: LhsVal
            TYPE(Float128), INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION R32_Multiply_Float128
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Multiply_R64(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal * RhsVal
            TYPE(Float128), INTENT(IN)  :: LhsVal
            tRealDP,        INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION Float128_Multiply_R64
        !------------------------------------------------------------
        MODULE FUNCTION R64_Multiply_Float128(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal * RhsVal
            tRealDP,        INTENT(IN)  :: LhsVal
            TYPE(Float128), INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION R64_Multiply_Float128
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Multiply_Float128(LhsVal, RhsVal) RESULT(OutVal)
            !^ OutVal = LhsVal * RhsVal
            TYPE(Float128), INTENT(IN)  :: LhsVal
            TYPE(Float128), INTENT(IN)  :: RhsVal
            TYPE(Float128)              :: OutVal
        END FUNCTION Float128_Multiply_Float128
        !------------------------------------------------------------
        ! 'Divide' operations
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_DivMod_Float128(Dividend, Divisor, Quotient, Remainder)
            TYPE(Float128), INTENT(IN)  :: Dividend
            TYPE(Float128), INTENT(IN)  :: Divisor
            TYPE(Float128), INTENT(OUT) :: Quotient
            TYPE(Float128), INTENT(OUT) :: Remainder
        END SUBROUTINE Float128_DivMod_Float128
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_DivMod_R32(Dividend, Divisor, Quotient, Remainder)
            TYPE(Float128), INTENT(IN)  :: Dividend
            tRealSP,        INTENT(IN)  :: Divisor
            TYPE(Float128), INTENT(OUT) :: Quotient
            TYPE(Float128), INTENT(OUT) :: Remainder
        END SUBROUTINE Float128_DivMod_R32
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_DivMod_R64(Dividend, Divisor, Quotient, Remainder)
            TYPE(Float128), INTENT(IN)  :: Dividend
            tRealDP,        INTENT(IN)  :: Divisor
            TYPE(Float128), INTENT(OUT) :: Quotient
            TYPE(Float128), INTENT(OUT) :: Remainder
        END SUBROUTINE Float128_DivMod_R64
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Over_R32(This, Other, Remainder)
            !^ This = This / Other
            TYPE(Float128),    INTENT(INOUT)    :: This
            tRealSP,           INTENT(IN)       :: Other
            tRealSP, OPTIONAL, INTENT(OUT)      :: Remainder
        END SUBROUTINE Float128_Over_R32
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Over_R64(This, Other, Remainder)
            !^ This = This / Other
            TYPE(Float128),    INTENT(INOUT)    :: This
            tRealDP,           INTENT(IN)       :: Other
            tRealDP, OPTIONAL, INTENT(OUT)      :: Remainder
        END SUBROUTINE Float128_Over_R64
        !------------------------------------------------------------
        MODULE SUBROUTINE Float128_Over_Float128(This, Other, Remainder)
            !^ This = This / Other
            TYPE(Float128),           INTENT(INOUT) :: This
            TYPE(Float128),           INTENT(IN)    :: Other
            TYPE(Float128), OPTIONAL, INTENT(OUT)   :: Remainder
        END SUBROUTINE Float128_Over_Float128
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Divide_R32(Dividend, Divisor) RESULT(Quotient)
            !^ Quotient = Dividend / Divisor
            TYPE(Float128), INTENT(IN)  :: Dividend
            tRealSP,        INTENT(IN)  :: Divisor
            TYPE(Float128)              :: Quotient
        END FUNCTION Float128_Divide_R32
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Divide_R64(Dividend, Divisor) RESULT(Quotient)
            !^ Quotient = Dividend / Divisor
            TYPE(Float128), INTENT(IN)  :: Dividend
            tRealDP,        INTENT(IN)  :: Divisor
            TYPE(Float128)              :: Quotient
        END FUNCTION Float128_Divide_R64
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Divide_Float128(Dividend, Divisor) RESULT(Quotient)
            !^ Quotient = Dividend / Divisor
            TYPE(Float128), INTENT(IN)  :: Dividend
            TYPE(Float128), INTENT(IN)  :: Divisor
            TYPE(Float128)              :: Quotient
        END FUNCTION Float128_Divide_Float128
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Mod_R32(Dividend, Divisor) RESULT(Remainder)
            !^ Remainder = Dividend MOD Divisor
            TYPE(Float128), INTENT(IN)  :: Dividend
            tRealSP,        INTENT(IN)  :: Divisor
            TYPE(Float128)              :: Remainder
        END FUNCTION Float128_Mod_R32
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Mod_R64(Dividend, Divisor) RESULT(Remainder)
            !^ Remainder = Dividend MOD Divisor
            TYPE(Float128), INTENT(IN)  :: Dividend
            tRealDP,        INTENT(IN)  :: Divisor
            TYPE(Float128)              :: Remainder
        END FUNCTION Float128_Mod_R64
        !------------------------------------------------------------
        MODULE FUNCTION Float128_Mod_Float128(Dividend, Divisor) RESULT(Remainder)
            !^ Remainder = Dividend MOD Divisor
            TYPE(Float128), INTENT(IN)  :: Dividend
            TYPE(Float128), INTENT(IN)  :: Divisor
            TYPE(Float128)              :: Remainder
        END FUNCTION Float128_Mod_Float128
    END INTERFACE

!** GENERIC DEFINITIONS:
    ! assignment (for conversions between Float128 and unsigned 32/64 bit integers)
    INTERFACE ASSIGNMENT(=)
        !^ **Operator Overload**: ASSIGNMENT(=) <br>
        !  **Purpose**:  To convert between a *Float128* number and an other real number. <br>
        !  **Usage**: <br>
        !   ! convert 32-bit real number (Fortran intrinsic type) to the *Float128* number <br>
        !   --->    F128 = R32 <br>
        !   ! convert the *Float128* number to 128-bit real number (Fortran intrinsic type) <br>
        !   --->    R128 = F128
        MODULE PROCEDURE Float128_Assign,   Float128_From_R32
        MODULE PROCEDURE Float128_From_R64, Float128_From_R128
        MODULE PROCEDURE Float128_To_R32,   Float128_To_R64
        MODULE PROCEDURE Float128_To_R128
    END INTERFACE
    ! constructor
    INTERFACE Float128
        !^ **Constructor Interface**: Float128 <br>
        !  **Purpose**:  To construct a *Float128* number. <br>
        !  **Usage**: <br>
        !   ! construct from 32-bit intrinsic integer number <br>
        !   --->    F128 = Float128(I32) <br>
        !   ! construct from 64-bit intrinsic real number <br>
        !   --->    F128 = Float128(R64) <br>
        !   ! construct from a decimal string <br>
        !   --->    I128 = Float128('1234567.890987654321011223344E-23132', ErrFlag, ErrMsg) <br>
        MODULE PROCEDURE Construct_Positive_Float128
        MODULE PROCEDURE I32_To_Float128,    I64_To_Float128
        MODULE PROCEDURE R32_To_Float128,    R64_To_Float128
        MODULE PROCEDURE R128_To_Float128,   DecString_To_Float128
    END INTERFACE
    INTERFACE ToF128
        MODULE PROCEDURE String_2_Float128
    END INTERFACE
    ! conversion
    INTERFACE ToI32
        !^ **Function Interface**: ToI32 <br>
        !  **Purpose**:  To convert a *Float128* number to a 32-bit integer number. <br>
        !  **Usage**: <br>
        !   --->    I32 = ToI32(F128)
        MODULE PROCEDURE I32_From_Float128
    END INTERFACE
    INTERFACE ToI64
        !^ **Function Interface**: ToI64 <br>
        !  **Purpose**:  To convert a *Float128* number to a 64-bit integer number. <br>
        !  **Usage**: <br>
        !   --->    I64 = ToI64(F128)
        MODULE PROCEDURE I64_From_Float128
    END INTERFACE
    INTERFACE ToR32
        !^ **Function Interface**: ToR32 <br>
        !  **Purpose**:  To convert a *Float128* number to a 32-bit real number. <br>
        !  **Usage**: <br>
        !   --->    R32 = ToR32(F128)
        MODULE PROCEDURE R32_From_Float128
    END INTERFACE
    INTERFACE ToR64
        !^ **Function Interface**: ToR64 <br>
        !  **Purpose**:  To convert a *Float128* number to a 64-bit real number. <br>
        !  **Usage**: <br>
        !   --->    R64 = ToR64(F128)
        MODULE PROCEDURE R64_From_Float128
    END INTERFACE
    INTERFACE ToR128
        !^ **Function Interface**: ToR128 <br>
        !  **Purpose**:  To convert a *Float128* number to a 128-bit real number. <br>
        !  **Usage**: <br>
        !   --->    R128 = ToR128(F128)
        MODULE PROCEDURE R128_From_Float128
    END INTERFACE
    INTERFACE ToDecString
        !^ **Function Interface**: ToDecString <br>
        !  **Purpose**:  To convert a *Float128* number to a decimal string. <br>
        !  **Usage**: <br>
        !   --->    Str = ToDecString(F128)
        MODULE PROCEDURE DecString_From_Float128
    END INTERFACE
    ! comparison
    INTERFACE OPERATOR(==)
        !^ **Operator Overload**: OPERATOR(==) <br>
        !  **Purpose**:  To check if values of two *Float128* numbers are equal.
        !   Return .TRUE. if both values are equal; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS == RHS <br>
        !   --->    IF (LHS .EQ. RHS) DoSomething
        MODULE PROCEDURE Float128_Equal
    END INTERFACE
    INTERFACE OPERATOR(/=)
        !^ **Operator Overload**: OPERATOR(/=) <br>
        !  **Purpose**:  To check if values of two *Float128* numbers are not equal.
        !   Return .TRUE. if both values are NOT equal; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS /= RHS <br>
        !   --->    IF (LHS .NE. RHS) DoSomething
        MODULE PROCEDURE Float128_NotEqual
    END INTERFACE
    INTERFACE OPERATOR(<)
        !^ **Operator Overload**: OPERATOR(<) <br>
        !  **Purpose**:  To check if the LHS value is less than the RHS value.
        !   Return .TRUE. if LHS < RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS < RHS <br>
        !   --->    IF (LHS .LT. RHS) DoSomething
        MODULE PROCEDURE Float128_LessThan
    END INTERFACE
    INTERFACE OPERATOR(<=)
        !^ **Operator Overload**: OPERATOR(<=) <br>
        !  **Purpose**:  To check if the LHS value is less than or equal to the RHS value.
        !   Return .TRUE. if LHS <= RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS <= RHS <br>
        !   --->    IF (LHS .LE. RHS) DoSomething
        MODULE PROCEDURE Float128_LessEqual
    END INTERFACE
    INTERFACE OPERATOR(>)
        !^ **Operator Overload**: OPERATOR(>) <br>
        !  **Purpose**:  To check if the LHS value is greater than the RHS value.
        !   Return .TRUE. if LHS > RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS > RHS <br>
        !   --->    IF (LHS .GT. RHS) DoSomething
        MODULE PROCEDURE Float128_GreaterThan
    END INTERFACE
    INTERFACE OPERATOR(>=)
        !^ **Operator Overload**: OPERATOR(>=) <br>
        !  **Purpose**:  To check if the LHS value is greater than or equal to the RHS value.
        !   Return .TRUE. if LHS >= RHS; otherwise return .FALSE.. <br>
        !  **Usage**: <br>
        !   --->    Flag = LHS >= RHS <br>
        !   --->    IF (LHS .GE. RHS) DoSomething
        MODULE PROCEDURE Float128_GreaterEqual
    END INTERFACE
    INTERFACE Compare
        !^ **Function Interface**: Compare <br>
        !  **Purpose**:  To compare *Float128* numbers and return <br>
        !   -1 if LHS < RHS, <br>
        !    0 if LHS == RHS, or <br>
        !    1 if LHS > RHS. <br>
        !  **Usage**: <br>
        !   --->    Flag = Compare(LHS, RHS) <br>
        !   --->    IF (Compare(LHS, RHS) /= 0) DoSomething
        MODULE PROCEDURE Float128_Compare
    END INTERFACE
    ! arithmetic
    INTERFACE OPERATOR(+)
        !^ **Operator Overload**: OPERATOR(+) <br>
        !  **Purpose**:  To perform a summation of two real numbers (at least one of which is
        !       a *Float128* number) or to add a unary plus sign to a *Float128* number. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = +INPUT <br>
        !   --->    OUTPUT = FIRST_IN + SECOND_IN
        MODULE PROCEDURE Float128_UnaryPlus,    Float128_Plus_Float128
        MODULE PROCEDURE Float128_Plus_R32,     R32_Plus_Float128
        MODULE PROCEDURE Float128_Plus_R64,     R64_Plus_Float128
    END INTERFACE
    INTERFACE OPERATOR(-)
        !^ **Operator Overload**: OPERATOR(-) <br>
        !  **Purpose**:  To perform a subtraction of two real numbers (at least one of which is
        !       a *Float128* number) or to perform a negation of a *Float128* number. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = -INPUT <br>
        !   --->    OUTPUT = FIRST_IN - SECOND_IN <br>
        MODULE PROCEDURE Float128_Negate,       Float128_Minus_Float128
        MODULE PROCEDURE Float128_Minus_R32,    R32_Minus_Float128
        MODULE PROCEDURE Float128_Minus_R64,    R64_Minus_Float128
    END INTERFACE
    INTERFACE OPERATOR(*)
        !^ **Operator Overload**: OPERATOR( * ) <br>
        !  **Purpose**:  To perform a multiplication of two real numbers (at least one of which is
        !       a *Float128* number). <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = FIRST_IN * SECOND_IN
        MODULE PROCEDURE Float128_Multiply_Float128
        MODULE PROCEDURE Float128_Multiply_R32, R32_Multiply_Float128
        MODULE PROCEDURE Float128_Multiply_R64, R64_Multiply_Float128
    END INTERFACE
    INTERFACE OPERATOR(/)
        !^ **Operator Overload**: OPERATOR(/) <br>
        !  **Purpose**:  To perform a division of two real numbers where the dividend (numerator)
        !       must be a *Float128* number. <br>
        !  **Usage**: <br>
        !   --->    OUTPUT = FIRST_IN * SECOND_IN
        MODULE PROCEDURE Float128_Divide_R32,   Float128_Divide_R64
        MODULE PROCEDURE Float128_Divide_Float128
    END INTERFACE
    INTERFACE MOD
        !^ **Function Interface**: MOD <br>
        !  **Purpose**:  To return the remainder of a division of two real numbers, where the
        !       dividend (numerator) must be a *Float128* number. <br>
        !  **Usage**: <br>
        !   --->    REM = MOD(NUMER, DENOM)
        MODULE PROCEDURE Float128_Mod_R32,      Float128_Mod_R64
        MODULE PROCEDURE Float128_Mod_Float128
    END INTERFACE
    INTERFACE DivMod
        !^ **Subroutine Interface**: DivMod <br>
        !  **Purpose**:  To perform a division of two real numbers where the dividend (numerator)
        !        must be a *Float128* number and to return both the quotient and the remainder. <br>
        !  **Usage**: <br>
        !   --->    CALL DivMod(NUMER, DENOM, QUOT, REM)
        MODULE PROCEDURE Float128_DivMod_R32,   Float128_DivMod_R64
        MODULE PROCEDURE Float128_DivMod_Float128
    END INTERFACE
    INTERFACE Increment
        !^ **Subroutine Interface**: Increment <br>
        !  **Purpose**:  To increase value of a *Float128* number by one. <br>
        !  **Usage**: <br>
        !   --->    CALL Increment(F128)
        MODULE PROCEDURE Float128_Increment
    END INTERFACE
    INTERFACE Decrement
        !^ **Subroutine Interface**: Decrement <br>
        !  **Purpose**:  To decrease value of a *Float128* by one. <br>
        !  **Usage**: <br>
        !   --->    CALL Decrement(F128)
        MODULE PROCEDURE Float128_Decrement
    END INTERFACE
    INTERFACE Add
        !^ **Subroutine Interface**: Add <br>
        !  **Purpose**:  To perform addition: This = This + Other. <br>
        !  **Usage**: <br>
        !   --->    CALL Add(This, Other)
        MODULE PROCEDURE Float128_Add_R32,      Float128_Add_R64
        MODULE PROCEDURE Float128_Add_Float128
    END INTERFACE
    INTERFACE Subtract
        !^ **Subroutine Interface**: Subtract <br>
        !  **Purpose**:  To perform subtraction: This = This - Other. <br>
        !  **Usage**: <br>
        !   --->    CALL Subtract(This, Other) <br>
        MODULE PROCEDURE Float128_Subtract_R32, Float128_Subtract_R64
        MODULE PROCEDURE Float128_Subtract_Float128
    END INTERFACE
    INTERFACE Multiply
        !^ **Subroutine Interface**: Multiply <br>
        !  **Purpose**:  To perform multiplication: This = This * Other. <br>
        !  **Usage**: <br>
        !   --->    CALL Multiply(This, Other)
        MODULE PROCEDURE Float128_Times_R32,    Float128_Times_R64
        MODULE PROCEDURE Float128_Times_Float128
    END INTERFACE
    INTERFACE Divide
        !^ **Subroutine Interface**: Divide <br>
        !  **Purpose**:  To perform a division: This = This / Other. <br>
        !  **Usage**: <br>
        !   --->    CALL Divide(This, Other)
        MODULE PROCEDURE Float128_Over_R32,     Float128_Over_R64
        MODULE PROCEDURE Float128_Over_Float128
    END INTERFACE
    ! auxiliary
    INTERFACE Assign
        MODULE PROCEDURE Assign_I32_To_Float128, Assign_I64_To_Float128
        MODULE PROCEDURE Assign_R32_To_Float128, Assign_R64_To_Float128
        MODULE PROCEDURE Assign_R128_To_Float128
    END INTERFACE
    ! inquiry / getter
    INTERFACE Is_NaN
        !^ **Function Interface**: Is_NaN <br>
        !  **Purpose**:  To check whether the input value is a NAN (not a number) or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Is_NaN(INPUT) <br>
        !   --->    IF (.NOT.Is_NaN(INPUT)) DoSomeThing
        MODULE PROCEDURE Float128_Is_NaN
    END INTERFACE
    INTERFACE Is_Infinite
        !^ **Function Interface**: Is_Infinite <br>
        !  **Purpose**:  To check whether the input value is an infinity or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = Is_Infinite(INPUT) <br>
        !   --->    IF (.NOT.Is_Infinite(INPUT)) DoSomeThing
        MODULE PROCEDURE Float128_Is_Infinite
    END INTERFACE
    INTERFACE Is_Zero
        !^ **Function Interface**: IsZero <br>
        !  **Purpose**:  To check whether the input value is zero or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsZero(INPUT) <br>
        !   --->    IF (.NOT.IsZero(INPUT)) DoSomeThing
        MODULE PROCEDURE Float128_Is_Zero
    END INTERFACE
    INTERFACE Is_Negative
        !^ **Function Interface**: IsNegative <br>
        !  **Purpose**:  To check whether the input value is negative or not. <br>
        !  **Usage**: <br>
        !   --->    Flag = IsNegative(INPUT) <br>
        !   --->    IF (.NOT.IsNegative(INPUT)) DoSomeThing
        MODULE PROCEDURE Float128_Is_Negative
    END INTERFACE
    INTERFACE GetExponent
        MODULE PROCEDURE Float128_GetExponent
    END INTERFACE
    INTERFACE GetExpUnbiased
        MODULE PROCEDURE Float128_GetExponent_Unbiased
    END INTERFACE
    INTERFACE GetMantHi
        MODULE PROCEDURE Float128_GetMantissaHigh
    END INTERFACE
    INTERFACE GetMantLo
        MODULE PROCEDURE Float128_GetMantissaLow
    END INTERFACE
    INTERFACE FloorNCeiling
        MODULE PROCEDURE Float128_FloorNCeiling
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE ELEMENTS SUBROUTINES OR FUNCTIONS:

!------------------------------------------------------------------------------
!
!                          ASSIGN ROUTINES
!
!------------------------------------------------------------------------------

SUBROUTINE AssignZero(F128, ChangeSign)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign the value of zero to the Float128 object with
    ! or without inverting its sign.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(INOUT)   :: F128
    tLogical,       INTENT(IN)      :: ChangeSign   ! if true, the sign will be changed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    F128%Negative = F128%Negative .NEQV. ChangeSign
    F128%Exponent = 0
    F128%MantHi   = 0_kInt64
    F128%MantLo   = 0_kInt64

    RETURN

END SUBROUTINE AssignZero

!******************************************************************************

SUBROUTINE AssignOne(F128, ChangeSign)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign the value of one to the Float128 object with
    ! or without inverting its sign.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(INOUT)   :: F128
    tLogical,       INTENT(IN)      :: ChangeSign   ! if true, the sign will be changed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    F128%Negative = F128%Negative .NEQV. ChangeSign
    F128%Exponent = EXPONENT_OF_ONE
    F128%MantHi   = 0_kInt64
    F128%MantLo   = 0_kInt64

    RETURN

END SUBROUTINE AssignOne

!******************************************************************************

SUBROUTINE AssignInfinity(F128, ChangeSign)

!** PURPOSE OF THIS SUBROUTINE:
    ! To assign the value of infinity to the Float128 object with
    ! or without inverting its sign.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(INOUT)   :: F128
    tLogical,       INTENT(IN)      :: ChangeSign   ! if true, the sign will be changed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    F128%Negative = F128%Negative .NEQV. ChangeSign
    F128%Exponent = EXPONENT_OF_INFINITY
    F128%MantHi   = 0_kInt64
    F128%MantLo   = 0_kInt64

    RETURN

END SUBROUTINE AssignInfinity

!******************************************************************************

FUNCTION AssignWithUnbiasedExponent(Negative, Exponent, MantHi, MantLo) RESULT(F128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct Float128 object from the specified input where
    ! the exponent is treated as the unbiased exponent value,
    ! whose 0 value corresponds to the Float128 value of 1.0.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLogical, INTENT(IN)    :: Negative
    tSInt32,  INTENT(IN)    :: Exponent     ! unbiased exponent
    tSInt64,  INTENT(IN)    :: MantHi
    tSInt64,  INTENT(IN)    :: MantLo
    TYPE(Float128)          :: F128

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    F128%Negative = Negative
    F128%Exponent = Exponent + EXPONENT_BIAS
    F128%MantHi   = MantHi
    F128%MantLo   = MantLo

    RETURN

END FUNCTION AssignWithUnbiasedExponent

!------------------------------------------------------------------------------
!
!                               INQUIRY ROUTINES
!
!------------------------------------------------------------------------------

FUNCTION Float128_Is_NaN(F128) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the number is NAN

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(IN)  :: F128
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (F128%Exponent == EXPONENT_OF_INFINITY).AND. &
           (IOR(F128%MantHi, F128%MantLo) /= 0_kInt64)

    RETURN

END FUNCTION Float128_Is_NaN

!******************************************************************************

FUNCTION Float128_Is_Infinite(F128) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the number is infinite (either positive or negative)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(IN)  :: F128
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (F128%Exponent == EXPONENT_OF_INFINITY).AND. &
           (IOR(F128%MantHi, F128%MantLo) == 0_kInt64)

    RETURN

END FUNCTION Float128_Is_Infinite

!******************************************************************************

FUNCTION Float128_Is_Negative(F128) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the number has negative value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(IN)  :: F128
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = F128%Negative

    RETURN

END FUNCTION Float128_Is_Negative

!******************************************************************************

FUNCTION Float128_Is_Zero(F128) RESULT(Flag)

!** PURPOSE OF THIS SUBROUTINE:
    ! To check whether the number is zero (either positive or negative)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(IN)  :: F128
    tLogical                    :: Flag

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Flag = (IOR(IOR(F128%MantHi, F128%MantLo), ToInt64(F128%Exponent)) == 0_kInt64)

    RETURN

END FUNCTION Float128_Is_Zero

!******************************************************************************

FUNCTION Float128_GetExponent(F128) RESULT(Exponent)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get the raw (biased) binary exponent
    ! i.e. 0x7FFFFFFF for values falling within the interval of {[1.0 .. 2.0)},
    ! 0x80000000 for {[2.0 .. 4.0)} etc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(IN)  :: F128
    tSInt32                     :: Exponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Exponent = F128%Exponent

    RETURN

END FUNCTION Float128_GetExponent

!******************************************************************************

FUNCTION Float128_GetExponent_Unbiased(F128) RESULT(Exponent)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get unbiased binary exponent
    ! i.e. 0 for values falling within the interval of {[1.0 .. 2.0)},
    ! 1 for {[2.0 .. 4.0)} etc.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(IN)  :: F128
    tSInt32                     :: Exponent

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Exponent = F128%Exponent - EXPONENT_BIAS

    RETURN

END FUNCTION Float128_GetExponent_Unbiased

!******************************************************************************

FUNCTION Float128_GetMantissaHigh(F128) RESULT(Mantissa)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get the most significant 64 bits of the fractional part of the mantissa

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(IN)  :: F128
    tSInt64                     :: Mantissa

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Mantissa = F128%MantHi

    RETURN

END FUNCTION Float128_GetMantissaHigh

!******************************************************************************

FUNCTION Float128_GetMantissaLow(F128) RESULT(Mantissa)

!** PURPOSE OF THIS SUBROUTINE:
    ! To get the least significant 64 bits of the fractional part of the mantissa

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(IN)  :: F128
    tSInt64                     :: Mantissa

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Mantissa = F128%MantLo

    RETURN

END FUNCTION Float128_GetMantissaLow

!------------------------------------------------------------------------------
!
!                           MISCELLANEOUS ROUTINES
!
!------------------------------------------------------------------------------

SUBROUTINE MakeSubnormal(F128, Exp2)

!** PURPOSE OF THIS SUBROUTINE:
    ! For a Float128 with a normal mantissa (with implied unity) and non-positive
    ! biased exponent, converts it into the conventional subnormal form, with the
    ! exponent = 0 and the mantissa shifted rightwards with explicit 1 in the
    ! appropriate position.  Shifts mantissa rightwards by |exp2| + 1 bits, sets
    ! explicit 1, and rounds it up, taking into account the bits having been shifted-out

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(INOUT)   :: F128
    tSInt64,        INTENT(INOUT)   :: Exp2
    ! Exp2 (IN)  : the exponent of the newly-found subnormal value (always negative)
    ! Exp2 (OUT) : the exponent for the new value, 0 in an ordinary case,
    !              and 1 if the rounding has led to overflow of the mantissa

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: ShiftedOutBit

!** FLOW

    ! just for convenience
    Exp2 = -Exp2

    IF (Exp2 > 127) THEN
        ! Effectively 0 or MIN_VALUE
        F128%MantHi = 0_kInt64
        F128%MantLo = 0_kInt64
        IF (Exp2 == 128) F128%MantLo = F128%MantLo + 1_kInt64    ! MIN_VALUE
        Exp2 = 0_kInt64                                          ! Exp2 >= 129 means 0
        RETURN
    END IF

    ShiftedOutBit = ShiftMantissa(F128, Exp2)

    ! it's subnormal
    Exp2 = 0_kInt64
    IF (ShiftedOutBit /= 0_kInt64) THEN
        F128%MantLo = F128%MantLo + 1_kInt64
        F128%MantHi = F128%MantHi + 1_kInt64
        IF ((F128%MantLo == 0_kInt64).AND.(F128%MantHi == 0_kInt64)) THEN
            ! Round up. carry beyond the higher word?
            Exp2 = Exp2 + 1_kInt64   ! it becomes MIN_NORMAL
        END IF
    END IF

    IF ((Exp2 == 0_kInt64).AND.(F128%MantLo == 0_kInt64).AND.(F128%MantHi == 0_kInt64)) THEN
        ! (not really) zero -> min value
        F128%MantLo = 1_kInt64
    END IF

    RETURN

END SUBROUTINE MakeSubnormal

!******************************************************************************

FUNCTION ShiftMantissa(F128, Exp2) RESULT(ShiftedOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To shift the mantissa by exp2 + 1 bits rightwards, to make
    ! a conventional subnormal value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(INOUT)   :: F128
    tSInt64,        INTENT(IN)      :: Exp2         ! unbiased exponent of the value (negated)
    tSInt64                         :: ShiftedOut   ! the highest bit that has been shifted out beyond
                                                    ! the two longs of mantissa (1L if it was 1, 0 otherwise)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! The highest of shifted out bits to evaluate carry
    ShiftedOut = IAND(F128%MantLo, 1_kInt64)
    F128%MantLo = IOR(SHIFTR(F128%MantLo, 1), SHIFTL(F128%MantHi, 63))
    ! move 1 bit right and set unity that was implied
    F128%MantHi = IOR(SHIFTR(F128%MantHi, 1), HIGH_BIT)

    IF (Exp2 >= 64) THEN
        ! the higher word move into the lower
        IF (Exp2 == 64) THEN
            ! former lowest bit of F128%MantHi now is the highest bit of F128%MantLo
            ShiftedOut = SHIFTR(F128%MantLo, 63)
        ELSE
            ! one of the bits of the high word
            ShiftedOut = IAND(SHIFTR(F128%MantHi, (Exp2 - 65)), 1_kInt64)
        END IF
        F128%MantLo = SHIFTR(F128%MantHi, Exp2 - 64)
        F128%MantHi = 0_kInt64
    ELSEIF (Exp2 > 0) THEN
        ! Shift both words
        ShiftedOut  = IAND(SHIFTR(F128%MantLo, Exp2 - 1), 1_kInt64)
        F128%MantLo = IOR(SHIFTR(F128%MantLo, Exp2), SHIFTL(F128%MantHi, 64 - Exp2))
        F128%MantHi = SHIFTR(F128%MantHi, Exp2)
    END IF

    RETURN

END FUNCTION ShiftMantissa

!******************************************************************************

FUNCTION String_2_Float128(cStr, ErrFlag, ErrMsg) RESULT(F128)

!** PURPOSE OF THIS SUBROUTINE:
    ! To convert a decimal string to a signed arbitrary-precision integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tCharStar,  TARGET,   INTENT(IN)    :: cStr     ! character string
    tLogical,   OPTIONAL, INTENT(OUT)   :: ErrFlag  ! true if input is not invalid
    tCharAlloc, OPTIONAL, INTENT(OUT)   :: ErrMsg   ! message if input is not invalid
    TYPE(Float128)                      :: F128     ! number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical        :: Valid
    tLogical        :: Negative
    tSInt8          :: SigDig(1:MAX_MANTISSA_LENGTH+1)
    tSInt32         :: SigCount
    tSInt64         :: ExpDec
    tSInt32         :: ExpCor

!** FLOW

    ! parse the input string
    Valid = Parse_F128_Number(cStr, Negative, SigDig, SigCount, ExpDec, ExpCor, ErrMsg)

    ! check whether the input string is valid or not
    IF (Valid) THEN
        ! set flag
        IF (PRESENT(ErrFlag)) ErrFlag = FalseVal
        ! get value of Float128
        F128 = AssembleFloat128(Negative, SigDig, SigCount, ExpDec, ExpCor)
    ELSE
        ! set flag
        IF (PRESENT(ErrFlag)) ErrFlag = TrueVal
        ! set Float128
        F128 = NOT_A_NUMBER
    END IF

    RETURN

END FUNCTION String_2_Float128

!******************************************************************************

FUNCTION Float128_FloorNCeiling(InVal, IsFloor) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine floor or ceiling of the input value depending on 'IsFloor' flag

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(IN)  :: InVal    ! input value to be floored or ceiled
    tLogical,       INTENT(IN)  :: IsFloor  ! true, return floor value
                                            ! otherwise, return ceiling value
    TYPE(Float128)              :: OutVal   ! floor (or ceiling) of the input value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    IF (IsFloor) THEN
        OutVal = Float128_FloorOrCeiling(InVal, MINUS_ONE, ZERO_F128, MINUS_ONE)
    ELSE
        OutVal = Float128_FloorOrCeiling(InVal, -ZERO_F128, ONE_F128, ONE_F128)
    END IF

    RETURN

END FUNCTION Float128_FloorNCeiling

!******************************************************************************

FUNCTION Float128_FloorOrCeiling(InVal, NegBound, PosBound, Sign) RESULT(OutVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To determine floor or ceiling of the input value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Float128), INTENT(IN)  :: InVal    ! input value to be floored or ceiled
    TYPE(Float128), INTENT(IN)  :: NegBound ! result for values in (-1, 0)
    TYPE(Float128), INTENT(IN)  :: PosBound ! result for values in (0, 1)
    TYPE(Float128), INTENT(IN)  :: Sign     ! the sign of the result
    TYPE(Float128)              :: OutVal   ! floor (or ceiling) of the input value

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER    :: SIGNIF_BIT_MASK = ToInt64(Z'FFFFFFFFFFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Exponent
    tSInt64     :: MaskHi, MaskLo

!** FLOW

    Exponent = GetExpUnbiased(InVal)
    OutVal = InVal

    IF (Exponent < 0) THEN
        ! Absolute value of argument is less than 1.
        ! floorOrCeil(-0.0) => -0.0
        ! floorOrCeil(+0.0) => +0.0
        IF (InVal == ZERO_F128) THEN
            RETURN
        ELSE
            IF (InVal < ZERO_F128) THEN
                OutVal = NegBound
            ELSE
                OutVal = PosBound
            END IF
            RETURN
        END IF
    ELSEIF (Exponent >= 128) THEN
        ! Infinity, NaN, or a value so large it must be integral
        RETURN
    END IF

    ! Else the argument is either an integral value already XOR it
    ! has to be rounded to one.

    IF (Exponent >= 64) THEN
        MaskHi = SHIFTR(SIGNIF_BIT_MASK, 63)
        MaskLo = SHIFTR(SIGNIF_BIT_MASK, Exponent - 64)
    ELSE
        MaskHi = SHIFTR(SIGNIF_BIT_MASK, Exponent)
        MaskLo = IOR(SHIFTR(SIGNIF_BIT_MASK, Exponent), &
                     SHIFTL(SHIFTL(SIGNIF_BIT_MASK, 1), 63 - Exponent))
    END IF

    IF ((IAND(MaskHi, InVal%MantHi) /= 0_kInt64).OR.(IAND(MaskLo, InVal%MantLo) /= 0_kInt64)) THEN
        OutVal%MantHi = IAND(OutVal%MantHi, NOT(MaskHi))
        OutVal%MantLo = IAND(OutVal%MantLo, NOT(MaskLo))
        IF (Sign*InVal > ZERO_F128) OutVal = OutVal + Sign
    END IF

    RETURN

END FUNCTION Float128_FloorOrCeiling

!******************************************************************************

END MODULE MBase_Float128

!******************************************************************************
