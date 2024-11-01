
MODULE MClass_BaseRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *BaseRNG* type and its related routines.
!   The *BaseRNG* type is an abstract type representing a pseudo random
!   number generator (PRNG).  It defines an application programming
!   interface (API) for uniformly-distributed random number generations.
!   Except for the *NextGaussian* and *NextExponential* methods, all other
!   so-called *Next* methods provided produce a sequence of random numbers
!   that follow a uniform distribution.  <br>
!   The *BaseRNG* type also provides a number of default implementations
!   of random number generations.  For better accuracy and performance,
!   these methods with a default implementation can be overridden.  All
!   other PRNG types should extend from this base type. <br>
!   By design, all pseudo random number generators should be initialized
!   before being used.  This means that the *Initialize* method must be
!   called before all other methods (with the exception of the *GetName*
!   and *GetSeedSize* methods) are used.  Otherwise, the generated output
!   sequence may not be a desirable one. <br>
!   The API and various implementations of this PRNG base type and many
!   of its subtypes are greatly influenced by those in the references
!   provided below. <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://commons.apache.org/proper/commons-rng/index.html">
!       Apache Commons RNG: Random Numbers Generators</a> <br>
!   [2] <a href="https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/random/package-summary.html">
!       Package: Java.Util.Random</a> <br>
!   [3] <a href="http://simul.iro.umontreal.ca/">Random Number Generators
!       by Pierre L'Ecuyer</a> <br>
!   [4] <a href="https://wp.csiro.au/alanmiller/random.html">Uniform Random Number Generation
!       by Alan J. Miller</a>

!** USE STATEMENTS:
    USE, INTRINSIC :: IEEE_ARITHMETIC
    USE MBase_Common
    USE MBase_ErrHandlers
    USE MBase_SIntUtil
    USE MBase_UInt128
    USE MBase_SInt128

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! base type for pseudo-random number generator
    PUBLIC :: BaseRNG
    ! parameters for a character set that may be used when calling the *NextString* method
    PUBLIC :: AlphaOnlyCap,    AlphaOnlyMix
    PUBLIC :: AlphaNumericCap, AlphaNumericMix
    PUBLIC :: DecimalString,   HexadecimalString
    ! helper parameters
    PUBLIC :: GOLDEN_RATIO_32, GOLDEN_RATIO_64
    PUBLIC :: SILVER_RATIO_32, SILVER_RATIO_64
    ! helper procedures
    PUBLIC :: GetRandomSeed32, GetRandomSeed64
    PUBLIC :: Mix_Stafford_13, Mix_Murmur, Mix_Lea
    PUBLIC :: ScrambleWell,    Fill_State, Extend_Seed
    ! procedure interfaces
    PUBLIC :: NextR64

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#define     tSInt128    TYPE(SInt128)
#define     tUInt128    TYPE(UInt128)

!** MODULE PARAMETERS:
    ! -----------------------------------------------------------------------------------
    ! -----                     Private Parameters                                  -----
    ! -----------------------------------------------------------------------------------
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_BaseRNG'
    ! +++ character sets +++
    tCharParam  :: SET_ALPHABETS_LOW = 'abcdefghijklmnopqrstuvwxyz'
    tCharParam  :: SET_DEC_DIGITS = '0123456789'
    tCharParam  :: SET_HEX_DIGITS = SET_DEC_DIGITS // 'ABCDEF'
    tCharParam  :: SET_ALPHABETS_CAP = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    tCharParam  :: SET_ALPHABETS_MIX = SET_ALPHABETS_LOW // SET_ALPHABETS_CAP
    tCharParam  :: SET_ALPHANUM_CAP  = SET_ALPHABETS_CAP // SET_DEC_DIGITS
    tCharParam  :: SET_ALPHANUM_MIX  = SET_ALPHABETS_LOW // SET_DEC_DIGITS // SET_ALPHABETS_CAP
    ! -----------------------------------------------------------------------------------
    ! -----                     Public Parameters                                   -----
    ! -----------------------------------------------------------------------------------
    !% The first 32 bits of the golden ratio (1+sqrt(5))/2, forced to be odd.
    !  Useful for producing good *Weyl* sequences or as an arbitrary nonzero odd value.
    tSInt32,   PARAMETER    :: GOLDEN_RATIO_32 = ToInt32(Z'9E3779B9')
    !% The first 64 bits of the golden ratio (1+sqrt(5))/2, forced to be odd.
    !  Useful for producing good *Weyl* sequences or as an arbitrary nonzero odd value.
    tSInt64,   PARAMETER    :: GOLDEN_RATIO_64 = ToInt64(Z'9E3779B97F4A7C15')
    !% The first 32 bits of the silver ratio 1+sqrt(2), forced to be odd.
    !  Useful for producing good *Weyl* sequences or as an arbitrary nonzero odd value.
    tSInt32,   PARAMETER    :: SILVER_RATIO_32 = ToInt32(Z'6A09E667')
    !% The first 64 bits of the silver ratio 1+sqrt(2), forced to be odd.
    !  Useful for producing good *Weyl* sequences or as an arbitrary nonzero odd value.
    tSInt64,   PARAMETER    :: SILVER_RATIO_64 = ToInt64(Z'6A09E667F3BCC909')
    ! +++ parameters for character-string type +++
    tSInt32,   PARAMETER    :: AlphaOnlyCap      = 1 !! upper-case alphabet
    tSInt32,   PARAMETER    :: AlphaOnlyMix      = 2 !! mixed-case alphabet
    tSInt32,   PARAMETER    :: AlphaNumericCap   = 3 !! upper-case alphabet + decimal number
    tSInt32,   PARAMETER    :: AlphaNumericMix   = 4 !! mixed-case alphabet + decimal number
    tSInt32,   PARAMETER    :: DecimalString     = 5 !! decimal (number) string
    tSInt32,   PARAMETER    :: HexadecimalString = 6 !! hexadecimal (number) string

!** DERIVED TYPE DEFINITIONS
    !> The *BaseRNG* type is an abstract PRNG type that provides an API
    !  for random number generations.  Some random number generations are
    !  deferred while others (with default implementation) can be overridden.
    TYPE, ABSTRACT  :: BaseRNG
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *InitWOSeedImpl* is a binding name of the *InitRNG* deferred procedure. <br>
        !  Use the *Initialize* method in place of the *InitWOSeedImpl* method to
        !  initialize the PRNG without specifying any seed(s).
        PROCEDURE(InitRNG),  DEFERRED   :: InitWOSeedImpl
        !> *NextIntegerImpl* is a binding name of the *NextI32* deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE(NextI32),  DEFERRED   :: NextIntegerImpl
        !> *NextLongImpl* is a binding name of the *NextI64* deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE(NextI64),  DEFERRED   :: NextLongImpl
        !> *NextI128Impl* is a binding name of the *NextI128* deferred procedure. <br>
        !  Use the *NextI128* method in place of the *NextI128Impl* method
        !  to generate a signed 128-bit integer number.
        PROCEDURE(NextI128), DEFERRED   :: NextI128Impl
        !> *NextU128Impl* is a binding name of the *NextU128* deferred procedure. <br>
        !  Use the *NextU128* method in place of the *NextU128Impl* method
        !  to generate an unsigned 128-bit integer number.
        PROCEDURE(NextU128), DEFERRED   :: NextU128Impl
        !> *NextDoubleImpl* is a binding name of the *NextR64* deferred procedure. <br>
        !  Use the *NextDouble* method in place of the *NextDoubleImpl* method
        !  to generate a 64-bit real number.
        PROCEDURE(NextR64),  DEFERRED   :: NextDoubleImpl
        !> *NextQuadImpl* is a binding name of the *NextR128* deferred procedure. <br>
        !  Use the *NextQuad* method in place of the *NextQuadImpl* method
        !  to generate a 128-bit real number.
        PROCEDURE(NextR128), DEFERRED   :: NextQuadImpl
        !> *ReInit* is a binding name of the *Reset* deferred procedure. <br>
        !  **Type-Bound Subroutine**: ReInit <br>
        !  **Purpose**:  To reset the PRNG to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL PRNG%ReInit()
        PROCEDURE(Reset),    DEFERRED   :: ReInit
        !> *GetName* is a binding name of the *RNGName* deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE(RNGName),  DEFERRED   :: GetName
        !> *GetSeedSize* is a binding name of the *SeedSize* deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize the PRNG. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE(SeedSize), DEFERRED   :: GetSeedSize
        ! ---------------------------------------------------------------------
        ! ----- Default-Implementation Procedures with Generic Interfaces -----
        ! ---------------------------------------------------------------------
        !> Use the *NextInteger* Method instead of this method.
        PROCEDURE   :: Default_NextIntegerLimits
        !> Use the *NextLong* Method instead of this method.
        PROCEDURE   :: Default_NextLongLimits
        !> Use the *NextI128* Method instead of this method.
        PROCEDURE   :: Default_NextI128Limits
        !> Use the *NextU128* Method instead of this method.
        PROCEDURE   :: Default_NextU128Limits
        !> Use the *NextSingle* Method instead of this method.
        PROCEDURE   :: Default_NextSingle
        !> Use the *NextSingle* Method instead of this method.
        PROCEDURE   :: Default_NextSingleLimits
        !> Use the *NextDouble* Method instead of this method.
        PROCEDURE   :: Default_NextDoubleLimits
        !> Use the *NextQuad* Method instead of this method.
        PROCEDURE   :: Default_NextQuadLimits
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Initialize <br>
        !  **Purpose**:  To initialize the pseudo-random number generator. <br>
        !  **Usage**: <br>
        !   --->    CALL RNG%Initialize()
        GENERIC     :: Initialize       => InitWOSeedImpl
        !> **Type-Bound Function**: NextInteger  <br>
        !  **Purpose**:  To return a 32-bit integer random number. <br>
        !  **Usage**: <br>
        !           ! any random value <br>
        !   --->    I32Val = RNG%NextInteger() <br>
        !           ! random value between 0 and Limit1 <br>
        !   --->    I32Val = RNG%NextInteger(Limit1) <br>
        !           ! random value between Limit1 and Limit2 <br>
        !   --->    I32Val = RNG%NextInteger(Limit1, Limit2) <br>
        !  **Note**: Values of specified limits can either be positive or negative.
        GENERIC     :: NextInteger      => NextIntegerImpl,          &
                                           Default_NextIntegerLimits
        !> **Type-Bound Function**: NextLong <br>
        !  **Purpose**:  To return a 64-bit integer random number. <br>
        !  **Usage**: <br>
        !           ! any random value <br>
        !   --->    I64Val = RNG%NextLong() <br>
        !           ! random value between 0 and Limit1 <br>
        !   --->    I64Val = RNG%NextLong(Limit1) <br>
        !           ! random value between Limit1 and Limit2 <br>
        !   --->    I64Val = RNG%NextLong(Limit1, Limit2) <br>
        !  **Note**: Values of specified limits can either be positive or negative.
        GENERIC     :: NextLong         => NextLongImpl, &
                                           Default_NextLongLimits
        !> **Type-Bound Function**: NextIndex  <br>
        !  **Purpose**:  To return an integer random number representing an index. <br>
        !  **Usage**: <br>
        !           ! any random value <br>
        !   --->    Index = RNG%NextIndex() <br>
        !           ! random value between 0 and Limit1 <br>
        !   --->    Index = RNG%NextIndex(Limit1) <br>
        !           ! random value between Limit1 and Limit2 <br>
        !   --->    Index = RNG%NextIndex(Limit1, Limit2) <br>
        !  **Note**: The number can be either 32-bit or 64-bit integer depending on the configuration
        !            set when the code is compiled.
#ifdef Indx32Bits
        GENERIC     :: NextIndex        => NextIntegerImpl,          &
                                           Default_NextIntegerLimits
#else
        GENERIC     :: NextIndex        => NextLongImpl, &
                                           Default_NextLongLimits
#endif
        !> **Type-Bound Function**: NextI128 <br>
        !  **Purpose**:  To return a signed 128-bit integer random number. <br>
        !  **Usage**: <br>
        !           ! any random value <br>
        !   --->    I128Val = RNG%NextI128() <br>
        !           ! random value between 0 and Limit1 <br>
        !   --->    I128Val = RNG%NextI128(Limit1) <br>
        !           ! random value between Limit1 and Limit2 <br>
        !   --->    I128Val = RNG%NextI128(Limit1, Limit2) <br>
        !  **Note**: Values of specified limits can either be positive or negative.
        GENERIC     :: NextI128         => NextI128Impl, &
                                           Default_NextI128Limits
        !> **Type-Bound Function**: NextU128 <br>
        !  **Purpose**:  To return an unsigned 128-bit integer random number. <br>
        !  **Usage**: <br>
        !           ! any random value <br>
        !   --->    U128Val = RNG%NextU128() <br>
        !           ! random value between 0 and Limit1 <br>
        !   --->    U128Val = RNG%NextU128(Limit1) <br>
        !           ! random value between Limit1 and Limit2 <br>
        !   --->    U128Val = RNG%NextU128(Limit1, Limit2) <br>
        !  **Note**: Values of specified limits can either be positive or negative.
        GENERIC     :: NextU128         => NextU128Impl, &
                                           Default_NextU128Limits
        !> **Type-Bound Function**: NextSingle <br>
        !  **Purpose**:  To return a 32-bit real random number. <br>
        !  **Usage**: <br>
        !           ! random value between 0.0 (inclusive) and 1.0 (exclusive) <br>
        !   --->    R32Val = RNG%NextSingle() <br>
        !           ! random value between 0.0 and Limit1 <br>
        !   --->    R32Val = RNG%NextSingle(Limit1) <br>
        !           ! random value between Limit1 and Limit2 <br>
        !   --->    R32Val = RNG%NextSingle(Limit1, Limit2) <br>
        !  **Note**: Values of specified limits can either be positive or negative.
        GENERIC     :: NextSingle       => Default_NextSingle,       &
                                           Default_NextSingleLimits
        !> **Type-Bound Function**: NextDouble <br>
        !  **Purpose**:  To return a 64-bit real random number. <br>
        !  **Usage**: <br>
        !           ! random value between 0.0 (inclusive) and 1.0 (exclusive) <br>
        !   --->    R64Val = RNG%NextDouble() <br>
        !           ! random value between 0.0 and Limit1 <br>
        !   --->    R64Val = RNG%NextDouble(Limit1) <br>
        !           ! random value between Limit1 and Limit2 <br>
        !   --->    R64Val = RNG%NextDouble(Limit1, Limit2) <br>
        !  **Note**: Values of specified limits can either be positive or negative.
        GENERIC     :: NextDouble       => NextDoubleImpl,           &
                                           Default_NextDoubleLimits
        !> **Type-Bound Function**: NextQuad <br>
        !  **Purpose**:  To return a 128-bit real random number. <br>
        !  **Usage**: <br>
        !           ! random value between 0.0 (inclusive) and 1.0 (exclusive) <br>
        !   --->    R128Val = RNG%NextQuad() <br>
        !           ! random value between 0.0 and Limit1 <br>
        !   --->    R128Val = RNG%NextQuad(Limit1) <br>
        !           ! random value between Limit1 and Limit2 <br>
        !   --->    R128Val = RNG%NextQuad(Limit1, Limit2) <br>
        !  **Note**: Values of specified limits can either be positive or negative.
        GENERIC     :: NextQuad         => NextQuadImpl,             &
                                           Default_NextQuadLimits
        !> **Type-Bound Function**: NextString <br>
        !  **Purpose**:  To return a random character string. <br>
        !  **Usage**: <br>
        !           ! random string with default settings <br>
        !   --->    Str = RNG%NextString() <br>
        !           ! random string with a specified character set <br>
        !   --->    Str = RNG%NextString(AlphaOnlyCap) <br>
        !           ! random string with a specified length <br>
        !   --->    Str = RNG%NextString(StrLen=OutLen) <br>
        !           ! random string with a specified maximum length <br>
        !   --->    Str = RNG%NextString(MaxLen=MaxLen)
        PROCEDURE   :: NextString       => Default_NextString
        !> **Type-Bound Function**: NextLogical <br>
        !  **Purpose**:  To return a logical random value. <br>
        !  **Usage**: <br>
        !   --->    LogVal = RNG%NextLogical()
        PROCEDURE   :: NextLogical      => Default_NextLogical
        !> **Type-Bound Subroutine**: NextLogicalArray <br>
        !  **Purpose**:  To fill the specified array with logical random values. <br>
        !  **Usage**: <br>
        !   --->    CALL RNG%NextLogicalArray(RndArr)
        PROCEDURE   :: NextLogicalArray => Default_NextLogicalArray
        !> **Type-Bound Function**: NextByte <br>
        !  **Purpose**:  To return a 8-bit integer random number. <br>
        !  **Usage**: <br>
        !   --->    I8Val = RNG%NextByte()
        PROCEDURE   :: NextByte         => Default_NextByte
        !> **Type-Bound Subroutine**: NextByteArray <br>
        !  **Purpose**:  To fill the specified array with 8-bit integer random numbers. <br>
        !  **Usage**: <br>
        !   --->    CALL RNG%NextByteArray(RndArr)
        PROCEDURE   :: NextByteArray    => Default_NextByteArray
        !> **Type-Bound Function**: NextShort <br>
        !  **Purpose**:  To return a 16-bit integer random number. <br>
        !  **Usage**: <br>
        !   --->    I16Val = RNG%NextShort()
        PROCEDURE   :: NextShort        => Default_NextShort
        !> **Type-Bound Subroutine**: NextShortArray <br>
        !  **Purpose**:  To fill the specified array with 16-bit integer random numbers. <br>
        !  **Usage**: <br>
        !   --->    CALL RNG%NextShortArray(RndArr)
        PROCEDURE   :: NextShortArray   => Default_NextShortArray
        !> **Type-Bound Subroutine**: NextIntegerArray <br>
        !  **Purpose**:  To fill the specified array with 32-bit integer random numbers. <br>
        !  **Usage**: <br>
        !   --->    CALL RNG%NextIntegerArray(RndArr)
        PROCEDURE   :: NextIntegerArray => Default_NextIntegerArray
        !> **Type-Bound Subroutine**: NextLongArray <br>
        !  **Purpose**:  To fill the specified array with 64-bit integer random numbers. <br>
        !  **Usage**: <br>
        !   --->    CALL RNG%NextLongArray(RndArr)
        PROCEDURE   :: NextLongArray    => Default_NextLongArray
        !> **Type-Bound Subroutine**: NextI128Array <br>
        !  **Purpose**:  To fill the specified array with signed 128-bit integer random numbers. <br>
        !  **Usage**: <br>
        !   --->    CALL RNG%NextI128Array(RndArr)
        PROCEDURE   :: NextI128Array    => Default_NextI128Array
        !> **Type-Bound Subroutine**: NextU128Array <br>
        !  **Purpose**:  To fill the specified array with unsigned 128-bit integer random numbers. <br>
        !  **Usage**: <br>
        !   --->    CALL RNG%NextU128Array(RndArr)
!        PROCEDURE   :: NextU128Array    => Default_NextU128Array
        !> **Type-Bound Subroutine**: NextSingleArray <br>
        !  **Purpose**:  To fill the specified array with 32-bit real random numbers. <br>
        !  **Usage**: <br>
        !   --->    CALL RNG%NextSingleArray(RndArr)
        PROCEDURE   :: NextSingleArray  => Default_NextSingleArray
        !> **Type-Bound Subroutine**: NextDoubleArray <br>
        !  **Purpose**:  To fill the specified array with 64-bit real random numbers. <br>
        !  **Usage**: <br>
        !   --->    CALL RNG%NextDoubleArray(RndArr)
        PROCEDURE   :: NextDoubleArray  => Default_NextDoubleArray
        !> **Type-Bound Subroutine**: NextQuadArray <br>
        !  **Purpose**:  To fill the specified array with 128-bit real random numbers. <br>
        !  **Usage**: <br>
        !   --->    CALL RNG%NextQuadArray(RndArr)
        PROCEDURE   :: NextQuadArray    => Default_NextQuadArray
        !> **Type-Bound Function**: NextGaussian <br>
        !  **Purpose**:  To return a 64-bit real value pseudo-randomly chosen from
        !                a Gaussian (normal) distribution whose mean is 0.0 and whose
        !                standard deviation is 1.0. <br>
        !  **Usage**: <br>
        !   --->    R64Val = RNG%NextGaussian()
        PROCEDURE   :: NextGaussian     => Default_NextGaussian
        !> **Type-Bound Function**: NextExponential <br>
        !  **Purpose**:  To return a nonnegative 64-bit real value pseudo-randomly chosen
        !                from a exponential distribution whose mean is 1.0. <br>
        !  **Usage**: <br>
        !   --->    R64Val = RNG%NextExponential()
        PROCEDURE   :: NextExponential  => Default_NextExponential
        ! ---------------------------------------------------------------------
    END TYPE BaseRNG

!** INTERFACE DEFINITIONS:
    ! abstract interface for deferred procedures
    ABSTRACT INTERFACE
        !> InitRNG is a deferred procedure to initialize the generator without
        !  any specified seed(s).  The PRNG initialized with this procedure
        !  should  produce sequences of values that are statistically independent
        !  of those of any other instances in the current program execution,
        !  but may, and typically does, vary across program invocations. <br>
        SUBROUTINE InitRNG(RNG)
            IMPORT
            !% random number generator
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
        !> NextI32 is a deferred procedure to return a random 32-bit-integer value. <br>
        FUNCTION NextI32(RNG) RESULT(RandNum)
            IMPORT
            !% random number generator
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            !% random number generated
            tSInt32                         :: RandNum
        END FUNCTION
        !> NextI64 is a deferred procedure to return a random 64-bit-integer value. <br>
        FUNCTION NextI64(RNG) RESULT(RandNum)
            IMPORT
            !% random number generator
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            !% random number generated
            tSInt64                         :: RandNum
        END FUNCTION
        !> NextI128 is a deferred procedure to return a random signed 128-bit-integer value. <br>
        FUNCTION NextI128(RNG) RESULT(RandNum)
            IMPORT
            !% random number generator
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            !% random number generated
            tSInt128                        :: RandNum
        END FUNCTION
        !> NextU128 is a deferred procedure to return a random unsigned 128-bit-integer value. <br>
        FUNCTION NextU128(RNG) RESULT(RandNum)
            IMPORT
            !% random number generator
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            !% random number generated
            tUInt128                        :: RandNum
        END FUNCTION
        !> NextR64 is a deferred procedure to return a random 64-bit-real value. <br>
        FUNCTION NextR64(RNG) RESULT(RandNum)
            IMPORT
            !% random number generator
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            !% random number generated
            tRealDP                         :: RandNum
        END FUNCTION
        !> NextR128 is a deferred procedure to return a random 128-bit-real value. <br>
        FUNCTION NextR128(RNG) RESULT(RandNum)
            IMPORT
            !% random number generator
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            !% random number generated
            tRealQP                         :: RandNum
        END FUNCTION
        !> Reset is a deferred procedure to reset the generator to its initial state. <br>
        SUBROUTINE Reset(RNG)
            IMPORT
            !% random number generator
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
        END SUBROUTINE
        !> RNGName is a deferred procedure to return the name of the generator. <br>
        FUNCTION RNGName(RNG) RESULT(Name)
            IMPORT
            !% random number generator
            CLASS(BaseRNG), INTENT(IN)  :: RNG
            !% generator's name
            tCharAlloc                  :: Name
        END FUNCTION
        !> SeedSize is a deferred procedure to return size of specified
        !  seed(s) needed to initialize the generator. <br>
        FUNCTION SeedSize(RNG) RESULT(Size)
            IMPORT
            !% random number generator
            CLASS(BaseRNG), INTENT(IN)  :: RNG
            !% size of specified seed(s)
            tIndex                      :: Size
        END FUNCTION
    END INTERFACE
    ! interface for procedures in the SubMClass_Rng_Ziggurate submodule
    INTERFACE
        !> To return a 64-bit floating point value pseudo-randomly chosen from a Gaussian
        !  (normal) distribution whose mean is 0.0 and whose standard deviation is 1.0.
        MODULE FUNCTION Default_NextGaussian(RNG) RESULT(RandNum)
            !% random number generator
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            !% random number generated
            tRealDP                         :: RandNum
        END FUNCTION
        !> To return a nonnegative 64-bit floating point value pseudo-randomly chosen
        !  from a exponential distribution whose mean is 1.0. <br>
        MODULE FUNCTION Default_NextExponential(RNG) RESULT(RandNum)
            !% random number generator
            CLASS(BaseRNG), INTENT(INOUT)   :: RNG
            !% random number generated
            tRealDP                         :: RandNum
        END FUNCTION
    END INTERFACE
    ! interface for procedures in the SubMClass_Rng_Auxiliary submodule
    INTERFACE
        !> To compute Stafford variant 13 of the 64-bit mixing function of
        ! the MurmurHash3 hash function.
        MODULE FUNCTION Mix_Stafford_13(Input) RESULT(Output)
            tSInt64, INTENT(IN) :: Input
            tSInt64             :: Output
        END FUNCTION
        !% To transform the initial state of a generator.
        MODULE FUNCTION ScrambleWell(Seed, Add) RESULT(Output)
            tSInt64, INTENT(IN) :: Seed     !! seed element
            tSInt32, INTENT(IN) :: Add      !! offset
            tSInt64             :: Output   !! the transformed seed element
        END FUNCTION
    END INTERFACE

!** GENERIC DECLARATIONS:
    !% To get a 64-bit integer value representing a seed value.
    INTERFACE GetRandomSeed64
        !> To return a random 64-bit integer value that may be useful for
        !  initializing a source of seed value(s).
        MODULE FUNCTION Get_Random_Seed64() RESULT(Output)
            tSInt64             :: Output
        END FUNCTION
    END INTERFACE
    !% To get a 32-bit integer value representing a seed value.
    INTERFACE GetRandomSeed32
        !> To return a random 32-bit integer value that may be useful for
        !  initializing a source of seed value(s).
        MODULE FUNCTION Get_Random_Seed32() RESULT(Output)
            tSInt32             :: Output
        END FUNCTION
    END INTERFACE
    !% To compute the mixing function of the MurmurHash3 hash function.
    INTERFACE Mix_Murmur
        !% To compute the 32-bit mixing function of the MurmurHash3 hash function.
        MODULE FUNCTION Mix_Murmur_32(Input) RESULT(Output)
            tSInt32, INTENT(IN) :: Input
            tSInt32             :: Output
        END FUNCTION
        !% To compute the 64-bit mixing function of the MurmurHash3 hash function.
        MODULE FUNCTION Mix_Murmur_64(Input) RESULT(Output)
            tSInt64, INTENT(IN) :: Input
            tSInt64             :: Output
        END FUNCTION
    END INTERFACE
    !% To compute Doug Lea's mixing function.
    INTERFACE Mix_Lea
        !% To compute Doug Lea's 32-bit mixing function.
        MODULE FUNCTION Mix_Lea_32(Input) RESULT(Output)
            tSInt32, INTENT(IN) :: Input
            tSInt32             :: Output
        END FUNCTION
        !% To compute Doug Lea's 64-bit mixing function.
        MODULE FUNCTION Mix_Lea_64(Input) RESULT(Output)
            tSInt64, INTENT(IN) :: Input
            tSInt64             :: Output
        END FUNCTION
    END INTERFACE
    !% To fill state(s) based on the given seed(s).
    INTERFACE Fill_State
        !% To fill 32-bit integer state(s) based on the given seed(s).
        MODULE SUBROUTINE Fill_State32(Seed, State)
            tSInt32, INTENT(IN)     :: Seed(0:)
            tSInt32, INTENT(OUT)    :: State(0:)
        END SUBROUTINE
        !% To fill 64-bit integer state(s) based on the given seed(s).
        MODULE SUBROUTINE Fill_State64(Seed, State)
            tSInt64, INTENT(IN)   :: Seed(0:)
            tSInt64, INTENT(OUT)  :: State(0:)
        END SUBROUTINE
    END INTERFACE
    !% To extend the seed if the length of SeedIn is less than that of SeedOut.
    INTERFACE Extend_Seed
        !% To extend the 32-bit integer seeds if the length of SeedIn is less than that of SeedOut.
        MODULE SUBROUTINE Extend_Seed32(SeedIn, SeedOut)
            tSInt32,  INTENT(IN)    :: SeedIn(0:)
            tSInt32,  INTENT(OUT)   :: SeedOut(0:)
        END SUBROUTINE
        !% To extend the 64-bit integer seeds if the length of SeedIn is less than that of SeedOut.
        MODULE SUBROUTINE Extend_Seed64(SeedIn, SeedOut)
            tSInt64, INTENT(IN)   :: SeedIn(0:)
            tSInt64, INTENT(OUT)  :: SeedOut(0:)
        END SUBROUTINE
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION Default_NextLogical(RNG) RESULT(RandVal)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a random logical value.  This default implementation
    !  uses the sign bit from a call to the 'NextInteger' procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      !! 'BaseRNG' object
    tLogical                        :: RandVal  !! random value

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandVal = (RNG%NextInteger() < 0)

    RETURN

END FUNCTION Default_NextLogical

!******************************************************************************

SUBROUTINE Default_NextLogicalArray(RNG, BoolArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To fill a user-supplied logical array with generated logical values
    !  based on calls to NextLogical().

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG          !! 'BaseRNG' object
    tLogical,       INTENT(OUT)     :: BoolArray(:) !! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(BoolArray)
        BoolArray(I) = RNG%NextLogical()
    END DO

    RETURN

END SUBROUTINE Default_NextLogicalArray

!******************************************************************************

FUNCTION Default_NextByte(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a random 8-bit-integer value.  This default implementation
    !  uses the 8 high-order bits from a call to the 'NextInteger' procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      !! 'BaseRNG' object
    tSInt8                          :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandNum = ToInt8(SHIFTR(RNG%NextInteger(), 24))

    RETURN

END FUNCTION Default_NextByte

!******************************************************************************

SUBROUTINE Default_NextByteArray(RNG, ByteArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To fill a user-supplied 8-bit-integer array with generated byte values
    !  pseudo-randomly chosen uniformly from the range of values between
    !  -128 (inclusive) and 127 (inclusive). <br>
    !  This default implementation generates random bytes from repeated
    !  calls to the 'NextLong' procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG              !! 'BaseRNG' object
    tSInt8,         INTENT(OUT)     :: ByteArray(0:)    !! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, ByteLen, WordLen, N
    tSInt64     :: RndLong

! FLOW

    ! initialize
    I = 0_kIndex
    ByteLen = SIZE(ByteArray, KIND=kIndex)
    WordLen = SHIFTA(ByteLen, 3)

    ! fill the byte array, 8 bytes at a time
    DO WHILE (WordLen > 0)
        WordLen = WordLen - 1
        RndLong = RNG%NextLong()
        N = 8
        DO WHILE (N > 0)
            N = N - 1
            ByteArray(I) = ToInt8(RndLong)
            I = I + 1
            RndLong = SHIFTR(RndLong, 8)
        END DO
    END DO

    ! fill the remaining bytes
    IF (I < ByteLen) THEN
        RndLong = RNG%NextLong()
        DO WHILE (I < ByteLen)
            ByteArray(I) = ToInt8(RndLong)
            I = I + 1
            RndLong = SHIFTR(RndLong, 8)
        END DO
    END IF

    RETURN

END SUBROUTINE Default_NextByteArray

!******************************************************************************

FUNCTION Default_NextShort(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a random 16-bit-integer value.  This default implementation
    !  uses the 16 high-order bits from a call to the 'NextInteger' procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      !! 'BaseRNG' object
    tSInt16                         :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandNum = ToInt16(SHIFTR(RNG%NextInteger(), 16))

    RETURN

END FUNCTION Default_NextShort

!******************************************************************************

SUBROUTINE Default_NextShortArray(RNG, ShortArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To fill a user-supplied 16-bit-integer array with generated short values
    !  pseudo-randomly chosen uniformly from the range of values between
    !  -32768 (inclusive) and 32767 (inclusive). <br>
    !  This default implementation generates random shorts from repeated
    !  calls to the 'NextLong' procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG              !! 'BaseRNG' object
    tSInt16,        INTENT(OUT)     :: ShortArray(0:)   !! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, ShortLen, WordLen, N
    tSInt64     :: RndLong

! FLOW

    ! initialize
    I = 0_kIndex
    ShortLen = SIZE(ShortArray, KIND=kIndex)
    WordLen  = SHIFTA(ShortLen, 2)

    ! fill the short array, 4 elements at a time
    DO WHILE (WordLen > 0)
        WordLen = WordLen - 1
        RndLong = RNG%NextLong()
        N = 4
        DO WHILE (N > 0)
            N = N - 1
            ShortArray(I) = ToInt16(RndLong)
            I = I + 1
            RndLong = SHIFTR(RndLong, 16)
        END DO
    END DO

    ! fill the remaining elements
    IF (I < ShortLen) THEN
        RndLong = RNG%NextLong()
        DO WHILE (I < ShortLen)
            ShortArray(I) = ToInt16(RndLong)
            I = I + 1
            RndLong = SHIFTR(RndLong, 16)
        END DO
    END IF

    RETURN

END SUBROUTINE Default_NextShortArray

!******************************************************************************

FUNCTION Default_NextIntegerLimits(RNG, Bound1, Bound2) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate a random 32-bit-integer value in the specified range.
    !  If *Bound2* is not specified, the value is in the range between
    !  0 and *Bound1*.  Otherwise, the value is in the range between
    !  *Bound1* and *Bound2*. <br>
    !  It should be noted that both *Bound1* and *Bound2* arguments can
    !  have either a positive or a negative value.  The returned value
    !  is always in the range between the lower limit (inclusive) and
    !  the upper limit (exclusive).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),    INTENT(INOUT)    :: RNG      !! 'BaseRNG' object
    tSInt32,           INTENT(IN)       :: Bound1   !! a required limit
    tSInt32, OPTIONAL, INTENT(IN)       :: Bound2   !! an optional limit
    tSInt32                             :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER    :: POW_32 = SHIFTL(1_kInt64, 32)
    tSInt64, PARAMETER    :: MaskL  = ToInt64(Z'FFFFFFFFFFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Lower, Diff, AddVal
    tSInt64     :: M, L, T

! FLOW

    ! check specified input and set Diff and Lower values
    IF (PRESENT(Bound2)) THEN
        Diff  = ABS(Bound1 - Bound2)
        Lower = MIN(Bound1, Bound2)
    ELSE
        Diff  = ABS(Bound1)
        Lower = MIN(Bound1, 0_kInt32)
    END IF

    ! return quickly if Diff is zero
    IF (Diff == 0_kInt32) THEN
        RandNum = Bound1
        RETURN
    END IF

    ! determine the AddVal based on Lemire (2019): Fast Random Integer Generation
    ! in an Interval (https://arxiv.org/abs/1805.10941)
    M = IAND(ToInt64(RNG%NextInteger()), MaskL) * Diff
    L = IAND(M, MaskL)
    IF (L < Diff) THEN
        ! 2^32 % N
        T = MOD(POW_32, ToInt64(Diff))
        DO WHILE (L < T)
            M = IAND(ToInt64(RNG%NextInteger()), MaskL) * ToInt64(Diff)
            L = IAND(M, MaskL)
        END DO
    END IF
    AddVal = ToInt32(SHIFTR(M, 32))

    ! get random number
    RandNum = Lower + AddVal

    RETURN

END FUNCTION Default_NextIntegerLimits

!******************************************************************************

SUBROUTINE Default_NextIntegerArray(RNG, IntegerArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To fill a user-supplied 32-bit-integer array with generated integer values
    !  based on calls to NextInteger().

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG              !! 'BaseRNG' object
    tSInt32,        INTENT(OUT)     :: IntegerArray(:)  !! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(IntegerArray)
        IntegerArray(I) = RNG%NextInteger()
    END DO

    RETURN

END SUBROUTINE Default_NextIntegerArray

!******************************************************************************

FUNCTION Default_NextLongLimits(RNG, Bound1, Bound2) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate a random 64-bit-integer value in the specified range.
    !  If *Bound2* is not specified, the value is in the range between
    !  0 and *Bound1*.  Otherwise, the value is in the range between
    !  *Bound1* and *Bound2*. <br>
    !  It should be noted that both *Bound1* and *Bound2* arguments can
    !  have either a positive or a negative value.  The returned value
    !  is always in the range between the lower limit (inclusive) and
    !  the upper limit (exclusive).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),    INTENT(INOUT)    :: RNG      !! 'BaseRNG' object
    tSInt64,           INTENT(IN)       :: Bound1   !! a required limit
    tSInt64, OPTIONAL, INTENT(IN)       :: Bound2   !! an optional limit
    tSInt64                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64 :: Lower, Diff
    tSInt64 :: Bits, AddVal

! FLOW

    ! check specified input and set Diff and Lower values
    IF (PRESENT(Bound2)) THEN
        Diff  = ABS(Bound1 - Bound2)
        Lower = MIN(Bound1, Bound2)
    ELSE
        Diff  = ABS(Bound1)
        Lower = MIN(Bound1, 0_kInt64)
    END IF

    ! return quickly if Diff is zero
    IF (Diff == 0_kInt64) THEN
        RandNum = Bound1
        RETURN
    END IF

    ! determine the AddVal
    DO
        Bits = SHIFTR(RNG%NextLong(), 1)
        AddVal  = MOD(Bits, Diff)
        IF (Bits - AddVal + (Diff - 1_kInt64) >= 0_kInt64) EXIT
    END DO

    ! get random number
    RandNum = Lower + AddVal

    RETURN

END FUNCTION Default_NextLongLimits

!******************************************************************************

SUBROUTINE Default_NextLongArray(RNG, LongArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To fill a user-supplied 64-bit-integer array with generated long values
    !  based on calls to NextLong().

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG          !! 'BaseRNG' object
    tSInt64,        INTENT(OUT)     :: LongArray(:) !! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(LongArray)
        LongArray(I) = RNG%NextLong()
    END DO

    RETURN

END SUBROUTINE Default_NextLongArray

!******************************************************************************

FUNCTION Default_NextI128Limits(RNG, Bound1, Bound2) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate a random signed 128-bit-integer value in the specified range.  If *Bound2*
    !  is not specified, the value is in the range between 0 and *Bound1*.  Otherwise, the
    !  value is in the range between *Bound1* and *Bound2*. <br>
    !  It should be noted that both *Bound1* and *Bound2* arguments can have either a positive
    !  or a negative value.  The returned value is always in the range between the lower limit
    !  (inclusive) and the upper limit (exclusive).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),     INTENT(INOUT)   :: RNG      !! 'BaseRNG' object
    tSInt128,           INTENT(IN)      :: Bound1   !! a required limit
    tSInt128, OPTIONAL, INTENT(IN)      :: Bound2   !! an optional limit
    tSInt128                            :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt128   :: Lower, Diff
    tSInt128   :: Bits, AddVal

! FLOW

    ! check specified input and set Diff and Lower values
    IF (PRESENT(Bound2)) THEN
        Diff  = ABS(Bound1 - Bound2)
        Lower = MIN(Bound1, Bound2)
    ELSE
        Diff  = ABS(Bound1)
        Lower = MIN(Bound1, ZeroI128)
    END IF

    ! return quickly if Diff is zero
    IF (Diff == ZeroI128) THEN
        RandNum = Bound1
        RETURN
    END IF

    ! determine the AddVal
    DO
        Bits = ShiftROnce(RNG%NextI128())
        AddVal  = MOD(Bits, Diff)
        IF (Bits - AddVal + (Diff - OneI128) >= ZeroI128) EXIT
    END DO

    ! get random number
    RandNum = Lower + AddVal

    RETURN

END FUNCTION Default_NextI128Limits

!******************************************************************************

SUBROUTINE Default_NextI128Array(RNG, I128Array)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To fill a user-supplied signed 128-bit-integer array with generated signed
    !  128-bit-integer values based on calls to NextI128().

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG          !! 'BaseRNG' object
    tSInt128,       INTENT(OUT)     :: I128Array(:) !! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(I128Array)
        I128Array(I) = RNG%NextI128()
    END DO

    RETURN

END SUBROUTINE Default_NextI128Array

!******************************************************************************

FUNCTION Default_NextU128Limits(RNG, Bound1, Bound2) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate a random unsigned 128-bit-integer value in the specified range.  If *Bound2*
    !  is not specified, the value is in the range between 0 and *Bound1*.  Otherwise, the
    !  value is in the range between *Bound1* and *Bound2*, where either one can be a lower
    !  or an upper limit.  The returned value is always in the range between the lower limit
    !  (inclusive) and the upper limit (exclusive).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),     INTENT(INOUT)   :: RNG      !! 'BaseRNG' object
    tUInt128,           INTENT(IN)      :: Bound1   !! a required limit
    tUInt128, OPTIONAL, INTENT(IN)      :: Bound2   !! an optional limit
    tUInt128                            :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt128   :: Lower, Diff
    tUInt128   :: Bits, AddVal

! FLOW

    ! check specified input and set Diff and Lower values
    IF (PRESENT(Bound2)) THEN
        IF (Bound1 .UGT. Bound2) THEN
            Diff  = Bound1 - Bound2
        ELSE
            Diff  = Bound2 - Bound1
        END IF
        Lower = MIN(Bound1, Bound2)
    ELSE
        Diff  = Bound1
        Lower = ZeroU128
    END IF

    ! return quickly if Diff is zero
    IF (Diff == ZeroU128) THEN
        RandNum = Bound1
        RETURN
    END IF

    ! determine the AddVal
    DO
        Bits = ShiftROnce(RNG%NextU128())
        AddVal  = UMOD(Bits, Diff)
        IF ((Bits + (Diff - OneU128)) .UGE. AddVal) EXIT
    END DO

    ! get random number
    RandNum = Lower + AddVal

    RETURN

END FUNCTION Default_NextU128Limits

!******************************************************************************

SUBROUTINE Default_NextU128Array(RNG, U128Array)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To fill a user-supplied unsigned 128-bit-integer array with generated unsigned
    !  128-bit-integer values based on calls to NextU128().

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG          !! 'BaseRNG' object
    tUInt128,       INTENT(OUT)     :: U128Array(:) !! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(U128Array)
        U128Array(I) = RNG%NextU128()
    END DO

    RETURN

END SUBROUTINE Default_NextU128Array

!******************************************************************************

FUNCTION Default_NextSingle(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a random 32-bit real value between zero (inclusive)
    !  and one (exclusive).  This default implementation uses the 24 high-order
    !  bits from a call to the 'NextInteger' procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG      !! 'BaseRNG' object
    tRealSP                         :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! although these three parameters use different formulas, they are essentially the same.
    tRealSP, PARAMETER  :: SNorm1 = 2.0_kSingle**(-24)
    tRealSP, PARAMETER  :: SNorm2 = 1.0_kSingle/SHIFTL(1, 24)
    tRealSP, PARAMETER  :: SNorm3 = 0.5_kSingle*EPSILON(1.0_kSingle)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandNum = SHIFTR(RNG%NextInteger(), 8)*SNorm1

    RETURN

END FUNCTION Default_NextSingle

!******************************************************************************

FUNCTION Default_NextSingleLimits(RNG, Bound1, Bound2) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate a random 32-bit real value in the specified range.
    !  If *Bound2* is not specified, the value is in the range between
    !  zero and *Bound1*.  Otherwise, the value is in the range between
    !  *Bound1* and *Bound2*. <br>
    !  It should be noted that both *Bound1* and *Bound2* arguments can
    !  have either a positive or a negative value.  The returned value
    !  is always in the range between the lower limit (inclusive) and
    !  the upper limit (exclusive).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),    INTENT(INOUT)    :: RNG      !! 'BaseRNG' object
    tRealSP,           INTENT(IN)       :: Bound1   !! a required limit
    tRealSP, OPTIONAL, INTENT(IN)       :: Bound2   !! an optional limit
    tRealSP                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tRealSP     :: Upper, Lower

! FLOW

    ! check for invalid input
    IF (.NOT.IEEE_IS_FINITE(Bound1)) THEN
        CALL Handle_ErrLevel('Default_NextSingleLimits', ModName, ErrWarning, &
                    'The *Bound1* value must be finite.')
        RandNum = Bound1
        RETURN
    END IF

    ! check specified input and set Upper and Lower values
    IF (PRESENT(Bound2)) THEN
        ! check for invalid input
        IF (.NOT.IEEE_IS_FINITE(Bound2)) THEN
            CALL Handle_ErrLevel('Default_NextSingleLimits', ModName, ErrWarning, &
                       'The *Bound2* value must be finite.')
            RandNum = Bound2
            RETURN
        END IF
        IF (Bound1 > Bound2) THEN
            Upper = Bound1
            Lower = Bound2
        ELSEIF (Bound1 < Bound2) THEN
            Upper = Bound2
            Lower = Bound1
        ELSE
            ! Bound1 = Bound2 so return Bound1
            RandNum = Bound1
            RETURN
        END IF
    ELSE
        IF (Bound1 > 0.0_kSingle) THEN
            Upper = Bound1
            Lower = 0.0_kSingle
        ELSEIF (Bound1 < 0.0_kSingle) THEN
            Upper = 0.0_kSingle
            Lower = Bound1
        ELSE
            ! Bound1 = 0 so return 0
            RandNum = 0.0_kSingle
            RETURN
        END IF
    END IF

    RandNum = RNG%NextSingle()

    ! This expression allows (Upper - Lower) to be infinite
    ! Lower + (Upper - Lower) * RandNum == Lower - Lower * RandNum + Upper * RandNum
    RandNum = (1.0_kSingle - RandNum) * Lower + RandNum * Upper

    IF (RandNum >= Upper) THEN
        ! correct rounding
#ifndef  __INTEL_COMPILER
        RandNum = IEEE_NEXT_DOWN_SP(Upper)
#else
        RandNum = IEEE_NEXT_DOWN(Upper)
#endif
    END IF

    RETURN

END FUNCTION Default_NextSingleLimits

!******************************************************************************

SUBROUTINE Default_NextSingleArray(RNG, SingleArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To fill a user-supplied 32-bit-floating-point array with generated
    !  single values based on calls to NextSingle().

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG              !! 'BaseRNG' object
    tRealSP,        INTENT(OUT)     :: SingleArray(:)   !! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(SingleArray)
        SingleArray(I) = RNG%NextSingle()
    END DO

    RETURN

END SUBROUTINE Default_NextSingleArray

!******************************************************************************

FUNCTION Default_NextDoubleLimits(RNG, Bound1, Bound2) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate a random 64-bit real value in the specified range.
    !  If *Bound2* is not specified, the value is in the range between
    !  zero and *Bound1*.  Otherwise, the value is in the range between
    !  *Bound1* and *Bound2*. <br>
    !  It should be noted that both *Bound1* and *Bound2* arguments can
    !  have either a positive or a negative value.  The returned value
    !  is always in the range between the lower limit (inclusive) and
    !  the upper limit (exclusive).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),    INTENT(INOUT)    :: RNG      !! 'BaseRNG' object
    tRealDP,           INTENT(IN)       :: Bound1   !! a required limit
    tRealDP, OPTIONAL, INTENT(IN)       :: Bound2   !! an optional limit
    tRealDP                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tRealDP     :: Upper, Lower

! FLOW

    ! check for invalid input
    IF (.NOT.IEEE_IS_FINITE(Bound1)) THEN
        CALL Handle_ErrLevel('Default_NextDoubleLimits', ModName, ErrWarning, &
                    'The *Bound1* value must be finite.')
        RandNum = Bound1
        RETURN
    END IF

    ! check specified input and set Upper and Lower values
    IF (PRESENT(Bound2)) THEN
        ! check for invalid input
        IF (.NOT.IEEE_IS_FINITE(Bound2)) THEN
            CALL Handle_ErrLevel('Default_NextDoubleLimits', ModName, ErrWarning, &
                       'The *Bound2* value must be finite.')
            RandNum = Bound2
            RETURN
        END IF
        IF (Bound1 > Bound2) THEN
            Upper = Bound1
            Lower = Bound2
        ELSEIF (Bound1 < Bound2) THEN
            Upper = Bound2
            Lower = Bound1
        ELSE
            ! Bound1 = Bound2 so return Bound1
            RandNum = Bound1
            RETURN
        END IF
    ELSE
        IF (Bound1 > 0.0_kDouble) THEN
            Upper = Bound1
            Lower = 0.0_kDouble
        ELSEIF (Bound1 < 0.0_kDouble) THEN
            Upper = 0.0_kDouble
            Lower = Bound1
        ELSE
            ! Bound1 = 0 so return 0
            RandNum = 0.0_kDouble
            RETURN
        END IF
    END IF

    RandNum = RNG%NextDouble()

    ! This expression allows (Upper - Lower) to be infinite
    ! Lower + (Upper - Lower) * RandNum == Lower - Lower * RandNum + Upper * RandNum
    RandNum = (1.0_kDouble - RandNum) * Lower + RandNum * Upper

    IF (RandNum >= Upper) THEN
        ! correct rounding
#ifndef  __INTEL_COMPILER
        RandNum = IEEE_NEXT_DOWN_DP(Upper)
#else
        RandNum = IEEE_NEXT_DOWN(Upper)
#endif
    END IF

    RETURN

END FUNCTION Default_NextDoubleLimits

!******************************************************************************

SUBROUTINE Default_NextDoubleArray(RNG, DoubleArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To fill a user-supplied 64-bit-floating-point array with generated
    !  single values based on calls to NextDouble().

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG              !! 'BaseRNG' object
    tRealDP,        INTENT(OUT)     :: DoubleArray(:)   !! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(DoubleArray)
        DoubleArray(I) = RNG%NextDouble()
    END DO

    RETURN

END SUBROUTINE Default_NextDoubleArray

!******************************************************************************

FUNCTION Default_NextQuadLimits(RNG, Bound1, Bound2) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate a random 128-bit real value in the specified range.
    !  If *Bound2* is not specified, the value is in the range between
    !  zero and *Bound1*.  Otherwise, the value is in the range between
    !  *Bound1* and *Bound2*. <br>
    !  It should be noted that both *Bound1* and *Bound2* arguments can
    !  have either a positive or a negative value.  The returned value
    !  is always in the range between the lower limit (inclusive) and
    !  the upper limit (exclusive).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),    INTENT(INOUT)    :: RNG      !! 'BaseRNG' object
    tRealQP,           INTENT(IN)       :: Bound1   !! a required limit
    tRealQP, OPTIONAL, INTENT(IN)       :: Bound2   !! an optional limit
    tRealQP                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tRealQP   :: Upper, Lower

! FLOW

    ! check for invalid input
    IF (.NOT.IEEE_IS_FINITE(Bound1)) THEN
        CALL Handle_ErrLevel('Default_NextQuadLimits', ModName, ErrWarning, &
                    'The *Bound1* value must be finite.')
        RandNum = Bound1
        RETURN
    END IF

    ! check specified input and set Upper and Lower values
    IF (PRESENT(Bound2)) THEN
        ! check for invalid input
        IF (.NOT.IEEE_IS_FINITE(Bound2)) THEN
            CALL Handle_ErrLevel('Default_NextQuadLimits', ModName, ErrWarning, &
                       'The *Bound2* value must be finite.')
            RandNum = Bound2
            RETURN
        END IF
        IF (Bound1 > Bound2) THEN
            Upper = Bound1
            Lower = Bound2
        ELSEIF (Bound1 < Bound2) THEN
            Upper = Bound2
            Lower = Bound1
        ELSE
            ! Bound1 = Bound2 so return Bound1
            RandNum = Bound1
            RETURN
        END IF
    ELSE
        IF (Bound1 > 0.0_kQuad) THEN
            Upper = Bound1
            Lower = 0.0_kQuad
        ELSEIF (Bound1 < 0.0_kQuad) THEN
            Upper = 0.0_kQuad
            Lower = Bound1
        ELSE
            ! Bound1 = 0 so return 0
            RandNum = 0.0_kQuad
            RETURN
        END IF
    END IF

    RandNum = RNG%NextQuad()

    ! This expression allows (Upper - Lower) to be infinite
    ! Lower + (Upper - Lower) * RandNum == Lower - Lower * RandNum + Upper * RandNum
    RandNum = (1.0_kQuad - RandNum) * Lower + RandNum * Upper

    IF (RandNum >= Upper) THEN
        ! correct rounding
#ifndef  __INTEL_COMPILER
        RandNum = IEEE_NEXT_DOWN_QP(Upper)
#else
        RandNum = IEEE_NEXT_DOWN(Upper)
#endif
    END IF

    RETURN

END FUNCTION Default_NextQuadLimits

!******************************************************************************

SUBROUTINE Default_NextQuadArray(RNG, QuadArray)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To fill a user-supplied 128-bit-floating-point array with generated
    !  single values based on calls to NextQuad().

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG), INTENT(INOUT)   :: RNG              !! 'BaseRNG' object
    tRealQP,        INTENT(OUT)     :: QuadArray(:)     !! random numbers

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW

    ! fill the array
    DO I = 1, SIZE(QuadArray)
        QuadArray(I) = RNG%NextQuad()
    END DO

    RETURN

END SUBROUTINE Default_NextQuadArray

!******************************************************************************

FUNCTION Default_NextString(RNG, StrType, StrLen, MaxLen) RESULT(RandStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate a random string (according to the specified optional
    !  input if they are present; otherwise, according default settings).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseRNG),     INTENT(INOUT)   :: RNG      !! 'BaseRNG' object
    tSInt32,  OPTIONAL, INTENT(IN)      :: StrType  !! type of string (1-6)
    tSInt32,  OPTIONAL, INTENT(IN)      :: StrLen   !! length of output string
    tSInt32,  OPTIONAL, INTENT(IN)      :: MaxLen   !! maximum length of output string; if StrLen present, this parameter is ignored
    tCharAlloc                          :: RandStr  !! random string

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: LenMax, OutLen, I, J
    tCharAlloc  :: CharacterSet
    tSInt32     :: CharSetLen

! FLOW

    LenMax = 0
    ! check input string length whether it is present and valid
    IF (PRESENT(StrLen)) THEN
        IF (StrLen > 0) THEN
            OutLen = StrLen
            LenMax = StrLen
        END IF
    END IF
    IF (LenMax == 0) THEN
        ! check input maximum length whether it is present and valid
        IF (PRESENT(MaxLen)) THEN
            IF (MaxLen > 0) LenMax = MaxLen
        END IF
        IF (LenMax == 0) LenMax = 100
        OutLen = RNG%NextInteger(0, LenMax)
    END IF

    ! determine character set
    IF (PRESENT(StrType)) THEN
        SELECT CASE (StrType)
        CASE (AlphaOnlyCap)
            CharacterSet = SET_ALPHABETS_CAP
        CASE (AlphaOnlyMix)
            CharacterSet = SET_ALPHABETS_MIX
        CASE (AlphaNumericCap)
            CharacterSet = SET_ALPHANUM_CAP
        CASE (AlphaNumericMix)
            CharacterSet = SET_ALPHANUM_MIX
        CASE (DecimalString)
            CharacterSet = SET_DEC_DIGITS
        CASE (HexadecimalString)
            CharacterSet = SET_HEX_DIGITS
        CASE DEFAULT
            CharacterSet = SET_ALPHANUM_MIX
        END SELECT
    ELSE
        CharacterSet = SET_ALPHANUM_MIX
    END IF
    ! determine length of the character set
    CharSetLen = LEN(CharacterSet)

    ! generate random string
    ALLOCATE(CHARACTER(LEN=OutLen) :: RandStr)
    DO I = 1, OutLen
        J = RNG%NextInteger(1, CharSetLen)
        RandStr(I:I) = CharacterSet(J:J)
    END DO

    RETURN

END FUNCTION Default_NextString

!******************************************************************************

#ifndef  __INTEL_COMPILER

FUNCTION IEEE_NEXT_DOWN_SP(X) RESULT(XNext)

!** PURPOSE OF THIS SUBROUTINE:
    ! To emulate IEEE_NEXT_DOWN intrinsic function since GFortran does not have one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealSP, INTENT(IN) :: X
    tRealSP             :: XNext

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tRealSP     :: RVal
    tSInt32     :: IVal
    EQUIVALENCE(IVal, RVal)

! FLOW

    ! get input
    RVal = X
    ! get next down
    IF (IVal > 0) IVal = IVal - 1
    ! set output
    XNext = RVal

    RETURN

END FUNCTION IEEE_NEXT_DOWN_SP

!******************************************************************************

FUNCTION IEEE_NEXT_DOWN_DP(X) RESULT(XNext)

!** PURPOSE OF THIS SUBROUTINE:
    ! To emulate IEEE_NEXT_DOWN intrinsic function since GFortran does not have one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealDP, INTENT(IN) :: X
    tRealDP             :: XNext

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tRealDP :: RVal
    tSInt64 :: IVal
    EQUIVALENCE(IVal, RVal)

! FLOW

    ! get input
    RVal = X
    ! get next down
    IF (IVal > 0) IVal = IVal - 1
    ! set output
    XNext = RVal

    RETURN

END FUNCTION IEEE_NEXT_DOWN_DP

!******************************************************************************

FUNCTION IEEE_NEXT_DOWN_QP(X) RESULT(XNext)

!** PURPOSE OF THIS SUBROUTINE:
    ! To emulate IEEE_NEXT_DOWN intrinsic function since GFortran does not have one.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tRealQP, INTENT(IN) :: X
    tRealQP             :: XNext

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tRealQP :: RVal
    tSInt64 :: IVal(2)
    EQUIVALENCE(IVal, RVal)

! FLOW

    ! get input
    RVal = X
    ! assuming little-endian order; get next down
    ! (if big-endian order just replace IVal(2) by IVal(1) in the following statement)
    IF (IVal(2) > 0) IVal(2) = IVal(2) - 1
    ! set output
    XNext = RVal

    RETURN

END FUNCTION IEEE_NEXT_DOWN_QP

!******************************************************************************
#endif

END MODULE MClass_BaseRNG

!******************************************************************************
