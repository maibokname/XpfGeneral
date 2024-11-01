
MODULE MClass_IntegerRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *IntegerRNG* type and its related routines.
!   The *IntegerRNG* type is an abstract PRNG type that directly extends
!   the *BaseRNG* type.   It defines additional methods for a so-called
!   *Integer* PRNG.  It also provides default implementations of some
!   deferred procedures required by a PRNG where other deferred procedures
!   must be implemented by its subtypes.  <br>
!   By design, the *IntegerRNG* type is provided as a base type for an
!   *Integer* PRNG whose main purpose is to produce a 32-bit integer random
!   number.   Therefore, all so-called *Integer* PRNGs are particularly
!   required to implement the *NextIntegerImpl* deferred procedure.

!** USE STATEMENTS:
    USE ISO_C_BINDING,      ONLY: C_LOC, C_F_POINTER, C_NULL_PTR
    USE MBase_Common
    USE MBase_MemHandlers,  ONLY: MemAlloc
    USE MBase_SInt128
    USE MBase_UInt128
    USE MClass_BaseRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: IntegerRNG
    ! auxiliary function
    PUBLIC :: I128_To_R128

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    !% a mask used for masking 32 upper bits of a 64-bit integer
    tSInt64, PARAMETER, PUBLIC  :: MaskL = ToInt64(Z'00000000FFFFFFFF')

!** DERIVED TYPE DEFINITIONS
    !> The *IntegerRNG* type is an abstract PRNG type that directly extends
    !  the *BaseRNG* type.  It is provided to aid the implementation of an
    !  *Integer* PRNG whose main purpose is to produce a 32-bit integer random
    !  number.  Therefore, all so-called *Integer* PRNGs should extend from
    !  this base type.
    TYPE, ABSTRACT, EXTENDS(BaseRNG)  :: IntegerRNG
        PRIVATE
        ! initial seed used to re-initialize the PRNG
        tSInt32,  ALLOCATABLE   :: InitSeed(:)
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----               Additional Deferred Procedure               -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a binding name of the *InitBase* deferred procedure. <br>
        !  This procedure is intentionally employed as a base initialization that
        !  all other initialization procedures should call this procedure.  Also,
        !  it is intended to be used internally by an implementor, not a user.
        PROCEDURE(InitBase), DEFERRED   :: BaseInit
        ! ---------------------------------------------------------------------
        ! -----               Deferred Procedures Implemented             -----
        ! ---------------------------------------------------------------------
        !> *InitWOSeedImpl* is a deferred procedure. <br>
        !  Use the *Initialize* method in place of the *InitWOSeedImpl* method to
        !  initialize the PRNG without specifying any seed(s).
        PROCEDURE       :: InitWOSeedImpl   => Default_InitNoSeed
        !> *ReInit* is a deferred procedure. <br>
        !  **Type-Bound Subroutine**: ReInit <br>
        !  **Purpose**:  To reset the PRNG to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL PRNG%ReInit()
        PROCEDURE       :: ReInit           => Default_ReInitialize
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl     => Default_NextLong
        !> *NextI128Impl* is a deferred procedure. <br>
        !  Use the *NextI128* method in place of the *NextI128Impl* method
        !  to generate a signed 128-bit integer number.
        PROCEDURE       :: NextI128Impl     => Default_NextI128
        !> *NextU128Impl* is a deferred procedure. <br>
        !  Use the *NextU128* method in place of the *NextU128Impl* method
        !  to generate an unsigned 128-bit integer number.
        PROCEDURE       :: NextU128Impl     => Default_NextU128
        !> *NextDoubleImpl* is a deferred procedure. <br>
        !  Use the *NextDouble* method in place of the *NextDoubleImpl* method
        !  to generate a 64-bit real number.
        PROCEDURE       :: NextDoubleImpl   => Default_NextDouble
        !> *NextQuadImpl* is a deferred procedure. <br>
        !  Use the *NextQuad* method in place of the *NextQuadImpl* method
        !  to generate a 128-bit real number.
        PROCEDURE       :: NextQuadImpl     => Default_NextQuad
        ! ---------------------------------------------------------------------
        ! -----               Public Procedures                           -----
        ! ---------------------------------------------------------------------
        !> *InitWSeedImpl* is a procedure to initialize the generator with
        !  specified seed(s).  Instances of the PRNG initialized with the same
        !  seed(s) in the same program should  produce identical sequences of
        !  values. <br>
        !  Use the *Initialize* method in place of the *InitWSeedImpl* method to
        !  initialize the PRNG with specified seed(s).
        PROCEDURE       :: InitWSeedImpl    => Default_InitWithSeeds
        !> **Type-Bound Subroutine**: CopySeed <br>
        !  **Purpose**:  To copy the initial seeds of the source PRNG to
        !                that of the destination PRNG. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcRNG%CopySeed(DstRNG) <br>
        !  **Usage**: This method is intended for internal use only.
        PROCEDURE       :: CopySeed         => Copy_InitSeed
        ! ---------------------------------------------------------------------
        ! -----               Generic Interface                           -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Initialize <br>
        !  **Purpose**:  To initialize the pseudo-random number generator. <br>
        !  **Usage**: <br>
        !   --->    CALL RNG%Initialize() <br>
        !   --->    CALL RNG%Initialize(Seeds)
        GENERIC         :: Initialize   => InitWSeedImpl
        ! ---------------------------------------------------------------------
    END TYPE IntegerRNG

!** INTERFACE DEFINITIONS:
    ! abstract interface for deferred procedure
    ABSTRACT INTERFACE
        !> InitBase is a deferred procedure to initialize the generator with
        !  specified seed(s).  Instances of the PRNG initialized with the same
        !  seed(s) in the same program should  produce identical sequences of
        !  values. <br>
        !  This procedure is intentionally employed as a base initialization that
        !  all other initialization procedures should call this procedure.  Also,
        !  it is intended to be used internally by an implementor, not a user.
        SUBROUTINE InitBase(RNG, Seed)
            IMPORT
            !% random number generator
            CLASS(IntegerRNG), INTENT(INOUT)    :: RNG
            !> seed(s) whose size depends on the PRNG to be initialized
            tSInt32,           INTENT(IN)       :: Seed(:)
        END SUBROUTINE
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Default_InitNoSeed(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize an 'IntegerRNG' object without specified seeds.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG  !! 'IntegerRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! allocate InitSeed used for re-initialization
    CALL MemAlloc(RNG%InitSeed, [1_kIndex])
    
    ! set InitSeed
    RNG%InitSeed = GetRandomSeed32() + GOLDEN_RATIO_32
    
    ! initialize the PRNG
    CALL RNG%BaseInit(RNG%InitSeed)
    
    RETURN

END SUBROUTINE Default_InitNoSeed

!******************************************************************************

SUBROUTINE Default_InitWithSeeds(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize an 'IntegerRNG' object with specified seeds.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG      !! 'IntegerRNG' object
    tSInt32,           INTENT(IN)       :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (SIZE(Seed, KIND=kIndex) >= 11_kIndex) THEN
        ! allocate InitSeed used for re-initialization
        CALL MemAlloc(RNG%InitSeed, [SIZE(Seed, KIND=kIndex)])
        ! set InitSeed
        RNG%InitSeed = Seed
    ELSE
        ! +++ a zero-sized array -> no valuable information available +++
        ! allocate InitSeed used for re-initialization
        CALL MemAlloc(RNG%InitSeed, [1_kIndex])
        ! set InitSeed
        RNG%InitSeed = GetRandomSeed32() + GOLDEN_RATIO_32
    END IF
    
    ! initialize the PRNG
    CALL RNG%BaseInit(RNG%InitSeed)
    
    RETURN

END SUBROUTINE Default_InitWithSeeds

!******************************************************************************

SUBROUTINE Default_ReInitialize(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To re-initialize an 'IntegerRNG' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG  !! 'IntegerRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (ALLOCATED(RNG%InitSeed)) THEN
        ! re-initialize the PRNG
        CALL RNG%BaseInit(RNG%InitSeed)
    ELSE
        ! initialize without seed
        CALL RNG%Initialize()
    END IF

    RETURN

END SUBROUTINE Default_ReInitialize

!******************************************************************************

SUBROUTINE Copy_InitSeed(SrcRng, DstRng)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy the InitSeed of the source to that of the destination.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntegerRNG), INTENT(IN)       :: SrcRng   !! the source
    CLASS(IntegerRNG), INTENT(INOUT)    :: DstRng   !! the destination

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! allocate InitSeed used for re-initialization
    CALL MemAlloc(DstRng%InitSeed, [SIZE(SrcRng%InitSeed, KIND=kIndex)])
    
    ! set InitSeed
    DstRng%InitSeed = SrcRng%InitSeed
    
    RETURN

END SUBROUTINE Copy_InitSeed

!******************************************************************************

FUNCTION Default_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG      !! 'IntegerRNG' object
    tSInt64                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    RandNum = IOR(SHIFTL(ToInt64(RNG%NextInteger()), 32), &
                  IAND(ToInt64(RNG%NextInteger()), MaskL))
    
    RETURN

END FUNCTION Default_NextLong

!******************************************************************************

FUNCTION Default_NextI128(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the signed 128-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG      !! 'IntegerRNG' object
    TYPE(SInt128)                       :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! construct SInt128 object from two 64-bit random numbers
    RandNum = SInt128(RNG%NextLong(), RNG%NextLong())
    
    RETURN

END FUNCTION Default_NextI128

!******************************************************************************

FUNCTION Default_NextU128(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the unsigned 128-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG      !! 'IntegerRNG' object
    TYPE(UInt128)                       :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! construct UInt128 object from two 64-bit random numbers
    RandNum = UInt128(RNG%NextLong(), RNG%NextLong())
    
    RETURN

END FUNCTION Default_NextU128

!******************************************************************************

FUNCTION Default_NextDouble(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a random 64-bit-floating-point value between zero (inclusive)
    !  and one (exclusive).
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG      !! 'IntegerRNG' object
    tRealDP                             :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! although these three parameters use different formulas, they are essentially the same.
    tRealDP, PARAMETER  :: DNorm1 = 2.0_kDouble**(-53)
    tRealDP, PARAMETER  :: DNorm2 = 1.0_kDouble/SHIFTL(1_kInt64, 53)
    tRealDP, PARAMETER  :: DNorm3 = 0.5_kDouble*EPSILON(1.0_kDouble)
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: High, Low

! FLOW
    
    ! Require the least significant 53-bits from a long.
    ! Join the most significant 26 from the first random integer
    ! with 27 from the second random integer.
    High = SHIFTL(ToInt64(SHIFTR(RNG%NextInteger(), 6)), 27)     ! 26-bits remain
    Low  = ToInt64(SHIFTR(RNG%NextInteger(), 5))                 ! 27-bits remain
    RandNum = IOR(High, Low)*DNorm1
    
    RETURN

END FUNCTION Default_NextDouble

!******************************************************************************

FUNCTION Default_NextQuad(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a random 128-bit-floating-point value between zero (inclusive)
    !  and one (exclusive).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IntegerRNG), INTENT(INOUT)    :: RNG      !! 'IntegerRNG' object
    tRealQP                             :: RandNum  !! random number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! although these two parameters use different formulas, they are essentially the same.
    tRealQP, PARAMETER  :: QNorm1 = 2.0_kQuad**(-113)
    tRealQP, PARAMETER  :: QNorm2 = 0.5_kQuad*EPSILON(1.0_kQuad)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: IntVal(4)
    tSInt32     :: Hi1, Hi2, Lo1, Lo2
    tUInt64     :: I128Hi, I128Lo
    tRealQP     :: R128

! FLOW
    
    ! get four random integer values
    Hi1 = SHIFTR(RNG%NextInteger(), 4)  ! use the most significant 28 bits
    Hi2 = SHIFTR(RNG%NextInteger(), 4)  ! use the most significant 28 bits
    Lo1 = SHIFTR(RNG%NextInteger(), 4)  ! use the most significant 28 bits
    Lo2 = SHIFTR(RNG%NextInteger(), 3)  ! use the most significant 29 bits
    
    ! join the most significant 57 bits of Low and 56 bits of High
    IntVal = 0
    CALL MVBITS(Lo2,  0, 29, IntVal(1),  0)
    CALL MVBITS(Lo1,  0,  3, IntVal(1), 29)
    CALL MVBITS(Lo1,  3, 25, IntVal(2),  0)
    CALL MVBITS(Hi2,  0,  7, IntVal(2), 25)
    CALL MVBITS(Hi2,  7, 21, IntVal(3),  0)
    CALL MVBITS(Hi1,  0, 11, IntVal(3), 21)
    CALL MVBITS(Hi1, 11, 17, IntVal(4),  0)
    
    ! get upper and lower 64 bits of 128-bit integer number
    I128Lo = IOR(SHIFTL(ToInt64(IntVal(2)), 32), ToInt64(IntVal(1)))    ! treated as unsigned
    I128Hi = IOR(SHIFTL(ToInt64(IntVal(4)), 32), ToInt64(IntVal(3)))    ! treated as signed
    
    ! convert 128-bit integer number to 128-bit real number
    R128 = I128_To_R128(I128Hi, I128Lo)

    ! normalize the 128-bit real random number
    ! Note: Although the above block treats 128-bit integer as signed number,
    !       it is always positive because only the lower 113 bits are set and
    !       the higher 15 bits are all zero.
    RandNum = R128*QNorm1
    
    RETURN

END FUNCTION Default_NextQuad

!******************************************************************************

FUNCTION I128_To_R128(I128Hi, I128Lo) RESULT(R128)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert a 128-bit integer value to a 128-bit real value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: I128Hi   !! upper 64 bits of the 128-bit integer
    tUInt64, INTENT(IN) :: I128Lo   !! lower 64 bits of the 128-bit integer
    tRealQP             :: R128     !! the 128-bit real

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! parameters for converting from 128-bit integer to 128-bit floating point number
    tRealQP, PARAMETER  :: TwoPow64     = 2.0_kQuad**64
    tUInt64, PARAMETER  :: TwoPow112(2) = [ 0_kInt64, 281474976710656_kInt64] ! SHIFTL(1, 112)
    tUInt32, PARAMETER  :: Mask         = ToInt32(Z'00007FFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical            :: Negative
    tSInt64             :: High
    tUInt64             :: Low
    tSInt32             :: S, Shift
    tUInt64             :: Exp
    TYPE(C_PTR)         :: cPtr     ! C pointer to IBits
    tRealQP, POINTER    :: fPtr     ! Fortran pointer to IBits
    tUInt64, TARGET     :: IBits(2)

! FLOW
    
    ! get sign flag and absolute values of components
    Negative = (I128Hi < 0_kInt64)
    IF (Negative) THEN
        High = NOT(I128Hi)
        IF (I128Lo == 0_kInt64) High = High + 1_kInt64
        Low  = NOT(I128Lo) + 1_kInt64
    ELSE
        High = I128Hi
        Low  = I128Lo
    END IF

    IF (High == 0_kInt64) THEN
        R128 = U64_To_R128(Low)
        IF (Negative) R128 = -R128
        RETURN
    END IF

    S = LEADZ(High)
    IF (S >= 15) THEN
        R128 = U64_To_R128(Low) + REAL(High, KIND=kQuad)*TwoPow64
        IF (Negative) R128 = -R128
        RETURN
    END IF

    ! Mask out the 113 MSBits
    Shift = 15 - S
    IBits(2) = SHIFTR(High, Shift)
    IBits(1) = IOR(SHIFTR(Low, Shift), SHIFTL(High, 64-Shift))

    ! get the binary exponent
    Exp = ToInt64(IAND(16510-S, Mask))   ! 16510 = 64 + 64 + 16383 - 1

    ! The leading bit is implicit, cancel it out to get the significand
    ! and also add the exponent
    IBits(1) = IEOR(IOR(SHIFTR(Low, Shift), SHIFTL(High, 64-Shift)), TwoPow112(1))
    IBits(2) = IOR(IEOR(SHIFTR(High, Shift), TwoPow112(2)), SHIFTL(Exp, 48))    ! 48 = 112 - 64

    !-----------------------------------------------------------------------!
    !+++++  Transfer output (R128 mapped to IBits using C_F_POINTER)   +++++!
    !-----------------------------------------------------------------------!
    IF (.NOT.IsLittleEndian) THEN
        ! big-endian so swap IBits(1) and IBits(2)
        BLOCK
            tUInt64 :: Tmp
            Tmp = IBits(1); IBits(1) = IBits(2); IBits(2) = Tmp
        END BLOCK
    END IF
    ! get a C pointer to IBits
    cPtr = C_LOC(IBits)
    ! associate a Fortran data pointer with the C pointer
    CALL C_F_POINTER(cPtr, fPtr)
    ! get a 128-bit real number equivalent to the 128-bit integer number
    R128 = fPtr

    ! check and add sign if needed
    IF (Negative) R128 = -R128

    ! nullify pointers
    NULLIFY(fPtr)
    cPtr = C_NULL_PTR
    
    RETURN

CONTAINS

    FUNCTION U64_To_R128(LongVal) RESULT(QuadVal)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To convert an unsigned 64-bit integer number to a 128-bit floating point number

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt64, INTENT(IN) :: LongVal  ! integer number treated as unsigned one
        tRealQP             :: QuadVal  ! floating point number

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    !** FLOW

        IF (LongVal >= 0_kInt64) THEN
            QuadVal = REAL(LongVal, KIND=kQuad)
        ELSE
            QuadVal = TwoPow64 + REAL(LongVal, KIND=kQuad)
        END IF

        RETURN

    END FUNCTION U64_To_R128

    !******************************************************************************

END FUNCTION I128_To_R128

!******************************************************************************

END MODULE MClass_IntegerRNG
    
!******************************************************************************
