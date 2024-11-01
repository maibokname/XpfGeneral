
MODULE MClass_LongRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *LongRNG* type and its related routines.
!   The *LongRNG* type is an abstract PRNG type that directly extends
!   the *BaseRNG* type.   It defines additional methods for a so-called
!   *Long* PRNG.  It also provides default implementations of some deferred
!   procedures required by a PRNG where other deferred procedures must be
!   implemented by its subtypes.  <br>
!   By design, the *LongRNG* type is provided as a base type for a *Long*
!   PRNG whose main purpose is to produce a 64-bit integer random number.
!   Therefore, all so-called *Long* PRNGs are particularly required to
!   implement the *NextLongImpl* deferred procedure.

!** USE STATEMENTS:
    USE ISO_C_BINDING,      ONLY: C_LOC, C_F_POINTER
    USE MBase_Common
    USE MBase_MemHandlers,  ONLY: MemAlloc
    USE MClass_BaseRNG
    USE MBase_SInt128
    USE MBase_UInt128
    USE MClass_IntegerRNG,  ONLY: I128_To_R128

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: LongRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *LongRNG* type is an abstract PRNG type that directly extends
    !  the *BaseRNG* type.  It is provided to aid the implementation of an
    !  *Long* PRNG whose main purpose is to produce a 64-bit integer random
    !  number.  Therefore, all so-called *Long* PRNGs should extend from
    !  this base type.
    TYPE, ABSTRACT, EXTENDS(BaseRNG)  :: LongRNG
        PRIVATE
        ! initial seed used to re-initialize the PRNG
        tSInt64, ALLOCATABLE    :: InitSeed(:)
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
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => Default_NextInteger
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
    END TYPE LongRNG

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
            CLASS(LongRNG), INTENT(INOUT)   :: RNG
            !> seed(s) whose size depends on the PRNG to be initialized
            tSInt64,        INTENT(IN)      :: Seed(:)
        END SUBROUTINE
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Default_InitNoSeed(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize an 'LongRNG' object without specified seeds.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LongRNG), INTENT(INOUT)   :: RNG  !! 'LongRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! allocate InitSeed used for re-initialization
    CALL MemAlloc(RNG%InitSeed, [1_kIndex])
    
    ! set InitSeed
    RNG%InitSeed = GetRandomSeed64() + GOLDEN_RATIO_64
    
    ! initialize the PRNG
    CALL RNG%BaseInit(RNG%InitSeed)
    
    RETURN

END SUBROUTINE Default_InitNoSeed

!******************************************************************************

SUBROUTINE Default_InitWithSeeds(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize an 'LongRNG' object with specified seeds.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LongRNG), INTENT(INOUT)   :: RNG      !! 'LongRNG' object
    tSInt64,        INTENT(IN)      :: Seed(:)  !! seed(s)

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
        RNG%InitSeed = GetRandomSeed64() + GOLDEN_RATIO_64
    END IF
    
    ! initialize the PRNG
    CALL RNG%BaseInit(RNG%InitSeed)
    
    RETURN

END SUBROUTINE Default_InitWithSeeds

!******************************************************************************

SUBROUTINE Default_ReInitialize(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To re-initialize an 'LongRNG' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LongRNG), INTENT(INOUT)   :: RNG  !! 'LongRNG' object

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
    CLASS(LongRNG), INTENT(IN)      :: SrcRng   !! the source
    CLASS(LongRNG), INTENT(INOUT)   :: DstRng   !! the destination

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

FUNCTION Default_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a random 32-bit-integer value.  This default implementation
    !  uses the 32 high-order bits from a call to the 'NextLong' procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LongRNG), INTENT(INOUT)   :: RNG      !! 'LongRNG' object
    tSInt32                         :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    RandNum = ToInt32(SHIFTR(RNG%NextLong(), 32))
    
    RETURN

END FUNCTION Default_NextInteger

!******************************************************************************

FUNCTION Default_NextI128(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the signed 128-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LongRNG), INTENT(INOUT)   :: RNG      !! 'LongRNG' object
    TYPE(SInt128)                   :: RandNum  !! random number

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
    CLASS(LongRNG), INTENT(INOUT)   :: RNG      !! 'LongRNG' object
    TYPE(UInt128)                   :: RandNum  !! random number

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
    !  and one (exclusive).  This default implementation uses the 53 high-order
    !  bits from a call to the 'NextLong' procedure.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LongRNG), INTENT(INOUT)   :: RNG      !! 'LongRNG' object
    tRealDP                         :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! although these three parameters use different formulas, they are essentially the same.
    tRealDP, PARAMETER  :: DNorm1 = 2.0_kDouble**(-53)
    tRealDP, PARAMETER  :: DNorm2 = 1.0_kDouble/SHIFTL(1_kInt64, 53)
    tRealDP, PARAMETER  :: DNorm3 = 0.5_kDouble*EPSILON(1.0_kDouble)
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    RandNum = SHIFTR(RNG%NextLong(), 11)*DNorm1
    
    RETURN

END FUNCTION Default_NextDouble

!******************************************************************************

FUNCTION Default_NextQuad(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return a random 128-bit-floating-point value between zero (inclusive)
    !  and one (exclusive).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(LongRNG), INTENT(INOUT)   :: RNG      !! 'LongRNG' object
    tRealQP                         :: RandNum  !! random number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    ! although these two parameters use different formulas, they are essentially the same.
    tRealQP, PARAMETER  :: QNorm1 = 2.0_kQuad**(-113)
    tRealQP, PARAMETER  :: QNorm2 = 0.5_kQuad*EPSILON(1.0_kQuad)
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: High, Low
    tUInt64     :: I128Hi, I128Lo
    tRealQP     :: R128

! FLOW
    
    ! get two long values
    High = RNG%NextLong()
    Low  = RNG%NextLong()
    
    ! join the most significant 57 bits of Low and 56 bits of High
    ! to get upper and lower 64 bits of 128-bit integer number
    I128Lo = 0_kInt64
    I128Hi = 0_kInt64
    CALL MVBITS(Low,   7, 57, I128Lo,  0)
    CALL MVBITS(High,  8,  7, I128Lo, 57)
    CALL MVBITS(High, 15, 49, I128Hi,  0)

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

END MODULE MClass_LongRNG
    
!******************************************************************************
