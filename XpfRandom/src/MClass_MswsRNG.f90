
MODULE MClass_MswsRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *MswsRNG* type and its related routines.
!   The *MswsRNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *MswsRNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the middle-square method
!   by John von Neumann where a Weyl sequence is used to run the middle
!   square.  It has a state size of 192-bits and the period of at least
!   2<sup>64</sup>. <br>
!   It is important to note that the *Msws* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/Middle-square_method">
!       Middle-square method</a> <br>
!   [2] <a href="https://arxiv.org/abs/1704.00358v3">B. Widynski. 2017.
!       Middle Square Weyl Sequence RNG</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/MiddleSquareWeylSequence.html">
!       Apache Commons RNG: Class MiddleSquareWeylSequence</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: MswsRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! Size of the seed array
    tSInt32,  PARAMETER :: SEED_SIZE = 3
    ! The default seed
    ! This has a high quality Weyl increment (containing many bit state transitions).
    tSInt64,  PARAMETER :: DEFAULT_SEED(1:SEED_SIZE) = [ToInt64(Z'012DE1BABB3C4104'), &
                           ToInt64(Z'C8161B4202294965'), ToInt64(Z'B5AD4ECEDA1CE2A9')]

!** DERIVED TYPE DEFINITIONS
    !> The *Mt32RNG* type is a *Long* PRNG type based on the Middle Square
    !  Weyl Sequence RNG by B. Widynski.
    TYPE, EXTENDS(LongRNG)  :: MswsRNG
        PRIVATE
        ! State of the generator
        tSInt64     :: X
        ! State of the Weyl sequence
        tSInt64     :: W
        ! Increment for the Weyl sequence.  This must be odd to ensure a full period.
        ! This is not final to support the restore functionality.
        tSInt64     :: S
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => MswsRNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl     => MswsRNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => MswsRNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => MswsRNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----                 Overridden Procedures                     -----
        ! ---------------------------------------------------------------------
        !> *InitWOSeedImpl* is a deferred procedure. <br>
        !  Use the *Initialize* method in place of the *InitWOSeedImpl* method to
        !  initialize the PRNG without specifying any seed(s).
        PROCEDURE       :: InitWOSeedImpl   => MswsRNG_InitNoSeed
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => MswsRNG_NextInteger
        ! ---------------------------------------------------------------------
    END TYPE MswsRNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE MswsRNG_InitNoSeed(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG without specified seeds.  <br>
    !  This procedure overrides the default implementation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MswsRNG), INTENT(INOUT)   :: RNG  !! 'MswsRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! initialize with default seeds
    CALL RNG%Initialize(DEFAULT_SEED)

    RETURN

END SUBROUTINE MswsRNG_InitNoSeed

!******************************************************************************

SUBROUTINE MswsRNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MswsRNG), INTENT(INOUT)   :: RNG      !! 'MswsRNG' object
    tSInt64,        INTENT(IN)      :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: SizeOfSeed
    tSInt64     :: Seed0(SEED_SIZE)

! FLOW
    
    ! set initial seed
    SizeOfSeed = SIZE(Seed)
    IF (SizeOfSeed < SEED_SIZE) THEN
        ! Complete the seed with a default to avoid low complexity Weyl increments
        Seed0(1:SizeOfSeed)           = Seed(1:SizeOfSeed)
        Seed0(SizeOfSeed+1:SEED_SIZE) = DEFAULT_SEED(SizeOfSeed+1:SEED_SIZE)
    ELSE
        Seed0 = Seed(1:SEED_SIZE)
    END IF

    ! set state variables
    RNG%X = Seed0(1)
    RNG%W = Seed0(2)
    ! Ensure the increment is odd to provide a maximal period Weyl sequence
    RNG%S = IOR(Seed0(3), 1_kInt64)

    RETURN

END SUBROUTINE MswsRNG_BaseInit

!******************************************************************************

FUNCTION MswsRNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MswsRNG), INTENT(INOUT)   :: RNG      !! 'MswsRNG' object
    tSInt32                         :: RandNum  !! random number
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    RNG%X = RNG%X * RNG%X
    RNG%W = RNG%W + RNG%S
    RNG%X = RNG%X + RNG%W
    RNG%X = IOR(SHIFTR(RNG%X, 32), SHIFTL(RNG%X, 32))
    RandNum = ToInt32(RNG%X)
    
    RETURN

END FUNCTION MswsRNG_NextInteger

!******************************************************************************

FUNCTION MswsRNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MswsRNG), INTENT(INOUT)   :: RNG      !! 'MswsRNG' object
    tSInt64                         :: RandNum  !! random number
    
!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER  :: Mask = ToInt64(Z'FFFFFFFF00000000')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: I1, I2

! FLOW

    RNG%X = RNG%X * RNG%X
    RNG%W = RNG%W + RNG%S
    RNG%X = RNG%X + RNG%W
    I1 = IAND(RNG%X, Mask)
    RNG%X = IOR(SHIFTR(RNG%X, 32), SHIFTL(RNG%X, 32))
    
    RNG%X = RNG%X * RNG%X
    RNG%W = RNG%W + RNG%S
    RNG%X = RNG%X + RNG%W
    I2 = SHIFTR(RNG%X, 32)
    RNG%X = IOR(I2, SHIFTL(RNG%X, 32))
    RandNum = IOR(I1, I2)
    
    RETURN

END FUNCTION MswsRNG_NextLong

!******************************************************************************

FUNCTION MswsRNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MswsRNG), INTENT(IN)  :: RNG      !! 'MswsRNG' object
    tCharAlloc                  :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'MswsRNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION MswsRNG_GetName

!******************************************************************************

FUNCTION MswsRNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MswsRNG), INTENT(IN)  :: RNG      !! 'MswsRNG' object
    tIndex                      :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = SEED_SIZE
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION MswsRNG_GetSeedSize

!******************************************************************************

END MODULE MClass_MswsRNG
    
!******************************************************************************
