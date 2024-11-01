
MODULE MClass_PcgRxsMXs64RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *PcgRxsMXs64RNG* type and its related routines.
!   The *PcgRxsMXs64RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *PcgRxsMXs64RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on an algorithm from the Permuted
!   Congruential Generator (PCG) family that that use an internal 64-bit Linear
!   Congruential Generator (LCG) combined with the RXS-M-XS (random xorshift;
!   multiply; xorshift) output transformation to create 64-bit output. <br>
!   The *PcgRxsMXs64RNG* type has state size of 128 bits and a period of
!   2<sup>64</sup>. <br>
!   It is important to note that the *PcgRxsMXs64* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.pcg-random.org/">
!       PCG, A Family of Better Random Number Generators.</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/PcgRxsMXs64.html">
!       Apache Commons RNG: Class PcgRxsMXs64</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: PcgRxsMXs64RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *PcgLcg32RNG* type is a *Long* PRNG type based on an algorithm from
    !  the Permuted Congruential Generator (PCG) family that use an internal 64-bit
    !  Linear Congruential Generator (LCG) and output 64-bits per cycle.
    TYPE, EXTENDS(LongRNG)  :: PcgRxsMXs64RNG
        PRIVATE
        ! The state of the LCG
        tSInt64     :: State
        ! The increment of the LCG
        tSInt64     :: Increment
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => PcgRxsMXs64RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl     => PcgRxsMXs64RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => PcgRxsMXs64RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => PcgRxsMXs64RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE PcgRxsMXs64RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE PcgRxsMXs64RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'PcgRxsMXs64RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgRxsMXs64RNG), INTENT(INOUT)    :: RNG      !! 'PcgRxsMXs64RNG' object
    tSInt64,               INTENT(IN)       :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: Seed0(2)

! FLOW

    ! set initial seed
    CALL Fill_State(Seed, Seed0)
    
    ! Ensure the increment is odd to provide a maximal period LCG.
    RNG%Increment = IOR(SHIFTL(Seed0(2), 1), 1_kInt64)
    RNG%State     = Bump(RNG, Seed0(1) + RNG%Increment)

    RETURN

END SUBROUTINE PcgRxsMXs64RNG_BaseInit

!******************************************************************************

FUNCTION PcgRxsMXs64RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgRxsMXs64RNG), INTENT(INOUT)    :: RNG      !! 'PcgRxsMXs64RNG' object
    tSInt64                                 :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: X, Word

! FLOW
    
    X = RNG%State
    RNG%State = Bump(RNG, RNG%State)
    Word = IEOR(SHIFTR(X, (SHIFTR(X, 59) + 5)), X) * (-5840758589994634535_kInt64)
    RandNum = IEOR(SHIFTR(Word, 43), Word)

    RETURN

END FUNCTION PcgRxsMXs64RNG_NextLong

!******************************************************************************

FUNCTION PcgRxsMXs64RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgRxsMXs64RNG), INTENT(IN)   :: RNG      !! 'PcgRxsMXs64RNG' object
    tCharAlloc                          :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'PcgRxsMXs64RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION PcgRxsMXs64RNG_GetName

!******************************************************************************

FUNCTION PcgRxsMXs64RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgRxsMXs64RNG), INTENT(IN)   :: RNG      !! 'PcgRxsMXs64RNG' object
    tIndex                              :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 2
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION PcgRxsMXs64RNG_GetSeedSize

!******************************************************************************

FUNCTION Bump(RNG, Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To provide the next state of the LCG.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgRxsMXs64RNG), INTENT(IN)   :: RNG      !! 'PcgRxsMXs64RNG' object
    tSInt64,               INTENT(IN)   :: Input    !! current state
    tSInt64                             :: Output   !! next state

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = Input * 6364136223846793005_kInt64 + RNG%Increment
    
    RETURN

END FUNCTION Bump

!******************************************************************************
END MODULE MClass_PcgRxsMXs64RNG
    
!******************************************************************************
