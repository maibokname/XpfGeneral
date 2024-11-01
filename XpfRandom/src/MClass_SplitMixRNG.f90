
MODULE MClass_SplitMixRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SplitMixRNG* type and its related routines.
!   The *SplitMixRNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *SplitMixRNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the *SplitMix* algorithm for
!   which is an algorithm for a splittable PRNG.  It is a fast RNG, with
!   64 bits of state, that can be used to initialize the state of other PRNGs. <br>
!   It is important to note that the *SplitMix* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://dl.acm.org/doi/10.1145/2714064.2660195">
!       Guy L. Steele, Jr., Doug Lea, and Christine H. Flood. 2014. Fast splittable
!       pseudorandom number generators. ACM SIGPLAN Notices, Vol. 49, No. 10, pp 453-472.</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/SplitMix64.html">
!       Apache Commons RNG: Class SplitMix64</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: SplitMixRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *SplitMixRNG* type is a *Long* PRNG type based on the
    !  *SplitMix* algorithm by Steele, Lea, and Flood.
    TYPE, EXTENDS(LongRNG)  :: SplitMixRNG
        PRIVATE
        tSInt64     :: State    ! working seed
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit     => SplitMixRNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl => SplitMixRNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName      => SplitMixRNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize  => SplitMixRNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE SplitMixRNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE SplitMixRNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'SplitMixRNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SplitMixRNG), INTENT(INOUT)   :: RNG      !! 'SplitMixRNG' object
    tSInt64,            INTENT(IN)      :: Seed(:)  !! seed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! set initial state
    RNG%State = Seed(1)

    RETURN

END SUBROUTINE SplitMixRNG_BaseInit

!******************************************************************************

FUNCTION SplitMixRNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SplitMixRNG), INTENT(INOUT)   :: RNG      !! 'SplitMixRNG' object
    tSInt64                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! update state
    RNG%State = RNG%State + GOLDEN_RATIO_64
    
    ! mix state
    RandNum = Mix_Stafford_13(RNG%State)
    
    RETURN

END FUNCTION SplitMixRNG_NextLong

!******************************************************************************

FUNCTION SplitMixRNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SplitMixRNG), INTENT(IN)  :: RNG      !! 'SplitMixRNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'SplitMixRNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION SplitMixRNG_GetName

!******************************************************************************

FUNCTION SplitMixRNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SplitMixRNG), INTENT(IN)  :: RNG      !! 'SplitMixRNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 1
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION SplitMixRNG_GetSeedSize

!******************************************************************************

END MODULE MClass_SplitMixRNG
    
!******************************************************************************
