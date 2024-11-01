
MODULE MClass_Sfc64RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Sfc64RNG* type and its related routines.
!   The *Sfc64RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *Sfc64RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the Small, Fast, Counting
!   (SFC) 64-bit generator by Chris Doty-Humphrey.  It has a state size of
!   256-bits. The period is a minimum of 2<sup>64</sup> and an average of
!   approximately 2<sup>255</sup>. <br>
!   It is important to note that the *Sfc64* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://pracrand.sourceforge.net/">PractRand Test Suite</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/DotyHumphreySmallFastCounting64.html">
!       Apache Commons RNG: Class DotyHumphreySmallFastCounting64</a>

!** PURPOSE OF THIS MODULE:
    ! This module contains a random number generator class based on the Small, Fast,
    ! Counting (SFC) 32-bit generator of Chris Doty-Humphrey.
    ! The state size is 128-bits.  The period is a minimum of 2**32 and an
    ! average of approximately 2**127.

!** REFERENCES:
    ! [1] PractRand test suite.  http://pracrand.sourceforge.net

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Sfc64RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *Sfc64RNG* type is a *Long* PRNG type based on the Small,
    !  Fast, Counting (SFC) 64-bit generator by Chris Doty-Humphrey.
    TYPE, EXTENDS(LongRNG)  :: Sfc64RNG
        PRIVATE
        ! the working states
        tSInt64     :: A, B, C
        ! the counter
        tSInt64     :: Counter
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => Sfc64RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl     => Sfc64RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => Sfc64RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => Sfc64RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE Sfc64RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Sfc64RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Sfc64RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Sfc64RNG), INTENT(INOUT)  :: RNG      ! 'Sfc64RNG' object
    tSInt64,         INTENT(IN)     :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I
    tSInt64     :: Seed0(3), RandNum

! FLOW

    ! set initial seed
    CALL Fill_State(Seed, Seed0)
    
    RNG%A = Seed0(1)
    RNG%B = Seed0(2)
    RNG%C = Seed0(3)
    RNG%Counter = 1_kInt64

    ! warming up before using the output
    DO I = 1, 18
        RandNum = RNG%NextLong()
    END DO

    RETURN

END SUBROUTINE Sfc64RNG_BaseInit

!******************************************************************************

FUNCTION Sfc64RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Sfc64RNG), INTENT(INOUT)  :: RNG      !! 'Sfc64RNG' object
    tSInt64                         :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandNum = RNG%A + RNG%B + RNG%Counter
    RNG%Counter = RNG%Counter + 1
    RNG%A = IEOR(RNG%B, SHIFTR(RNG%B, 11))
    RNG%B = RNG%C + SHIFTL(RNG%C, 3)
    RNG%C = RotateLeft(RNG%C, 24) + RandNum

    RETURN

END FUNCTION Sfc64RNG_NextLong

!******************************************************************************

FUNCTION Sfc64RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Sfc64RNG), INTENT(IN) :: RNG      !! 'Sfc64RNG' object
    tCharAlloc                  :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Sfc64RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Sfc64RNG_GetName

!******************************************************************************

FUNCTION Sfc64RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Sfc64RNG), INTENT(IN) :: RNG      !! 'Sfc64RNG' object
    tIndex                      :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 3
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Sfc64RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_Sfc64RNG
    
!******************************************************************************
