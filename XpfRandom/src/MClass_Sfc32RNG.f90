
MODULE MClass_Sfc32RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Sfc32RNG* type and its related routines.
!   The *Sfc32RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *Sfc32RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on the Small, Fast, Counting
!   (SFC) 32-bit generator by Chris Doty-Humphrey.  It has a state size of
!   128-bits. The period is a minimum of 2<sup>32</sup> and an average of
!   approximately 2<sup>127</sup>. <br>
!   It is important to note that the *Sfc32* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://pracrand.sourceforge.net/">PractRand Test Suite</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/DotyHumphreySmallFastCounting32.html">
!       Apache Commons RNG: Class DotyHumphreySmallFastCounting32</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Sfc32RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *Sfc32RNG* type is an *Integer* PRNG type based on the Small,
    !  Fast, Counting (SFC) 32-bit generator by Chris Doty-Humphrey.
    TYPE, EXTENDS(IntegerRNG)  :: Sfc32RNG
        PRIVATE
        ! the working states
        tSInt32     :: A, B, C
        ! the counter
        tSInt32     :: Counter
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => Sfc32RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => Sfc32RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => Sfc32RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => Sfc32RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE Sfc32RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Sfc32RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Sfc32RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Sfc32RNG), INTENT(INOUT)  :: RNG      !! 'Sfc32RNG' object
    tSInt32,         INTENT(IN)     :: Seed(:)  !! seed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Seed0(3)
    tSInt32     :: I, RandNum

! FLOW

    ! set initial seed
    CALL Fill_State(Seed, Seed0)
    
    ! set states and counter
    RNG%A = Seed0(1)
    RNG%B = Seed0(2)
    RNG%C = Seed0(3)
    RNG%Counter = 1

    ! warming up before using the output
    DO I = 1, 15
        RandNum = RNG%NextInteger()
    END DO

    RETURN

END SUBROUTINE Sfc32RNG_BaseInit

!******************************************************************************

FUNCTION Sfc32RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Sfc32RNG), INTENT(INOUT)  :: RNG      !! 'Sfc32RNG' object
    tSInt32                         :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandNum = RNG%A + RNG%B + RNG%Counter
    RNG%Counter = RNG%Counter + 1
    RNG%A = IEOR(RNG%B, SHIFTR(RNG%B, 9))
    RNG%B = RNG%C + SHIFTL(RNG%C, 3)
    RNG%C = RotateLeft(RNG%C, 21) + RandNum

    RETURN

END FUNCTION Sfc32RNG_NextInteger

!******************************************************************************

FUNCTION Sfc32RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Sfc32RNG), INTENT(IN) :: RNG      !! 'Sfc32RNG' object
    tCharAlloc                  :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Sfc32RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Sfc32RNG_GetName

!******************************************************************************

FUNCTION Sfc32RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Sfc32RNG), INTENT(IN) :: RNG      !! 'Sfc32RNG' object
    tIndex                      :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 3
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Sfc32RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_Sfc32RNG
    
!******************************************************************************
