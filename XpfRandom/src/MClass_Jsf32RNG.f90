
MODULE MClass_Jsf32RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Jsf32RNG* type and its related routines.
!   The *Jsf32RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *Jsf32RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on Bob Jenkins's small fast
!   non-cryptographic (32-bit variant) PRNG.  It has a state size of 128-bits.
!   The shortest period is expected to be about 2<sup>94</sup> and it is expected
!   that about one seed will run into another seed within 2<sup>64</sup> values. <br>
!   It is important to note that the *Jsf32* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://burtleburtle.net/bob/rand/smallprng.html">
!       A small non-cryptographic PRNG</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/JenkinsSmallFast32.html">
!       Apache Commons RNG: Class JenkinsSmallFast32</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Jsf32RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *Jsf32RNG* type is an *Integer* PRNG type based on the small
    !  fast non-cryptographic PRNG by Bob Jenkins.
    TYPE, EXTENDS(IntegerRNG)  :: Jsf32RNG
        PRIVATE
        ! the working states
        tSInt32     :: A, B, C, D
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => Jsf32RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => Jsf32RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => Jsf32RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => Jsf32RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE Jsf32RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Jsf32RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Jsf32RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Jsf32RNG), INTENT(INOUT)  :: RNG      !! 'Jsf32RNG' object
    tSInt32,         INTENT(IN)     :: Seed(:)  !! seed

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  PARAMETER :: K = ToInt32(Z'F1EA5EED')
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I
    tSInt32     :: RandNum

! FLOW

    ! set initial seed
    RNG%B = Seed(1)

    RNG%A = K
    RNG%C = RNG%B
    RNG%D = RNG%B
    
    ! warming up before using the output
    DO I = 1, 20
        RandNum = RNG%NextInteger()
    END DO

    RETURN

END SUBROUTINE Jsf32RNG_BaseInit

!******************************************************************************

FUNCTION Jsf32RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Jsf32RNG), INTENT(INOUT)  :: RNG      !! 'Jsf32RNG' object
    tSInt32                         :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: E

! FLOW

    E = RNG%A - RotateLeft(RNG%B, 27)
    RNG%A = IEOR(RNG%B, RotateLeft(RNG%C, 17))
    RNG%B = RNG%C + RNG%D
    RNG%C = RNG%D + E
    RNG%D = E + RNG%A
    RandNum = RNG%D
    
    RETURN

END FUNCTION Jsf32RNG_NextInteger

!******************************************************************************

FUNCTION Jsf32RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Jsf32RNG), INTENT(IN) :: RNG      !! 'Jsf32RNG' object
    tCharAlloc                  :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Jsf32RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Jsf32RNG_GetName

!******************************************************************************

FUNCTION Jsf32RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Jsf32RNG), INTENT(IN) :: RNG      !! 'Jsf32RNG' object
    tIndex                      :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 1
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Jsf32RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_Jsf32RNG
    
!******************************************************************************
