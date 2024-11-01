
MODULE MClass_Jsf64RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Jsf64RNG* type and its related routines.
!   The *Jsf64RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *Jsf64RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on Bob Jenkins's small fast
!   non-cryptographic (64-bit variant) PRNG.  It has a state size of 256-bits. <br>
!   It is important to note that the *Jsf64* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://burtleburtle.net/bob/rand/smallprng.html">
!       A small non-cryptographic PRNG</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/JenkinsSmallFast64.html">
!       Apache Commons RNG: Class JenkinsSmallFast64</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Jsf64RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *Jsf32RNG* type is a *Long* PRNG type based on the small
    !  fast non-cryptographic PRNG by Bob Jenkins.
    TYPE, EXTENDS(LongRNG)  :: Jsf64RNG
        PRIVATE
        ! the working states
        tSInt64     :: A, B, C, D
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit     => Jsf64RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl => Jsf64RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName      => Jsf64RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize  => Jsf64RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE Jsf64RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Jsf64RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Jsf64RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Jsf64RNG), INTENT(INOUT)  :: RNG      !! 'Jsf64RNG' object
    tSInt64,         INTENT(IN)     :: Seed(:)  !! seed(s)

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, PARAMETER  :: K = ToInt64(Z'00000000F1EA5EED')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I
    tSInt64     :: RandNum

! FLOW

    ! set initial seed
    RNG%B = Seed(1)
    
    RNG%A = K
    RNG%C = RNG%B
    RNG%D = RNG%B
    
    ! warming up before using the output
    DO I = 1, 20
        RandNum = RNG%NextLong()
    END DO

    RETURN

END SUBROUTINE Jsf64RNG_BaseInit

!******************************************************************************

FUNCTION Jsf64RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Jsf64RNG), INTENT(INOUT)  :: RNG      !! 'Jsf64RNG' object
    tSInt64                         :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64  :: E

! FLOW

    E = RNG%A - RotateLeft(RNG%B, 7)
    RNG%A = IEOR(RNG%B, RotateLeft(RNG%C, 13))
    RNG%B = RNG%C + RotateLeft(RNG%D, 37)
    RNG%C = RNG%D + E
    RNG%D = E + RNG%A
    RandNum = RNG%D
    
    RETURN

END FUNCTION Jsf64RNG_NextLong

!******************************************************************************

FUNCTION Jsf64RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Jsf64RNG), INTENT(IN) :: RNG      !! 'Jsf64RNG' object
    tCharAlloc                  :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Jsf64RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Jsf64RNG_GetName

!******************************************************************************

FUNCTION Jsf64RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Jsf64RNG), INTENT(IN) :: RNG      !! 'Jsf64RNG' object
    tIndex                      :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 1
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Jsf64RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_Jsf64RNG
    
!******************************************************************************
