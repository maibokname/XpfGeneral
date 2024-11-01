
MODULE MClass_Kiss32RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Kiss32RNG* type and its related routines.
!   The *Kiss32RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *Kiss32RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on the 32-bit *KISS* (Keep
!   it Simple Stupid) algorithm by George Marsaglia. <br>
!   The 32-bit *KISS* algorithm consists of a combination of four sub-generators,
!   each with 32 bits of state, of three kinds: <br>
!   - one linear congruential generator modulo 2<sup>32</sup> <br>
!   - one general binary linear generator over the vector space GF(2)<sup>32</sup> <br>
!   - two multiply-with-carry generators modulo 2<sup>16</sup>, with different
!     parameters <br>
!   The four generators are updated independently, and their states are combined
!   to form a sequence of 32-bit output words with period of about 2<sup>123</sup>. <br>
!   It is important to note that the *KISS* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="http://www.cse.yorku.ca/~oz/marsaglia-rng.html">
!       Random Number for C: End, at last?</a> <br>
!   [2] <a href="https://programmingpraxis.com/2010/10/05/george-marsaglias-random-number-generators/">
!       George Marsaglia's Random Number Generators</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/KISSRandom.html">
!       Apache Commons RNG: Class KISSRandom</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Kiss32RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *Kiss32RNG* type is an *Integer* PRNG type based on the *KISS*
    !  (Keep it Simple Stupid) algorithm by George Marsaglia.
    TYPE, EXTENDS(IntegerRNG)  :: Kiss32RNG
        PRIVATE
        ! the working states
        tSInt32     :: Z, W, Jsr, Jcong
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => Kiss32RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => Kiss32RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => Kiss32RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => Kiss32RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE Kiss32RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Kiss32RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kiss32RNG), INTENT(INOUT) :: RNG      !! 'Kiss32RNG' object
    tSInt32,          INTENT(IN)    :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Seed0(4)

! FLOW

    ! set initial seed
    CALL Fill_State(Seed, Seed0)

    RNG%Z     = Seed0(1)
    RNG%W     = Seed0(2)
    RNG%Jsr   = Seed0(3)
    RNG%Jcong = Seed0(4)

    RETURN

END SUBROUTINE Kiss32RNG_BaseInit

!******************************************************************************

FUNCTION Kiss32RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kiss32RNG), INTENT(INOUT) :: RNG      !! 'Kiss32RNG' object
    tSInt32                         :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: MWC

! FLOW

    ! compute new state values for MWC
    RNG%Z = 36969 * IAND(RNG%Z, 65535) + SHIFTR(RNG%Z, 16)
    RNG%W = 18000 * IAND(RNG%W, 65535) + SHIFTR(RNG%W, 16)
    
    ! +++ multiply-with-carry generators +++
    MWC = SHIFTL(RNG%Z, 16) + RNG%W

    ! +++ 3-shift-register generator +++
    ! With correction mentioned in the reference# 3 (swap 17 and 13 places)
    RNG%Jsr = IEOR(RNG%Jsr, SHIFTL(RNG%Jsr, 13))    ! original 17
    RNG%Jsr = IEOR(RNG%Jsr, SHIFTR(RNG%Jsr, 17))    ! original 13
    RNG%Jsr = IEOR(RNG%Jsr, SHIFTL(RNG%Jsr, 5))

    ! +++ congruential generator +++
    RNG%Jcong = 69069 * RNG%Jcong + 1234567

    ! +++ KISS generator +++
    RandNum = IEOR(MWC, RNG%Jcong) + RNG%Jsr

    RETURN

END FUNCTION Kiss32RNG_NextInteger

!******************************************************************************

FUNCTION Kiss32RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kiss32RNG), INTENT(IN)    :: RNG      !! 'Kiss32RNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Kiss32RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Kiss32RNG_GetName

!******************************************************************************

FUNCTION Kiss32RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kiss32RNG), INTENT(IN)    :: RNG      !! 'Kiss32RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 4
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Kiss32RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_Kiss32RNG
    
!******************************************************************************
