
MODULE MClass_Sip24RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Sip24RNG* type and its related routines.
!   The *Sip24RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *Sip24RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the *SipHash24* algorithm.
!   The *SipHash24* algorithm is a member of the *SipHash* family, which
!   is an add-rotate-xor (ARX) based family of pseudorandom functions by
!   by J.P. Aumasson and D.J. Bernstein. <br>
!   It is important to note that the *Sip24* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/SipHash">SipHash</a> <br>
!   [2] <a href="https://github.com/veorq/SipHash">SipHash: High-speed
!       secure pseudorandom function for short messages.</a> <br>
!   [3] <a href="https://github.com/vnmakarov/mum-hash/blob/master/src/sip24-prng.h">
!       Pseudo Random Number Generator (PRNG) based on SipHash24.</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Sip24RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! Size of the state vector
    tSInt32,  PARAMETER :: SEED_SIZE = 4

!** DERIVED TYPE DEFINITIONS
    !> The *Sip24RNG* type is a *Long* PRNG type based on the *SipHash24* algorithm
    !  by J.P. Aumasson and D.J. Bernstein.
    TYPE, EXTENDS(LongRNG)  :: Sip24RNG
        PRIVATE
        !% initial state
        tSInt64 :: InitState(SEED_SIZE)
        !% current state
        tSInt64 :: State(SEED_SIZE)
        !% index into the current state (position of the output)
        tIndex  :: Index
        !% counter (used as a current message)
        tSInt64 :: Counter
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: Update       => Sip24RNG_Update
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => Sip24RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl     => Sip24RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => Sip24RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => Sip24RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE Sip24RNG

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Sip24RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Sip24RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Sip24RNG), INTENT(INOUT)  :: RNG      !! 'Sip24RNG' object
    tSInt64,         INTENT(IN)     :: Seed(:)  !! seed(s)

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER  ::IV(4) = [ToInt64(Z'736F6D6570736575'), &
                                    ToInt64(Z'646F72616E646F6D'), &
                                    ToInt64(Z'6C7967656E657261'), &
                                    ToInt64(Z'7465646279746573')]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: Seed0(SEED_SIZE)
    tIndex      :: I

! FLOW

    ! set initial seed
    CALL Fill_State(Seed, Seed0)

    ! set initial state
    DO I = 1, SEED_SIZE
        RNG%InitState(I) = IEOR(Seed0(I), IV(I))
    END DO

    ! set index and counter
    RNG%Index   = 5_kIndex
    RNG%Counter = 0_kInt64

    RETURN

END SUBROUTINE Sip24RNG_BaseInit

!******************************************************************************

FUNCTION Sip24RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Sip24RNG), INTENT(INOUT)  :: RNG      !! 'Sip24RNG' object
    tSInt64                         :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (RNG%Index > 4_kIndex) THEN
        ! reset index
        RNG%Index = 1_kIndex
        ! update current state
        CALL RNG%Update()
        ! update counter
        RNG%Counter = RNG%Counter + 1
    END IF

    ! get random number
    RandNum = RNG%State(RNG%Index)

    ! update index
    RNG%Index = RNG%Index + 1_kIndex

    RETURN

END FUNCTION Sip24RNG_NextLong

!******************************************************************************

SUBROUTINE Sip24RNG_Update(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To update the current state of the PRNG.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Sip24RNG), INTENT(INOUT)  :: RNG  !! 'Sip24RNG' object

!** SUBROUTINE MACRO DEFINITIONS:
#define RotateLeft(V,P)     ISHFTC(V,  P)
#define HalfRound(A,B,C,D,S,T) \
    A = A + B; \
    C = C + D; \
    B = IEOR(RotateLeft(B, S), A); \
    D = IEOR(RotateLeft(D, T), C); \
    A = RotateLeft(A, 32);
#define DoubleRound(V0,V1,V2,V3) \
    HalfRound(V0, V1, V2, V3, 13, 16); \
    HalfRound(V2, V1, V0, V3, 17, 21); \
    HalfRound(V0, V1, V2, V3, 13, 16); \
    HalfRound(V2, V1, V0, V3, 17, 21);

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! reset current state to initial state
    RNG%State = RNG%InitState

    ! update current state
    RNG%State(3) = IEOR(RNG%State(3), RNG%Counter)
    ASSOCIATE(V0 => RNG%State(1), V1 => RNG%State(2), &
              V2 => RNG%State(3), V3 => RNG%State(4))
        DoubleRound(V0, V1, V2, V3)
        V0 = IEOR(V0, RNG%Counter)
        DoubleRound(V0, V1, V2, V3)
        V2 = IEOR(V2, ToInt64(Z'00000000000000FF'))
        DoubleRound(V0, V1, V2, V3)
        DoubleRound(V0, V1, V2, V3)
    END ASSOCIATE

    RETURN

#undef RotateLeft
#undef HalfRound
#undef DoubleRound

END SUBROUTINE Sip24RNG_Update

!******************************************************************************

FUNCTION Sip24RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Sip24RNG), INTENT(IN) :: RNG      !! 'Sip24RNG' object
    tCharAlloc                  :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'Sip24RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Sip24RNG_GetName

!******************************************************************************

FUNCTION Sip24RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Sip24RNG), INTENT(IN) :: RNG      !! 'Sip24RNG' object
    tIndex                      :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Size = SEED_SIZE
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Sip24RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_Sip24RNG

!******************************************************************************
