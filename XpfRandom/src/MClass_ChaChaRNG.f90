
MODULE MClass_ChaChaRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *ChaChaRNG* type and its related routines.
!   The *ChaChaRNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *ChaChaRNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the *ChaCha* algorithm.
!   The *ChaCha* algorithm is a member of the ChaCha family of stream ciphers
!   (a variant of the Salsa20 family of stream ciphers) designed by D.J.
!   Bernstein.  As a result, the *ChaCha* PRNG is a cryptographic-level PRNG
!   as the stream cypher on which it is based. <br>
!   It is important to note that the *ChaCha* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://cr.yp.to/chacha.html">The ChaCha family of stream ciphers</a> <br>
!   [2] <a href="https://github.com/vnmakarov/mum-hash/blob/master/src/chacha-prng.h">
!       Pseudo Random Number Generator (PRNG) based on ChaCha stream cipher.</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_UIntUtil,   ONLY: ToUnsignedLong
    USE MClass_BaseRNG
    USE MClass_IntegerRNG,   ONLY: MaskL
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: ChaChaRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! Size of the seed required
    tSInt32,  PARAMETER :: SEED_SIZE  = 5
    tSInt32,  PARAMETER :: STATE_SIZE = 16

!** DERIVED TYPE DEFINITIONS
    !> The *ChaChaRNG* type is a *Long* PRNG type based on the *ChaCha* algorithm
    !  by D.J. Bernstein.
    TYPE, EXTENDS(LongRNG)  :: ChaChaRNG
        PRIVATE
        !% current state and parts of the recently generated numbers.
        tUInt32 :: Input(STATE_SIZE)
        tUInt32 :: Output(STATE_SIZE)
        !% index into the output
        tIndex  :: Index
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: Update       => ChaChaRNG_Update
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => ChaChaRNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl     => ChaChaRNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => ChaChaRNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => ChaChaRNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE ChaChaRNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE ChaChaRNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'ChaChaRNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ChaChaRNG), INTENT(INOUT) :: RNG      !! 'ChaChaRNG' object
    tSInt64,          INTENT(IN)    :: Seed(:)  !! seed(s)

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! random prime numbers
    tSInt32,  PARAMETER :: Primes(4) = [ToInt32(Z'FA835867'), &
                                        ToInt32(Z'2086CA69'), &
                                        ToInt32(Z'1467C0FB'), &
                                        ToInt32(Z'638E2B99')]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: Seed0(SEED_SIZE)
    tUInt32     :: Key(8), IV(2)
    tIndex      :: I, J

! FLOW

    ! set initial seed
    CALL Fill_State(Seed, Seed0)
    
    ! set Key and IV
    J = 1
    DO I = 1, 4
        Key(J) = ToInt32(IAND(Seed0(I), MaskL))
        J = J + 1
        Key(J) = ToInt32(SHIFTR(Seed0(I), 32))
        J = J + 1
    END DO
    IV(1) = ToInt32(IAND(Seed0(5), MaskL))
    IV(2) = ToInt32(SHIFTR(Seed0(5), 32))

    ! set initial state
    RNG%Input(1:4) = Primes(1:4)
    DO I = 1, 8
        RNG%Input(I+4) = Key(I)
    END DO
    RNG%Input(13:14) = 0_kInt32
    RNG%Input(15:16) = IV(1:2)
    
    ! set index and counter
    RNG%Index   = 17_kIndex
    
    RETURN

END SUBROUTINE ChaChaRNG_BaseInit

!******************************************************************************

FUNCTION ChaChaRNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ChaChaRNG), INTENT(INOUT) :: RNG      !! 'ChaChaRNG' object
    tSInt64                         :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (RNG%Index > 16_kIndex) THEN
        ! reset index
        RNG%Index = 1_kIndex
        ! update current state
        CALL RNG%Update()
    END IF
    
    ! get random number
    RandNum = IOR(SHIFTL(ToUnsignedLong(RNG%Output(RNG%Index)), 32), &
                  ToUnsignedLong(RNG%Output(RNG%Index+1)))

    ! update index
    RNG%Index = RNG%Index + 2_kIndex
    
    RETURN

END FUNCTION ChaChaRNG_NextLong

!******************************************************************************

SUBROUTINE ChaChaRNG_Update(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To update the current state of the PRNG.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ChaChaRNG), INTENT(INOUT)  :: RNG  !! 'ChaChaRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! set output
    RNG%Output = RNG%Input
    
    ! transform output
    CALL ChaChaRNG_Salsa20(RNG%Output)
    
    ! add input to output
    RNG%Output = RNG%Output + RNG%Input
    
    ! update input
    RNG%Input(13) = RNG%Input(13) + 1
    IF (RNG%Input(13) == 0_kInt32) THEN
        ! If it is becoming zero we produced too many numbers by current PRNG.
        RNG%Input(14) = RNG%Input(14) + 1
    END IF

    RETURN

CONTAINS

    SUBROUTINE ChaChaRNG_Salsa20(State)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To perform ChaCha state transformation.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt32, INTENT(INOUT)  :: State(0:)

    !** SUBROUTINE MACRO DEFINITIONS:
#define RotateLeft(V,P)     ISHFTC(V,  P)
#define ChachaQuaterRound(A,B,C,D) \
    A = A + B; \
    D = RotateLeft(IEOR(D, A), 16); \
    C = C + D; \
    B = RotateLeft(IEOR(B, C), 12); \
    A = A + B; \
    D = RotateLeft(IEOR(D, A), 8); \
    C = C + D; \
    B = RotateLeft(IEOR(B, C), 7);

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: I

    !** FLOW

        DO I = 8, 1, -2
            ChachaQuaterRound(State(0), State(4), State(8),  State(12))
            ChachaQuaterRound(State(1), State(5), State(9),  State(13))
            ChachaQuaterRound(State(2), State(6), State(10), State(14))
            ChachaQuaterRound(State(3), State(7), State(11), State(15))
            ChachaQuaterRound(State(0), State(5), State(10), State(15))
            ChachaQuaterRound(State(1), State(6), State(11), State(12))
            ChachaQuaterRound(State(2), State(7), State(8),  State(13))
            ChachaQuaterRound(State(3), State(4), State(9),  State(14))
        END DO
    
        RETURN

#undef RotateLeft
#undef ChachaQuaterRound

    END SUBROUTINE ChaChaRNG_Salsa20

    !******************************************************************************

END SUBROUTINE ChaChaRNG_Update

!******************************************************************************

FUNCTION ChaChaRNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ChaChaRNG), INTENT(IN)    :: RNG      !! 'ChaChaRNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'ChaChaRNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION ChaChaRNG_GetName

!******************************************************************************

FUNCTION ChaChaRNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ChaChaRNG), INTENT(IN)    :: RNG      !! 'ChaChaRNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = SEED_SIZE
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION ChaChaRNG_GetSeedSize

!******************************************************************************

END MODULE MClass_ChaChaRNG
    
!******************************************************************************
