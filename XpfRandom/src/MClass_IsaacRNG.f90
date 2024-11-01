
MODULE MClass_IsaccRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *IsaccRNG* type and its related routines.
!   The *IsaccRNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *IsaccRNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on the ISAAC (Indirection,
!   Shift, Accumulate, Add, and Count) algorithm.  the *IsaccRNG* type is
!   a fast cryptographic pseudo-random number generator due to the fact
!   that the ISAAC algorithm  has been designed to be cryptographically
!   secure and is inspired by the stream cipher *RC4*. <br>
!   Cycles of generated random sequences are guaranteed to be at least
!   2<sup>40</sup> values long, and they are 2<sup>8295</sup> values long
!   on average.   The results are uniformly distributed, unbiased, and
!   unpredictable unless the seed is known. <br>
!   It is important to note that the *ISAAC* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="http://burtleburtle.net/bob/rand/isaacafa.html">ISAAC: a fast
!       cryptographic pseudo-random number generator</a> <br>
!   [2] <a href="https://en.wikipedia.org/wiki/ISAAC_(cipher)">ISAAC (cipher)
!       - Wikipedia</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/ISAACRandom.html">
!       Apache Commons RNG: Class ISAACRandom</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: IsaccRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! Log of size of Rsl and Mem.
    tSInt32,  PARAMETER :: L_SIZE = 8
    ! Size of Rsl and Mem.
    tSInt32,  PARAMETER :: N_SIZE = SHIFTL(1, L_SIZE)
    ! Half-size of rsl() and mem().
    tSInt32,  PARAMETER :: H_SIZE = SHIFTA(N_SIZE, 1)
    ! For pseudo-random lookup.
    tSInt32,  PARAMETER :: MASK = SHIFTL(N_SIZE - 1, 2)
    ! The golden ratio.
    tSInt32,  PARAMETER :: GLD_RATIO = ToInt32(Z'9E3779B9')

!** DERIVED TYPE DEFINITIONS
    !> The *IsaccRNG* type is an *Integer* PRNG type based on the *ISAAC*
    !  (Indirection, Shift, Accumulate, Add, and Count) algorithm.
    TYPE, EXTENDS(IntegerRNG)  :: IsaccRNG
        PRIVATE
        ! The results given to the user.
        tSInt32     :: Rsl(0:N_SIZE-1)
        ! The internal state.
        tSInt32     :: Mem(0:N_SIZE-1)
        ! Count through the results in rsl().
        tSInt32     :: Count
        ! Accumulator.
        tSInt32     :: IsaacA
        ! The last result.
        tSInt32     :: IsaacB
        ! Counter, guarantees cycle is at least 2^40.
        tSInt32     :: IsaacC
        ! Service variable.
        tSInt32     :: IsaacX
        ! Service variable.
        tSInt32     :: IsaacI
        ! Service variable.
        tSInt32     :: IsaacJ
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => IsaccRNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => IsaccRNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => IsaccRNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => IsaccRNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE IsaccRNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE IsaccRNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'IsaccRNG' object with specified seed(s)
    !  where the maximum seed size is 256.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IsaccRNG), INTENT(INOUT)  :: RNG      !! 'IsaccRNG' object
    tSInt32,         INTENT(IN)     :: Seed(:)  !! seeds

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, PARAMETER  :: CPar = ToInt64(Z'000000006C078965')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Seed0(0:N_SIZE-1)
    tSInt32     :: MinLen, SeedLen
    tIndex      :: J
    tSInt64     :: K

! FLOW

    ! set initial seed
    SeedLen = SIZE(Seed)
    MinLen  = MIN(SeedLen, N_SIZE)
    Seed0(0:MinLen-1) = Seed(1:MinLen)

    ! fill the rest of the seed
    IF (SeedLen < N_SIZE) THEN
        DO J = SeedLen, N_SIZE-1
            K = Seed0(J - SeedLen)
            Seed0(J) = ToInt32(IAND(CPar * IEOR(K, SHIFTA(K, 30)) + J, MaskL))
        END DO
    END IF
    RNG%Rsl = Seed0
    
    CALL Initialize_State()

    RETURN
    
    CONTAINS

    SUBROUTINE Initialize_State()

    !** PURPOSE OF THIS SUBROUTINE:
        ! To initialize or re-initialize the states of the generator.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        ! na

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: Array(0:7)
        tSInt32     :: I, J

    ! FLOW
    
        RNG%IsaacA = 0
        RNG%IsaacB = 0
        RNG%IsaacC = 0
        Array = GLD_RATIO
        DO J = 0, 3
            CALL Shuffle(Array)
        END DO
        ! fill in mem() with messy stuff
        DO J = 0, N_SIZE-1, 8
            Array(0:7) = Array(0:7) + RNG%Rsl(J:J+7)
            CALL Shuffle(Array)
            ! set state
            RNG%Mem(J:J+7) = Array(0:7)
        END DO
        ! second pass makes all of seed affect all of mem
        DO J = 0, N_SIZE-1, 8
            Array(0:7) = Array(0:7) + RNG%Mem(J:J+7)
            CALL Shuffle(Array)
            ! set state
            RNG%Mem(J:J+7) = Array(0:7)
        END DO
        CALL ISAAC_Generate(RNG)
        RNG%Count = N_SIZE - 1

        RETURN

    END SUBROUTINE Initialize_State

    !******************************************************************************

    SUBROUTINE Shuffle(Array)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To shuffle the specified array

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(INOUT) :: Array(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW
    
        Array(0) = IEOR(Array(0), SHIFTL(Array(1), 11))
        Array(3) = Array(3) + Array(0)
        Array(1) = Array(1) + Array(2)
        Array(1) = IEOR(Array(1), SHIFTR(Array(2), 2))
        Array(4) = Array(4) + Array(1)
        Array(2) = Array(2) + Array(3)
        Array(2) = IEOR(Array(2), SHIFTL(Array(3), 8))
        Array(5) = Array(5) + Array(2)
        Array(3) = Array(3) + Array(4)
        Array(3) = IEOR(Array(3), SHIFTR(Array(4), 16))
        Array(6) = Array(6) + Array(3)
        Array(4) = Array(4) + Array(5)
        Array(4) = IEOR(Array(4), SHIFTL(Array(5), 10))
        Array(7) = Array(7) + Array(4)
        Array(5) = Array(5) + Array(6)
        Array(5) = IEOR(Array(5), SHIFTR(Array(6), 4))
        Array(0) = Array(0) + Array(5)
        Array(6) = Array(6) + Array(7)
        Array(6) = IEOR(Array(6), SHIFTL(Array(7), 8))
        Array(1) = Array(1) + Array(6)
        Array(7) = Array(7) + Array(0)
        Array(7) = IEOR(Array(7), SHIFTR(Array(0), 9))
        Array(2) = Array(2) + Array(7)
        Array(0) = Array(0) + Array(1)

        RETURN

    END SUBROUTINE Shuffle

    !******************************************************************************

END SUBROUTINE IsaccRNG_BaseInit

!******************************************************************************

FUNCTION IsaccRNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IsaccRNG), INTENT(INOUT)  :: RNG      !! 'IsaccRNG' object
    tSInt32                         :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (RNG%Count < 0) THEN
        CALL ISAAC_Generate(RNG)
        RNG%Count = N_SIZE - 1
    END IF
    RandNum = RNG%Rsl(RNG%Count)
    RNG%Count = RNG%Count - 1

    RETURN

END FUNCTION IsaccRNG_NextInteger

!******************************************************************************

FUNCTION IsaccRNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IsaccRNG), INTENT(IN) :: RNG      !! 'IsaccRNG' object
    tCharAlloc                  :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'IsaccRNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION IsaccRNG_GetName

!******************************************************************************

FUNCTION IsaccRNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IsaccRNG), INTENT(IN) :: RNG      !! 'IsaccRNG' object
    tIndex                      :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = N_SIZE
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION IsaccRNG_GetSeedSize

!******************************************************************************

SUBROUTINE ISAAC_Generate(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    ! To generate 256 results.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(IsaccRNG), INTENT(INOUT)  :: RNG  ! 'IsaccRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    RNG%IsaacI = 0
    RNG%IsaacJ = H_SIZE
    RNG%IsaacC = RNG%IsaacC + 1
    RNG%IsaacB = RNG%IsaacB + RNG%IsaacC
    DO WHILE (RNG%IsaacI < H_SIZE)
        CALL ISAAC_II()
    END DO
    RNG%IsaacJ = 0
    DO WHILE (RNG%IsaacJ < H_SIZE)
        CALL ISAAC_II()
    END DO
    
    RETURN

CONTAINS

    SUBROUTINE ISAAC_II()

        ! Intermediate internal loop
    
        RNG%IsaacX = RNG%Mem(RNG%IsaacI)
        RNG%IsaacA = IEOR(RNG%IsaacA, SHIFTL(RNG%IsaacA, 13))
        RNG%IsaacA = RNG%IsaacA + RNG%Mem(RNG%IsaacJ)
        RNG%IsaacJ = RNG%IsaacJ + 1
        CALL ISAAC_III()
        RNG%IsaacX = RNG%Mem(RNG%IsaacI)
        RNG%IsaacA = IEOR(RNG%IsaacA, SHIFTR(RNG%IsaacA, 6))
        RNG%IsaacA = RNG%IsaacA + RNG%Mem(RNG%IsaacJ)
        RNG%IsaacJ = RNG%IsaacJ + 1
        CALL ISAAC_III()
        RNG%IsaacX = RNG%Mem(RNG%IsaacI)
        RNG%IsaacA = IEOR(RNG%IsaacA, SHIFTL(RNG%IsaacA, 2))
        RNG%IsaacA = RNG%IsaacA + RNG%Mem(RNG%IsaacJ)
        RNG%IsaacJ = RNG%IsaacJ + 1
        CALL ISAAC_III()
        RNG%IsaacX = RNG%Mem(RNG%IsaacI)
        RNG%IsaacA = IEOR(RNG%IsaacA, SHIFTR(RNG%IsaacA, 16))
        RNG%IsaacA = RNG%IsaacA + RNG%Mem(RNG%IsaacJ)
        RNG%IsaacJ = RNG%IsaacJ + 1
        CALL ISAAC_III()
            
        RETURN

    END SUBROUTINE ISAAC_II

    !******************************************************************************

    SUBROUTINE ISAAC_III()

        ! Lowest level internal loop
    
        RNG%Mem(RNG%IsaacI) = RNG%Mem(SHIFTA(IAND(RNG%IsaacX, MASK), 2)) + RNG%IsaacA + &
                              RNG%IsaacB
        RNG%IsaacB = RNG%Mem(SHIFTA(IAND(SHIFTA(RNG%Mem(RNG%IsaacI), L_SIZE), MASK), 2)) + &
                     RNG%IsaacX
        RNG%Rsl(RNG%IsaacI) = RNG%IsaacB
        RNG%IsaacI = RNG%IsaacI + 1
            
        RETURN

    END SUBROUTINE ISAAC_III

    !******************************************************************************

END SUBROUTINE ISAAC_Generate

!******************************************************************************

END MODULE MClass_IsaccRNG
    
!******************************************************************************
