
MODULE MClass_Mt32RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Mt32RNG* type and its related routines.
!   The *Mt32RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *Mt32RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on the 32-bit Mersenne Twister
!   algorithm by Makoto Matsumoto and Takuji Nishimura.  The *Mt32* PRNG
!   features an extremely long period (2<sup>19937</sup> - 1) and
!   623-dimensional equidistribution up to 32 bits accuracy. <br>
!   It is important to note that the *Mt32* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/ARTICLES/mt.pdf">
!       M. Matsumoto and T. Nishimura.  1998.  Mersenne Twister: A 623-Dimensionally
!       Equidistributed Uniform Pseudo-Random Number Generator.  ACM Transactions on
!       Modeling and Computer Simulation, Vol. 8, No. 1, January 1998, pp 3-30.</a> <br>
!   [2] <a href="http://www.math.sci.hiroshima-u.ac.jp/m-mat/MT/MT2002/emt19937ar.html">
!       Mersenne Twister with improved initialization.</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/MersenneTwister.html">
!       Apache Commons RNG: Class MersenneTwister</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Mt32RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! Mask 32 most significant bits.
    tSInt64,  PARAMETER :: INT_MASK_LONG   = ToInt64(Z'00000000FFFFFFFF')
    ! Most significant w-r bits.
    tSInt64,  PARAMETER :: UPPER_MASK_LONG = ToInt64(Z'0000000080000000')
    ! Least significant r bits.
    tSInt64,  PARAMETER :: LOWER_MASK_LONG = ToInt64(Z'000000007FFFFFFF')
    ! Most significant w-r bits.
    tSInt32,  PARAMETER :: UPPER_MASK = ToInt32(Z'80000000')
    ! Least significant r bits.
    tSInt32,  PARAMETER :: LOWER_MASK = ToInt32(Z'7FFFFFFF')
    ! Size of the bytes pool.
    tSInt32,  PARAMETER :: N = 624
    ! Period second parameter.
    tSInt32,  PARAMETER :: M = 397
    ! X * MATRIX_A for X = {0, 1}.
    tSInt32,  PARAMETER :: MAG01(0:1) = [0, ToInt32(Z'9908B0DF')]

!** DERIVED TYPE DEFINITIONS
    !> The *Mt32RNG* type is an *Integer* PRNG type based on the Mersenne
    !  Twister algorithm by Makoto Matsumoto and Takuji Nishimura.
    TYPE, EXTENDS(IntegerRNG)  :: Mt32RNG
        PRIVATE
        ! bytes pool
        tSInt32     :: MT(0:N-1)
        ! current index in the bytes pool
        tSInt32     :: MTI
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => Mt32RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => Mt32RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => Mt32RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => Mt32RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE Mt32RNG

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

SUBROUTINE Mt32RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Mt32RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt32RNG), INTENT(INOUT)   :: RNG      !! 'Mt32RNG' object
    tSInt32,        INTENT(IN)      :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set initial states
    IF (SIZE(Seed) < N) THEN
        CALL FillStateVariable(Seed, RNG%MT)
    ELSE
        RNG%MT(0:N-1) = Seed(1:N)
    END IF

    ! initial index
    RNG%MTI = N

    RETURN

    CONTAINS

    SUBROUTINE FillStateVariable(Seed, State)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To fill the state variable based on the given seed.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(IN)    :: Seed(0:)
        tSInt32,  INTENT(OUT)   :: State(0:)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt64, PARAMETER  :: Mask = ToInt64(Z'00000000FFFFFFFF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: NextIndex

    ! FLOW

        CALL Initialize_State(State)

        NextIndex = Mix_Seed_N_State(Seed, State)

        CALL Mix_State(State, NextIndex)

        ! MSB is 1, ensuring non-zero initial array
        State(0) = ToInt32(UPPER_MASK_LONG)

        RETURN

    END SUBROUTINE FillStateVariable

    !******************************************************************************

    SUBROUTINE Initialize_State(State)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To fill the State using a defined pseudo-random sequence.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(OUT)   :: State(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt64     :: MT
        tIndex      :: I

    ! FLOW

        MT = IAND(19650218_kInt64, INT_MASK_LONG)
        State(0) = ToInt32(MT)
        DO I = 1, SIZE(State)-1
            MT = IAND((1812433253_kInt64 * IEOR(MT, SHIFTA(MT, 30)) + I), INT_MASK_LONG)
            State(I) = ToInt32(MT)
        END DO

        RETURN

    END SUBROUTINE Initialize_State

    !******************************************************************************

    FUNCTION Mix_Seed_N_State(Seed, State) RESULT(NextID)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To mix the seed into the state using a non-linear combination.  The procedure
        ! uses K steps where {K = MAX(SIZE(State), SIZE(Seed))}.  If the seed is smaller
        ! than the state it is wrapped to obtain enough values.  If the seed is larger
        ! than the state then the procedure visits entries in the state multiple times.
        ! Also, to return the index immediately after the most recently visited position
        ! in the state array.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(INOUT) :: State(0:)
        tSInt32,  INTENT(IN)    :: Seed(0:)
        tSInt32                 :: NextID

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: StateSize, SeedSize, MaxSize
        tSInt32     :: I, J, K
        tSInt64     :: A, B, C

    ! FLOW

        StateSize = SIZE(State)
        SeedSize  = SIZE(Seed)
        MaxSize   = MAX(StateSize, SeedSize)
        I = 1
        J = 0

        DO K = MaxSize, 1, -1
            IF (State(I) < 0) THEN
                A = IOR(IAND(ToInt64(State(I)), LOWER_MASK_LONG), UPPER_MASK_LONG)
            ELSE
                A = IOR(IAND(ToInt64(State(I)), LOWER_MASK_LONG), 0_kInt64)
            END IF
            IF (State(I-1) < 0) THEN
                B = IOR(IAND(ToInt64(State(I-1)), LOWER_MASK_LONG), UPPER_MASK_LONG)
            ELSE
                B = IOR(IAND(ToInt64(State(I-1)), LOWER_MASK_LONG), 0_kInt64)
            END IF
            ! Non linear
            C = IEOR(A, (IEOR(B, SHIFTA(B, 30)) * 1664525_kInt64)) + Seed(J) + J
            State(I) = ToInt32(IAND(C, INT_MASK_LONG))
            I = I + 1
            J = J + 1
            IF (I >= StateSize) THEN
                State(0) = State(StateSize - 1)
                I = 1
            END IF
            IF (J >= SeedSize) THEN
                J = 0
            END IF
        END DO

        ! return the next index
        NextID = I

        RETURN

    END FUNCTION Mix_Seed_N_State

    !******************************************************************************

    SUBROUTINE Mix_State(State, StartID)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To mix each position of the state using a non-linear combination. The
        ! procedure starts from the specified index in the state array and wraps
        ! iteration through the array if required.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(INOUT) :: State(0:)
        tSInt32,  INTENT(IN)    :: StartID

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: StateSize
        tSInt32     :: I, K
        tSInt64     :: A, B, C

    ! FLOW

        StateSize = SIZE(State)
        I = StartID

        DO K = StateSize, 1, -1
            IF (State(I) < 0) THEN
                A = IOR(IAND(ToInt64(State(I)), LOWER_MASK_LONG), UPPER_MASK_LONG)
            ELSE
                A = IOR(IAND(ToInt64(State(I)), LOWER_MASK_LONG), 0_kInt64)
            END IF
            IF (State(I-1) < 0) THEN
                B = IOR(IAND(ToInt64(State(I-1)), LOWER_MASK_LONG), UPPER_MASK_LONG)
            ELSE
                B = IOR(IAND(ToInt64(State(I-1)), LOWER_MASK_LONG), 0_kInt64)
            END IF
            ! Non linear
            C = IEOR(A, (IEOR(B, SHIFTA(B, 30)) * 1566083941_kInt64)) - I
            State(I) = ToInt32(IAND(C, INT_MASK_LONG))
            I = I + 1
            IF (I >= StateSize) THEN
                State(0) = State(StateSize - 1)
                I = 1
            END IF
        END DO

        RETURN

    END SUBROUTINE Mix_State

    !******************************************************************************

END SUBROUTINE Mt32RNG_BaseInit

!******************************************************************************

FUNCTION Mt32RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt32RNG), INTENT(INOUT)   :: RNG      !! 'Mt32RNG' object
    tSInt32                         :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Tempering parameters
    tSInt32,  PARAMETER :: TMaskB = ToInt32(Z'9D2C5680')
    tSInt32,  PARAMETER :: TMaskC = ToInt32(Z'EFC60000')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Y, MTCurr, MTNext, K

! FLOW

    IF (RNG%MTI >= N) THEN
        ! Generate N words at one time
        MTNext = RNG%MT(0)
        DO K = 0, N-M-1
            MTCurr = MTNext
            MTNext = RNG%MT(K + 1)
            Y = IOR(IAND(MTCurr, UPPER_MASK), IAND(MTNext, LOWER_MASK))
            RNG%MT(K) = IEOR(IEOR(RNG%MT(K + M), SHIFTR(Y, 1)), MAG01(IAND(Y, 1)))
        END DO
        DO K = N-M, N-2
            MTCurr = MTNext
            MTNext = RNG%MT(K + 1)
            Y = IOR(IAND(MTCurr, UPPER_MASK), IAND(MTNext, LOWER_MASK))
            RNG%MT(K) = IEOR(IEOR(RNG%MT(K + (M - N)), SHIFTR(Y, 1)), MAG01(IAND(Y, 1)))
        END DO
        Y = IOR(IAND(MTNext, UPPER_MASK), IAND(RNG%MT(0), LOWER_MASK))
        RNG%MT(N - 1) = IEOR(IEOR(RNG%MT(M - 1), SHIFTR(Y, 1)), MAG01(IAND(Y, 1)))

        RNG%MTI = 0
    END IF

    Y = RNG%MT(RNG%MTI)
    RNG%MTI = RNG%MTI + 1

    ! Tempering
    Y = IEOR(Y, SHIFTR(Y, 11))
    Y = IEOR(Y, IAND(SHIFTL(Y, 7), TMaskB))
    Y = IEOR(Y, IAND(SHIFTL(Y, 15), TMaskC))
    Y = IEOR(Y, SHIFTR(Y, 18))

    RandNum = Y

    RETURN

END FUNCTION Mt32RNG_NextInteger

!******************************************************************************

FUNCTION Mt32RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt32RNG), INTENT(IN)  :: RNG      !! 'Mt32RNG' object
    tCharAlloc                  :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'Mt32RNG'
    ! to prevent warning of unused variable(s)
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Mt32RNG_GetName

!******************************************************************************

FUNCTION Mt32RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mt32RNG), INTENT(IN)  :: RNG      !! 'Mt32RNG' object
    tIndex                      :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = N
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Mt32RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_Mt32RNG

!******************************************************************************
