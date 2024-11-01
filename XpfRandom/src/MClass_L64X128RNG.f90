
MODULE MClass_L64X128RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *L64X128RNG* type and its related routines.
!   The *L64X128RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type. <br>
!   In particular, the *L64X128RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the *L64X128* algorithms.
!   The *L64X128* algorithms are specific members of the LXM family of
!   algorithms for pseudo-random number generators where <br>
!   -  L stands for Linear congruential generator (LCG); <br>
!   -  X stands for Xor-based generator (XBG); and <br>
!   -  M stands for Mix. <br>
!   The *L64X128RNG* type can represent two PRNG classes: <br>
!   - the L64X128Mix RNG, or <br>
!   - the L64X128StarStar RNG. <br>
!   Both PRNG classes employ a 64-bit LCG, a 128-bit XBG and a mixing function
!   where its memory footprint is 256 bits and its period is roughly 2<sup>192</sup>. <br>
!   By default, the *L64X128Mix* RNG is employed.  However, the *L64X128StarStar*
!   PRNG can be utilized by specifying an algorithm flag to true when initializing
!   the generator. <br>
!   In addition to common operations of a PRNG, the *L64X128RNG* type provides
!   the *Split* method to split a generator into two instances (the original and
!   a new instance) where the two generators can be used concurrently.  Presumably,
!   the new generator is statistically independent and uniform.  <br>
!   It is important to note that the *L64X128* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences. <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://dl.acm.org/doi/abs/10.1145/3485525">
!       Steele and Vigna (2021).  LXM: better splittable pseudo-random number generators
!      (and almost as fast). Proceedings of the ACM on Programming Languages, Volume 5,
!       Article 148, pp 1-31.</a> <br>
!   [2] <a href="https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/random/package-summary.html">
!       Package: Java.Util.Random</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: L64X128RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *L64X128RNG* type is a *Long* PRNG type based on the *L64X128*
    !  algorithms, which are specific members of the LXM family of algorithms
    !  for pseudo-random number generators.
    TYPE, EXTENDS(LongRNG)  :: L64X128RNG
        PRIVATE
        ! The parameter that is used as an additive constant for the LCG.  Must be odd.
        tSInt64     :: A
        ! The per-instance state: S for the LCG, and X0 and X1 for the XBG.
        ! At least one of X0 and X1 must be nonzero.
        tSInt64     :: S, X0, X1
        ! algorithm flag
        tLogical    :: UseStarStar = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: InitWithFlag => L64X128RNG_InitWithFlag
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => L64X128RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextLongImpl     => L64X128RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => L64X128RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => L64X128RNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----               Generic Interface                           -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Initialize <br>
        !  **Purpose**:  To initialize the pseudo-random number generator. <br>
        !  **Usage**: <br>
        !   ! initialize without seed; use L64X128 (default) algorithm <br>
        !   --->    CALL RNG%Initialize() <br>
        !   ! initialize with seed(s); use L64X128 (default) algorithm <br>
        !   --->    CALL RNG%Initialize(Seeds) <br>
        !   ! initialize without seed; use L64X128StarStar algorithm <br>
        !   --->    CALL RNG%Initialize(.TRUE.) <br>
        !   ! initialize with seed(s); use L64X128StarStar algorithm <br>
        !   --->    CALL RNG%Initialize(.TRUE., Seeds)
        GENERIC         :: Initialize       => InitWithFlag
        ! ---------------------------------------------------------------------
        ! -----                 Public Procedures                         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Split <br>
        !  **Purpose**:  To return a new PRNG, split off from the current one. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%Split() <br>
        !  **Note**: The *Split* method is intended to be used in a parallel environment
        !            where the current generator and the new one can be used concurrently.
        PROCEDURE       :: Split            => L64X128RNG_Split
        ! ---------------------------------------------------------------------
    END TYPE L64X128RNG

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE L64X128RNG_InitWithFlag(RNG, Seed, UseStarStar)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified algorithm flag.  Optionally,
    !  initial seeds can be specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'L64X128RNG' object
    CLASS(L64X128RNG), INTENT(INOUT)    :: RNG
    !> algorithm flag <br>
    ! - true, use L64X128StarStar algorithm. <br>
    ! - false, use L64X128 algorithm.
    tLogical,          INTENT(IN)       :: UseStarStar
    !% optional 64-bit integer seed(s)
    tSInt64, OPTIONAL, INTENT(IN)       :: Seed(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set algorithm flag
    RNG%UseStarStar = UseStarStar

    IF (PRESENT(Seed)) THEN
        ! initialize the generator with seed(s)
        CALL RNG%Initialize(Seed)
    ELSE
        ! initialize the generator without seed
        CALL RNG%Initialize()
    END IF

    RETURN

END SUBROUTINE L64X128RNG_InitWithFlag

!******************************************************************************

SUBROUTINE L64X128RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'L64X128RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L64X128RNG), INTENT(INOUT)    :: RNG      !! 'L64X128RNG' object
    tSInt64,           INTENT(IN)       :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64  :: InitSeed

! FLOW

    ! set initial seed
    InitSeed = IEOR(Seed(1), SILVER_RATIO_64)

    ! set parameters as follows:
    ! The seed is hashed by Mix_Murmur to produce the 'A' parameter.
    ! The seed is hashed by Mix_Stafford_13 to produce the initial 'X0',
    ! which will then be used to produce the first generated value.
    ! Then 'X1' is filled in as if by a SplitMix PRNG with GOLDEN_RATIO_64
    ! as the gamma value and Mix_Stafford_13 as the mixer.
    CALL L64X128RNG_SetParameters(RNG, Mix_Murmur(InitSeed), 1_kInt64, Mix_Stafford_13(InitSeed), &
                                  Mix_Stafford_13(InitSeed + GOLDEN_RATIO_64))

    RETURN

END SUBROUTINE L64X128RNG_BaseInit

!******************************************************************************

FUNCTION L64X128RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L64X128RNG), INTENT(INOUT)    :: RNG      !! 'L64X128RNG' object
    tSInt64                             :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Multiplier used in the LCG portion of the algorithm.
    ! Chosen based on research by Sebastiano Vigna and Guy Steele (2019).
    ! The spectral scores for dimensions 2 through 8 for the multiplier 0xd1342543de82ef95L
    ! are [0.958602, 0.937479, 0.870757, 0.822326, 0.820405, 0.813065, 0.760215].
    tSInt64, PARAMETER  ::M = ToInt64(Z'D1342543DE82EF95')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: Q0, Q1

! FLOW

    ! Compute the RandNum based on current state information
    ! (this allows the computation to be overlapped with state update).
    IF (RNG%UseStarStar) THEN
        ! use 'L64X128StarStar' algorithm
        RandNum = RotateLeft((RNG%S + RNG%X0)*5_kInt64, 7)*9_kInt64   ! "starstar" scrambler
    ELSE
        ! use 'L64X128' algorithm
        RandNum = Mix_Lea(RNG%S + RNG%X0)
    END IF

    ! Update the LCG subgenerator
    RNG%S = M * RNG%S + RNG%A

    ! Update the XBG subgenerator
    Q0 = RNG%X0
    Q1 = RNG%X1

    ! xoroshiro128v1_0
    Q1 = IEOR(Q1, Q0)
    Q0 = RotateLeft(Q0, 24)
    Q0 = IEOR(IEOR(Q0, Q1), SHIFTL(Q1, 16))
    Q1 = RotateLeft(Q1, 37)

    RNG%X0 = Q0
    RNG%X1 = Q1

    RETURN

END FUNCTION L64X128RNG_NextLong

!******************************************************************************

FUNCTION L64X128RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L64X128RNG), INTENT(IN)   :: RNG      !! 'L64X128RNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (RNG%UseStarStar) THEN
        Name = 'L64X128StarStarRNG'
    ELSE
        Name = 'L64X128RNG'
    END IF
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION L64X128RNG_GetName

!******************************************************************************

FUNCTION L64X128RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L64X128RNG), INTENT(IN)   :: RNG      !! 'L64X128RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 1
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION L64X128RNG_GetSeedSize

!******************************************************************************

SUBROUTINE L64X128RNG_SetParameters(RNG, A, S, X0, X1)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set parameters of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(L64X128RNG), INTENT(INOUT) :: RNG
    tSInt64,          INTENT(IN)    :: A
    tSInt64,          INTENT(IN)    :: S
    tSInt64,          INTENT(IN)    :: X0
    tSInt64,          INTENT(IN)    :: X1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64 :: V

! FLOW

    ! Force a to be odd
    RNG%A  = IOR(A, 1_kInt64)
    RNG%S  = S

    ! check if X0 and X1 are both zero or not
    IF (IOR(X0, X1) == 0) THEN
        ! If X0 and X1 are both zero, we must choose nonzero values.
        V = S + GOLDEN_RATIO_64
        ! At least one of the two values generated here will be nonzero.
        RNG%X0 = Mix_Stafford_13(V)
        RNG%X1 = Mix_Stafford_13(V + GOLDEN_RATIO_64)
    ELSE
        RNG%X0 = X0
        RNG%X1 = X1
    END IF

    RETURN

END SUBROUTINE L64X128RNG_SetParameters

!******************************************************************************

FUNCTION L64X128RNG_Split(RNG, Brine) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize and return a new 'L64X128RNG' object that shares no mutable
    !  state with this object. However, with very high probability, the set of
    !  values collectively generated by the two objects has the same statistical
    !  properties as if the same quantity of values were generated by a single
    !  thread using a single object.  Either or both of the two objects may be
    !  further split using this routine, and the same expected statistical
    !  properties apply to the entire set of generators constructed by such
    !  recursive splitting.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'L64X128RNG' object
    CLASS(L64X128RNG), INTENT(INOUT)    :: RNG
    !> a long value, of which the low 63 bits provide a unique id among calls
    !  to this routine for constructing a single series of Generator objects.
    tSInt64, OPTIONAL,INTENT(IN)        :: Brine
    !% new 'L64X128RNG' object
    TYPE(L64X128RNG)                    :: NewRNG

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: Salt

! FLOW

    ! check optional input
    IF (PRESENT(Brine)) THEN
        Salt = Brine
    ELSE
        Salt = RNG%NextLong()
    END IF

    ! copy initial seeds for re-initialization
    CALL RNG%CopySeed(NewRNG)

    ! set parameters by picking a new object at random but use the 63 bits
    ! of the salt for 'A'
    CALL L64X128RNG_SetParameters(NewRNG, SHIFTL(Salt, 1), RNG%NextLong(), &
                                  RNG%NextLong(), RNG%NextLong())

    RETURN

END FUNCTION L64X128RNG_Split

!******************************************************************************

END MODULE MClass_L64X128RNG

!******************************************************************************
