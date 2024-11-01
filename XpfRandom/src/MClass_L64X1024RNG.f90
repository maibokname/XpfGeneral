
MODULE MClass_L64X1024RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *L64X1024RNG* type and its related routines.
!   The *L64X1024RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type. <br>
!   In particular, the *L64X1024RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the *L64X1024* algorithm.  The
!   *L64X1024* algorithm is a specific member of the LXM family of algorithms
!   for pseudo-random number generators where <br>
!   -  L stands for Linear congruential generator (LCG); <br>
!   -  X stands for Xor-based generator (XBG); and <br>
!   -  M stands for Mix. <br>
!   The *L64X1024RNG* type employs a 64-bit LCG, a 1024-bit XBG and a mixing function
!   where its memory footprint is 1184 bits and its period is roughly 2<sup>1088</sup>. <br>
!   In addition to common operations of a PRNG, the *L64X1024RNG* type provides
!   the *Split* method to split a generator into two instances (the original and
!   a new instance) where the two generators can be used concurrently.  Presumably,
!   the new generator is statistically independent and uniform.  <br>
!   It is important to note that the *L64X1024* PRNG requires an explicit
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
    PUBLIC :: L64X1024RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    tSInt32,  PARAMETER :: N = 16   ! length of array X

!** DERIVED TYPE DEFINITIONS
    !> The *L64X1024RNG* type is a *Long* PRNG type based on the *L64X1024*
    !  algorithms, which are specific members of the LXM family of algorithms
    !  for pseudo-random number generators.
    TYPE, EXTENDS(LongRNG)  :: L64X1024RNG
        PRIVATE
        ! The parameter that is used as an additive constant for the LCG.  Must be odd.
        tSInt64     :: A
        ! The per-instance state: S for the LCG, and X0 ... X15 for the XBG.
        ! At least one of the four fields Xs must be nonzero.
        tSInt64     :: S
        tSInt64     :: X(0:N-1)
        tSInt32     :: P
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => L64X1024RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextLongImpl     => L64X1024RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => L64X1024RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => L64X1024RNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----                 Public Procedures                         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Split <br>
        !  **Purpose**:  To return a new PRNG, split off from the current one. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%Split() <br>
        !  **Note**: The *Split* method is intended to be used in a parallel environment
        !            where the current generator and the new one can be used concurrently.
        PROCEDURE       :: Split            => L64X1024RNG_Split
        ! ---------------------------------------------------------------------
    END TYPE L64X1024RNG

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE L64X1024RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'L64X1024RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L64X1024RNG), INTENT(INOUT)   :: RNG      !! 'L64X1024RNG' object
    tSInt64,            INTENT(IN)      :: Seed(:)  !! seed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: InitSeed, X(0:15), XSeed
    tIndex      :: I

! FLOW

    ! set initial seed
    InitSeed = IEOR(Seed(1), SILVER_RATIO_64)

    ! set parameters as follows:
    ! The seed is hashed by Mix_Murmur to produce the 'A' parameter.
    ! The seed is hashed by Mix_Stafford_13 to produce the initial 'X0',
    ! which will then be used to produce the first generated value.
    ! Then, the other 'X' values are filled in as if by a SplitMix PRNG
    ! with GOLDEN_RATIO_64 as the gamma value and Mix_Stafford_13 as the mixer.
    XSeed = InitSeed
    DO I = 0, 15
        X(I) = Mix_Stafford_13(XSeed)
        XSeed = XSeed + GOLDEN_RATIO_64
    END DO
    CALL L64X1024RNG_SetParameters(RNG, Mix_Murmur(InitSeed), 1_kInt64, X)

    RETURN

END SUBROUTINE L64X1024RNG_BaseInit

!******************************************************************************

FUNCTION L64X1024RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L64X1024RNG), INTENT(INOUT)   :: RNG      !! 'L64X1024RNG' object
    tSInt64                             :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Multiplier used in the LCG portion of the algorithm.
    ! Chosen based on research by Sebastiano Vigna and Guy Steele (2019).
    ! The spectral scores for dimensions 2 through 8 for the multiplier 0xd1342543de82ef95L
    ! are [0.958602, 0.937479, 0.870757, 0.822326, 0.820405, 0.813065, 0.760215].
    tSInt64, PARAMETER  ::M = ToInt64(Z'D1342543DE82EF95')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: S0, S15
    tSInt32     :: Q

! FLOW

    ! First part of xoroshiro1024: fetch array data
    Q = RNG%P
    RNG%P = IAND(RNG%P+1, N-1)
    S0  = RNG%X(RNG%P)
    S15 = RNG%X(Q)

    ! Compute the RandNum based on current state information
    ! (this allows the computation to be overlapped with state update).
    RandNum = Mix_Lea(RNG%S + S0)

    ! Update the LCG subgenerator
    RNG%S = M * RNG%S + RNG%A

    ! Second part of xoroshiro1024: update array data
    S15 = IEOR(S15, S0)
    RNG%X(Q) = IEOR(IEOR(RotateLeft(S0, 25), S15), SHIFTL(S15, 27))
    RNG%X(RNG%P) = RotateLeft(S15, 36)

    RETURN

END FUNCTION L64X1024RNG_NextLong

!******************************************************************************

FUNCTION L64X1024RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L64X1024RNG), INTENT(IN)  :: RNG      !! 'L64X1024RNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'L64X1024RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION L64X1024RNG_GetName

!******************************************************************************

FUNCTION L64X1024RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L64X1024RNG), INTENT(IN)  :: RNG      !! 'L64X1024RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 1
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION L64X1024RNG_GetSeedSize

!******************************************************************************

SUBROUTINE L64X1024RNG_SetParameters(RNG, A, S, X)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set parameters of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(L64X1024RNG), INTENT(INOUT) :: RNG
    tSInt64,           INTENT(IN)    :: A
    tSInt64,           INTENT(IN)    :: S
    tSInt64,           INTENT(IN)    :: X(0:15)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: V, Cond
    tIndex      :: I

! FLOW

    ! initialize P
    RNG%P = N - 1

    ! Force a to be odd
    RNG%A = IOR(A, 1_kInt64)
    RNG%S = S

    ! check if all Xs are zero or not
    Cond = IOR(X(0), X(1))
    DO I = 2, 15
        Cond = IOR(Cond, X(I))
    END DO
    IF (Cond == 0) THEN
        ! If all Xs are zero, we must choose nonzero values.
        V = S + GOLDEN_RATIO_64
        ! At least 15 of the 16 values generated here will be nonzero.
        RNG%X(0) = Mix_Stafford_13(V)
        DO I = 1, 15
            V = V + GOLDEN_RATIO_64
            RNG%X(I) = Mix_Stafford_13(V)
        END DO
    ELSE
        RNG%X(0:15) = X(0:15)
    END IF

    RETURN

END SUBROUTINE L64X1024RNG_SetParameters

!******************************************************************************

FUNCTION L64X1024RNG_Split(RNG, Brine) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize and return a new 'L64X1024RNG' object that shares no mutable
    !  state with this object. However, with very high probability, the set of
    !  values collectively generated by the two objects has the same statistical
    !  properties as if the same quantity of values were generated by a single
    !  thread using a single object.  Either or both of the two objects may be
    !  further split using this routine, and the same expected statistical
    !  properties apply to the entire set of generators constructed by such
    !  recursive splitting.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'L64X1024RNG' object
    CLASS(L64X1024RNG), INTENT(INOUT)   :: RNG
    !> a long value, of which the low 63 bits provide a unique id among calls
    !  to this routine for constructing a single series of Generator objects.
    tSInt64, OPTIONAL,INTENT(IN)        :: Brine
    !% new 'L64X1024RNG' object
    TYPE(L64X1024RNG)                   :: NewRNG

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: Salt, S, X(0:15)
    tIndex      :: I

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
    S = RNG%NextLong()
    DO I = 0, 15
        X(I) = RNG%NextLong()
    END DO
    CALL L64X1024RNG_SetParameters(NewRNG, SHIFTL(Salt, 1), S, X)

    RETURN

END FUNCTION L64X1024RNG_Split

!******************************************************************************

END MODULE MClass_L64X1024RNG

!******************************************************************************
