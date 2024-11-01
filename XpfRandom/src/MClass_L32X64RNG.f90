
MODULE MClass_L32X64RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *L32X64RNG* type and its related routines.
!   The *L32X64RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type. <br>
!   In particular, the *L32X64RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on the *L32X64* algorithm.
!   The *L32X64* algorithm is a specific member of the LXM family of algorithms
!   for pseudo-random number generators where <br>
!   -  L stands for Linear congruential generator (LCG); <br>
!   -  X stands for Xor-based generator (XBG); and <br>
!   -  M stands for Mix. <br>
!   The *L32X64RNG* type employs a 32-bit LCG, a 64-bit XBG and a mixing function
!   where its memory footprint is 128 bits and its period is roughly 2<sup>96</sup>. <br>
!   In addition to common operations of a PRNG, the *L32X64RNG* type provides
!   the *Split* method to split a generator into two instances (the original and
!   a new instance) where the two generators can be used concurrently.  Presumably,
!   the new generator is statistically independent and uniform.  <br>
!   It is important to note that the *L32X64* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  Also, it should be noted that the implementation of
!   this PRNG is based on reference #2 where the *LXM3264RNG* type, which is
!   based on the same algorithm, is based its implementation on reference #3. <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://dl.acm.org/doi/abs/10.1145/3485525">
!       Steele and Vigna (2021).  LXM: better splittable pseudo-random number generators
!      (and almost as fast). Proceedings of the ACM on Programming Languages, Volume 5,
!       Article 148, pp 1-31.</a> <br>
!   [2] <a href="https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/random/package-summary.html">
!       Package: Java.Util.Random</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/L32X64Mix.html">
!       Apache Commons RNG: Class L32X64Mix</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: L32X64RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *L32X64RNG* type is an *Integer* PRNG type based on the *L32X64*
    !  algorithm, which is a specific member of the LXM family of algorithms
    !  for pseudo-random number generators.
    TYPE, EXTENDS(IntegerRNG)  :: L32X64RNG
        PRIVATE
        !  The parameter that is used as an additive constant for the LCG.  Must be odd.
        tSInt32     :: A
        ! The per-instance state: S for the LCG, and X0 and X1 for the XBG.
        ! At least one of X0 and X1 must be nonzero.
        tSInt32     :: S, X0, X1
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: SetParameters    => L32X64RNG_SetParameters
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => L32X64RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => L32X64RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => L32X64RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => L32X64RNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----                 Public Procedures                         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Split <br>
        !  **Purpose**:  To return a new PRNG, split off from the current one. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%Split() <br>
        !  **Note**: The *Split* method is intended to be used in a parallel environment
        !            where the current generator and the new one can be used concurrently.
        PROCEDURE       :: Split            => L32X64RNG_Split
        ! ---------------------------------------------------------------------
    END TYPE L32X64RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE L32X64RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'L32X64RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L32X64RNG), INTENT(INOUT) :: RNG      !! 'L32X64RNG' object
    tSInt32,          INTENT(IN)    :: Seed(:)  !! seeds

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: IntSeeds(2)
    tSInt64     :: InitSeed

! FLOW
    
    ! extend integer seeds
    CALL Extend_Seed(Seed, IntSeeds)
    
    ! set initial long seed
    InitSeed = IOR(IAND(ToInt64(IntSeeds(1)), MaskL), SHIFTL(ToInt64(IntSeeds(2)), 32)) &
               + GOLDEN_RATIO_64
    InitSeed = IEOR(InitSeed, SILVER_RATIO_64)
    
    ! set parameters as follows:
    ! The high half of the seed is hashed by Mix_Murmur to produce the 'A' parameter.
    ! The low half of the seed is hashed by Mix_Lea to produce the initial 'X0',
    ! which will then be used to produce the first generated value.
    ! Then 'X1' is filled in as if by a SplitMix PRNG with GOLDEN_RATIO_32
    ! as the gamma value and Mix_Lea as the mixer.
    CALL RNG%SetParameters(Mix_Murmur(ToInt32(SHIFTR(InitSeed, 32))), &
                           1, Mix_Lea(ToInt32(InitSeed)), &
                           Mix_Lea(ToInt32(InitSeed) + GOLDEN_RATIO_32))

    RETURN

END SUBROUTINE L32X64RNG_BaseInit

!******************************************************************************

FUNCTION L32X64RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L32X64RNG), INTENT(INOUT) :: RNG      !! 'L32X64RNG' object
    tSInt32                         :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Multiplier used in the LCG portion of the algorithm.
    ! Chosen based on research by Sebastiano Vigna and Guy Steele (2019).
    ! The spectral scores for dimensions 2 through 8 for the multiplier 0xadb4a92d
    ! are [0.975884, 0.936244, 0.755793, 0.877642, 0.751300, 0.789333, 0.728869].
    tSInt32,  PARAMETER :: M = ToInt32(Z'ADB4A92D')
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32        :: Q0, Q1

! FLOW
    
    ! Compute the RandNum based on current state information
    ! (this allows the computation to be overlapped with state update).
    RandNum = Mix_Lea(RNG%S + RNG%X0)

    ! Update the LCG subgenerator
    RNG%S = M * RNG%S + RNG%A

    ! Update the XBG subgenerator
    Q0 = RNG%X0
    Q1 = RNG%X1
    
    ! xoroshiro64
    Q1 = IEOR(Q1, Q0)
    Q0 = RotateLeft(Q0, 26)
    Q0 = IEOR(IEOR(Q0, Q1), SHIFTL(Q1, 9))
    Q1 = RotateLeft(Q1, 13)
    
    RNG%X0 = Q0
    RNG%X1 = Q1
    
    RETURN

END FUNCTION L32X64RNG_NextInteger

!******************************************************************************

FUNCTION L32X64RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L32X64RNG), INTENT(IN)    :: RNG      !! 'L32X64RNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'L32X64RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION L32X64RNG_GetName

!******************************************************************************

FUNCTION L32X64RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L32X64RNG), INTENT(IN)    :: RNG      !! 'L32X64RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 2
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION L32X64RNG_GetSeedSize

!******************************************************************************

SUBROUTINE L32X64RNG_SetParameters(RNG, A, S, X0, X1)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set parameters of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L32X64RNG), INTENT(INOUT)  :: RNG
    tSInt32,          INTENT(IN)     :: A
    tSInt32,          INTENT(IN)     :: S
    tSInt32,          INTENT(IN)     :: X0
    tSInt32,          INTENT(IN)     :: X1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: V

! FLOW
    
    ! Force a to be odd
    RNG%A  = IOR(A, 1)
    RNG%S  = S
    
    ! check if X0 and X1 are both zero or not
    IF (IOR(X0, X1) == 0) THEN
        ! If X0 and X1 are both zero, we must choose nonzero values.
        V = S + GOLDEN_RATIO_32
        ! At least one of the two values generated here will be nonzero.
        RNG%X0 = Mix_Murmur(V)
        RNG%X1 = Mix_Murmur(V + GOLDEN_RATIO_32)
    ELSE
        RNG%X0 = X0
        RNG%X1 = X1
    END IF
    
    RETURN

END SUBROUTINE L32X64RNG_SetParameters

!******************************************************************************

FUNCTION L32X64RNG_Split(RNG, Brine) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a new generator split off from the current instance.
    !  The new generator is initialized and returned where it shares
    !  no mutable state with the current generator.  However, with very
    !  high probability, the set of values collectively generated by
    !  the two generators has the same statistical properties as if the
    !  same quantity of values were generated by a single thread using
    !  a single generator.  Either or both of the two generators may be
    !  further split using this routine, and the same expected statistical 
    !  properties apply to the entire set of generators constructed by
    !  such recursive splitting.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'L32X64RNG' object
    CLASS(L32X64RNG),  INTENT(INOUT)    :: RNG
    !> a long value, of which the low 63 bits provide a unique id among calls
    !  to this routine for constructing a single series of Generator objects.
    tSInt64, OPTIONAL, INTENT(IN)       :: Brine
    !% new 'L32X64RNG' object
    TYPE(L32X64RNG)                     :: NewRNG

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

    ! set parameters by picking a new object at random but use the low 31 bits
    ! of the salt for 'A'
    CALL L32X64RNG_SetParameters(NewRNG, SHIFTL(ToInt32(Salt), 1), RNG%NextInteger(), &
                                 RNG%NextInteger(), RNG%NextInteger())
    
    RETURN

END FUNCTION L32X64RNG_Split

!******************************************************************************

END MODULE MClass_L32X64RNG
    
!******************************************************************************
