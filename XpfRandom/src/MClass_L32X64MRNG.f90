
MODULE MClass_L32X64MRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *L32X64MRNG* type and its related routines.
!   The *L32X64MRNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type. <br>
!   In particular, the *L32X64MRNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on the *L32X64* algorithm.
!   The *L32X64* algorithm is a specific member of the LXM family of algorithms
!   for pseudo-random number generators where <br>
!   -  L stands for Linear congruential generator (LCG); <br>
!   -  X stands for Xor-based generator (XBG); and <br>
!   -  M stands for Mix. <br>
!   The *L32X64MRNG* type employs a 32-bit LCG, a 64-bit XBG and a mixing function
!   where its memory footprint is 128 bits and its period is roughly 2<sup>96</sup>. <br>
!   In addition to common operations of a PRNG, the *L32X64MRNG* type provides
!   the *Split* method to split a generator into two instances (the original and 
!   a new instance) where the two generators can be used concurrently.  Presumably,
!   the new generator is statistically independent and uniform. <br>
!   The *L32X64MRNG* type also provides the *Jump* and *LongJump* methods where
!   a large (or very large) number of steps of the output sequence can be advanced
!   in a single operation.  Each method creates (and also returns) a copy of the
!   input PRNG and then advances the state of the specified PRNG.  The PRNG and its
!   copy produce non-overlapping output for the length of the jump intendedly for
!   use in parallel computations. <br>
!   It is important to note that the *L32X64* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  Also, it should be noted that the implementation of
!   this PRNG type is based on reference #3 where the *L32X64RNG* type,
!   which is based on the same algorithm, is based its implementation on
!   reference #2. <br>
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
    PUBLIC :: L32X64MRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! 32-bit LCG multiplier. Note: (M % 8) = 5.
    tSInt32,  PARAMETER :: M = ToInt32(Z'ADB4A92D')
    ! Jump constant {m'} for an advance of the 32-bit LCG by 2^16.
    ! Computed as: {m' = m^(2^16) (mod 2^32)}.
    tSInt32,  PARAMETER :: M32P = ToInt32(Z'65640001')
    ! Jump constant precursor for {c'} for an advance of the 32-bit LCG by 2^16.
    ! Computed as:
    ! product_{i=0}^{15} { M^(2^i) + 1 } (mod 2^32)
    ! The jump is computed for the LCG with an update step of {s = m * s + c} as:
    ! s = m' * s + c' * c
    tSInt32,  PARAMETER :: C32P = ToInt32(Z'046B0000')

!** DERIVED TYPE DEFINITIONS
    !> The *L32X64MRNG* type is an *Integer* PRNG type based on the *L32X64*
    !  algorithm, which is a specific member of the LXM family of algorithms
    !  for pseudo-random number generators.
    TYPE, EXTENDS(IntegerRNG)  :: L32X64MRNG
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
        PROCEDURE, PRIVATE  :: MakeACopy        => L32X64MRNG_Copy
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE           :: BaseInit         => L32X64MRNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE           :: NextIntegerImpl  => L32X64MRNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE           :: GetName          => L32X64MRNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE           :: GetSeedSize      => L32X64MRNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----                 Public Procedures                         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Split <br>
        !  **Purpose**:  To return a new PRNG, split off from the current one. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%Split() <br>
        !  **Note**: The *Split* method is intended to be used in a parallel environment
        !            where the current generator and the new one can be used concurrently.
        PROCEDURE           :: Split            => L32X64MRNG_Split
        !> **Type-Bound Function**: Jump <br>
        !  **Purpose**:  To create a copy of the specified PRNG and then retreat
        !                the state of the current PRNG.  The copy is returned as
        !                a new PRNG. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%Jump() <br>
        !  **Note**: The *Jump* method is equivalent to calling the *NextInteger*
        !            method repeatedly a large number of times.
        PROCEDURE           :: Jump             => L32X64MRNG_Jump
        !> **Type-Bound Function**: LongJump <br>
        !  **Purpose**:  To create a copy of the specified PRNG and then retreat
        !                the state of the current PRNG.  The copy is returned as
        !                a new PRNG. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%LongJump() <br>
        !  **Note**: The *LongJump* method is equivalent to calling the *NextInteger*
        !            method repeatedly a very large number of times.
        PROCEDURE           :: LongJump         => L32X64MRNG_LongJump
        ! ---------------------------------------------------------------------
    END TYPE L32X64MRNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE L32X64MRNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'L32X64MRNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L32X64MRNG), INTENT(INOUT)    :: RNG      !! 'L32X64MRNG' object
    tSInt32,           INTENT(IN)       :: Seed(:)  !! seed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Seed0(4)

! FLOW
    
    ! set initial seed
    CALL Extend_Seed(Seed, Seed0)
    
    ! Additive parameter must be odd
    RNG%A  = IOR(Seed0(1), 1)
    RNG%S  = Seed0(2)
    RNG%X0 = Seed0(3)
    RNG%X1 = Seed0(4)

    RETURN

END SUBROUTINE L32X64MRNG_BaseInit

!******************************************************************************

FUNCTION L32X64MRNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L32X64MRNG), INTENT(INOUT)    :: RNG      !! 'L32X64MRNG' object
    tSInt32                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32        :: Q0, Q1

! FLOW
    
    ! +++ Mix +++
    RandNum = Mix_Lea(RNG%S + RNG%X0)

    ! +++ LCG Update +++
    RNG%S = M * RNG%S + RNG%A

    ! +++ XBG Update +++
    Q0 = RNG%X0
    Q1 = RNG%X1
    ! xoroshiro64
    Q1 = IEOR(Q1, Q0)
    Q0 = IEOR(IEOR(RotateLeft(Q0, 26), Q1), SHIFTL(Q1, 9))  ! a, b
    Q1 = RotateLeft(Q1, 13)                                 ! c
    RNG%X0 = Q0
    RNG%X1 = Q1
    
    RETURN

END FUNCTION L32X64MRNG_NextInteger

!******************************************************************************

FUNCTION L32X64MRNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L32X64MRNG), INTENT(IN)   :: RNG      !! 'L32X64MRNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'L32X64MRNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION L32X64MRNG_GetName

!******************************************************************************

FUNCTION L32X64MRNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L32X64MRNG), INTENT(IN)   :: RNG      !! 'L32X64MRNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 4
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION L32X64MRNG_GetSeedSize

!******************************************************************************

FUNCTION L32X64MRNG_Split(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a new generator split off from the current instance.
    !  Use the current generator to generate an initial seed and also
    !  employ it as a source of randomness used to initialize the
    !  new generator.    

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L32X64MRNG), INTENT(INOUT)    :: RNG      !! 'L32X64MRNG' object
    TYPE(L32X64MRNG)                    :: NewRNG   !! new 'L32X64MRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: LongSeed
    tSInt32     :: Seed0(4)

! FLOW
    
    ! The upper half of the long seed is discarded so use nextInt
    LongSeed = RNG%NextInteger()
    
    ! LCG state. The addition uses the input seed.
    ! The LCG addition parameter is set to odd so left-shift the seed.
    Seed0(1) = ToInt32(SHIFTL(LongSeed, 1))
    Seed0(2) = RNG%NextInteger()
    ! XBG state must not be all zero
    Seed0(3) = RNG%NextInteger()
    Seed0(4) = RNG%NextInteger()
    IF (IOR(Seed0(3), Seed0(4)) == 0) THEN
        ! SplitMix style seed ensures at least one non-zero value
        Seed0(3) = Mix_Lea(Seed0(2))
        Seed0(4) = Mix_Lea(Seed0(2) + GOLDEN_RATIO_32)
    END IF
    
    ! initialize the New PRNG with initial seeds
    CALL NewRNG%Initialize(Seed0)

    RETURN

END FUNCTION L32X64MRNG_Split

!******************************************************************************

FUNCTION L32X64MRNG_Copy(Src) RESULT(Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !> To copy all components of the source object to the destination object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L32X64MRNG), INTENT(IN)   :: Src  !! source object
    TYPE(L32X64MRNG)                :: Dst  !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! copy states
    Dst%A     = Src%A
    Dst%S     = Src%S
    Dst%X0    = Src%X0
    Dst%X1    = Src%X1
    
    ! copy initial seeds for re-initialization
    CALL Src%CopySeed(Dst)
    
    RETURN

END FUNCTION L32X64MRNG_Copy

!******************************************************************************

FUNCTION L32X64MRNG_Jump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  one.  The new generator is returned.  <br>
    !  The jump is performed by advancing the state of the LCG sub-generator by 1 cycle.
    !  The XBG state is unchanged.  The jump size is the equivalent of moving the state
    !  backwards by (2<sup>64</sup> - 1) positions. It can provide up to 2<sup>32</sup>
    !  non-overlapping subsequences.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L32X64MRNG), INTENT(INOUT)    :: RNG      !! 'L32X64MRNG' object
    TYPE(L32X64MRNG)                    :: NewRNG   !! new 'L32X64MRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()
    
    ! Advance the LCG 1 step
    RNG%S = M * RNG%S + RNG%A
    
    RETURN

END FUNCTION L32X64MRNG_Jump

!******************************************************************************

FUNCTION L32X64MRNG_LongJump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  one.  The new generator is returned.  <br>
    !  The jump is performed by advancing the state of the LCG sub-generator by
    !  2<sup>16</sup> cycles.  The XBG state is unchanged.  The jump size is the
    !  equivalent of moving the state backwards by roughly 2<sup>80</sup> positions.
    !  It can provide up to 2<sup>16</sup> non-overlapping subsequences of length
    !  about 2<sup>80</sup>; each subsequence can provide up to 2<sup>16</sup>
    !  non-overlapping subsequences of length (2<sup>64</sup> - 1) using the
    !  Jump() method.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L32X64MRNG), INTENT(INOUT)    :: RNG      !! 'L32X64MRNG' object
    TYPE(L32X64MRNG)                    :: NewRNG   !! new 'L32X64MRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()
    
    ! Advance the LCG 2**16 steps
    RNG%S = M32P * RNG%S + C32P * RNG%A
    
    RETURN

END FUNCTION L32X64MRNG_LongJump

!******************************************************************************

END MODULE MClass_L32X64MRNG
    
!******************************************************************************
