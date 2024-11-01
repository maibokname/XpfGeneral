
MODULE MClass_L128XMRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *L128XMRNG* type and its related routines.
!   The *L128XMRNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type. <br>
!   In particular, the *L128XMRNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the *L128XM* algorithms.
!   The *L128XM* algorithms are specific members of the LXM family of algorithms
!   for pseudo-random number generators where <br>
!   -  L stands for Linear congruential generator (LCG); <br>
!   -  X stands for Xor-based generator (XBG); and <br>
!   -  M stands for Mix. <br>
!   The *L128XMRNG* type can represent three PRNG classes:  <br>
!   - the L128X128Mix RNG, <br>
!   - the L128X256Mix RNG, or <br>
!   - the L128X1024Mix RNG.  <br>
!   The *L128X128* RNG employs a 128-bit LCG, a 128-bit XBG and a mixing function
!   where its memory footprint is 384 bits and its period is roughly 2<sup>256</sup>. <br>
!   The *L128X256* RNG employ a 128-bit LCG, a 256-bit XBG and a mixing function
!   where its memory footprint is 512 bits and its period is roughly 2<sup>384</sup>. <br>
!   The *L128X1024* RNGs employ a 128-bit LCG, a 1024-bit XBG and a mixing function
!   where its memory footprint is 1312 bits and its period is roughly 2<sup>1152</sup>. <br>
!   By default, The *L128X128Mix* PRNG is employed.  However, other *L128XM* PRNG
!   variants can be utilized by specifying an algorithm flag (between 1 to 3)
!   when initializing the generator.  <br>
!   In addition to common operations of a PRNG, the *L128XMRNG* type provides
!   the *Split* method to split a generator into two instances (the original and
!   a new instance) where the two generators can be used concurrently.  Presumably,
!   the new generator is statistically independent and uniform. <br>
!   The *L128XMRNG* type also provides the *Jump* and *LongJump* methods where
!   a large (or very large) number of steps of the output sequence can be advanced
!   in a single operation.  Each method creates (and also returns) a copy of the
!   input PRNG and then advances the state of the specified PRNG.  The PRNG and its
!   copy produce non-overlapping output for the length of the jump intendedly for
!   use in parallel computations. <br>
!   It is important to note that the *L128XM* PRNG requires an explicit initialization
!   by first calling the *Initialize* method before using any other methods.  Otherwise,
!   the generator may produce undesirable random sequences.  Also, it should be noted
!   that the implementation of this PRNG type is based on references #3-5 whereas the
!   *L128X128RNG*, *L128X256RNG*, and *L128X1024RNG* types, which are based on the same
!   algorithms, are based their implementation on reference #2.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://dl.acm.org/doi/abs/10.1145/3485525">
!       Steele and Vigna (2021).  LXM: better splittable pseudo-random number generators
!      (and almost as fast). Proceedings of the ACM on Programming Languages, Volume 5,
!       Article 148, pp 1-31.</a> <br>
!   [2] <a href="https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/random/package-summary.html">
!       Package: Java.Util.Random</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/L128X128Mix.html">
!       Apache Commons RNG: Class L128X128Mix</a> <br>
!   [4] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/L128X256Mix.html">
!       Apache Commons RNG: Class L128X256Mix</a> <br>
!   [5] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/L128X1024Mix.html">
!       Apache Commons RNG: L128X1024Mix</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MemHandlers,  ONLY: MemAlloc, MemFree
    USE MBase_UIntUtil,     ONLY: UnsignedMultiplyHigh => UMul128_Upper64
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: L128XMRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! Low half of 128-bit LCG multiplier. The upper half is 1.
    tSInt64,  PARAMETER :: M128L  = ToInt64(Z'D605BBB58C8ABBFD')
    ! High half of the jump constant {m'} for an advance of the 128-bit LCG by 2^64.
    ! The low half is 1. Computed as: {m' = m^(2^64) (mod 2^128)}.
    tSInt64,  PARAMETER :: M128PH = ToInt64(Z'31F179F5224754F4')
    ! High half of the jump constant for an advance of the 128-bit LCG by 2^64.
    ! The low half is zero. Computed as:
    ! product_{i=0}^{63} { M^(2^i) + 1 } (mod 2^128)
    ! The jump is computed for the LCG with an update step of {s = m * s + c} as:
    ! s = m' * s + c' * c
    tSInt64,  PARAMETER :: C128PH = ToInt64(Z'61139B28883277C3')
    ! size of LCG states
    tIndex,   PARAMETER :: LCG_STATE_SIZE = 4_kIndex
    ! Parameters for algorithm flag
    tSInt32,  PARAMETER, PUBLIC :: L128X128Mix  = 1 !! flag for L128X128Mix PRNG
    tSInt32,  PARAMETER, PUBLIC :: L128X256Mix  = 2 !! flag for L128X256Mix PRNG
    tSInt32,  PARAMETER, PUBLIC :: L128X1024Mix = 3 !! flag for L128X1024Mix PRNG

!** DERIVED TYPE DEFINITIONS
    !> The *L128XMRNG* type is a *Long* PRNG type based on the *L128XM*
    !  algorithms, which are specific members of the LXM family of
    !  algorithms for pseudo-random number generators.
    TYPE, EXTENDS(LongRNG)  :: L128XMRNG
        PRIVATE
        ! The parameters that are used as additive constants for the LCG.  AL must be odd.
        tSInt64             :: AH, AL
        ! The per-instance state: SH and SL for the LCG, and X0 ... for the XBG.
        ! At least one of the fields Xs must be nonzero.
        tSInt64                 :: SH, SL
        tSInt64, ALLOCATABLE    :: X(:)
        ! index into the X "state" array
        tSInt32                 :: Indx
        ! algorithm flag
        tSInt32                 :: L128XMAlgo = L128X128Mix
        ! pointers to Next and Update routines
        PROCEDURE(Next),   POINTER  :: NextOutput  => NULL()
        PROCEDURE(Update), POINTER  :: UpdateState => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: InitWithFlag => L128XMRNG_InitWithFlag
        PROCEDURE, PRIVATE  :: MakeACopy    => L128XMRNG_Copy
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => L128XMRNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextLongImpl     => L128XMRNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => L128XMRNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => L128XMRNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----               Generic Interface                           -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Initialize <br>
        !  **Purpose**:  To initialize the pseudo-random number generator. <br>
        !  **Usage**: <br>
        !   ! initialize without seed; use L128X128Mix (default) algorithm <br>
        !   --->    CALL RNG%Initialize() <br>
        !   ! initialize with seed(s); use L128X128Mix (default) algorithm <br>
        !   --->    CALL RNG%Initialize(Seeds) <br>
        !   ! initialize without seed; use L128X256Mix algorithm <br>
        !   --->    CALL RNG%Initialize(L128X256Mix) <br>
        !   ! initialize with seed(s); use L128X1024Mix algorithm <br>
        !   --->    CALL RNG%Initialize(L128X1024Mix, Seeds)
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
        PROCEDURE       :: Split            => L128XMRNG_Split
        !> **Type-Bound Function**: Jump <br>
        !  **Purpose**:  To create a copy of the specified PRNG and then retreat
        !                the state of the current PRNG.  The copy is returned as
        !                a new PRNG. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%Jump() <br>
        !  **Note**: The *Jump* method is equivalent to calling the *NextInteger*
        !            method repeatedly a large number of times.
        PROCEDURE           :: Jump             => L128XMRNG_Jump
        !> **Type-Bound Function**: LongJump <br>
        !  **Purpose**:  To create a copy of the specified PRNG and then retreat
        !                the state of the current PRNG.  The copy is returned as
        !                a new PRNG. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%LongJump() <br>
        !  **Note**: The *LongJump* method is equivalent to calling the *NextInteger*
        !            method repeatedly a very large number of times.
        PROCEDURE           :: LongJump         => L128XMRNG_LongJump
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the PRNG.
        FINAL           :: L128XMRNG_Finalization
        ! ---------------------------------------------------------------------
    END TYPE L128XMRNG

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Next(RNG, NextIndex) RESULT(Output)
            IMPORT
            CLASS(L128XMRNG), INTENT(IN)    :: RNG
            tSInt32,          INTENT(OUT)   :: NextIndex
            tSInt64                         :: Output
        END FUNCTION Next
        SUBROUTINE Update(RNG, NextIndex)
            IMPORT
            CLASS(L128XMRNG), INTENT(INOUT) :: RNG
            tSInt32,          INTENT(IN)    :: NextIndex
        END SUBROUTINE Update
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE L128XMRNG_InitWithFlag(RNG, L128XMAlgo, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified algorithm flag.  Optionally,
    !  initial seeds can be specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'L128XMRNG' object
    CLASS(L128XMRNG),   INTENT(INOUT)   :: RNG
    !% algorithm flag (1-4); if out of applicable range, set it to default (L128X128Mix)
    tSInt32,            INTENT(IN)      :: L128XMAlgo
    !% optional seed(s)
    tSInt64,  OPTIONAL, INTENT(IN)      :: Seed(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check and set algorithm flag
    IF (IN_RANGE(L128XMAlgo, 1, 3)) THEN
        RNG%L128XMAlgo = L128XMAlgo
    ELSE
        RNG%L128XMAlgo = L128X128Mix
    END IF

    IF (PRESENT(Seed)) THEN
        ! initialize the generator with seed(s)
        CALL RNG%Initialize(Seed)
    ELSE
        ! initialize the generator without seed
        CALL RNG%Initialize()
    END IF

    RETURN

END SUBROUTINE L128XMRNG_InitWithFlag

!******************************************************************************

SUBROUTINE L128XMRNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'L128XMRNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128XMRNG), INTENT(INOUT) :: RNG      !! 'L128XMRNG' object
    tSInt64,          INTENT(IN)    :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64, ALLOCATABLE    :: Seed0(:)
    tIndex                  :: SeedSize, XBGStateSize

! FLOW

    ! get array sizes and allocate working arrays
    SELECT CASE (RNG%L128XMAlgo)
    CASE (L128X128Mix)
        XBGStateSize = 2_kIndex
    CASE (L128X256Mix)
        XBGStateSize = 4_kIndex
    CASE (L128X1024Mix)
        XBGStateSize = 16_kIndex
    END SELECT
    SeedSize = XBGStateSize + LCG_STATE_SIZE
    CALL MemAlloc(Seed0, SeedSize, StartID=0_kIndex)
    CALL MemAlloc(RNG%X, XBGStateSize, StartID=0_kIndex)

    ! set initial seed
    CALL Extend_Seed(Seed, Seed0)

    ! set states
    RNG%AH = Seed0(0)
    RNG%AL = IOR(Seed0(1), 1_kInt64)   ! Additive parameter must be odd
    RNG%SH = Seed0(2)
    RNG%SL = Seed0(3)
    RNG%X(0:XBGStateSize-1) = Seed0(4:SeedSize-1)

    ! Initializing to (XBGStateSize - 1) ensures that (Indx + 1) % XBGStateSize == 0
    ! and the first state picked from the XBG generator is state[0].
    RNG%Indx = XBGStateSize - 1_kIndex

    ! set pointers to NextOutput and Update procedures
    SELECT CASE (RNG%L128XMAlgo)
    CASE (L128X128Mix)
        RNG%NextOutput  => X128256_Next
        RNG%UpdateState => X128_Update
    CASE (L128X256Mix)
        RNG%NextOutput  => X128256_Next
        RNG%UpdateState => X256_Update
    CASE (L128X1024Mix)
        RNG%NextOutput  => X1024_Next
        RNG%UpdateState => X1024_Update
    END SELECT

    RETURN

END SUBROUTINE L128XMRNG_BaseInit

!******************************************************************************

FUNCTION L128XMRNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128XMRNG), INTENT(INOUT) :: RNG      !! 'L128XMRNG' object
    tSInt64                         :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32        :: NextIndex

! FLOW

    ! compute next output
    RandNum = RNG%NextOutput(NextIndex)

    ! update states
    CALL RNG%UpdateState(NextIndex)

    RETURN

END FUNCTION L128XMRNG_NextLong

!******************************************************************************

FUNCTION L128XMRNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128XMRNG), INTENT(IN)    :: RNG      !! 'L128XMRNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT CASE (RNG%L128XMAlgo)
    CASE (L128X128Mix)
        Name = 'L128X128MixRNG'
    CASE (L128X256Mix)
        Name = 'L128X256MixRNG'
    CASE (L128X1024Mix)
        Name = 'L128X1024MixRNG'
    END SELECT

    RETURN

END FUNCTION L128XMRNG_GetName

!******************************************************************************

FUNCTION L128XMRNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128XMRNG), INTENT(IN) :: RNG      !! 'L128XMRNG' object
    tIndex                      :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: XBGStateSize

! FLOW

    SELECT CASE (RNG%L128XMAlgo)
    CASE (L128X128Mix)
        XBGStateSize = 2_kIndex
    CASE (L128X256Mix)
        XBGStateSize = 4_kIndex
    CASE (L128X1024Mix)
        XBGStateSize = 16_kIndex
    END SELECT
    Size = XBGStateSize + LCG_STATE_SIZE

    RETURN

END FUNCTION L128XMRNG_GetSeedSize

!******************************************************************************

FUNCTION UnsignedAddHigh(X, Y) RESULT(Z)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute upper 64 bits of addition of two 64-bit unsigned integers
    !  This method is computing a carry bit for a 128-bit linear congruential
    !  generator (LCG).  The method is not applicable to all arguments.
    !  Some computations can be dropped if the Y argument is assumed to
    !  be the LCG addition, which should be odd to ensure a full period LCG.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: X, Y     !! Y is assumed to have the lowest bit set to 1.
    tSInt64             :: Z        !! the carry (either 0 or 1)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ! Method compiles to 13 bytes as Java byte code.
    ! This is below the default of 35 for code inlining.
    !
    ! The unsigned add of left + right may have a 65-bit result.
    ! If both values are shifted right by 1 then the sum will be
    ! within a 64-bit long. The right is assumed to have a low
    ! bit of 1 which has been lost in the shift. The method must
    ! compute if a 1 was shifted off the left which would have
    ! triggered a carry when adding to the right's assumed 1.
    ! The intermediate 64-bit result is shifted
    ! 63 bits to obtain the most significant bit of the 65-bit result.
    ! Using -1 is the same as a shift of (64 - 1) as only the last 6 bits
    ! are used by the shift but requires 1 less byte in java byte code.
    !
    !    01100001      left
    ! +  10011111      right always has low bit set to 1
    !
    !    0110000   1   carry last bit of left
    ! +  1001111   |
    ! +        1 <-+
    ! = 10000000       carry bit generated
    Z = SHIFTL((SHIFTR(X, 1) + SHIFTR(Y, 1) + IAND(X, 1_kInt64)), 1)

    RETURN

END FUNCTION UnsignedAddHigh

!******************************************************************************

FUNCTION X128256_Next(RNG, NextIndex) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128XMRNG), INTENT(IN)    :: RNG          !! 'L128X128RNG'
    tSInt32,          INTENT(OUT)   :: NextIndex    !! dummy here
    tSInt64                         :: RandNum      !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! Compute the RandNum based on current state information
    ! (this allows the computation to be overlapped with state update).
    RandNum = Mix_Lea(RNG%SH + RNG%X(0))

    NextIndex = 0

    RETURN

END FUNCTION X128256_Next

!******************************************************************************

SUBROUTINE X128_Update(RNG, NextIndex)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To update the states of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128XMRNG), INTENT(INOUT) :: RNG          !! 'L128X128RNG' object
    tSInt32,          INTENT(IN)    :: NextIndex    !! dummy here

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: U, Q0, Q1

! FLOW

    ! Update the LCG subgenerator
    ! The LCG is, in effect, s = ((1LL << 64) + ML) * s + a, if only we had 128-bit arithmetic.
    ! High half
    U = M128L * RNG%SL
    RNG%SH = (M128L * RNG%SH) + UnsignedMultiplyHigh(M128L, RNG%SL) + RNG%SL + RNG%AH &
                              + UnsignedAddHigh(U, RNG%AL)      ! Carry propagation
    ! Low half
    RNG%SL = U + RNG%AL

    ! Update the XBG subgenerator
    Q0 = RNG%X(0)
    Q1 = RNG%X(1)

    ! xoroshiro128v1_0
    Q1 = IEOR(Q1, Q0)
    Q0 = RotateLeft(Q0, 24)
    Q0 = IEOR(IEOR(Q0, Q1), SHIFTL(Q1, 16))
    Q1 = RotateLeft(Q1, 37)

    RNG%X(0) = Q0
    RNG%X(1) = Q1
    ASSOCIATE (Dummy => NextIndex); END ASSOCIATE

    RETURN

END SUBROUTINE X128_Update

!******************************************************************************

SUBROUTINE X256_Update(RNG, NextIndex)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To update the states of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128XMRNG), INTENT(INOUT) :: RNG          !! 'L128X128RNG' object
    tSInt32,          INTENT(IN)    :: NextIndex    !! dummy here

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: U, Q0, Q1, Q2, Q3, T

! FLOW

    ! Update the LCG subgenerator
    ! The LCG is, in effect, s = ((1LL << 64) + ML) * s + a, if only we had 128-bit arithmetic.
    ! High half
    U = M128L * RNG%SL
    RNG%SH = (M128L * RNG%SH) + UnsignedMultiplyHigh(M128L, RNG%SL) + RNG%SL + RNG%AH &
                              + UnsignedAddHigh(U, RNG%AL)      ! Carry propagation
    ! Low half
    RNG%SL = U + RNG%AL

    ! Update the XBG subgenerator
    Q0 = RNG%X(0)
    Q1 = RNG%X(1)
    Q2 = RNG%X(2)
    Q3 = RNG%X(3)

    ! xoshiro256 1.0
    T  = SHIFTL(Q1, 17)
    Q2 = IEOR(Q2, Q0)
    Q3 = IEOR(Q3, Q1)
    Q1 = IEOR(Q1, Q2)
    Q0 = IEOR(Q0, Q3)
    Q2 = IEOR(Q2, T)
    Q3 = RotateLeft(Q3, 45)

    RNG%X(0) = Q0
    RNG%X(1) = Q1
    RNG%X(2) = Q2
    RNG%X(3) = Q3
    ASSOCIATE (Dummy => NextIndex); END ASSOCIATE

    RETURN

END SUBROUTINE X256_Update

!******************************************************************************

FUNCTION X1024_Next(RNG, NextIndex) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128XMRNG), INTENT(IN)    :: RNG          !! 'L128X128RNG'
    tSInt32,          INTENT(OUT)   :: NextIndex    !! dummy here
    tSInt64                         :: RandNum      !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    NextIndex = IAND(RNG%Indx+1, SIZE(RNG%X)-1)

    ! Compute the RandNum based on current state information
    ! (this allows the computation to be overlapped with state update).
    RandNum = Mix_Lea(RNG%SH + RNG%X(NextIndex))

    RETURN

END FUNCTION X1024_Next

!******************************************************************************

SUBROUTINE X1024_Update(RNG, NextIndex)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To update the states of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128XMRNG), INTENT(INOUT) :: RNG          !! 'L128X128RNG' object
    tSInt32,          INTENT(IN)    :: NextIndex    !! dummy here

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: S0, S15, U
    tSInt32     :: Q

! FLOW

    ! First part of xoroshiro1024: fetch array data
    Q = RNG%Indx
    RNG%Indx = NextIndex
    S0  = RNG%X(RNG%Indx)
    S15 = RNG%X(Q)

    ! Update the LCG subgenerator
    ! The LCG is, in effect, s = ((1LL << 64) + ML) * s + a, if only we had 128-bit arithmetic.
    ! High half
    U = M128L * RNG%SL
    RNG%SH = (M128L * RNG%SH) + UnsignedMultiplyHigh(M128L, RNG%SL) + RNG%SL + RNG%AH &
                              + UnsignedAddHigh(U, RNG%AL)      ! Carry propagation
    ! Low half
    RNG%SL = U + RNG%AL

    ! Second part of xoroshiro1024: update array data
    S15 = IEOR(S15, S0)
    RNG%X(Q) = IEOR(IEOR(RotateLeft(S0, 25), S15), SHIFTL(S15, 27))
    RNG%X(RNG%Indx) = RotateLeft(S15, 36)

    RETURN

END SUBROUTINE X1024_Update

!******************************************************************************

FUNCTION L128XMRNG_Copy(Src) RESULT(Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy all components of the source object to the destination object

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128XMRNG), INTENT(INOUT) :: Src  !! source object
    TYPE(L128XMRNG)                 :: Dst  !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Dst%AH          =  Src%AH
    Dst%AL          =  Src%AL
    Dst%SH          =  Src%SH
    Dst%SL          =  Src%SL
    ALLOCATE(Dst%X, SOURCE=Src%X)
    Dst%Indx        =  Src%Indx
    Dst%L128XMAlgo  =  Src%L128XMAlgo
    Dst%NextOutput  => Src%NextOutput
    Dst%UpdateState => Src%UpdateState
    CALL Src%CopySeed(Dst)

    RETURN

END FUNCTION L128XMRNG_Copy

!******************************************************************************

FUNCTION L128XMRNG_Jump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  instance.  The new instance is returned.  <br>
    !  The jump is performed by advancing the state of the LCG sub-generator by 1 cycle.
    !  The XBG state is unchanged.  <br>
    !  For L128X128MixRNG, the jump size is the equivalent of moving the state backwards
    !  by (2<sup>128</sup> - 1) positions.  It can provide up to 2<sup>128</sup>
    !  non-overlapping subsequences.  <br>
    !  For L128X256MixRNG, the jump size is the equivalent of moving the state backwards
    !  by (2<sup>256</sup> - 1) positions.  It can provide up to 2<sup>128</sup>
    !  non-overlapping subsequences.  <br>
    !  For L128X1024MixRNG, the jump size is the equivalent of moving the state backwards
    !  by (2<sup>1024</sup> - 1) positions.  It can provide up to 2<sup>128</sup>
    !  non-overlapping subsequences.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128XMRNG), INTENT(INOUT) :: RNG      !! 'L128XMRNG' object
    TYPE(L128XMRNG)                 :: NewRNG   !! new 'L128XMRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: SH, SL, U

! FLOW

    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()

    ! Advance the LCG 1 step
    ! The LCG is, in effect, "s = m * s + a" where m = ((1LL << 64) + ML)
    SH = RNG%SH
    SL = RNG%SL
    U = M128L * SL
    ! High half
    RNG%SH = M128L * SH + UnsignedMultiplyHigh(M128L, SL) + SL + RNG%AH + &
             UnsignedAddHigh(U, RNG%AL)     ! Carry propagation
    ! Low half
    RNG%SL = U + RNG%AL

    RETURN

END FUNCTION L128XMRNG_Jump

!******************************************************************************

FUNCTION L128XMRNG_LongJump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  instance.  The new instance is returned.  <br>
    !  The jump is performed by advancing the state of the LCG sub-generator by
    !  2<sup>64</sup> cycles.  The XBG state is unchanged.  <br>
    !  For L128X128MixRNG, the jump size is the equivalent of moving the state backwards
    !  by roughly 2<sup>192</sup> positions.  It can provide up to 2<sup>64</sup>
    !  non-overlapping subsequences of length approximately 2<sup>192</sup>; each
    !  subsequence can provide up to 2<sup>64</sup> non-overlapping subsequences of
    !  length (2<sup>128</sup> - 1) using the Jump() method.  <br>
    !  For L128X256MixRNG, the jump size is the equivalent of moving the state backwards
    !  by roughly 2<sup>320</sup> positions.  It can provide up to 2<sup>64</sup>
    !  non-overlapping subsequences of length approximately 2<sup>320</sup>; each
    !  subsequence can provide up to 2<sup>64</sup> non-overlapping subsequences of
    !  length (2<sup>256</sup> - 1) using the Jump() method.  <br>
    !  For L128X256MixRNG, the jump size is the equivalent of moving the state backwards
    !  by roughly 2<sup>1088</sup> positions.  It can provide up to 2<sup>64</sup>
    !  non-overlapping subsequences of length approximately 2<sup>1088</sup>; each
    !  subsequence can provide up to 2<sup>64</sup> non-overlapping subsequences of
    !  length (2<sup>1024</sup> - 1) using the Jump() method.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128XMRNG), INTENT(INOUT) :: RNG      !! 'L128XMRNG' object
    TYPE(L128XMRNG)                 :: NewRNG   !! new 'L128XMRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()

    ! Advance the LCG 2**64 steps
    RNG%SH = RNG%SH + M128PH * RNG%SL + C128PH * RNG%AL

    RETURN

END FUNCTION L128XMRNG_LongJump

!******************************************************************************

FUNCTION L128XMRNG_Split(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a new generator split off from the current instance.
    !  Use the current generator to generate an initial seed and also
    !  employ it as a source of randomness used to initialize the
    !  new generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128XMRNG), INTENT(INOUT) :: RNG      !! 'L128XMRNG' object
    TYPE(L128XMRNG)                 :: NewRNG   !! new 'L128XMRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex                  :: SeedSize, I
    tSInt64                 :: Seed, X
    tSInt64, ALLOCATABLE    :: S(:)

! FLOW

    ! allocate working variables
    SeedSize = RNG%GetSeedSize()
    CALL MemAlloc(S, SeedSize, StartID=0_kIndex)

    Seed = RNG%NextLong()

    ! LCG state. The addition lower-half uses the input seed.
    ! The LCG addition parameter is set to odd so left-shift the seed.
    S(0) = RNG%NextLong()
    S(1) = SHIFTL(Seed, 1)
    S(2) = RNG%NextLong()
    S(3) = RNG%NextLong()

    ! XBG state must not be all zero
    X = 0_kInt64
    DO I = LCG_STATE_SIZE, SeedSize-1
        S(I) = RNG%NextLong()
        X = IOR(X, S(I))
    END DO
    IF (X == 0_kInt64) THEN
        !/ SplitMix style seed ensures at least one non-zero value
        X = S(LCG_STATE_SIZE - 1_kIndex)
        DO I = LCG_STATE_SIZE, SeedSize-1
            S(I) = Mix_Lea(X)
            X = X + GOLDEN_RATIO_64
        END DO
    END IF

    ! initialize the new generator
    CALL NewRNG%Initialize(RNG%L128XMAlgo, S)

    RETURN

END FUNCTION L128XMRNG_Split

!******************************************************************************

SUBROUTINE L128XMRNG_Finalization(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the 'L128XMRNG' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(L128XMRNG), INTENT(INOUT)   :: RNG  !! 'L128XMRNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemFree(RNG%X)
    NULLIFY(RNG%NextOutput)
    NULLIFY(RNG%UpdateState)

    RETURN

END SUBROUTINE L128XMRNG_Finalization

!******************************************************************************

END MODULE MClass_L128XMRNG

!******************************************************************************
