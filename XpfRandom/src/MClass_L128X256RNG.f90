
MODULE MClass_L128X256RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *L128X256RNG* type and its related routines.
!   The *L128X256RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type. <br>
!   In particular, the *L128X256RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the *L128X256* algorithm.  The
!   *L128X256* algorithm is a specific member of the LXM family of algorithms
!   for pseudo-random number generators where <br>
!   -  L stands for Linear congruential generator (LCG); <br>
!   -  X stands for Xor-based generator (XBG); and <br>
!   -  M stands for Mix. <br>
!   The *L128X256RNG* type employs a 128-bit LCG, a 256-bit XBG and a mixing function
!   where its memory footprint is 512 bits and its period is roughly 2<sup>384</sup>. <br>
!   In addition to common operations of a PRNG, the *L128X256RNG* type provides
!   the *Split* method to split a generator into two instances (the original and
!   a new instance) where the two generators can be used concurrently.  Presumably,
!   the new generator is statistically independent and uniform.  <br>
!   It is important to note that the *L128X256* PRNG requires an explicit
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
    USE MBase_UIntUtil,     ONLY: OPERATOR (.ULT.), U128_Multiply_High => UMul128_Upper64
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: L128X256RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *L128X256RNG* type is a *Long* PRNG type based on the *L128X256*
    !  algorithms, which are specific members of the LXM family of algorithms
    !  for pseudo-random number generators.
    TYPE, EXTENDS(LongRNG)  :: L128X256RNG
        PRIVATE
        ! The parameters that are used as additive constants for the LCG.  AL must be odd.
        tSInt64     :: AH, AL
        ! The per-instance state: SH and SL for the LCG, and X0, X1, X2 and X3 for the XBG.
        ! At least one of the four fields Xs must be nonzero.
        tSInt64     :: SH, SL, X0, X1, X2, X3
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => L128X256RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextLongImpl     => L128X256RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => L128X256RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => L128X256RNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----                 Public Procedures                         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Split <br>
        !  **Purpose**:  To return a new PRNG, split off from the current one. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%Split() <br>
        !  **Note**: The *Split* method is intended to be used in a parallel environment
        !            where the current generator and the new one can be used concurrently.
        PROCEDURE       :: Split            => L128X256RNG_Split
        ! ---------------------------------------------------------------------
    END TYPE L128X256RNG

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE L128X256RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'L128X256RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128X256RNG), INTENT(INOUT)   :: RNG          !! 'L128X256RNG' object
    tSInt64,            INTENT(IN)      :: Seed(:)      !! seed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64  :: InitSeed1, InitSeed2, XSeed1, XSeed2, XSeed3

! FLOW

    ! set initial seed
    InitSeed1 = IEOR(Seed(1), SILVER_RATIO_64)
    InitSeed2 = InitSeed1 + GOLDEN_RATIO_64
    XSeed1 = InitSeed2 + GOLDEN_RATIO_64
    XSeed2 = XSeed1 + GOLDEN_RATIO_64
    XSeed3 = XSeed2 + GOLDEN_RATIO_64
    ! set parameters as follows:
    ! The seed is hashed by Mix_Murmur to produce the 'A' parameter.
    ! The seed is hashed by Mix_Stafford_13 to produce the initial 'X0',
    ! which will then be used to produce the first generated value.
    ! Then 'X1' is filled in as if by a SplitMix PRNG with GOLDEN_RATIO_64
    ! as the gamma value and Mix_Stafford_13 as the mixer.
    CALL L128X256RNG_SetParameters(RNG, Mix_Murmur(InitSeed1), Mix_Murmur(InitSeed2), &
                                   0_kInt64, 1_kInt64, Mix_Stafford_13(InitSeed2),            &
                                   Mix_Stafford_13(XSeed1), Mix_Stafford_13(XSeed2),        &
                                   Mix_Stafford_13(XSeed3))

    RETURN

END SUBROUTINE L128X256RNG_BaseInit

!******************************************************************************

FUNCTION L128X256RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128X256RNG), INTENT(INOUT)   :: RNG      !! 'L128X256RNG' object
    tSInt64                             :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    ! Low half of multiplier used in the LCG portion of the algorithm;
    ! the overall multiplier is (2**64 + ML).
    ! Chosen based on research by Sebastiano Vigna and Guy Steele (2019).
    ! The spectral scores for dimensions 2 through 8 for the multiplier 0x1d605bbb58c8abbfdLL
    ! are [0.991889, 0.907938, 0.830964, 0.837980, 0.780378, 0.797464, 0.761493].
    tSInt64, PARAMETER  ::ML = ToInt64(Z'D605BBB58C8ABBFD')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: U, Q0, Q1, Q2, Q3, T

! FLOW

    ! Compute the RandNum based on current state information
    ! (this allows the computation to be overlapped with state update).
    RandNum = Mix_Lea(RNG%SH + RNG%X0)

    ! Update the LCG subgenerator
    ! The LCG is, in effect, s = ((1LL << 64) + ML) * s + a, if only we had 128-bit arithmetic.
    U = ML * RNG%SL
    RNG%SH = (ML * RNG%SH) + U128_Multiply_High(ML, RNG%SL) + RNG%SL + RNG%AH
    RNG%SL = U + RNG%AL
    ! Handle the carry propagation from low half to high half.
    IF (RNG%SL .ULT. U) RNG%SH = RNG%SH + 1_kInt64

    ! Update the XBG subgenerator
    Q0 = RNG%X0
    Q1 = RNG%X1
    Q2 = RNG%X2
    Q3 = RNG%X3

    ! xoshiro256 1.0
    T  = SHIFTL(Q1, 17)
    Q2 = IEOR(Q2, Q0)
    Q3 = IEOR(Q3, Q1)
    Q1 = IEOR(Q1, Q2)
    Q0 = IEOR(Q0, Q3)
    Q2 = IEOR(Q2, T)
    Q3 = RotateLeft(Q3, 45)

    RNG%X0 = Q0
    RNG%X1 = Q1
    RNG%X2 = Q2
    RNG%X3 = Q3

    RETURN

END FUNCTION L128X256RNG_NextLong

!******************************************************************************

FUNCTION L128X256RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128X256RNG), INTENT(IN)  :: RNG      !! 'L128X256RNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'L128X256RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION L128X256RNG_GetName

!******************************************************************************

FUNCTION L128X256RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(L128X256RNG), INTENT(IN)  :: RNG      !! 'L128X256RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 1
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION L128X256RNG_GetSeedSize

!******************************************************************************

SUBROUTINE L128X256RNG_SetParameters(RNG, AH, AL, SH, SL, X0, X1, X2, X3)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set parameters of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(L128X256RNG), INTENT(INOUT) :: RNG
    tSInt64,           INTENT(IN)    :: AH, AL
    tSInt64,           INTENT(IN)    :: SH, SL
    tSInt64,           INTENT(IN)    :: X0, X1
    tSInt64,           INTENT(IN)    :: X2, X3

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64 :: V

! FLOW

    ! Force a to be odd
    RNG%AH = AH
    RNG%AL = IOR(AL, 1_kInt64)
    RNG%SH = SH
    RNG%SL = SL

    ! check if all Xs are zero or not
    IF (IOR(IOR(IOR(X0, X1), X2), X3) == 0) THEN
        ! If all Xs are zero, we must choose nonzero values.
        V = SH + GOLDEN_RATIO_64
        ! At least three of the four values generated here will be nonzero.
        RNG%X0 = Mix_Stafford_13(V)
        V = V + GOLDEN_RATIO_64
        RNG%X1 = Mix_Stafford_13(V)
        V = V + GOLDEN_RATIO_64
        RNG%X2 = Mix_Stafford_13(V)
        RNG%X3 = Mix_Stafford_13(V + GOLDEN_RATIO_64)
    ELSE
        RNG%X0 = X0
        RNG%X1 = X1
        RNG%X2 = X2
        RNG%X3 = X3
    END IF

    RETURN

END SUBROUTINE L128X256RNG_SetParameters

!******************************************************************************

FUNCTION L128X256RNG_Split(RNG, Brine) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize and return a new 'L128X256RNG' object that shares no mutable
    !  state with this object. However, with very high probability, the set of
    !  values collectively generated by the two objects has the same statistical
    !  properties as if the same quantity of values were generated by a single
    !  thread using a single object.  Either or both of the two objects may be
    !  further split using this routine, and the same expected statistical
    !  properties apply to the entire set of generators constructed by such
    !  recursive splitting.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'L128X256RNG' object
    CLASS(L128X256RNG), INTENT(INOUT)   :: RNG
    !> a long value, of which the low 63 bits provide a unique id among calls
    !  to this routine for constructing a single series of Generator objects.
    tSInt64, OPTIONAL,INTENT(IN)        :: Brine
    !% new 'L128X256RNG' object
    TYPE(L128X256RNG)                   :: NewRNG

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
    ! of the salt for (the low half of) 'A'
    CALL L128X256RNG_SetParameters(NewRNG, RNG%NextLong(), SHIFTL(Salt, 1), RNG%NextLong(), &
                                   RNG%NextLong(), RNG%NextLong(), RNG%NextLong(),          &
                                   RNG%NextLong(), RNG%NextLong())

    RETURN

END FUNCTION L128X256RNG_Split

!******************************************************************************

END MODULE MClass_L128X256RNG

!******************************************************************************
