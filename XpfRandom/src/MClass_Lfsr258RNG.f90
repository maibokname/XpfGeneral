
MODULE MClass_Lfsr258RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Lfsr258RNG* type and its related routines.
!   The *Lfsr258RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *Lfsr258RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the 64-bit *LFSR* ( composite
!   linear feedback shift register) algorithm by Pierre L'Ecuyer. <br>
!   The *LFSR258* PRNG has five 64-bit states combined by a bitwise xor.
!   Its period length is approximatively 2<sup>258</sup>. <br>
!   It is important to note that the *LFSR258* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  Also, it should be noted that the implementation of
!   this PRNG is based on reference #2-3. <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.ams.org/journals/mcom/1999-68-225/S0025-5718-99-01039-X/">
!       L'Ecuyer, P. 1999. Tables of maximally equidistributed combined LFSR
!       generators. Mathematics of Computation, 68(225): 261-269. </a> <br>
!   [2] <a href="https://wp.csiro.au/alanmiller/random/lfsr258.f90">
!       Lin_Feedback_Shift_Reg: a Fortran 90 module implementation of the LFSR258
!       generator by Alan Miller. </a> <br>
!   [3] <a href="http://umontreal-simul.github.io/ssj/docs/master/classumontreal_1_1ssj_1_1rng_1_1LFSR258.html">
!       Package umontreal.ssj.rng: LFSR258 Class Reference. </a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Lfsr258RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_Lfsr258RNG'

!** DERIVED TYPE DEFINITIONS
    !> The *Lfsr258RNG* type is a *Long* PRNG type based on 64-bit composite
    !  linear feedback shift register (LFSR) algorithm by Pierre L'Ecuyer.
    TYPE, EXTENDS(LongRNG)  :: Lfsr258RNG
        PRIVATE
        tSInt64     :: State1 =  153587801
        tSInt64     :: State2 = -759022222
        tSInt64     :: State3 =  1288503317
        tSInt64     :: State4 = -1718083407
        tSInt64     :: State5 = -123456789
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => Lfsr258RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl     => Lfsr258RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => Lfsr258RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => Lfsr258RNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----                 Overridden Procedures                     -----
        ! ---------------------------------------------------------------------
        !> *NextDoubleImpl* is an overridden procedure. <br>
        !  Use the *NextDouble* method in place of the *NextDoubleImpl* method
        !  to generate a 64-bit real number.
        PROCEDURE       :: NextDoubleImpl               => Lfsr258RNG_NextDouble
        !> *Default_NextIntegerLimits* is an overridden procedure. <br>
        !  Use the *NextInteger* method in place of the *Default_NextIntegerLimits*
        !  method to generate a 32-bit integer number between the given bound.
        PROCEDURE       :: Default_NextIntegerLimits    => Lfsr258RNG_NextIntegerLimits
        ! ---------------------------------------------------------------------
    END TYPE Lfsr258RNG

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Lfsr258RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Lfsr258RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Lfsr258RNG), INTENT(INOUT)    :: RNG      !! 'Lfsr258RNG' object
    tSInt64,           INTENT(IN)       :: Seed(:)  !! seed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set initial seeds
    SELECT CASE (SIZE(Seed))
    CASE (1)
        RNG%State1 = IEOR(RNG%State1, Seed(1))
    CASE (2)
        RNG%State1 = IEOR(RNG%State1, Seed(1))
        RNG%State2 = IEOR(RNG%State2, Seed(2))
    CASE (3)
        RNG%State1 = IEOR(RNG%State1, Seed(1))
        RNG%State2 = IEOR(RNG%State2, Seed(2))
        RNG%State3 = IEOR(RNG%State3, Seed(3))
    CASE (4)
        RNG%State1 = IEOR(RNG%State1, Seed(1))
        RNG%State2 = IEOR(RNG%State2, Seed(2))
        RNG%State3 = IEOR(RNG%State3, Seed(3))
        RNG%State4 = IEOR(RNG%State4, Seed(4))
    CASE (5)
        RNG%State1 = IEOR(RNG%State1, Seed(1))
        RNG%State2 = IEOR(RNG%State2, Seed(2))
        RNG%State3 = IEOR(RNG%State3, Seed(3))
        RNG%State4 = IEOR(RNG%State4, Seed(4))
        RNG%State5 = IEOR(RNG%State5, Seed(5))
    END SELECT

    IF (IAND(RNG%State1,       -2_kInt64) == 0_kInt64) RNG%State1 = RNG%State1 - 8388607_kInt64
    IF (IAND(RNG%State2,     -512_kInt64) == 0_kInt64) RNG%State2 = RNG%State2 - 8388607_kInt64
    IF (IAND(RNG%State3,    -4096_kInt64) == 0_kInt64) RNG%State3 = RNG%State3 - 8388607_kInt64
    IF (IAND(RNG%State4,  -131072_kInt64) == 0_kInt64) RNG%State4 = RNG%State4 - 8388607_kInt64
    IF (IAND(RNG%State5, -8388608_kInt64) == 0_kInt64) RNG%State5 = RNG%State5 - 8388607_kInt64

    RETURN

END SUBROUTINE Lfsr258RNG_BaseInit

!******************************************************************************

FUNCTION Lfsr258RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Lfsr258RNG), INTENT(INOUT)    :: RNG      !! 'Lfsr258RNG' object
    tSInt64                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64  :: B

! FLOW

    B          = SHIFTR(IEOR(SHIFTL(RNG%State1,      1), RNG%State1), 53)
    RNG%State1 = IEOR(SHIFTL(IAND(RNG%State1,       -2_kInt64), 10), B)
    B          = SHIFTR(IEOR(SHIFTL(RNG%State2,     24), RNG%State2), 50)
    RNG%State2 = IEOR(SHIFTL(IAND(RNG%State2,     -512_kInt64), 5), B)
    B          = SHIFTR(IEOR(SHIFTL(RNG%State3,      3), RNG%State3), 23)
    RNG%State3 = IEOR(SHIFTL(IAND(RNG%State3,    -4096_kInt64), 29), B)
    B          = SHIFTR(IEOR(SHIFTL(RNG%State4,      5), RNG%State4), 24)
    RNG%State4 = IEOR(SHIFTL(IAND(RNG%State4,  -131072_kInt64), 23), B)
    B          = SHIFTR(IEOR(SHIFTL(RNG%State5,      3), RNG%State5), 33)
    RNG%State5 = IEOR(SHIFTL(IAND(RNG%State5, -8388608_kInt64), 8), B)

    RandNum = IEOR(IEOR(IEOR(IEOR(RNG%State1, RNG%State2), RNG%State3), RNG%State4), RNG%State5)

    RETURN

END FUNCTION Lfsr258RNG_NextLong

!******************************************************************************

FUNCTION Lfsr258RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Lfsr258RNG), INTENT(IN)   :: RNG      !! 'Lfsr258RNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'Lfsr258RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Lfsr258RNG_GetName

!******************************************************************************

FUNCTION Lfsr258RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Lfsr258RNG), INTENT(IN)   :: RNG      !! 'Lfsr258RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 5
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Lfsr258RNG_GetSeedSize

!******************************************************************************

FUNCTION Lfsr258RNG_NextDouble(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random floating-point value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Lfsr258RNG), INTENT(INOUT)    :: RNG      !! 'Lfsr258RNG' object
    tRealDP                             :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tRealDP, PARAMETER  :: InvTwo24 = 5.9604644775390625E-8_kDouble     ! 2**(-24)
    tRealDP, PARAMETER  :: Epsilon  = 5.5511151231257827E-17_kDouble    ! 2**(-54)
    ! equivalent a NORM = 1.0 / ToInt64(Z'FFFFFFFFFFFFF800')
    tRealDP, PARAMETER  :: DNorm = 0.5_kDouble/ToInt64(Z'7FFFFFFFFFFFFC00')
    tRealDP, PARAMETER  :: DMax  = ToInt64(Z'FFFFFFFFFFFFF800')*DNorm + 1.0_kDouble

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I
    tSInt64     :: LongRnd
    tRealDP     :: U(2)

! FLOW

    DO I = 1, 2
        LongRnd = RNG%NextLong()
        ! Make sure that double values 0 and 1 never occur
        IF (LongRnd <= 0_kInt64) THEN
            U(I) = LongRnd*DNorm + DMax
        ELSE
            U(I) = LongRnd*DNorm
        END IF
    END DO
    
    RandNum = MOD((U(1) + U(2)* InvTwo24), 1.0_kDouble) + Epsilon

    RETURN

END FUNCTION Lfsr258RNG_NextDouble

!******************************************************************************

FUNCTION Lfsr258RNG_NextIntegerLimits(RNG, Bound1, Bound2) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To generate a random 32-bit-integer value in the specified range.
    !  If *Bound2* is not specified, the value is in the range between
    !  0 and *Bound1*.  Otherwise, the value is in the range between
    !  *Bound1* and *Bound2*. <br>
    !  It should be noted that both *Bound1* and *Bound2* arguments can
    !  have either a positive or a negative value.  The returned value
    !  is always in between the lower limit (inclusive) and the upper
    !  limit (exclusive). <br>
    !  This routine overrides the default implementation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Lfsr258RNG),  INTENT(INOUT)   :: RNG      !! 'Lfsr258RNG' object
    tSInt32,            INTENT(IN)      :: Bound1   !! a required limit
    tSInt32,  OPTIONAL, INTENT(IN)      :: Bound2   !! an optional limit
    tSInt32                             :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER  :: TwoPow66 = ToInt64(Z'4000000000000000')   ! 4*(2**64)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Lower, Diff
    tSInt64     :: D, Q, R, LongRnd

! FLOW

    ! check specified input and set Diff and Lower values
    IF (PRESENT(Bound2)) THEN
        Diff  = ABS(Bound1 - Bound2)
        Lower = MIN(Bound1, Bound2)
    ELSE
        Diff  = ABS(Bound1)
        Lower = MIN(Bound1, 0_kInt32)
    END IF
    
    ! return quickly if Diff is zero
    IF (Diff == 0_kInt32) THEN
        RandNum = Bound1
        RETURN
    END IF

    D = Diff + 1_kInt64
    Q = TwoPow66 / D
    R = MOD(TwoPow66, D)
    DO
        LongRnd = SHIFTR(RNG%NextLong(), 2)
        IF (LongRnd < TwoPow66 - R) EXIT
    END DO

    RandNum = ToInt32(LongRnd / Q) + Lower

    RETURN

END FUNCTION Lfsr258RNG_NextIntegerLimits

!******************************************************************************

END MODULE MClass_Lfsr258RNG

!******************************************************************************
