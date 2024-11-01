
MODULE MClass_Mrg32k3aRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Mrg32k3aRNG* type and its related routines.
!   The *Mrg32k3aRNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *Mrg32k3aRNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on the 32-bit *MRG* (combined
!   multiple recursive generator) algorithm by Pierre L'Ecuyer. <br>
!   The *Mrg32k3a* PRNG has six 32-bit states stored in 64-bit integers.
!   Its period length is approximatively 2<sup>191</sup>. <br>
!   It is important to note that the *Mrg32k3a* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  Also, it should be noted that the implementation of
!   this PRNG is based on references #2-3. <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.ams.org/journals/mcom/1999-68-225/S0025-5718-99-01039-X/">
!       L'Ecuyer, P. 1999. Good Parameters and Implementations for Combined Multiple
!       Recursive Random Number Generators. Operations Research 47(1):159-164. </a> <br>
!   [2] <a href="http://umontreal-simul.github.io/ssj/docs/master/classumontreal_1_1ssj_1_1rng_1_1MRG32k3aL.html">
!       Package umontreal.ssj.rng: MRG32k3aL Class Reference. </a> <br>
!   [3] <a href="http://simul.iro.umontreal.ca/rng/MRG32k3a.c">
!       32-bits Random number generator U(0,1): MRG32k3a - a C source code. </a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_UIntUtil
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Mrg32k3aRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_Mrg32k3aRNG'
    tSInt64,   PARAMETER    :: M1           = 4294967087_kInt64
    tSInt64,   PARAMETER    :: M2           = 4294944443_kInt64
    tSInt64,   PARAMETER    :: A12          = 1403580_kInt64
    tSInt64,   PARAMETER    :: A13N         = 810728_kInt64
    tSInt64,   PARAMETER    :: A21          = 527612_kInt64
    tSInt64,   PARAMETER    :: A23N         = 1370589_kInt64
    tRealDP,   PARAMETER    :: Norm         = 2.328306549295727688E-10_kDouble
    tSInt64,   PARAMETER    :: DefaultSeed  = 12345_kInt64

!** DERIVED TYPE DEFINITIONS
    !> The *Mrg32k3aRNG* type is an *Integer* PRNG type based on 32-bit
    !  combined multiple recursive generator (MRG) algorithm by Pierre L'Ecuyer.
    TYPE, EXTENDS(IntegerRNG)  :: Mrg32k3aRNG
        PRIVATE
        ! states
        tSInt64 :: State(0:5) = DefaultSeed
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: NextValue    => Mrg32k3aRNG_NextValue
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE   :: BaseInit         => Mrg32k3aRNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE   :: NextIntegerImpl  => Mrg32k3aRNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE   :: GetName          => Mrg32k3aRNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE   :: GetSeedSize      => Mrg32k3aRNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----                 Overridden Procedures                     -----
        ! ---------------------------------------------------------------------
        !> *NextDoubleImpl* is an overridden procedure. <br>
        !  Use the *NextDouble* method in place of the *NextDoubleImpl* method
        !  to generate a 64-bit real number.
        PROCEDURE   :: NextDoubleImpl               => Mrg32k3aRNG_NextDouble
        !> *Default_NextIntegerLimits* is an overridden procedure. <br>
        !  Use the *NextInteger* method in place of the *Default_NextIntegerLimits*
        !  method to generate a 32-bit integer number between the given bound.
        PROCEDURE   :: Default_NextIntegerLimits    => Mrg32k3aRNG_NextIntegerLimits
        ! ---------------------------------------------------------------------
    END TYPE Mrg32k3aRNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Mrg32k3aRNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Mrg32k3aRNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mrg32k3aRNG), INTENT(INOUT)   :: RNG      !! 'Mrg32k3aRNG' object
    tSInt32,            INTENT(IN)      :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: LongSeed(6)
    tIndex      :: I
    tLogical    :: AreAllZero

! FLOW
    
    ! set LongSeed to default seed
    LongSeed = DefaultSeed
    
    ! set LongSeed to specified seeds
    DO I = 1, SIZE(Seed)
        LongSeed(I) = IEOR(LongSeed(I), ToUnsignedLong(Seed(I)))
        IF (I == 6) EXIT
    END DO
    
    ! check whether LongSeed are all zero or not
    AreAllZero = TrueVal
    DO I = 1, 6
        IF (LongSeed(I) /= 0_kInt64) THEN
            AreAllZero = FalseVal
            EXIT
        END IF
    END DO
    
    ! set state for valid elements of LongSeed
    IF (.NOT.AreAllZero) THEN
        DO I = 1, 3
            IF (LongSeed(I) < M1) RNG%State(I-1) = LongSeed(I)
        END DO
        DO I = 4, 6
            IF (LongSeed(I) < M2) RNG%State(I-1) = LongSeed(I)
        END DO
    END IF
    
    RETURN

END SUBROUTINE Mrg32k3aRNG_BaseInit

!******************************************************************************

FUNCTION Mrg32k3aRNG_NextValue(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value with 32-bit accuracy.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mrg32k3aRNG), INTENT(INOUT)   :: RNG      !! 'Mrg32k3aRNG' object
    tSInt64                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: P1, P2

! FLOW

    ! Component 1
    P1 = MOD((A12 * RNG%State(1) - A13N * RNG%State(0)), M1)
    IF (P1 < 0_kInt64) P1 = P1 + M1
    RNG%State(0) = RNG%State(1)
    RNG%State(1) = RNG%State(2)
    RNG%State(2) = P1

    ! Component 2
    P2 = MOD((A21 * RNG%State(5) - A23N * RNG%State(3)), M2)
    IF (P2 < 0_kInt64) P2 = P2 + M2
    RNG%State(3) = RNG%State(4)
    RNG%State(4) = RNG%State(5)
    RNG%State(5) = P2

    ! Combination
    IF (P1 > P2) THEN
        RandNum = P1 - P2
    ELSE
        RandNum = P1 - P2 + M1
    END IF
    
    RETURN

END FUNCTION Mrg32k3aRNG_NextValue

!******************************************************************************

FUNCTION Mrg32k3aRNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mrg32k3aRNG), INTENT(INOUT)   :: RNG      !! 'Mrg32k3aRNG' object
    tSInt32                             :: RandNum  !! random number
        
!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER  ::MaskL = ToInt64(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandNum = ToInt32(IAND(RNG%NextValue(), MaskL))
    
    RETURN

END FUNCTION Mrg32k3aRNG_NextInteger

!******************************************************************************

FUNCTION Mrg32k3aRNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mrg32k3aRNG), INTENT(IN)  :: RNG      !! 'Mrg32k3aRNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Mrg32k3aRNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Mrg32k3aRNG_GetName

!******************************************************************************

FUNCTION Mrg32k3aRNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mrg32k3aRNG), INTENT(IN)  :: RNG      !! 'Mrg32k3aRNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 6
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Mrg32k3aRNG_GetSeedSize

!******************************************************************************

FUNCTION Mrg32k3aRNG_NextDouble(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random floating-point value.  This routine
    !  overrides the default implementation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mrg32k3aRNG), INTENT(INOUT)   :: RNG      !! 'Mrg32k3aRNG' object
    tRealDP                             :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tRealDP, PARAMETER  :: InvTwo24 = 5.9604644775390625E-8_kDouble     ! 2**(-24)
    tRealDP, PARAMETER  :: Epsilon  = 5.5511151231257827E-17_kDouble    ! 2**(-54)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tRealDP     :: U(2)

! FLOW
    
    U(1) = RNG%NextValue()*Norm
    U(2) = RNG%NextValue()*Norm
    
    RandNum = MOD((U(1) + U(2)*InvTwo24), 1.0_kDouble) + Epsilon
    
    RETURN

END FUNCTION Mrg32k3aRNG_NextDouble

!******************************************************************************

FUNCTION Mrg32k3aRNG_NextIntegerLimits(RNG, Bound1, Bound2) RESULT(RandNum)

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
    CLASS(Mrg32k3aRNG), INTENT(INOUT)   :: RNG      !! 'Mrg32k3aRNG' object
    tSInt32,            INTENT(IN)      :: Bound1   !! a required limit
    tSInt32,  OPTIONAL, INTENT(IN)      :: Bound2   !! an optional limit
    tSInt32                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Lower, Diff

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

    ! This works even for an interval [0, 2**31 - 1]. It would not with 
    ! ToInt32(RNG%NextDouble*(Upper - Lower + 1)) + Lower
    RandNum = ToInt32(RNG%NextDouble()*(Diff + 1.0_kDouble)) + Lower

    RETURN

END FUNCTION Mrg32k3aRNG_NextIntegerLimits

!******************************************************************************

END MODULE MClass_Mrg32k3aRNG
    
!******************************************************************************
