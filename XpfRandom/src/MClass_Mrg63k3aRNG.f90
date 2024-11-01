
MODULE MClass_Mrg63k3aRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Mrg63k3aRNG* type and its related routines.
!   The *Mrg63k3aRNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by n *Long* PRNG type.  <br>
!   In particular, the *Mrg63k3aRNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the 63-bit *MRG* (combined
!   multiple recursive generator) algorithm by Pierre L'Ecuyer. <br>
!   The *Mrg63k3a* PRNG has six 63-bit states stored in 64-bit integers.
!   Its period length is approximatively 2<sup>377</sup>. <br>
!   It is important to note that the *Mrg63k3a* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  Also, it should be noted that the implementation of
!   this PRNG is based on references #2. <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.ams.org/journals/mcom/1999-68-225/S0025-5718-99-01039-X/">
!       L'Ecuyer, P. 1999. Good Parameters and Implementations for Combined Multiple
!       Recursive Random Number Generators. Operations Research 47(1):159-164. </a> <br>
!   [2] <a href="http://simul.iro.umontreal.ca/rng/MRG63k3a.c">
!       63-bits Random number generator U(0,1): MRG63k3a - a C source code. </a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Mrg63k3aRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_Mrg63k3aRNG'
    tSInt64,   PARAMETER    :: M1           = 9223372036854769163_kInt64
    tSInt64,   PARAMETER    :: M2           = 9223372036854754679_kInt64
    tSInt64,   PARAMETER    :: A12          = 1754669720_kInt64
    tSInt64,   PARAMETER    :: Q12          = 5256471877_kInt64
    tSInt64,   PARAMETER    :: R12          = 251304723_kInt64
    tSInt64,   PARAMETER    :: A13N         = 3182104042_kInt64
    tSInt64,   PARAMETER    :: Q13          = 2898513661_kInt64
    tSInt64,   PARAMETER    :: R13          = 394451401_kInt64
    tSInt64,   PARAMETER    :: A21          = 31387477935_kInt64
    tSInt64,   PARAMETER    :: Q21          = 293855150_kInt64
    tSInt64,   PARAMETER    :: R21          = 143639429_kInt64
    tSInt64,   PARAMETER    :: A23N         = 6199136374_kInt64
    tSInt64,   PARAMETER    :: Q23          = 1487847900_kInt64
    tSInt64,   PARAMETER    :: R23          = 985240079_kInt64
    tRealDP,   PARAMETER    :: Norm         = 1.0842021724855052E-19_kDouble
    tSInt64,   PARAMETER    :: DefaultSeed  = 123456789_kInt64

!** DERIVED TYPE DEFINITIONS
    !> The *Mrg63k3aRNG* type is a *Long* PRNG type based on 63-bit
    !  combined multiple recursive generator (MRG) algorithm by Pierre L'Ecuyer.
    TYPE, EXTENDS(LongRNG)  :: Mrg63k3aRNG
        PRIVATE
        ! states
        tSInt64     :: State(0:5) = DefaultSeed
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: NextValue    => Mrg63k3aRNG_NextValue
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE   :: BaseInit         => Mrg63k3aRNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE   :: NextLongImpl     => Mrg63k3aRNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE   :: GetName          => Mrg63k3aRNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE   :: GetSeedSize      => Mrg63k3aRNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----                 Overridden Procedures                     -----
        ! ---------------------------------------------------------------------
        !> *NextDoubleImpl* is an overridden procedure. <br>
        !  Use the *NextDouble* method in place of the *NextDoubleImpl* method
        !  to generate a 64-bit real number.
        PROCEDURE   :: NextDoubleImpl               => Mrg63k3aRNG_NextDouble
        !> *Default_NextIntegerLimits* is an overridden procedure. <br>
        !  Use the *NextInteger* method in place of the *Default_NextIntegerLimits*
        !  method to generate a 32-bit integer number between the given bound.
        PROCEDURE   :: Default_NextIntegerLimits    => Mrg63k3aRNG_NextIntegerLimits
        ! ---------------------------------------------------------------------
    END TYPE Mrg63k3aRNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Mrg63k3aRNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Mrg63k3aRNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mrg63k3aRNG), INTENT(INOUT)   :: RNG      !! 'Mrg63k3aRNG' object
    tSInt64,            INTENT(IN)      :: Seed(:)  !! seed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: LongSeed(6)
    tIndex      :: I
    tLogical    :: AreAllZero

! FLOW
    
    ! set LongSeed to default seed
    LongSeed = DefaultSeed
    
    ! set LongSeed to specified seeds
    DO I = 1, SIZE(Seed)
        LongSeed(I) = IEOR(LongSeed(I), Seed(I))
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

END SUBROUTINE Mrg63k3aRNG_BaseInit

!******************************************************************************

FUNCTION Mrg63k3aRNG_NextValue(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mrg63k3aRNG), INTENT(INOUT)   :: RNG      !! 'Mrg63k3aRNG' object
    tSInt64                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: H, P12, P13, P21, P23

! FLOW

    ! Component 1
    H = RNG%State(0) / Q13
    P13 = A13n * (RNG%State(0) - H * Q13) - H * R13
    H = RNG%State(1) / Q12
    P12 = A12 * (RNG%State(1) - H * Q12) - H * R12
    IF (P13 < 0) P13 = P13 + M1
    IF (P12 < 0) THEN
        P12 = P12 + M1 - P13
    ELSE
        P12 = P12 - P13
    END IF
    IF (P12 < 0) P12 = P12 + M1
    RNG%State(0) = RNG%State(1)
    RNG%State(1) = RNG%State(2)
    RNG%State(2) = P12

    ! Component 2
    H = RNG%State(3) / Q23
    P23 = A23n * (RNG%State(3) - H * Q23) - H * R23
    H = RNG%State(5) / Q21
    P21 = A21 * (RNG%State(5) - H * Q21) - H * R21
    IF (P23 < 0) P23 = P23 + M2
    IF (P21 < 0) THEN
        P21 = P21 + M2 - P23
    ELSE
        P21 = P21 - P23
    END IF
    IF (P21 < 0) P21 = P21 + M2
    RNG%State(3) = RNG%State(4)
    RNG%State(4) = RNG%State(5)
    RNG%State(5) = P21

    ! Combination
    IF (P12 > P21) THEN
        RandNum = P12 - P21
    ELSE
        RandNum = P12 - P21 + M1
    END IF
    
    RETURN

END FUNCTION Mrg63k3aRNG_NextValue

!******************************************************************************

FUNCTION Mrg63k3aRNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mrg63k3aRNG), INTENT(INOUT)   :: RNG      !! 'Mrg63k3aRNG' object
    tSInt64                             :: RandNum  !! random number
        
!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER  ::MaskL = ToInt64(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandNum = IEOR(SHIFTL(RNG%NextValue(), 32), IAND(RNG%NextValue(), MaskL))
    
    RETURN

END FUNCTION Mrg63k3aRNG_NextLong

!******************************************************************************

FUNCTION Mrg63k3aRNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mrg63k3aRNG), INTENT(IN)  :: RNG      !! 'Mrg63k3aRNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Mrg63k3aRNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Mrg63k3aRNG_GetName

!******************************************************************************

FUNCTION Mrg63k3aRNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mrg63k3aRNG), INTENT(IN)  :: RNG      !! 'Mrg63k3aRNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 6
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Mrg63k3aRNG_GetSeedSize

!******************************************************************************

FUNCTION Mrg63k3aRNG_NextDouble(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random floating-point value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mrg63k3aRNG), INTENT(INOUT)   :: RNG      !! 'Mrg63k3aRNG' object
    tRealDP                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    RandNum = RNG%NextValue()*Norm
    
    RETURN

END FUNCTION Mrg63k3aRNG_NextDouble

!******************************************************************************

FUNCTION Mrg63k3aRNG_NextIntegerLimits(RNG, Bound1, Bound2) RESULT(RandNum)

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
    CLASS(Mrg63k3aRNG), INTENT(INOUT)   :: RNG      !! 'Mrg63k3aRNG' object
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

    ! This works even for an interval [0, 2^31 - 1]. It would not with 
    ! ToInt32(RNG%NextDouble*(Upper - Lower + 1)) + Lower
    RandNum = ToInt32(RNG%NextDouble()*(Diff + 1.0_kDouble)) + Lower

    RETURN

END FUNCTION Mrg63k3aRNG_NextIntegerLimits

!******************************************************************************

END MODULE MClass_Mrg63k3aRNG
    
!******************************************************************************
