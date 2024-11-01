
MODULE MClass_RanLuxRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *RanLuxRNG* type and its related routines.
!   The *RanLuxRNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *RanLuxRNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on the *RANLUX* algorithm
!   by Martin Luscher. <br>
!   Similar to the *RCARRY* algorithm, the *RANLUX* algorithm employs a
!   subtract-with-borrow (SWB) algorithm with a period on the order of
!   10<sup>171</sup>.  However, it throws away some of the numbers generated
!   in order to eliminate correlations.  Therefore, the *RANLUX* algorithm
!   trades execution speed for quality; by choosing a larger luxury setting
!   one gets better random numbers slower. <br>
!   The standard luxury levels provided are: <br>
!   - Level 0  (p=24): equivalent to the original RCARRY of Marsaglia
!     and Zaman, very long period, but fails many tests. <br>
!   - Level 1  (p=48): considerable improvement in quality over level 0,
!     now passes the gap test, but still fails spectral test. <br>
!   - Level 2  (p=97): passes all known tests, but theoretically still
!     defective. <br>
!   - Level 3  (p=223): DEFAULT VALUE.  Any theoretically possible
!     correlations have very small chance of being observed. <br>
!   - Level 4  (p=389): highest possible luxury, all 24 bits chaotic. <br>
!   The luxury level can be specified when initializing the generator. <br>
!   It is important to note that the *RanLux* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.sciencedirect.com/science/article/abs/pii/001046559490233X">
!       F. James. 1994.  RANLUX: A Fortran implementation of the high-quality
!       pseudorandom number generator of Luscher.  Computer Physics Communications,
!       79(1), pp. 111-114. </a> <br>
!   [2] <a href="https://www.sciencedirect.com/science/article/abs/pii/0010465594902321">
!       M. Luscher. 1994.  A portable high-quality random number generator for
!       lattice field theory simulations.  Computer Physics Communications, 79(1),
!       pp. 100-110. </a> <br>
!   [3] <a href="https://projecteuclid.org/journals/annals-of-applied-probability/volume-1/issue-3/A-New-Class-of-Random-Number-Generators/10.1214/aoap/1177005878.full">
!        G. Marsaglia and A. Zaman. 1991. A New Class of Random Number Generators.
!        The Annals of Applied Probability, 1(3), pp. 462-480. </a> <br>
!   [4] <a href="https://wp.csiro.au/alanmiller/random/luxury.f90">
!        Luxury: a Fortran 90 module implementation of the RANLUX generator by Alan Miller. </a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil,   ONLY: MIN_I32, MAX_I32
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: RanLuxRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! maximum luxury level
    tSInt32,  PARAMETER :: MaxLevel = 4
    ! default luxury level
    tSInt32,  PARAMETER :: LuxLevDflt = 3
    ! default seed
    tSInt32,  PARAMETER :: SeedDflt = 314159265
    ! 2**12
    tRealSP,  PARAMETER :: TwoPow12 = 4096.0
    ! 2**24
    tSInt32,  PARAMETER :: ITwoPow24 = 2**24
    ! constant
    tSInt32,  PARAMETER :: ICons = 2147483563
    ! number of additional random numbers that need to be 'thrown away'
    ! every 24 numbers is set using 'LuxLev' variable.
    tSInt32,  PARAMETER :: NdSkip(0:MaxLevel) = [0, 24, 73, 199, 365]
    tSInt32,  PARAMETER :: IGiga = 1000000000


!** DERIVED TYPE DEFINITIONS
    !> The *RanLuxRNG* type is an *Integer* PRNG type based on the *RANLUX*
    !  algorithm by Martin Luscher.
    TYPE, EXTENDS(IntegerRNG)  :: RanLuxRNG
        PRIVATE
        !% indices
        tSInt32     :: I24 = 24
        tSInt32     :: J24 = 10
        !% next 24 inte4ger numbers
        tSInt32     :: Next(24)
        !> luxury level (0-4 or 24-1999) <br>
        !  - Value in the range [0, 4] corresponds to the standard luxury level. <br>
        !  - Value in the range [24, 1999] corresponds to the *p* (skipping) parameter . <br>
        tSInt32     :: LuxLev = LuxLevDflt
        !% how many numbers generate and skip
        tSInt32     :: NSkip
        !% the seed number used to initialize the generator
        tSInt32     :: JSeed
        !% numbers delivered to a user after the skipping
        tSInt32     :: In24 = 0
        !% total generated numbers
        tSInt32     :: Kount = 0
        !% state vector
        tRealSP     :: State(24)
        !% carry
        tSInt32     :: Carry = 0.0
        tSInt32     :: TwoM24, TwoM12
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: InitWithLuxLevel => RanLuxRNG_InitWithLuxLevel
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit             => RanLuxRNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl      => RanLuxRNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName              => RanLuxRNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize          => RanLuxRNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----               Generic Interface                           -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Initialize <br>
        !  **Purpose**:  To initialize the pseudo-random number generator. <br>
        !  **Usage**: <br>
        !   ! initialize without seed; use default luxury level (= 3) <br>
        !   --->    CALL RNG%Initialize() <br>
        !   ! initialize with seed(s); use default luxury level (= 3) <br>
        !   --->    CALL RNG%Initialize(Seeds) <br>
        !   ! initialize without seed; use specified luxury level (= 1) <br>
        !   --->    CALL RNG%Initialize(1) <br>
        !   ! initialize with seed(s); use specified luxury level (= 4) <br>
        !   --->    CALL RNG%Initialize(4, Seeds) <br>
        !  **Usage**: If specified, the luxury level should be between 0 and 4.
        GENERIC         :: Initialize           => InitWithLuxLevel
        ! ---------------------------------------------------------------------
        ! -----                 Overridden Procedures                     -----
        ! ---------------------------------------------------------------------
        !> *Default_NextSingle* is an overridden procedure. <br>
        !  Use the *NextSingle* method in place of the *Default_NextSingle*
        !  method to generate a 32-bit real number.
        PROCEDURE       :: Default_NextSingle   => RanLuxRNG_NextSingle
        ! ---------------------------------------------------------------------
    END TYPE RanLuxRNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE RanLuxRNG_InitWithLuxLevel(RNG, LuxLevel, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified luxury level.  Optionally,
    !  initial Seeds can be specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'RanLuxRNG' object
    CLASS(RanLuxRNG),   INTENT(INOUT)   :: RNG
    !> luxury level (0-4 or 24-1999) <br>
    !  - Value in the range [0, 4] corresponds to the standard luxury level. <br>
    !  - Value in the range [24, 1999] corresponds to the *p* (skipping) parameter . <br>
    !  - Value out of valid ranges is set to a valid value.
    tSInt32,            INTENT(IN)      :: LuxLevel
    !% optional 32-bit integer seed(s)
    tSInt32,  OPTIONAL, INTENT(IN)      :: Seed(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: ILux

! FLOW

    ! check and set luxury level
    IF (LuxLevel < 0) THEN
        RNG%LuxLev = LuxLevDflt
    ELSEIF (LuxLevel <= MaxLevel) THEN
        RNG%LuxLev = LuxLevel
    ELSEIF ((LuxLevel < 24).OR.(LuxLevel > 2000)) THEN
        RNG%LuxLev = MaxLevel
    ELSE
        RNG%LuxLev = LuxLevel
        DO ILux = 0, MaxLevel
            IF (LuxLevel == NdSkip(ILux)+24) RNG%LuxLev = ILux
        END DO
    END IF

    IF (PRESENT(Seed)) THEN
        ! initialize the generator with seed(s)
        CALL RNG%Initialize(Seed)
    ELSE
        ! initialize the generator without seed
        CALL RNG%Initialize()
    END IF

    RETURN

END SUBROUTINE RanLuxRNG_InitWithLuxLevel

!******************************************************************************

SUBROUTINE RanLuxRNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'RanLuxRNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RanLuxRNG), INTENT(INOUT) :: RNG      !! 'RanLuxRNG' object
    tSInt32,          INTENT(IN)    :: Seed(:)  !! seed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I, ISeeds(24), K

! FLOW

    ! set JSeed
    IF (Seed(1) <= 0)  THEN
        RNG%JSeed = SeedDflt
    ELSE
        RNG%JSeed = Seed(1)
    END IF
    
    ! set NSkip
    IF (RNG%LuxLev <= MaxLevel) THEN
        RNG%NSkip = NdSkip(RNG%LuxLev)
    ELSE
        RNG%NSkip = RNG%LuxLev - 24
    END IF

    ! set other working variables
    RNG%In24 = 0
    RNG%Kount = 0

    RNG%TwoM24 = 1.0
    DO I = 1, 24
        RNG%TwoM24 = RNG%TwoM24 * 0.5
        K = RNG%JSeed / 53668
        RNG%JSeed = 40014 * (RNG%JSeed-K*53668) - K * 12211
        IF (RNG%JSeed < 0) RNG%JSeed = RNG%JSeed + ICons
        ISeeds(I) = MOD(RNG%JSeed, ITwoPow24)
    END DO
    RNG%TwoM12 = RNG%TwoM24 * 4096.0
    DO I = 1, 24
        RNG%State(I) = REAL(ISeeds(I)) * RNG%TwoM24
        RNG%Next(I) = I - 1
    END DO
    RNG%Next(1) = 24
    RNG%I24 = 24
    RNG%J24 = 10
    RNG%Carry = 0.0
    IF (RNG%State(24) == 0.0) RNG%Carry = RNG%TwoM24

    RETURN

END SUBROUTINE RanLuxRNG_BaseInit

!******************************************************************************

FUNCTION RanLuxRNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RanLuxRNG), INTENT(INOUT) :: RNG      !! 'RanLuxRNG' object
    tSInt32                         :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tRealSP, PARAMETER  :: Diff = ToRealSP(MAX_I32) - ToRealSP(MIN_I32)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandNum = ToInt32(RNG%NextSingle()*Diff) + MIN_I32

    RETURN

END FUNCTION RanLuxRNG_NextInteger

!******************************************************************************

FUNCTION RanLuxRNG_NextSingle(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random real value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RanLuxRNG), INTENT(INOUT) :: RNG      !! 'RanLuxRNG' object
    tRealSP                         :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: ISkip
    tRealSP     :: Uni

! FLOW

    ! subtract-with-borrow generator
    Uni = RNG%State(RNG%J24) - RNG%State(RNG%I24) - RNG%Carry
    IF (Uni < 0.0) THEN
        Uni = Uni + 1.0
        RNG%Carry = RNG%TwoM24
    ELSE
        RNG%Carry = 0.0
    END IF
    RNG%State(RNG%I24) = Uni
    RNG%I24 = RNG%Next(RNG%I24)
    RNG%J24 = RNG%Next(RNG%J24)
    RandNum = Uni
    
    ! small numbers (with less than 12 "significant" bits) are "padded".
    IF (Uni < RNG%TwoM12) RandNum = RandNum + RNG%TwoM24 * RNG%State(RNG%J24)
    
    ! zero is forbidden in case someone takes a logarithm
    IF (RandNum == 0.0) RandNum = RNG%TwoM24 * RNG%TwoM24
    
    ! Skipping to luxury.  As proposed by Martin Luscher.
    RNG%In24 = RNG%In24 + 1
    IF (RNG%In24 == 24) THEN
        RNG%In24 = 0
        RNG%Kount = RNG%Kount + RNG%NSkip
        DO ISkip = 1, RNG%NSkip
            Uni = RNG%State(RNG%J24) - RNG%State(RNG%I24) - RNG%Carry
            IF (Uni < 0.0) THEN
                Uni = Uni + 1.0
                RNG%Carry = RNG%TwoM24
            ELSE
                RNG%Carry = 0.0
            END IF
            RNG%State(RNG%I24) = Uni
            RNG%I24 = RNG%Next(RNG%I24)
            RNG%J24 = RNG%Next(RNG%J24)
        END DO
    END IF

    RNG%Kount = RNG%Kount + 1
    IF (RNG%Kount >= IGiga) THEN
        RNG%Kount = RNG%Kount - IGiga
    END IF

    RETURN

END FUNCTION RanLuxRNG_NextSingle

!******************************************************************************

FUNCTION RanLuxRNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RanLuxRNG), INTENT(IN)    :: RNG      !! 'RanLuxRNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'RanLuxRNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION RanLuxRNG_GetName

!******************************************************************************

FUNCTION RanLuxRNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(RanLuxRNG), INTENT(IN)    :: RNG      !! 'RanLuxRNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 1
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION RanLuxRNG_GetSeedSize

!******************************************************************************

END MODULE MClass_RanLuxRNG
    
!******************************************************************************
