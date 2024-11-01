
MODULE MClass_XoRoShiRo128RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *XoRoShiRo128RNG* type and its related routines.
!   The *XoRoShiRo128RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *XoRoShiRo128RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on algorithms from the Xor-Shift-Rotate
!   family of generators by Sebastiano Vigna. <br>
!   The *XoRoShiRo128RNG* type can represent three PRNG classes: <br>
!   - the XoRoShiRo128StarStar RNG, which is a fast all-purpose 64-bit generator, <br>
!   - the XoRoShiRo128Plus  RNG, which is a fast 64-bit generator suitable for
!     real number generation, or <br>
!   - the XoRoShiRo128PlusPlus RNG, which is a fast all-purpose 64-bit generator. <br>
!   By default, the XoRoShiRo128StarStar RNG is employed.  However, other XoRoShiRo128
!   PRNG variants can be utilized by specifying an algorithm flag (between 1 to 3)
!   when initializing the generator.  The *XoRoShiRo128RNG* type has state size of
!   128 bits and period of 2<sup>128</sup>-1. <br>
!   In addition to common operations of a PRNG, the *XoRoShiRo128RNG* type provides
!   the *Jump* and *LongJump* methods where a large (or very large) number of steps
!   of the output sequence can be advanced in a single operation.  Each method creates
!   (and also returns) a copy of the input PRNG and then advances the state of the
!   specified PRNG.  The PRNG and its copy produce non-overlapping output for the
!   length of the jump intendedly for use in parallel computations. <br>
!   It is important to note that the *XoRoShiRo128* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://prng.di.unimi.it/">
!       Xoshiro/Xoroshiro Generators and the PRNG shootout.</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/XoRoShiRo128StarStar.html">
!       Apache Commons RNG: Class XoRoShiRo128StarStar</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/XoRoShiRo128Plus.html">
!       Apache Commons RNG: Class XoRoShiRo128Plus</a> <br>
!   [4] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/XoRoShiRo128PlusPlus.html">
!       Apache Commons RNG: Class XoRoShiRo128PlusPlus</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: XoRoShiRo128RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! Size of the state vector
    tSInt32,  PARAMETER :: SEED_SIZE = 2
    ! The coefficients for the jump function
    tSInt64,  PARAMETER :: JUMP_COEFFICIENTS(0:SEED_SIZE-1) = [ &
        ToInt64(Z'DF900294D8F554A5'), ToInt64(Z'170865DF4B3201FC')]
    ! The coefficients for the long jump function
    tSInt64,  PARAMETER :: LONG_JUMP_COEFFICIENTS(0:SEED_SIZE-1) = [ &
        ToInt64(Z'D2A98B26625EEE7B'), ToInt64(Z'DDDF9B1090AA7AC1')]
    ! The coefficients for the jump function for XoRoShiRo128PlusPlus
    tSInt64,  PARAMETER :: JUMP_COEF_PLUSPLUS(0:SEED_SIZE-1) = [ &
        ToInt64(Z'2BD7A6A6E99C2DDC'), ToInt64(Z'0992CCAF6A6FCA05')]
    ! The coefficients for the long jump function for XoRoShiRo128PlusPlus
    tSInt64,  PARAMETER :: LONG_JUMP_COEF_PLUSPLUS(0:SEED_SIZE-1) = [ &
        ToInt64(Z'360FD5F2CF8D5D99'), ToInt64(Z'9C6E6877736C46E3')]
    ! Parameters for algorithm flag
    tSInt32,  PARAMETER, PUBLIC :: XoRoShiRo128StarStar = 1 !! flag for XoRoShiRo128StarStar PRNG
    tSInt32,  PARAMETER, PUBLIC :: XoRoShiRo128Plus     = 2 !! flag for XoRoShiRo128Plus PRNG
    tSInt32,  PARAMETER, PUBLIC :: XoRoShiRo128PlusPlus = 3 !! flag for XoRoShiRo128PlusPlus PRNG

!** DERIVED TYPE DEFINITIONS
    !> The *XoRoShiRo128RNG* type is a *Long* PRNG type based on algorithms from
    !  the Xor-Shift-Rotate family of generators by Sebastiano Vigna.
    TYPE, EXTENDS(LongRNG)  :: XoRoShiRo128RNG
        PRIVATE
        ! states
        tSInt64     :: State0, State1
        ! algorithm flag
        tSInt32     :: XoRoShiRoAlgo = XoRoShiRo128StarStar
        ! function to return the next integer output
        PROCEDURE(Next),   POINTER  :: NextOutput  => NULL()
        PROCEDURE(Update), POINTER  :: UpdateState => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: InitWithFlag     => XoRoShiRo128RNG_InitWithFlag
        PROCEDURE, PRIVATE  :: PerformJump      => XoRoShiRo128RNG_Perform_Jump
        PROCEDURE, PRIVATE  :: MakeACopy        => XoRoShiRo128RNG_Copy
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => XoRoShiRo128RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextLongImpl  => XoRoShiRo128RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => XoRoShiRo128RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => XoRoShiRo128RNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----               Generic Interface                           -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Initialize <br>
        !  **Purpose**:  To initialize the pseudo-random number generator. <br>
        !  **Usage**: <br>
        !   ! initialize without seed; use XoRoShiRo128StarStar (default) algorithm <br>
        !   --->    CALL RNG%Initialize() <br>
        !   ! initialize with seed(s); use XoRoShiRo128StarStar (default) algorithm <br>
        !   --->    CALL RNG%Initialize(Seeds) <br>
        !   ! initialize without seed; use XoRoShiRo128Plus algorithm <br>
        !   --->    CALL RNG%Initialize(XoRoShiRo128Plus) <br>
        !   ! initialize with seed(s); use XoRoShiRo128PlusPlus algorithm <br>
        !   --->    CALL RNG%Initialize(XoRoShiRo128PlusPlus, Seeds)
        GENERIC         :: Initialize       => InitWithFlag
        ! ---------------------------------------------------------------------
        ! -----                 Public Procedures                         -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Jump <br>
        !  **Purpose**:  To create a copy of the specified PRNG and then retreat
        !                the state of the current PRNG.  The copy is returned as
        !                a new PRNG. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%Jump() <br>
        !  **Note**: The *Jump* method is equivalent to calling the *NextInteger*
        !            method repeatedly a large number of times.
        PROCEDURE       :: Jump             => XoRoShiRo128RNG_Jump
        !> **Type-Bound Function**: LongJump <br>
        !  **Purpose**:  To create a copy of the specified PRNG and then retreat
        !                the state of the current PRNG.  The copy is returned as
        !                a new PRNG. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%LongJump() <br>
        !  **Note**: The *LongJump* method is equivalent to calling the *NextInteger*
        !            method repeatedly a very large number of times.
        PROCEDURE       :: LongJump         => XoRoShiRo128RNG_LongJump
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the PRNG.
        FINAL           :: XoRoShiRo128RNG_Finalization
        ! ---------------------------------------------------------------------
    END TYPE XoRoShiRo128RNG

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Next(RNG, S0, S1) RESULT(Output)
            IMPORT
            CLASS(XoRoShiRo128RNG), INTENT(IN)  :: RNG
            tSInt64,                INTENT(IN)  :: S0, S1
            tSInt64                             :: Output
        END FUNCTION Next
        SUBROUTINE Update(RNG, S0, S1)
            IMPORT
            CLASS(XoRoShiRo128RNG), INTENT(INOUT)   :: RNG
            tSInt64,                INTENT(IN)      :: S0, S1
        END SUBROUTINE Update
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE XoRoShiRo128RNG_InitWithFlag(RNG, XoRoShiRoAlgo, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified algorithm flag.  Optionally,
    !  initial seeds can be specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'XoRoShiRo128RNG' object
    CLASS(XoRoShiRo128RNG), INTENT(INOUT)   :: RNG
    !> algorithm flag <br>
    ! - 1, use XoRoShiRo128StarStar algorithm. <br>
    ! - 2, use XoRoShiRo128Plus algorithm. <br>
    ! - 3, use XoRoShiRo128PlusPlus algorithm.
    tSInt32,                INTENT(IN)      :: XoRoShiRoAlgo
    !% optional 32-bit integer seed(s)
    tSInt64, OPTIONAL,      INTENT(IN)      :: Seed(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check and set algorithm flag
    IF (IN_RANGE(XoRoShiRoAlgo, 1, 3)) THEN
        RNG%XoRoShiRoAlgo = XoRoShiRoAlgo
    ELSE
        RNG%XoRoShiRoAlgo = XoRoShiRo128StarStar
    END IF

    IF (PRESENT(Seed)) THEN
        ! initialize the generator with seed(s)
        CALL RNG%Initialize(Seed)
    ELSE
        ! initialize the generator without seed
        CALL RNG%Initialize()
    END IF

    RETURN

END SUBROUTINE XoRoShiRo128RNG_InitWithFlag

!******************************************************************************

SUBROUTINE XoRoShiRo128RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo128RNG), INTENT(INOUT)   :: RNG      !! 'XoRoShiRo128RNG' object
    tSInt64,                INTENT(IN)      :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: Seed0(0:SEED_SIZE-1)

! FLOW

    ! set initial seed
    CALL Fill_State(Seed, Seed0)

    ! copy seeds to states
    RNG%State0 = Seed0(0)
    RNG%State1 = Seed0(1)

    ! set pointer to next output function
    SELECT CASE (RNG%XoRoShiRoAlgo)
    CASE (XoRoShiRo128StarStar)
        RNG%NextOutput  => XoRoShiRo128StarStar_Next
        RNG%UpdateState => XoRoShiRo128_Update
    CASE (XoRoShiRo128Plus)
        RNG%NextOutput  => XoRoShiRo128Plus_Next
        RNG%UpdateState => XoRoShiRo128_Update
    CASE (XoRoShiRo128PlusPlus)
        RNG%NextOutput  => XoRoShiRo128PlusPlus_Next
        RNG%UpdateState => XoRoShiRo128PlusPlus_Update
    END SELECT

    RETURN

END SUBROUTINE XoRoShiRo128RNG_BaseInit

!******************************************************************************

FUNCTION XoRoShiRo128RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo128RNG), INTENT(INOUT)   :: RNG      !! 'XoRoShiRo128RNG' object
    tSInt64                                 :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: S0, S1

! FLOW

    ! get current states
    S0 = RNG%State0
    S1 = RNG%State1

    ! update states
    CALL RNG%UpdateState(S0, S1)

    ! get next integer output
    RandNum = RNG%NextOutput(S0, S1)

    RETURN

END FUNCTION XoRoShiRo128RNG_NextLong

!******************************************************************************

FUNCTION XoRoShiRo128RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo128RNG), INTENT(IN)  :: RNG      !! 'XoRoShiRo128RNG' object
    tCharAlloc                          :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT CASE (RNG%XoRoShiRoAlgo)
    CASE (XoRoShiRo128StarStar)
        Name = 'XoRoShiRo128StarStarRNG'
    CASE (XoRoShiRo128Plus)
        Name = 'XoRoShiRo128PlusRNG'
    CASE (XoRoShiRo128PlusPlus)
        Name = 'XoRoShiRo128PlusPlusRNG'
    END SELECT

    RETURN

END FUNCTION XoRoShiRo128RNG_GetName

!******************************************************************************

FUNCTION XoRoShiRo128RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo128RNG), INTENT(IN)  :: RNG  !! 'XoRoShiRo128RNG' object
    tIndex                              :: Size !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = SEED_SIZE
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION XoRoShiRo128RNG_GetSeedSize

!******************************************************************************

FUNCTION XoRoShiRo128StarStar_Next(RNG, S0, S1) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoRoShiRo128StarStar generator, which is a fast all-purpose
    !  64-bit generator with memory footprint of 128 bits and the period
    !  of 2**128-1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo128RNG), INTENT(IN)  :: RNG      !! 'XoRoShiRo128RNG' object
    tSInt64,                INTENT(IN)  :: S0, S1
    tSInt64                             :: Output   !! 64-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Output = RotateLeft(S0 * 5, 7) * 9
    ASSOCIATE (Dummy => RNG, Dummy2 => S1); END ASSOCIATE

    RETURN

END FUNCTION XoRoShiRo128StarStar_Next

!******************************************************************************

FUNCTION XoRoShiRo128Plus_Next(RNG, S0, S1) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoRoShiRo128Plus generator, which is a fast 64-bit generator
    !  suitable for floating-point-number generation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo128RNG), INTENT(IN)  :: RNG      !! 'XoRoShiRo128RNG' object
    tSInt64,                INTENT(IN)  :: S0, S1
    tSInt64                             :: Output   !! 64-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Output = S0 + S1
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION XoRoShiRo128Plus_Next

!******************************************************************************

FUNCTION XoRoShiRo128PlusPlus_Next(RNG, S0, S1) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoRoShiRo128PlusPlus generator, which is a fast all-purpose
    !  64-bit generator with memory footprint of 128 bits.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo128RNG), INTENT(IN)  :: RNG      !! 'XoRoShiRo128RNG' object
    tSInt64,                INTENT(IN)  :: S0, S1
    tSInt64                             :: Output   !! 64-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Output = RotateLeft(S0 + S1, 17) + S0
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION XoRoShiRo128PlusPlus_Next

!******************************************************************************

SUBROUTINE XoRoShiRo128_Update(RNG, S0, S1)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To update the states of the XoRoShiRo128Plus and XoRoShiRo128StarStar generators.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo128RNG), INTENT(INOUT)   :: RNG      !! 'XoRoShiRo128RNG' object
    tSInt64,                INTENT(IN)      :: S0, S1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: S11

! FLOW

    S11 = IEOR(S1, S0)
    RNG%State0 = IEOR(IEOR(RotateLeft(S0, 24), S11), SHIFTL(S11, 16))   ! a, b
    RNG%State1 = RotateLeft(S11, 37)                                    ! c

    RETURN

END SUBROUTINE XoRoShiRo128_Update

!******************************************************************************

SUBROUTINE XoRoShiRo128PlusPlus_Update(RNG, S0, S1)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To update the states of the XoRoShiRo128PlusPlus generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo128RNG), INTENT(INOUT)   :: RNG      !! 'XoRoShiRo128RNG' object
    tSInt64,                INTENT(IN)      :: S0, S1

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: S11

! FLOW

    S11 = IEOR(S1, S0)
    RNG%State0 = IEOR(IEOR(RotateLeft(S0, 49), S11), SHIFTL(S11, 21))   ! a, b
    RNG%State1 = RotateLeft(S11, 28)                                    ! c

    RETURN

END SUBROUTINE XoRoShiRo128PlusPlus_Update

!******************************************************************************

SUBROUTINE XoRoShiRo128RNG_Perform_Jump(RNG, JCoef)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the jump to advance the generator state.
    !  Resets the cached state of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo128RNG), INTENT(INOUT)   :: RNG          !! 'XoRoShiRo128RNG' object
    tSInt64,                INTENT(IN)      :: JCoef(0:)    !! jump coefficients

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: S0, S1, Dummy
    tSInt32     :: I, B

! FLOW

    S0 = 0_kInt64
    S1 = 0_kInt64
    DO I = 0, SEED_SIZE-1
        DO B = 0, 63
            IF (IAND(JCoef(I), SHIFTL(1_kInt64, B)) /= 0_kInt64) THEN
                S0 = IEOR(S0, RNG%State0)
                S1 = IEOR(S1, RNG%State1)
            END IF
            Dummy = RNG%NextLong()
        END DO
    END DO
    RNG%State0 = S0
    RNG%State1 = S1

    RETURN

END SUBROUTINE XoRoShiRo128RNG_Perform_Jump

!******************************************************************************

FUNCTION XoRoShiRo128RNG_Copy(Src) RESULT(Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy all components of the source object to the destination object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo128RNG), INTENT(INOUT)   :: Src  !! source object
    TYPE(XoRoShiRo128RNG)                   :: Dst  !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! copy components
    Dst%State0        =  Src%State0
    Dst%State1        =  Src%State1
    Dst%XoRoShiRoAlgo =  Src%XoRoShiRoAlgo
    Dst%NextOutput    => Src%NextOutput
    Dst%UpdateState   => Src%UpdateState
    CALL Src%CopySeed(Dst)

    RETURN

END FUNCTION XoRoShiRo128RNG_Copy

!******************************************************************************

FUNCTION XoRoShiRo128RNG_Jump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  instance.  The new instance is returned.  <br>
    !  The current state will be advanced in a single operation by the equivalent
    !  of a number of sequential calls to a method that updates the state of the
    !  generator.  The jump size is the equivalent of 2<sup>64</sup> calls to the
    !  generator's NextLong() method.  It can provide up to 2<sup>64</sup> 
    !  non-overlapping subsequences.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo128RNG), INTENT(INOUT)   :: RNG      !! 'XoRoShiRo128RNG' object
    TYPE(XoRoShiRo128RNG)                   :: NewRNG   !! new 'XoRoShiRo128RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()

    ! advance states of the current instance
    IF (RNG%XoRoShiRoAlgo == XoRoShiRo128PlusPlus) THEN
        CALL RNG%PerformJump(JUMP_COEF_PLUSPLUS)
    ELSE
        CALL RNG%PerformJump(JUMP_COEFFICIENTS)
    END IF

    RETURN

END FUNCTION XoRoShiRo128RNG_Jump

!******************************************************************************

FUNCTION XoRoShiRo128RNG_LongJump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  instance.  The new instance is returned.  <br>
    !  The current state will be advanced in a single operation by the equivalent of
    !  a number of sequential calls to a method that updates the state of the generator.
    !  The jump size is the equivalent of 2<sup>96</sup> calls to the generator's
    !  NextLong() method.  It can provide up to 2<sup>32</sup> non-overlapping
    !  subsequences of length 2<sup>96</sup>; each subsequence can provide up to
    !  2<sup>32</sup> non-overlapping subsequences of length 2<sup>64</sup>
    !  using the generator's Jump() method.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo128RNG), INTENT(INOUT)   :: RNG      !! 'XoRoShiRo128RNG' object
    TYPE(XoRoShiRo128RNG)                   :: NewRNG   !! new 'XoRoShiRo128RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()

    ! advance states of the current instance
    IF (RNG%XoRoShiRoAlgo == XoRoShiRo128PlusPlus) THEN
        CALL RNG%PerformJump(LONG_JUMP_COEF_PLUSPLUS)
    ELSE
        CALL RNG%PerformJump(LONG_JUMP_COEFFICIENTS)
    END IF

    RETURN

END FUNCTION XoRoShiRo128RNG_LongJump

!******************************************************************************

SUBROUTINE XoRoShiRo128RNG_Finalization(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the 'XoRoShiRo128RNG' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(XoRoShiRo128RNG), INTENT(INOUT)   :: RNG  !! 'XoRoShiRo128RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    NULLIFY(RNG%NextOutput)
    NULLIFY(RNG%UpdateState)
    
    RETURN

END SUBROUTINE XoRoShiRo128RNG_Finalization

!******************************************************************************

END MODULE MClass_XoRoShiRo128RNG

!******************************************************************************
