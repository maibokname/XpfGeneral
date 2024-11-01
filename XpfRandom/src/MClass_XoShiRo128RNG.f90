
MODULE MClass_XoShiRo128RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *XoShiRo128RNG* type and its related routines.
!   The *XoShiRo128RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *XoShiRo128RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on algorithms from the Xor-Shift-Rotate
!   family of generators by Sebastiano Vigna. <br>
!   The *XoShiRo128RNG* type can represent three PRNG classes: <br>
!   - the XoShiRo128StarStar RNG, which is a fast all-purpose 32-bit generator, <br>
!   - the XoShiRo128Plus  RNG, which is a fast 32-bit generator suitable for
!     real number generation, or <br>
!   - the XoShiRo128PlusPlus RNG, which is a fast all-purpose 32-bit generator. <br>
!   By default, the XoShiRo128StarStar RNG is employed.  However, other XoShiRo128
!   PRNG variants can be utilized by specifying an algorithm flag (between 1 to 3)
!   when initializing the generator.  The *XoShiRo128RNG* type has state size of
!   128 bits. <br>
!   In addition to common operations of a PRNG, the *XoShiRo128RNG* type provides
!   the *Jump* and *LongJump* methods where a large (or very large) number of steps
!   of the output sequence can be advanced in a single operation.  Each method creates
!   (and also returns) a copy of the input PRNG and then advances the state of the
!   specified PRNG.  The PRNG and its copy produce non-overlapping output for the
!   length of the jump intendedly for use in parallel computations. <br>
!   It is important to note that the *XoShiRo128* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://prng.di.unimi.it/">
!       Xoshiro/Xoroshiro Generators and the PRNG shootout.</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/XoShiRo128StarStar.html">
!       Apache Commons RNG: Class XoShiRo128StarStar</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/XoShiRo128Plus.html">
!       Apache Commons RNG: Class XoShiRo128Plus</a> <br>
!   [4] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/XoShiRo128PlusPlus.html">
!       Apache Commons RNG: Class XoShiRo128PlusPlus</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: XoShiRo128RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! Size of the state vector
    tSInt32,  PARAMETER :: SEED_SIZE = 4
    ! The coefficients for the jump function
    tSInt32,  PARAMETER :: JUMP_COEFFICIENTS(0:SEED_SIZE-1) = [ &
        ToInt32(Z'8764000B'), ToInt32(Z'F542D2D3'),         &
        ToInt32(Z'6FA035C3'), ToInt32(Z'77F2DB5B')]
    ! The coefficients for the long jump function
    tSInt32,  PARAMETER :: LONG_JUMP_COEFFICIENTS(0:SEED_SIZE-1) = [ &
        ToInt32(Z'B523952E'), ToInt32(Z'0B6F099F'),              &
        ToInt32(Z'CCF5A0EF'), ToInt32(Z'1C580662')]
    ! Parameters for algorithm flag
    tSInt32,  PARAMETER, PUBLIC :: XoShiRo128StarStar = 1   !! flag for XoShiRo128StarStar PRNG
    tSInt32,  PARAMETER, PUBLIC :: XoShiRo128Plus     = 2   !! flag for XoShiRo128Plus PRNG
    tSInt32,  PARAMETER, PUBLIC :: XoShiRo128PlusPlus = 3   !! flag for XoShiRo128PlusPlus PRNG

!** DERIVED TYPE DEFINITIONS
    !> The *XoShiRo128RNG* type is an *Integer* PRNG type based on algorithms from
    !  the Xor-Shift-Rotate family of generators by Sebastiano Vigna.
    TYPE, EXTENDS(IntegerRNG)  :: XoShiRo128RNG
        PRIVATE
        ! states
        tSInt32     :: State0, State1, State2, State3
        ! algorithm flag
        tSInt32     :: XoShiRoAlgo = XoShiRo128StarStar
        ! function to return the next integer output
        PROCEDURE(Next), POINTER    :: NextOutput => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: InitWithFlag     => XoShiRo128RNG_InitWithFlag
        PROCEDURE, PRIVATE  :: PerformJump      => XoShiRo128RNG_Perform_Jump
        PROCEDURE, PRIVATE  :: MakeACopy        => XoShiRo128RNG_Copy
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => XoShiRo128RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => XoShiRo128RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => XoShiRo128RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => XoShiRo128RNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----               Generic Interface                           -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Initialize <br>
        !  **Purpose**:  To initialize the pseudo-random number generator. <br>
        !  **Usage**: <br>
        !   ! initialize without seed; use XoShiRo128StarStar (default) algorithm <br>
        !   --->    CALL RNG%Initialize() <br>
        !   ! initialize with seed(s); use XoShiRo128StarStar (default) algorithm <br>
        !   --->    CALL RNG%Initialize(Seeds) <br>
        !   ! initialize without seed; use XoShiRo128Plus algorithm <br>
        !   --->    CALL RNG%Initialize(XoShiRo128Plus) <br>
        !   ! initialize with seed(s); use XoShiRo128PlusPlus algorithm <br>
        !   --->    CALL RNG%Initialize(XoShiRo128PlusPlus, Seeds)
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
        PROCEDURE       :: Jump             => XoShiRo128RNG_Jump
        !> **Type-Bound Function**: LongJump <br>
        !  **Purpose**:  To create a copy of the specified PRNG and then retreat
        !                the state of the current PRNG.  The copy is returned as
        !                a new PRNG. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%LongJump() <br>
        !  **Note**: The *LongJump* method is equivalent to calling the *NextInteger*
        !            method repeatedly a very large number of times.
        PROCEDURE       :: LongJump         => XoShiRo128RNG_LongJump
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the PRNG.
        FINAL           :: XoShiRo128RNG_Finalization
        ! ---------------------------------------------------------------------
    END TYPE XoShiRo128RNG

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Next(RNG) RESULT(Output)
            IMPORT
            CLASS(XoShiRo128RNG), INTENT(IN)    :: RNG
            tSInt32                             :: Output
        END FUNCTION Next
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE XoShiRo128RNG_InitWithFlag(RNG, XoShiRoAlgo, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified algorithm flag.  Optionally,
    !  initial seeds can be specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'XoShiRo128RNG' object
    CLASS(XoShiRo128RNG), INTENT(INOUT) :: RNG
    !> algorithm flag <br>
    ! - 1, use XoShiRo128StarStar algorithm. <br>
    ! - 2, use XoShiRo128Plus algorithm. <br>
    ! - 3, use XoShiRo128PlusPlus algorithm.
    tSInt32,              INTENT(IN)    :: XoShiRoAlgo
    !% optional 32-bit integer seed(s)
    tSInt32,  OPTIONAL,   INTENT(IN)    :: Seed(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check and set algorithm flag
    IF (IN_RANGE(XoShiRoAlgo, 1, 3)) THEN
        RNG%XoShiRoAlgo = XoShiRoAlgo
    ELSE
        RNG%XoShiRoAlgo = XoShiRo128StarStar
    END IF

    IF (PRESENT(Seed)) THEN
        ! initialize the generator with seed(s)
        CALL RNG%Initialize(Seed)
    ELSE
        ! initialize the generator without seed
        CALL RNG%Initialize()
    END IF

    RETURN

END SUBROUTINE XoShiRo128RNG_InitWithFlag

!******************************************************************************

SUBROUTINE XoShiRo128RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo128RNG), INTENT(INOUT) :: RNG      !! 'XoShiRo128RNG' object
    tSInt32,              INTENT(IN)    :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Seed0(0:SEED_SIZE-1)

! FLOW

    ! set initial seed
    CALL Fill_State(Seed, Seed0)
    
    ! copy seeds to states
    RNG%State0 = Seed0(0)
    RNG%State1 = Seed0(1)
    RNG%State2 = Seed0(2)
    RNG%State3 = Seed0(3)
    
    ! set pointer to next output function
    SELECT CASE (RNG%XoShiRoAlgo)
    CASE (XoShiRo128StarStar)
        RNG%NextOutput => XoShiRo128StarStar_Next
    CASE (XoShiRo128Plus)
        RNG%NextOutput => XoShiRo128Plus_Next 
    CASE (XoShiRo128PlusPlus)
        RNG%NextOutput => XoShiRo128PlusPlus_Next 
    END SELECT

    RETURN

END SUBROUTINE XoShiRo128RNG_BaseInit

!******************************************************************************

FUNCTION XoShiRo128RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo128RNG), INTENT(INOUT) :: RNG      !! 'XoShiRo128RNG' object
    tSInt32                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: TmpState

! FLOW
    
    ! get next integer output
    RandNum = RNG%NextOutput()

    ! update states
    TmpState   = SHIFTL(RNG%State1, 9)
    RNG%State2 = IEOR(RNG%State2, RNG%State0)
    RNG%State3 = IEOR(RNG%State3, RNG%State1)
    RNG%State1 = IEOR(RNG%State1, RNG%State2)
    RNG%State0 = IEOR(RNG%State0, RNG%State3)
    RNG%State2 = IEOR(RNG%State2, TmpState)
    RNG%State3 = RotateLeft(RNG%State3, 11)

    RETURN

END FUNCTION XoShiRo128RNG_NextInteger

!******************************************************************************

FUNCTION XoShiRo128RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo128RNG), INTENT(IN)    :: RNG  !! 'XoShiRo128RNG' object
    tCharAlloc                          :: Name !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (RNG%XoShiRoAlgo)
    CASE (XoShiRo128StarStar)
        Name = 'XoShiRo128StarStarRNG'
    CASE (XoShiRo128Plus)
        Name = 'XoShiRo128PlusRNG'
    CASE (XoShiRo128PlusPlus)
        Name = 'XoShiRo128PlusPlusRNG'
    END SELECT
    
    RETURN

END FUNCTION XoShiRo128RNG_GetName

!******************************************************************************

FUNCTION XoShiRo128RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo128RNG), INTENT(IN)    :: RNG  !! 'XoShiRo128RNG' object
    tIndex                              :: Size !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 4
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION XoShiRo128RNG_GetSeedSize

!******************************************************************************

FUNCTION XoShiRo128StarStar_Next(RNG) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoShiRo128StarStar generator, which is a fast all-purpose
    !  32-bit generator with memory footprint of 128 bits.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo128RNG), INTENT(IN)    :: RNG      !! 'XoShiRo128RNG' object
    tSInt32                             :: Output   !! 32-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = RotateLeft(RNG%State0 * 5, 7) * 9
    
    RETURN

END FUNCTION XoShiRo128StarStar_Next

!******************************************************************************

FUNCTION XoShiRo128Plus_Next(RNG) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoShiRo128Plus generator, which is a fast 32-bit generator
    !  suitable for floating-point-number generation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo128RNG), INTENT(IN)    :: RNG      !! 'XoShiRo128RNG' object
    tSInt32                             :: Output   !! 32-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = RNG%State0 + RNG%State3
    
    RETURN

END FUNCTION XoShiRo128Plus_Next

!******************************************************************************

FUNCTION XoShiRo128PlusPlus_Next(RNG) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoShiRo128PlusPlus generator, which is a fast all-purpose
    !  32-bit generator with memory footprint of 128 bits.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo128RNG), INTENT(IN)    :: RNG      !! 'XoShiRo128RNG' object
    tSInt32                             :: Output   !! 32-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = RotateLeft(RNG%State0 + RNG%State3, 7) + RNG%State0
    
    RETURN

END FUNCTION XoShiRo128PlusPlus_Next

!******************************************************************************

SUBROUTINE XoShiRo128RNG_Perform_Jump(RNG, JCoef)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the jump to advance the generator state.
    !  Resets the cached state of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo128RNG), INTENT(INOUT) :: RNG          !! 'XoShiRo128RNG' object
    tSInt32,              INTENT(IN)    :: JCoef(0:)    !! jump coefficients

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: S0, S1, S2, S3, I, B, Dummy

! FLOW
    
    S0 = 0
    S1 = 0
    S2 = 0
    S3 = 0
    DO I = 0, SEED_SIZE-1
        DO B = 0, 31
            IF (IAND(JCoef(I), SHIFTL(1, B)) /= 0) THEN
                S0 = IEOR(S0, RNG%State0)
                S1 = IEOR(S1, RNG%State1)
                S2 = IEOR(S2, RNG%State2)
                S3 = IEOR(S3, RNG%State3)
            END IF
            Dummy = RNG%NextInteger()
        END DO
    END DO
    RNG%State0 = S0
    RNG%State1 = S1
    RNG%State2 = S2
    RNG%State3 = S3
    
    RETURN

END SUBROUTINE XoShiRo128RNG_Perform_Jump

!******************************************************************************

FUNCTION XoShiRo128RNG_Copy(Src) RESULT(Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy all components of the source object to the destination object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo128RNG), INTENT(INOUT) :: Src  !! source object
    TYPE(XoShiRo128RNG)                 :: Dst  !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! copy states
    Dst%State0      =  Src%State0
    Dst%State1      =  Src%State1
    Dst%State2      =  Src%State2
    Dst%State3      =  Src%State3
    
    ! copy algorithm flag and pointer
    Dst%XoShiRoAlgo =  Src%XoShiRoAlgo
    Dst%NextOutput  => Src%NextOutput
    
    ! copy initial seeds for re-initialization
    CALL Src%CopySeed(Dst)

    RETURN

END FUNCTION XoShiRo128RNG_Copy

!******************************************************************************

FUNCTION XoShiRo128RNG_Jump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  instance.  The new instance is returned.  <br>
    !  The current state will be advanced in a single operation by the equivalent of
    !  a number of sequential calls to a method that updates the state of the generator.
    !  The jump size is the equivalent of 2<sup>64</sup> calls to the generator's 
    !  NextInteger() method.  It can provide up to 2<sup>64</sup> non-overlapping
    !  subsequences.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo128RNG), INTENT(INOUT) :: RNG      !! 'XoShiRo128RNG' object
    TYPE(XoShiRo128RNG)                 :: NewRNG   !! new 'XoShiRo128RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()
    
    ! advance states of the current instance
    CALL RNG%PerformJump(JUMP_COEFFICIENTS)
    
    RETURN

END FUNCTION XoShiRo128RNG_Jump

!******************************************************************************

FUNCTION XoShiRo128RNG_LongJump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  instance.  The new instance is returned.  <br>
    !  The current state will be advanced in a single operation by the equivalent of
    !  a number of sequential calls to a method that updates the state of the generator.
    !  The jump size is the equivalent of 2<sup>96</sup> calls to the generator's 
    !  NextInteger() method.  It can provide up to 2<sup>32</sup> non-overlapping
    !  subsequences of length 2<sup>96</sup>; each subsequence can provide up to
    !  2<sup>32</sup> non-overlapping subsequences of length 2<sup>64</sup> using
    !  the generator's Jump() method.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo128RNG), INTENT(INOUT) :: RNG      !! 'XoShiRo128RNG' object
    TYPE(XoShiRo128RNG)                 :: NewRNG   !! new 'XoShiRo128RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()
    
    ! advance states of the current instance
    CALL RNG%PerformJump(LONG_JUMP_COEFFICIENTS)
    
    RETURN

END FUNCTION XoShiRo128RNG_LongJump

!******************************************************************************

SUBROUTINE XoShiRo128RNG_Finalization(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the 'XoShiRo128RNG' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(XoShiRo128RNG), INTENT(INOUT)   :: RNG  !! 'XoShiRo128RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    NULLIFY(RNG%NextOutput)
    
    RETURN

END SUBROUTINE XoShiRo128RNG_Finalization

!******************************************************************************

END MODULE MClass_XoShiRo128RNG
    
!******************************************************************************
