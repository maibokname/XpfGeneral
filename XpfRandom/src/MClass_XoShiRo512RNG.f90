
MODULE MClass_XoShiRo512RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *XoShiRo512RNG* type and its related routines.
!   The *XoShiRo512RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *XoShiRo512RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on algorithms from the Xor-Shift-Rotate
!   family of generators by Sebastiano Vigna. <br>
!   The *XoShiRo512RNG* type can represent three PRNG classes: <br>
!   - the XoShiRo512StarStar RNG, which is a fast all-purpose 64-bit generator, <br>
!   - the XoShiRo512Plus  RNG, which is a fast 64-bit generator suitable for
!     real number generation, or <br>
!   - the XoShiRo512PlusPlus RNG, which is a fast all-purpose 64-bit generator. <br>
!   By default, the XoShiRo512StarStar RNG is employed.  However, other XoShiRo512
!   PRNG variants can be utilized by specifying an algorithm flag (between 1 to 3)
!   when initializing the generator.  The *XoShiRo512RNG* type has state size of
!   512 bits and period of 2<sup>512</sup>-1. <br>
!   In addition to common operations of a PRNG, the *XoShiRo512RNG* type provides
!   the *Jump* and *LongJump* methods where a large (or very large) number of steps
!   of the output sequence can be advanced in a single operation.  Each method creates
!   (and also returns) a copy of the input PRNG and then advances the state of the
!   specified PRNG.  The PRNG and its copy produce non-overlapping output for the
!   length of the jump intendedly for use in parallel computations. <br>
!   It is important to note that the *XoShiRo512* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://prng.di.unimi.it/">
!       Xoshiro/Xoroshiro Generators and the PRNG shootout.</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/XoShiRo512StarStar.html">
!       Apache Commons RNG: Class XoShiRo512StarStar</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/XoShiRo512Plus.html">
!       Apache Commons RNG: Class XoShiRo512Plus</a> <br>
!   [4] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/XoShiRo512PlusPlus.html">
!       Apache Commons RNG: Class XoShiRo512PlusPlus</a>

!** PURPOSE OF THIS MODULE:
    ! This module contains a random number generator class based on the
    ! Xor-Shift-Rotate family of 64-bit generators with 512-bits of state.

!** REFERENCES:
    ! [1] XorShiRo / XoRoShiro Generators.  http://xoshiro.di.unimi.it

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: XoShiRo512RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! Size of the state vector
    tSInt32,  PARAMETER :: SEED_SIZE = 8
    ! The coefficients for the jump function
    tSInt64,  PARAMETER :: JUMP_COEFFICIENTS(0:SEED_SIZE-1) = [   &
        ToInt64(Z'33ED89B6E7A353F9'), ToInt64(Z'760083D7955323BE'), &
        ToInt64(Z'2837F2FBB5F22FAE'), ToInt64(Z'4B8C5674D309511C'), &
        ToInt64(Z'B11AC47A7BA28C25'), ToInt64(Z'F1BE7667092BCC1C'), &
        ToInt64(Z'53851EFDB6DF0AAF'), ToInt64(Z'1EBBC8B23EAF25DB')]
    ! The coefficients for the long jump function
    tSInt64,  PARAMETER :: LONG_JUMP_COEFFICIENTS(0:SEED_SIZE-1) = [ &
        ToInt64(Z'11467FEF8F921D28'), ToInt64(Z'A2A819F2E79C8EA8'),    &
        ToInt64(Z'A8299FC284B3959A'), ToInt64(Z'B4D347340CA63EE1'),    &
        ToInt64(Z'1CB0940BEDBFF6CE'), ToInt64(Z'D956C5C4FA1F8E17'),    &
        ToInt64(Z'915E38FD4EDA93BC'), ToInt64(Z'5B3CCDFA5D7DACA5')]
    ! Parameters for algorithm flag
    tSInt32,  PARAMETER, PUBLIC :: XoShiRo512StarStar = 1 !! flag for XoShiRo512StarStar PRNG
    tSInt32,  PARAMETER, PUBLIC :: XoShiRo512Plus     = 2 !! flag for XoShiRo512Plus PRNG
    tSInt32,  PARAMETER, PUBLIC :: XoShiRo512PlusPlus = 3 !! flag for XoShiRo512PlusPlus PRNG

!** DERIVED TYPE DEFINITIONS
    !> The *XoShiRo512RNGRNG* type is a *Long* PRNG type based on algorithms from
    !  the Xor-Shift-Rotate family of generators by Sebastiano Vigna.
    TYPE, EXTENDS(LongRNG)  :: XoShiRo512RNG
        PRIVATE
        ! states
        tSInt64     :: State(0:SEED_SIZE-1)
        ! algorithm flag
        tSInt32     :: XoShiRoAlgo = XoShiRo512StarStar
        ! function to return the next integer output
        PROCEDURE(Next), POINTER    :: NextOutput => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: InitWithFlag     => XoShiRo512RNG_InitWithFlag
        PROCEDURE, PRIVATE  :: PerformJump      => XoShiRo512RNG_Perform_Jump
        PROCEDURE, PRIVATE  :: MakeACopy        => XoShiRo512RNG_Copy
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => XoShiRo512RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextLongImpl  => XoShiRo512RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => XoShiRo512RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => XoShiRo512RNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----               Generic Interface                           -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Initialize <br>
        !  **Purpose**:  To initialize the pseudo-random number generator. <br>
        !  **Usage**: <br>
        !   ! initialize without seed; use XoShiRo512StarStar (default) algorithm <br>
        !   --->    CALL RNG%Initialize() <br>
        !   ! initialize with seed(s); use XoShiRo512StarStar (default) algorithm <br>
        !   --->    CALL RNG%Initialize(Seeds) <br>
        !   ! initialize without seed; use XoShiRo512Plus algorithm <br>
        !   --->    CALL RNG%Initialize(XoShiRo512Plus) <br>
        !   ! initialize with seed(s); use XoShiRo512PlusPlus algorithm <br>
        !   --->    CALL RNG%Initialize(XoShiRo512PlusPlus, Seeds)
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
        PROCEDURE       :: Jump             => XoShiRo512RNG_Jump
        !> **Type-Bound Function**: LongJump <br>
        !  **Purpose**:  To create a copy of the specified PRNG and then retreat
        !                the state of the current PRNG.  The copy is returned as
        !                a new PRNG. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%LongJump() <br>
        !  **Note**: The *LongJump* method is equivalent to calling the *NextInteger*
        !            method repeatedly a very large number of times.
        PROCEDURE       :: LongJump         => XoShiRo512RNG_LongJump
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the PRNG.
        FINAL           :: XoShiRo512RNG_Finalization
        ! ---------------------------------------------------------------------
    END TYPE XoShiRo512RNG

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Next(RNG) RESULT(Output)
            IMPORT
            CLASS(XoShiRo512RNG), INTENT(IN)    :: RNG
            tSInt64                             :: Output
        END FUNCTION Next
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE XoShiRo512RNG_InitWithFlag(RNG, XoShiRoAlgo, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified algorithm flag.  Optionally,
    !  initial seeds can be specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'XoShiRo512RNG' object
    CLASS(XoShiRo512RNG), INTENT(INOUT) :: RNG
    !> algorithm flag <br>
    ! - 1, use XoShiRo512StarStar algorithm. <br>
    ! - 2, use XoShiRo512Plus algorithm. <br>
    ! - 3, use XoShiRo512PlusPlus algorithm.
    tSInt32,              INTENT(IN)    :: XoShiRoAlgo
    !% optional 32-bit integer seed(s)
    tSInt64, OPTIONAL,    INTENT(IN)    :: Seed(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check and set algorithm flag
    IF (IN_RANGE(XoShiRoAlgo, 1, 3)) THEN
        RNG%XoShiRoAlgo = XoShiRoAlgo
    ELSE
        RNG%XoShiRoAlgo = XoShiRo512StarStar
    END IF

    IF (PRESENT(Seed)) THEN
        ! initialize the generator with seed(s)
        CALL RNG%Initialize(Seed)
    ELSE
        ! initialize the generator without seed
        CALL RNG%Initialize()
    END IF

    RETURN

END SUBROUTINE XoShiRo512RNG_InitWithFlag

!******************************************************************************

SUBROUTINE XoShiRo512RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    ! To initialize the 'XoShiRo512RNG' object with optional initial seeds.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo512RNG), INTENT(INOUT) :: RNG          !! 'XoShiRo512RNG' object
    tSInt64,              INTENT(IN)    :: Seed(:)      !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: Seed0(0:SEED_SIZE-1)

! FLOW

    ! set initial seed
    CALL Fill_State(Seed, Seed0)
    
    ! copy seeds to states
    RNG%State = Seed0

    ! set pointer to next output function
    SELECT CASE (RNG%XoShiRoAlgo)
    CASE (XoShiRo512StarStar)
        RNG%NextOutput => XoShiRo512StarStar_Next
    CASE (XoShiRo512Plus)
        RNG%NextOutput => XoShiRo512Plus_Next 
    CASE (XoShiRo512PlusPlus)
        RNG%NextOutput => XoShiRo512PlusPlus_Next 
    END SELECT

    RETURN

END SUBROUTINE XoShiRo512RNG_BaseInit

!******************************************************************************

FUNCTION XoShiRo512RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo512RNG), INTENT(INOUT) :: RNG      !! 'XoShiRo512RNG' object
    tSInt64                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: TmpState

! FLOW
    
    ! get next integer output
    RandNum = RNG%NextOutput()

    ! update states
    TmpState   = SHIFTL(RNG%State(1), 11)
    
    RNG%State(2) = IEOR(RNG%State(2), RNG%State(0))
    RNG%State(5) = IEOR(RNG%State(5), RNG%State(1))
    RNG%State(1) = IEOR(RNG%State(1), RNG%State(2))
    RNG%State(7) = IEOR(RNG%State(7), RNG%State(3))
    RNG%State(3) = IEOR(RNG%State(3), RNG%State(4))
    RNG%State(4) = IEOR(RNG%State(4), RNG%State(5))
    RNG%State(0) = IEOR(RNG%State(0), RNG%State(6))
    RNG%State(6) = IEOR(RNG%State(6), RNG%State(7))
    
    RNG%State(6) = IEOR(RNG%State(6), TmpState)
    RNG%State(7) = RotateLeft(RNG%State(7), 21)
    
    RETURN

END FUNCTION XoShiRo512RNG_NextLong

!******************************************************************************

FUNCTION XoShiRo512RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo512RNG), INTENT(IN)    :: RNG      !! 'XoShiRo512RNG' object
    tCharAlloc                          :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (RNG%XoShiRoAlgo)
    CASE (XoShiRo512StarStar)
        Name = 'XoShiRo512StarStarRNG'
    CASE (XoShiRo512Plus)
        Name = 'XoShiRo512PlusRNG'
    CASE (XoShiRo512PlusPlus)
        Name = 'XoShiRo512PlusPlusRNG'
    END SELECT
    
    RETURN

END FUNCTION XoShiRo512RNG_GetName

!******************************************************************************

FUNCTION XoShiRo512RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo512RNG), INTENT(IN)    :: RNG  !! 'XoShiRo512RNG' object
    tIndex                              :: Size !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = SEED_SIZE
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION XoShiRo512RNG_GetSeedSize

!******************************************************************************

FUNCTION XoShiRo512StarStar_Next(RNG) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoShiRo512StarStar generator, which is a fast all-purpose 64-bit
    !  generator with memory footprint of 512 bits and the period of 2**512-1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo512RNG), INTENT(IN)    :: RNG      !! 'XoShiRo512RNG' object
    tSInt64                             :: Output   !! 64-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = RotateLeft(RNG%State(1) * 5, 7) * 9
    
    RETURN

END FUNCTION XoShiRo512StarStar_Next

!******************************************************************************

FUNCTION XoShiRo512Plus_Next(RNG) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoShiRo512Plus generator, which is a fast 64-bit generator
    !  suitable for floating-point-number generation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo512RNG), INTENT(IN)    :: RNG      !! 'XoShiRo512RNG' object
    tSInt64                             :: Output   !! 64-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = RNG%State(0) + RNG%State(2)
    
    RETURN

END FUNCTION XoShiRo512Plus_Next

!******************************************************************************

FUNCTION XoShiRo512PlusPlus_Next(RNG) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the next integer output based on the current states of
    ! the XoShiRo512PlusPlus generator, which is a fast all-purpose
    ! 64-bit generator with memory footprint of 512 bits.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo512RNG), INTENT(IN)    :: RNG      ! 'XoShiRo512RNG' object
    tSInt64                             :: Output   ! 64-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = RotateLeft(RNG%State(0) + RNG%State(2), 17) + RNG%State(2)
    
    RETURN

END FUNCTION XoShiRo512PlusPlus_Next

!******************************************************************************

SUBROUTINE XoShiRo512RNG_Perform_Jump(RNG, JCoef)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the jump to advance the generator state.
    !  Resets the cached state of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo512RNG), INTENT(INOUT) :: RNG          !! 'XoShiRo512RNG' object
    tSInt64,              INTENT(IN)    :: JCoef(0:)    !! jump coefficients

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: S(0:SEED_SIZE-1), Dummy
    tSInt32     :: I, J, B

! FLOW
    
    S = 0_kInt64
    DO I = 0, SEED_SIZE-1
        DO B = 0, 63
            IF (IAND(JCoef(I), SHIFTL(1_kInt64, B)) /= 0_kInt64) THEN
                DO J = 0, SEED_SIZE-1
                    S(J) = IEOR(S(J), RNG%State(J))
                END DO
            END IF
            Dummy = RNG%NextLong()
        END DO
    END DO
    RNG%State = S
    
    RETURN

END SUBROUTINE XoShiRo512RNG_Perform_Jump

!******************************************************************************

FUNCTION XoShiRo512RNG_Copy(Src) RESULT(Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy all components of the source object to the destination object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo512RNG), INTENT(INOUT) :: Src  !! source object
    TYPE(XoShiRo512RNG)                 :: Dst  !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! copy components
    Dst%State       =  Src%State
    Dst%XoShiRoAlgo =  Src%XoShiRoAlgo
    Dst%NextOutput  => Src%NextOutput
    CALL Src%CopySeed(Dst)

    RETURN

END FUNCTION XoShiRo512RNG_Copy

!******************************************************************************

FUNCTION XoShiRo512RNG_Jump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  instance.  The new instance is returned.  <br>
    !  The current state will be advanced in a single operation by the equivalent of
    !  a number of sequential calls to a method that updates the state of the generator.
    !  The jump size is the equivalent of 2<sup>256</sup> calls to the generator's 
    !  NextLong() method.  It can provide up to 2<sup>256</sup> non-overlapping
    !  subsequences.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo512RNG), INTENT(INOUT) :: RNG      !! 'XoShiRo512RNG' object
    TYPE(XoShiRo512RNG)                 :: NewRNG   !! new 'XoShiRo512RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()
    
    ! advance states of the current instance
    CALL RNG%PerformJump(JUMP_COEFFICIENTS)
    
    RETURN

END FUNCTION XoShiRo512RNG_Jump

!******************************************************************************

FUNCTION XoShiRo512RNG_LongJump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  instance.  The new instance is returned.  <br>
    !  The current state will be advanced in a single operation by the equivalent of
    !  a number of sequential calls to a method that updates the state of the generator.
    !  The jump size is the equivalent of 2<sup>384</sup> calls to the generator's 
    !  NextLong() method.  It can provide up to 2<sup>128</sup> non-overlapping
    !  subsequences of length 2<sup>384</sup>; each subsequence can provide up to
    !  2<sup>128</sup> non-overlapping subsequences of length 2<sup>256</sup> using
    !  the generator's Jump() method.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo512RNG), INTENT(INOUT) :: RNG      !! 'XoShiRo512RNG' object
    TYPE(XoShiRo512RNG)                 :: NewRNG   !! new 'XoShiRo512RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()
    
    ! advance states of the current instance
    CALL RNG%PerformJump(LONG_JUMP_COEFFICIENTS)
    
    RETURN

END FUNCTION XoShiRo512RNG_LongJump

!******************************************************************************

SUBROUTINE XoShiRo512RNG_Finalization(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the 'XoShiRo512RNG' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(XoShiRo512RNG), INTENT(INOUT)   :: RNG  !! 'XoShiRo512RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    NULLIFY(RNG%NextOutput)
    
    RETURN

END SUBROUTINE XoShiRo512RNG_Finalization

!******************************************************************************

END MODULE MClass_XoShiRo512RNG
    
!******************************************************************************
