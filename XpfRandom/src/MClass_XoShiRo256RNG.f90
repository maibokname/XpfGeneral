
MODULE MClass_XoShiRo256RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *XoShiRo256RNG* type and its related routines.
!   The *XoShiRo256RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *XoShiRo256RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on algorithms from the Xor-Shift-Rotate
!   family of generators by Sebastiano Vigna. <br>
!   The *XoShiRo256RNG* type can represent three PRNG classes: <br>
!   - the XoShiRo256StarStar RNG, which is a fast all-purpose 64-bit generator, <br>
!   - the XoShiRo256Plus  RNG, which is a fast 64-bit generator suitable for
!     real number generation, or <br>
!   - the XoShiRo256PlusPlus RNG, which is a fast all-purpose 64-bit generator. <br>
!   By default, the XoShiRo256StarStar RNG is employed.  However, other XoShiRo256
!   PRNG variants can be utilized by specifying an algorithm flag (between 1 to 3)
!   when initializing the generator.  The *XoShiRo256RNG* type has state size of
!   256 bits and period of 2<sup>256</sup>-1. <br>
!   In addition to common operations of a PRNG, the *XoShiRo256RNG* type provides
!   the *Jump* and *LongJump* methods where a large (or very large) number of steps
!   of the output sequence can be advanced in a single operation.  Each method creates
!   (and also returns) a copy of the input PRNG and then advances the state of the
!   specified PRNG.  The PRNG and its copy produce non-overlapping output for the
!   length of the jump intendedly for use in parallel computations. <br>
!   It is important to note that the *XoShiRo256* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://prng.di.unimi.it/">
!       Xoshiro/Xoroshiro Generators and the PRNG shootout.</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/XoShiRo256StarStar.html">
!       Apache Commons RNG: Class XoShiRo256StarStar</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/XoShiRo256Plus.html">
!       Apache Commons RNG: Class XoShiRo256Plus</a> <br>
!   [4] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/XoShiRo256PlusPlus.html">
!       Apache Commons RNG: Class XoShiRo256PlusPlus</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: XoShiRo256RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! Size of the state vector
    tSInt32,  PARAMETER :: SEED_SIZE = 4
    ! The coefficients for the jump function
    tSInt64,  PARAMETER :: JUMP_COEFFICIENTS(0:SEED_SIZE-1) = [             &
        ToInt64(Z'180EC6D33CFD0ABA'), ToInt64(Z'D5A61266F0C9392C'), &
        ToInt64(Z'A9582618E03FC9AA'), ToInt64(Z'39ABDC4529B1661C')]
    ! The coefficients for the long jump function
    tSInt64,  PARAMETER :: LONG_JUMP_COEFFICIENTS(0:SEED_SIZE-1) = [        &
        ToInt64(Z'76E15D3EFEFDCBBF'), ToInt64(Z'C5004E441C522FB3'), &
        ToInt64(Z'77710069854EE241'), ToInt64(Z'39109BB02ACBE635')]
    ! Parameters for algorithm flag
    tSInt32,  PARAMETER, PUBLIC :: XoShiRo256StarStar = 1 !! flag for XoShiRo256StarStar PRNG
    tSInt32,  PARAMETER, PUBLIC :: XoShiRo256Plus     = 2 !! flag for XoShiRo256Plus PRNG
    tSInt32,  PARAMETER, PUBLIC :: XoShiRo256PlusPlus = 3 !! flag for XoShiRo256PlusPlus PRNG

!** DERIVED TYPE DEFINITIONS
    !> The *XoShiRo256RNGRNG* type is a *Long* PRNG type based on algorithms from
    !  the Xor-Shift-Rotate family of generators by Sebastiano Vigna.
    TYPE, EXTENDS(LongRNG)  :: XoShiRo256RNG
        PRIVATE
        ! states
        tSInt64  :: State0, State1, State2, State3
        ! algorithm flag
        tSInt32     :: XoShiRoAlgo = XoShiRo256StarStar
        ! function to return the next integer output
        PROCEDURE(Next), POINTER    :: NextOutput => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: InitWithFlag     => XoShiRo256RNG_InitWithFlag
        PROCEDURE, PRIVATE  :: PerformJump      => XoShiRo256RNG_Perform_Jump
        PROCEDURE, PRIVATE  :: MakeACopy        => XoShiRo256RNG_Copy
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => XoShiRo256RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextLongImpl  => XoShiRo256RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => XoShiRo256RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => XoShiRo256RNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----               Generic Interface                           -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Initialize <br>
        !  **Purpose**:  To initialize the pseudo-random number generator. <br>
        !  **Usage**: <br>
        !   ! initialize without seed; use XoShiRo256StarStar (default) algorithm <br>
        !   --->    CALL RNG%Initialize() <br>
        !   ! initialize with seed(s); use XoShiRo256StarStar (default) algorithm <br>
        !   --->    CALL RNG%Initialize(Seeds) <br>
        !   ! initialize without seed; use XoShiRo256Plus algorithm <br>
        !   --->    CALL RNG%Initialize(XoShiRo256Plus) <br>
        !   ! initialize with seed(s); use XoShiRo256PlusPlus algorithm <br>
        !   --->    CALL RNG%Initialize(XoShiRo256PlusPlus, Seeds)
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
        PROCEDURE       :: Jump             => XoShiRo256RNG_Jump
        !> **Type-Bound Function**: LongJump <br>
        !  **Purpose**:  To create a copy of the specified PRNG and then retreat
        !                the state of the current PRNG.  The copy is returned as
        !                a new PRNG. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%LongJump() <br>
        !  **Note**: The *LongJump* method is equivalent to calling the *NextInteger*
        !            method repeatedly a very large number of times.
        PROCEDURE       :: LongJump         => XoShiRo256RNG_LongJump
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the PRNG.
        FINAL           :: XoShiRo256RNG_Finalization
        ! ---------------------------------------------------------------------
    END TYPE XoShiRo256RNG

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Next(RNG) RESULT(Output)
            IMPORT
            CLASS(XoShiRo256RNG), INTENT(IN)    :: RNG
            tSInt64                             :: Output
        END FUNCTION Next
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE XoShiRo256RNG_InitWithFlag(RNG, XoShiRoAlgo, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified algorithm flag.  Optionally,
    !  initial seeds can be specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'XoShiRo256RNG' object
    CLASS(XoShiRo256RNG), INTENT(INOUT) :: RNG
    !> algorithm flag <br>
    ! - 1, use XoShiRo256StarStar algorithm. <br>
    ! - 2, use XoShiRo256Plus algorithm. <br>
    ! - 3, use XoShiRo256PlusPlus algorithm.
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
        RNG%XoShiRoAlgo = XoShiRo256StarStar
    END IF

    IF (PRESENT(Seed)) THEN
        ! initialize the generator with seed(s)
        CALL RNG%Initialize(Seed)
    ELSE
        ! initialize the generator without seed
        CALL RNG%Initialize()
    END IF

    RETURN

END SUBROUTINE XoShiRo256RNG_InitWithFlag

!******************************************************************************

SUBROUTINE XoShiRo256RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    ! To initialize the 'XoShiRo256RNG' object with optional initial seeds.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo256RNG), INTENT(INOUT) :: RNG          !! 'XoShiRo256RNG' object
    tSInt64,              INTENT(IN)    :: Seed(:)      !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: Seed0(0:SEED_SIZE-1)

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
    CASE (XoShiRo256StarStar)
        RNG%NextOutput => XoShiRo256StarStar_Next
    CASE (XoShiRo256Plus)
        RNG%NextOutput => XoShiRo256Plus_Next 
    CASE (XoShiRo256PlusPlus)
        RNG%NextOutput => XoShiRo256PlusPlus_Next 
    END SELECT

    RETURN

END SUBROUTINE XoShiRo256RNG_BaseInit

!******************************************************************************

FUNCTION XoShiRo256RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo256RNG), INTENT(INOUT) :: RNG      !! 'XoShiRo256RNG' object
    tSInt64                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: TmpState

! FLOW
    
    ! get next integer output
    RandNum = RNG%NextOutput()

    ! update states
    TmpState   = SHIFTL(RNG%State1, 17)
    RNG%State2 = IEOR(RNG%State2, RNG%State0)
    RNG%State3 = IEOR(RNG%State3, RNG%State1)
    RNG%State1 = IEOR(RNG%State1, RNG%State2)
    RNG%State0 = IEOR(RNG%State0, RNG%State3)
    RNG%State2 = IEOR(RNG%State2, TmpState)
    RNG%State3 = RotateLeft(RNG%State3, 45)

    RETURN

END FUNCTION XoShiRo256RNG_NextLong

!******************************************************************************

FUNCTION XoShiRo256RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo256RNG), INTENT(IN)    :: RNG      !! 'XoShiRo256RNG' object
    tCharAlloc                          :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (RNG%XoShiRoAlgo)
    CASE (XoShiRo256StarStar)
        Name = 'XoShiRo256StarStarRNG'
    CASE (XoShiRo256Plus)
        Name = 'XoShiRo256PlusRNG'
    CASE (XoShiRo256PlusPlus)
        Name = 'XoShiRo256PlusPlusRNG'
    END SELECT
    
    RETURN

END FUNCTION XoShiRo256RNG_GetName

!******************************************************************************

FUNCTION XoShiRo256RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo256RNG), INTENT(IN)    :: RNG  !! 'XoShiRo256RNG' object
    tIndex                              :: Size !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = SEED_SIZE
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION XoShiRo256RNG_GetSeedSize

!******************************************************************************

FUNCTION XoShiRo256StarStar_Next(RNG) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoShiRo256StarStar generator, which is a fast all-purpose 64-bit
    !  generator with memory footprint of 256 bits and the period of 2**256-1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo256RNG), INTENT(IN)    :: RNG      !! 'XoShiRo256RNG' object
    tSInt64                             :: Output   !! 64-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = RotateLeft(RNG%State1 * 5, 7) * 9
    
    RETURN

END FUNCTION XoShiRo256StarStar_Next

!******************************************************************************

FUNCTION XoShiRo256Plus_Next(RNG) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoShiRo256Plus generator, which is a fast 64-bit generator
    !  suitable for floating-point-number generation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo256RNG), INTENT(IN)    :: RNG      !! 'XoShiRo256RNG' object
    tSInt64                             :: Output   !! 64-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = RNG%State0 + RNG%State3
    
    RETURN

END FUNCTION XoShiRo256Plus_Next

!******************************************************************************

FUNCTION XoShiRo256PlusPlus_Next(RNG) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoShiRo256PlusPlus generator, which is a fast all-purpose
    !  64-bit generator with memory footprint of 256 bits.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo256RNG), INTENT(IN)    :: RNG      !! 'XoShiRo256RNG' object
    tSInt64                             :: Output   !! 64-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = RotateLeft(RNG%State0 + RNG%State3, 23) + RNG%State0
    
    RETURN

END FUNCTION XoShiRo256PlusPlus_Next

!******************************************************************************

SUBROUTINE XoShiRo256RNG_Perform_Jump(RNG, JCoef)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the jump to advance the generator state.
    !  Resets the cached state of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo256RNG), INTENT(INOUT) :: RNG          !! 'XoShiRo256RNG' object
    tSInt64,              INTENT(IN)    :: JCoef(0:)    !! jump coefficients

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: S0, S1, S2, S3, Dummy
    tSInt32     :: I, B

! FLOW
    
    S0 = 0_kInt64
    S1 = 0_kInt64
    S2 = 0_kInt64
    S3 = 0_kInt64
    DO I = 0, SEED_SIZE-1
        DO B = 0, 63
            IF (IAND(JCoef(I), SHIFTL(1_kInt64, B)) /= 0_kInt64) THEN
                S0 = IEOR(S0, RNG%State0)
                S1 = IEOR(S1, RNG%State1)
                S2 = IEOR(S2, RNG%State2)
                S3 = IEOR(S3, RNG%State3)
            END IF
            Dummy = RNG%NextLong()
        END DO
    END DO
    RNG%State0 = S0
    RNG%State1 = S1
    RNG%State2 = S2
    RNG%State3 = S3
    
    RETURN

END SUBROUTINE XoShiRo256RNG_Perform_Jump

!******************************************************************************

FUNCTION XoShiRo256RNG_Copy(Src) RESULT(Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy all components of the source object to the destination object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo256RNG), INTENT(INOUT) :: Src  !! source object
    TYPE(XoShiRo256RNG)                 :: Dst  !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! copy components
    Dst%State0      =  Src%State0
    Dst%State1      =  Src%State1
    Dst%State2      =  Src%State2
    Dst%State3      =  Src%State3
    Dst%XoShiRoAlgo =  Src%XoShiRoAlgo
    Dst%NextOutput  => Src%NextOutput
    CALL Src%CopySeed(Dst)

    RETURN

END FUNCTION XoShiRo256RNG_Copy

!******************************************************************************

FUNCTION XoShiRo256RNG_Jump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  instance.  The new instance is returned.  <br>
    !  The current state will be advanced in a single operation by the equivalent of
    !  a number of sequential calls to a method that updates the state of the generator.
    !  The jump size is the equivalent of 2<sup>128</sup> calls to the generator's 
    !  NextLong() method.  It can provide up to 2<sup>128</sup> non-overlapping
    !  subsequences.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo256RNG), INTENT(INOUT) :: RNG      !! 'XoShiRo256RNG' object
    TYPE(XoShiRo256RNG)                 :: NewRNG   !! new 'XoShiRo256RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()
    
    ! advance states of the current instance
    CALL RNG%PerformJump(JUMP_COEFFICIENTS)
    
    RETURN

END FUNCTION XoShiRo256RNG_Jump

!******************************************************************************

FUNCTION XoShiRo256RNG_LongJump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  instance.  The new instance is returned.  <br>
    !  The current state will be advanced in a single operation by the equivalent of
    !  a number of sequential calls to a method that updates the state of the generator.
    !  The jump size is the equivalent of 2<sup>192</sup> calls to the generator's 
    !  NextLong() method.  It can provide up to 2<sup>64</sup> non-overlapping
    !  subsequences of length 2<sup>192</sup>; each subsequence can provide up to
    !  2<sup>64</sup> non-overlapping subsequences of length 2<sup>128</sup> using
    !  the generator's Jump() method.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoShiRo256RNG), INTENT(INOUT) :: RNG      !! 'XoShiRo256RNG' object
    TYPE(XoShiRo256RNG)                 :: NewRNG   !! new 'XoShiRo256RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()
    
    ! advance states of the current instance
    CALL RNG%PerformJump(LONG_JUMP_COEFFICIENTS)
    
    RETURN

END FUNCTION XoShiRo256RNG_LongJump

!******************************************************************************

SUBROUTINE XoShiRo256RNG_Finalization(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the 'XoShiRo256RNG' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(XoShiRo256RNG), INTENT(INOUT)   :: RNG  !! 'XoShiRo256RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    NULLIFY(RNG%NextOutput)
    
    RETURN

END SUBROUTINE XoShiRo256RNG_Finalization

!******************************************************************************

END MODULE MClass_XoShiRo256RNG
    
!******************************************************************************
