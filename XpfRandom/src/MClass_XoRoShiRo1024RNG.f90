
MODULE MClass_XoRoShiRo1024RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *XoRoShiRo1024RNG* type and its related routines.
!   The *XoRoShiRo1024RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *XoRoShiRo1024RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on algorithms from the Xor-Shift-Rotate
!   family of generators by Sebastiano Vigna. <br>
!   The *XoRoShiRo1024RNG* type can represent three PRNG classes: <br>
!   - the XoRoShiRo1024StarStar RNG, which is a large-state all-purpose 64-bit generator, <br>
!   - the XoRoShiRo1024Star  RNG, which is a large-state 64-bit generator suitable for
!     real number generation, or <br>
!   - the XoRoShiRo1024PlusPlus RNG, which is a large-state all-purpose 64-bit generator. <br>
!   By default, the XoRoShiRo1024StarStar RNG is employed.  However, other XoRoShiRo1024
!   PRNG variants can be utilized by specifying an algorithm flag (between 1 to 3)
!   when initializing the generator.  The *XoRoShiRo1024RNG* type has state size of
!   1024 bits and period of 2<sup>1024</sup>-1. <br>
!   In addition to common operations of a PRNG, the *XoRoShiRo1024RNG* type provides
!   the *Jump* and *LongJump* methods where a large (or very large) number of steps
!   of the output sequence can be advanced in a single operation.  Each method creates
!   (and also returns) a copy of the input PRNG and then advances the state of the
!   specified PRNG.  The PRNG and its copy produce non-overlapping output for the
!   length of the jump intendedly for use in parallel computations. <br>
!   It is important to note that the *XoRoShiRo1024* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://prng.di.unimi.it/">
!       Xoshiro/Xoroshiro Generators and the PRNG shootout.</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/XoRoShiRo1024StarStar.html">
!       Apache Commons RNG: Class XoRoShiRo1024StarStar</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/XoRoShiRo1024Star.html">
!       Apache Commons RNG: Class XoRoShiRo1024Star</a> <br>
!   [4] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source64/XoRoShiRo1024PlusPlus.html">
!       Apache Commons RNG: Class XoRoShiRo1024PlusPlus</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: XoRoShiRo1024RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! Size of the state vector
    tSInt32,  PARAMETER :: SEED_SIZE = 16
    ! The coefficients for the jump function
    ! The coefficients for the long jump function
    tSInt64,  PARAMETER :: JUMP_COEFFICIENTS(0:SEED_SIZE-1) = [     &
        ToInt64(Z'931197D8E3177F17'), ToInt64(Z'B59422E0B9138C5F'),   &
        ToInt64(Z'F06A6AFB49D668BB'), ToInt64(Z'ACB8A6412C8A1401'),   &
        ToInt64(Z'12304EC85F0B3468'), ToInt64(Z'B7DFE7079209891E'),   &
        ToInt64(Z'405B7EEC77D9EB14'), ToInt64(Z'34EAD68280C44E4A'),   &
        ToInt64(Z'E0E4BA3E0AC9E366'), ToInt64(Z'8F46EDA8348905B7'),   &
        ToInt64(Z'328BF4DBAD90D6FF'), ToInt64(Z'C8FD6FB31C9EFFC3'),   &
        ToInt64(Z'E899D452D4B67652'), ToInt64(Z'45F387286ADE3205'),   &
        ToInt64(Z'03864F454A8920BD'), ToInt64(Z'A68FA28725B1B384')]
    tSInt64,  PARAMETER :: LONG_JUMP_COEFFICIENTS(0:SEED_SIZE-1) = [ &
        ToInt64(Z'7374156360BBF00F'), ToInt64(Z'4630C2EFA3B3C1F6'),    &
        ToInt64(Z'6654183A892786B1'), ToInt64(Z'94F7BFCBFB0F1661'),    &
        ToInt64(Z'27D8243D3D13EB2D'), ToInt64(Z'9701730F3DFB300F'),    &
        ToInt64(Z'2F293BAAE6F604AD'), ToInt64(Z'A661831CB60CD8B6'),    &
        ToInt64(Z'68280C77D9FE008C'), ToInt64(Z'50554160F5BA9459'),    &
        ToInt64(Z'2FC20B17EC7B2A9A'), ToInt64(Z'49189BBDC8EC9F8F'),    &
        ToInt64(Z'92A65BCA41852CC1'), ToInt64(Z'F46820DD0509C12A'),    &
        ToInt64(Z'52B00C35FBF92185'), ToInt64(Z'1E5B3B7F589E03C1')]
    ! Parameters for algorithm flag
    tSInt32,  PARAMETER, PUBLIC :: XoRoShiRo1024StarStar = 1    !! flag for XoRoShiRo1024StarStar PRNG
    tSInt32,  PARAMETER, PUBLIC :: XoRoShiRo1024Star     = 2    !! flag for XoRoShiRo1024Star PRNG
    tSInt32,  PARAMETER, PUBLIC :: XoRoShiRo1024PlusPlus = 3    !! flag for XoRoShiRo1024PlusPlus PRNG

!** DERIVED TYPE DEFINITIONS
    !> The *XoRoShiRo1024RNG* type is a *Long* PRNG type based on algorithms from
    !  the Xor-Shift-Rotate family of generators by Sebastiano Vigna.
    TYPE, EXTENDS(LongRNG)  :: XoRoShiRo1024RNG
        PRIVATE
        ! states
        tSInt64     :: State(0:SEED_SIZE-1)
        ! index in "state" array
        tSInt32     :: Index
        ! algorithm flag
        tSInt32     :: XoRoShiRoAlgo = XoRoShiRo1024StarStar
        ! function to return the next integer output
        PROCEDURE(Next), POINTER    :: NextOutput  => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: InitWithFlag     => XoRoShiRo1024RNG_InitWithFlag
        PROCEDURE, PRIVATE  :: PerformJump      => XoRoShiRo1024RNG_Perform_Jump
        PROCEDURE, PRIVATE  :: MakeACopy        => XoRoShiRo1024RNG_Copy
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => XoRoShiRo1024RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextLongImpl  => XoRoShiRo1024RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => XoRoShiRo1024RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => XoRoShiRo1024RNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----               Generic Interface                           -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Initialize <br>
        !  **Purpose**:  To initialize the pseudo-random number generator. <br>
        !  **Usage**: <br>
        !   ! initialize without seed; use XoRoShiRo1024StarStar (default) algorithm <br>
        !   --->    CALL RNG%Initialize() <br>
        !   ! initialize with seed(s); use XoRoShiRo1024StarStar (default) algorithm <br>
        !   --->    CALL RNG%Initialize(Seeds) <br>
        !   ! initialize without seed; use XoRoShiRo1024Star algorithm <br>
        !   --->    CALL RNG%Initialize(XoRoShiRo1024Star) <br>
        !   ! initialize with seed(s); use XoRoShiRo1024PlusPlus algorithm <br>
        !   --->    CALL RNG%Initialize(XoRoShiRo1024PlusPlus, Seeds)
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
        PROCEDURE       :: Jump             => XoRoShiRo1024RNG_Jump
        !> **Type-Bound Function**: LongJump <br>
        !  **Purpose**:  To create a copy of the specified PRNG and then retreat
        !                the state of the current PRNG.  The copy is returned as
        !                a new PRNG. <br>
        !  **Usage**: <br>
        !   --->    NewRNG = PRNG%LongJump() <br>
        !  **Note**: The *LongJump* method is equivalent to calling the *NextInteger*
        !            method repeatedly a very large number of times.
        PROCEDURE       :: LongJump         => XoRoShiRo1024RNG_LongJump
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the PRNG.
        FINAL           :: XoRoShiRo1024RNG_Finalization
        ! ---------------------------------------------------------------------
    END TYPE XoRoShiRo1024RNG

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Next(RNG, S0, S15) RESULT(Output)
            IMPORT
            CLASS(XoRoShiRo1024RNG), INTENT(IN) :: RNG
            tSInt64,                 INTENT(IN) :: S0, S15
            tSInt64                             :: Output
        END FUNCTION Next
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE XoRoShiRo1024RNG_InitWithFlag(RNG, XoRoShiRoAlgo, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified algorithm flag.  Optionally,
    !  initial seeds can be specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'XoRoShiRo1024RNG' object
    CLASS(XoRoShiRo1024RNG), INTENT(INOUT)  :: RNG
    !> algorithm flag <br>
    ! - 1, use XoRoShiRo1024StarStar algorithm. <br>
    ! - 2, use XoRoShiRo1024Star algorithm. <br>
    ! - 3, use XoRoShiRo1024PlusPlus algorithm.
    tSInt32,                 INTENT(IN)     :: XoRoShiRoAlgo
    !% optional 32-bit integer seed(s)
    tSInt64, OPTIONAL,       INTENT(IN)     :: Seed(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check and set algorithm flag
    IF (IN_RANGE(XoRoShiRoAlgo, 1, 3)) THEN
        RNG%XoRoShiRoAlgo = XoRoShiRoAlgo
    ELSE
        RNG%XoRoShiRoAlgo = XoRoShiRo1024StarStar
    END IF

    IF (PRESENT(Seed)) THEN
        ! initialize the generator with seed(s)
        CALL RNG%Initialize(Seed)
    ELSE
        ! initialize the generator without seed
        CALL RNG%Initialize()
    END IF

    RETURN

END SUBROUTINE XoRoShiRo1024RNG_InitWithFlag

!******************************************************************************

SUBROUTINE XoRoShiRo1024RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo1024RNG), INTENT(INOUT)  :: RNG      !! 'XoRoShiRo1024RNG' object
    tSInt64,                 INTENT(IN)     :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: Seed0(0:SEED_SIZE-1)

! FLOW

    ! set initial seed
    CALL Fill_State(Seed, Seed0)
    
    ! copy seeds to states
    RNG%State = Seed0

    ! set pointer to next output function
    SELECT CASE (RNG%XoRoShiRoAlgo)
    CASE (XoRoShiRo1024StarStar)
        RNG%NextOutput  => XoRoShiRo1024StarStar_Next
    CASE (XoRoShiRo1024Star)
        RNG%NextOutput  => XoRoShiRo1024Star_Next
    CASE (XoRoShiRo1024PlusPlus)
        RNG%NextOutput  => XoRoShiRo1024PlusPlus_Next
    END SELECT

    RETURN

END SUBROUTINE XoRoShiRo1024RNG_BaseInit

!******************************************************************************

FUNCTION XoRoShiRo1024RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo1024RNG), INTENT(INOUT)  :: RNG      !! 'XoRoShiRo1024RNG' object
    tSInt64                                 :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: S0, S15
    tSInt32     :: Q

! FLOW

    ! update index
    Q = RNG%Index
    RNG%Index = IAND((RNG%Index + 1), 15)
    
    ! get random number
    S0 = RNG%State(RNG%Index)
    S15 = RNG%State(Q)
    RandNum = RNG%NextOutput(S0, S15)

    ! update state
    S15 = IEOR(S15, S0)
    RNG%State(Q) = IEOR(IEOR(RotateLeft(S0, 25), S15), SHIFTL(S15, 27))
    RNG%State(RNG%Index) = RotateLeft(S15, 36)

    RETURN

END FUNCTION XoRoShiRo1024RNG_NextLong

!******************************************************************************

FUNCTION XoRoShiRo1024RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo1024RNG), INTENT(IN) :: RNG      !! 'XoRoShiRo1024RNG' object
    tCharAlloc                          :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (RNG%XoRoShiRoAlgo)
    CASE (XoRoShiRo1024StarStar)
        Name = 'XoRoShiRo1024StarStarRNG'
    CASE (XoRoShiRo1024Star)
        Name = 'XoRoShiRo1024StarRNG'
    CASE (XoRoShiRo1024PlusPlus)
        Name = 'XoRoShiRo1024PlusPlusRNG'
    END SELECT
    
    RETURN

END FUNCTION XoRoShiRo1024RNG_GetName

!******************************************************************************

FUNCTION XoRoShiRo1024RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo1024RNG), INTENT(IN) :: RNG  !! 'XoRoShiRo1024RNG' object
    tIndex                              :: Size !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = SEED_SIZE
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION XoRoShiRo1024RNG_GetSeedSize

!******************************************************************************

FUNCTION XoRoShiRo1024StarStar_Next(RNG, S0, S15) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoRoShiRo1024StarStar generator, which is large-state all-purpose
    !  64-bit generator with memory footprint of 1024 bits and the period
    !  of 2**1024-1.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo1024RNG), INTENT(IN) :: RNG      !! 'XoRoShiRo1024RNG' object
    tSInt64,                 INTENT(IN) :: S0, S15
    tSInt64                             :: Output   !! 64-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = RotateLeft(S0 * 5, 7) * 9
    ASSOCIATE (Dummy => RNG, Dummy2 => S15); END ASSOCIATE
    
    RETURN

END FUNCTION XoRoShiRo1024StarStar_Next

!******************************************************************************

FUNCTION XoRoShiRo1024Star_Next(RNG, S0, S15) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoRoShiRo1024Plus generator, which is a large-state 64-bit
    !  generator suitable for floating-point-number generation.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo1024RNG), INTENT(IN) :: RNG      !! 'XoRoShiRo1024RNG' object
    tSInt64,                 INTENT(IN) :: S0, S15
    tSInt64                             :: Output   !! 64-bit output

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER  ::Multiplier = ToInt64(Z'9E3779B97F4A7C13')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = S0 * Multiplier
    ASSOCIATE (Dummy => RNG, Dummy2 => S15); END ASSOCIATE
    
    RETURN

END FUNCTION XoRoShiRo1024Star_Next

!******************************************************************************

FUNCTION XoRoShiRo1024PlusPlus_Next(RNG, S0, S15) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoRoShiRo1024PlusPlus generator, which is a large-state all-purpose
    !  64-bit generator with memory footprint of 1024 bits.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo1024RNG), INTENT(IN) :: RNG      !! 'XoRoShiRo1024RNG' object
    tSInt64,                 INTENT(IN) :: S0, S15
    tSInt64                             :: Output   !! 64-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = RotateLeft(S0 + S15, 23) + S15
    ASSOCIATE (Dummy => RNG); END ASSOCIATE
    
    RETURN

END FUNCTION XoRoShiRo1024PlusPlus_Next

!******************************************************************************

SUBROUTINE XoRoShiRo1024RNG_Perform_Jump(RNG, JCoef)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the jump to advance the generator state.
    !  Resets the cached state of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo1024RNG), INTENT(INOUT)  :: RNG          !! 'XoRoShiRo1024RNG' object
    tSInt64,                 INTENT(IN)     :: JCoef(0:)    !! jump coefficients

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: NewState(0:SEED_SIZE-1)
    tSInt64     :: Dummy
    tSInt32     :: I, J, B

! FLOW
    
    NewState = 0_kInt64
    DO I = 0, SEED_SIZE-1
        DO B = 0, 63
            IF (IAND(JCoef(I), SHIFTL(1_kInt64, B)) /= 0_kInt64) THEN
                DO J = 0, SEED_SIZE-1
                    NewState(J) = IEOR(NewState(J), RNG%State(IAND(J + RNG%Index, 15)))
                END DO
            END IF
            Dummy = RNG%NextLong()
        END DO
    END DO
    ! Note: Calling the NextLong() function updates 'index'.
    ! The present index effectively becomes 0.
    DO J = 0, SEED_SIZE-1
        RNG%State(IAND(J + RNG%Index, 15)) = NewState(J)
    END DO
    
    RETURN

END SUBROUTINE XoRoShiRo1024RNG_Perform_Jump

!******************************************************************************

FUNCTION XoRoShiRo1024RNG_Copy(Src) RESULT(Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy all components of the source object to the destination object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo1024RNG), INTENT(INOUT)   :: Src  !! source object
    TYPE(XoRoShiRo1024RNG)                   :: Dst  !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! copy components
    Dst%State         =  Src%State
    Dst%Index         =  Src%Index
    Dst%XoRoShiRoAlgo =  Src%XoRoShiRoAlgo
    Dst%NextOutput    => Src%NextOutput
    CALL Src%CopySeed(Dst)

    RETURN

END FUNCTION XoRoShiRo1024RNG_Copy

!******************************************************************************

FUNCTION XoRoShiRo1024RNG_Jump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  instance.  The new instance is returned.  <br>
    !  The current state will be advanced in a single operation by the equivalent of
    !  a number of sequential calls to a method that updates the state of the generator.
    !  The jump size is the equivalent of 2<sup>512</sup> calls to the generator's 
    !  NextLong() method.  It can provide up to 2<sup>512</sup> non-overlapping
    !  subsequences.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo1024RNG), INTENT(INOUT)  :: RNG      !! 'XoRoShiRo1024RNG' object
    TYPE(XoRoShiRo1024RNG)                  :: NewRNG   !! new 'XoRoShiRo1024RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()
    
    ! advance states of the current instance
    CALL RNG%PerformJump(JUMP_COEFFICIENTS)
    
    RETURN

END FUNCTION XoRoShiRo1024RNG_Jump

!******************************************************************************

FUNCTION XoRoShiRo1024RNG_LongJump(RNG) RESULT(NewRNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To create a copy of the generator and then advances the state of the current
    !  instance.  The new instance is returned.  <br>
    !  The current state will be advanced in a single operation by the equivalent of 
    !  a number of sequential calls to a method that updates the state of the generator.
    !  The jump size is the equivalent of 2<sup>768</sup> calls to the generator's 
    !  NextLong() method.  It can provide up to 2<sup>256</sup> non-overlapping
    !  subsequences of length 2<sup>768</sup>; each subsequence can provide up to
    !  2<sup>256</sup> non-overlapping subsequences of length 2<sup>512</sup> using
    !  the generator's Jump() method.
  
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo1024RNG), INTENT(INOUT)  :: RNG      !! 'XoRoShiRo1024RNG' object
    TYPE(XoRoShiRo1024RNG)                  :: NewRNG   !! new 'XoRoShiRo1024RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! make a copy of the generator
    NewRNG = RNG%MakeACopy()
    
    ! advance states of the current instance
    CALL RNG%PerformJump(LONG_JUMP_COEFFICIENTS)
    
    RETURN

END FUNCTION XoRoShiRo1024RNG_LongJump

!******************************************************************************

SUBROUTINE XoRoShiRo1024RNG_Finalization(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the 'XoRoShiRo1024RNG' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(XoRoShiRo1024RNG), INTENT(INOUT)   :: RNG  !! 'XoRoShiRo1024RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    NULLIFY(RNG%NextOutput)
    
    RETURN

END SUBROUTINE XoRoShiRo1024RNG_Finalization

!******************************************************************************

END MODULE MClass_XoRoShiRo1024RNG
    
!******************************************************************************
