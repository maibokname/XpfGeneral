
MODULE MClass_PcgLcg32RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *PcgLcg32RNG* type and its related routines.
!   The *PcgLcg32RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *PcgLcg32RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on algorithms from the Permuted
!   Congruential Generator (PCG) family that use an internal 64-bit Linear
!   Congruential Generator (LCG) and output 32-bits per cycle. <br>
!   The *PcgLcg32RNG* type can represent two PRNG classes: <br>
!   - a PCG that is composed of a 64-bit LCG combined with the XSH-RR (xorshift; 
!     random rotate) output transformation, or <br>
!   - a PCG that is composed of a 64-bit LCG combined with the XSH-RS (xorshift; 
!     random shift) output transformation. <br>
!   By default, the random rotate (RR) transformation is employed.  However, the
!   random shift (RS) transformation can be utilized by specifying an algorithm
!   flag to true when initializing the generator.  The *PcgLcg32RNG* type has
!   state size of 128 bits and a period of 2<sup>64</sup>. <br>
!   It is important to note that the *PcgLcg32* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.pcg-random.org/">
!       PCG, A Family of Better Random Number Generators.</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/PcgXshRr32.html">
!       Apache Commons RNG: Class PcgXshRr32</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/PcgXshRs32.html">
!       Apache Commons RNG: Class PcgXshRs32</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: PcgLcg32RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *PcgLcg32RNG* type is an *Integer* PRNG type based on algorithms from
    !  the Permuted Congruential Generator (PCG) family that use an internal 64-bit
    !  Linear Congruential Generator (LCG) and output 32-bits per cycle.
    TYPE, EXTENDS(IntegerRNG)  :: PcgLcg32RNG
        PRIVATE
        ! The state of the LCG
        tSInt64     :: State
        ! The increment of the LCG
        tSInt64     :: Increment
        ! algorithm flag
        tLogical    :: UseRandShift = FalseVal
        ! function to transform the state
        PROCEDURE(Transform), NOPASS, POINTER   :: Permute => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: InitLongSeeds    => PcgLcg32RNG_InitLongSeeds
        PROCEDURE, PRIVATE  :: InitWithFlag     => PcgLcg32RNG_InitWithFlag
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => PcgLcg32RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => PcgLcg32RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => PcgLcg32RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => PcgLcg32RNG_GetSeedSize
        ! ---------------------------------------------------------------------
        ! -----               Generic Interface                           -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Initialize <br>
        !  **Purpose**:  To initialize the pseudo-random number generator. <br>
        !  **Usage**: <br>
        !   ! initialize without seed; use random rotate (default) transformation <br>
        !   --->    CALL RNG%Initialize() <br>
        !   ! initialize with seed(s); use random rotate (default) transformation <br>
        !   --->    CALL RNG%Initialize(Seeds) <br>
        !   ! initialize without seed; use random shift transformation <br>
        !   --->    CALL RNG%Initialize(.TRUE.) <br>
        !   ! initialize with seed(s); use random shift transformation <br>
        !   --->    CALL RNG%Initialize(.TRUE., Seeds)
        GENERIC         :: Initialize       => InitWithFlag
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the PRNG.
        FINAL           :: PcgLcg32RNG_Finalization
        ! ---------------------------------------------------------------------
    END TYPE PcgLcg32RNG

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Transform(Input) RESULT(Output)
            IMPORT
            tSInt64, INTENT(IN) :: Input
            tSInt32             :: Output
        END FUNCTION Transform
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE PcgLcg32RNG_InitWithFlag(RNG, UseRandShift, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified algorithm flag.  Optionally,
    !  initial seeds can be specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'PcgLcg32RNG' object
    CLASS(PcgLcg32RNG), INTENT(INOUT)   :: RNG
    !> algorithm flag <br>
    ! - true, the PCG generator is composed of a 64-bit LCG combined with
    !   the XSH-RS (xorshift; random shift) output transformation. <br>
    ! - false, the PCG generator is composed of a 64-bit LCG combined with
    !   the XSH-RR (xorshift; random rotate) output transformation.
    tLogical,           INTENT(IN)      :: UseRandShift
    !% optional 32-bit integer seed(s)
    tSInt32,  OPTIONAL, INTENT(IN)      :: Seed(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set algorithm flag
    RNG%UseRandShift = UseRandShift

    IF (PRESENT(Seed)) THEN
        ! initialize the generator with seed(s)
        CALL RNG%Initialize(Seed)
    ELSE
        ! initialize the generator without seed
        CALL RNG%Initialize()
    END IF

    RETURN

END SUBROUTINE PcgLcg32RNG_InitWithFlag

!******************************************************************************

SUBROUTINE PcgLcg32RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with specified 32-bit integer seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgLcg32RNG), INTENT(INOUT)   :: RNG          !! 'PcgLcg32RNG' object
    tSInt32,            INTENT(IN)      :: Seed(:)      !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: IntSeed(4)
    tSInt64     :: LngSeed(2)

! FLOW

    ! fill 32-bit integer seeds
    CALL Fill_State(Seed, IntSeed)
    
    ! get 64-bit integer seeds
    LngSeed(1) = IOR(IAND(ToInt64(IntSeed(1)), MaskL), SHIFTL(ToInt64(IntSeed(2)), 32)) &
                    + GOLDEN_RATIO_64
    LngSeed(2) = IOR(IAND(ToInt64(IntSeed(3)), MaskL), SHIFTL(ToInt64(IntSeed(4)), 32)) &
                    + GOLDEN_RATIO_64
    
    ! initialize with 64-bit integer seeds
    CALL RNG%InitLongSeeds(LngSeed)

    RETURN

END SUBROUTINE PcgLcg32RNG_BaseInit

!******************************************************************************

SUBROUTINE PcgLcg32RNG_InitLongSeeds(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'PcgLcg32RNG' object with 64-bit integer seeds whose size is 2.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'PcgLcg32RNG' object
    CLASS(PcgLcg32RNG), INTENT(INOUT)   :: RNG
    !% long seeds
    tSInt64,            INTENT(IN)      :: Seed(2)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set pointer to transformation function
    IF (RNG%UseRandShift) THEN
        RNG%Permute => Transform_Shift
    ELSE
        RNG%Permute => Transform_Rotate
    END IF

    ! Ensure the increment is odd to provide a maximal period LCG.
    RNG%Increment = IOR(SHIFTL(Seed(2), 1), 1_kInt64)
    RNG%State     = Bump(RNG, Seed(1) + RNG%Increment)
    
    RETURN

END SUBROUTINE PcgLcg32RNG_InitLongSeeds

!******************************************************************************

FUNCTION PcgLcg32RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgLcg32RNG), INTENT(INOUT)   :: RNG      !! 'PcgLcg32RNG' object
    tSInt32                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: X

! FLOW
    
    X = RNG%State
    RNG%State = Bump(RNG, RNG%State)
    RandNum = RNG%Permute(X)

    RETURN

END FUNCTION PcgLcg32RNG_NextInteger

!******************************************************************************

FUNCTION PcgLcg32RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgLcg32RNG), INTENT(IN)  :: RNG      !! 'PcgLcg32RNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (RNG%UseRandShift) THEN
        Name = 'PcgLcg32_XSH_RS_RNG'
    ELSE
        Name = 'PcgLcg32_XSH_RR_RNG'
    END IF
    
    RETURN

END FUNCTION PcgLcg32RNG_GetName

!******************************************************************************

FUNCTION PcgLcg32RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgLcg32RNG), INTENT(IN)  :: RNG      !! 'PcgLcg32RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 4
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION PcgLcg32RNG_GetSeedSize

!******************************************************************************

FUNCTION Bump(RNG, Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To provide the next state of the LCG.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgLcg32RNG), INTENT(IN)  :: RNG      !! 'PcgLcg32RNG' object
    tSInt64,            INTENT(IN)  :: Input    !! current state
    tSInt64                         :: Output   !! next state

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = Input * 6364136223846793005_kInt64 + RNG%Increment
    
    RETURN

END FUNCTION Bump

!******************************************************************************
FUNCTION Transform_Rotate(Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To transform the 64-bit state of the generator to a 32-bit output
    !  using random rotate.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Input    !! current state
    tSInt32             :: Output   !! 32-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Count

! FLOW
    
    Count  = ToInt32(SHIFTR(Input, 59))
    Output = RotateRight(ToInt32(SHIFTR(IEOR(Input, SHIFTR(Input, 18)), 27)), Count)
    
    RETURN

END FUNCTION Transform_Rotate

!******************************************************************************

FUNCTION Transform_Shift(Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To transform the 64-bit state of the generator to a 32-bit output
    !  using random shift.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt64, INTENT(IN) :: Input    !! current state
    tSInt32             :: Output   !! 32-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Count

! FLOW
    
    Count  = ToInt32(SHIFTR(Input, 61))
    Output = ToInt32(SHIFTR(IEOR(Input, SHIFTR(Input, 22)), 22 + Count))
    
    RETURN

END FUNCTION Transform_Shift

!******************************************************************************

SUBROUTINE PcgLcg32RNG_Finalization(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the 'PcgLcg32RNG' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(PcgLcg32RNG), INTENT(INOUT)   :: RNG  !! 'PcgLcg32RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    NULLIFY(RNG%Permute)
    
    RETURN

END SUBROUTINE PcgLcg32RNG_Finalization

!******************************************************************************

END MODULE MClass_PcgLcg32RNG
    
!******************************************************************************
