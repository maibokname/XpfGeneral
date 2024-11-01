
MODULE MClass_PcgMcg32RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *PcgMcg32RNG* type and its related routines.
!   The *PcgMcg32RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *PcgMcg32RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on algorithms from the Permuted
!   Congruential Generator (PCG) family that use an internal 64-bit Multiplicative
!   Congruential Generator (MCG) and output 32-bits per cycle. <br>
!   The *PcgMcg32RNG* type can represent two PRNG classes: <br>
!   - a PCG that is composed of a 64-bit MCG combined with the XSH-RR (xorshift; 
!     random rotate) output transformation, or <br>
!   - a PCG that is composed of a 64-bit MCG combined with the XSH-RS (xorshift; 
!     random shift) output transformation. <br>
!   By default, the random rotate (RR) transformation is employed.  However, the
!   random shift (RS) transformation can be utilized by specifying an algorithm
!   flag to true when initializing the generator.  The *PcgMcg32RNG* type has
!   state size of 64 bits and a period of 2<sup>62</sup>. <br>
!   It is important to note that the *PcgMcg32* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.pcg-random.org/">
!       PCG, A Family of Better Random Number Generators.</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/PcgMcgXshRr32.html">
!       Apache Commons RNG: Class PcgMcgXshRr32</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/PcgMcgXshRs32.html">
!       Apache Commons RNG: Class PcgMcgXshRs32</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: PcgMcg32RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *PcgMcg32RNG* type is an *Integer* PRNG type based on algorithms from
    !  the Permuted Congruential Generator (PCG) family that use an internal 64-bit
    !  Multiplicative Congruential Generator (MCG) and output 32-bits per cycle.
    TYPE, EXTENDS(IntegerRNG)  :: PcgMcg32RNG
        PRIVATE
        ! The state of the MCG
        tSInt64     :: State
        ! algorithm flag
        tLogical    :: UseRandShift = FalseVal
        ! function to transform the state
        PROCEDURE(Transform), NOPASS, POINTER   :: Permute => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: InitLongSeeds    => PcgMcg32RNG_InitLongSeeds
        PROCEDURE, PRIVATE  :: InitWithFlag     => PcgMcg32RNG_InitWithFlag
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => PcgMcg32RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => PcgMcg32RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => PcgMcg32RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => PcgMcg32RNG_GetSeedSize
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
        FINAL           :: PcgMcg32RNG_Finalization
        ! ---------------------------------------------------------------------
    END TYPE PcgMcg32RNG

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

SUBROUTINE PcgMcg32RNG_InitWithFlag(RNG, UseRandShift, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified algorithm flag.  Optionally,
    !  initial seeds can be specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'PcgMcg32RNG' object
    CLASS(PcgMcg32RNG), INTENT(INOUT)   :: RNG
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

END SUBROUTINE PcgMcg32RNG_InitWithFlag

!******************************************************************************

SUBROUTINE PcgMcg32RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with specified 32-bit integer seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgMcg32RNG), INTENT(INOUT)   :: RNG          !! 'PcgMcg32RNG' object
    tSInt32,            INTENT(IN)      :: Seed(:)      !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: IntSeed(2)
    tSInt64     :: LngSeed

! FLOW

    ! fill 32-bit integer seeds
    CALL Fill_State(Seed, IntSeed)
    
    ! get 64-bit integer seeds
    LngSeed = IOR(IAND(ToInt64(IntSeed(1)), MaskL), SHIFTL(ToInt64(IntSeed(2)), 32)) &
              + GOLDEN_RATIO_64

    ! initialize with a 64-bit integer seed
    CALL RNG%InitLongSeeds(LngSeed)

    RETURN

END SUBROUTINE PcgMcg32RNG_BaseInit

!******************************************************************************

SUBROUTINE PcgMcg32RNG_InitLongSeeds(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'PcgMcg32RNG' object with a 64-bit integer seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'PcgMcg32RNG' object
    CLASS(PcgMcg32RNG), INTENT(INOUT)   :: RNG
    !% long seed
    tSInt64,            INTENT(IN)      :: Seed

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set pointer to transformation function
    IF (RNG%UseRandShift) THEN
        RNG%Permute => Transform_Shift
    ELSE
        RNG%Permute => Transform_Rotate
    END IF

    ! A seed of zero will result in a non-functional MCG; it must be odd for a maximal
    ! period MCG. The multiplication factor always sets the 2 least-significant bits to 1
    ! if they are already 1 so these are explicitly set. Bit k (zero-based) will have
    ! period 2^(k-1) starting from bit 2 with a period of 1. Bit 63 has period 2^62.
    RNG%State = IOR(Seed, 3_kInt64)

    RETURN

END SUBROUTINE PcgMcg32RNG_InitLongSeeds

!******************************************************************************

FUNCTION PcgMcg32RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgMcg32RNG), INTENT(INOUT)   :: RNG      !! 'PcgMcg32RNG' object
    tSInt32                             :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: X

! FLOW
    
    X = RNG%State
    RNG%State = Bump(RNG, RNG%State)
    RandNum = RNG%Permute(X)

    RETURN

END FUNCTION PcgMcg32RNG_NextInteger

!******************************************************************************

FUNCTION PcgMcg32RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgMcg32RNG), INTENT(IN)  :: RNG      !! 'PcgMcg32RNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (RNG%UseRandShift) THEN
        Name = 'PcgMcg32_XSH_RS_RNG'
    ELSE
        Name = 'PcgMcg32_XSH_RR_RNG'
    END IF
    
    RETURN

END FUNCTION PcgMcg32RNG_GetName

!******************************************************************************

FUNCTION PcgMcg32RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgMcg32RNG), INTENT(IN)  :: RNG      !! 'PcgMcg32RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 2
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION PcgMcg32RNG_GetSeedSize

!******************************************************************************

FUNCTION Bump(RNG, Input) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To provide the next state of the MCG.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(PcgMcg32RNG), INTENT(IN)  :: RNG      !! 'PcgMcg32RNG' object
    tSInt64,            INTENT(IN)  :: Input    !! current state
    tSInt64                         :: Output   !! next state

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = Input * 6364136223846793005_kInt64
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

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

SUBROUTINE PcgMcg32RNG_Finalization(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the 'PcgMcg32RNG' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(PcgMcg32RNG), INTENT(INOUT)   :: RNG  !! 'PcgMcg32RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    NULLIFY(RNG%Permute)
    
    RETURN

END SUBROUTINE PcgMcg32RNG_Finalization

!******************************************************************************

END MODULE MClass_PcgMcg32RNG
    
!******************************************************************************
