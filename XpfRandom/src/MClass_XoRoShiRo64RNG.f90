
MODULE MClass_XoRoShiRo64RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *XoRoShiRo64RNG* type and its related routines.
!   The *XoRoShiRo64RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *XoRoShiRo64RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on algorithms from the Xor-Shift-Rotate
!   family of generators by Sebastiano Vigna. <br>
!   The *XoRoShiRo64RNG* type can represent two PRNG classes: <br>
!   - the XoRoShiRo64StarStar RNG, which is a fast all-purpose 32-bit generator, or <br>
!   - the XoRoShiRo64Star RNG, which is a fast 32-bit generator suitable for
!     real number generation. <br>
!   By default, the *XoRoShiRo64StarStar* RNG is employed.  However, the *XoRoShiRo64Star*
!   RNG can be utilized by specifying an algorithm flag to true when initializing the
!   generator.  The *XoRoShiRo64RNG* type has state size of 64 bits. <br>
!   It is important to note that the *XoRoShiRo64* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://prng.di.unimi.it/">
!       Xoshiro/Xoroshiro Generators and the PRNG shootout.</a> <br>
!   [2] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/XoRoShiRo64StarStar.html">
!       Apache Commons RNG: Class XoRoShiRo64StarStar</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/XoRoShiRo64Star.html">
!       Apache Commons RNG: Class XoRoShiRo64Star</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: XoRoShiRo64RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! Size of the state vector
    tSInt32,  PARAMETER :: SEED_SIZE = 2
    ! multiplier
    tSInt32,  PARAMETER :: MPar = ToInt32(Z'9E3779BB')

!** DERIVED TYPE DEFINITIONS
    !> The *XoRoShiRo64RNG* type is an *Integer* PRNG type based on algorithms from
    !  the Xor-Shift-Rotate family of generators by Sebastiano Vigna.
    TYPE, EXTENDS(IntegerRNG)  :: XoRoShiRo64RNG
        PRIVATE
        ! states
        tSInt32     :: State0, State1
        ! algorithm flag
        tLogical    :: UseXRSR64Star = FalseVal
        ! function to return the next integer output
        PROCEDURE(Next), POINTER    :: NextOutput => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: InitWithFlag     => XoRoShiRo64RNG_InitWithFlag
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => XoRoShiRo64RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => XoRoShiRo64RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => XoRoShiRo64RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => XoRoShiRo64RNG_GetSeedSize
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
        FINAL           :: XoRoShiRo64RNG_Finalization
        ! ---------------------------------------------------------------------
    END TYPE XoRoShiRo64RNG

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Next(RNG) RESULT(Output)
            IMPORT
            CLASS(XoRoShiRo64RNG), INTENT(IN)   :: RNG
            tSInt32                             :: Output
        END FUNCTION Next
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE XoRoShiRo64RNG_InitWithFlag(RNG, UseXRSR64Star, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified algorithm flag.  Optionally,
    !  initial seeds can be specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'XoRoShiRo64RNG' object
    CLASS(XoRoShiRo64RNG), INTENT(INOUT)    :: RNG
    !> algorithm flag <br>
    ! - true, use XoRoShiRo64Star algorithm. <br>
    ! - false, use XoRoShiRo64StarStar algorithm.
    tLogical,              INTENT(IN)       :: UseXRSR64Star
    !% optional 32-bit integer seed(s)
    tSInt32,  OPTIONAL,    INTENT(IN)       :: Seed(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set algorithm flag
    RNG%UseXRSR64Star = UseXRSR64Star

    IF (PRESENT(Seed)) THEN
        ! initialize the generator with seed(s)
        CALL RNG%Initialize(Seed)
    ELSE
        ! initialize the generator without seed
        CALL RNG%Initialize()
    END IF

    RETURN

END SUBROUTINE XoRoShiRo64RNG_InitWithFlag

!******************************************************************************

SUBROUTINE XoRoShiRo64RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with specified 32-bit integer seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo64RNG), INTENT(INOUT)    :: RNG      !! 'XoRoShiRo64RNG' object
    tSInt32,               INTENT(IN)       :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Seed0(0:1)

! FLOW

    ! set initial seed
    CALL Fill_State(Seed, Seed0)
    
    ! copy seeds to states
    RNG%State0 = Seed0(0)
    RNG%State1 = Seed0(1)
    
    ! set pointer to next output function
    IF (RNG%UseXRSR64Star) THEN
        RNG%NextOutput => XoRoShiRoStar_Next
    ELSE
        RNG%NextOutput => XoRoShiRoStarStar_Next 
    END IF

    RETURN

END SUBROUTINE XoRoShiRo64RNG_BaseInit

!******************************************************************************

FUNCTION XoRoShiRo64RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo64RNG), INTENT(INOUT)    :: RNG      !! 'XoRoShiRo64RNG' object
    tSInt32                                 :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: S0, S1

! FLOW
    
    ! get next integer output
    RandNum = RNG%NextOutput()

    ! update states
    S0 = RNG%State0
    S1 = RNG%State1

    S1 = IEOR(S1, S0)
    RNG%State0 = IEOR(IEOR(RotateLeft(S0, 26), S1), SHIFTL(S1, 9))  ! a, b
    RNG%State1 = RotateLeft(S1, 13)                                 ! c

    RETURN

END FUNCTION XoRoShiRo64RNG_NextInteger

!******************************************************************************

FUNCTION XoRoShiRo64RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo64RNG), INTENT(IN)   :: RNG      !! 'XoRoShiRo64RNG' object
    tCharAlloc                          :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (RNG%UseXRSR64Star) THEN
        Name = 'XoRoShiRo128StarRNG'
    ELSE
        Name = 'XoRoShiRo128StarStarRNG'
    END IF
    
    RETURN

END FUNCTION XoRoShiRo64RNG_GetName

!******************************************************************************

FUNCTION XoRoShiRo64RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo64RNG), INTENT(IN)   :: RNG      !! 'XoRoShiRo64RNG' object
    tIndex                              :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 2
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION XoRoShiRo64RNG_GetSeedSize

!******************************************************************************

FUNCTION XoRoShiRoStarStar_Next(RNG) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoRoShiRo64StarStar generator, which is a fast all-purpose
    !  32-bit generator with memory footprint of 64 bits.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo64RNG), INTENT(IN)   :: RNG      !! 'XoRoShiRo64RNG' object
    tSInt32                             :: Output   !! 32-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Output = RotateLeft(RNG%State0*MPar, 5) * 5
    
    RETURN

END FUNCTION XoRoShiRoStarStar_Next

!******************************************************************************

FUNCTION XoRoShiRoStar_Next(RNG) RESULT(Output)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the next integer output based on the current states of
    !  the XoRoShiRo64Star generator, which is a fast 32-bit generator
    !  suitable for floating-point-number generation.

    IMPLICIT NONE
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XoRoShiRo64RNG), INTENT(IN)   :: RNG      !! 'XoRoShiRo64RNG' object
    tSInt32                             :: Output   !! 32-bit output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Output = RNG%State0*MPar
    
    RETURN

END FUNCTION XoRoShiRoStar_Next

!******************************************************************************

SUBROUTINE XoRoShiRo64RNG_Finalization(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the 'XoRoShiRo64RNG' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(XoRoShiRo64RNG), INTENT(INOUT)   :: RNG  !! 'XoRoShiRo64RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    NULLIFY(RNG%NextOutput)
    
    RETURN

END SUBROUTINE XoRoShiRo64RNG_Finalization

!******************************************************************************

END MODULE MClass_XoRoShiRo64RNG
    
!******************************************************************************
