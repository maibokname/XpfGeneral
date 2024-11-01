
MODULE MClass_Mwc256RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Mwc256RNG* type and its related routines.
!   The *Mwc256RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *Mwc256RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on the *Multiply-With-Carry*
!   (MWC) algorithm by George Marsaglia.  Its memory footprint is 8224 bits
!   and its period is roughly 2<sup>8222</sup>.
!   It is important to note that the *MWC* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/Multiply-with-carry_pseudorandom_number_generator">
!       Multiply-with-carry pseudorandom number generator</a> <br>
!   [2] <a href="https://crypto.stackexchange.com/questions/10359/what-stops-the-multiply-with-carry-rng-from-being-a-cryptographically-secure-prn">
!       What stops the Multiply-With-Carry RNG from being a Cryptographically Secure PRNG?</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/MultiplyWithCarry256.html">
!       Apache Commons RNG: Class MultiplyWithCarry256</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Mwc256RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! Length of the state array
    tIndex, PARAMETER   :: Q_SIZE = 256_kIndex
    ! Multiply
    tSInt64, PARAMETER  ::A = 809430660_kInt64

!** DERIVED TYPE DEFINITIONS
    !> The *Mwc256RNG* type is an *Integer* PRNG type based on the *MWC*
    !  (Multiply-With-Carry) algorithm by George Marsaglia.
    TYPE, EXTENDS(IntegerRNG)  :: Mwc256RNG
        PRIVATE
        ! the working states
        tSInt32     :: State(0:Q_SIZE-1)
        ! current index in "state" array
        tSInt32     :: Index
        ! carry
        tSInt32     :: Carry
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => Mwc256RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => Mwc256RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => Mwc256RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => Mwc256RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE Mwc256RNG

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Mwc256RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Mwc256RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mwc256RNG), INTENT(INOUT) :: RNG      !! 'Mwc256RNG' object
    tSInt32,          INTENT(IN)    :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Seed0(0:Q_SIZE)

! FLOW

    ! set initial seed
    CALL Fill_State(Seed, Seed0)

    ! initial state
    RNG%State(0:Q_SIZE-1) = Seed0(0:Q_SIZE-1)

    ! last element of the "seed" is the initial "carry"
    ! Marsaglia's recommendation: 0 <= carry < A
    RNG%Carry = ToInt32(MOD(ABS(ToInt64(Seed0(Q_SIZE))), A))

    ! initial index
    RNG%Index = Q_SIZE

    RETURN

END SUBROUTINE Mwc256RNG_BaseInit

!******************************************************************************

FUNCTION Mwc256RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mwc256RNG), INTENT(INOUT) :: RNG      !! 'Mwc256RNG' object
    tSInt32                         :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,  PARAMETER :: MaskB = ToInt32(Z'000000FF')
    tSInt64,  PARAMETER :: MaskL = ToInt64(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: T

! FLOW

    ! produce an index in the range 0-255
    RNG%Index = IAND(RNG%Index, MaskB)

    ! compute next random number
    T = A * IAND(ToInt64(RNG%State(RNG%Index)), MaskL) + RNG%Carry
    RandNum = ToInt32(T)

    ! update state, carry and index
    RNG%Carry = ToInt32(SHIFTR(T, 32))
    RNG%State(RNG%Index) = RandNum
    RNG%Index = RNG%Index + 1

    RETURN

END FUNCTION Mwc256RNG_NextInteger

!******************************************************************************

FUNCTION Mwc256RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mwc256RNG), INTENT(IN)    :: RNG      !! 'Mwc256RNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'Mwc256RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Mwc256RNG_GetName

!******************************************************************************

FUNCTION Mwc256RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Mwc256RNG), INTENT(IN)    :: RNG      !! 'Mwc256RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = Q_SIZE + 1_kIndex
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Mwc256RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_Mwc256RNG

!******************************************************************************
