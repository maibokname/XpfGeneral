
MODULE MClass_Cmwc4096RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Cmwc4096RNG* type and its related routines.
!   The *Cmwc4096RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *Cmwc4096RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on the *Complimentary-Multiply-With-Carry*
!   (CMWC) algorithm by George Marsaglia.  Its memory footprint is 131104 bits
!   and its period is roughly 2<sup>131104</sup>.
!   It is important to note that the *CMWC* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/Multiply-with-carry_pseudorandom_number_generator">
!       Multiply-with-carry pseudorandom number generator</a> <br>
!   [2] <a href="https://crypto.stackexchange.com/questions/10359/what-stops-the-multiply-with-carry-rng-from-being-a-cryptographically-secure-prn">
!       What stops the Multiply-With-Carry RNG from being a Cryptographically Secure PRNG?</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_UIntUtil,   ONLY: OPERATOR(.ULT.), ToUnsignedLong
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Cmwc4096RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! Length of the state array
    tIndex,   PARAMETER :: Q_SIZE = 4096_kIndex
    ! Multiply
    tSInt64,  PARAMETER :: C_MAX = 809430660_kInt64
    ! Multiply
    tSInt64,  PARAMETER :: A = 18782_kInt64
    ! Complimentary
    tSInt32,  PARAMETER :: R = ToInt32(Z'FFFFFFFE')

!** DERIVED TYPE DEFINITIONS
    !> The *Cmwc4096RNG* type is an *Integer* PRNG type based on the *CMWC*
    !  (Complimentary-Multiply-With-Carry) algorithm by George Marsaglia.
    TYPE, EXTENDS(IntegerRNG)  :: Cmwc4096RNG
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
        PROCEDURE       :: BaseInit         => Cmwc4096RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => Cmwc4096RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => Cmwc4096RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => Cmwc4096RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE Cmwc4096RNG

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Cmwc4096RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Cmwc4096RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Cmwc4096RNG), INTENT(INOUT)   :: RNG      !! 'Cmwc4096RNG' object
    tSInt32,            INTENT(IN)      :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: Seed0(0:Q_SIZE)

! FLOW

    ! set initial seed
    CALL Fill_State(Seed, Seed0)

    ! initial state
    RNG%State(0:Q_SIZE-1) = Seed0(0:Q_SIZE-1)
    
    ! last element of the "seed" is the initial "carry"
    ! Marsaglia's recommendation: 0 <= carry < C_MAX
    RNG%Carry = ToInt32(MOD(ABS(ToUnsignedLong(Seed0(Q_SIZE))), C_MAX))

    ! initial index
    RNG%Index = Q_SIZE

    RETURN

END SUBROUTINE Cmwc4096RNG_BaseInit

!******************************************************************************

FUNCTION Cmwc4096RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Cmwc4096RNG), INTENT(INOUT)   :: RNG      !! 'Cmwc4096RNG' object
    tSInt32                             :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,  PARAMETER :: MaskB = ToInt32(Z'00000FFF')
    tSInt64,  PARAMETER :: MaskL = ToInt64(Z'00000000FFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: T
    tSInt32     :: X

! FLOW

    ! produce an index in the range 0-4095
    RNG%Index = IAND(RNG%Index, MaskB)

    ! compute intermediate variables and update carry
    ! where Carry = T / MaskL and X = MOD(T, MaskL)
    T = A * IAND(ToUnsignedLong(RNG%State(RNG%Index)), MaskL) + RNG%Carry
    RNG%Carry = ToInt32(SHIFTR(T, 32))
    X = ToInt32(T + RNG%Carry)
    IF (X .ULT. RNG%Carry) THEN
        X = X + 1
        RNG%Carry = RNG%Carry + 1
    END IF

    ! compute next random number
    RandNum = R - X

    ! update state  and index
    RNG%State(RNG%Index) = RandNum
    RNG%Index = RNG%Index + 1

    RETURN

END FUNCTION Cmwc4096RNG_NextInteger

!******************************************************************************

FUNCTION Cmwc4096RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Cmwc4096RNG), INTENT(IN)  :: RNG      !! 'Cmwc4096RNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'Cmwc4096RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Cmwc4096RNG_GetName

!******************************************************************************

FUNCTION Cmwc4096RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Cmwc4096RNG), INTENT(IN)  :: RNG      !! 'Cmwc4096RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = Q_SIZE + 1_kIndex
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Cmwc4096RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_Cmwc4096RNG

!******************************************************************************
