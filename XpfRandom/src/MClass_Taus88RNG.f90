
MODULE MClass_Taus88RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Taus88RNG* type and its related routines.
!   The *Taus88RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *Taus88RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on Tausworthe random number
!   generators by Pierre L'Ecuyer. <br>
!   The *TAUS88* PRNG has three 32-bit states combined by a bitwise xor.
!   Its period length is approximatively 2<sup>88</sup>. <br>
!   It is important to note that the *TAUS88* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  Also, it should be noted that the implementation of
!   this PRNG is based on reference #2.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.ams.org/journals/mcom/1996-65-213/S0025-5718-96-00696-5/">
!       L'Ecuyer, P. 1996. Maximally equidistributed combined Tausworthe
!       generators. Mathematics of Computation, 65(213): 203-213. </a> <br>
!   [2] <a href="https://wp.csiro.au/alanmiller/random/taus88.f90">
!       Ecuyer_random: a Fortran 90 module implementation of the TAUS88
!       generator by Alan Miller. </a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Taus88RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *Taus88RNG* type is an *Integer* PRNG type based on a Tausworthe
    !  random number generator by Pierre L'Ecuyer.
    TYPE, EXTENDS(IntegerRNG)  :: Taus88RNG
        PRIVATE
        tSInt32     :: State1 =  1234
        tSInt32     :: State2 = -4567
        tSInt32     :: State3 =  7890
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => Taus88RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => Taus88RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => Taus88RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => Taus88RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE Taus88RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

SUBROUTINE Taus88RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Taus88RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Taus88RNG), INTENT(INOUT) :: RNG      !! 'Taus88RNG' object
    tSInt32,          INTENT(IN)    :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! set initial seeds
    SELECT CASE (SIZE(Seed))
    CASE (1)
        RNG%State1 = Seed(1)
    CASE (2)
        RNG%State1 = Seed(1)
        RNG%State2 = Seed(2)
    CASE (3)
        RNG%State1 = Seed(1)
        RNG%State2 = Seed(2)
        RNG%State3 = Seed(3)
    END SELECT
    
    IF (IAND(RNG%State1,  -2) == 0) RNG%State1 = RNG%State1 - 1023
    IF (IAND(RNG%State2,  -8) == 0) RNG%State2 = RNG%State2 - 1023
    IF (IAND(RNG%State3, -16) == 0) RNG%State3 = RNG%State3 - 1023

    RETURN

END SUBROUTINE Taus88RNG_BaseInit

!******************************************************************************

FUNCTION Taus88RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Taus88RNG), INTENT(INOUT) :: RNG      !! 'Taus88RNG' object
    tSInt32                         :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: B

! FLOW
    
    B          = SHIFTR(IEOR(SHIFTL(RNG%State1, 13), RNG%State1), 19)
    RNG%State1 = IEOR(SHIFTL(IAND(RNG%State1, -2), 12), B)
    B          = SHIFTR(IEOR(SHIFTL(RNG%State2, 2), RNG%State2), 25)
    RNG%State2 = IEOR(SHIFTL(IAND(RNG%State2, -8), 4), B)
    B          = SHIFTR(IEOR(SHIFTL(RNG%State3, 3), RNG%State3), 11)
    RNG%State3 = IEOR(SHIFTL(IAND(RNG%State3, -16), 17), B)
    
    RandNum = IEOR(IEOR(RNG%State1, RNG%State2), RNG%State3)
    
    RETURN

END FUNCTION Taus88RNG_NextInteger

!******************************************************************************

FUNCTION Taus88RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Taus88RNG), INTENT(IN)    :: RNG      !! 'Taus88RNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Taus88RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Taus88RNG_GetName

!******************************************************************************

FUNCTION Taus88RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Taus88RNG), INTENT(IN)    :: RNG      !! 'Taus88RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 3
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Taus88RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_Taus88RNG
    
!******************************************************************************
