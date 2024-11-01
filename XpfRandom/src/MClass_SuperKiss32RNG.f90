
MODULE MClass_SuperKiss32RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SuperKiss32RNG* type and its related routines.
!   The *SuperKiss32RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *SuperKiss32RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on the 32-bit *SuperKISS* algorithm
!   by George Marsaglia. <br>
!   The 32-bit *SuperKiss* algorithm consists of a combination of three sub-generators: <br>
!   - complementary-multiply-with-carry (CMWC) generator <br>
!   - linear congruential generator, and <br>
!   - xorshift generator. <br>
!   The three generators are updated independently, and their states are combined
!   to form a sequence of 32-bit output words. <br>
!   The *SuperKiss32RNG* type has a memory footprint of 1320608 bits and a period of
!   (5)(2<sup>1320481</sup>)(2<sup>32</sup>-1). <br>
!   It is important to note that the *SuperKiss32* PRNG requires an explicit
!   initialization by first calling the *BaseInit* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/KISS_(algorithm)">
!       KISS (algorithm)</a> <br>
!   [2] <a href="https://groups.google.com/g/sci.math/c/QiTrbq0XeyM/m/_qSRAdMaeK8J">
!       SuperKISS for 32- and 64-bit RNGs in both C and Fortran</a> <br>
!   [3] <a href="http://forums.silverfrost.com/viewtopic.php?p=20716&sid=ef071ed8302541d789d1e08c0fbda580">
!       Help for a 64-bit RNG</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: SuperKiss32RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tIndex,   PARAMETER :: QSIZE = 41265_kIndex
    tSInt32,  PARAMETER :: CMul  = 69609_kInt32
    tSInt32,  PARAMETER :: CAdd  = 123_kInt32

!** DERIVED TYPE DEFINITIONS
    !> The *SuperKiss32RNG* type is an *Integer* PRNG type based on the *SuperKISS*
    !  algorithm by George Marsaglia.
    TYPE, EXTENDS(IntegerRNG)  :: SuperKiss32RNG
        PRIVATE
        !% state of CMWC generator
        tSInt32     :: QState(QSIZE)
        !% index into QState
        tIndex      :: Index = QSIZE + 1_kIndex
        !% carry
        tSInt32     :: Carry = 362_kInt32
        !% state of the linear congruential generator
        tSInt32     :: CState = 1236789_kInt32
        !% state of the xorshift generator
        tSInt32     :: XState = 521288629_kInt32
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *BaseInit* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => SuperKiss32RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => SuperKiss32RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => SuperKiss32RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => SuperKiss32RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE SuperKiss32RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE SuperKiss32RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'SuperKiss32RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SuperKiss32RNG), INTENT(INOUT)    :: RNG      !! 'SuperKiss32RNG' object
    tSInt32,               INTENT(IN)       :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I

! FLOW
    
    ! set initial seed(s)
    SELECT CASE (SIZE(Seed))
    CASE (1)
        RNG%Carry  = IEOR(RNG%Carry,  Seed(1))
    CASE (2)
        RNG%Carry  = IEOR(RNG%Carry,  Seed(1))
        RNG%CState = IEOR(RNG%CState, Seed(2))
    CASE (3)
        RNG%Carry  = IEOR(RNG%Carry,  Seed(1))
        RNG%CState = IEOR(RNG%CState, Seed(2))
        RNG%XState = IEOR(RNG%XState, Seed(3))
    END SELECT
    
    ! fill QState with Congruential + Xorshift
    DO I = 1_kIndex, QSIZE
        ! Congruential generator
        RNG%CState = CMul*RNG%CState + CAdd
        ! Xorshift generator
        RNG%XState = IEOR(RNG%XState, SHIFTL(RNG%XState, 13))
        RNG%XState = IEOR(RNG%XState, SHIFTR(RNG%XState, 17))
        RNG%XState = IEOR(RNG%XState, SHIFTL(RNG%XState, 5))    ! left shift in ref#2 (right in #3)
        ! fill Q
        RNG%QState(I) = RNG%CState + RNG%XState
    END DO

    RETURN

END SUBROUTINE SuperKiss32RNG_BaseInit

!******************************************************************************

FUNCTION SuperKiss32RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 32-bit random integer value.
 
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SuperKiss32RNG), INTENT(INOUT)    :: RNG      !! 'SuperKiss32RNG' object
    tSInt32                                 :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32        :: QState

! FLOW
    
    ! CMWC generator
    IF (RNG%Index <= QSIZE) THEN
        QState = RNG%QState(RNG%Index)
    ELSE
        RNG%Index = 1
        QState = Refill(RNG%QState, RNG%Carry, RNG%Index)
    END IF
    
    ! Congruential generator
    RNG%CState = CMul*RNG%CState + CAdd

    ! Xorshift generator
    RNG%XState = IEOR(RNG%XState, SHIFTL(RNG%XState, 13))
    RNG%XState = IEOR(RNG%XState, SHIFTR(RNG%XState, 17))
    RNG%XState = IEOR(RNG%XState, SHIFTL(RNG%XState, 5))    ! left shift in ref#2 (right in #3)

    ! SuperKISS generator
    RandNum = QState + RNG%CState + RNG%XState

    RETURN

CONTAINS

    FUNCTION Refill(Q, C, ID) RESULT(Q1)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To refill the QState array and return the first QState.
 
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(INOUT) :: Q(:) !! QState array
        tSInt32,  INTENT(INOUT) :: C    !! Carry
        tIndex,   INTENT(INOUT) :: ID   !! Index
        tSInt32                 :: Q1   !! the first QState

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex  :: I
        tSInt32    :: Z, H

    ! FLOW
    
        ! refill QState
        DO I = 1_kIndex, QSIZE
            H = IAND(C, 1_kInt32)
            Z = SHIFTR(SHIFTL(Q(I), 9), 1) + SHIFTR(SHIFTL(Q(I), 7), 1) + SHIFTR(C, 1)
            C = SHIFTR(Q(I), 23) + SHIFTR(Q(I), 25) + SHIFTR(Z, 31)
            Q(I) = NOT(SHIFTL(Z, 1) + H)
        END DO
        
        ! return the first QState
        Q1 = Q(ID)
        
        ! update index
        ID = ID + 1_kIndex
    
        RETURN
    
    END FUNCTION Refill

    !**************************************************************************

END FUNCTION SuperKiss32RNG_NextInteger

!******************************************************************************

FUNCTION SuperKiss32RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SuperKiss32RNG), INTENT(IN)   :: RNG      !! 'SuperKiss32RNG' object
    tCharAlloc                          :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'SuperKiss32RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION SuperKiss32RNG_GetName

!******************************************************************************

FUNCTION SuperKiss32RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SuperKiss32RNG), INTENT(IN)   :: RNG      !! 'SuperKiss32RNG' object
    tIndex                              :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 3_kIndex
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION SuperKiss32RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_SuperKiss32RNG
    
!******************************************************************************
