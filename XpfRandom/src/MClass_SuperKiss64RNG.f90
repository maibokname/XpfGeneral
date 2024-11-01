
MODULE MClass_SuperKiss64RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SuperKiss64RNG* type and its related routines.
!   The *SuperKiss64RNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *SuperKiss64RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the 64-bit *SuperKISS* algorithm
!   by George Marsaglia. <br>
!   The 64-bit *SuperKiss* algorithm consists of a combination of three sub-generators: <br>
!   - complementary-multiply-with-carry (CMWC) generator <br>
!   - linear congruential generator, and <br>
!   - xorshift generator. <br>
!   The three generators are updated independently, and their states are combined
!   to form a sequence of 64-bit output words. <br>
!   The *SuperKiss64RNG* type has a memory footprint of 1320672 bits and a period of
!   (5)(2<sup>1320480</sup>)(2<sup>64</sup>-1). <br>
!   It is important to note that the *SuperKiss64* PRNG requires an explicit
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
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: SuperKiss64RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    tIndex,  PARAMETER  :: QSIZE = 20632_kIndex
    tSInt64, PARAMETER  :: CMul  = 6906969069_kInt64
    tSInt64, PARAMETER  :: CAdd  = 123_kInt64

!** DERIVED TYPE DEFINITIONS
    !> The *SuperKiss64RNG* type is a *Long* PRNG type based on the *SuperKISS*
    !  algorithm by George Marsaglia.
    TYPE, EXTENDS(LongRNG)  :: SuperKiss64RNG
        PRIVATE
        !% state of CMWC generator
        tSInt64     :: QState(QSIZE)
        !% index into QState
        tIndex      :: Index = QSIZE + 1_kIndex
        !% carry
        tSInt64     :: Carry = 36243678541_kInt64
        !% state of the linear congruential generator
        tSInt64     :: CState = 12367890123456_kInt64
        !% state of the xorshift generator
        tSInt64     :: XState = 521288629546311_kInt64
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *BaseInit* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit     => SuperKiss64RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl => SuperKiss64RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName      => SuperKiss64RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize  => SuperKiss64RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE SuperKiss64RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE SuperKiss64RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'SuperKiss64RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SuperKiss64RNG), INTENT(INOUT)    :: RNG      !! 'SuperKiss64RNG' object
    tSInt64,               INTENT(IN)       :: Seed(:)  !! seed(s)

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
        RNG%XState = IEOR(RNG%XState, SHIFTL(RNG%XState, 43))
        ! fill Q
        RNG%QState(I) = RNG%CState + RNG%XState
    END DO

    RETURN

END SUBROUTINE SuperKiss64RNG_BaseInit

!******************************************************************************

FUNCTION SuperKiss64RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.
 
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SuperKiss64RNG), INTENT(INOUT)    :: RNG      !! 'SuperKiss64RNG' object
    tSInt64                                 :: RandNum  !! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: QState

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
    RNG%XState = IEOR(RNG%XState, SHIFTL(RNG%XState, 43))

    ! SuperKISS generator
    RandNum = QState + RNG%CState + RNG%XState

    RETURN

CONTAINS

    FUNCTION Refill(Q, C, ID) RESULT(Q1)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To refill the QState array and return the first QState.
 
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt64, INTENT(INOUT)  :: Q(:) !! QState array
        tSInt64, INTENT(INOUT)  :: C    !! Carry
        tIndex,  INTENT(INOUT)  :: ID   !! Index
        tSInt64                 :: Q1   !! the first QState

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex  :: I
        tSInt64 :: Z, H

    ! FLOW
    
        ! refill QState
        DO I = 1_kIndex, QSIZE
            H = IAND(C, 1_kInt64)
            Z = SHIFTR(SHIFTL(Q(I), 41), 1) + SHIFTR(SHIFTL(Q(I), 39), 1) + SHIFTR(C, 1)
            C = SHIFTR(Q(I), 23) + SHIFTR(Q(I), 25) + SHIFTR(Z, 63)
            Q(I) = NOT(SHIFTL(Z, 1) + H)
        END DO
        
        ! return the first QState
        Q1 = Q(ID)
        
        ! update index
        ID = ID + 1_kIndex
    
        RETURN
    
    END FUNCTION Refill

    !**************************************************************************

END FUNCTION SuperKiss64RNG_NextLong

!******************************************************************************

FUNCTION SuperKiss64RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SuperKiss64RNG), INTENT(IN)   :: RNG      !! 'SuperKiss64RNG' object
    tCharAlloc                          :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'SuperKiss64RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION SuperKiss64RNG_GetName

!******************************************************************************

FUNCTION SuperKiss64RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SuperKiss64RNG), INTENT(IN)   :: RNG      !! 'SuperKiss64RNG' object
    tIndex                              :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 3_kIndex
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION SuperKiss64RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_SuperKiss64RNG
    
!******************************************************************************
