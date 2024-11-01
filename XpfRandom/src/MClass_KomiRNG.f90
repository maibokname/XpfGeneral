
MODULE MClass_KomiRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *KomiRNG* type and its related routines.
!   The *KomiRNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *KomiRNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the KomiRand algorithm by
!   Aleksey Vaneev.  It has a state size of 128-bits and a period of
!   2<sup>64</sup>. <br>
!   It is important to note that the *Komi* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://github.com/avaneev/komihash">KOMIHASH - Very fast, high-quality
!       hash function, discrete-incremental and streamed hashing-capable (non-cryptographic,
!       in C) + PRNG.</a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_UIntUtil, ONLY: U128_Multiply => UMul128
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: KomiRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> The *KomiRNG* type is a *Long* PRNG type based on a simple, but reliable,
    !  self-starting, and fast 64-bit PRNG by Aleksey Vaneev.
    TYPE, EXTENDS(LongRNG)  :: KomiRNG
        PRIVATE
        tSInt64             :: Seed1    ! working seed
        tSInt64             :: Seed2    ! working seed
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => KomiRNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl     => KomiRNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => KomiRNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => KomiRNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE KomiRNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE KomiRNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'KomiRNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KomiRNG), INTENT(INOUT)   :: RNG      !! 'KomiRNG' object
    tSInt64,        INTENT(IN)      :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I
    tSInt64     :: RandNum

! FLOW
    
    ! set initial seed
    RNG%Seed1 = Seed(1)
    IF (SIZE(Seed) > 1) THEN
        RNG%Seed2 = Seed(2)
    ELSE
        RNG%Seed2 = Seed(1)
    END IF

    ! warming up before using the output
    DO I = 1, 5
        RandNum = RNG%NextLong()
    END DO
    
    RETURN

END SUBROUTINE KomiRNG_BaseInit

!******************************************************************************

FUNCTION KomiRNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KomiRNG), INTENT(INOUT)   :: RNG      !! 'KomiRNG' object
    tSInt64                         :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER  ::M = ToInt64(Z'AAAAAAAAAAAAAAAA')
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: OutLo, OutHi

! FLOW
    
    CALL U128_Multiply(RNG%Seed1, RNG%Seed2, OutLo, OutHi)
    RNG%Seed2 = RNG%Seed2 + (OutHi + M)
    RNG%Seed1 = IEOR(RNG%Seed2, OutLo)
    RandNum   = RNG%Seed1
    
    RETURN

END FUNCTION KomiRNG_NextLong

!******************************************************************************

FUNCTION KomiRNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KomiRNG), INTENT(IN)  :: RNG      !! 'KomiRNG' object
    tCharAlloc                  :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'KomiRNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION KomiRNG_GetName

!******************************************************************************

FUNCTION KomiRNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KomiRNG), INTENT(IN)  :: RNG      !! 'KomiRNG' object
    tIndex                      :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 2
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION KomiRNG_GetSeedSize

!******************************************************************************

END MODULE MClass_KomiRNG
    
!******************************************************************************
