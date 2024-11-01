
MODULE MClass_WyRNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *WyRNG* type and its related routines.
!   The *WyRNG* type is a *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by a *Long* PRNG type.  <br>
!   In particular, the *WyRNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on a combination of WyRand
!   and WyHash64 algorithms by Wang Yi.  It has a state size of 128-bits. <br>
!   It is important to note that the *Wy* PRNG requires an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://github.com/wangyi-fudan/wyhash">WYHASH and WYRAND - The FASTEST
!       QUALITY hash function, random number generators (PRNG) and hash map.</a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_UIntUtil,   ONLY: U128_Multiply => UMul128
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: WyRNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! M and N parameters for "wyhash_final_version_3"
!    tSInt64, PARAMETER  ::M = ToInt64(Z'A0761D6478BD642F')
!    tSInt64, PARAMETER  ::N = ToInt64(Z'E7037ED1A0B428DB')
    ! M and N parameters for "wyhash_final_version_4_2"
    tSInt64, PARAMETER  ::M = ToInt64(Z'2D358DCCAA6C78A5')
    tSInt64, PARAMETER  ::N = ToInt64(Z'8BB84B93962EACC9')

!** DERIVED TYPE DEFINITIONS
    !> The *WyRNG* type is a *Long* PRNG type based on a combination of
    ! WyRand and WyHash64 algorithms by Wang Yi.
    TYPE, EXTENDS(LongRNG)  :: WyRNG
        PRIVATE
        ! tSInt64     :: State1 = ToInt64(Z'8EBC6AF09C88C6E3')
        ! tSInt64     :: State2 = ToInt64(Z'589965CC75374CC')
        tSInt64     :: State1 = ToInt64(Z'4B33A62ED433D4A3')
        tSInt64     :: State2 = ToInt64(Z'4D5A2DA51DE1AA47')
        tSInt32     :: Counter = 0
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => WyRNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl     => WyRNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => WyRNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => WyRNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE WyRNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE WyRNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'WyRNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WyRNG), INTENT(INOUT) :: RNG      !! 'WyRNG' object
    tSInt64,      INTENT(IN)    :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I
    tSInt64     :: RandNum

! FLOW
    
    ! set initial seed
    SELECT CASE (SIZE(Seed))
    CASE (1)
        RNG%State1 = IEOR(RNG%State1, Seed(1))
    CASE (2)
        RNG%State1 = IEOR(RNG%State1, Seed(1))
        RNG%State2 = IEOR(RNG%State2, Seed(2))
    END SELECT
    
    ! warming up before using the output
    DO I = 1, 20
        RandNum = RNG%NextLong()
    END DO
    
    RETURN

END SUBROUTINE WyRNG_BaseInit

!******************************************************************************

FUNCTION WyRNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WyRNG), INTENT(INOUT) :: RNG      !! 'WyRNG' object
    tSInt64                     :: RandNum  !! random number
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! update counter
    RNG%Counter = RNG%Counter + 1
    
    ! update states and compute random number based on counter
    SELECT CASE (RNG%Counter)
    CASE (1)
        RandNum = WyHash64(RNG%State1, RNG%State2)
    CASE (2)
        RandNum = WyRand(RNG%State1)
    CASE (3)
        RandNum = WyHash64(RNG%State2, RNG%State1)
    CASE (4)
        RandNum = WyRand(RNG%State2)
        ! wrap around
        RNG%Counter = 0
    END SELECT
    
    RETURN

END FUNCTION WyRNG_NextLong

!******************************************************************************

FUNCTION WyRNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WyRNG), INTENT(IN)    :: RNG      !! 'WyRNG' object
    tCharAlloc                  :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'WyRNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION WyRNG_GetName

!******************************************************************************

FUNCTION WyRNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WyRNG), INTENT(IN)    :: RNG      !! 'WyRNG' object
    tIndex                      :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 2
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION WyRNG_GetSeedSize

!******************************************************************************

FUNCTION WyRand(Seed) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To pseudo random number based on the 'WyRand' algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(INOUT)  :: Seed
    tUInt64                 :: RandNum
    
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! update Seed
    Seed = Seed + M
    
    ! perform multiplication and mixing
    RandNum  = WyMix(Seed, IEOR(Seed, N))
    
    RETURN

END FUNCTION WyRand

!******************************************************************************

FUNCTION WyHash64(A, B) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return deterministic pseudo random numbers that can pass BigCrush and PractRand.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(INOUT)  :: A, B
    tUInt64                 :: RandNum

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! update A and B
    A = IEOR(A, M)
    B = IEOR(B, N)
    
    ! perform 128-bit multiplication (WyMum)
    CALL WyMum(A, B)
    
    ! perform multiplication and mixing
    RandNum  = WyMix(IEOR(A, M), IEOR(B, N))

    RETURN

END FUNCTION WyHash64

!******************************************************************************

FUNCTION WyMix(A, B) RESULT(MixNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication and mixing of the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(IN) :: A, B
    tUInt64             :: MixNum

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: OutLo, OutHi

! FLOW

    ! get input
    OutLo = A
    OutHi = B
    
    ! perform 128-bit multiplication (WyMum)
    CALL WyMum(OutLo, OutHi)
    
    ! mixing the output
    MixNum  = IEOR(OutLo, OutHi)
    
    RETURN

END FUNCTION WyMix

!******************************************************************************

SUBROUTINE WyMum(A, B)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform multiplication of the specified input.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt64, INTENT(INOUT)  :: A, B

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: OutLo, OutHi

! FLOW

    ! perform 128-bit multiplication (WyMum)
    CALL U128_Multiply(A, B, OutLo, OutHi)
    ! note: the next two statements are for WYHASH_CONDOM > 1
    ! -> extra protection against entropy loss (probability=2^-63),
    !    aka. "blind multiplication"
    A = IEOR(A, OutLo)
    B = IEOR(B, OutHi)
    
    RETURN

END SUBROUTINE WyMum

!******************************************************************************

END MODULE MClass_WyRNG
    
!******************************************************************************
