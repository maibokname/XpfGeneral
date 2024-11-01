
MODULE MClass_Kiss64RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Kiss64RNG* type and its related routines.
!   The *Kiss64RNG* type is an *Long* PRNG type that directly extends
!   the *LongRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Long* PRNG type.  <br>
!   In particular, the *Kiss64RNG* type provides an implementation of the
!   *NextLongImpl* deferred procedure based on the 64-bit *KISS* (Keep it
!   Simple Stupid) algorithm by George Marsaglia. <br>
!   The 64-bit *KISS* algorithm consists of a combination of four sub-generators,
!   each with 64 bits of state, of three kinds: <br>
!   - one linear congruential generator modulo 2<sup>64</sup> <br>
!   - one general binary linear generator over the vector space GF(2)<sup>64</sup> <br>
!   - two multiply-with-carry generators modulo 2<sup>32</sup>, with different
!     parameters <br>
!   The four generators are updated independently, and their states are combined
!   to form a sequence of 64-bit output words with period of about 2<sup>250</sup>. <br>
!   It is important to note that the *Kiss64* PRNG requires an explicit
!   initialization by first calling the *BaseInit* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/KISS_(algorithm)">
!       KISS (algorithm)</a> <br>
!   [2] <a href="http://www0.cs.ucl.ac.uk/staff/d.jones/GoodPracticeRNG.pdf">
!       David Jones.  Good Practice in (Pseudo) Random Number Generation for
!       Bioinformatics Applications.</a> <br>
!   [3] <a href="https://github.com/jenetics/prngine/blob/master/prngine/src/main/java/io/jenetics/prngine/KISS64Random.java">
!       PRNGine - Pseudo Random Number Engines for Monte Carlo simulations: 
!       Class KISS64Random</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_UIntUtil,   ONLY: ToUnsignedLong
    USE MClass_BaseRNG
    USE MClass_LongRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Kiss64RNG

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    !% a mask used for masking 32 upper bits of a 64-bit integer
    tSInt64, PARAMETER, PUBLIC  :: MaskL = ToInt64(Z'00000000FFFFFFFF')

!** DERIVED TYPE DEFINITIONS
    !> The *Kiss64RNG* type is a *Long* PRNG type based on the *KISS*
    !  (Keep it Simple Stupid) algorithm by George Marsaglia.
    TYPE, EXTENDS(LongRNG)  :: Kiss64RNG
        PRIVATE
        !% state of the linear congruential generator
        tSInt64     :: X = 123456789123_kInt64
        !% state of the xor-shift generator
        tSInt64     :: Y = 987654321987_kInt64
        !% states and carries of the multiply-with-carry generators
        tSInt32     :: Z1 = 43219876_kInt32
        tSInt32     :: C1 = 6543217_kInt32
        tSInt32     :: Z2 = 21987643_kInt32
        tSInt32     :: C2 = 1732654_kInt32
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *BaseInit* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit     => Kiss64RNG_BaseInit
        !> *NextLongImpl* is a deferred procedure. <br>
        !  Use the *NextLong* method in place of the *NextLongImpl* method
        !  to generate a 64-bit integer number.
        PROCEDURE       :: NextLongImpl => Kiss64RNG_NextLong
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName      => Kiss64RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize  => Kiss64RNG_GetSeedSize
        ! ---------------------------------------------------------------------
    END TYPE Kiss64RNG

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Kiss64RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the 'Kiss64RNG' object with specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kiss64RNG), INTENT(INOUT)  :: RNG      !! 'Kiss64RNG' object
    tSInt64,          INTENT(IN)     :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: I
    tSInt64     :: Wrk

! FLOW
    
    ! set initial seeds
    SELECT CASE (SIZE(Seed))
    CASE (1)
        RNG%X = Seed(1)
    CASE (2)
        RNG%X = Seed(1)
        RNG%Y = Seed(2)
    CASE (3)
        RNG%X = Seed(1)
        RNG%Y = Seed(2)
        RNG%Z1 = ToInt32(SHIFTR(Seed(3), 32))
        RNG%C1 = ToInt32(IAND(Seed(3), MaskL))
    CASE (4)
        RNG%X = Seed(1)
        RNG%Y = Seed(2)
        RNG%Z1 = ToInt32(SHIFTR(Seed(3), 32))
        RNG%C1 = ToInt32(IAND(Seed(3), MaskL))
        RNG%Z2 = ToInt32(SHIFTR(Seed(4), 32))
        RNG%C2 = ToInt32(IAND(Seed(4), MaskL))
    END SELECT
    
    ! warming up before using the output
    DO I = 1, 20
        Wrk = RNG%NextLong()
    END DO
    
    RETURN

END SUBROUTINE Kiss64RNG_BaseInit

!******************************************************************************

FUNCTION Kiss64RNG_NextLong(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the 64-bit random integer value.
 
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kiss64RNG), INTENT(INOUT) :: RNG      !! 'Kiss64RNG' object
    tSInt64                         :: RandNum  !! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt64, PARAMETER  ::A = 1490024343005336237_kInt64    ! = ToInt64(Z'14ADA13ED78492AD')
    tSInt64, PARAMETER  ::B = 123456789_kInt64
    tSInt64, PARAMETER  ::M1 = 4294584393_kInt64
    tSInt64, PARAMETER  ::M2 = 4246477509_kInt64

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt64     :: T

! FLOW

    ! congruential generator
    RNG%X = A*RNG%X + B
    
    ! xor-shift generator
    RNG%Y = IEOR(RNG%Y, SHIFTL(RNG%Y, 21))
    RNG%Y = IEOR(RNG%Y, SHIFTR(RNG%Y, 17))
    RNG%Y = IEOR(RNG%Y, SHIFTL(RNG%Y, 30))
    
    ! multiply-with-carry generator #1
    T = M1*RNG%Z1 + RNG%C1
    RNG%C1 = ToInt32(SHIFTR(T, 32))   ! get upper 32 bits
    RNG%Z1 = ToInt32(IAND(T, MaskL))  ! get lower 32 bits

    ! multiply-with-carry generator #2
    T = M2*RNG%Z2 + RNG%C2
    RNG%C2 = ToInt32(SHIFTR(T, 32))   ! get upper 32 bits
    RNG%Z2 = ToInt32(IAND(T, MaskL))  ! get lower 32 bits

    ! KISS generator
    RandNum = RNG%X + RNG%Y + RNG%Z1 + SHIFTL(ToUnsignedLong(RNG%Z2), 32)
    
    RETURN

END FUNCTION Kiss64RNG_NextLong

!******************************************************************************

FUNCTION Kiss64RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the name of the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kiss64RNG), INTENT(IN)    :: RNG      !! 'Kiss64RNG' object
    tCharAlloc                      :: Name     !! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Kiss64RNG'
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Kiss64RNG_GetName

!******************************************************************************

FUNCTION Kiss64RNG_GetSeedSize(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kiss64RNG), INTENT(IN)    :: RNG      !! 'Kiss64RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Size = 4
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Kiss64RNG_GetSeedSize

!******************************************************************************

END MODULE MClass_Kiss64RNG
    
!******************************************************************************
