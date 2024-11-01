
MODULE MClass_Well32RNG

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Well32RNG* type and its related routines.
!   The *Well32RNG* type is an *Integer* PRNG type that directly extends
!   the *IntegerRNG* type.  Therefore, it provides all remaining deferred
!   procedures required by an *Integer* PRNG type.  <br>
!   In particular, the *Well32RNG* type provides an implementation of the
!   *NextIntegerImpl* deferred procedure based on the Well Equidistributed
!   Long-period Linear (WELL) algorithms by Francois Panneton, Pierre L'Ecuyer,
!   and Makoto Matsumoto. <br>
!   The *Well32RNG* type can represent six PRNG classes: <br>
!   - a WELL512a PRNG, <br>
!   - a WELL1024a PRNG, <br>
!   - a WELL19937a PRNG, <br>
!   - a WELL19937c PRNG, <br>
!   - a WELL49937a PRNG, or <br>
!   - a WELL49937b PRNG <br>
!   where the numbers in the PRNG names give their state size in bits and the
!   letter suffixes denote variants of the same size. <br>
!   By default, The *WELL49937b* PRNG is employed.  However, other Well32 PRNG
!   variants can be utilized by specifying an algorithm flag (between 1 to 6)
!   when initializing the generator.  <br>
!   It is important to note that the *Well32* PRNGs require an explicit
!   initialization by first calling the *Initialize* method before using
!   any other methods.  Otherwise, the generator may produce undesirable
!   random sequences.  However, as exceptions, one of the *GetSeedSize*
!   methods can be called  before the *Initialize* method to inquire the
!   size of seeds required by a specific PRNG.  <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://dl.acm.org/doi/10.1145/1132973.1132974">
!       F. Panneton, P. L'Ecuyer, and M. Matsumoto.  2006.  Improved Long-Period
!       Generators Based on Linear Recurrences Modulo 2.  ACM Transactions on
!       Mathematical Software, 32(1): 1-16.</a> <br>
!   [2] <a href="https://en.wikipedia.org/wiki/Well_equidistributed_long-period_linear">
!       Well equidistributed long-period linear PRNG</a> <br>
!   [3] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/AbstractWell.html">
!       Apache Commons RNG: Class AbstractWell</a> <br>
!   [4] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/Well512a.html">
!       Apache Commons RNG: Class Well512a</a> <br>
!   [5] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/Well1024a.html">
!       Apache Commons RNG: Class Well1024a</a> <br>
!   [6] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/Well19937a.html">
!       Apache Commons RNG: Class Well19937a</a> <br>
!   [7] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/Well19937c.html">
!       Apache Commons RNG: Class Well19937c</a> <br>
!   [8] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/Well44497a.html">
!       Apache Commons RNG: Class Well44497a</a> <br>
!   [9] <a href="https://commons.apache.org/proper/commons-rng/commons-rng-core/apidocs/org/apache/commons/rng/core/source32/Well44497b.html">
!       Apache Commons RNG: Class Well44497b</a>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MemHandlers,  ONLY: MemAlloc, MemFree
    USE MClass_BaseRNG
    USE MClass_IntegerRNG

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Well32RNG
    PUBLIC :: CalculateBlockCount

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! Block size
    tSInt32,  PARAMETER :: BLOCK_SIZE = 32
    ! parameters for WELL512a
    tSInt32,  PARAMETER :: K_512  = 512
    tSInt32,  PARAMETER :: M1_512 = 13
    tSInt32,  PARAMETER :: M2_512 = 9
    tSInt32,  PARAMETER :: M3_512 = 5
    ! parameters for WELL1024a
    tSInt32,  PARAMETER :: K_1024  = 1024
    tSInt32,  PARAMETER :: M1_1024 = 3
    tSInt32,  PARAMETER :: M2_1024 = 24
    tSInt32,  PARAMETER :: M3_1024 = 10
    ! parameters for WELL19937a and WELL19937c
    tSInt32,  PARAMETER :: K_19937  = 19937
    tSInt32,  PARAMETER :: M1_19937 = 70
    tSInt32,  PARAMETER :: M2_19937 = 179
    tSInt32,  PARAMETER :: M3_19937 = 449
    ! parameters for WELL49937a and WELL49937b
    tSInt32,  PARAMETER :: K_49937  = 49937
    tSInt32,  PARAMETER :: M1_49937 = 23
    tSInt32,  PARAMETER :: M2_49937 = 481
    tSInt32,  PARAMETER :: M3_49937 = 229
    ! parameters for algorithm flag
    tSInt32,  PARAMETER, PUBLIC :: WELL512a   = 1   !! flag for Well512a PRNG
    tSInt32,  PARAMETER, PUBLIC :: WELL1024a  = 2   !! flag for Well1024a PRNG
    tSInt32,  PARAMETER, PUBLIC :: WELL19937a = 3   !! flag for Well19937a PRNG
    tSInt32,  PARAMETER, PUBLIC :: WELL19937c = 4   !! flag for Well19937c PRNG
    tSInt32,  PARAMETER, PUBLIC :: WELL49937a = 5   !! flag for Well49937a PRNG
    tSInt32,  PARAMETER, PUBLIC :: WELL49937b = 6   !! flag for Well49937b PRNG

!** DERIVED TYPE DEFINITIONS
    !> The *Well32RNG* type is an *Integer* PRNG type based on the Well
    !  Equidistributed Long-period Linear (WELL) generators by Francois
    !  Panneton, Pierre L'Ecuyer, and Makoto Matsumoto.
    TYPE, EXTENDS(IntegerRNG)  :: Well32RNG
        PRIVATE
        ! Current index in the bytes pool
        tSInt32                     :: Index
        ! Bytes pool
        tSInt32,     ALLOCATABLE    :: V(:)
        ! Index indirection table giving for each index its predecessor taking table size into account.
        tSInt32,     ALLOCATABLE    :: IRm1(:)
        ! Index indirection table giving for each index its second predecessor taking table size into account.
        tSInt32,     ALLOCATABLE    :: IRm2(:)
        ! Index indirection table giving for each index the value index + m1 taking table size into account.
        tSInt32,     ALLOCATABLE    :: I1(:)
        ! Index indirection table giving for each index the value index + m2 taking table size into account.
        tSInt32,     ALLOCATABLE    :: I2(:)
        ! Index indirection table giving for each index the value index + m3 taking table size into account.
        tSInt32,     ALLOCATABLE    :: I3(:)
        ! algorithm flag
        tSInt32                     :: WellAlgo = WELL49937b
        ! function to return the next integer output
        PROCEDURE(Next), POINTER    :: NextOutput => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Private Procedures                        -----
        ! ---------------------------------------------------------------------
        ! initialization procedures
        PROCEDURE, PRIVATE  :: InitWithFlag => WELL32RNG_InitWithFlag
        ! auxiliary procedures
        PROCEDURE, PRIVATE  :: GetIndexPred
        PROCEDURE, PRIVATE  :: GetIndexPred2
        PROCEDURE, PRIVATE  :: GetIndexM1
        PROCEDURE, PRIVATE  :: GetIndexM2
        PROCEDURE, PRIVATE  :: GetIndexM3
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures                       -----
        ! ---------------------------------------------------------------------
        !> *BaseInit* is a deferred procedure intended to be used internally. <br>
        !  Use the *Initialize* method to initialize the PRNG instead.
        PROCEDURE       :: BaseInit         => Well32RNG_BaseInit
        !> *NextIntegerImpl* is a deferred procedure. <br>
        !  Use the *NextInteger* method in place of the *NextIntegerImpl* method
        !  to generate a 32-bit integer number.
        PROCEDURE       :: NextIntegerImpl  => Well32RNG_NextInteger
        !> *GetName* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the PRNG. <br>
        !  **Usage**: <br>
        !   --->    Name = PRNG%GetName()
        PROCEDURE       :: GetName          => Well32RNG_GetName
        !> *GetSeedSize* is a deferred procedure. <br>
        !  **Type-Bound Function**: GetSeedSize <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator for the default algorithm (WELL49937b). <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize()
        PROCEDURE       :: GetSeedSize      => Well32RNG_GetSeedSize_I
        !> **Type-Bound Function**: GetSeedSize2 <br>
        !  **Purpose**:  To get size of specified seed(s) needed to initialize
        !                the generator for a specified algorithm. <br>
        !  **Usage**: <br>
        !   --->    SeedSize = PRNG%GetSeedSize2(WELL1024a)
        PROCEDURE       :: GetSeedSize2     => Well32RNG_GetSeedSize_II
        ! ---------------------------------------------------------------------
        ! -----               Generic Interface                           -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: Initialize <br>
        !  **Purpose**:  To initialize the pseudo-random number generator. <br>
        !  **Usage**: <br>
        !   ! initialize without seed; use WELL49937b (default) algorithm <br>
        !   --->    CALL RNG%Initialize() <br>
        !   ! initialize with seed(s); use WELL49937b (default) algorithm <br>
        !   --->    CALL RNG%Initialize(Seeds) <br>
        !   ! initialize without seed; use WELL512a algorithm <br>
        !   --->    CALL RNG%Initialize(WELL512a) <br>
        !   ! initialize with seed(s); use WELL19937c algorithm <br>
        !   --->    CALL RNG%Initialize(WELL19937c, Seeds)
        GENERIC         :: Initialize       => InitWithFlag
        ! ---------------------------------------------------------------------
        ! -----             Final Procedure                               -----
        ! ---------------------------------------------------------------------
        !> To perform finalization of the PRNG.
        FINAL           :: Well32RNG_Finalization
        ! ---------------------------------------------------------------------
    END TYPE Well32RNG

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        FUNCTION Next(RNG) RESULT(RandNum)
            IMPORT
            CLASS(Well32RNG), INTENT(INOUT) :: RNG      ! 'Well32RNG' object
            tSInt32                         :: RandNum  ! random number
        END FUNCTION
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE WELL32RNG_InitWithFlag(RNG, WellAlgo, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified algorithm flag.  Optionally,
    !  initial seeds can be specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'WELL32RNG' object
    CLASS(WELL32RNG),   INTENT(INOUT)   :: RNG
    !% algorithm flag (1-6); if out of applicable range, set it to default (WELL49937b)
    tSInt32,            INTENT(IN)      :: WellAlgo
    !% optional seed(s)
    tSInt32,  OPTIONAL, INTENT(IN)      :: Seed(:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! check and set algorithm flag
    IF (IN_RANGE(WellAlgo, 1, 6)) THEN
        RNG%WellAlgo = WellAlgo
    ELSE
        RNG%WellAlgo = WELL49937b
    END IF

    IF (PRESENT(Seed)) THEN
        ! initialize the generator with seed(s)
        CALL RNG%Initialize(Seed)
    ELSE
        ! initialize the generator without seed
        CALL RNG%Initialize()
    END IF

    RETURN

END SUBROUTINE WELL32RNG_InitWithFlag

!******************************************************************************

SUBROUTINE WELL32RNG_BaseInit(RNG, Seed)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the PRNG with the specified seed(s).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(WELL32RNG), INTENT(INOUT) :: RNG      !! 'WELL32RNG' object
    tSInt32,          INTENT(IN)    :: Seed(:)  !! seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! initialize the generator and set pointer to next output function
    ! based on the algorithm flag
    SELECT CASE (RNG%WellAlgo)
    CASE (WELL512a)
        CALL Init_Internal(RNG, K_512, M1_512, M2_512, M3_512, Seed)
        RNG%NextOutput => Well512RNG_NextInteger
    CASE (WELL1024a)
        CALL Init_Internal(RNG, K_1024, M1_1024, M2_1024, M3_1024, Seed)
        RNG%NextOutput => Well1024RNG_NextInteger
    CASE (WELL19937a, WELL19937c)
        CALL Init_Internal(RNG, K_19937, M1_19937, M2_19937, M3_19937, Seed)
        RNG%NextOutput => Well19937RNG_NextInteger
    CASE (WELL49937a, WELL49937b)
        CALL Init_Internal(RNG, K_49937, M1_49937, M2_49937, M3_49937, Seed)
        RNG%NextOutput => Well49937RNG_NextInteger
    END SELECT

    RETURN

    CONTAINS

    SUBROUTINE Init_Internal(RNG, K, M1, M2, M3, Seed)

    !** PURPOSE OF THIS SUBROUTINE:
        !^ To initialize the 'Well32RNG' object.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(Well32RNG), INTENT(INOUT) :: RNG      !! 'Well32RNG' object
        tSInt32,          INTENT(IN)    :: K        !! number of bits in the pool
        tSInt32,          INTENT(IN)    :: M1       !! first parameter of the algorithm
        tSInt32,          INTENT(IN)    :: M2       !! second parameter of the algorithm
        tSInt32,          INTENT(IN)    :: M3       !! third parameter of the algorithm
        tSInt32,          INTENT(IN)    :: Seed(:)  !! seeds

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex      :: N, J

    ! FLOW

        ! calculate the number of 32-bits blocks
        N = CalculateBlockCount(K)

        ! allocate working arrays
        CALL MemAlloc(RNG%V,     N, StartID=0_kIndex)
        CALL MemAlloc(RNG%IRm1,  N, StartID=0_kIndex)
        CALL MemAlloc(RNG%IRm2,  N, StartID=0_kIndex)
        CALL MemAlloc(RNG%I1,    N, StartID=0_kIndex)
        CALL MemAlloc(RNG%I2,    N, StartID=0_kIndex)
        CALL MemAlloc(RNG%I3,    N, StartID=0_kIndex)

        ! set initial seed
        CALL Fill_State_Internal(Seed, RNG%V)

        ! Initial index
        RNG%Index = 0

        ! pre-compute indirection index tables. These tables are used for optimizing access
        ! they allow saving computations like "(j + r - 2) % r" with costly modulo operations
        DO J = 0, N-1
            RNG%IRm1(J) = MOD(J + N - 1, N)
            RNG%IRm2(J) = MOD(J + N - 2, N)
            RNG%I1(J)   = MOD(J + M1, N)
            RNG%I2(J)   = MOD(J + M2, N)
            RNG%I3(J)   = MOD(J + M3, N)
        END DO

        RETURN

    END SUBROUTINE Init_Internal

    !**************************************************************************

    SUBROUTINE Fill_State_Internal(Seed, State)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To fill State based on the given seed.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt32,  INTENT(IN)    :: Seed(0:)
        tSInt32,  INTENT(OUT)   :: State(0:)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tSInt64, PARAMETER  :: Mask = ToInt64(Z'00000000FFFFFFFF')

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tSInt32     :: SeedSize
        tSInt64     :: Current
        tIndex      :: I

    ! FLOW

        SeedSize = SIZE(Seed)
        State(0:SeedSize-1) = Seed(0:SeedSize-1)
        DO I = SeedSize, SIZE(State)-1
            Current = State(I-SeedSize)
            State(I) = ToInt32(IAND((1812433253_kInt64 * IEOR(Current, SHIFTA(Current, 30)) + I), Mask))
        END DO

        RETURN

    END SUBROUTINE Fill_State_Internal

    !**************************************************************************

END SUBROUTINE WELL32RNG_BaseInit

!******************************************************************************

FUNCTION CalculateBlockCount(K) RESULT(Count)

!** PURPOSE OF THIS SUBROUTINE:
    ! To calculate the number of 32-bits blocks.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  INTENT(IN)    :: K        ! Number of bits in the pool
    tSInt32                 :: Count    ! the number of 32-bits blocks

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! The bits pool contains k bits, k = r w - p where r is the number
    ! of w bits blocks, w is the block size (always 32 in the original paper)
    ! and p is the number of unused bits in the last block.
    Count = (K + BLOCK_SIZE - 1) / BLOCK_SIZE

    RETURN

END FUNCTION CalculateBlockCount

!******************************************************************************

FUNCTION GetIndexPred(RNG, Index) RESULT(IndxOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the predecessor of the given index modulo the table size

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Well32RNG), INTENT(IN)    :: RNG      ! 'Well32RNG' object
    tSInt32,          INTENT(IN)    :: Index    ! index the index to look at
    tSInt32                         :: IndxOut  ! (index - 1) % table size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IndxOut = RNG%IRm1(Index)

    RETURN

END FUNCTION GetIndexPred

!******************************************************************************

FUNCTION GetIndexPred2(RNG, Index) RESULT(IndxOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the second predecessor of the given index modulo the table size

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Well32RNG), INTENT(IN)    :: RNG      ! 'Well32RNG' object
    tSInt32,          INTENT(IN)    :: Index    ! index the index to look at
    tSInt32                         :: IndxOut  ! (index - 2) % table size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IndxOut = RNG%IRm2(Index)

    RETURN

END FUNCTION GetIndexPred2

!******************************************************************************

FUNCTION GetIndexM1(RNG, Index) RESULT(IndxOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return index + M1 modulo the table size

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Well32RNG), INTENT(IN)    :: RNG      ! 'Well32RNG' object
    tSInt32,          INTENT(IN)    :: Index    ! index the index to look at
    tSInt32                         :: IndxOut  ! (index + M1) % table size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IndxOut = RNG%I1(Index)

    RETURN

END FUNCTION GetIndexM1

!******************************************************************************

FUNCTION GetIndexM2(RNG, Index) RESULT(IndxOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return index + M2 modulo the table size

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Well32RNG), INTENT(IN)    :: RNG      ! 'Well32RNG' object
    tSInt32,          INTENT(IN)    :: Index    ! index the index to look at
    tSInt32                         :: IndxOut  ! (index + M2) % table size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IndxOut = RNG%I2(Index)

    RETURN

END FUNCTION GetIndexM2

!******************************************************************************

FUNCTION GetIndexM3(RNG, Index) RESULT(IndxOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return index + M3 modulo the table size

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Well32RNG), INTENT(IN)    :: RNG      ! 'Well32RNG' object
    tSInt32,          INTENT(IN)    :: Index    ! index the index to look at
    tSInt32                         :: IndxOut  ! (index + M3) % table size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IndxOut = RNG%I3(Index)

    RETURN

END FUNCTION GetIndexM3

!******************************************************************************

FUNCTION Well512RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the 32-bit random integer value for WELL512a

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Well32RNG), INTENT(INOUT) :: RNG      ! 'Well32RNG' object
    tSInt32                         :: RandNum  ! random number

!** SUBROUTINE PARAMETER DECLARATIONS:
    tSInt32,  PARAMETER :: CPAR = ToInt32(Z'DA442D24')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: IndexRm1, VI, VI1, VI2
    tSInt32     :: Z0, Z1, Z2, Z3, Z4

! FLOW

    IndexRm1 = RNG%GetIndexPred(RNG%Index)

    VI  = RNG%V(RNG%Index)
    VI1 = RNG%V(RNG%GetIndexM1(RNG%Index))
    VI2 = RNG%V(RNG%GetIndexM2(RNG%Index))
    Z0  = RNG%V(IndexRm1)

    ! the values below include the errata of the original article
    Z1 = IEOR(IEOR(VI, SHIFTL(VI, 16)), IEOR(VI1, SHIFTL(VI1, 15)))
    Z2 = IEOR(VI2, SHIFTR(VI2, 11))
    Z3 = IEOR(Z1, Z2)
    Z4 = IEOR(IEOR(IEOR(IEOR(Z0, SHIFTL(Z0, 2)), IEOR(Z1, SHIFTL(Z1, 18))), &
                   SHIFTL(Z2, 28)), IEOR(Z3, IAND(SHIFTL(Z3, 5), CPAR)))

    RNG%V(RNG%Index) = Z3
    RNG%V(IndexRm1) = Z4
    RNG%Index = IndexRm1

    RandNum = Z4

    RETURN

END FUNCTION Well512RNG_NextInteger

!******************************************************************************

FUNCTION Well1024RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the 32-bit random integer value for WELL1024a

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Well32RNG), INTENT(INOUT) :: RNG      ! 'Well32RNG' object
    tSInt32                         :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: IndexRm1, V0, VM1, VM2, VM3
    tSInt32     :: Z0, Z1, Z2, Z3, Z4

! FLOW

    IndexRm1 = RNG%GetIndexPred(RNG%Index)

    V0  = RNG%V(RNG%Index)
    VM1 = RNG%V(RNG%GetIndexM1(RNG%Index))
    VM2 = RNG%V(RNG%GetIndexM2(RNG%Index))
    VM3 = RNG%V(RNG%GetIndexM3(RNG%Index))

    Z0 = RNG%V(IndexRm1)
    Z1 = IEOR(V0, IEOR(VM1, SHIFTR(VM1, 8)))
    Z2 = IEOR(IEOR(VM2, SHIFTL(VM2, 19)), IEOR(VM3, SHIFTL(VM3, 14)))
    Z3 = IEOR(Z1, Z2)
    Z4 = IEOR(IEOR(IEOR(Z0, SHIFTL(Z0, 11)), IEOR(Z1, SHIFTL(Z1, 7))), IEOR(Z2, SHIFTL(Z2, 13)))

    RNG%V(RNG%Index) = Z3
    RNG%V(IndexRm1) = Z4
    RNG%Index = IndexRm1

    RandNum = Z4

    RETURN

END FUNCTION Well1024RNG_NextInteger

!******************************************************************************

FUNCTION Well19937RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the 32-bit random integer value for WELL19937a and WELL19937c

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Well32RNG), INTENT(INOUT) :: RNG      ! 'Well32RNG' object
    tSInt32                         :: RandNum  ! random number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  PARAMETER :: MaskA = ToInt32(Z'7FFFFFFF')
    tSInt32,  PARAMETER :: MaskB = ToInt32(Z'80000000')
    tSInt32,  PARAMETER :: C1 = ToInt32(Z'E46E1700')
    tSInt32,  PARAMETER :: C2 = ToInt32(Z'9B868000')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: IndexRm1, IndexRm2, V0, VM1, VM2, VM3
    tSInt32     :: Z0, Z1, Z2, Z3, Z4

! FLOW

    IndexRm1 = RNG%GetIndexPred(RNG%Index)
    IndexRm2 = RNG%GetIndexPred2(RNG%Index)

    V0  = RNG%V(RNG%Index)
    VM1 = RNG%V(RNG%GetIndexM1(RNG%Index))
    VM2 = RNG%V(RNG%GetIndexM2(RNG%Index))
    VM3 = RNG%V(RNG%GetIndexM3(RNG%Index))

    Z0 = IEOR(IAND(MaskB, RNG%V(IndexRm1)), IAND(MaskA, RNG%V(IndexRm2)))
    Z1 = IEOR(IEOR(V0, SHIFTL(V0, 25)), IEOR(VM1, SHIFTR(VM1, 27)))
    Z2 = IEOR(SHIFTR(VM2, 9), IEOR(VM3, SHIFTR(VM3, 1)))
    Z3 = IEOR(Z1, Z2)
    Z4 = IEOR(IEOR(IEOR(Z0, IEOR(Z1, SHIFTL(Z1, 9))), IEOR(Z2, SHIFTL(Z2, 21))), &
              IEOR(Z3, SHIFTR(Z3, 21)))

    RNG%V(RNG%Index) = Z3
    RNG%V(IndexRm1)  = Z4
    RNG%V(IndexRm2)  = IAND(RNG%V(IndexRm2), MaskB)
    RNG%Index = IndexRm1

    IF (RNG%WellAlgo == WELL19937c) THEN
        ! Matsumoto-Kurita tempering to get a maximally equidistributed generator
        Z4 = IEOR(Z4, IAND(SHIFTL(Z4, 7), C1))
        Z4 = IEOR(Z4, IAND(SHIFTL(Z4, 15), C2))
    END IF
    RandNum = Z4

    RETURN

END FUNCTION Well19937RNG_NextInteger

!******************************************************************************

FUNCTION Well49937RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the 32-bit random integer value for WELL49937a and WELL49937b

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Well32RNG), INTENT(INOUT) :: RNG      ! 'Well32RNG' object
    tSInt32                         :: RandNum  ! random number

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tSInt32,  PARAMETER :: MaskA = ToInt32(Z'00007FFF')
    tSInt32,  PARAMETER :: MaskB = ToInt32(Z'FFFF8000')
    tSInt32,  PARAMETER :: M1 = ToInt32(Z'FBFFFFFF')
    tSInt32,  PARAMETER :: M2 = ToInt32(Z'00020000')
    tSInt32,  PARAMETER :: M3 = ToInt32(Z'B729FCEC')
    tSInt32,  PARAMETER :: C1 = ToInt32(Z'93DD1400')
    tSInt32,  PARAMETER :: C2 = ToInt32(Z'FA118000')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: IndexRm1, IndexRm2, V0, VM1, VM2, VM3
    tSInt32     :: Z0, Z1, Z2, Z3, Z4, Z2Prime, Z2Second

! FLOW

    IndexRm1 = RNG%GetIndexPred(RNG%Index)
    IndexRm2 = RNG%GetIndexPred2(RNG%Index)

    V0  = RNG%V(RNG%Index)
    VM1 = RNG%V(RNG%GetIndexM1(RNG%Index))
    VM2 = RNG%V(RNG%GetIndexM2(RNG%Index))
    VM3 = RNG%V(RNG%GetIndexM3(RNG%Index))

    !  the values below include the errata of the original article
    Z0 = IEOR(IAND(MaskB, RNG%V(IndexRm1)), IAND(MaskA, RNG%V(IndexRm2)))
    Z1 = IEOR(IEOR(V0, SHIFTL(V0, 24)), IEOR(VM1, SHIFTR(VM1, 30)))
    Z2 = IEOR(IEOR(VM2, SHIFTL(VM2, 10)), SHIFTL(VM3, 26))
    Z3 = IEOR(Z1, Z2)
    Z2Prime  = IAND(IEOR(SHIFTL(Z2, 9), SHIFTR(Z2, 23)), M1)
    IF (IAND(Z2, M2) == 0) THEN
        Z2Second = Z2Prime
    ELSE
        Z2Second = IEOR(Z2Prime, M3)
    END IF
    Z4 = IEOR(IEOR(IEOR(Z0, IEOR(Z1, SHIFTR(Z1, 20))), Z2Second), Z3)

    RNG%V(RNG%Index) = Z3
    RNG%V(IndexRm1)  = Z4
    RNG%V(IndexRm2)  = IAND(RNG%V(IndexRm2), MaskB)
    RNG%Index = IndexRm1

    IF (RNG%WellAlgo == WELL49937b) THEN
        ! Matsumoto-Kurita tempering to get a maximally equidistributed generator
        Z4 = IEOR(Z4, IAND(SHIFTL(Z4, 7), C1))
        Z4 = IEOR(Z4, IAND(SHIFTL(Z4, 15), C2))
    END IF
    RandNum = Z4

    RETURN

END FUNCTION Well49937RNG_NextInteger

!******************************************************************************

FUNCTION Well32RNG_NextInteger(RNG) RESULT(RandNum)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the 32-bit random integer value

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Well32RNG), INTENT(INOUT) :: RNG      ! 'Well32RNG' object
    tSInt32                         :: RandNum  ! random number

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    RandNum = RNG%NextOutput()

    RETURN

END FUNCTION Well32RNG_NextInteger

!******************************************************************************

FUNCTION Well32RNG_GetName(RNG) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    ! To return the name of the generator

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Well32RNG), INTENT(IN)    :: RNG      ! 'Well32RNG' object
    tCharAlloc                      :: Name     ! name of the generator

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT CASE (RNG%WellAlgo)
    CASE (WELL512a)
        Name = 'Well512aRNG'
    CASE (WELL1024a)
        Name = 'Well1024aRNG'
    CASE (WELL19937a)
        Name = 'Well19937aRNG'
    CASE (WELL19937c)
        Name = 'Well19937cRNG'
    CASE (WELL49937a)
        Name = 'Well49937aRNG'
    CASE (WELL49937b)
        Name = 'Well49937bRNG'
    END SELECT

    RETURN

END FUNCTION Well32RNG_GetName

!******************************************************************************

FUNCTION Well32RNG_GetSeedSize_I(RNG) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator
    !  for default algorithm (WELL49937b).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Well32RNG), INTENT(IN)    :: RNG      !! 'Well32RNG' object
    tIndex                          :: Size     !! size of specified seed(s)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: K

! FLOW
    
    K = K_49937
    Size = CalculateBlockCount(K)
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Well32RNG_GetSeedSize_I

!******************************************************************************

FUNCTION Well32RNG_GetSeedSize_II(RNG, WellAlgo) RESULT(Size)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return size of specified seed(s) needed to initialize the generator
    !  for the specified algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    !% 'Well32RNG' object
    CLASS(Well32RNG), INTENT(IN)    :: RNG
    !% algorithm flag (1-6); if out of applicable range, set it to default (WELL49937b)
    tSInt32,          INTENT(IN)    :: WellAlgo
    !% size of specified seed(s)
    tIndex                          :: Size

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tSInt32     :: K

! FLOW
    
    SELECT CASE (WellAlgo)
    CASE (WELL512a)
        K = K_512
    CASE (WELL1024a)
        K = K_1024
    CASE (WELL19937a, WELL19937c)
        K = K_19937
    CASE (WELL49937a, WELL49937b)
        K = K_49937
    CASE DEFAULT
        K = K_49937
    END SELECT
    Size = CalculateBlockCount(K)
    ASSOCIATE (Dummy => RNG); END ASSOCIATE

    RETURN

END FUNCTION Well32RNG_GetSeedSize_II

!******************************************************************************

SUBROUTINE Well32RNG_Finalization(RNG)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform finalization of the 'Well32RNG' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Well32RNG), INTENT(INOUT)   :: RNG  !! 'Well32RNG' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemFree(RNG%V)
    CALL MemFree(RNG%IRm1)
    CALL MemFree(RNG%IRm2)
    CALL MemFree(RNG%I1)
    CALL MemFree(RNG%I2)
    CALL MemFree(RNG%I3)
    NULLIFY(RNG%NextOutput)
    
    RETURN

END SUBROUTINE Well32RNG_Finalization

!******************************************************************************

END MODULE MClass_Well32RNG

!******************************************************************************
