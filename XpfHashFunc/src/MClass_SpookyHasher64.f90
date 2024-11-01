
MODULE MClass_SpookyHasher64

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SpookyHasher64* and *SpookyHasher128* types and their
!   related routines.  The *SpookyHasher64* type is a hasher type that extends directly
!   from the <a href="../module/mclass_hasher64.html#type-hasher64">Hasher64</a> type.
!   It provides all deferred procedures required by a *Hasher64* class and outputs the
!   hash value as a 64-bit integer.  The *SpookyHasher128* type is a hasher type that
!   extends from the *SpookyHasher64* type and provides two additional methods (the
!   *Finalize128* and *HashDirect128* methods) to output the hash value as a 128-bit
!   integer. <br>
!   Both hashers employ the *Spooky* hash algorithm by Bob Jenkins [1].  As hashers,
!   they can be used to compute the hash value incrementally.  They also provide a method
!   to compute the hash value directly (i.e. non-incrementally).  The following code snippet
!   shows a typical usage of the hashers.
!   <Pre><Code style="color:MidnightBlue;">
!   ! first, initialize the hasher (once)
!   CALL Hasher%Initialize(Seed)
!   ! then, put data into the hasher (a number of times)
!   CALL Hasher%Update(Input, InpSize)
!               ...
!               ...
!               ...
!   ! finally, get the hash value from the hasher (once)
!   HashCode = Hasher%Finalize()
!   </Code></Pre>
!   However, if the *Update* method is to be called only one time, then the *HashDirect*
!   method should be employed as follows.
!   <Pre><Code style="color:MidnightBlue;">
!   ! directly compute the hash value
!   HashCode = Hasher%HashDirect(Input, InpSize, Seed)
!   </Code></Pre>
!   To compute the hash value as a 128-bit integer, simply replace the *Finalize* and
!   *HashDirect* methods by the *Finalize128* and *HashDirect128* methods, respectively. <br>
!   <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://burtleburtle.net/bob/hash/spooky.html">SpookyHash: a 128-bit
!       non-cryptographic hash. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SInt128
    USE MClass_Hasher64

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: SpookyHasher64
    PUBLIC :: SpookyHasher128

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define     tSInt128        TYPE(SInt128)
#define     MixData(A, B, C, D, E, Pos, Inp, Off) \
    A = A + PackFull(Inp, Off); \
    B = IEOR(B, C); \
    D = IEOR(D, A); \
    A = RotateLeft(A, Pos); \
    D = D + E;
#define     MixH(A, B, C, Pos) \
    A = A + B; \
    C = IEOR(C, A); \
    B = RotateLeft(B, Pos);
#define     MixH1(A, B, C, P) \
    A = RotateLeft(A, P); \
    A = A + B; \
    C = IEOR(C, A);
#define     MixH2(A, B, P) \
    A = IEOR(A, B); \
    B = RotateLeft(B, P); \
    A = A + B;
#define     ShortMix(A, B, C, D) \
    MixH1(C, D, A, 50); \
    MixH1(D, A, B, 52); \
    MixH1(A, B, C, 30); \
    MixH1(B, C, D, 41); \
    MixH1(C, D, A, 54); \
    MixH1(D, A, B, 48); \
    MixH1(A, B, C, 38); \
    MixH1(B, C, D, 37); \
    MixH1(C, D, A, 62); \
    MixH1(D, A, B, 34); \
    MixH1(A, B, C,  5); \
    MixH1(B, C, D, 36);

!** MODULE PARAMETERS:
    tUInt64, PARAMETER  :: GOLDEN_RATIO_64 = ToInt64(Z'9E3779B97F4A7C15')
    tUInt64, PARAMETER  :: SC = ToInt64(Z'DEADBEEFDEADBEEF')
    tIndex,  PARAMETER  :: StateSize  = 12_kIndex
    tIndex,  PARAMETER  :: BlockSize  = StateSize*8_kIndex
    tIndex,  PARAMETER  :: SmallLimit = BlockSize*2_kIndex
    tIndex,  PARAMETER  :: BlockLen   = SmallLimit

!** DERIVED TYPE DEFINITIONS
    !> *SpookyHasher64* is a hasher type that outputs the hash value as a 64-bit integer.
    !  It employs the *Spooky* hash algorithm by Bob Jenkins.
    TYPE, EXTENDS(Hasher64) :: SpookyHasher64
        PRIVATE
        !% state
        tUInt64         :: State(StateSize)     = 0_kInt64
        !% buffer array used to store input data
        tUInt8          :: BufArr(0:BlockLen-1) = 0_kInt8
        !% flag indicating whether to remove sign from the final hash value
        tLogical        :: RemoveSign           = FalseVal
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName          => Spooky_GetName
        !> *GetBlockLength* is a procedure to return the block length of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockLength   => Spooky_BlockLength
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: SetBufPtr        => Spooky_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: ProcessBlock     => Spooky_ProcessBlock
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the hasher. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Initialize(Seed)            ! hash value with sign <br>
        !   --->    CALL Hasher%Initialize(Seed, .TRUE.)    ! remove sign from hash value <br>
        PROCEDURE   :: Initialize       => Spooky_Initialize
        !> **Type-Bound Subroutine**: InitializeWSeeds <br>
        !  **Purpose**:  To initialize the hasher with two seeds. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%InitializeWSeeds(Seed1, Seed2)          ! hash value with sign <br>
        !   --->    CALL Hasher%InitializeWSeeds(Seed1, Seed2, .TRUE.)  ! remove sign from hash value <br>
        PROCEDURE   :: InitializeWSeeds => Spooky_Initialize_WithSeeds
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 64-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize         => Spooky_Finalize
        !> **Type-Bound Function**: HashDirect <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect       => Spooky_HashDirect
    END TYPE SpookyHasher64
    !> *SpookyHasher128* is a hasher type that outputs the hash value as a 128-bit integer.
    !  It is a subtype of the *SpookyHasher64* type.
    TYPE, EXTENDS(SpookyHasher64) :: SpookyHasher128
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName          => Spooky_GetName128
        !> **Type-Bound Function**: Finalize128 <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 128-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize128()
        PROCEDURE   :: Finalize128     => Spooky_Finalize128
        !> **Type-Bound Function**: HashDirect128 <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect128(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect128(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect128(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect128   => Spooky_HashDirect128
    END TYPE SpookyHasher128

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION Spooky_GetName(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SpookyHasher64), INTENT(IN)   :: HS   !! a hasher (HS) object
    tCharAlloc                          :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Name = 'Spooky_Hahser64'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Spooky_GetName

!******************************************************************************

FUNCTION Spooky_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SpookyHasher64), INTENT(IN)   :: HS       !! a hasher (HS) object
    tIndex                              :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Length = BlockLen
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Spooky_BlockLength

!******************************************************************************

SUBROUTINE Spooky_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SpookyHasher64), TARGET, INTENT(INOUT)    :: HS           !! a hasher (HS) object
    tUInt8,               POINTER, INTENT(INOUT)    :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => HS%BufArr

    RETURN

END SUBROUTINE Spooky_SetBufPtr

!******************************************************************************

SUBROUTINE Spooky_ProcessBlock(HS, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SpookyHasher64), INTENT(INOUT)    :: HS           !! a hasher (HS) object
    tUInt8,                INTENT(IN)       :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: Offset, I

!** FLOW

    ! Important Note: HS%BufLong is an alias of HS%BufArr, which is the same as
    !                 BytesIn so this routine does not use BytesIn directly but
    !                 through HS%BufLong instead.
    ASSOCIATE (H0 => HS%State(1),  H1  => HS%State(2),  H2  => HS%State(3), &
               H3 => HS%State(4),  H4  => HS%State(5),  H5  => HS%State(6), &
               H6 => HS%State(7),  H7  => HS%State(8),  H8  => HS%State(9), &
               H9 => HS%State(10), H10 => HS%State(11), H11 => HS%State(12))
        Offset = 0_kIndex
        DO I = 1, 2
            ! mixing internal states and data
            MixData(H0,  H2,  H10, H11, H1,  11, BytesIn, Offset)
            MixData(H1,  H3,  H11, H0,  H2,  32, BytesIn, Offset+8_kIndex)
            MixData(H2,  H4,  H0,  H1,  H3,  43, BytesIn, Offset+16_kIndex)
            MixData(H3,  H5,  H1,  H2,  H4,  31, BytesIn, Offset+24_kIndex)
            MixData(H4,  H6,  H2,  H3,  H5,  17, BytesIn, Offset+32_kIndex)
            MixData(H5,  H7,  H3,  H4,  H6,  28, BytesIn, Offset+40_kIndex)
            MixData(H6,  H8,  H4,  H5,  H7,  39, BytesIn, Offset+48_kIndex)
            MixData(H7,  H9,  H5,  H6,  H8,  57, BytesIn, Offset+56_kIndex)
            MixData(H8,  H10, H6,  H7,  H9,  55, BytesIn, Offset+64_kIndex)
            MixData(H9,  H11, H7,  H8,  H10, 54, BytesIn, Offset+72_kIndex)
            MixData(H10, H0,  H8,  H9,  H11, 22, BytesIn, Offset+80_kIndex)
            MixData(H11, H1,  H9,  H10, H0,  46, BytesIn, Offset+88_kIndex)
            Offset = Offset + BlockSize
        END DO
    END ASSOCIATE

    RETURN

END SUBROUTINE Spooky_ProcessBlock

!******************************************************************************

SUBROUTINE Spooky_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SpookyHasher64), TARGET, INTENT(INOUT)    :: HS   !! a hasher (HS) object
    tUInt64,                       INTENT(IN)       :: Seed !! seed
    tLogical,            OPTIONAL, INTENT(IN)       :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL HS%InitializeWSeeds(Seed, Seed+GOLDEN_RATIO_64, RemoveSign)

    RETURN

END SUBROUTINE Spooky_Initialize

!******************************************************************************

SUBROUTINE Spooky_Initialize_WithSeeds(HS, Seed1, Seed2, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher with two seeds.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SpookyHasher64), TARGET, INTENT(INOUT)    :: HS       !! a hasher (HS) object
    tUInt64,                       INTENT(IN)       :: Seed1    !! seed1
    tUInt64,                       INTENT(IN)       :: Seed2    !! seed2
    tLogical,            OPTIONAL, INTENT(IN)       :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ASSOCIATE (H0 => HS%State(1),  H1  => HS%State(2),  H2  => HS%State(3), &
               H3 => HS%State(4),  H4  => HS%State(5),  H5  => HS%State(6), &
               H6 => HS%State(7),  H7  => HS%State(8),  H8  => HS%State(9), &
               H9 => HS%State(10), H10 => HS%State(11), H11 => HS%State(12))
        H0  = Seed1
        H3  = Seed1
        H6  = Seed1
        H9  = Seed1
        H1  = Seed2
        H4  = Seed2
        H7  = Seed2
        H10 = Seed2
        H2  = SC
        H5  = SC
        H8  = SC
        H11 = SC
    END ASSOCIATE
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE Spooky_Initialize_WithSeeds

!******************************************************************************

FUNCTION Spooky_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 64-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SpookyHasher64), INTENT(INOUT)    :: HS       !! a hasher (HS) object
    tUInt64                                 :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Remaining

!** FLOW

    Remaining = HS%GetBufLen()
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        CALL Spooky_Finalize64(HS, Remaining)
        HashCode = HS%State(1)
    ELSE
        HashCode = SmallHash(HS%BufArr, Remaining, HS%State(1), HS%State(2))
    END IF

    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInt64(Z'7FFFFFFFFFFFFFFF'))

    ! reset the hasher
    HS%State  = 0_kInt64
    HS%BufArr = 0_kInt8
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

END FUNCTION Spooky_Finalize

!******************************************************************************

FUNCTION Spooky_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SpookyHasher64),  INTENT(INOUT)   :: HS           !! a hasher (HS) object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tUInt64,  OPTIONAL,     INTENT(IN)      :: Seed         !! seed
    tLogical, OPTIONAL,     INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tUInt64                                 :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Seed0

!** FLOW

    SET_OPTION(Seed0, 0_kInt64, Seed)

    ! The following code illustrates simple use of the hasher.
    CALL HS%Initialize(Seed0, RemoveSign)
    CALL HS%Update(Input, InpSize)
    HashCode = HS%Finalize()

    RETURN

END FUNCTION Spooky_HashDirect

!******************************************************************************

FUNCTION Spooky_GetName128(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SpookyHasher128), INTENT(IN)  :: HS   !! a hasher (HS) object
    tCharAlloc                          :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Name = 'Spooky_Hahser128'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Spooky_GetName128

!******************************************************************************

FUNCTION Spooky_Finalize128(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 128-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SpookyHasher128), INTENT(INOUT)   :: HS       !! a hasher (HS) object
    tSInt128                                :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Remaining

!** FLOW

    Remaining = HS%GetBufLen()
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        CALL Spooky_Finalize64(HS, Remaining)
        HashCode%Low  = HS%State(1)
        HashCode%High = HS%State(2)
    ELSE
        HashCode%Low = SmallHash(HS%BufArr, Remaining, HS%State(1), HS%State(2), HashCode%High)
    END IF

    ! remove sign if needed
    IF (HS%RemoveSign) HashCode%High = IAND(HashCode%High, ToInt64(Z'7FFFFFFFFFFFFFFF'))

    ! reset the hasher
    HS%State  = 0_kInt64
    HS%BufArr = 0_kInt8
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

END FUNCTION Spooky_Finalize128

!******************************************************************************

FUNCTION Spooky_HashDirect128(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SpookyHasher128), INTENT(INOUT)   :: HS           !! a hasher (HS) object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tUInt64,  OPTIONAL,     INTENT(IN)      :: Seed         !! seed
    tLogical, OPTIONAL,     INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tSInt128                                :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: Seed0

!** FLOW

    SET_OPTION(Seed0, 0_kInt64, Seed)

    ! The following code illustrates simple use of the hasher.
    CALL HS%Initialize(Seed0, RemoveSign)
    CALL HS%Update(Input, InpSize)
    HashCode = HS%Finalize128()

    RETURN

END FUNCTION Spooky_HashDirect128

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                   OPTIMAL VERSIONS OF AUXILIARY ROUTINES                    +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION SmallHash(Input, Length, Seed1, Seed2, HashHigh) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compute hash code for small input using the SpookyHash (version 2)
    ! hash algorithm by Bob Jenkins.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8,            INTENT(IN)   :: Input(0:)    ! input bytes
    tIndex,            INTENT(IN)   :: Length       ! size of input bytes
    tUInt64,           INTENT(IN)   :: Seed1        ! seed
    tUInt64, OPTIONAL, INTENT(IN)   :: Seed2        ! seed
    tUInt64, OPTIONAL, INTENT(OUT)  :: HashHigh     ! 64-bit upper hash code
    tUInt64                         :: HashCode     ! 64-bit lower hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: H0, H1, H2, H3
    tIndex      :: Remaining, Offset

!** FLOW

    ! initialize
    H0 = Seed1
    H1 = Seed1+GOLDEN_RATIO_64
    IF (PRESENT(Seed2)) H1 = Seed2
    H2 = SC
    H3 = SC
    Remaining = Length
    Offset = 0

    DO WHILE (Remaining >= 32)
        H2 = H2 + PackFull(Input, Offset)
        H3 = H3 + PackFull(Input, Offset+8)
        ShortMix(H0, H1, H2, H3)
        H0 = H0 + PackFull(Input, Offset+16)
        H1 = H1 + PackFull(Input, Offset+24)
        ! update indices
        Offset = Offset + 32
        Remaining = Remaining - 32
    END DO

    IF (Remaining >= 16) THEN
        H2 = H2 + PackFull(Input, Offset)
        H3 = H3 + PackFull(Input, Offset+8)
        ShortMix(H0, H1, H2, H3)
        ! update indices
        Offset = Offset + 16
        Remaining = Remaining - 16
    END IF

    H3 = H3 + SHIFTL(ToInt64(Length), 56)

    IF (Remaining >= 8) THEN
        H2 = H2 + PackFull(Input, Offset)
        ! update indices
        Offset = Offset + 8
        Remaining = Remaining - 8
        IF (Remaining > 0) H3 = H3 + PackPartial(Input, Offset, Remaining)
    ELSEIF (Remaining > 0) THEN
        H2 = H2 + PackPartial(Input, Offset, Remaining)
    ELSE
        H2 = H2 + SC
        H3 = H3 + SC
    END IF

    ! ShortEnd
    MixH2(H3, H2, 15)
    MixH2(H0, H3, 52)
    MixH2(H1, H0, 26)
    MixH2(H2, H1, 51)
    MixH2(H3, H2, 28)
    MixH2(H0, H3,  9)
    MixH2(H1, H0, 47)
    MixH2(H2, H1, 54)
    MixH2(H3, H2, 32)
    MixH2(H0, H3, 25)
    MixH2(H1, H0, 63)

    ! set output
    HashCode = H0
    IF (PRESENT(HashHigh)) HashHigh = H1

    RETURN

END FUNCTION SmallHash

!******************************************************************************

SUBROUTINE Spooky_Finalize64(HS, Remaining)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 64-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SpookyHasher64), INTENT(INOUT)    :: HS           !! a hasher (HS) object
    tIndex,                INTENT(IN)       :: Remaining    !! remaining length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: PartialSize, WholeWords, I
    tUInt64     :: Partial

!** FLOW

    ASSOCIATE (H0 => HS%State(1),  H1  => HS%State(2),  H2  => HS%State(3), &
                H3 => HS%State(4),  H4  => HS%State(5),  H5  => HS%State(6), &
                H6 => HS%State(7),  H7  => HS%State(8),  H8  => HS%State(9), &
                H9 => HS%State(10), H10 => HS%State(11), H11 => HS%State(12), &
                Input => HS%BufArr)
        PartialSize = IAND(Remaining, 7_kIndex)
        WholeWords  = SHIFTR(Remaining, 3)
        IF (PartialSize > 0) THEN
            Partial = PackPartial(Input, SHIFTL(WholeWords, 3), PartialSize)
            SELECT CASE (WholeWords)
            CASE (0)
                H0 = H0 + Partial
            CASE (1)
                H1 = H1 + Partial
            CASE (2)
                H2 = H2 + Partial
            CASE (3)
                H3 = H3 + Partial
            CASE (4)
                H4 = H4 + Partial
            CASE (5)
                H5 = H5 + Partial
            CASE (6)
                H6 = H6 + Partial
            CASE (7)
                H7 = H7 + Partial
            CASE (8)
                H8 = H8 + Partial
            CASE (9)
                H9 = H9 + Partial
            CASE (10)
                H10 = H10 + Partial
            CASE (11)
                H11 = H11 + Partial
            END SELECT
        END IF

        ! fall-through is intentional
        IF (WholeWords  >= 11_kIndex) H10 = H10 + PackFull(Input, 80_kIndex)
        IF (WholeWords  >= 10_kIndex) H9  = H9  + PackFull(Input, 72_kIndex)
        IF (WholeWords  >=  9_kIndex) H8  = H8  + PackFull(Input, 64_kIndex)
        IF (WholeWords  >=  8_kIndex) H7  = H7  + PackFull(Input, 56_kIndex)
        IF (WholeWords  >=  7_kIndex) H6  = H6  + PackFull(Input, 48_kIndex)
        IF (WholeWords  >=  6_kIndex) H5  = H5  + PackFull(Input, 40_kIndex)
        IF (WholeWords  >=  5_kIndex) H4  = H4  + PackFull(Input, 32_kIndex)
        IF (WholeWords  >=  4_kIndex) H3  = H3  + PackFull(Input, 24_kIndex)
        IF (WholeWords  >=  3_kIndex) H2  = H2  + PackFull(Input, 16_kIndex)
        IF (WholeWords  >=  2_kIndex) H1  = H1  + PackFull(Input, 8_kIndex)
        IF (WholeWords  >=  1_kIndex) H0  = H0  + PackFull(Input, 0_kIndex)

        H11 = H11 + SHIFTL(ToInt64(Remaining), 56)

        ! end mixing with 3 iterations
        DO I = 1, 3
            MixH(H11, H1,  H2,  44)
            MixH(H0,  H2,  H3,  15)
            MixH(H1,  H3,  H4,  34)
            MixH(H2,  H4,  H5,  21)
            MixH(H3,  H5,  H6,  38)
            MixH(H4,  H6,  H7,  33)
            MixH(H5,  H7,  H8,  10)
            MixH(H6,  H8,  H9,  13)
            MixH(H7,  H9,  H10, 38)
            MixH(H8,  H10, H11, 53)
            MixH(H9,  H11, H0,  42)
            MixH(H10, H0,  H1,  54)
        END DO
    END ASSOCIATE

    RETURN

END SUBROUTINE Spooky_Finalize64

!******************************************************************************

PURE FUNCTION PackFull(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at offset 'Off' into the 64-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! offset
    tUInt64             :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: Input(0:7)
    tUInt64     :: Output
    EQUIVALENCE (Input, Output)

! FLOW

    ! implementation algorithm #7
    Input(0:7) = Buf(Off:Off+7)
    Res = Output

    RETURN

END FUNCTION PackFull

!******************************************************************************

PURE FUNCTION PackPartial(Buf, Off, Length) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack seven or fewer bytes of the array 'Buf' at offset 'Off'
    ! into the 64-bit word 'Res', in little-endian convention
    ! (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! offset
    tIndex, INTENT(IN)  :: Length   ! the number of bytes to pack (between 1 to 7)
    tUInt64             :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: Wrk(0:7)

! FLOW

    ! initialize
    Wrk = 0_kInt8

    ! gather available bytes
    Wrk(0:Length-1) = Buf(Off:Off+Length-1)

    ! pack bytes into word
    Res = PackFull(Wrk, 0_kIndex)

    RETURN

END FUNCTION PackPartial

!******************************************************************************

END MODULE MClass_SpookyHasher64

!******************************************************************************
