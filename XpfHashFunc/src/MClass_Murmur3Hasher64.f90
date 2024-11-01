
MODULE MClass_Murmur3Hasher64

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Murmur3Hasher64* and *Murmur3Hasher128* types and their
!   related routines.  The *Murmur3Hasher64* type is a hasher type that extends directly
!   from the <a href="../module/mclass_hasher64.html#type-hasher64">Hasher64</a> type.
!   It provides all deferred procedures required by a *Hasher64* class and outputs the
!   hash value as a 64-bit integer.  The *Murmur3Hasher128* type is a hasher type that
!   extends from the *Murmur3Hasher64* type and provides two additional methods (the
!   *Finalize128* and *HashDirect128* methods) to output the hash value as a 128-bit
!   integer. <br>
!   Both hashers employ the *Murmur3* hash algorithm by Austin Appleby [1, 2].  As hashers,
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
!   [1] <a href="https://github.com/aappleby/smhasher">SMHasher: a test suite designed to
!       test the distribution, collision, and performance properties of non-cryptographic
!       hash functions. </a> <br>
!   [2] <a href="https://github.com/OpenHFT/Zero-Allocation-Hashing/tree/ea">
!       Zero-Allocation Hashing for Java. </a> <br>

!** USE STATEMENTS:
    USE ISO_C_BINDING,      ONLY: C_LOC, C_F_POINTER, C_PTR, C_NULL_PTR
    USE MBase_Common
    USE MBase_SInt128
    USE MClass_Hasher64

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Murmur3Hasher64
    PUBLIC :: Murmur3Hasher128

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define     tSInt128        TYPE(SInt128)
#define     MaskI8(X)       IAND(ToInt64(X), ToInt64(Z'00000000000000FF'))
#define     K1_Mixing(K) \
    K = K*C1; \
    K = RotateLeft(K, 31); \
    K = K*C2;
#define     K2_Mixing(K) \
    K = K*C2; \
    K = RotateLeft(K, 33); \
    K = K*C1;
#define     FinalMixing(H) \
    H = IEOR(H, SHIFTR(H, 33)); \
    H = H*ToInt64(Z'FF51AFD7ED558CCD'); \
    H = IEOR(H, SHIFTR(H, 33)); \
    H = H*ToInt64(Z'C4CEB9FE1A85EC53'); \
    H = IEOR(H, SHIFTR(H, 33));

!** MODULE PARAMETERS:
    tUInt64, PARAMETER  :: C1 = ToInt64(Z'87C37B91114253D5')
    tUInt64, PARAMETER  :: C2 = ToInt64(Z'4CF5AD432745937F')
    tIndex,  PARAMETER  :: BlockLen = 16_kIndex

!** DERIVED TYPE DEFINITIONS
    !> *Murmur3Hasher64* is a hasher type that outputs the hash value as a 64-bit integer.
    !  It employs the *Murmur3* hash algorithm by Austin Appleby.
    TYPE, EXTENDS(Hasher64) :: Murmur3Hasher64
        PRIVATE
        !% state
        tUInt64             :: State(2)             = 0_kInt64
        !% buffer array used to store input data
        tUInt8              :: BufArr(0:BlockLen-1) = 0_kInt8
        !% pointer to the buffer array as 64-bit integers
        tUInt64, POINTER    :: BufLong(:)           => NULL()
        !% flag indicating whether to remove sign from the final hash value
        tLogical            :: RemoveSign           = FalseVal
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName          => Murmur3_GetName
        !> *GetBlockLength* is a procedure to return the block length of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockLength   => Murmur3_BlockLength
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: SetBufPtr        => Murmur3_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: ProcessBlock     => Murmur3_ProcessBlock
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the hasher. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Initialize(Seed)            ! hash value with sign <br>
        !   --->    CALL Hasher%Initialize(Seed, .TRUE.)    ! remove sign from hash value <br>
        PROCEDURE   :: Initialize       => Murmur3_Initialize
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 64-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize         => Murmur3_Finalize
        !> **Type-Bound Function**: HashDirect <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect       => Murmur3_HashDirect
    END TYPE Murmur3Hasher64
    !> *Murmur3Hasher128* is a hasher type that outputs the hash value as a 128-bit integer.
    !  It is a subtype of the *Murmur3Hasher64* type.
    TYPE, EXTENDS(Murmur3Hasher64) :: Murmur3Hasher128
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName          => Murmur3_GetName128
        !> **Type-Bound Function**: Finalize128 <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 128-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize128()
        PROCEDURE   :: Finalize128     => Murmur3_Finalize128
        !> **Type-Bound Function**: HashDirect128 <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect128(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect128(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect128(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect128   => Murmur3_HashDirect128
    END TYPE Murmur3Hasher128

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION Murmur3_GetName(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher64), INTENT(IN)  :: HS   !! a hasher (HS) object
    tCharAlloc                          :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Name = 'Murmur3_Hahser64'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Murmur3_GetName

!******************************************************************************

FUNCTION Murmur3_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher64), INTENT(IN)  :: HS       !! a hasher (HS) object
    tIndex                              :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Length = BlockLen
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Murmur3_BlockLength

!******************************************************************************

SUBROUTINE Murmur3_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher64), TARGET, INTENT(INOUT)   :: HS           !! a hasher (HS) object
    tUInt8,                POINTER, INTENT(INOUT)   :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => HS%BufArr

    RETURN

END SUBROUTINE Murmur3_SetBufPtr

!******************************************************************************

SUBROUTINE Murmur3_ProcessBlock(HS, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher64), INTENT(INOUT)   :: HS           !! a hasher (HS) object
    tUInt8,                 INTENT(IN)      :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: K1, K2

!** FLOW

    ! Important Note: HS%BufLong is an alias of HS%BufArr, which is the same as
    !                 BytesIn so this routine does not use BytesIn directly but
    !                 through HS%BufLong instead.
    ASSOCIATE (H1      => HS%State(1), H2    => HS%State(2), &
               LongVal => HS%BufLong,  Dummy => BytesIn)
        K1 = LongVal(1)
        K2 = LongVal(2)

        K1_Mixing(K1)
        H1 = IEOR(H1, K1)
        H1 = RotateLeft(H1, 27)
        H1 = H1 + H2
        H1 = H1*5_kInt64 + ToInt64(Z'0000000052DCE729')

        K2_Mixing(K2)
        H2 = IEOR(H2, K2)
        H2 = RotateLeft(H2, 31)
        H2 = H2 + H1
        H2 = H2*5_kInt64 + ToInt64(Z'0000000038495AB5')
    END ASSOCIATE

    RETURN

END SUBROUTINE Murmur3_ProcessBlock

!******************************************************************************

SUBROUTINE Murmur3_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher64), TARGET, INTENT(INOUT)   :: HS   !! a hasher (HS) object
    tUInt64,                        INTENT(IN)      :: Seed !! seed
    tLogical,             OPTIONAL, INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(C_PTR)     :: CPtr     ! C pointer to the input

!** FLOW

    HS%State = Seed
    CPtr = C_LOC(HS%BufArr)
    CALL C_F_POINTER(CPtr, HS%BufLong, SHAPE=[BlockLen/8])
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()
    cPtr = C_NULL_PTR

    RETURN

END SUBROUTINE Murmur3_Initialize

!******************************************************************************

FUNCTION Murmur3_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 64-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher64), INTENT(INOUT)   :: HS       !! a hasher (HS) object
    tUInt64                                 :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length, Remaining

!** FLOW

    Remaining = HS%GetBufLen()
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        Length = HS%GetBlockCount()*HS%GetBlockLength() + Remaining
    ELSE
        Length = Remaining
    END IF

    CALL Murmur3_Finalize64(HS, Length, Remaining)
    HashCode = HS%State(1)

    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInt64(Z'7FFFFFFFFFFFFFFF'))

    ! reset the hasher
    HS%State  = 0_kInt64
    HS%BufArr = 0_kInt8
    NULLIFY(HS%BufLong)
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

END FUNCTION Murmur3_Finalize

!******************************************************************************

FUNCTION Murmur3_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher64), INTENT(INOUT)   :: HS           !! a hasher (HS) object
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

END FUNCTION Murmur3_HashDirect

!******************************************************************************

FUNCTION Murmur3_GetName128(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher128), INTENT(IN) :: HS   !! a hasher (HS) object
    tCharAlloc                          :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Name = 'Murmur3_Hahser128'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Murmur3_GetName128

!******************************************************************************

FUNCTION Murmur3_Finalize128(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 128-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher128), INTENT(INOUT)  :: HS       !! a hasher (HS) object
    tSInt128                                :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length, Remaining

!** FLOW

    Remaining = HS%GetBufLen()
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        Length = HS%GetBlockCount()*HS%GetBlockLength() + Remaining
    ELSE
        Length = Remaining
    END IF

    CALL Murmur3_Finalize64(HS, Length, Remaining)
    HashCode%Low  = HS%State(1)
    HashCode%High = HS%State(1) + HS%State(2)

    ! remove sign if needed
    IF (HS%RemoveSign) HashCode%High = IAND(HashCode%High, ToInt64(Z'7FFFFFFFFFFFFFFF'))

    ! reset the hasher
    HS%State  = 0_kInt64
    HS%BufArr = 0_kInt8
    NULLIFY(HS%BufLong)
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

END FUNCTION Murmur3_Finalize128

!******************************************************************************

FUNCTION Murmur3_HashDirect128(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher128), INTENT(INOUT)  :: HS           !! a hasher (HS) object
    TYPE(*),  CONTIGUOUS,    INTENT(IN)     :: Input(..)    !! input data (any type and rank)
    tIndex,                  INTENT(IN)     :: InpSize      !! size of the input (in bytes)
    tUInt64,  OPTIONAL,      INTENT(IN)     :: Seed         !! seed
    tLogical, OPTIONAL,      INTENT(IN)     :: RemoveSign
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

END FUNCTION Murmur3_HashDirect128

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                   OPTIMAL VERSIONS OF AUXILIARY ROUTINES                    +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE Murmur3_Finalize64(HS, Length, Remaining)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 64-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher64), INTENT(INOUT)   :: HS           !! a hasher (HS) object
    tIndex,                 INTENT(IN)      :: Length       !! total length
    tIndex,                 INTENT(IN)      :: Remaining    !! remaining length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: K1, K2

!** FLOW

    ASSOCIATE (H1      => HS%State(1), H2    => HS%State(2), &
               LongVal => HS%BufLong,  Input => HS%BufArr)
        IF (Remaining > 0_kIndex) THEN
            K1 = 0_kInt64
            K2 = 0_kInt64
            SELECT CASE (Remaining)
            CASE (8:15)
                K1 = LongVal(1)
                K2 = PackPartial(Input, 8_kIndex, Remaining-8_kIndex)
            CASE (1:7)
                K1 = PackPartial(Input, 0_kIndex, Remaining)
            END SELECT
            K1_Mixing(K1)
            H1 = IEOR(H1, K1)
            K2_Mixing(K2)
            H2 = IEOR(H2, K2)
        END IF

        ! Do a few final mixes of the hash to ensure the last few bytes are well-incorporated
        H1 = IEOR(H1, ToInt64(Length))
        H2 = IEOR(H2, ToInt64(Length))
        H1 = H1 + H2
        H2 = H2 + H1
        FinalMixing(H1)
        FinalMixing(H2)

        H1 = H1 + H2
    END ASSOCIATE

    RETURN

END SUBROUTINE Murmur3_Finalize64

!******************************************************************************

FUNCTION PackPartial(Buf, Off, Length) RESULT(Res)

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
    tUInt8          :: Wrk(0:7)
! FLOW

    ! initialize
    Wrk = 0_kInt8

    ! gather available bytes
    Wrk(0:Length-1) = Buf(Off:Off+Length-1)

    ! pack bytes into word
#define MaskInt64(X)     IAND(ToInt64(X), ToInt64(Z'00000000000000FF'))
    Res =        MaskInt64(Wrk(0))      + SHIFTL(MaskInt64(Wrk(1)),  8) + &
          SHIFTL(MaskInt64(Wrk(2)), 16) + SHIFTL(MaskInt64(Wrk(3)), 24) + &
          SHIFTL(MaskInt64(Wrk(4)), 32) + SHIFTL(MaskInt64(Wrk(5)), 40) + &
          SHIFTL(MaskInt64(Wrk(6)), 48) + SHIFTL(MaskInt64(Wrk(7)), 56)
#undef MaskInt64

    RETURN

END FUNCTION PackPartial

!**************************************************************************

END MODULE MClass_Murmur3Hasher64

!******************************************************************************
