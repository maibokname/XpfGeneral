
MODULE MClass_MetroHasher64

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *MetroHasher64* type and its related routines.
!   The *MetroHasher64* type is a hasher type that extends directly from the
!   <a href="../module/mclass_hasher64.html#type-hasher64">Hasher64</a> type.
!   It provides all deferred procedures required by a *Hasher64* class and
!   outputs the hash value as a 64-bit integer. <br>
!   The *MetroHasher64* type employs the *Metro* hash algorithm for 64-bit integer
!   output by J. Andrew Rogers [1, 2].  As a hasher, it can be used to compute the
!   hash value incrementally.  It also provides a method to compute the hash
!   value directly (i.e. non-incrementally).  The following code snippet shows
!   a typical usage of the hasher.
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
!   However, if the *Update* method is to be called only one time, then the
!   *HashDirect* method should be employed as follows.
!   <Pre><Code style="color:MidnightBlue;">
!   ! directly compute the hash value
!   HashCode = Hasher%HashDirect(Input, InpSize, Seed)
!   </Code></Pre>
!^ **REFERENCES**: <br>
!   [1] <a href="https://github.com/jandrewrogers/MetroHash">MetroHash: Faster, Better
!       Hash Functions. </a> <br>
!   [2] <a href="https://github.com/OpenHFT/Zero-Allocation-Hashing/tree/ea">
!       Zero-Allocation Hashing for Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_Hasher64

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: MetroHasher64

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI8(X)               IAND(ToInt64(X), Z'00000000000000FF')
#define MaskI16(X)              IAND(ToInt64(X), Z'000000000000FFFF')
#define MaskI32(X)              IAND(ToInt64(X), Z'00000000FFFFFFFF')
#define Pack_U16(Buf,Index)     MaskI16(PackShort(Buf, Index))
#define Pack_U32(Buf,Index)     MaskI32(PackInteger(Buf, Index))

!** MODULE PARAMETERS:
    tUInt64, PARAMETER  :: K0 = ToInt64(Z'00000000D6D018F5')
    tUInt64, PARAMETER  :: K1 = ToInt64(Z'00000000A2AA033B')
    tUInt64, PARAMETER  :: K2 = ToInt64(Z'0000000062992FC1')
    tUInt64, PARAMETER  :: K3 = ToInt64(Z'0000000030BC5B29')
    tIndex,  PARAMETER  :: BlockLen = 32_kIndex

!** DERIVED TYPE DEFINITIONS
    !> *MetroHasher64* is a hasher type that outputs the hash value as a 64-bit integer.
    !  It employs the *Metro* hash algorithm by J. Andrew Rogers.
    TYPE, EXTENDS(Hasher64) :: MetroHasher64
        PRIVATE
        tUInt64     :: InitHash             = 0_kInt64
        !% state
        tUInt64     :: State(4)             = 0_kInt64
        !% buffer array used to store input data
        tUInt8      :: BufArr(0:BlockLen-1) = 0_kInt8
        !% flag indicating whether to remove sign from the final hash value
        tLogical    :: RemoveSign   = FalseVal
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName          => Metro_GetName
        !> *GetBlockLength* is a procedure to return the block length of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockLength   => Metro_BlockLength
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: SetBufPtr        => Metro_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: ProcessBlock     => Metro_ProcessBlock
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the hasher. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Initialize(Seed)            ! hash value with sign <br>
        !   --->    CALL Hasher%Initialize(Seed, .TRUE.)    ! remove sign from hash value <br>
        PROCEDURE   :: Initialize       => Metro_Initialize
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 64-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize         => Metro_Finalize
        !> **Type-Bound Function**: HashDirect <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect       => Metro_HashDirect
    END TYPE MetroHasher64

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION Metro_GetName(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MetroHasher64), INTENT(IN)    :: HS   !! a hasher (HS) object
    tCharAlloc                          :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Name = 'Metro_Hahser64'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Metro_GetName

!******************************************************************************

FUNCTION Metro_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MetroHasher64), INTENT(IN)    :: HS       !! a hasher (HS) object
    tIndex                              :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    Length = BlockLen
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Metro_BlockLength

!******************************************************************************

SUBROUTINE Metro_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MetroHasher64), TARGET, INTENT(INOUT) :: HS           !! a hasher (HS) object
    tUInt8,              POINTER, INTENT(INOUT) :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => HS%BufArr

    RETURN

END SUBROUTINE Metro_SetBufPtr

!******************************************************************************

SUBROUTINE Metro_ProcessBlock(HS, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MetroHasher64), INTENT(INOUT) :: HS           !! a hasher (HS) object
    tUInt8,               INTENT(IN)    :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ASSOCIATE (V0 => HS%State(1), V1 => HS%State(2), &
               V2 => HS%State(3), V3 => HS%State(4))
        V0 = V0 + PackLong(BytesIn, 0_kIndex)*K0
        V0 = RotateRight(V0, 29) + V2
        V1 = V1 + PackLong(BytesIn, 8_kIndex)*K1
        V1 = RotateRight(V1, 29) + V3
        V2 = V2 + PackLong(BytesIn, 16_kIndex)*K2
        V2 = RotateRight(V2, 29) + V0
        V3 = V3 + PackLong(BytesIn, 24_kIndex)*K3
        V3 = RotateRight(V3, 29) + V1
    END ASSOCIATE

    RETURN

END SUBROUTINE Metro_ProcessBlock

!******************************************************************************

SUBROUTINE Metro_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MetroHasher64), TARGET, INTENT(INOUT) :: HS   !! a hasher (HS) object
    tUInt64,                      INTENT(IN)    :: Seed !! seed
    tLogical,           OPTIONAL, INTENT(IN)    :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ASSOCIATE (V0 => HS%State(1), V1 => HS%State(2), &
               V2 => HS%State(3), V3 => HS%State(4))
        V0 = (Seed + K2)*K0
        V1 = V0
        V2 = V0
        V3 = V0
        HS%InitHash = V0
    END ASSOCIATE
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE Metro_Initialize

!******************************************************************************

FUNCTION Metro_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 64-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MetroHasher64), INTENT(INOUT) :: HS       !! a hasher (HS) object
    tUInt64                             :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length, Remaining, Offset

!** FLOW

    Remaining = HS%GetBufLen()
    Offset = 0_kIndex
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        Length = HS%GetBlockCount()*HS%GetBlockLength() + Remaining
    ELSE
        Length = Remaining
    END IF

    IF (Length >= 32_kIndex) THEN
        ASSOCIATE (V0 => HS%State(1), V1 => HS%State(2), &
                   V2 => HS%State(3), V3 => HS%State(4))
            V2 = IEOR(V2, RotateRight(((V0 + V3)*K0) + V1, 37)*K1)
            V3 = IEOR(V3, RotateRight(((V1 + V2)*K1) + V0, 37)*K0)
            V0 = IEOR(V0, RotateRight(((V0 + V2)*K0) + V3, 37)*K1)
            V1 = IEOR(V1, RotateRight(((V1 + V3)*K1) + V2, 37)*K0)

            HashCode = HS%InitHash + IEOR(V0, V1)
        END ASSOCIATE
    ELSE
        HashCode = HS%State(1)
    END IF

    ASSOCIATE (Input => HS%BufArr)
        IF (Remaining >= 16_kIndex) THEN
            BLOCK
                tUInt64 :: V0, V1
                V0 = HashCode + (PackLong(Input, Offset)*K2)
                V0 = RotateRight(V0, 29)*K3
                V1 = HashCode + (PackLong(Input, Offset+8_kIndex)*K2)
                V1 = RotateRight(V1, 29)*K3
                V0 = IEOR(V0, RotateRight(V0*K0, 21) + V1)
                V1 = IEOR(V1, RotateRight(V1*K3, 21) + V0)
                HashCode = HashCode + V1
            END BLOCK
            ! update indices
            Offset    = Offset + 16_kIndex
            Remaining = Remaining - 16_kIndex
        END IF

        IF (Remaining >= 8_kIndex) THEN
            HashCode = HashCode + PackLong(Input, Offset)*K3
            HashCode = IEOR(HashCode, RotateRight(HashCode, 55)*K1)

            ! update indices
            Offset    = Offset + 8_kIndex
            Remaining = Remaining - 8_kIndex
        END IF

        IF (Remaining >= 4_kIndex) THEN
            HashCode = HashCode + Pack_U32(Input, Offset)*K3
            HashCode = IEOR(HashCode, RotateRight(HashCode, 26)*K1)

            ! update indices
            Offset    = Offset + 4_kIndex
            Remaining = Remaining - 4_kIndex
        END IF

        IF (Remaining >= 2_kIndex) THEN
            HashCode = HashCode + Pack_U16(Input, Offset)*K3
            HashCode = IEOR(HashCode, RotateRight(HashCode, 48)*K1)

            ! update indices
            Offset    = Offset + 2_kIndex
            Remaining = Remaining - 2_kIndex
        END IF

        IF (Remaining >= 1_kIndex) THEN
            HashCode = HashCode + MaskI8(Input(Offset))*K3
            HashCode = IEOR(HashCode, RotateRight(HashCode, 37)*K1)
        END IF
    END ASSOCIATE

    HashCode = IEOR(HashCode, RotateRight(HashCode, 28))
    HashCode = HashCode*K0
    HashCode = IEOR(HashCode, RotateRight(HashCode, 29))

    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInt64(Z'7FFFFFFFFFFFFFFF'))

    ! reset the hasher
    HS%State  = 0_kInt64
    HS%BufArr = 0_kInt8
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

END FUNCTION Metro_Finalize

!******************************************************************************

FUNCTION Metro_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MetroHasher64),   INTENT(INOUT)   :: HS           !! a hasher (HS) object
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

END FUNCTION Metro_HashDirect

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                   OPTIMAL VERSIONS OF AUXILIARY ROUTINES                    +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

PURE FUNCTION PackLong(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at Offset 'Off' into the 64-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! Offset
    tUInt64             :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: Input(0:7)
    tUInt64     :: Output
    EQUIVALENCE (Output, Input)

! FLOW

    ! implementation algorithm #7
    Input(0:7) = Buf(Off:Off+7)
    Res = Output

    RETURN

END FUNCTION PackLong

!**************************************************************************

PURE FUNCTION PackInteger(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at Offset 'Off' into the 32-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! Offset
    tUInt32             :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

#define MaskInt32(X)  IAND(ToInt32(X), Z'000000FF')

    ! implementation algorithm #1
    Res = IOR(IOR(IOR(       MaskInt32(Buf(Off)),         &
                      SHIFTL(MaskInt32(Buf(Off+1)),  8)), &
                      SHIFTL(MaskInt32(Buf(Off+2)), 16)), &
                      SHIFTL(MaskInt32(Buf(Off+3)), 24))

#undef MaskInt32

    RETURN

END FUNCTION PackInteger

!******************************************************************************

PURE FUNCTION PackShort(Buf, Off) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack the array 'Buf' at offset 'Off' into the 16-bit word 'Res',
    ! in little-endian convention (least significant byte first).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  ! buffer
    tIndex, INTENT(IN)  :: Off      ! offset
    tUInt16             :: Res      ! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! implementation algorithm #3 (comparable to #1)
#define UnsignedByte(Val, Off)  IAND(ToInt32(Val(Off)), Z'000000FF')
#define UnsignedShort(Val, Off) IOR(UnsignedByte(Val, Off), SHIFTL(UnsignedByte(Val, Off+1), 8))
    Res = ToInt16(UnsignedShort(Buf, Off))
#undef UnsignedByte
#undef UnsignedShort

    RETURN

END FUNCTION PackShort

!******************************************************************************

END MODULE MClass_MetroHasher64

!******************************************************************************
