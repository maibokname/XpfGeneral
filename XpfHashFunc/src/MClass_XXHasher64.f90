
MODULE MClass_XXHasher64

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *XXHasher64* type and its related routines.
!   The *XXHasher64* type is a hasher type that extends directly from the
!   <a href="../module/mclass_hasher64.html#type-hasher64">Hasher64</a> type.
!   It provides all deferred procedures required by a *Hasher64* class and
!   outputs the hash value as a 64-bit integer. <br>
!   The *XXHasher64* type employs the *XX* hash algorithm for 64-bit integer
!   output by Yann Collet [1, 2].  As a hasher, it can be used to compute the
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
!   [1] <a href="https://github.com/Cyan4973/xxHash">xxHash: Extremely fast hash algorithm. </a> <br>
!   [2] <a href="https://github.com/OpenHFT/Zero-Allocation-Hashing/tree/ea">
!       Zero-Allocation Hashing for Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_Hasher64
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: XXHasher64

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI8(X)           IAND(ToInt64(X), Z'00000000000000FF')
#define MaskI32(X)          IAND(ToInt64(X), Z'00000000FFFFFFFF')
#define XXH64_Round(Acc, Inp) \
    Acc = Acc + Inp*XXH_PRIME64_2; \
    Acc = RotateLeft(Acc, 31); \
    Acc = Acc*XXH_PRIME64_1;
#define XXH64_MergeRound(Acc, Val) \
    Val = Val*XXH_PRIME64_2; \
    Val = RotateLeft(Val, 31); \
    Val = Val*XXH_PRIME64_1; \
    Acc = IEOR(Acc, Val); \
    Acc = Acc*XXH_PRIME64_1 + XXH_PRIME64_4;
#define GetU8(In, Off)      MaskI8(In(Off))
#define Pack_U32(In, Off)   MaskI32(PackInteger(In, Off))

!** MODULE PARAMETERS:
    tUInt64, PARAMETER  :: XXH_PRIME64_1 = ToInt64(Z'9E3779B185EBCA87')  ! < 0b1001111000110111011110011011000110000101111010111100101010000111 >
    tUInt64, PARAMETER  :: XXH_PRIME64_2 = ToInt64(Z'C2B2AE3D27D4EB4F')  ! < 0b1100001010110010101011100011110100100111110101001110101101001111 >
    tUInt64, PARAMETER  :: XXH_PRIME64_3 = ToInt64(Z'165667B19E3779F9')  ! < 0b0001011001010110011001111011000110011110001101110111100111111001 >
    tUInt64, PARAMETER  :: XXH_PRIME64_4 = ToInt64(Z'85EBCA77C2B2AE63')  ! < 0b1000010111101011110010100111011111000010101100101010111001100011 >
    tUInt64, PARAMETER  :: XXH_PRIME64_5 = ToInt64(Z'27D4EB2F165667C5')  ! < 0b0010011111010100111010110010111100010110010101100110011111000101 >
    tIndex,  PARAMETER  :: BlockLen = 32_kIndex

!** DERIVED TYPE DEFINITIONS
    !> *XXHasher64* is a hasher type that outputs the hash value as a 64-bit integer.
    !  It employs the *XX* hash algorithm by Yann Collet.
    TYPE, EXTENDS(Hasher64) :: XXHasher64
        PRIVATE
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
        PROCEDURE   :: GetName          => XX_GetName
        !> *GetBlockLength* is a procedure to return the block length of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockLength   => XX_BlockLength
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: SetBufPtr        => XX_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: ProcessBlock     => XX_ProcessBlock
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the hasher. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Initialize(Seed)            ! hash value with sign <br>
        !   --->    CALL Hasher%Initialize(Seed, .TRUE.)    ! remove sign from hash value <br>
        PROCEDURE   :: Initialize       => XX_Initialize
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 64-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize         => XX_Finalize
        !> **Type-Bound Function**: HashDirect <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect       => XX_HashDirect
    END TYPE XXHasher64

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION XX_GetName(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XXHasher64), INTENT(IN)   :: HS   !! a hasher (HS) object
    tCharAlloc                      :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Name = 'XX_Hahser64'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION XX_GetName

!******************************************************************************

FUNCTION XX_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XXHasher64), INTENT(IN)   :: HS       !! a hasher (HS) object
    tIndex                          :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION XX_BlockLength

!******************************************************************************

SUBROUTINE XX_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XXHasher64), TARGET, INTENT(INOUT)    :: HS           !! a hasher (HS) object
    tUInt8,           POINTER, INTENT(INOUT)    :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => HS%BufArr

    RETURN

END SUBROUTINE XX_SetBufPtr

!******************************************************************************

SUBROUTINE XX_ProcessBlock(HS, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XXHasher64), INTENT(INOUT)    :: HS           !! a hasher (HS) object
    tUInt8,            INTENT(IN)       :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ASSOCIATE (V1 => HS%State(1), V2 => HS%State(2), &
               V3 => HS%State(3), V4 => HS%State(4))
        XXH64_Round(V1, PackLong(BytesIn, 0_kIndex))
        XXH64_Round(V2, PackLong(BytesIn, 8_kIndex))
        XXH64_Round(V3, PackLong(BytesIn, 16_kIndex))
        XXH64_Round(V4, PackLong(BytesIn, 24_kIndex))
    END ASSOCIATE

    RETURN

END SUBROUTINE XX_ProcessBlock

!******************************************************************************

SUBROUTINE XX_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XXHasher64), TARGET, INTENT(INOUT)    :: HS   !! a hasher (HS) object
    tUInt64,                   INTENT(IN)       :: Seed !! seed
    tLogical,        OPTIONAL, INTENT(IN)       :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ASSOCIATE (V1 => HS%State(1), V2 => HS%State(2), &
               V3 => HS%State(3), V4 => HS%State(4))
        V1 = Seed + XXH_PRIME64_1 + XXH_PRIME64_2
        V2 = Seed + XXH_PRIME64_2
        V3 = Seed
        V4 = Seed - XXH_PRIME64_1
    END ASSOCIATE
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE XX_Initialize

!******************************************************************************

FUNCTION XX_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 64-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XXHasher64), INTENT(INOUT)    :: HS       !! a hasher (HS) object
    tUInt64                             :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: K1
    tIndex      :: Length, Remaining, Offset

!** FLOW

    Remaining = HS%GetBufLen()
    Offset = 0_kIndex
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        Length = HS%GetBlockCount()*HS%GetBlockLength() + Remaining
        ASSOCIATE (V1 => HS%State(1), V2 => HS%State(2), &
                   V3 => HS%State(3), V4 => HS%State(4))
            HashCode = RotateLeft(V1, 1) + RotateLeft(V2, 7) + &
                       RotateLeft(V3, 12) + RotateLeft(V4, 18)
            XXH64_MergeRound(HashCode, V1)
            XXH64_MergeRound(HashCode, V2)
            XXH64_MergeRound(HashCode, V3)
            XXH64_MergeRound(HashCode, V4)
        END ASSOCIATE
    ELSE
        Length = Remaining
        HashCode = HS%State(3) + XXH_PRIME64_5
    END IF
    
    HashCode = HashCode + ToInt64(Length)

    ! XXH64_finalize
    ASSOCIATE (Input => HS%BufArr)
        DO WHILE (Remaining >= 8_kIndex)
            K1 = 0_kInt64
            XXH64_Round(K1, PackLong(Input, Offset))
            HashCode = IEOR(HashCode, K1)
            HashCode = RotateLeft(HashCode, 27)*XXH_PRIME64_1 + XXH_PRIME64_4
            Offset = Offset + 8_kIndex
            Remaining = Remaining - 8_kIndex
        END DO

        IF (Remaining >= 4_kIndex) THEN
            HashCode = IEOR(HashCode, Pack_U32(Input, Offset)*XXH_PRIME64_1)
            HashCode = RotateLeft(HashCode, 23)*XXH_PRIME64_2 + XXH_PRIME64_3
            Offset = Offset + 4_kIndex
            Remaining = Remaining - 4_kIndex
        END IF

        DO WHILE (Remaining /= 0_kIndex)
            HashCode = IEOR(HashCode, GetU8(Input, Offset)*XXH_PRIME64_5)
            HashCode = RotateLeft(HashCode, 11)*XXH_PRIME64_1
            Offset = Offset + 1_kIndex
            Remaining = Remaining - 1_kIndex
        END DO
    END ASSOCIATE

    ! XXH32_avalanche
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 33))
    HashCode = HashCode*XXH_PRIME64_2
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 29))
    HashCode = HashCode*XXH_PRIME64_3
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 32))

    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInt64(Z'7FFFFFFFFFFFFFFF'))

    ! reset the hasher
    HS%State  = 0_kInt64
    HS%BufArr = 0_kInt8
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

END FUNCTION XX_Finalize

!******************************************************************************

FUNCTION XX_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XXHasher64),      INTENT(INOUT)   :: HS           !! a hasher (HS) object
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

END FUNCTION XX_HashDirect

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
    EQUIVALENCE (Input, Output)

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

END MODULE MClass_XXHasher64

!******************************************************************************
