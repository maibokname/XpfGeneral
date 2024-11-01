
MODULE MClass_XXHasher32

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *XXHasher32* type and its related routines.
!   The *XXHasher32* type is a hasher type that extends directly from the
!   <a href="../module/mclass_hasher32.html#type-hasher32">Hasher32</a> type.
!   It provides all deferred procedures required by a *Hasher32* class and
!   outputs the hash value as a 32-bit integer. <br>
!   The *XXHasher32* type employs the *XX* hash algorithm for 32-bit integer
!   output by Yann Collet [1].  As a hasher, it can be used to compute the
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

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_Hasher32
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: XXHasher32

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define Pack_U8(Inp,Off)    IAND(ToInt32(Inp(Off)), Z'000000FF')
#define MaskInt32(X)        IAND(ToInt32(X), Z'000000FF')
#define Pack_I32(Buf,Off)   (MaskInt32(Buf(Off)) + SHIFTL(MaskInt32(Buf(Off+1)),  8) + \
                            SHIFTL(MaskInt32(Buf(Off+2)), 16) + SHIFTL(MaskInt32(Buf(Off+3)), 24))
#define XXH32_Round(Acc, Inp) \
    Acc = Acc + Inp*XXH_PRIME32_2; \
    Acc = RotateLeft(Acc, 13); \
    Acc = Acc*XXH_PRIME32_1;

!** MODULE PARAMETERS:
    tUInt32,  PARAMETER     :: XXH_PRIME32_1 = ToInt32(Z'9E3779B1')
    tUInt32,  PARAMETER     :: XXH_PRIME32_2 = ToInt32(Z'85EBCA77')
    tUInt32,  PARAMETER     :: XXH_PRIME32_3 = ToInt32(Z'C2B2AE3D')
    tUInt32,  PARAMETER     :: XXH_PRIME32_4 = ToInt32(Z'27D4EB2F')
    tUInt32,  PARAMETER     :: XXH_PRIME32_5 = ToInt32(Z'165667B1')

!** DERIVED TYPE DEFINITIONS
    !> *XXHasher32* is a hasher type that outputs the hash value as a 32-bit integer.
    !  It employs the *XX* hash algorithm by Yann Collet.
    TYPE, EXTENDS(Hasher32) :: XXHasher32
        PRIVATE
        !% state
        tUInt32     :: State(4)     = 0_kInt32
        !% buffer array used to store input data
        tUInt8      :: BufArr(0:15) = 0_kInt8
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
        !                in a 32-bit integer.  The object is reset. <br>
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
    END TYPE XXHasher32

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
    CLASS(XXHasher32), INTENT(IN)   :: HS   !! a hasher (HS) object
    tCharAlloc                      :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Name = 'XX_Hahser32'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION XX_GetName

!******************************************************************************

FUNCTION XX_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XXHasher32), INTENT(IN)   :: HS       !! a hasher (HS) object
    tIndex                          :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Length = 16_kIndex
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION XX_BlockLength

!******************************************************************************

SUBROUTINE XX_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XXHasher32), TARGET, INTENT(INOUT)    :: HS           !! a hasher (HS) object
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
    CLASS(XXHasher32), INTENT(INOUT)    :: HS           !! a hasher (HS) object
    tUInt8,            INTENT(IN)       :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    ASSOCIATE (V1 => HS%State(1), V2 => HS%State(2), &
               V3 => HS%State(3), V4 => HS%State(4))
        IF (HS%GetBlockCount() == 0_kIndex) THEN
            V1 = V1 + XXH_PRIME32_1 + XXH_PRIME32_2
            V2 = V2 + XXH_PRIME32_2
            V4 = V4 - XXH_PRIME32_1
        END IF
        XXH32_Round(V1, Pack_I32(BytesIn, 0_kIndex))
        XXH32_Round(V2, Pack_I32(BytesIn, 4_kIndex))
        XXH32_Round(V3, Pack_I32(BytesIn, 8_kIndex))
        XXH32_Round(V4, Pack_I32(BytesIn, 12_kIndex))
    END ASSOCIATE

    RETURN

END SUBROUTINE XX_ProcessBlock

!******************************************************************************

SUBROUTINE XX_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XXHasher32), TARGET, INTENT(INOUT)    :: HS   !! a hasher (HS) object
    tUInt32,                   INTENT(IN)       :: Seed !! seed
    tLogical,        OPTIONAL, INTENT(IN)       :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    HS%State = Seed
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE XX_Initialize

!******************************************************************************

FUNCTION XX_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 32-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XXHasher32), INTENT(INOUT)    :: HS       !! a hasher (HS) object
    tUInt32                             :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length, Remaining, Offset

!** FLOW
    
    ! initialize
    Remaining = HS%GetBufLen()
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        Length   = HS%GetBlockCount()*HS%GetBlockLength() + Remaining
        HashCode = RotateLeft(HS%State(1), 1)  + RotateLeft(HS%State(2), 7) + &
                   RotateLeft(HS%State(3), 12) + RotateLeft(HS%State(4), 18)
    ELSE
        Length   = Remaining
        HashCode = HS%State(3) + XXH_PRIME32_5
    END IF

    HashCode = HashCode + ToInt32(Length)

    ! XXH32_finalize
    Offset = 0_kIndex
    DO WHILE (Remaining >= 4_kIndex)
        HashCode = HashCode + Pack_I32(HS%BufArr, Offset)*XXH_PRIME32_3
        HashCode = RotateLeft(HashCode, 17)*XXH_PRIME32_4
        Offset = Offset + 4_kIndex
        Remaining = Remaining - 4_kIndex
    END DO

    DO WHILE (Remaining /= 0_kIndex)
        HashCode = HashCode + Pack_U8(HS%BufArr, Offset)*XXH_PRIME32_5
        HashCode = RotateLeft(HashCode, 11)*XXH_PRIME32_1
        Offset = Offset + 1_kIndex
        Remaining = Remaining - 1_kIndex
    END DO
        
    ! XXH32_avalanche
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 15))
    HashCode = HashCode*XXH_PRIME32_2
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 13))
    HashCode = HashCode*XXH_PRIME32_3
    HashCode = IEOR(HashCode, SHIFTR(HashCode, 16))
    
    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInt32(Z'7FFFFFFF'))

    ! reset the hasher
    HS%State  = 0_kInt32
    HS%BufArr = 0_kInt8
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

END FUNCTION XX_Finalize

!******************************************************************************

FUNCTION XX_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(XXHasher32),      INTENT(INOUT)   :: HS           !! a hasher (HS) object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tUInt32,  OPTIONAL,     INTENT(IN)      :: Seed         !! seed
    tLogical, OPTIONAL,     INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tUInt32                                 :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32     :: Seed0

!** FLOW

    SET_OPTION(Seed0, 0_kInt32, Seed)
    
    ! The following code illustrates simple use of the hasher.
    CALL HS%Initialize(Seed0, RemoveSign)
    CALL HS%Update(Input, InpSize)
    HashCode = HS%Finalize()

    RETURN

END FUNCTION XX_HashDirect

!******************************************************************************

END MODULE MClass_XXHasher32

!******************************************************************************
