
MODULE MClass_HalfSip24Hasher32

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *HalfSip24Hasher32* type and its related routines.
!   The *HalfSip24Hasher32* type is a hasher type that extends directly from the
!   <a href="../module/mclass_hasher32.html#type-hasher32">Hasher32</a> type.
!   It provides all deferred procedures required by a *Hasher32* class and
!   outputs the hash value as a 32-bit integer. <br>
!   The *HalfSip24Hasher32* type employs the *HalfSip24* hash algorithm for
!   32-bit integer output by Jean-Philippe Aumasson [1].  As a hasher, it can be
!   used to compute the hash value incrementally.  It also provides a method to
!   compute the hash value directly (i.e. non-incrementally).  The following code
!   snippet illustrates a typical usage of the hasher.
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
!   [1] <a href="https://github.com/veorq/SipHash">SipHash: high-speed secure pseudorandom
!       function for short messages. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_Hasher32
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: HalfSip24Hasher32

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define MaskI32(X)      IAND(ToInt64(X), ToInt64(Z'00000000FFFFFFFF'))
#define SipRound(V0, V1, V2, V3) \
    V0 = V0 + V1; \
    V1 = RotateLeft(V1, 5); \
    V1 = IEOR(V1,  V0); \
    V0 = RotateLeft(V0, 16); \
    V2 = V2 + V3; \
    V3 = RotateLeft(V3, 8); \
    V3 = IEOR(V3,  V2); \
    V0 = V0 + V3; \
    V3 = RotateLeft(V3, 7); \
    V3 = IEOR(V3,  V0); \
    V2 = V2 + V1; \
    V1 = RotateLeft(V1, 13); \
    V1 = IEOR(V1,  V2); \
    V2 = RotateLeft(V2, 16);
#define MaskInt32(X)          IAND(ToInt32(X), Z'000000FF')
#define UnsignedShort(Val, Off) IOR(MaskInt32(Val(Off)), SHIFTL(MaskInt32(Val(Off+1)), 8))
#define PackFull(Buf, Off)      IOR(UnsignedShort(Buf, Off), SHIFTL(UnsignedShort(Buf, Off+2), 16))

!** MODULE PARAMETERS:
    tUInt8, PARAMETER   :: DefaultKey(8) = [ToInt8(Z'AF'), ToInt8(Z'D7'), &
                                            ToInt8(Z'FB'), ToInt8(Z'CA'), &
                                            ToInt8(Z'BB'), ToInt8(Z'4B'), &
                                            ToInt8(Z'40'), ToInt8(Z'7E')]

!** DERIVED TYPE DEFINITIONS
    !> *HalfSip24Hasher32* is a hasher type that outputs the hash value as a 32-bit integer.
    !  It employs the *HalfSip24* hash algorithm by Jean-Philippe Aumasson.
    TYPE, EXTENDS(Hasher32) :: HalfSip24Hasher32
        PRIVATE
        !% state
        tUInt32     :: State(4)    = 0_kInt32
        !% buffer array used to store input data
        tUInt8      :: BufArr(0:3) = 0_kInt8
        !% flag indicating whether to remove sign from the final hash value
        tLogical    :: RemoveSign  = FalseVal
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName          => HalfSip24_GetName
        !> *GetBlockLength* is a procedure to return the block length of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockLength   => HalfSip24_BlockLength
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: SetBufPtr        => HalfSip24_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: ProcessBlock     => HalfSip24_ProcessBlock
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the hasher. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Initialize(Seed)            ! hash value with sign <br>
        !   --->    CALL Hasher%Initialize(Seed, .TRUE.)    ! remove sign from hash value <br>
        PROCEDURE   :: Initialize       => HalfSip24_Initialize
        !> **Type-Bound Subroutine**: InitializeWKey <br>
        !  **Purpose**:  To initialize the hasher with a key. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%InitializeWKey(Seed, Key)           ! hash value with sign <br>
        !   --->    CALL Hasher%InitializeWKey(Seed, Key, .TRUE.)   ! remove sign from hash value <br>
        PROCEDURE   :: InitializeWKey   => HalfSip24_Initialize_wKey
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 32-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize         => HalfSip24_Finalize
        !> **Type-Bound Function**: HashDirect <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect       => HalfSip24_HashDirect
    END TYPE HalfSip24Hasher32

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION HalfSip24_GetName(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HalfSip24Hasher32), INTENT(IN)    :: HS   !! a hasher (HS) object
    tCharAlloc                              :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Name = 'HalfSip24_Hahser32'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION HalfSip24_GetName

!******************************************************************************

FUNCTION HalfSip24_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HalfSip24Hasher32), INTENT(IN)    :: HS       !! a hasher (HS) object
    tIndex                                  :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Length = 4_kIndex
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION HalfSip24_BlockLength

!******************************************************************************

SUBROUTINE HalfSip24_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HalfSip24Hasher32), TARGET, INTENT(INOUT) :: HS           !! a hasher (HS) object
    tUInt8,                  POINTER, INTENT(INOUT) :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => HS%BufArr

    RETURN

END SUBROUTINE HalfSip24_SetBufPtr

!******************************************************************************

SUBROUTINE HalfSip24_ProcessBlock(HS, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HalfSip24Hasher32), INTENT(INOUT) :: HS           !! a hasher (HS) object
    tUInt8,                   INTENT(IN)    :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32     :: Mi

!** FLOW

    ASSOCIATE (V0 => HS%State(1), V1 => HS%State(2), &
               V2 => HS%State(3), V3 => HS%State(4))
        Mi = PackFull(BytesIn, 0_kIndex)
        V3 = IEOR(V3, Mi)
        SipRound(V0, V1, V2, V3)
        SipRound(V0, V1, V2, V3)
        V0 = IEOR(V0, Mi)
    END ASSOCIATE

    RETURN

END SUBROUTINE HalfSip24_ProcessBlock

!******************************************************************************

SUBROUTINE HalfSip24_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HalfSip24Hasher32), TARGET, INTENT(INOUT) :: HS   !! a hasher (HS) object
    tUInt32,                          INTENT(IN)    :: Seed !! seed
    tLogical,               OPTIONAL, INTENT(IN)    :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL HS%InitializeWKey(Seed, DefaultKey, RemoveSign)

    RETURN

END SUBROUTINE HalfSip24_Initialize

!******************************************************************************

SUBROUTINE HalfSip24_Initialize_wKey(HS, Seed, Key, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HalfSip24Hasher32), TARGET, INTENT(INOUT) :: HS       !! a hasher (HS) object
    tUInt32,                          INTENT(IN)    :: Seed     !! seed
    tUInt8,                           INTENT(IN)    :: Key(0:7) !! key bytes
    tLogical,               OPTIONAL, INTENT(IN)    :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32     :: K0, K1

!** FLOW

    ASSOCIATE (V0 => HS%State(1), V1 => HS%State(2), &
               V2 => HS%State(3), V3 => HS%State(4))
        V0 = 0
        V1 = Seed
        V2 = ToInt32(Z'6C796765')
        V3 = ToInt32(Z'74656462')
        K0 = PackFull(Key, 0)
        K1 = PackFull(Key, 4)
        V3 = IEOR(V3, K1)
        V2 = IEOR(V2, K0)
        V1 = IEOR(V1, K1)
        V0 = IEOR(V0, K0)
    END ASSOCIATE
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE HalfSip24_Initialize_wKey

!******************************************************************************

FUNCTION HalfSip24_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 32-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HalfSip24Hasher32), INTENT(INOUT) :: HS       !! a hasher (HS) object
    tUInt32                                 :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32     :: B
    tIndex      :: Length, Remaining

!** FLOW
    
    ! initialize
    Remaining = HS%GetBufLen()
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        Length   = HS%GetBlockCount()*HS%GetBlockLength() + Remaining
    ELSE
        Length   = Remaining
    END IF
    B  = SHIFTL(ToInt32(Length), 24)

    ! process remaining
    IF (Remaining > 0_kIndex) B = B + Pack_Partial(HS%BufArr, Remaining)

    ! finalize
    ASSOCIATE (V0 => HS%State(1), V1 => HS%State(2), &
               V2 => HS%State(3), V3 => HS%State(4))
        V3 = IEOR(V3, B)
        SipRound(V0, V1, V2, V3)
        SipRound(V0, V1, V2, V3)
        V0 = IEOR(V0, B)
        V2 = IEOR(V2, Z'000000FF')
        SipRound(V0, V1, V2, V3)
        SipRound(V0, V1, V2, V3)
        SipRound(V0, V1, V2, V3)
        SipRound(V0, V1, V2, V3)
        HashCode = IEOR(V1, V3)
    END ASSOCIATE
    
    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInt32(Z'7FFFFFFF'))

    ! reset the hasher
    HS%State  = 0_kInt32
    HS%BufArr = 0_kInt8
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

CONTAINS

    FUNCTION Pack_Partial(Buf, Length) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To pack seven or fewer bytes of the array 'Buf' at offset 'Off'
        ! into the 64-bit word 'Res', in little-endian convention
        ! (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt8, INTENT(INOUT)   :: Buf(0:)  ! buffer
        tIndex, INTENT(IN)      :: Length   ! the number of bytes to pack (between 1 to 3)
        tUInt32                 :: Res      ! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW
        
        Buf(Length:3) = 0_kInt8
        Res = PackFull(Buf, 0_kIndex)

        RETURN

    END FUNCTION Pack_Partial

    !**************************************************************************

END FUNCTION HalfSip24_Finalize

!******************************************************************************

FUNCTION HalfSip24_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HalfSip24Hasher32), INTENT(INOUT) :: HS           !! a hasher (HS) object
    TYPE(*), CONTIGUOUS,      INTENT(IN)    :: Input(..)    !! input data (any type and rank)
    tIndex,                   INTENT(IN)    :: InpSize      !! size of the input (in bytes)
    tUInt32,  OPTIONAL,       INTENT(IN)    :: Seed         !! seed
    tLogical, OPTIONAL,       INTENT(IN)    :: RemoveSign
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

END FUNCTION HalfSip24_HashDirect

!******************************************************************************

END MODULE MClass_HalfSip24Hasher32

!******************************************************************************
