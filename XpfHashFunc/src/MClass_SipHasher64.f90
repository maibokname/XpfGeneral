
MODULE MClass_SipHasher64

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SipHasher64* type and its related routines.
!   The *SipHasher64* type is a hasher type that extends directly from the
!   <a href="../module/mclass_hasher64.html#type-hasher64">Hasher64</a> type.
!   It provides all deferred procedures required by a *Hasher64* class and
!   outputs the hash value as a 64-bit integer. <br>
!   The *SipHasher64* type employs the *Sip* hash algorithm for 64-bit integer
!   output by Jean-Philippe Aumasson [1].  As a hasher, it can be used to compute
!   the hash value incrementally.  It also provides a method to compute the hash
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
!   [1] <a href="https://github.com/veorq/SipHash">SipHash: high-speed secure pseudorandom
!       function for short messages. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_Hasher64
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: SipHasher64

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define     MaskI64(X)      IAND(ToInt64(X), ToInt64(Z'00000000000000FF'))
#define     SipRound(A,B,C,D) \
    A = A + B; \
    B = RotateLeft(B, 13); \
    B = IEOR(B, A); \
    A = RotateLeft(A, 32); \
    C = C + D; \
    D = RotateLeft(D, 16); \
    D = IEOR(D, C); \
    A = A + D; \
    D = RotateLeft(D, 21); \
    D = IEOR(D, A); \
    C = C + B; \
    B = RotateLeft(B, 17); \
    B = IEOR(B, C); \
    C = RotateLeft(C, 32);

!** MODULE PARAMETERS:
    tUInt64, PARAMETER  :: IV0 = ToInt64(Z'736F6D6570736575')
    tUInt64, PARAMETER  :: IV1 = ToInt64(Z'646F72616E646F6D')
    tUInt64, PARAMETER  :: IV2 = ToInt64(Z'6C7967656E657261')
    tUInt64, PARAMETER  :: IV3 = ToInt64(Z'7465646279746573')
    tUInt8, PARAMETER   :: DefaultKey(16) = [                               &
                ToInt8(Z'F7'), ToInt8(Z'43'), ToInt8(Z'24'), ToInt8(Z'8E'), &
                ToInt8(Z'E0'), ToInt8(Z'35'), ToInt8(Z'90'), ToInt8(Z'E6'), &
                ToInt8(Z'81'), ToInt8(Z'3A'), ToInt8(Z'26'), ToInt8(Z'4C'), &
                ToInt8(Z'3C'), ToInt8(Z'28'), ToInt8(Z'52'), ToInt8(Z'BB')]
    tIndex,  PARAMETER  :: BlockLen = 8_kIndex

!** DERIVED TYPE DEFINITIONS
    !> *SipHasher64* is a hasher type that outputs the hash value as a 64-bit integer.
    !  It employs the *Sip* hash algorithm by Jean-Philippe Aumasson.
    TYPE, EXTENDS(Hasher64) :: SipHasher64
        PRIVATE
        !% state
        tUInt64     :: State(4)             = 0_kInt64
        !% number of C rounds
        tIndex      :: cRound               = 2_kIndex
        !% number of D rounds
        tIndex      :: dRound               = 4_kIndex
        !% buffer array used to store input data
        tUInt8      :: BufArr(0:BlockLen-1) = 0_kInt8
        !% flag indicating whether to remove sign from the final hash value
        tLogical    :: RemoveSign   = FalseVal
    CONTAINS
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE   :: GetName          => Sip_GetName
        !> *GetBlockLength* is a procedure to return the block length of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockLength   => Sip_BlockLength
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: SetBufPtr        => Sip_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this hasher. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: ProcessBlock     => Sip_ProcessBlock
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the hasher with default keys and rounds (i.e. Sip24). <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Initialize(Seed)            ! hash value with sign <br>
        !   --->    CALL Hasher%Initialize(Seed, .TRUE.)    ! remove sign from hash value <br>
        PROCEDURE   :: Initialize       => Sip_Initialize
        !> **Type-Bound Subroutine**: InitializeWKey <br>
        !  **Purpose**:  To initialize the hasher with keys and optionally the number of rounds. <br>
        !  **Usage**: <br>
        !   ! hash value (with default rounds and sign) <br>
        !   --->    CALL Hasher%InitializeWKey(Seed, Key) <br>
        !   ! hash value with specified rounds <br>
        !   --->    CALL Hasher%InitializeWKey(Seed, Key, cRound=1, dRound=3) <br>
        !   ! hash value with specified remove sign flag <br>
        !   --->    CALL Hasher%InitializeWKey(Seed, Key, RemoveSign=.TRUE.) <br>
        PROCEDURE   :: InitializeWKey   => Sip_Initialize_wKey
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 64-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE   :: Finalize         => Sip_Finalize
        !> **Type-Bound Function**: HashDirect <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally) with optional
        !                seed and remove sign flag (and default key and number of rounds). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirect       => Sip_HashDirect
        !> **Type-Bound Function**: HashDirectWKey <br>
        !  **Purpose**:  To compute the hash value directly with specified seed and keys
        !                and optionally the number of rounds. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirectWKey(Input, InpSize, Seed, Key) <br>
        !   --->    HashCode = Hasher%HashDirectWKey(Input, InpSize, Seed, Key, cRound=2, dRound=4) <br>
        !   --->    HashCode = Hasher%HashDirectWKey(Input, InpSize, Seed, Key, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE   :: HashDirectWKey   => Sip_HashDirect_wKey
    END TYPE SipHasher64

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

FUNCTION Sip_GetName(HS) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the display name for this hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SipHasher64), INTENT(IN)  :: HS   !! a hasher (HS) object
    tCharAlloc                      :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Name = 'Sip_Hahser64'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Sip_GetName

!******************************************************************************

FUNCTION Sip_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SipHasher64), INTENT(IN)  :: HS       !! a hasher (HS) object
    tIndex                          :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Sip_BlockLength

!******************************************************************************

SUBROUTINE Sip_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SipHasher64), TARGET, INTENT(INOUT)   :: HS           !! a hasher (HS) object
    tUInt8,            POINTER, INTENT(INOUT)   :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => HS%BufArr

    RETURN

END SUBROUTINE Sip_SetBufPtr

!******************************************************************************

SUBROUTINE Sip_ProcessBlock(HS, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SipHasher64), INTENT(INOUT)   :: HS           !! a hasher (HS) object
    tUInt8,             INTENT(IN)      :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: M
    tIndex      :: I

!** FLOW

    ASSOCIATE (V0 => HS%State(1), V1 => HS%State(2), &
               V2 => HS%State(3), V3 => HS%State(4))
        M = PackFull(BytesIn, 0_kIndex)
        V3 = IEOR(V3, M)
        DO I = 1, HS%cRound
            SipRound(V0, V1, V2, V3)
        END DO
        V0 = IEOR(V0, M)
    END ASSOCIATE

    RETURN

END SUBROUTINE Sip_ProcessBlock

!******************************************************************************

SUBROUTINE Sip_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SipHasher64), TARGET, INTENT(INOUT)   :: HS   !! a hasher (HS) object
    tUInt64,                    INTENT(IN)      :: Seed !! seed
    tLogical,         OPTIONAL, INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    CALL HS%InitializeWKey(Seed, DefaultKey, RemoveSign=RemoveSign)

    RETURN

END SUBROUTINE Sip_Initialize

!******************************************************************************

SUBROUTINE Sip_Initialize_wKey(HS, Seed, Key, cRound, dRound, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher with keys and optionally the number of rounds.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SipHasher64), TARGET, INTENT(INOUT)   :: HS           !! a hasher (HS) object
    tUInt64,                    INTENT(IN)      :: Seed         !! seed
    tUInt8,                     INTENT(IN)      :: Key(0:15)    !! key bytes
    tIndex,           OPTIONAL, INTENT(IN)      :: cRound       !! number of C rounds
    tIndex,           OPTIONAL, INTENT(IN)      :: dRound       !! number of D rounds
    tLogical,         OPTIONAL, INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: K0, K1

!** FLOW

    ASSOCIATE (V0 => HS%State(1), V1 => HS%State(2), &
               V2 => HS%State(3), V3 => HS%State(4))
        K0 = PackFull(Key, 0_kIndex)
        K1 = PackFull(Key, 8_kIndex)
        V0 = IEOR(IV0, K0)
        V1 = IEOR(IV1, K1) + Seed
        V2 = IEOR(IV2, K0)
        V3 = IEOR(IV3, K1)
    END ASSOCIATE
    SET_OPTION(HS%cRound, 2_kIndex, cRound)
    SET_OPTION(HS%dRound, 4_kIndex, dRound)
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE Sip_Initialize_wKey

!******************************************************************************

FUNCTION Sip_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 64-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SipHasher64), INTENT(INOUT)   :: HS       !! a hasher (HS) object
    tUInt64                             :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt64     :: B
    tIndex      :: Length, Remaining, I

!** FLOW

    ! initialize
    Remaining = HS%GetBufLen()
    IF (HS%GetBlockCount() > 0_kIndex) THEN
        Length = HS%GetBlockCount()*HS%GetBlockLength() + Remaining
    ELSE
        Length = Remaining
    END IF

    ! process remaining bytes
    B  = SHIFTL(ToInt64(Length), 56)
    IF (Remaining > 0) B = IOR(B, PackPartial(HS%BufArr, 0_kIndex, Remaining))

    ASSOCIATE (V0 => HS%State(1), V1 => HS%State(2), &
               V2 => HS%State(3), V3 => HS%State(4))
        ! mix states with B
        V3 = IEOR(V3, B)
        DO I = 1, HS%cRound
            SipRound(V0, V1, V2, V3)
        END DO
        V0 = IEOR(V0, B)

        ! final mix
        V2 = IEOR(V2, ToInt64(Z'00000000000000FF'))
        DO I = 1, HS%dRound
            SipRound(V0, V1, V2, V3)
        END DO
        HashCode = IEOR(IEOR(IEOR(V0, V1), V2), V3)
    END ASSOCIATE

    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInt64(Z'7FFFFFFFFFFFFFFF'))

    ! reset the hasher
    HS%State  = 0_kInt64
    HS%BufArr = 0_kInt8
    HS%cRound = 2_kIndex
    HS%dRound = 4_kIndex
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

END FUNCTION Sip_Finalize

!******************************************************************************

FUNCTION Sip_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental) without seed or with one seed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SipHasher64),     INTENT(INOUT)   :: HS           !! a hasher (HS) object
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

END FUNCTION Sip_HashDirect

!******************************************************************************

FUNCTION Sip_HashDirect_wKey(HS, Input, InpSize, Seed, Key, cRound, &
                             dRound, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly with specified seed and keys
    !  and optionally the number of rounds.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SipHasher64),     INTENT(INOUT)   :: HS           !! a hasher (HS) object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tUInt64,                INTENT(IN)      :: Seed         !! seed
    tUInt8,                 INTENT(IN)      :: Key(0:15)    !! key bytes
    tIndex,       OPTIONAL, INTENT(IN)      :: cRound       !! number of C rounds
    tIndex,       OPTIONAL, INTENT(IN)      :: dRound       !! number of D rounds
    tLogical,     OPTIONAL, INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.
    tUInt64                                 :: HashCode     !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    ! The following code illustrates simple use of the hasher.
    CALL HS%InitializeWKey(Seed, Key, cRound, dRound, RemoveSign)
    CALL HS%Update(Input, InpSize)
    HashCode = HS%Finalize()

    RETURN

END FUNCTION Sip_HashDirect_wKey

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                   OPTIMAL VERSIONS OF AUXILIARY ROUTINES                    +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FUNCTION PackFull(ByteArr, Offset) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To convert an array of 8-bit integers starting at the offset to
    !  a 64-bit integer value using the *SHIFTL* and *IOR* intrinsic
    !  functions.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: ByteArr(0:)  !! byte array
    tIndex, INTENT(IN)  :: Offset       !! offset
    tUInt64             :: Res          !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! little-endian order
    Res = IOR(IOR(IOR(IOR(IOR(IOR(IOR(       MaskI64(ByteArr(Offset)),          &
                                      SHIFTL(MaskI64(ByteArr(Offset+1)),  8)),  &
                                      SHIFTL(MaskI64(ByteArr(Offset+2)), 16)),  &
                                      SHIFTL(MaskI64(ByteArr(Offset+3)), 24)),  &
                                      SHIFTL(MaskI64(ByteArr(Offset+4)), 32)),  &
                                      SHIFTL(MaskI64(ByteArr(Offset+5)), 40)),  &
                                      SHIFTL(MaskI64(ByteArr(Offset+6)), 48)),  &
                                      SHIFTL(MaskI64(ByteArr(Offset+7)), 56))

    ! big-endian order
!    Res = IOR(IOR(IOR(IOR(IOR(IOR(IOR(SHIFTL(MaskI64(ByteArr(Offset)),   56),  &
!                                      SHIFTL(MaskI64(ByteArr(Offset+1)), 48)), &
!                                      SHIFTL(MaskI64(ByteArr(Offset+2)), 40)), &
!                                      SHIFTL(MaskI64(ByteArr(Offset+3)), 32)), &
!                                      SHIFTL(MaskI64(ByteArr(Offset+4)), 24)), &
!                                      SHIFTL(MaskI64(ByteArr(Offset+5)), 16)), &
!                                      SHIFTL(MaskI64(ByteArr(Offset+6)),  8)), &
!                                             MaskI64(ByteArr(Offset+7)))

    RETURN

END FUNCTION PackFull

!**************************************************************************

FUNCTION PackPartial(Buf, Off, Length) RESULT(Res)

!** PURPOSE OF THIS SUBROUTINE:
    ! To pack three or fewer bytes of the array 'Buf' at offset 'Off'
    ! into the 64-bit word 'Res'.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tUInt8, INTENT(IN)  :: Buf(0:)  !! buffer
    tIndex, INTENT(IN)  :: Off      !! offset
    tIndex, INTENT(IN)  :: Length   !! the number of bytes to pack (between 1 to 7)
    tUInt64             :: Res      !! result

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8      :: Wrk(0:7)

! FLOW

    ! initialize
    Wrk = 0_kInt8

    ! gather available bytes in little-endian order
    Wrk(0:Length-1) = Buf(Off:Off+Length-1)

    ! gather available bytes in big-endian order
!    Wrk(Length-1:0:-1) = Buf(Off:Off+Length-1)
    
    ! pack bytes into word
    Res = PackFull(Wrk, 0_kIndex)

    RETURN

END FUNCTION PackPartial

!******************************************************************************

END MODULE MClass_SipHasher64

!******************************************************************************
