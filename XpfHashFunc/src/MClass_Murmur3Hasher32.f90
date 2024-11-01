
MODULE MClass_Murmur3Hasher32

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Murmur3Hasher32* type and its related routines.
!   The *Murmur3Hasher32* type is a hasher type that extends directly from the
!   <a href="../module/mclass_hasher32.html#type-hasher32">Hasher32</a> type.
!   It provides all deferred procedures required by a *Hasher32* class and
!   outputs the hash value as a 32-bit integer. <br>
!   The *Murmur3Hasher32* type employs the *Murmur3* hash algorithm for 32-bit
!   integer output by Austin Appleby [1].  As a hasher, it can be used to compute
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
!   [1] <a href="https://github.com/aappleby/smhasher">SMHasher: a test suite designed to
!       test the distribution, collision, and performance properties of non-cryptographic
!       hash functions. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_Hasher32
    
    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Murmur3Hasher32

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../../MacroDef/Macro - Util Definitions.f90"
#define FinalMixing(H) \
    H = IEOR(H, SHIFTR(H, 16)); \
    H = H*ToInt32(Z'85EBCA6B'); \
    H = IEOR(H, SHIFTR(H, 13)); \
    H = H*ToInt32(Z'C2B2AE35'); \
    H = IEOR(H, SHIFTR(H, 16));
#define MaskInt32(X)          IAND(ToInt32(X), Z'000000FF')
#define UnsignedShort(Val, Off) IOR(MaskInt32(Val(Off)), SHIFTL(MaskInt32(Val(Off+1)), 8))
#define PackFull(Buf, Off)      IOR(UnsignedShort(Buf, Off), SHIFTL(UnsignedShort(Buf, Off+2), 16))

!** MODULE PARAMETERS:
    tUInt32,   PARAMETER    :: C1 = ToInt32(Z'CC9E2D51')
    tUInt32,   PARAMETER    :: C2 = ToInt32(Z'1B873593')

!** DERIVED TYPE DEFINITIONS
    !> *Murmur3Hasher32* is a hasher type that outputs the hash value as a 32-bit integer.
    !  It employs the *Murmur3* hash algorithm by Austin Appleby.
    TYPE, EXTENDS(Hasher32) :: Murmur3Hasher32
        PRIVATE
        !% state
        tUInt32     :: State       = 0_kInt32
        !% buffer array used to store input data
        tUInt8      :: BufArr(0:3) = 0_kInt8
        !% flag indicating whether to remove sign from the final hash value
        tLogical    :: RemoveSign  = FalseVal
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
        !                in a 32-bit integer.  The object is reset. <br>
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
    END TYPE Murmur3Hasher32

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
    CLASS(Murmur3Hasher32), INTENT(IN)  :: HS   !! a hasher (HS) object
    tCharAlloc                          :: Name !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Name = 'Murmur3_Hahser32'
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Murmur3_GetName

!******************************************************************************

FUNCTION Murmur3_BlockLength(HS) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the *block length* for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher32), INTENT(IN)  :: HS       !! a hasher (HS) object
    tIndex                              :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    Length = 4_kIndex
    ASSOCIATE(Dummy => HS); END ASSOCIATE

    RETURN

END FUNCTION Murmur3_BlockLength

!******************************************************************************

SUBROUTINE Murmur3_SetBufPtr(HS, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher32), TARGET, INTENT(INOUT)   :: HS           !! a hasher (HS) object
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
    CLASS(Murmur3Hasher32), INTENT(INOUT)   :: HS           !! a hasher (HS) object
    tUInt8,                 INTENT(IN)      :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt32     :: K1

!** FLOW

    ASSOCIATE (H1 => HS%State)
        ! get input
        K1 = PackFull(BytesIn, 0_kIndex)
        ! mixing input with constants
        K1 = K1*C1
        K1 = RotateLeft(K1, 15)
        K1 = K1*C2
        H1 = IEOR(H1, K1)
        H1 = RotateLeft(H1, 13) 
        H1 = H1*5 + ToInt32(Z'E6546B64')
    END ASSOCIATE

    RETURN

END SUBROUTINE Murmur3_ProcessBlock

!******************************************************************************

SUBROUTINE Murmur3_Initialize(HS, Seed, RemoveSign)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize the hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher32), TARGET, INTENT(INOUT)   :: HS   !! a hasher (HS) object
    tUInt32,                        INTENT(IN)      :: Seed !! seed
    tLogical,             OPTIONAL, INTENT(IN)      :: RemoveSign
    !^ flag indicating whether to remove sign bit or not. <br>
    !  - If true, always returns a positive value of the hash code. <br>
    !  - If false, returns either a positive or negative value of the hash code. <br>
    !  - default is false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    HS%State  = Seed
    SET_OPTION(HS%RemoveSign, FalseVal, RemoveSign)
    CALL HS%Reset()

    RETURN

END SUBROUTINE Murmur3_Initialize

!******************************************************************************

FUNCTION Murmur3_Finalize(HS) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  in a 32-bit integer.  The object is reset.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher32), INTENT(INOUT)   :: HS       !! a hasher (HS) object
    tUInt32                                 :: HashCode !! hash code

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length, Remaining
    tUInt32     :: K1

!** FLOW
    
    ! initialize
    Remaining = HS%GetBufLen()
    Length    = HS%GetBlockCount()*HS%GetBlockLength() + Remaining
    HashCode  = HS%State
    
    ! process remaining bytes
    IF (Remaining > 0) THEN
        K1 = Pack_Partial(HS%BufArr, 0_kIndex, Remaining)
        K1 = K1*C1
        K1 = RotateLeft(K1, 15)
        K1 = K1*C2
        HashCode = IEOR(HashCode, K1)
    END IF
        
    ! finalize the hash value
    HashCode = ToInt32(IEOR(ToIndex(HashCode), Length))
    FinalMixing(HashCode)
    
    ! remove sign if needed
    IF (HS%RemoveSign) HashCode = IAND(HashCode, ToInt32(Z'7FFFFFFF'))

    ! reset the hasher
    HS%State  = 0_kInt32
    HS%BufArr = 0_kInt8
    HS%RemoveSign = FalseVal
    CALL HS%Reset()

    RETURN

CONTAINS

    FUNCTION Pack_Partial(Buf, Off, Length) RESULT(Res)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To pack seven or fewer bytes of the array 'Buf' at offset 'Off'
        ! into the 64-bit word 'Res', in little-endian convention
        ! (least significant byte first).

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tUInt8, INTENT(IN)  :: Buf(0:)  ! buffer
        tIndex, INTENT(IN)  :: Off      ! offset
        tIndex, INTENT(IN)  :: Length   ! the number of bytes to pack (between 1 to 3)
        tUInt32             :: Res      ! result

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tUInt8      :: Wrk(0:3)

    ! FLOW
        
        Wrk(0:Length-1) = Buf(Off:Off+Length-1)
        Wrk(Length:3)   = 0_kInt8
        Res = PackFull(Wrk, 0)

        RETURN

    END FUNCTION Pack_Partial

    !**************************************************************************

END FUNCTION Murmur3_Finalize

!******************************************************************************

FUNCTION Murmur3_HashDirect(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To compute the hash value directly (non-incremental).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Murmur3Hasher32), INTENT(INOUT)   :: HS           !! a hasher (HS) object
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

END FUNCTION Murmur3_HashDirect

!******************************************************************************

END MODULE MClass_Murmur3Hasher32

!******************************************************************************
