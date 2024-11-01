
MODULE MClass_Blake3

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Blake3* type and its related routines.
!   The *Blake3* type is a *digest* type extending directly from the
!   <a href="../module/mclass_basedigest.html#type-basedigest">BaseDigest</a>
!   type.  It implements all deferred procedures required by a digest type. <br>
!   The *Blake3* type implements an incremental cryptographic hash function
!   by employing the *BLAKE3 message-digest* algorithm [1].  The implementation
!   here is based mainly on the official implementation in C [2]. <br>
!   Similar to the *Blake2-based* digest types, the *Blake3* type can perform
!   keyed hashing providing that a user specifies the key during an initialization
!   of the digest object by calling the *CreateHMAC* method in place of the *Create*
!   method.  In addition, the *Blake3* type can be used as a key derivative function
!   (KDF) by specifying the context string during an initialization of the digest
!   object with the *CreateKDF* method.  Furthermore, the *Blake3* type can be used
!   as an extendable-output function (XOF) by specifying the hash output length
!   during a finalization of the digest object where the *DigestWOutLen* method
!   is called. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://github.com/BLAKE3-team/BLAKE3-specs">The BLAKE3 paper:
!       specifications, analysis, and design rationale. </a> <br>
!   [2] <a href="https://github.com/BLAKE3-team/BLAKE3">Blake3: The official Rust
!       and C implementations of BLAKE3 cryptographic hash function. </a> <br>

!** USE STATEMENTS:
    USE, INTRINSIC  :: ISO_C_BINDING,   ONLY: C_SIZEOF
    USE MBase_Common
    USE MBase_MemHandlers,              ONLY: MemAlloc
    USE MBase_ByteUtil,                 ONLY: AnyType_2_ByteArrPtr, &
                                              ByteArr_2_HexStr => ToHexStr_BE
    USE MBase_BytePack,                 ONLY: BytePackLE, ByteUnpackLE
    USE MClass_BaseDigest

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Blake3

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tCharParam          :: ModName = 'MClass_Blake3'
    tIndex,   PARAMETER :: BLAKE3_KEY_LEN   = 8_kIndex
    tIndex,   PARAMETER :: BLAKE3_OUT_LEN   = 32_kIndex
    tIndex,   PARAMETER :: BLAKE3_BLOCK_LEN = 64_kIndex
    tIndex,   PARAMETER :: BLAKE3_CHUNK_LEN = 1024_kIndex
    tIndex,   PARAMETER :: BLAKE3_MAX_DEPTH = 54_kIndex
    !---------------------------------------------------------------------
    tByte,    PARAMETER :: CHUNK_START         = SHIFTL(1_kInt8, 0)
    tByte,    PARAMETER :: CHUNK_END           = SHIFTL(1_kInt8, 1)
    tByte,    PARAMETER :: PARENT              = SHIFTL(1_kInt8, 2)
    tByte,    PARAMETER :: ROOT                = SHIFTL(1_kInt8, 3)
    tByte,    PARAMETER :: KEYED_HASH          = SHIFTL(1_kInt8, 4)
    tByte,    PARAMETER :: DERIVE_KEY_CONTEXT  = SHIFTL(1_kInt8, 5)
    tByte,    PARAMETER :: DERIVE_KEY_MATERIAL = SHIFTL(1_kInt8, 6)
    !---------------------------------------------------------------------
    tInteger, PARAMETER    :: IV(0:7) = [                   &
            ToInt32(Z'6A09E667'), ToInt32(Z'BB67AE85'), &
            ToInt32(Z'3C6EF372'), ToInt32(Z'A54FF53A'), &
            ToInt32(Z'510E527F'), ToInt32(Z'9B05688C'), &
            ToInt32(Z'1F83D9AB'), ToInt32(Z'5BE0CD19')]
    !---------------------------------------------------------------------

!** DERIVED TYPE DEFINITIONS
    !> *ChunkState* is a helper object and a private type.
    TYPE ChunkState
        !% chaining value
        tInteger    :: CV(0:BLAKE3_KEY_LEN-1)
        !% buffer
        tByte       :: Buf(0:BLAKE3_BLOCK_LEN-1)
        !% number of bytes used in the buffer
        tByte       :: BufLen
        !% counter
        tLong       :: Counter
        !% flags
        tByte       :: Flags
        !% the number of block compressed
        tByte       :: BlocksCompressed
    END TYPE ChunkState
    !> *Blake3* is a concrete *digest* type that implements an incremental cryptographic
    !  hash function by employing the *BLAKE3 message-digest* algorithm.  It can also be
    !  utilized as a hash-based message authentication code (HMAC).  It can be used as
    !  a key derivation function (KDF) as well.  In addition, it can be employed as an
    !  extendable-output function (XOF) to output the hash value with a desired length.
    TYPE, EXTENDS(BaseDigest) :: Blake3
        PRIVATE
        !% stored key
        tInteger            :: Key(0:BLAKE3_KEY_LEN-1)
        !% chunk state variable
        TYPE(ChunkState)    :: Chunk
        !% chaining value stack
        tByte               :: CVStack(0:(BLAKE3_MAX_DEPTH+1)*BLAKE3_OUT_LEN-1)
        !% the number of chaining values occupying the stack
        tByte               :: CVStackLen
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: DoInit       => Blake3_InitBase
        PROCEDURE, PRIVATE  :: DoUpdate     => Blake3_Update
        PROCEDURE, PRIVATE  :: Blake3_Finalize
        PROCEDURE, PRIVATE  :: Blake3_Finalize_Seek
        GENERIC,   PRIVATE  :: DoFinal      => Blake3_Finalize, Blake3_Finalize_Seek
        PROCEDURE, PRIVATE  :: MergeCVStack => Blake3_Merge_CVStack
        PROCEDURE, PRIVATE  :: PushCV       => Blake3_Push_CV
        PROCEDURE, PRIVATE  :: Blake3_ByteDigest_wOutLen
        PROCEDURE, PRIVATE  :: Blake3_ByteDigest_wInputNOutLen
        PROCEDURE, PRIVATE  :: Blake3_HexDigest_wOutLen
        PROCEDURE, PRIVATE  :: Blake3_HexDigest_wInputNOutLen
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures Implemented           -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to perform
        !  default initialization of the *digest* object.
        PROCEDURE   :: Initialize           => Blake3_Initialize
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE   :: GetClone             => Blake3_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE   :: GetName              => Blake3_GetName
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE   :: Reset                => Blake3_Reset
        !> Use the *Update* method in place of the *InsertBytes* method to insert input
        !  data as an array of bytes (8-bit integers) where the offset (zero-based) into
        !  the array and the length of the input in bytes are specified.
        PROCEDURE   :: InsertBytes          => Blake3_InsertBytes
        !> Use the *Update* method in place of the *InsertGen* method to insert
        !  input data in a generic way where the *Input* argument can be any type and
        !  any rank and the *InpSize* argument specifies the size of input data in bytes.
        PROCEDURE   :: InsertGen            => Blake3_InsertGen
        !> Use the *Digest* method in place of the *ByteDigest* method to finalize the
        !  current hash computation and return the hash value as an array of bytes
        !  in a newly-allocated array.
        PROCEDURE   :: ByteDigest           => Blake3_ByteDigest
        !> Use the *Digest* method in place of the *ByteDigest_wInput* method to insert final
        !  input in a generic way and then finalize the current hash computation and return
        !  the hash value as an array of bytes in a newly-allocated array.
        PROCEDURE   :: ByteDigest_wInput    => Blake3_ByteDigest_wInput
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: CreateHMAC <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object
        !                as a *HMAC* object (for keyed hashing). <br>
        !  **Usage**: <br>
        !   --->    CALL MD%CreateHMAC(Key) <br>
        PROCEDURE   :: CreateHMAC           => Blake3_InitHMAC
        !> **Type-Bound Subroutine**: CreateKDF <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object
        !                as a *KDF* object (for key derivation). <br>
        !  **Usage**: <br>
        !   --->    CALL MD%CreateKDF(ContextString) <br>
        PROCEDURE   :: CreateKDF            => Blake3_InitKDF
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interface                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: DigestWOutLen <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash output
        !                with specified length.  The object is reset.  Some final input data
        !                can be inserted. <br>
        !  **Usage**: <br>
        !   ! finalize hash computation and return hash output as a byte array <br>
        !   --->    CALL MD%DigestWOutLen(ByteArr, OutLen) <br>
        !   ! insert final input and return hash output as a byte array <br>
        !   --->    CALL MD%DigestWOutLen(Input, InpSize, ByteArr, OutLen) <br>
        !   ! finalize hash computation and return hash output as a hexadecimal string <br>
        !   --->    CALL MD%DigestWOutLen(HexStr, OutLen) <br>
        !   ! insert final input and return hash output as a hexadecimal string <br>
        !   --->    CALL MD%DigestWOutLen(Input, InpSize, HexStr, OutLen) <br>
        GENERIC     :: DigestWOutLen        => Blake3_ByteDigest_wOutLen, &
                                               Blake3_ByteDigest_wInputNOutLen, &
                                               Blake3_HexDigest_wOutLen, &
                                               Blake3_HexDigest_wInputNOutLen
        ! ---------------------------------------------------------------------
    END TYPE Blake3

!** INTERFACE DEFINITIONS:
    INTERFACE
        !----------------------------------------------------------------------
        MODULE SUBROUTINE Blake3_InitBase(MD, Key, Flags)
            CLASS(Blake3), INTENT(INOUT)    :: MD
            tInteger,      INTENT(IN)       :: Key(0:BLAKE3_KEY_LEN-1)
            tByte,         INTENT(IN)       :: Flags
        END SUBROUTINE Blake3_InitBase
        !----------------------------------------------------------------------
        MODULE SUBROUTINE Blake3_Merge_CVStack(MD, TotalLen)
            CLASS(Blake3), TARGET, INTENT(INOUT)    :: MD
            tLong,                 INTENT(IN)       :: TotalLen
        END SUBROUTINE Blake3_Merge_CVStack
        !----------------------------------------------------------------------
        MODULE SUBROUTINE Blake3_Push_CV(MD, NewCV, ChunkCounter)
            CLASS(Blake3), INTENT(INOUT)    :: MD
            tByte,         INTENT(IN)       :: NewCV(0:BLAKE3_OUT_LEN-1)
            tLong,         INTENT(IN)       :: ChunkCounter
        END SUBROUTINE Blake3_Push_CV
        !----------------------------------------------------------------------
        MODULE SUBROUTINE Blake3_Update(MD, BytesIn, Offset, Length)
            CLASS(Blake3), INTENT(INOUT)    :: MD
            tByte,         INTENT(IN)       :: BytesIn(0:)
            tIndex,        INTENT(IN)       :: Offset
            tIndex,        INTENT(IN)       :: Length
        END SUBROUTINE Blake3_Update
        !----------------------------------------------------------------------
        MODULE SUBROUTINE Blake3_Finalize_Seek(MD, Seek, Output, OutLen)
            CLASS(Blake3), INTENT(INOUT)    :: MD
            tLong,         INTENT(IN)       :: Seek
            tByte,         INTENT(INOUT)    :: Output(0:)
            tIndex,        INTENT(IN)       :: OutLen
        END SUBROUTINE Blake3_Finalize_Seek
        !----------------------------------------------------------------------
        MODULE SUBROUTINE Blake3_Finalize(MD, Output, OutLen)
            CLASS(Blake3), INTENT(INOUT)    :: MD
            tByte,         INTENT(OUT)      :: Output(0:)
            tIndex,        INTENT(IN)       :: OutLen
        END SUBROUTINE Blake3_Finalize
        !----------------------------------------------------------------------
        MODULE SUBROUTINE ChunkState_Reset(Chunk, Key, Counter)
            TYPE(ChunkState), INTENT(INOUT) :: Chunk
            tInteger,         INTENT(IN)    :: Key(0:7)
            tLong,            INTENT(IN)    :: Counter
        END SUBROUTINE ChunkState_Reset
        !----------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Blake3_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with
    !  default algorithm and default hash output length.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), INTENT(INOUT)    :: MD   !! 'Blake3' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL MD%DoInit(IV, 0_kInt8)
   
    RETURN

END SUBROUTINE Blake3_Initialize

!******************************************************************************

SUBROUTINE Blake3_InitHMAC(MD, Key)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct Blake3 hasher with key.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), INTENT(INOUT)    :: MD
    tByte,         INTENT(IN)       :: Key(0:BLAKE3_KEY_LEN*4-1)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger        :: KeyWords(0:7)

! FLOW

    CALL BytePackLE(Key, 0_kIndex, KeyWords)
    CALL MD%DoInit(KeyWords, KEYED_HASH)

    RETURN

END SUBROUTINE Blake3_InitHMAC

!******************************************************************************

SUBROUTINE Blake3_InitKDF(BaseMD, ContextStr)

!** PURPOSE OF THIS SUBROUTINE:
    ! To construct Blake3 hasher with context string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), INTENT(INOUT)    :: BaseMD
    tCharStar,     INTENT(IN)       :: ContextStr

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    TYPE(Blake3)    :: ContextMD
    tByte           :: HashKey(0:BLAKE3_OUT_LEN-1)
    tInteger        :: HashWords(0:BLAKE3_KEY_LEN-1)

! FLOW

    ! hash the context string
    CALL ContextMD%DoInit(IV, DERIVE_KEY_CONTEXT)
    CALL ContextMD%Update(ContextStr, LEN(ContextStr, KIND=kIndex))
    CALL ContextMD%DoFinal(HashKey, BLAKE3_KEY_LEN*4)

    ! initialize with the context hash
    CALL BytePackLE(HashKey, 0_kIndex, HashWords)
    CALL BaseMD%DoInit(HashWords, DERIVE_KEY_MATERIAL)

    RETURN

END SUBROUTINE Blake3_InitKDF

!******************************************************************************

SUBROUTINE Blake3_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset components of the digest.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), INTENT(INOUT)    :: MD   !! 'Blake3' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    CALL ChunkState_Reset(MD%Chunk, MD%Key, 0_kInt64)
    MD%CVStackLen = 0_kInt8
    
    RETURN

END SUBROUTINE Blake3_Reset

!******************************************************************************

SUBROUTINE Blake3_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3),                  INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(Blake3 :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (Blake3)
        Dst%Key        = Src%Key
        Dst%Chunk      = Src%Chunk
        Dst%CVStackLen = Src%CVStackLen
        Dst%CVStack    = Src%CVStack
    END SELECT
        
    RETURN

END SUBROUTINE Blake3_GetClone

!******************************************************************************

FUNCTION Blake3_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), INTENT(IN)   :: MD       !! 'Blake3' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Blake3'
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION Blake3_GetName

!******************************************************************************

SUBROUTINE Blake3_InsertBytes(MD, ByteArr, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data as a byte (8-bit integer) array where offset (zero-based)
    !  and length are specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), INTENT(INOUT)    :: MD             !! 'Blake3' object
    tByte,         INTENT(IN)       :: ByteArr(0:)    !! a byte array of input data
    tIndex,        INTENT(IN)       :: Offset         !! the offset in input data
    tIndex,        INTENT(IN)       :: Length         !! the length of input data in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%DoUpdate(ByteArr, Offset, Length)

    RETURN

END SUBROUTINE Blake3_InsertBytes

!******************************************************************************

SUBROUTINE Blake3_InsertGen(MD, Input, InpSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data in a generic way where the *Input* argument can be
    !  any type and any rank and the *InpSize* argument specifies the size of
    !  the input data in a number of bytes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3),          INTENT(INOUT)   :: MD       !! 'Blake3' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)!! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize  !! size of the input (in bytes)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: InpPtr(:)

! FLOW
    
    ! set pointer to the input
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)
    
    ! insert the input data as a byte array
    CALL MD%DoUpdate(InpPtr, 0_kIndex, InpSize)

    NULLIFY(InpPtr)

    RETURN

END SUBROUTINE Blake3_InsertGen

!******************************************************************************

SUBROUTINE Blake3_ByteDigest(MD, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes in a newly-allocated array.
    !  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3),      INTENT(INOUT)   :: MD           !! 'Blake3' object
    tByte, ALLOCATABLE, INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: HashLen

! FLOW

    HashLen = BLAKE3_OUT_LEN
    CALL MemAlloc(ByteArr, HashLen, StartID=0_kIndex)
    CALL MD%DoFinal(0_kInt64, ByteArr, HashLen)
    CALL MD%Reset()

    RETURN

END SUBROUTINE Blake3_ByteDigest

!******************************************************************************

SUBROUTINE Blake3_ByteDigest_wInput(MD, Input, InpSize, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash
    !  computation and return the hash value as an array of (8-bit integer) bytes
    !  in a newly-allocated array.  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3),          INTENT(INOUT)   :: MD           !! 'Blake3' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tByte,   ALLOCATABLE,   INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%Update(Input, InpSize)
    CALL MD%Digest(ByteArr)
        
    RETURN

END SUBROUTINE Blake3_ByteDigest_wInput

!******************************************************************************

SUBROUTINE Blake3_ByteDigest_wOutLen(MD, ByteArr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes. The digest object is reset. <br>

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), INTENT(INOUT)    :: MD                   !! 'Blake3' object
    tIndex,        INTENT(IN)       :: OutputLen            !! the desired output length in bytes
    tByte,         INTENT(OUT)      :: ByteArr(OutputLen)   !! an array containing the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (OutputLen > 0_kIndex) THEN
        CALL MD%DoFinal(0_kInt64, ByteArr, OutputLen)
        CALL MD%Reset()
    END IF

    RETURN

END SUBROUTINE Blake3_ByteDigest_wOutLen

!******************************************************************************

SUBROUTINE Blake3_ByteDigest_wInputNOutLen(MD, Input, InpSize, ByteArr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash
    !  computation and return the hash value as an array of (8-bit integer) bytes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3),          INTENT(INOUT)   :: MD                   !! 'Blake3' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)            !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize              !! size of the input (in bytes)
    tIndex,                 INTENT(IN)      :: OutputLen            !! the desired output length in bytes
    tByte,                  INTENT(OUT)     :: ByteArr(OutputLen)   !! an array containing the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%Update(Input, InpSize)
    CALL MD%DigestWOutLen(ByteArr, OutputLen)
        
    RETURN

END SUBROUTINE Blake3_ByteDigest_wInputNOutLen

!******************************************************************************

SUBROUTINE Blake3_HexDigest_wOutLen(MD, HexStr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as a hexadecimal string in a newly-allocated character string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3), INTENT(INOUT)    :: MD           !! 'BaseDigest' object
    tCharAlloc,    INTENT(OUT)      :: HexStr       !! the hash output as a hexadecimal string
    tIndex,        INTENT(IN)       :: OutputLen    !! the desired output length in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte   :: ByteArr(OutputLen)

! FLOW

    CALL MD%DigestWOutLen(ByteArr, OutputLen)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)

    RETURN

END SUBROUTINE Blake3_HexDigest_wOutLen

!******************************************************************************

SUBROUTINE Blake3_HexDigest_wInputNOutLen(MD, Input, InpSize, HexStr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash computation
    !  and return the hash value as a hexadecimal string in a newly-allocated character string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake3),          INTENT(INOUT)   :: MD           !! 'BaseDigest' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tCharAlloc,             INTENT(OUT)     :: HexStr       !! the hash output as a hexadecimal string
    tIndex,                 INTENT(IN)      :: OutputLen    !! the desired output length in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte   :: ByteArr(OutputLen)

! FLOW

    CALL MD%DigestWOutLen(Input, InpSize, ByteArr, OutputLen)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)

    RETURN

END SUBROUTINE Blake3_HexDigest_wInputNOutLen

!******************************************************************************

END MODULE MClass_Blake3
    
!******************************************************************************
