
MODULE MClass_Blake2Core

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Blake2Core* type and its related routines.
!   The *Blake2Core* type is an abstract *digest* type extending directly
!   from the <a href="../module/mclass_basedigest.html#type-basedigest">
!   BaseDigest</a> type.  It defines and implements additional methods
!   (most of which is intended to be used internally) to provide an
!   extended API for an incremental cryptographic hash function. <br>
!   By design, the *Blake2Core* type is intended to be used as a parent
!   type for all *BLAKE2-based* digest types.  The *Blake2Core* type
!   provides an implementation of an incremental cryptographic hash function
!   by employing the *BLAKE2 message-digest* algorithm [1, 2].  It takes care
!   core operations (i.e. some of the deferred procedures and additional methods
!   provided) required by a *BLAKE2-based digest* object where its subtypes must
!   take care the remaining operations (i.e the rest of the deferred procedures).
!   The implementation of the *Blake2Core* type and its subtypes are based
!   mainly on the references [3, 4]. <br>
!   It should be noted that the *BLAKE2 message-digest* algorithm has several
!   variants with additional features such as keyed hashing (that is, MAC or
!   PRF), hashing with a salt, personalization and/or incremental tree-hashing.
!   The algorithm can also produce a variable-length hash output.  Among these
!   additional features, the *Blake2Core* type and its subtypes only allow
!   keyed hashing and variable-length hash output. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.aumasson.jp/blake/book/">J.P. Aumasson, W. Meier,
!       R.C.W. Phan, and L. Henzen. 2015. The Hash Function BLAKE. Springer. </a> <br>
!   [2] <a href="https://www.blake2.net/">BLAKE2 - Fast Secure Hashing. </a> <br>
!   [3] <a href="https://github.com/kocakosm/jblake2">JBlake2: A pure Java
!       implementation of BLAKE2 (RFC 7693). </a> <br>
!   [4] <a href="https://github.com/BLAKE2/BLAKE2">BLAKE2 official implementations. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MemHandlers,  ONLY: MemAlloc, MemFree
    USE MBase_ByteUtil,     ONLY: AnyType_2_ByteArrPtr
    USE MClass_BaseDigest

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Blake2Core

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> *Blake2Core* is an abstract *digest* type provided to be a parent type
    !  for other *Blake2-based digest* types to implement incremental cryptographic
    !  hash functions.
    TYPE, ABSTRACT, EXTENDS(BaseDigest) :: Blake2Core
        PRIVATE
        !% the number of bytes of input currently stored in the buffer array
        tIndex              :: BufLen    = 0_kIndex
        !% the size in bytes of hash output
        tIndex              :: DigestLen = 32_kIndex
        !% the size in bytes of the buffer array
        tIndex              :: BlockLen  = 64_kIndex
        !% a copy of the specified key for keyed hashing
        tByte, ALLOCATABLE  :: Key(:)
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Additional Deferred Procedures            -----
        ! ---------------------------------------------------------------------
        !> *SetBufPtr* is a binding name of the *BCSetPtr* deferred procedure. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE(BCSetPtr),  DEFERRED  :: SetBufPtr
        !> *ProcessBlock* is a binding name of the *BCProcess* deferred procedure. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE(BCProcess), DEFERRED  :: ProcessBlock
        !> *EncodeOutput* is a binding name of the *BCEncode* deferred procedure. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE(BCEncode),  DEFERRED  :: EncodeOutput
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures Implemented           -----
        ! ---------------------------------------------------------------------
        !> Use the *Update* method in place of the *InsertBytes* method to insert input
        !  data as an array of bytes (8-bit integers) where the offset (zero-based) into
        !  the array and the length of the input in bytes are specified.
        PROCEDURE   :: InsertBytes          => Blake2Core_InsertBytes
        !> Use the *Update* method in place of the *InsertGen* method to insert
        !  input data in a generic way where the *Input* argument can be any type and
        !  any rank and the *InpSize* argument specifies the size of input data in bytes.
        PROCEDURE   :: InsertGen            => Blake2Core_InsertGen
        !> Use the *Digest* method in place of the *ByteDigest* method to finalize the
        !  current hash computation and return the hash value as an array of bytes
        !  in a newly-allocated array.
        PROCEDURE   :: ByteDigest           => Blake2Core_ByteDigest
        !> Use the *Digest* method in place of the *ByteDigest_wInput* method to insert final
        !  input in a generic way and then finalize the current hash computation and return
        !  the hash value as an array of bytes in a newly-allocated array.
        PROCEDURE   :: ByteDigest_wInput    => Blake2Core_ByteDigest_wInput
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> *CoreInit* is a procedure to initialize the Blake2Core's components. <br>
        !  This procedure is NOT intended to be used by a user but all initialization procedures
        !  implemented by a *concrete* digest type should call this method.
        PROCEDURE   :: CoreInit     => Blake2Core_Init
        !> *CoreReset* is a procedure to reset components of the digest to their initial values. <br>
        !  This procedure is NOT intended to be used by a user but the *Reset* method implemented
        !  by a *concrete* digest type should call this method.
        PROCEDURE   :: CoreReset    => Blake2Core_Reset
        !> *CoreClone* is a procedure to copy components of the source object to the destination one. <br>
        !  This procedure is NOT intended to be used by a user but the *GetClone* method implemented
        !  by a *concrete* digest type should call this method.
        PROCEDURE   :: CoreClone    => Blake2Core_Clone
        !> *GetBufLen* is a procedure to get the number of bytes of input currently
        !  stored in the buffer. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBufLen    => Blake2Core_BufLen
        !> *GetKeyLen* is a procedure to get the length of the specified key. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetKeyLen    => Blake2Core_KeyLen
          !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the hash output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE   :: GetDigestLen => Blake2Core_DigestLen
        ! ---------------------------------------------------------------------
  END TYPE Blake2Core

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> BCSetPtr is a deferred procedure to set the pointer *BufPtr* to
        !  the actual buffer array with starting index of zero. <br>
        SUBROUTINE BCSetPtr(MD, BufPtr)
            IMPORT
            CLASS(Blake2Core), TARGET, INTENT(INOUT)    :: MD           !! *Blake2Core* object
            tByte,            POINTER, INTENT(INOUT)    :: BufPtr(:)    !! a buffer pointer
        END SUBROUTINE
        !> BCProcess is a deferred procedure to process one block of data. <br>
        SUBROUTINE BCProcess(MD, BytesIn, LastBlock)
            IMPORT
            CLASS(Blake2Core), INTENT(INOUT)    :: MD           !! *Blake2Core* object
            tByte,             INTENT(IN)       :: BytesIn(0:)  !! the data block
            tLogical,          INTENT(IN)       :: LastBlock    !! true if input block is the last one
        END SUBROUTINE
        !> BCEncode is a deferred procedure to perform the encoding of the output data
        !  (e.g. unpacking the state variables into the output data).
        SUBROUTINE BCEncode(MD, BytesOut)
            IMPORT
            CLASS(Blake2Core), INTENT(INOUT)    :: MD           !! 'Blake2Core' object
            tByte,             INTENT(INOUT)    :: BytesOut(0:) !! the output buffer
        END SUBROUTINE
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Blake2Core_Init(MD, BlockLen, DigestLen, Key)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2Core), INTENT(INOUT)    :: MD           !! 'Blake2Core' object
    tIndex,            INTENT(IN)       :: BlockLen     !! block length
    tIndex,            INTENT(IN)       :: DigestLen    !! digest length
    tByte, OPTIONAL,   INTENT(IN)       :: Key(0:)      !! key

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: DefaultLen, KeyLen

! FLOW
    
    ! set block length
    MD%BlockLen = BlockLen
        
    ! set digest length
    DefaultLen = BlockLen/2_kIndex
    IF ((DigestLen >= 1_kIndex).AND.(DigestLen <= DefaultLen)) THEN
        MD%DigestLen = DigestLen
    ELSE
        MD%DigestLen = DefaultLen
    END IF
        
    ! allocate and copy key if present
    IF (PRESENT(Key)) THEN
        KeyLen = MIN(SIZE(Key, KIND=kIndex), DefaultLen)
        IF (KeyLen > 0_kIndex) THEN
            CALL MemAlloc(MD%Key, KeyLen, StartID=0_kIndex)
            MD%Key(0:KeyLen-1) = Key(0:KeyLen-1)
        END IF
    ELSE
        CALL MemFree(MD%Key)
    END IF
        
    ! reset
    CALL MD%Reset()
        
    RETURN

END SUBROUTINE Blake2Core_Init

!******************************************************************************

SUBROUTINE Blake2Core_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset components of the digest.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2Core), INTENT(INOUT)  :: MD   !! 'Blake2Core' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: BufPtr(:)
    tIndex          :: MinSize, KeyLen

!** FLOW
    
    MD%BufLen = 0_kIndex
    KeyLen = MD%GetKeyLen()
    IF (KeyLen > 0_kIndex) THEN
        ! set pointer to the buffer array
        CALL MD%SetBufPtr(BufPtr)
        MinSize = MIN(KeyLen, MD%BlockLen)
        BufPtr(0:MinSize-1) = MD%Key(0:MinSize-1)
        IF (MinSize < MD%BlockLen) THEN
            BufPtr(MinSize:MD%BlockLen-1) = 0_kInt8
        END IF
        MD%BufLen = MD%BlockLen
        NULLIFY(BufPtr)
    END IF

    RETURN

END SUBROUTINE Blake2Core_Reset

!******************************************************************************

SUBROUTINE Blake2Core_Clone(SrcMD, DstMD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy components of the source digest.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2Core), INTENT(IN)     :: SrcMD   !! source object
    CLASS(Blake2Core), INTENT(INOUT)  :: DstMD   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    DstMD%BufLen    = SrcMD%BufLen
    DstMD%DigestLen = SrcMD%DigestLen
    DstMD%BlockLen  = SrcMD%BlockLen
    CALL MemAlloc(DstMD%Key, SIZE(SrcMD%Key, KIND=kIndex), StartID=0_kIndex)
    DstMD%Key       = SrcMD%Key

    RETURN

END SUBROUTINE Blake2Core_Clone

!******************************************************************************

FUNCTION Blake2Core_BufLen(MD) RESULT(BufLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of bytes of input currently stored in the buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2Core), INTENT(IN)   :: MD       !! 'Blake2Core' object
    tIndex                          :: BufLen   !! number of bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    BufLen = MD%BufLen

    RETURN

END FUNCTION Blake2Core_BufLen

!******************************************************************************

SUBROUTINE Blake2Core_InsertBytes(MD, ByteArr, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data as a byte (8-bit integer) array where offset (zero-based)
    !  and length are specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2Core), INTENT(INOUT)    :: MD           !! 'Blake2Core' object
    tByte,             INTENT(IN)       :: ByteArr(0:)  !! a byte array of input data
    tIndex,            INTENT(IN)       :: Offset       !! the offset in input data
    tIndex,            INTENT(IN)       :: Length       !! the length of input data in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: BufPtr(:)
    tIndex          :: CurrLen, CopyLen, CurrOff

! FLOW
    
    ! set pointer to the buffer array
    CALL MD%SetBufPtr(BufPtr)

    CurrLen = Length
    CurrOff = Offset
    DO WHILE (CurrLen > 0_kIndex)
        ! process the input if the buffer is full
        IF (MD%BufLen == MD%BlockLen) THEN
            CALL MD%ProcessBlock(BufPtr, FalseVal)
            MD%BufLen = 0_kIndex
        END IF
        CopyLen = MD%BlockLen - MD%BufLen
        IF (CopyLen > CurrLen) CopyLen = CurrLen
        ! store input in the buffer array
        BufPtr(MD%BufLen:MD%BufLen+CopyLen-1_kIndex) = ByteArr(CurrOff:CurrOff+CopyLen-1_kIndex)
        CurrOff = CurrOff + CopyLen
        MD%BufLen = MD%BufLen + CopyLen
        CurrLen = CurrLen - CopyLen
    END DO

    NULLIFY(BufPtr)

    RETURN

END SUBROUTINE Blake2Core_InsertBytes

!******************************************************************************

SUBROUTINE Blake2Core_InsertGen(MD, Input, InpSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data in a generic way where the *Input* argument can be
    !  any type and any rank and the *InpSize* argument specifies the size of
    !  the input data in a number of bytes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2Core),      INTENT(INOUT)   :: MD       !! 'Blake2Core' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)!! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize  !! size of the input (in bytes)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: InpPtr(:)

! FLOW
    
    ! set pointer to the input
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)
    
    ! insert the input data as a byte array
    CALL MD%Update(InpPtr, 0_kIndex, InpSize)

    NULLIFY(InpPtr)

    RETURN

END SUBROUTINE Blake2Core_InsertGen

!******************************************************************************

SUBROUTINE Blake2Core_ByteDigest(MD, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes in a newly-allocated array.
    !  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2Core),  INTENT(INOUT)   :: MD           !! 'Blake2Core' object
    tByte, ALLOCATABLE, INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: BufPtr(:)

! FLOW
    
    ! set pointer to the buffer array
    CALL MD%SetBufPtr(BufPtr)

    ! pad zero to the end of the buffer
    BufPtr(MD%BufLen:MD%BlockLen-1) = 0_kInt8
        
    ! process the last block of data
    CALL MD%ProcessBlock(BufPtr, TrueVal)
        
    ! get output
    CALL MemAlloc(ByteArr, MD%DigestLen, StartID=0_kIndex)
    CALL MD%EncodeOutput(ByteArr)
        
    ! reset the states
    CALL MD%Reset()

    ! free pointer
    NULLIFY(BufPtr)

    RETURN

END SUBROUTINE Blake2Core_ByteDigest

!******************************************************************************

SUBROUTINE Blake2Core_ByteDigest_wInput(MD, Input, InpSize, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash
    !  computation and return the hash value as an array of (8-bit integer) bytes
    !  in a newly-allocated array.  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2Core),      INTENT(INOUT)   :: MD           !! 'Blake2Core' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tByte,   ALLOCATABLE,   INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%Update(Input, InpSize)
    CALL MD%Digest(ByteArr)
        
    RETURN

END SUBROUTINE Blake2Core_ByteDigest_wInput

!******************************************************************************

FUNCTION Blake2Core_DigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the output length.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2Core), INTENT(IN)   :: MD       !! 'Blake2Core' object
    tIndex                          :: Length   !! digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Length = MD%DigestLen

    RETURN

END FUNCTION Blake2Core_DigestLen

!******************************************************************************

FUNCTION Blake2Core_KeyLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get key length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2Core), INTENT(IN)   :: MD       !! 'Blake2Key' object
    tIndex                          :: Length   !! the key length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (ALLOCATED(MD%Key)) THEN
        Length = SIZE(MD%Key, KIND=kIndex)
    ELSE
        Length = 0_kIndex
    END IF
        
    RETURN

END FUNCTION Blake2Core_KeyLen

!******************************************************************************

END MODULE MClass_Blake2Core
    
!******************************************************************************
