
MODULE MClass_MDEngine

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *MDEngine* type and its related routines.
!   The *MDEngine* type is an abstract *digest* type extending directly
!   from the <a href="../module/mclass_basedigest.html#type-basedigest">
!   BaseDigest</a> type.  It defines additional methods (most of which
!   is intended to be used internally) to provide an extended API for
!   an incremental cryptographic hash function. <br>
!   By design, the *MDEngine* type is intended to be used as a template
!   (a parent type) to implement a cryptographic hash function.  It takes
!   care some of the deferred procedures required by a *digest* object.
!   If practical, all other *digest* types that implement specific hash
!   functions should extend from this template type.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MemHandlers,  ONLY: MemAlloc
    USE MBase_ByteUtil,     ONLY: AnyType_2_ByteArrPtr
    USE MClass_BaseDigest

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: MDEngine

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"

!** MODULE PARAMETERS:
    ! parameters for the first padding byte
    tUInt8, PARAMETER, PUBLIC   :: FByte00 = ToInt8(Z'00')
    tUInt8, PARAMETER, PUBLIC   :: FByte01 = ToInt8(Z'01')
    tUInt8, PARAMETER, PUBLIC   :: FByte80 = ToInt8(Z'80')
    tUInt8, PARAMETER, PUBLIC   :: FByteFF = ToInt8(Z'FF')

!** DERIVED TYPE DEFINITIONS
    !> *MDEngine* is an abstract *digest* type provided to be a template
    !  (a parent class) for other *digest* types to implement incremental
    !  cryptographic hash functions.
    TYPE, ABSTRACT, EXTENDS(BaseDigest) :: MDEngine
        PRIVATE
        !% the number of blocks of input processed
        tIndex          :: BlockCount = 0_kIndex
        !% the number of bytes of input currently stored in the buffer
        tIndex          :: BufLen     = 0_kIndex
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Additional Deferred Procedures            -----
        ! ---------------------------------------------------------------------
        !> *GetDigestLen* is a binding name of the *DEDigestLen* deferred procedure. <br>
        !  **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE(DEDigestLen),   DEFERRED  :: GetDigestLen
        !> *GetBlockLen* is a binding name of the *DEBlockLen* deferred procedure. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE(DEBlockLen),    DEFERRED  :: GetBlockLen
        !> *SetBufPtr* is a binding name of the *DESetPtr* deferred procedure. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE(DESetPtr),      DEFERRED  :: SetBufPtr
        !> *ProcessBlock* is a binding name of the *DEProcess* deferred procedure. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE(DEProcess),     DEFERRED  :: ProcessBlock
        !> *DoPadding* is a binding name of the *DEPadding* deferred procedure. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE(DEPadding),     DEFERRED  :: DoPadding
        !> *AddBitsNPad* is a binding name of the *DEAddBitsNPad* deferred procedure. <br>
        !  This procedure is intended to be used by a digest type that implements
        !  a hash function that is an entrant of the SHA-3 competition.  It is mainly
        !  used for a test purpose.
        PROCEDURE(DEAddBitsNPad), DEFERRED  :: AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures Implemented           -----
        ! ---------------------------------------------------------------------
        !> Use the *Update* method in place of the *InsertBytes* method to insert input
        !  data as an array of bytes (8-bit integers) where the offset (zero-based) into
        !  the array and the length of the input in bytes are specified.
        PROCEDURE   :: InsertBytes          => MDEngine_InsertBytes
        !> Use the *Update* method in place of the *InsertGen* method to insert
        !  input data in a generic way where the *Input* argument can be any type and
        !  any rank and the *InpSize* argument specifies the size of input data in bytes.
        PROCEDURE   :: InsertGen            => MDEngine_InsertGen
        !> Use the *Digest* method in place of the *ByteDigest* method to finalize the
        !  current hash computation and return the hash value as an array of bytes
        !  in a newly-allocated array.
        PROCEDURE   :: ByteDigest           => MDEngine_ByteDigest
        !> Use the *Digest* method in place of the *ByteDigest_wInput* method to insert final
        !  input in a generic way and then finalize the current hash computation and return
        !  the hash value as an array of bytes in a newly-allocated array.
        PROCEDURE   :: ByteDigest_wInput    => MDEngine_ByteDigest_wInput
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> *EngineReset* is a procedure to reset components of the digest to their initial values. <br>
        !  This procedure is NOT intended to be used by a user but the *Reset* method implemented
        !  by a *concrete* digest type should call this method.
        PROCEDURE   :: EngineReset      => MDEngine_Reset
        !> *EngineClone* is a procedure to copy components of the source object to the destination one. <br>
        !  This procedure is NOT intended to be used by a user but the *GetClone* method implemented
        !  by a *concrete* digest type should call this method.
        PROCEDURE   :: EngineClone      => MDEngine_Clone
        !> *GetBlockCount* is a procedure to get the number of blocks of input processed. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockCount    => MDEngine_BlockCount
        !> *GetBufLen* is a procedure to get the number of bytes of input currently
        !  stored in the buffer. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBufLen        => MDEngine_BufLen
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: MDEngine_ByteDigest_AddBits
        PROCEDURE, PRIVATE  :: MDEngine_HexDigest_AddBits
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: AddBitsNDigest <br>
        !  **Purpose**:  To add the last byte and then finalize the current hash computation
        !                and return the hash output. The object is reset. <br>
        !  **Usage**: <br>
        !   ! insert final input and return hash output as a byte array <br>
        !   --->    CALL MD%AddBitsNDigest(Input, InpSize, ByteArr) <br>
        !   ! insert final input and return hash output as a hexadecimal string <br>
        !   --->    CALL MD%AddBitsNDigest(Input, InpSize, HexStr) <br>
        !  **Note**: <br>
        !  This method is only used for an input message whose bit length is not
        !  a multiple of eight (i.e. a message with partial bytes). <br>
        !  The method is intended to be used by a digest type that implements a hash function
        !  that is an entrant of the SHA-3 competition.  It is mainly used for a test purpose.
        GENERIC     :: AddBitsNDigest   => MDEngine_ByteDigest_AddBits, &
                                           MDEngine_HexDigest_AddBits
    END TYPE MDEngine

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> DEDigestLen is a deferred procedure to return the natural hash
        !  function output length (in bytes).
        FUNCTION DEDigestLen(MD) RESULT(Length)
            IMPORT
            CLASS(MDEngine), INTENT(IN) :: MD       !! 'MDEngine' object
            tIndex                      :: Length   !! the digest length
        END FUNCTION
        !> DEBlockLen is a deferred procedure to return the *block length*
        !  for the hash function. <br>
        FUNCTION DEBlockLen(MD) RESULT(Length)
            IMPORT
            CLASS(MDEngine), INTENT(IN) :: MD       !! *MDEngine* object
            tIndex                      :: Length   !! the block length
        END FUNCTION
        !> DESetPtr is a deferred procedure to set the pointer *BufPtr* to
        !  the actual buffer array with starting index of zero. <br>
        SUBROUTINE DESetPtr(MD, BufPtr)
            IMPORT
            CLASS(MDEngine), TARGET, INTENT(INOUT)  :: MD           !! *MDEngine* object
            tUInt8,         POINTER, INTENT(INOUT)  :: BufPtr(:)    !! a buffer pointer
        END SUBROUTINE
        !> DEProcess is a deferred procedure to process one block of data. <br>
        SUBROUTINE DEProcess(MD, BytesIn)
            IMPORT
            CLASS(MDEngine), INTENT(INOUT)    :: MD           !! *MDEngine* object
            tUInt8,          INTENT(IN)       :: BytesIn(0:)  !! the data block
        END SUBROUTINE
        !> DEPadding is a deferred procedure to perform the final padding and store
        !  the result in the provided buffer.  This method shall call the *Flush*
        !  method and then the *Update* method with the appropriate padding data
        !  in order to get the full input data.
        SUBROUTINE DEPadding(MD, BytesOut, Offset)
            IMPORT
            CLASS(MDEngine), INTENT(INOUT)  :: MD           !! 'MDEngine' object
            tUInt8,          INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
            tIndex,          INTENT(IN)     :: Offset       !! the output offset
        END SUBROUTINE
        !> DEAddBitsNPad is a deferred procedure to add the last byte and then perform
        !  the final padding and store the result in the provided buffer.  This method
        !  shall call the *Flush* and then the *Update* method with the appropriate
        !  padding data in order to get the full input data.
        SUBROUTINE DEAddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)
            IMPORT
            CLASS(MDEngine), INTENT(INOUT)  :: MD           !! 'MDEngine' object
            tUInt8,          INTENT(IN)     :: LastByte     !! the last byte
            tUInt8,          INTENT(IN)     :: NBits        !! number of bits in the last byte
            tUInt8,          INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
            tIndex,          INTENT(IN)     :: Offset       !! the output offset
        END SUBROUTINE
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE MDEngine_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset components of the digest.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDEngine), INTENT(INOUT)  :: MD   !! 'MDEngine' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    MD%BufLen     = 0_kIndex
    MD%BlockCount = 0_kIndex

    RETURN

END SUBROUTINE MDEngine_Reset

!******************************************************************************

SUBROUTINE MDEngine_Clone(SrcMD, DstMD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy components of the source digest.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDEngine), INTENT(IN)     :: SrcMD   !! source object
    CLASS(MDEngine), INTENT(INOUT)  :: DstMD   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    DstMD%BufLen     = SrcMD%BufLen
    DstMD%BlockCount = SrcMD%BlockCount

    RETURN

END SUBROUTINE MDEngine_Clone

!******************************************************************************

FUNCTION MDEngine_BlockCount(MD) RESULT(BlockCount)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of blocks of input processed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDEngine), INTENT(IN) :: MD           !! 'MDEngine' object
    tIndex                      :: BlockCount   !! number of blocks

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    BlockCount = MD%BlockCount

    RETURN

END FUNCTION MDEngine_BlockCount

!******************************************************************************

FUNCTION MDEngine_BufLen(MD) RESULT(BufLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of bytes of input currently stored in the buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDEngine), INTENT(IN) :: MD       !! 'MDEngine' object
    tIndex                      :: BufLen   !! number of bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    BufLen = MD%BufLen

    RETURN

END FUNCTION MDEngine_BufLen

!******************************************************************************

SUBROUTINE MDEngine_InsertBytes(MD, ByteArr, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data as a byte (8-bit integer) array where offset (zero-based)
    !  and length are specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDEngine), INTENT(INOUT)  :: MD           !! 'MDEngine' object
    tUInt8,          INTENT(IN)     :: ByteArr(0:)  !! a byte array of input data
    tIndex,          INTENT(IN)     :: Offset       !! the offset in input data
    tIndex,          INTENT(IN)     :: Length       !! the length of input data in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8, POINTER :: BufPtr(:)
    tIndex          :: CurrLen, CopyLen, CurrOff

! FLOW
    
    ! set pointer to the buffer array
    CALL MD%SetBufPtr(BufPtr)
    
    CurrLen = Length
    CurrOff = Offset
    DO WHILE (CurrLen > 0_kIndex)
        CopyLen = MD%GetBlockLen() - MD%BufLen
        IF (CopyLen > CurrLen) CopyLen = CurrLen
        ! store input in the buffer array
        BufPtr(MD%BufLen:MD%BufLen+CopyLen-1_kIndex) = ByteArr(CurrOff:CurrOff+CopyLen-1_kIndex)
        CurrOff = CurrOff + CopyLen
        MD%BufLen = MD%BufLen + CopyLen
        CurrLen = CurrLen - CopyLen
        ! process the input if the buffer is full
        IF (MD%BufLen == MD%GetBlockLen()) THEN
            CALL MD%ProcessBlock(BufPtr)
            MD%BlockCount = MD%BlockCount + 1_kIndex
            MD%BufLen = 0_kIndex
        END IF
    END DO

    NULLIFY(BufPtr)

    RETURN

END SUBROUTINE MDEngine_InsertBytes

!******************************************************************************

SUBROUTINE MDEngine_InsertGen(MD, Input, InpSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data in a generic way where the *Input* argument can be
    !  any type and any rank and the *InpSize* argument specifies the size of
    !  the input data in a number of bytes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDEngine),        INTENT(INOUT)   :: MD       !! 'MDEngine' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)!! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize  !! size of the input (in bytes)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8, POINTER :: InpPtr(:)

! FLOW
    
    ! set pointer to the input
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)
    
    ! insert the input data as a byte array
    CALL MD%Update(InpPtr, 0_kIndex, InpSize)

    NULLIFY(InpPtr)

    RETURN

END SUBROUTINE MDEngine_InsertGen

!******************************************************************************

SUBROUTINE MDEngine_ByteDigest(MD, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes in a newly-allocated array.
    !  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDEngine),     INTENT(INOUT)  :: MD           !! 'MDEngine' object
    tUInt8, ALLOCATABLE, INTENT(OUT)    :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemAlloc(ByteArr, MD%GetDigestLen(), StartID=0_kIndex)
    CALL MD%DoPadding(ByteArr, 0_kIndex)
    CALL MD%Reset()
        
    RETURN

END SUBROUTINE MDEngine_ByteDigest

!******************************************************************************

SUBROUTINE MDEngine_ByteDigest_wInput(MD, Input, InpSize, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash
    !  computation and return the hash value as an array of (8-bit integer) bytes
    !  in a newly-allocated array.  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDEngine),        INTENT(INOUT)   :: MD           !! 'MDEngine' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tUInt8,  ALLOCATABLE,   INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%Update(Input, InpSize)
    CALL MD%Digest(ByteArr)
        
    RETURN

END SUBROUTINE MDEngine_ByteDigest_wInput

!******************************************************************************

SUBROUTINE MDEngine_ByteDigest_AddBits(MD, LastByte, NBits, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then finalize the current hash computation
    !  and return the hash output as a byte array. <br>
    !  This routine is only used for an input message whose bit length is not
    !  a multiple of eight (i.e. a message with partial bytes). <br>
    !  This procedure is intended to be used by a digest type that implements
    !  a hash function that is an entrant of the SHA-3 competition.  It is mainly
    !  used for a test purpose.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDEngine),     INTENT(INOUT)  :: MD           !! 'MDEngine' object
    tUInt8,              INTENT(IN)     :: LastByte     !! the last byte
    tUInt8,              INTENT(IN)     :: NBits        !! number of bits in the last byte
    tUInt8, ALLOCATABLE, INTENT(OUT)    :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemAlloc(ByteArr, MD%GetDigestLen(), StartID=0_kIndex)
    CALL MD%AddBitsNPad(LastByte, NBits, ByteArr, 0_kIndex)
    CALL MD%Reset()
        
    RETURN

END SUBROUTINE MDEngine_ByteDigest_AddBits

!******************************************************************************

SUBROUTINE MDEngine_HexDigest_AddBits(MD, LastByte, NBits, HexStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then finalize the current hash computation
    !  and return the hash output as a hexadecimal string. <br>
    !  This routine is only used for an input message whose bit length is not
    !  a multiple of eight (i.e. a message with partial bytes). <br>
    !  This procedure is intended to be used by a digest type that implements
    !  a hash function that is an entrant of the SHA-3 competition.  It is mainly
    !  used for a test purpose.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MDEngine), INTENT(INOUT)  :: MD           !! 'MDEngine' object
    tUInt8,          INTENT(IN)     :: LastByte     !! the last byte
    tUInt8,          INTENT(IN)     :: NBits        !! number of bits in the last byte
    tCharAlloc,      INTENT(OUT)    :: HexStr       !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8, ALLOCATABLE :: ByteArr(:)

! FLOW

    CALL MD%AddBitsNDigest(LastByte, NBits, ByteArr)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)
        
    RETURN

END SUBROUTINE MDEngine_HexDigest_AddBits

!******************************************************************************

END MODULE MClass_MDEngine
    
!******************************************************************************
