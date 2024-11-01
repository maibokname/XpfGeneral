
MODULE MClass_BaseHasher

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *BaseHasher* type and its related routines.
!   The *BaseHasher* type is an abstract type representing a base type for
!   a *hasher* object, which is an incremental non-cryptographic hash function.
!   By design, it defines a somewhat *incomplete* application programming
!   interface (API) for some operations of an incremental hash function.
!   The <a href="../module/mclass_hasher32.html#type-hasher32">Hasher32</a>
!   type, which is an abstract type representing a 32-bit-integer hasher,
!   defines additional methods and completes the API of an incremental
!   32-bit-integer hash function.
!   Also, the <a href="../module/mclass_hasher64.html#type-hasher64">Hasher64</a>
!   type, which is an abstract type representing a 64-bit-integer hasher,
!   defines additional methods and completes the API of an incremental
!   64-bit-integer hash function.
!   All other (concrete) hasher types that implement specific hash functions
!   extend from either the *Hasher32* type or the *Hasher64* type.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: BaseHasher

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> *BaseHasher* is an abstract type representing a base hasher that
    !  defines an *incomplete* application programming interface (API)
    !  for an incremental non-cryptographic hash function.
    TYPE, ABSTRACT  :: BaseHasher
        PRIVATE
        !% the number of blocks of input processed
        tIndex          :: BlockCount = 0_kIndex
        !% the number of bytes of input currently stored in the buffer
        tIndex          :: BufLen     = 0_kIndex
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *GetName* is a binding name of the *HSName* deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the hasher. <br>
        !  **Usage**: <br>
        !   --->    Name = Hasher%GetName()
        PROCEDURE(HSName),     DEFERRED :: GetName
        !> *GetBlockLength* is a binding name of the *HSBlockLen* deferred procedure. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE(HSBlockLen), DEFERRED :: GetBlockLength
        !> *SetBufPtr* is a binding name of the *HSSetPtr* deferred procedure. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE(HSSetPtr),   DEFERRED :: SetBufPtr
        !> *ProcessBlock* is a binding name of the *HSProcess* deferred procedure. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE(HSProcess),  DEFERRED :: ProcessBlock
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> *Reset* is a procedure to reset components of the hasher to their initial values. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: Reset            => HS_Reset
        !> *GetBlockCount* is a procedure to get the number of blocks of input processed. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBlockCount    => HS_BlockCount
        !> *GetBufLen* is a procedure to get the number of bytes of input currently
        !  stored in the buffer. <br>
        !  This procedure is NOT intended to be used by a user.
        PROCEDURE   :: GetBufLen        => HS_BufLen
        !> **Type-Bound Subroutine**: Update <br>
        !  **Purpose**:  To insert input data into the hasher (i.e. temporarily stored in the
        !                buffer array) and process block(s) of data if necessary. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Update(Input, InpSize) <br>
        !  **Important Note**: The specified input can be any type and any rank where
        !               its size is the number of bytes of storage used by the input.
        PROCEDURE   :: Update           => HS_Update
    END TYPE BaseHasher

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> HSName is a deferred procedure to return the name of the hasher. <br>
        FUNCTION HSName(HS) RESULT(Name)
            IMPORT
            CLASS(BaseHasher), INTENT(IN)   :: HS   !! a hasher (HS) object
            tCharAlloc                      :: Name !! name of the hash function
        END FUNCTION
        !> HSBlockLen is a deferred procedure to return the *block length*
        !  for the hash function. <br>
        FUNCTION HSBlockLen(HS) RESULT(Length)
            IMPORT
            CLASS(BaseHasher), INTENT(IN)   :: HS       !! a hasher (HS) object
            tIndex                          :: Length   !! the block length
        END FUNCTION
        !> HSSetPtr is a deferred procedure to set the pointer *BufPtr* to
        !  the actual buffer array with starting index of zero. <br>
        SUBROUTINE HSSetPtr(HS, BufPtr)
            IMPORT
            CLASS(BaseHasher), TARGET, INTENT(INOUT)    :: HS           !! a hasher (HS) object
            tUInt8,           POINTER, INTENT(INOUT)    :: BufPtr(:)    !! a buffer pointer
        END SUBROUTINE
        !> HSProcess is a deferred procedure to process one block of data. <br>
        SUBROUTINE HSProcess(HS, BytesIn)
            IMPORT
            CLASS(BaseHasher), INTENT(INOUT)    :: HS           !! a hasher (HS) object
            tUInt8,            INTENT(IN)       :: BytesIn(0:)  !! the data block
        END SUBROUTINE
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE HS_Reset(HS)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset components of the hasher.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseHasher), INTENT(INOUT)    :: HS   !! a hasher (HS) object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    HS%BufLen     = 0_kIndex
    HS%BlockCount = 0_kIndex

    RETURN

END SUBROUTINE HS_Reset

!******************************************************************************

FUNCTION HS_BlockCount(HS) RESULT(BlockCount)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of blocks of input processed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseHasher), INTENT(IN)   :: HS           !! a hasher (HS) object
    tIndex                          :: BlockCount   !! number of blocks

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    BlockCount = HS%BlockCount

    RETURN

END FUNCTION HS_BlockCount

!******************************************************************************

FUNCTION HS_BufLen(HS) RESULT(BufLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the number of bytes of input currently stored in the buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseHasher), INTENT(IN)   :: HS       !! a hasher (HS) object
    tIndex                          :: BufLen   !! number of bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    BufLen = HS%BufLen

    RETURN

END FUNCTION HS_BufLen

!******************************************************************************

SUBROUTINE HS_Update(HS, Input, InpSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data into the hasher (i.e. temporary stored in the buffer array)
    !  and process block(s) of data if necessary.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseHasher),   INTENT(INOUT)  :: HS           !! a hasher (HS) object
    TYPE(*), CONTIGUOUS, INTENT(IN)     :: Input(..)    !! input data (any type and rank)
    tIndex,              INTENT(IN)     :: InpSize      !! size of the input (in bytes)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8, POINTER :: InpPtr(:)
    tUInt8, POINTER :: BufPtr(:)
    tIndex          :: CurrLen, CopyLen, Offset

!** FLOW
    
    ! set pointers
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)
    CALL HS%SetBufPtr(BufPtr)
    
    ! copy input data to the buffer array and process block if necessary
    CurrLen = InpSize
    Offset  = 1_kIndex
    DO WHILE (CurrLen > 0_kIndex)
        CopyLen = HS%GetBlockLength() - HS%BufLen
        IF (CopyLen > CurrLen) CopyLen = CurrLen
        ! Important note: BufPtr has the starting index of zero whereas
        !                 InpPtr has the starting index of one.
        BufPtr(HS%BufLen:HS%BufLen+CopyLen-1_kIndex) = InpPtr(Offset:Offset+CopyLen-1_kIndex)
        Offset = Offset + CopyLen
        HS%BufLen = HS%BufLen + CopyLen
        CurrLen = CurrLen - CopyLen
        IF (HS%BufLen == HS%GetBlockLength()) THEN
            CALL HS%ProcessBlock(BufPtr)
            HS%BlockCount = HS%BlockCount + 1_kIndex
            HS%BufLen = 0_kIndex
        END IF
    END DO
    
    ! free pointer
    NULLIFY(InpPtr)
    NULLIFY(BufPtr)

    RETURN

END SUBROUTINE HS_Update

!******************************************************************************

END MODULE MClass_BaseHasher

!******************************************************************************
