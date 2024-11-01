
MODULE MClass_KP1600Core

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *KP1600Core* type and its related routines.
!   The *KP1600Core* type is an abstract *digest* type extending directly
!   from the <a href="../module/mclass_basedigest.html#type-basedigest">
!   BaseDigest</a> type.  It implements additional methods to provide an
!   extended API for an incremental cryptographic hash function.  Several
!   of these additional methods are the same as those methods defined by the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a> type. <br>
!   By design, the *KP1600Core* type is intended to be used as a parent
!   type for all *Keccak-based* digest types.  The *KP1600Core* type
!   provides an implementation of an incremental cryptographic hash function
!   by employing the *Keccak message-digest* algorithm with the internal
!   state size of 1600 bits (i.e. Keccak-p[1600, nRound]) [1].  It takes care
!   core operations (i.e. some of the deferred procedures and additional methods
!   provided) required by a *Keccak-based digest* object where its subtypes must
!   take care the remaining operations (i.e the rest of the deferred procedures).
!   The *KP1600Core* type utilizes the <a href="../module/mclass_kp1600sponge.html#type-kp1600sponge">
!   KP1600Sponge</a> type, which provides all basic operations of the
!   *Keccak-p[1600, nRound]* algorithm.  The implementation of the *KP1600Sponge*
!   type is mainly based on the *eXtended Keccak Code Package* (XKCP) [2].<br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://doi.org/10.6028/NIST.FIPS.202">SHA-3 Standard:
!       Permutation-Based Hash and Extendable-Output Functions. </a> <br>
!   [2] <a href="https://github.com/XKCP/XKCP">The eXtended Keccak Code Package. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MemHandlers,  ONLY: MemAlloc
    USE MBase_ByteUtil,     ONLY: AnyType_2_ByteArrPtr
    USE MClass_BaseDigest
    USE MClass_KP1600Sponge

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: KP1600Core

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    ! maximum number of rounds of permutation
    tInteger, PARAMETER :: MaxNrRounds = 24

!** DERIVED TYPE DEFINITIONS
    !> *KP1600Core* is an abstract *digest* type provided to be a parent
    !  type for other *Keccak-based digest* types to implement incremental
    !  cryptographic hash functions.
    TYPE, ABSTRACT, EXTENDS(BaseDigest) :: KP1600Core
        PRIVATE
        !% The sponge instance
        TYPE(KP1600Sponge)  :: Sponge
        !% The value of the capacity in bits
        tInteger            :: Capacity = 512
        !% The number of permutation rounds
        tInteger            :: NRounds = 24
        !% The length of output in bytes
        tIndex              :: DigestLen = 32_kIndex
        !% The initial bits appended to the end of input
        tByte               :: InitSuffix = ToInt8(Z'80')
        !% The current bits appended to the end of input
        tByte               :: CurrSuffix = ToInt8(Z'80')
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures Implemented           -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE   :: Reset                => KP1600Core_Reset
        !> Use the *Update* method in place of the *InsertBytes* method to insert input
        !  data as an array of bytes (8-bit integers) where the offset (zero-based) into
        !  the array and the length of the input in bytes are specified.
        PROCEDURE   :: InsertBytes          => KP1600Core_InsertBytes
        !> Use the *Update* method in place of the *InsertGen* method to insert
        !  input data in a generic way where the *Input* argument can be any type and
        !  any rank and the *InpSize* argument specifies the size of input data in bytes.
        PROCEDURE   :: InsertGen            => KP1600Core_InsertGen
        !> Use the *Digest* method in place of the *ByteDigest* method to finalize the
        !  current hash computation and return the hash value as an array of bytes
        !  in a newly-allocated array.
        PROCEDURE   :: ByteDigest           => KP1600Core_ByteDigest
        !> Use the *Digest* method in place of the *ByteDigest_wInput* method to insert final
        !  input in a generic way and then finalize the current hash computation and return
        !  the hash value as an array of bytes in a newly-allocated array.
        PROCEDURE   :: ByteDigest_wInput    => KP1600Core_ByteDigest_wInput
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> *CoreInit* is a procedure to initialize components of the 'KP1600Core' object. <br>
        !  This procedure is NOT intended to be used by a user but the *Initialize*
        !  method(s) implemented by a *concrete* digest type should call this method.
        PROCEDURE   :: CoreInit             => KP1600Core_Initialize
        !> *CopyState* is a procedure to copy components of the source to the destination. <br>
        !  This procedure is NOT intended to be used by a user but the *GetClone* method
        !  implemented by a *concrete* digest type should call this method.
        PROCEDURE   :: CopyState            => KP1600Core_CopyState
        !> *DoFinalWOutLen* is a procedure to finalize the digest object with a specified
        !  hash output length. <br>
        !  This procedure is NOT intended to be used by a user.  It is provided to aid the
        !  development and implementation of the *Keccak-based digest* types that can be
        !  used as an extendable-output function (XOF).
        PROCEDURE   :: DoFinalWOutLen       => KP1600Core_ByteDigest_wOutLen
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE   :: GetDigestLen         => KP1600Core_GetDigestLen
        !> **Type-Bound Function**: GetSponge <br>
        !  **Purpose**:  To return a pointer to the sponge component of the digest object. <br>
        !  **Usage**: <br>
        !   --->    Instance => MD%GetSponge()
        PROCEDURE   :: GetSponge            => KP1600Core_GetSponge
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: AddBitsNPad  => KP1600Core_AddBitsNPad
        PROCEDURE, PRIVATE  :: KP1600Core_ByteDigest_AddBits
        PROCEDURE, PRIVATE  :: KP1600Core_HexDigest_AddBits
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
        GENERIC     :: AddBitsNDigest   => KP1600Core_ByteDigest_AddBits, &
                                           KP1600Core_HexDigest_AddBits
    END TYPE KP1600Core

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE KP1600Core_Initialize(MD, Capacity, Suffix, DigestLen, NRounds)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize components of the 'KP1600Core' object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Core),  INTENT(INOUT)   :: MD           !! 'KP1600Core' object
    tInteger,           INTENT(IN)      :: Capacity     !! the value of the capacity C
    tByte,              INTENT(IN)      :: Suffix       !! suffix for padding
    tIndex,             INTENT(IN)      :: DigestLen    !! the desired length of output in bytes
    tInteger, OPTIONAL, INTENT(IN)      :: NRounds      !! number of permutation rounds

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! set values related to optional input
    MD%NRounds = MaxNrRounds
    IF (PRESENT(NRounds)) MD%NRounds = NRounds
    
    ! set values related to required input
    MD%Capacity = Capacity
    MD%DigestLen = DigestLen
    MD%InitSuffix = Suffix
    
    ! initialize the sponge
    CALL MD%Reset()

    RETURN

END SUBROUTINE KP1600Core_Initialize

!******************************************************************************

SUBROUTINE KP1600Core_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset components of the digest.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Core), INTENT(INOUT)    :: MD   !! 'KP1600Core' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    CALL MD%Sponge%Initialize(MD%Capacity, MD%NRounds)
    MD%CurrSuffix = MD%InitSuffix
    
    RETURN

END SUBROUTINE KP1600Core_Reset

!******************************************************************************

SUBROUTINE KP1600Core_CopyState(SrcMD, DstMD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To copy essential components of the source digest.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Core), INTENT(INOUT)    :: SrcMD   !! source object
    CLASS(KP1600Core), INTENT(INOUT)    :: DstMD   !! destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    CALL SrcMD%Sponge%CopyState(DstMD%Sponge)
    DstMD%Capacity = SrcMD%Capacity
    DstMD%NRounds = SrcMD%NRounds
    DstMD%DigestLen = SrcMD%DigestLen
    DstMD%InitSuffix = SrcMD%InitSuffix
    DstMD%CurrSuffix = SrcMD%CurrSuffix
    
    RETURN

END SUBROUTINE KP1600Core_CopyState

!******************************************************************************

FUNCTION KP1600Core_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the output length.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Core), INTENT(IN)   :: MD       !! 'KP1600_Core' object
    tIndex                          :: Length   !! digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Length = MD%DigestLen

    RETURN

END FUNCTION KP1600Core_GetDigestLen

!******************************************************************************

FUNCTION KP1600Core_GetSponge(MD) RESULT(Sponge)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the sponge component of the digest object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Core),  TARGET, INTENT(IN)  :: MD       !! 'KP1600_Core' object
    TYPE(KP1600Sponge), POINTER             :: Sponge   !! sponge object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Sponge => MD%Sponge

    RETURN

END FUNCTION KP1600Core_GetSponge

!******************************************************************************

SUBROUTINE KP1600Core_InsertBytes(MD, ByteArr, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data as a byte (8-bit integer) array where offset (zero-based)
    !  and length are specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Core), INTENT(INOUT)  :: MD             !! 'KP1600Core' object
    tByte,             INTENT(IN)     :: ByteArr(0:)    !! a byte array of input data
    tIndex,            INTENT(IN)     :: Offset         !! the offset in input data
    tIndex,            INTENT(IN)     :: Length         !! the length of input data in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: Flag

! FLOW

    Flag = MD%Sponge%Absorb(ByteArr(Offset:), Length)

    RETURN

END SUBROUTINE KP1600Core_InsertBytes

!******************************************************************************

SUBROUTINE KP1600Core_InsertGen(MD, Input, InpSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data in a generic way where the *Input* argument can be
    !  any type and any rank and the *InpSize* argument specifies the size of
    !  the input data in a number of bytes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Core),      INTENT(INOUT)   :: MD       !! 'KP1600Core' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)!! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize  !! size of the input (in bytes)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: InpPtr(:)
    tByte           :: Flag

! FLOW
    
    ! set pointer to the input
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)
    
    ! insert the input data as a byte array
    Flag = MD%Sponge%Absorb(InpPtr, InpSize)

    NULLIFY(InpPtr)

    RETURN

END SUBROUTINE KP1600Core_InsertGen

!******************************************************************************

SUBROUTINE KP1600Core_ByteDigest(MD, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes in a newly-allocated array.
    !  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Core),  INTENT(INOUT)   :: MD           !! 'KP1600Core' object
    tByte, ALLOCATABLE, INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: Flag

! FLOW

    IF (MD%DigestLen > 0_kIndex) THEN
        CALL MemAlloc(ByteArr, MD%DigestLen, StartID=0_kIndex)
        IF (MD%Sponge%AbsorbLastFewBits(MD%CurrSuffix) == SUCCESS) THEN
            Flag = MD%Sponge%Squeeze(ByteArr, MD%DigestLen)
        END IF
        CALL MD%Reset()
    END IF

    RETURN

END SUBROUTINE KP1600Core_ByteDigest

!******************************************************************************

SUBROUTINE KP1600Core_ByteDigest_wInput(MD, Input, InpSize, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash
    !  computation and return the hash value as an array of (8-bit integer) bytes
    !  in a newly-allocated array.  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Core),      INTENT(INOUT)   :: MD           !! 'KP1600Core' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tByte,   ALLOCATABLE,   INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%Update(Input, InpSize)
    CALL MD%Digest(ByteArr)
        
    RETURN

END SUBROUTINE KP1600Core_ByteDigest_wInput

!******************************************************************************

SUBROUTINE KP1600Core_ByteDigest_AddBits(MD, LastByte, NBits, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then finalize the current hash computation
    !  and return the hash output as a byte array. <br>
    !  This routine is only used for an input message whose bit length is not
    !  a multiple of eight (i.e. a message with partial bytes). <br>
    !  This procedure is intended to be used by a digest type that implements
    !  a hash function that is an entrant of the SHA-3 competition.  It is mainly
    !  used for a test purpose.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Core),  INTENT(INOUT)   :: MD           !! 'KP1600Core' object
    tByte,              INTENT(IN)      :: LastByte     !! the last byte
    tByte,              INTENT(IN)      :: NBits        !! number of bits in the last byte
    tByte, ALLOCATABLE, INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (MD%DigestLen > 0_kIndex) THEN
        CALL MemAlloc(ByteArr, MD%DigestLen, StartID=0_kIndex)
        CALL MD%AddBitsNPad(LastByte, NBits, ByteArr, 0_kIndex)
        CALL MD%Reset()
    END IF

    RETURN

END SUBROUTINE KP1600Core_ByteDigest_AddBits

!******************************************************************************

SUBROUTINE KP1600Core_HexDigest_AddBits(MD, LastByte, NBits, HexStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then finalize the current hash computation
    !  and return the hash output as a hexadecimal string. <br>
    !  This routine is only used for an input message whose bit length is not
    !  a multiple of eight (i.e. a message with partial bytes). <br>
    !  This procedure is intended to be used by a digest type that implements
    !  a hash function that is an entrant of the SHA-3 competition.  It is mainly
    !  used for a test purpose.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Core), INTENT(INOUT)    :: MD           !! 'KP1600Core' object
    tByte,             INTENT(IN)       :: LastByte     !! the last byte
    tByte,             INTENT(IN)       :: NBits        !! number of bits in the last byte
    tCharAlloc,        INTENT(OUT)      :: HexStr       !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, ALLOCATABLE  :: ByteArr(:)

! FLOW

    CALL MD%AddBitsNDigest(LastByte, NBits, ByteArr)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)
        
    RETURN

END SUBROUTINE KP1600Core_HexDigest_AddBits

!******************************************************************************

SUBROUTINE KP1600Core_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Core), INTENT(INOUT)    :: MD           !! 'KP1600Core' object
    tByte,             INTENT(IN)       :: LastByte     !! the last byte
    tByte,             INTENT(IN)       :: NBits        !! number of bits in the last byte
    tByte,             INTENT(INOUT)    :: BytesOut(0:) !! the output buffer
    tIndex,            INTENT(IN)       :: Offset       !! the offset of the output buffer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: Flag
    tInteger    :: DelimitedLastBytes
    tByte       :: OneByte(0:0)

! FLOW

    IF (NBits > 0_kInt8) THEN
        ! Concatenate the last few bits provided here with those of the suffix
        DelimitedLastBytes = IEOR(ToInt32(SHIFTR(LastByte, (8_kIndex - NBits))), &
                                  SHIFTL(ToInt32(MD%CurrSuffix), NBits))
        IF (IAND(DelimitedLastBytes, ToInt32(Z'0000FF00')) == 0_kInt32) THEN
            MD%CurrSuffix = ToInt8(DelimitedLastBytes)
        ELSE
            OneByte(0) = ToInt8(DelimitedLastBytes)
            Flag = MD%Sponge%Absorb(OneByte, 1_kIndex)
            MD%CurrSuffix = ToInt8(SHIFTR(DelimitedLastBytes, 8))
        END IF
    END IF

    IF (MD%Sponge%AbsorbLastFewBits(MD%CurrSuffix) == SUCCESS) THEN
        Flag = MD%Sponge%Squeeze(BytesOut(Offset:), MD%DigestLen)
    END IF

    RETURN

END SUBROUTINE KP1600Core_AddBitsNPad

!******************************************************************************

SUBROUTINE KP1600Core_ByteDigest_wOutLen(MD, ByteArr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes. The digest object is reset. <br>
    !  Note: The routine will ignore the output length specified during
    !  initialization if the given *OutputLen* is valid (>= 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(KP1600Core), INTENT(INOUT)    :: MD                   !! 'KP1600Core' object
    tIndex,            INTENT(IN)       :: OutputLen            !! the desired output length in bytes
    tByte,             INTENT(OUT)      :: ByteArr(OutputLen)   !! an array containing the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: Flag

! FLOW

    IF (OutputLen > 0_kIndex) THEN
        IF (MD%Sponge%AbsorbLastFewBits(MD%CurrSuffix) == SUCCESS) THEN
            Flag = MD%Sponge%Squeeze(ByteArr, OutputLen)
        END IF
        CALL MD%Reset()
    END IF

    RETURN

END SUBROUTINE KP1600Core_ByteDigest_wOutLen

!******************************************************************************

END MODULE MClass_KP1600Core
    
!******************************************************************************
