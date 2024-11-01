
MODULE MClass_BaseDigest

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *BaseDigest* type and its related routines.
!   The *BaseDigest* type is an abstract type representing a base type for
!   a *(message) digest* object, which is an incremental cryptographic hash function.
!   The *BaseDigest* type defines an application programming interface (API)
!   for various common operations shared by most cryptographic hash functions.
!   Therefore, all (message) digest types should extend from this base type. <br>
!   Typically, a *digest* object maintains a running state for a hash computation.
!   Therefore, it must first be initialized via the *Create* method.  Then, input
!   data can be inserted (a number of times) into the digest object (which normally
!   stored in a buffer and/or processed if a buffer is fulled) by using the *Update*
!   method.  Finally, the result can be obtained from the *Digest* method (where
!   some final data can be inserted as well). <br>
!   When a digest output has been produced, the *digest* object is automatically reset
!   to its initialized state, and can thus be used immediately for another digest
!   operation.  The state of a hash computation can be copied by using the *GetClone*
!   method; this can be used to get a partial hash result without interrupting the
!   complete computation.  The new (clone) object evolves independently of the source
!   object. <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ByteUtil,   ONLY: ByteArr_2_HexStr => ToHexStr_BE

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: BaseDigest
    PUBLIC :: ByteArr_2_HexStr

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> *BaseDigest* is an abstract type representing a base type for a
    !  *(Message) Digest* object.  It defines an API for common operations
    !  of an incremental cryptographic hash function.
    TYPE, ABSTRACT  :: BaseDigest
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Deferred Procedures                   -----
        ! ---------------------------------------------------------------------
        !> *Initialize* is a binding name of the *MDCreate* deferred procedure. <br>
        !  Use the *Create* method in place of the *Initialize* method to perform
        !  any essential initialization of a *digest* object.
        PROCEDURE(MDCreate),       DEFERRED :: Initialize
        !> *Reset* is a binding name of the *MDReset* deferred procedure. <br>
        !  **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE(MDReset),        DEFERRED :: Reset
        !> *GetClone* is a binding name of the *MDClone* deferred procedure. <br>
        !  **Type-Bound Function**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE(MDClone),        DEFERRED :: GetClone
        !> *GetName* is a binding name of the *MDName* deferred procedure. <br>
        !  **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE(MDName),         DEFERRED :: GetName
        !> *InsertBytes* is a binding name of the *MDInsertBytes* deferred procedure. <br>
        !  Use the *Update* method in place of the *InsertBytes* method to insert input
        !  data as an array of bytes (8-bit integers) where the offset (zero-based) into
        !  the array and the length of the input in bytes are specified.
        PROCEDURE(MDInsertBytes),  DEFERRED :: InsertBytes
        !> *InsertGen* is a binding name of the *MDInsertGen* deferred procedure. <br>
        !  Use the *Update* method in place of the *InsertGen* method to insert
        !  input data in a generic way where the *Input* argument can be any type and
        !  any rank and the *InpSize* argument specifies the size of input data in bytes.
        PROCEDURE(MDInsertGen),    DEFERRED :: InsertGen
        !> *ByteDigest* is a binding name of the *MDByteDigest* deferred procedure. <br>
        !  Use the *Digest* method in place of the *ByteDigest* method to finalize the
        !  current hash computation and return the hash value as an array of bytes
        !  in a newly-allocated array.
        PROCEDURE(MDByteDigest),   DEFERRED :: ByteDigest
        !> *ByteDigest_wInput* is a binding name of the *MDByteDigestII* deferred procedure. <br>
        !  Use the *Digest* method in place of the *ByteDigest_wInput* method to insert final
        !  input in a generic way and then finalize the current hash computation and return
        !  the hash value as an array of bytes in a newly-allocated array.
        PROCEDURE(MDByteDigestII), DEFERRED :: ByteDigest_wInput
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> HexDigest is a private procedure to finalize the current hash computation and
        !  return the hash value as a hexadecimal string in a newly-allocated character string. <br>
        !  Use the *Digest* method in place of the *HexDigest* method.
        PROCEDURE, PRIVATE  :: HexDigest        => HexStr_Digest
        !> HexDigest_wInput is a private procedure to insert final input in a generic
        !  way and then finalize the current hash computation and return the hash value as
        !  a hexadecimal string in a newly-allocated character string. <br>
        !  Use the *Digest* method in place of the *HexDigest_wInput* method.
        PROCEDURE, PRIVATE  :: HexDigest_wInput => HexStr_Digest_wInput
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Create()
        GENERIC     :: Create   => Initialize
        !> **Type-Bound Subroutine**: Update <br>
        !  **Purpose**:  To insert input data into the *digest* object (commonly stored in a
        !                buffer array) and process block(s) of data if necessary. <br>
        !  **Usage**: <br>
        !   ! insert input data in a generic way <br>
        !   --->    CALL MD%Update(Input, InpSize) <br>
        !   ! insert input data as an array of bytes (8-bit integers) <br>
        !   --->    CALL MD%Update(ByteArr, Offset, Length) <br>
        GENERIC     :: Update   => InsertBytes, InsertGen
        !> **Type-Bound Subroutine**: Digest <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash output.
        !                The object is reset.  Some final input data can be inserted. <br>
        !  **Usage**: <br>
        !   ! finalize hash computation and return hash output as a byte array <br>
        !   --->    CALL MD%Digest(ByteArr) <br>
        !   ! insert final input and return hash output as a byte array <br>
        !   --->    CALL MD%Digest(Input, InpSize, ByteArr) <br>
        !   ! finalize hash computation and return hash output as a hexadecimal string <br>
        !   --->    CALL MD%Digest(HexStr) <br>
        !   ! insert final input and return hash output as a hexadecimal string <br>
        !   --->    CALL MD%Digest(Input, InpSize, HexStr) <br>
        GENERIC     :: Digest   => ByteDigest, ByteDigest_wInput, &
                                   HexDigest,  HexDigest_wInput
    END TYPE BaseDigest

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> MDCreate is a deferred procedure to perform any essential initialization
        !  of a *digest* object.
        SUBROUTINE MDCreate(MD)
            IMPORT
            CLASS(BaseDigest), INTENT(INOUT)    :: MD   !! 'BaseDigest' object
        END SUBROUTINE
        !> MDReset is a deferred procedure to reset the hash algorithm state.
        SUBROUTINE MDReset(MD)
            IMPORT
            CLASS(BaseDigest), INTENT(INOUT)    :: MD   !! 'BaseDigest' object
        END SUBROUTINE
        !> MDClone is a deferred procedure to clone the current state. The returned
        !  object evolves independently of this object.
        SUBROUTINE MDClone(Src, Dst)
            IMPORT
            CLASS(BaseDigest),              INTENT(INOUT)   :: Src  !! a source digest object
            CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst  !! a destination (clone) digest object
        END SUBROUTINE
        !> MDName is a deferred procedure to get the display name for this hash
        !  function (e.g. "SHA-1" for SHA-1).
        FUNCTION MDName(MD) RESULT(Name)
            IMPORT
            CLASS(BaseDigest), INTENT(IN)   :: MD   !! 'BaseDigest' object
            tCharAlloc                      :: Name !! name of the hash function
        END FUNCTION
        !> MDInsertBytes is a deferred procedure to insert input data as a byte 
        !  (8-bit integer) array where offset (zero-based) and length are specified.
        SUBROUTINE MDInsertBytes(MD, ByteArr, Offset, Length)
            IMPORT
            CLASS(BaseDigest), INTENT(INOUT)    :: MD           !! 'BaseDigest' object
            tUInt8,            INTENT(IN)       :: ByteArr(0:)  !! a byte array of input data
            tIndex,            INTENT(IN)       :: Offset       !! the offset in input data
            tIndex,            INTENT(IN)       :: Length       !! the length of input data in bytes
        END SUBROUTINE
        !> MDInsertGen is a deferred procedure to insert input data in a generic way
        !  where the *Input* argument can be any type and any rank and the *InpSize*
        !  argument specifies the size of the input data in a number of bytes.
        SUBROUTINE MDInsertGen(MD, Input, InpSize)
            IMPORT
            CLASS(BaseDigest),      INTENT(INOUT)   :: MD       !! 'BaseDigest' object
            TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)!! input data (any type and rank)
            tIndex,                 INTENT(IN)      :: InpSize  !! size of the input (in bytes)
        END SUBROUTINE
        !> MDByteDigest is a deferred procedure to finalize the current hash computation
        !  and return the hash value as an array of (8-bit integer) bytes in a newly-
        !  allocated array.  The digest object is reset.
        SUBROUTINE MDByteDigest(MD, ByteArr)
            IMPORT
            CLASS(BaseDigest),   INTENT(INOUT)  :: MD           !! 'BaseDigest' object
            tUInt8, ALLOCATABLE, INTENT(OUT)    :: ByteArr(:)   !! the hash output
        END SUBROUTINE
        !> MDByteDigestII is a deferred procedure to insert final input in a generic way
        !  and then finalize the current hash computation and return the hash value as
        !  an array of (8-bit integer) bytes in a newly-allocated array.  The digest
        !  object is reset.
        SUBROUTINE MDByteDigestII(MD, Input, InpSize, ByteArr)
            IMPORT
            CLASS(BaseDigest),      INTENT(INOUT)   :: MD           !! 'BaseDigest' object
            TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
            tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
            tUInt8,  ALLOCATABLE,   INTENT(OUT)     :: ByteArr(:)   !! the hash output
        END SUBROUTINE
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE HexStr_Digest(MD, HexStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as a hexadecimal string in a newly-allocated character string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDigest), INTENT(INOUT)    :: MD       !! 'BaseDigest' object
    tCharAlloc,        INTENT(OUT)      :: HexStr   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8, ALLOCATABLE :: ByteArr(:)

! FLOW

    CALL MD%Digest(ByteArr)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)

    RETURN

END SUBROUTINE HexStr_Digest

!******************************************************************************

SUBROUTINE HexStr_Digest_wInput(MD, Input, InpSize, HexStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash computation
    !  and return the hash value as a hexadecimal string in a newly-allocated character string.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BaseDigest),      INTENT(INOUT)   :: MD       !! 'BaseDigest' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)!! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize  !! size of the input (in bytes)
    tCharAlloc,             INTENT(OUT)     :: HexStr   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tUInt8, ALLOCATABLE :: ByteArr(:)

! FLOW

    CALL MD%Digest(Input, InpSize, ByteArr)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)

    RETURN

END SUBROUTINE HexStr_Digest_wInput

!******************************************************************************

END MODULE MClass_BaseDigest

!******************************************************************************
