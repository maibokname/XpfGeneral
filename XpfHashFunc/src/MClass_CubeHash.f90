
MODULE MClass_CubeHash

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *CubeHash* type and its related routines.
!   The *CubeHash* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *CubeHash* type implements an incremental cryptographic hash
!   function by employing the *CubeHash message-digest* algorithm [1].
!   The implementation here is mainly based on the references [2]. <br>
!   The *CubeHash* type represents four cryptographic hash functions:
!   the *CubeHash-224*, *CubeHash-256*, *CubeHash-384*, and *CubeHash-512*
!   hash functions.  By default, the *CubeHash* type represents the
!   *CubeHash-512* hash function.  However, a user can specify the
!   *Security* argument (to one of the four applicable values: 224, 256,
!   384 and 512) when initializing the digest object in order to use a
!   different hash function and get a different hash output size. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="http://cubehash.cr.yp.to/">CubeHash: a simple hash function. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil,           ONLY: ToDecStrSigned
    USE MBase_BytePack,           ONLY: BytePackLE, ByteUnpackLE
    USE MClass_BaseDigest
    USE MClass_MDEngine

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: CubeHash

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tIndex,   PARAMETER :: BlockLen = 32_kIndex
    tIndex,   PARAMETER :: DLen224  = 28_kIndex
    tIndex,   PARAMETER :: DLen256  = 32_kIndex
    tIndex,   PARAMETER :: DLen384  = 48_kIndex
    tIndex,   PARAMETER :: DLen512  = 64_kIndex
    tInteger, PARAMETER :: IV224(0:31) = [                                      &
        ToInt32(Z'B0FC8217'), ToInt32(Z'1BEE1A90'), ToInt32(Z'829E1A22'), &
        ToInt32(Z'6362C342'), ToInt32(Z'24D91C30'), ToInt32(Z'03A7AA24'), &
        ToInt32(Z'A63721C8'), ToInt32(Z'85B0E2EF'), ToInt32(Z'F35D13F3'), &
        ToInt32(Z'41DA807D'), ToInt32(Z'21A70CA6'), ToInt32(Z'1F4E9774'), &
        ToInt32(Z'B3E1C932'), ToInt32(Z'EB0A79A8'), ToInt32(Z'CDDAAA66'), &
        ToInt32(Z'E2F6ECAA'), ToInt32(Z'0A713362'), ToInt32(Z'AA3080E0'), &
        ToInt32(Z'D8F23A32'), ToInt32(Z'CEF15E28'), ToInt32(Z'DB086314'), &
        ToInt32(Z'7F709DF7'), ToInt32(Z'ACD228A4'), ToInt32(Z'704D6ECE'), &
        ToInt32(Z'AA3EC95F'), ToInt32(Z'E387C214'), ToInt32(Z'3A6445FF'), &
        ToInt32(Z'9CAB81C3'), ToInt32(Z'C73D4B98'), ToInt32(Z'D277AEBE'), &
        ToInt32(Z'FD20151C'), ToInt32(Z'00CB573E')]
    tInteger, PARAMETER :: IV256(0:31) = [                                      &
        ToInt32(Z'EA2BD4B4'), ToInt32(Z'CCD6F29F'), ToInt32(Z'63117E71'), &
        ToInt32(Z'35481EAE'), ToInt32(Z'22512D5B'), ToInt32(Z'E5D94E63'), &
        ToInt32(Z'7E624131'), ToInt32(Z'F4CC12BE'), ToInt32(Z'C2D0B696'), &
        ToInt32(Z'42AF2070'), ToInt32(Z'D0720C35'), ToInt32(Z'3361DA8C'), &
        ToInt32(Z'28CCECA4'), ToInt32(Z'8EF8AD83'), ToInt32(Z'4680AC00'), &
        ToInt32(Z'40E5FBAB'), ToInt32(Z'D89041C3'), ToInt32(Z'6107FBD5'), &
        ToInt32(Z'6C859D41'), ToInt32(Z'F0B26679'), ToInt32(Z'09392549'), &
        ToInt32(Z'5FA25603'), ToInt32(Z'65C892FD'), ToInt32(Z'93CB6285'), &
        ToInt32(Z'2AF2B5AE'), ToInt32(Z'9E4B4E60'), ToInt32(Z'774ABFDD'), &
        ToInt32(Z'85254725'), ToInt32(Z'15815AEB'), ToInt32(Z'4AB6AAD6'), &
        ToInt32(Z'9CDAF8AF'), ToInt32(Z'D6032C0A')]
    tInteger, PARAMETER :: IV384(0:31) = [                                      &
        ToInt32(Z'E623087E'), ToInt32(Z'04C00C87'), ToInt32(Z'5EF46453'), &
        ToInt32(Z'69524B13'), ToInt32(Z'1A05C7A9'), ToInt32(Z'3528DF88'), &
        ToInt32(Z'6BDD01B5'), ToInt32(Z'5057B792'), ToInt32(Z'6AA7A922'), &
        ToInt32(Z'649C7EEE'), ToInt32(Z'F426309F'), ToInt32(Z'CB629052'), &
        ToInt32(Z'FC8E20ED'), ToInt32(Z'B3482BAB'), ToInt32(Z'F89E5E7E'), &
        ToInt32(Z'D83D4DE4'), ToInt32(Z'44BFC10D'), ToInt32(Z'5FC1E63D'), &
        ToInt32(Z'2104E6CB'), ToInt32(Z'17958F7F'), ToInt32(Z'DBEAEF70'), &
        ToInt32(Z'B4B97E1E'), ToInt32(Z'32C195F6'), ToInt32(Z'6184A8E4'), &
        ToInt32(Z'796C2543'), ToInt32(Z'23DE176D'), ToInt32(Z'D33BBAEC'), &
        ToInt32(Z'0C12E5D2'), ToInt32(Z'4EB95A7B'), ToInt32(Z'2D18BA01'), &
        ToInt32(Z'04EE475F'), ToInt32(Z'1FC5F22E')]
    tInteger, PARAMETER :: IV512(0:31) = [                                      &
        ToInt32(Z'2AEA2A61'), ToInt32(Z'50F494D4'), ToInt32(Z'2D538B8B'), &
        ToInt32(Z'4167D83E'), ToInt32(Z'3FEE2313'), ToInt32(Z'C701CF8C'), &
        ToInt32(Z'CC39968E'), ToInt32(Z'50AC5695'), ToInt32(Z'4D42C787'), &
        ToInt32(Z'A647A8B3'), ToInt32(Z'97CF0BEF'), ToInt32(Z'825B4537'), &
        ToInt32(Z'EEF864D2'), ToInt32(Z'F22090C4'), ToInt32(Z'D0E5CD33'), &
        ToInt32(Z'A23911AE'), ToInt32(Z'FCD398D9'), ToInt32(Z'148FE485'), &
        ToInt32(Z'1B017BEF'), ToInt32(Z'B6444532'), ToInt32(Z'6A536159'), &
        ToInt32(Z'2FF5781C'), ToInt32(Z'91FA7934'), ToInt32(Z'0DBADEA9'), &
        ToInt32(Z'D65C8A2B'), ToInt32(Z'A5A70E75'), ToInt32(Z'B1C62456'), &
        ToInt32(Z'BC796576'), ToInt32(Z'1921C8F7'), ToInt32(Z'E7989AF1'), &
        ToInt32(Z'7795D246'), ToInt32(Z'D43E3B44')]

!** DERIVED TYPE DEFINITIONS
    !> *CubeHash* is a concrete *digest* type that implements an incremental
    !  cryptographic hash function based on the CubeHash hash functions.
    TYPE, EXTENDS(MDEngine) :: CubeHash
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state
        tInteger    :: State(0:31) = IV512(0:31)
        !% security strength in bits
        tInteger    :: Security = 512
        !% length of hash output in bytes
        tIndex      :: DigestLen = DLen512
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWSecurity* method to
        !  initialize the *digest* object with specified security.
        PROCEDURE, PRIVATE  :: InitializeWSecurity  => CubeHash_Initialize_wSecurity
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (CubeHash-512).
        PROCEDURE       :: Initialize   => CubeHash_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => CubeHash_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => CubeHash_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => CubeHash_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => CubeHash_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => CubeHash_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => CubeHash_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => CubeHash_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => CubeHash_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => CubeHash_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (CubeHash-512) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the CubeHash-256 algorithm <br>
        !   --->    CALL MD%Create(256) <br>
        GENERIC         :: Create       => InitializeWSecurity
    END TYPE CubeHash

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE CubeHash_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CubeHash), INTENT(INOUT)  :: MD    !! 'CubeHash' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%Security = 512
    MD%DigestLen = DLen512
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE CubeHash_Initialize

!******************************************************************************

SUBROUTINE CubeHash_Initialize_wSecurity(MD, Security)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified Security.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CubeHash), INTENT(INOUT)  :: MD           !! 'CubeHash' object
    tInteger,        INTENT(IN)     :: Security
    !^ Strength of security in bits with four possible values: 224, 256, 384 and 512.
    !  If the specified value is NOT valid, it is set to the default (512) value.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (Security)
    CASE (224, 256, 384, 512)
        MD%Security = Security
    CASE DEFAULT
        MD%Security = 512
    END SELECT
    
    SELECT CASE (MD%Security)
    CASE (224)
        MD%DigestLen = DLen224
    CASE (256)
        MD%DigestLen = DLen256
    CASE (384)
        MD%DigestLen = DLen384
    CASE (512)
        MD%DigestLen = DLen512
    END SELECT
    
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE CubeHash_Initialize_wSecurity

!******************************************************************************

SUBROUTINE CubeHash_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CubeHash), INTENT(INOUT)  :: MD   !! 'CubeHash' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    SELECT CASE (MD%Security)
    CASE (224)
        MD%State = IV224
    CASE (256)
        MD%State = IV256
    CASE (384)
        MD%State = IV384
    CASE (512)
        MD%State = IV512
    END SELECT
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE CubeHash_Reset

!******************************************************************************

SUBROUTINE CubeHash_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CubeHash),                INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(CubeHash :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (CubeHash)
        CALL Dst%Create(Src%Security)
        Dst%State  = Src%State
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE CubeHash_GetClone

!******************************************************************************

FUNCTION CubeHash_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CubeHash), INTENT(IN) :: MD       !! 'CubeHash' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'CubeHash-' // ToDecStrSigned(MD%Security)

    RETURN

END FUNCTION CubeHash_GetName

!******************************************************************************

FUNCTION CubeHash_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CubeHash), INTENT(IN) :: MD       !! 'CubeHash' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Length = MD%DigestLen

    RETURN

END FUNCTION CubeHash_GetDigestLen

!******************************************************************************

FUNCTION CubeHash_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CubeHash), INTENT(IN) :: MD       !! 'CubeHash' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION CubeHash_GetBlockLen

!******************************************************************************

SUBROUTINE CubeHash_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CubeHash), TARGET, INTENT(INOUT)  :: MD           !! 'CubeHash' object
    tByte,          POINTER, INTENT(INOUT)  :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE CubeHash_SetBufPtr

!******************************************************************************

SUBROUTINE CubeHash_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CubeHash), INTENT(INOUT)  :: MD           !! 'CubeHash' object
    tByte,           INTENT(IN)     :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J
    tInteger    :: MW

! FLOW
    
    ! input block
    J = 0_kIndex
    DO I = 0_kIndex, 7_kIndex
        CALL BytePackLE(BytesIn, J, MW)
        MD%State(I) = IEOR(MD%State(I), MW)
        J = J + 4_kIndex
    END DO

    ! perform 16 rounds of mixing
    CALL MixState_16Rounds(MD%State)

    RETURN

END SUBROUTINE CubeHash_ProcessBlock

!******************************************************************************

SUBROUTINE CubeHash_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CubeHash), INTENT(INOUT)  :: MD           !! 'CubeHash' object
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE CubeHash_DoPadding

!******************************************************************************

SUBROUTINE CubeHash_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(CubeHash), INTENT(INOUT)  :: MD           !! 'CubeHash' object
    tByte,           INTENT(IN)     :: LastByte     !! the last byte
    tByte,           INTENT(IN)     :: NBits        !! number of bits in the last byte
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Ptr, I, J, DLen
    tByte       :: Z

! FLOW

    ! padding
    ASSOCIATE(TmpBuf => MD%BufArr)   
        Ptr = MD%GetBufLen()
        Z = SHIFTR(FByte80, NBits)
        TmpBuf(Ptr) = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
        Ptr = Ptr + 1_kIndex
        IF (Ptr < 32_kIndex) THEN
            TmpBuf(Ptr:31) = FByte00
        END IF
        CALL MD%ProcessBlock(TmpBuf)
    END ASSOCIATE

    ! finalizing
    MD%State(31) = IEOR(MD%State(31), 1)
    DO I = 0, 9
        CALL MixState_16Rounds(MD%State)
    END DO
    DLen = MD%GetDigestLen()
    I = 0_kIndex
    J = 0_kIndex
    DO
        CALL ByteUnpackLE(MD%State(I), BytesOut, Offset+J)
        I = I + 1_kIndex
        J = J + 4_kIndex
        IF (J == DLen) EXIT
    END DO
        
    RETURN

END SUBROUTINE CubeHash_AddBitsNPad

!******************************************************************************

SUBROUTINE MixState_16Rounds(H)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform mixing of the state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: H(0:31)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: I

!** SUBROUTINE MACRO DEFINITIONS:
#define ROUND_EVEN(X)   \
    X(16) = (X(0) + X(16)); \
    X(0)  = RotateLeft(X(0), 7); \
    X(17) = (X(1) + X(17)); \
    X(1)  = RotateLeft(X(1), 7); \
    X(18) = (X(2) + X(18)); \
    X(2)  = RotateLeft(X(2), 7); \
    X(19) = (X(3) + X(19)); \
    X(3)  = RotateLeft(X(3), 7); \
    X(20) = (X(4) + X(20)); \
    X(4)  = RotateLeft(X(4), 7); \
    X(21) = (X(5) + X(21)); \
    X(5)  = RotateLeft(X(5), 7); \
    X(22) = (X(6) + X(22)); \
    X(6)  = RotateLeft(X(6), 7); \
    X(23) = (X(7) + X(23)); \
    X(7)  = RotateLeft(X(7), 7); \
    X(24) = (X(8) + X(24)); \
    X(8)  = RotateLeft(X(8), 7); \
    X(25) = (X(9) + X(25)); \
    X(9)  = RotateLeft(X(9), 7); \
    X(26) = (X(10) + X(26)); \
    X(10) = RotateLeft(X(10), 7); \
    X(27) = (X(11) + X(27)); \
    X(11) = RotateLeft(X(11), 7); \
    X(28) = (X(12) + X(28)); \
    X(12) = RotateLeft(X(12), 7); \
    X(29) = (X(13) + X(29)); \
    X(13) = RotateLeft(X(13), 7); \
    X(30) = (X(14) + X(30)); \
    X(14) = RotateLeft(X(14), 7); \
    X(31) = (X(15) + X(31)); \
    X(15) = RotateLeft(X(15), 7); \
    X(8)  = IEOR(X(8), X(16)); \
    X(9)  = IEOR(X(9), X(17)); \
    X(10) = IEOR(X(10), X(18)); \
    X(11) = IEOR(X(11), X(19)); \
    X(12) = IEOR(X(12), X(20)); \
    X(13) = IEOR(X(13), X(21)); \
    X(14) = IEOR(X(14), X(22)); \
    X(15) = IEOR(X(15), X(23)); \
    X(0)  = IEOR(X(0), X(24)); \
    X(1)  = IEOR(X(1), X(25)); \
    X(2)  = IEOR(X(2), X(26)); \
    X(3)  = IEOR(X(3), X(27)); \
    X(4)  = IEOR(X(4), X(28)); \
    X(5)  = IEOR(X(5), X(29)); \
    X(6)  = IEOR(X(6), X(30)); \
    X(7)  = IEOR(X(7), X(31)); \
    X(18) = (X(8) + X(18)); \
    X(8)  = RotateLeft(X(8), 11); \
    X(19) = (X(9) + X(19)); \
    X(9)  = RotateLeft(X(9), 11); \
    X(16) = (X(10) + X(16)); \
    X(10) = RotateLeft(X(10), 11); \
    X(17) = (X(11) + X(17)); \
    X(11) = RotateLeft(X(11), 11); \
    X(22) = (X(12) + X(22)); \
    X(12) = RotateLeft(X(12), 11); \
    X(23) = (X(13) + X(23)); \
    X(13) = RotateLeft(X(13), 11); \
    X(20) = (X(14) + X(20)); \
    X(14) = RotateLeft(X(14), 11); \
    X(21) = (X(15) + X(21)); \
    X(15) = RotateLeft(X(15), 11); \
    X(26) = (X(0) + X(26)); \
    X(0)  = RotateLeft(X(0), 11); \
    X(27) = (X(1) + X(27)); \
    X(1)  = RotateLeft(X(1), 11); \
    X(24) = (X(2) + X(24)); \
    X(2)  = RotateLeft(X(2), 11); \
    X(25) = (X(3) + X(25)); \
    X(3)  = RotateLeft(X(3), 11); \
    X(30) = (X(4) + X(30)); \
    X(4)  = RotateLeft(X(4), 11); \
    X(31) = (X(5) + X(31)); \
    X(5)  = RotateLeft(X(5), 11); \
    X(28) = (X(6) + X(28)); \
    X(6)  = RotateLeft(X(6), 11); \
    X(29) = (X(7) + X(29)); \
    X(7)  = RotateLeft(X(7), 11); \
    X(12) = IEOR(X(12), X(18)); \
    X(13) = IEOR(X(13), X(19)); \
    X(14) = IEOR(X(14), X(16)); \
    X(15) = IEOR(X(15), X(17)); \
    X(8)  = IEOR(X(8), X(22)); \
    X(9)  = IEOR(X(9), X(23)); \
    X(10) = IEOR(X(10), X(20)); \
    X(11) = IEOR(X(11), X(21)); \
    X(4)  = IEOR(X(4), X(26)); \
    X(5)  = IEOR(X(5), X(27)); \
    X(6)  = IEOR(X(6), X(24)); \
    X(7)  = IEOR(X(7), X(25)); \
    X(0)  = IEOR(X(0), X(30)); \
    X(1)  = IEOR(X(1), X(31)); \
    X(2)  = IEOR(X(2), X(28)); \
    X(3)  = IEOR(X(3), X(29));
#define ROUND_ODD(X)       \
    X(19) = (X(12) + X(19)); \
    X(12) = RotateLeft(X(12), 7); \
    X(18) = (X(13) + X(18)); \
    X(13) = RotateLeft(X(13), 7); \
    X(17) = (X(14) + X(17)); \
    X(14) = RotateLeft(X(14), 7); \
    X(16) = (X(15) + X(16)); \
    X(15) = RotateLeft(X(15), 7); \
    X(23) = (X(8) + X(23)); \
    X(8)  = RotateLeft(X(8), 7); \
    X(22) = (X(9) + X(22)); \
    X(9)  = RotateLeft(X(9), 7); \
    X(21) = (X(10) + X(21)); \
    X(10) = RotateLeft(X(10), 7); \
    X(20) = (X(11) + X(20)); \
    X(11) = RotateLeft(X(11), 7); \
    X(27) = (X(4) + X(27)); \
    X(4)  = RotateLeft(X(4), 7); \
    X(26) = (X(5) + X(26)); \
    X(5)  = RotateLeft(X(5), 7); \
    X(25) = (X(6) + X(25)); \
    X(6)  = RotateLeft(X(6), 7); \
    X(24) = (X(7) + X(24)); \
    X(7)  = RotateLeft(X(7), 7); \
    X(31) = (X(0) + X(31)); \
    X(0)  = RotateLeft(X(0), 7); \
    X(30) = (X(1) + X(30)); \
    X(1)  = RotateLeft(X(1), 7); \
    X(29) = (X(2) + X(29)); \
    X(2)  = RotateLeft(X(2), 7); \
    X(28) = (X(3) + X(28)); \
    X(3)  = RotateLeft(X(3), 7); \
    X(4)  = IEOR(X(4), X(19)); \
    X(5)  = IEOR(X(5), X(18)); \
    X(6)  = IEOR(X(6), X(17)); \
    X(7)  = IEOR(X(7), X(16)); \
    X(0)  = IEOR(X(0), X(23)); \
    X(1)  = IEOR(X(1), X(22)); \
    X(2)  = IEOR(X(2), X(21)); \
    X(3)  = IEOR(X(3), X(20)); \
    X(12) = IEOR(X(12), X(27)); \
    X(13) = IEOR(X(13), X(26)); \
    X(14) = IEOR(X(14), X(25)); \
    X(15) = IEOR(X(15), X(24)); \
    X(8)  = IEOR(X(8), X(31)); \
    X(9)  = IEOR(X(9), X(30)); \
    X(10) = IEOR(X(10), X(29)); \
    X(11) = IEOR(X(11), X(28)); \
    X(17) = (X(4) + X(17)); \
    X(4)  = RotateLeft(X(4), 11); \
    X(16) = (X(5) + X(16)); \
    X(5)  = RotateLeft(X(5), 11); \
    X(19) = (X(6) + X(19)); \
    X(6)  = RotateLeft(X(6), 11); \
    X(18) = (X(7) + X(18)); \
    X(7)  = RotateLeft(X(7), 11); \
    X(21) = (X(0) + X(21)); \
    X(0)  = RotateLeft(X(0), 11); \
    X(20) = (X(1) + X(20)); \
    X(1)  = RotateLeft(X(1), 11); \
    X(23) = (X(2) + X(23)); \
    X(2)  = RotateLeft(X(2), 11); \
    X(22) = (X(3) + X(22)); \
    X(3)  = RotateLeft(X(3), 11); \
    X(25) = (X(12) + X(25)); \
    X(12) = RotateLeft(X(12), 11); \
    X(24) = (X(13) + X(24)); \
    X(13) = RotateLeft(X(13), 11); \
    X(27) = (X(14) + X(27)); \
    X(14) = RotateLeft(X(14), 11); \
    X(26) = (X(15) + X(26)); \
    X(15) = RotateLeft(X(15), 11); \
    X(29) = (X(8) + X(29)); \
    X(8)  = RotateLeft(X(8), 11); \
    X(28) = (X(9) + X(28)); \
    X(9)  = RotateLeft(X(9), 11); \
    X(31) = (X(10) + X(31)); \
    X(10) = RotateLeft(X(10), 11); \
    X(30) = (X(11) + X(30)); \
    X(11) = RotateLeft(X(11), 11); \
    X(0)  = IEOR(X(0), X(17)); \
    X(1)  = IEOR(X(1), X(16)); \
    X(2)  = IEOR(X(2), X(19)); \
    X(3)  = IEOR(X(3), X(18)); \
    X(4)  = IEOR(X(4), X(21)); \
    X(5)  = IEOR(X(5), X(20)); \
    X(6)  = IEOR(X(6), X(23)); \
    X(7)  = IEOR(X(7), X(22)); \
    X(8)  = IEOR(X(8), X(25)); \
    X(9)  = IEOR(X(9), X(24)); \
    X(10) = IEOR(X(10), X(27)); \
    X(11) = IEOR(X(11), X(26)); \
    X(12) = IEOR(X(12), X(29)); \
    X(13) = IEOR(X(13), X(28)); \
    X(14) = IEOR(X(14), X(31)); \
    X(15) = IEOR(X(15), X(30));

! FLOW
    
    DO I = 1, 8
        ROUND_EVEN(H)
        ROUND_ODD(H)
    END DO
        
    RETURN

END SUBROUTINE MixState_16Rounds

!******************************************************************************

END MODULE MClass_CubeHash
    
!******************************************************************************
