
MODULE MClass_MD4

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *MD4* type and its related routines.
!   The *MD4* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdhelper.html#type-mdhelper">MDHelper</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *MD4* type implements an incremental cryptographic hash
!   function by employing the *MD4 message-digest* algorithm [1]
!   where the algorithm is described in RFC 1320.  The implementation
!   here is mainly based on the *SPHLIB* implementation [2].  <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://datatracker.ietf.org/doc/html/rfc1320">RFC1320:
!       The MD4 Message-Digest Algorithm. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_BytePack,           ONLY: BytePackLE, ByteUnpackLE
    USE MClass_BaseDigest
    USE MClass_MDHelper

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: MD4

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tIndex,   PARAMETER :: BlockLen  = 64_kIndex
    tIndex,   PARAMETER :: DigestLen = 16_kIndex
    tInteger, PARAMETER :: IV(0:3) = [                      &
            ToInt32(Z'67452301'), ToInt32(Z'EFCDAB89'), &
            ToInt32(Z'98BADCFE'), ToInt32(Z'10325476')]

!** DERIVED TYPE DEFINITIONS
    !> *MD4* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing
    !  the *MD4 message-digest* algorithm.
    TYPE, EXTENDS(MDHelper) :: MD4
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state
        tInteger    :: State(0:3) = IV(0:3)
    CONTAINS
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object before starting using it.
        PROCEDURE       :: Initialize   => MD4_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => MD4_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone         => MD4_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName          => MD4_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen     => MD4_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen      => MD4_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr        => MD4_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock     => MD4_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding        => MD4_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad      => MD4_AddBitsNPad
    END TYPE MD4

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE MD4_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD4), INTENT(INOUT)   :: MD   !! 'MD4' object

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: LittleEndian = TrueVal
        
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL MD%HelperInit(LittleEndian, 8_kIndex)
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE MD4_Initialize

!******************************************************************************

SUBROUTINE MD4_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD4), INTENT(INOUT)   :: MD   !! 'MD4' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    MD%State = IV
    CALL MD%HelperReset()

    RETURN

END SUBROUTINE MD4_Reset

!******************************************************************************

SUBROUTINE MD4_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD4),                     INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(MD4 :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (MD4)
        CALL Dst%Create()
        Dst%State  = Src%State
        Dst%BufArr = Src%BufArr
        CALL Src%HelperClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE MD4_GetClone

!******************************************************************************

FUNCTION MD4_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD4), INTENT(IN)  :: MD       !! 'MD4' object
    tCharAlloc              :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'MD4'
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION MD4_GetName

!******************************************************************************

FUNCTION MD4_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD4), INTENT(IN)  :: MD       !! 'MD4' object
    tIndex                  :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = DigestLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION MD4_GetDigestLen

!******************************************************************************

FUNCTION MD4_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD4), INTENT(IN)  :: MD       !! 'MD4' object
    tIndex                  :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION MD4_GetBlockLen

!******************************************************************************

SUBROUTINE MD4_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD4), TARGET, INTENT(INOUT)   :: MD           !! 'MD4' object
    tByte,     POINTER, INTENT(INOUT)   :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE MD4_SetBufPtr

!******************************************************************************

SUBROUTINE MD4_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD4), INTENT(INOUT)   :: MD           !! 'MD4' object
    tByte,      INTENT(IN)      :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: A, B, C, D
    tInteger    :: X(0:15)

!** SUBROUTINE MACRO DEFINITIONS:
#define FRound(B, C, D)     IEOR(IAND(IEOR(C, D), B), D)
#define GRound(B, C, D)     IOR(IAND(D, C), IAND(IOR(D, C), B))
#define HRound(B, C, D)     IEOR(IEOR(B, C), D)

! FLOW

    A = MD%State(0)
    B = MD%State(1)
    C = MD%State(2)
    D = MD%State(3)

    CALL BytePackLE(BytesIn, 0_kIndex, X)
        
    A = RotateLeft(A + FRound(B, C, D) + X( 0), 3)
    D = RotateLeft(D + FRound(A, B, C) + X( 1), 7)
    C = RotateLeft(C + FRound(D, A, B) + X( 2), 11)
    B = RotateLeft(B + FRound(C, D, A) + X( 3), 19)
    A = RotateLeft(A + FRound(B, C, D) + X( 4), 3)
    D = RotateLeft(D + FRound(A, B, C) + X( 5), 7)
    C = RotateLeft(C + FRound(D, A, B) + X( 6), 11)
    B = RotateLeft(B + FRound(C, D, A) + X( 7), 19)
    A = RotateLeft(A + FRound(B, C, D) + X( 8), 3)
    D = RotateLeft(D + FRound(A, B, C) + X( 9), 7)
    C = RotateLeft(C + FRound(D, A, B) + X(10), 11)
    B = RotateLeft(B + FRound(C, D, A) + X(11), 19)
    A = RotateLeft(A + FRound(B, C, D) + X(12), 3)
    D = RotateLeft(D + FRound(A, B, C) + X(13), 7)
    C = RotateLeft(C + FRound(D, A, B) + X(14), 11)
    B = RotateLeft(B + FRound(C, D, A) + X(15), 19)
      
    A = RotateLeft(A + GRound(B, C, D) + X( 0) + ToInt32(Z'5A827999'), 3)
    D = RotateLeft(D + GRound(A, B, C) + X( 4) + ToInt32(Z'5A827999'), 5)
    C = RotateLeft(C + GRound(D, A, B) + X( 8) + ToInt32(Z'5A827999'), 9)
    B = RotateLeft(B + GRound(C, D, A) + X(12) + ToInt32(Z'5A827999'), 13)
    A = RotateLeft(A + GRound(B, C, D) + X( 1) + ToInt32(Z'5A827999'), 3)
    D = RotateLeft(D + GRound(A, B, C) + X( 5) + ToInt32(Z'5A827999'), 5)
    C = RotateLeft(C + GRound(D, A, B) + X( 9) + ToInt32(Z'5A827999'), 9)
    B = RotateLeft(B + GRound(C, D, A) + X(13) + ToInt32(Z'5A827999'), 13)
    A = RotateLeft(A + GRound(B, C, D) + X( 2) + ToInt32(Z'5A827999'), 3)
    D = RotateLeft(D + GRound(A, B, C) + X( 6) + ToInt32(Z'5A827999'), 5)
    C = RotateLeft(C + GRound(D, A, B) + X(10) + ToInt32(Z'5A827999'), 9)
    B = RotateLeft(B + GRound(C, D, A) + X(14) + ToInt32(Z'5A827999'), 13)
    A = RotateLeft(A + GRound(B, C, D) + X( 3) + ToInt32(Z'5A827999'), 3)
    D = RotateLeft(D + GRound(A, B, C) + X( 7) + ToInt32(Z'5A827999'), 5)
    C = RotateLeft(C + GRound(D, A, B) + X(11) + ToInt32(Z'5A827999'), 9)
    B = RotateLeft(B + GRound(C, D, A) + X(15) + ToInt32(Z'5A827999'), 13)
        
    A = RotateLeft(A + HRound(B, C, D) + X( 0) + ToInt32(Z'6ED9EBA1'), 3)
    D = RotateLeft(D + HRound(A, B, C) + X( 8) + ToInt32(Z'6ED9EBA1'), 9)
    C = RotateLeft(C + HRound(D, A, B) + X( 4) + ToInt32(Z'6ED9EBA1'), 11)
    B = RotateLeft(B + HRound(C, D, A) + X(12) + ToInt32(Z'6ED9EBA1'), 15)
    A = RotateLeft(A + HRound(B, C, D) + X( 2) + ToInt32(Z'6ED9EBA1'), 3)
    D = RotateLeft(D + HRound(A, B, C) + X(10) + ToInt32(Z'6ED9EBA1'), 9)
    C = RotateLeft(C + HRound(D, A, B) + X( 6) + ToInt32(Z'6ED9EBA1'), 11)
    B = RotateLeft(B + HRound(C, D, A) + X(14) + ToInt32(Z'6ED9EBA1'), 15)
    A = RotateLeft(A + HRound(B, C, D) + X( 1) + ToInt32(Z'6ED9EBA1'), 3)
    D = RotateLeft(D + HRound(A, B, C) + X( 9) + ToInt32(Z'6ED9EBA1'), 9)
    C = RotateLeft(C + HRound(D, A, B) + X( 5) + ToInt32(Z'6ED9EBA1'), 11)
    B = RotateLeft(B + HRound(C, D, A) + X(13) + ToInt32(Z'6ED9EBA1'), 15)
    A = RotateLeft(A + HRound(B, C, D) + X( 3) + ToInt32(Z'6ED9EBA1'), 3)
    D = RotateLeft(D + HRound(A, B, C) + X(11) + ToInt32(Z'6ED9EBA1'), 9)
    C = RotateLeft(C + HRound(D, A, B) + X( 7) + ToInt32(Z'6ED9EBA1'), 11)
    B = RotateLeft(B + HRound(C, D, A) + X(15) + ToInt32(Z'6ED9EBA1'), 15)  

    MD%State(0) = MD%State(0) + A
    MD%State(1) = MD%State(1) + B
    MD%State(2) = MD%State(2) + C
    MD%State(3) = MD%State(3) + D
        
    RETURN

#undef FRound
#undef GRound
#undef HRound

END SUBROUTINE MD4_ProcessBlock

!******************************************************************************

SUBROUTINE MD4_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD4), INTENT(INOUT)   :: MD           !! 'MD4' object
    tByte,      INTENT(INOUT)   :: BytesOut(0:) !! the output buffer
    tIndex,     INTENT(IN)      :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    CALL MD%HelperPadding()
    CALL ByteUnpackLE(MD%State, BytesOut, Offset)

    RETURN

END SUBROUTINE MD4_DoPadding

!******************************************************************************

SUBROUTINE MD4_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD4), INTENT(INOUT)   :: MD           !! 'MD4' object
    tByte,      INTENT(IN)      :: LastByte     !! the last byte
    tByte,      INTENT(IN)      :: NBits        !! number of bits in the last byte
    tByte,      INTENT(INOUT)   :: BytesOut(0:) !! the output buffer
    tIndex,     INTENT(IN)      :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! MD4 algorithm does not handle partial bytes
    CALL MD%DoPadding(BytesOut, Offset)
    ASSOCIATE(Dummy => LastByte, Dummy2 => NBits); END ASSOCIATE

    RETURN

END SUBROUTINE MD4_AddBitsNPad

!******************************************************************************

END MODULE MClass_MD4
    
!******************************************************************************
