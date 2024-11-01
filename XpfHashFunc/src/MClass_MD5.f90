
MODULE MClass_MD5

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *MD5* type and its related routines.
!   The *MD5* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdhelper.html#type-mdhelper">MDHelper</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *MD5* type implements an incremental cryptographic hash
!   function by employing the *MD5 message-digest* algorithm where
!   the algorithm is described in RFC 1321 [1].  The implementation
!   here is mainly based on the *SPHLIB* implementation [2].  <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://datatracker.ietf.org/doc/html/rfc1321">RFC1321: 
!       The MD5 Message-Digest Algorithm. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_BytePack,           ONLY: BytePackLE, ByteUnpackLE
    USE MClass_BaseDigest
    USE MClass_MDHelper

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: MD5

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
    !> *MD5* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing
    !  the *MD5 message-digest* algorithm.
    TYPE, EXTENDS(MDHelper) :: MD5
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state
        tInteger    :: State(0:3) = IV(0:3)
    CONTAINS
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object before starting using it.
        PROCEDURE       :: Initialize   => MD5_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => MD5_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone         => MD5_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName          => MD5_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen     => MD5_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen      => MD5_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr        => MD5_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock     => MD5_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding        => MD5_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad      => MD5_AddBitsNPad
    END TYPE MD5

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE MD5_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD5), INTENT(INOUT)   :: MD   !! 'MD5' object

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: LittleEndian = TrueVal
        
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL MD%HelperInit(LittleEndian, 8_kIndex)
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE MD5_Initialize

!******************************************************************************

SUBROUTINE MD5_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD5), INTENT(INOUT)   :: MD   !! 'MD5' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    MD%State = IV
    CALL MD%HelperReset()

    RETURN

END SUBROUTINE MD5_Reset

!******************************************************************************

SUBROUTINE MD5_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD5),                     INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(MD5 :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (MD5)
        CALL Dst%Create()
        Dst%State  = Src%State
        Dst%BufArr = Src%BufArr
        CALL Src%HelperClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE MD5_GetClone

!******************************************************************************

FUNCTION MD5_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD5), INTENT(IN)  :: MD       !! 'MD5' object
    tCharAlloc              :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'MD5'
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION MD5_GetName

!******************************************************************************

FUNCTION MD5_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD5), INTENT(IN)  :: MD       !! 'MD5' object
    tIndex                  :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = DigestLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION MD5_GetDigestLen

!******************************************************************************

FUNCTION MD5_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD5), INTENT(IN)  :: MD       !! 'MD5' object
    tIndex                  :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION MD5_GetBlockLen

!******************************************************************************

SUBROUTINE MD5_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD5), TARGET, INTENT(INOUT)   :: MD           !! 'MD5' object
    tByte,     POINTER, INTENT(INOUT)   :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE MD5_SetBufPtr

!******************************************************************************

SUBROUTINE MD5_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD5), INTENT(INOUT)   :: MD           !! 'MD5' object
    tByte,      INTENT(IN)      :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: A, B, C, D
    tInteger    :: X(0:15)

!** SUBROUTINE MACRO DEFINITIONS:
#define FRound(X, Y, Z)     IOR(IAND(Y, X), IAND(Z, NOT(X)))
#define GRound(X, Y, Z)     IOR(IAND(X, Z), IAND(Y, NOT(Z)))
#define HRound(X, Y, Z)     IEOR(IEOR(X, Y), Z)
#define IRound(X, Y, Z)     IEOR(Y, IOR(X, NOT(Z)))

! FLOW

    A = MD%State(0)
    B = MD%State(1)
    C = MD%State(2)
    D = MD%State(3)

    CALL BytePackLE(BytesIn, 0_kIndex, X)
        
    A = B + RotateLeft(A + FRound(B, C, D) + X( 0) + ToInt32(Z'D76AA478'), 7)
    D = A + RotateLeft(D + FRound(A, B, C) + X( 1) + ToInt32(Z'E8C7B756'), 12)
    C = D + RotateLeft(C + FRound(D, A, B) + X( 2) + ToInt32(Z'242070DB'), 17)
    B = C + RotateLeft(B + FRound(C, D, A) + X( 3) + ToInt32(Z'C1BDCEEE'), 22)
    A = B + RotateLeft(A + FRound(B, C, D) + X( 4) + ToInt32(Z'F57C0FAF'), 7)
    D = A + RotateLeft(D + FRound(A, B, C) + X( 5) + ToInt32(Z'4787C62A'), 12)
    C = D + RotateLeft(C + FRound(D, A, B) + X( 6) + ToInt32(Z'A8304613'), 17)
    B = C + RotateLeft(B + FRound(C, D, A) + X( 7) + ToInt32(Z'FD469501'), 22)
    A = B + RotateLeft(A + FRound(B, C, D) + X( 8) + ToInt32(Z'698098D8'), 7)
    D = A + RotateLeft(D + FRound(A, B, C) + X( 9) + ToInt32(Z'8B44F7AF'), 12)
    C = D + RotateLeft(C + FRound(D, A, B) + X(10) + ToInt32(Z'FFFF5BB1'), 17)
    B = C + RotateLeft(B + FRound(C, D, A) + X(11) + ToInt32(Z'895CD7BE'), 22)
    A = B + RotateLeft(A + FRound(B, C, D) + X(12) + ToInt32(Z'6B901122'), 7)
    D = A + RotateLeft(D + FRound(A, B, C) + X(13) + ToInt32(Z'FD987193'), 12)
    C = D + RotateLeft(C + FRound(D, A, B) + X(14) + ToInt32(Z'A679438E'), 17)
    B = C + RotateLeft(B + FRound(C, D, A) + X(15) + ToInt32(Z'49B40821'), 22)

    A = B + RotateLeft(A + GRound(B, C, D) + X( 1) + ToInt32(Z'F61E2562'), 5)
    D = A + RotateLeft(D + GRound(A, B, C) + X( 6) + ToInt32(Z'C040B340'), 9)
    C = D + RotateLeft(C + GRound(D, A, B) + X(11) + ToInt32(Z'265E5A51'), 14)
    B = C + RotateLeft(B + GRound(C, D, A) + X( 0) + ToInt32(Z'E9B6C7AA'), 20)
    A = B + RotateLeft(A + GRound(B, C, D) + X( 5) + ToInt32(Z'D62F105D'), 5)
    D = A + RotateLeft(D + GRound(A, B, C) + X(10) + ToInt32(Z'02441453'), 9)
    C = D + RotateLeft(C + GRound(D, A, B) + X(15) + ToInt32(Z'D8A1E681'), 14)
    B = C + RotateLeft(B + GRound(C, D, A) + X( 4) + ToInt32(Z'E7D3FBC8'), 20)
    A = B + RotateLeft(A + GRound(B, C, D) + X( 9) + ToInt32(Z'21E1CDE6'), 5)
    D = A + RotateLeft(D + GRound(A, B, C) + X(14) + ToInt32(Z'C33707D6'), 9)
    C = D + RotateLeft(C + GRound(D, A, B) + X( 3) + ToInt32(Z'F4D50D87'), 14)
    B = C + RotateLeft(B + GRound(C, D, A) + X( 8) + ToInt32(Z'455A14ED'), 20)
    A = B + RotateLeft(A + GRound(B, C, D) + X(13) + ToInt32(Z'A9E3E905'), 5)
    D = A + RotateLeft(D + GRound(A, B, C) + X( 2) + ToInt32(Z'FCEFA3F8'), 9)
    C = D + RotateLeft(C + GRound(D, A, B) + X( 7) + ToInt32(Z'676F02D9'), 14)
    B = C + RotateLeft(B + GRound(C, D, A) + X(12) + ToInt32(Z'8D2A4C8A'), 20)

    A = B + RotateLeft(A + HRound(B, C, D) + X( 5) + ToInt32(Z'FFFA3942'), 4)
    D = A + RotateLeft(D + HRound(A, B, C) + X( 8) + ToInt32(Z'8771F681'), 11)
    C = D + RotateLeft(C + HRound(D, A, B) + X(11) + ToInt32(Z'6D9D6122'), 16)
    B = C + RotateLeft(B + HRound(C, D, A) + X(14) + ToInt32(Z'FDE5380C'), 23)
    A = B + RotateLeft(A + HRound(B, C, D) + X( 1) + ToInt32(Z'A4BEEA44'), 4)
    D = A + RotateLeft(D + HRound(A, B, C) + X( 4) + ToInt32(Z'4BDECFA9'), 11)
    C = D + RotateLeft(C + HRound(D, A, B) + X( 7) + ToInt32(Z'F6BB4B60'), 16)
    B = C + RotateLeft(B + HRound(C, D, A) + X(10) + ToInt32(Z'BEBFBC70'), 23)
    A = B + RotateLeft(A + HRound(B, C, D) + X(13) + ToInt32(Z'289B7EC6'), 4)
    D = A + RotateLeft(D + HRound(A, B, C) + X( 0) + ToInt32(Z'EAA127FA'), 11)
    C = D + RotateLeft(C + HRound(D, A, B) + X( 3) + ToInt32(Z'D4EF3085'), 16)
    B = C + RotateLeft(B + HRound(C, D, A) + X( 6) + ToInt32(Z'04881D05'), 23)
    A = B + RotateLeft(A + HRound(B, C, D) + X( 9) + ToInt32(Z'D9D4D039'), 4)
    D = A + RotateLeft(D + HRound(A, B, C) + X(12) + ToInt32(Z'E6DB99E5'), 11)
    C = D + RotateLeft(C + HRound(D, A, B) + X(15) + ToInt32(Z'1FA27CF8'), 16)
    B = C + RotateLeft(B + HRound(C, D, A) + X( 2) + ToInt32(Z'C4AC5665'), 23)

    A = B + RotateLeft(A + IRound(B, C, D) + X( 0) + ToInt32(Z'F4292244'), 6)
    D = A + RotateLeft(D + IRound(A, B, C) + X( 7) + ToInt32(Z'432AFF97'), 10)
    C = D + RotateLeft(C + IRound(D, A, B) + X(14) + ToInt32(Z'AB9423A7'), 15)
    B = C + RotateLeft(B + IRound(C, D, A) + X( 5) + ToInt32(Z'FC93A039'), 21)
    A = B + RotateLeft(A + IRound(B, C, D) + X(12) + ToInt32(Z'655B59C3'), 6)
    D = A + RotateLeft(D + IRound(A, B, C) + X( 3) + ToInt32(Z'8F0CCC92'), 10)
    C = D + RotateLeft(C + IRound(D, A, B) + X(10) + ToInt32(Z'FFEFF47D'), 15)
    B = C + RotateLeft(B + IRound(C, D, A) + X( 1) + ToInt32(Z'85845DD1'), 21)
    A = B + RotateLeft(A + IRound(B, C, D) + X( 8) + ToInt32(Z'6FA87E4F'), 6)
    D = A + RotateLeft(D + IRound(A, B, C) + X(15) + ToInt32(Z'FE2CE6E0'), 10)
    C = D + RotateLeft(C + IRound(D, A, B) + X( 6) + ToInt32(Z'A3014314'), 15)
    B = C + RotateLeft(B + IRound(C, D, A) + X(13) + ToInt32(Z'4E0811A1'), 21)
    A = B + RotateLeft(A + IRound(B, C, D) + X( 4) + ToInt32(Z'F7537E82'), 6)
    D = A + RotateLeft(D + IRound(A, B, C) + X(11) + ToInt32(Z'BD3AF235'), 10)
    C = D + RotateLeft(C + IRound(D, A, B) + X( 2) + ToInt32(Z'2AD7D2BB'), 15)
    B = C + RotateLeft(B + IRound(C, D, A) + X( 9) + ToInt32(Z'EB86D391'), 21)

    MD%State(0) = MD%State(0) + A
    MD%State(1) = MD%State(1) + B
    MD%State(2) = MD%State(2) + C
    MD%State(3) = MD%State(3) + D
        
    RETURN

#undef FRound
#undef GRound
#undef HRound
#undef IRound

END SUBROUTINE MD5_ProcessBlock

!******************************************************************************

SUBROUTINE MD5_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD5), INTENT(INOUT)   :: MD           !! 'MD5' object
    tByte,      INTENT(INOUT)   :: BytesOut(0:) !! the output buffer
    tIndex,     INTENT(IN)      :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    CALL MD%HelperPadding()
    CALL ByteUnpackLE(MD%State, BytesOut, Offset)

    RETURN

END SUBROUTINE MD5_DoPadding

!******************************************************************************

SUBROUTINE MD5_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD5), INTENT(INOUT)   :: MD           !! 'MD5' object
    tByte,      INTENT(IN)      :: LastByte     !! the last byte
    tByte,      INTENT(IN)      :: NBits        !! number of bits in the last byte
    tByte,      INTENT(INOUT)   :: BytesOut(0:) !! the output buffer
    tIndex,     INTENT(IN)      :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! MD5 algorithm does not handle partial bytes
    CALL MD%DoPadding(BytesOut, Offset)
    ASSOCIATE(Dummy => LastByte, Dummy2 => NBits); END ASSOCIATE

    RETURN

END SUBROUTINE MD5_AddBitsNPad

!******************************************************************************

END MODULE MClass_MD5
    
!******************************************************************************
