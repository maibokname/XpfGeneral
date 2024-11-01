
MODULE MClass_SHA0

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SHA0* type and its related routines.
!   The *SHA0* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdhelper.html#type-mdhelper">MDHelper</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *SHA0* type implements an incremental cryptographic hash
!   function by employing the *SHA-0 message-digest* algorithm [1].
!   The implementation here is mainly based on the *SPHLIB*
!   implementation [2].  <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://en.wikipedia.org/wiki/Secure_Hash_Algorithms">Secure
!       Hash Algorithms. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_BytePack,           ONLY: BytePackBE, ByteUnpackBE
    USE MClass_BaseDigest
    USE MClass_MDHelper

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: SHA0

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tIndex,   PARAMETER :: BlockLen  = 64_kIndex
    tIndex,   PARAMETER :: DigestLen = 20_kIndex
    tInteger, PARAMETER :: IV(0:4) = [                      &
            ToInt32(Z'67452301'), ToInt32(Z'EFCDAB89'), &
            ToInt32(Z'98BADCFE'), ToInt32(Z'10325476'), &
            ToInt32(Z'C3D2E1F0')]

!** DERIVED TYPE DEFINITIONS
    !> *SHA0* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing
    !  the *SHA-0 message-digest* algorithm.
    TYPE, EXTENDS(MDHelper) :: SHA0
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state
        tInteger    :: State(0:4) = IV(0:4)
    CONTAINS
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object before starting using it.
        PROCEDURE       :: Initialize   => SHA0_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => SHA0_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone         => SHA0_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName          => SHA0_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen     => SHA0_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen      => SHA0_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr        => SHA0_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock     => SHA0_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding        => SHA0_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad      => SHA0_AddBitsNPad
    END TYPE SHA0

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE SHA0_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA0), INTENT(INOUT) :: MD    !! 'SHA0' object

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLogical, PARAMETER :: LittleEndian = FalseVal
        
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL MD%HelperInit(LittleEndian, 8_kIndex)
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE SHA0_Initialize

!******************************************************************************

SUBROUTINE SHA0_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA0), INTENT(INOUT)  :: MD   !! 'SHA0' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    MD%State = IV
    CALL MD%HelperReset()

    RETURN

END SUBROUTINE SHA0_Reset

!******************************************************************************

SUBROUTINE SHA0_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA0),                    INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(SHA0 :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (SHA0)
        CALL Dst%Create()
        Dst%State  = Src%State
        Dst%BufArr = Src%BufArr
        CALL Src%HelperClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE SHA0_GetClone

!******************************************************************************

FUNCTION SHA0_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA0), INTENT(IN) :: MD       !! 'SHA0' object
    tCharAlloc              :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'SHA-0'
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION SHA0_GetName

!******************************************************************************

FUNCTION SHA0_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA0), INTENT(IN) :: MD       !! 'SHA0' object
    tIndex                  :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = DigestLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION SHA0_GetDigestLen

!******************************************************************************

FUNCTION SHA0_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA0), INTENT(IN) :: MD       !! 'SHA0' object
    tIndex                  :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION SHA0_GetBlockLen

!******************************************************************************

SUBROUTINE SHA0_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA0), TARGET, INTENT(INOUT)  :: MD           !! 'SHA0' object
    tByte,      POINTER, INTENT(INOUT)  :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE SHA0_SetBufPtr

!******************************************************************************

SUBROUTINE SHA0_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA0), INTENT(INOUT)  :: MD           !! 'SHA0' object
    tByte,       INTENT(IN)     :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL PARAMETER DECLARATIONS:
    tInteger, PARAMETER :: K1   = ToInt32(Z'5A827999')
    tInteger, PARAMETER :: K2   = ToInt32(Z'6ED9EBA1')
    tInteger, PARAMETER :: K3   = ToInt32(Z'8F1BBCDC')
    tInteger, PARAMETER :: K4   = ToInt32(Z'CA62C1D6')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: A, B, C, D, E
    tInteger    :: W(0:15)

!** SUBROUTINE MACRO DEFINITIONS:
#define FRound(X, Y, Z)     IEOR(IAND(IEOR(Y, Z), X), Z)
#define GRound(X, Y, Z)     IEOR(IEOR(X, Y), Z)
#define HRound(X, Y, Z)     IOR(IAND(Z, Y), IAND(IOR(Z, Y), X))
#define IRound(X, Y, Z)     GRound(X, Y, Z)
#define XOR4(A, B, C, D)    IEOR(IEOR(IEOR(A, B), C), D)

! FLOW

    A = MD%State(0)
    B = MD%State(1)
    C = MD%State(2)
    D = MD%State(3)
    E = MD%State(4)

    CALL BytePackBE(BytesIn, 0_kIndex, W)
        
    ! F round
    E = RotateLeft(A, 5) + FRound(B, C, D) + E + W(0) + K1
    B = RotateLeft(B, 30)
    D = RotateLeft(E, 5) + FRound(A, B, C) + D + W(1) + K1
    A = RotateLeft(A, 30)
    C = RotateLeft(D, 5) + FRound(E, A, B) + C + W(2) + K1
    E = RotateLeft(E, 30)
    B = RotateLeft(C, 5) + FRound(D, E, A) + B + W(3) + K1
    D = RotateLeft(D, 30)
    A = RotateLeft(B, 5) + FRound(C, D, E) + A + W(4) + K1
    C = RotateLeft(C, 30)
    E = RotateLeft(A, 5) + FRound(B, C, D) + E + W(5) + K1
    B = RotateLeft(B, 30)
    D = RotateLeft(E, 5) + FRound(A, B, C) + D + W(6) + K1
    A = RotateLeft(A, 30)
    C = RotateLeft(D, 5) + FRound(E, A, B) + C + W(7) + K1
    E = RotateLeft(E, 30)
    B = RotateLeft(C, 5) + FRound(D, E, A) + B + W(8) + K1
    D = RotateLeft(D, 30)
    A = RotateLeft(B, 5) + FRound(C, D, E) + A + W(9) + K1
    C = RotateLeft(C, 30)
    E = RotateLeft(A, 5) + FRound(B, C, D) + E + W(10) + K1
    B = RotateLeft(B, 30)
    D = RotateLeft(E, 5) + FRound(A, B, C) + D + W(11) + K1
    A = RotateLeft(A, 30)
    C = RotateLeft(D, 5) + FRound(E, A, B) + C + W(12) + K1
    E = RotateLeft(E, 30)
    B = RotateLeft(C, 5) + FRound(D, E, A) + B + W(13) + K1
    D = RotateLeft(D, 30)
    A = RotateLeft(B, 5) + FRound(C, D, E) + A + W(14) + K1
    C = RotateLeft(C, 30)
    E = RotateLeft(A, 5) + FRound(B, C, D) + E + W(15) + K1
    B = RotateLeft(B, 30)
    W(0) = XOR4(W(13), W(8), W(2), W(0))
    D = RotateLeft(E, 5) + FRound(A, B, C) + D + W(0) + K1
    A = RotateLeft(A, 30)
    W(1) = XOR4(W(14), W(9), W(3), W(1))
    C = RotateLeft(D, 5) + FRound(E, A, B) + C + W(1) + K1
    E = RotateLeft(E, 30)
    W(2) = XOR4(W(15), W(10), W(4), W(2))
    B = RotateLeft(C, 5) + FRound(D, E, A) + B + W(2) + K1
    D = RotateLeft(D, 30)
    W(3) = XOR4(W(0), W(11), W(5), W(3))
    A = RotateLeft(B, 5) + FRound(C, D, E) + A + W(3) + K1
    C = RotateLeft(C, 30)
        
    ! G round
    W(4) = XOR4(W(1), W(12), W(6), W(4))
    E = RotateLeft(A, 5) + GRound(B, C, D) + E + W(4) + K2
    B = RotateLeft(B, 30)
    W(5) = XOR4(W(2), W(13), W(7), W(5))
    D = RotateLeft(E, 5) + GRound(A, B, C) + D + W(5) + K2
    A = RotateLeft(A, 30)
    W(6) = XOR4(W(3), W(14), W(8), W(6))
    C = RotateLeft(D, 5) + GRound(E, A, B) + C + W(6) + K2
    E = RotateLeft(E, 30)
    W(7) = XOR4(W(4), W(15), W(9), W(7))
    B = RotateLeft(C, 5) + GRound(D, E, A) + B + W(7) + K2
    D = RotateLeft(D, 30)
    W(8) = XOR4(W(5), W(0), W(10), W(8))
    A = RotateLeft(B, 5) + GRound(C, D, E) + A + W(8) + K2
    C = RotateLeft(C, 30)
    W(9) = XOR4(W(6), W(1), W(11), W(9))
    E = RotateLeft(A, 5) + GRound(B, C, D) + E + W(9) + K2
    B = RotateLeft(B, 30)
    W(10) = XOR4(W(7), W(2), W(12), W(10))
    D = RotateLeft(E, 5) + GRound(A, B, C) + D + W(10) + K2
    A = RotateLeft(A, 30)
    W(11) = XOR4(W(8), W(3), W(13), W(11))
    C = RotateLeft(D, 5) + GRound(E, A, B) + C + W(11) + K2
    E = RotateLeft(E, 30)
    W(12) = XOR4(W(9), W(4), W(14), W(12))
    B = RotateLeft(C, 5) + GRound(D, E, A) + B + W(12) + K2
    D = RotateLeft(D, 30)
    W(13) = XOR4(W(10), W(5), W(15), W(13))
    A = RotateLeft(B, 5) + GRound(C, D, E) + A + W(13) + K2
    C = RotateLeft(C, 30)
    W(14) = XOR4(W(11), W(6), W(0), W(14))
    E = RotateLeft(A, 5) + GRound(B, C, D) + E + W(14) + K2
    B = RotateLeft(B, 30)
    W(15) = XOR4(W(12), W(7), W(1), W(15))
    D = RotateLeft(E, 5) + GRound(A, B, C) + D + W(15) + K2
    A = RotateLeft(A, 30)
    W(0) = XOR4(W(13), W(8), W(2), W(0))
    C = RotateLeft(D, 5) + GRound(E, A, B) + C + W(0) + K2
    E = RotateLeft(E, 30)
    W(1) = XOR4(W(14), W(9), W(3), W(1))
    B = RotateLeft(C, 5) + GRound(D, E, A) + B + W(1) + K2
    D = RotateLeft(D, 30)
    W(2) = XOR4(W(15), W(10), W(4), W(2))
    A = RotateLeft(B, 5) + GRound(C, D, E) + A + W(2) + K2
    C = RotateLeft(C, 30)
    W(3) = XOR4(W(0), W(11), W(5), W(3))
    E = RotateLeft(A, 5) + GRound(B, C, D) + E + W(3) + K2
    B = RotateLeft(B, 30)
    W(4) = XOR4(W(1), W(12), W(6), W(4))
    D = RotateLeft(E, 5) + GRound(A, B, C) + D + W(4) + K2
    A = RotateLeft(A, 30)
    W(5) = XOR4(W(2), W(13), W(7), W(5))
    C = RotateLeft(D, 5) + GRound(E, A, B) + C + W(5) + K2
    E = RotateLeft(E, 30)
    W(6) = XOR4(W(3), W(14), W(8), W(6))
    B = RotateLeft(C, 5) + GRound(D, E, A) + B + W(6) + K2
    D = RotateLeft(D, 30)
    W(7) = XOR4(W(4), W(15), W(9), W(7))
    A = RotateLeft(B, 5) + GRound(C, D, E) + A + W(7) + K2
    C = RotateLeft(C, 30)
        
    ! H round
    W(8) = XOR4(W(5), W(0), W(10), W(8))
    E = RotateLeft(A, 5) + HRound(B, C, D) + E + W(8) + K3
    B = RotateLeft(B, 30)
    W(9) = XOR4(W(6), W(1), W(11), W(9))
    D = RotateLeft(E, 5) + HRound(A, B, C) + D + W(9) + K3
    A = RotateLeft(A, 30)
    W(10) = XOR4(W(7), W(2), W(12), W(10))
    C = RotateLeft(D, 5) + HRound(E, A, B) + C + W(10) + K3
    E = RotateLeft(E, 30)
    W(11) = XOR4(W(8), W(3), W(13), W(11))
    B = RotateLeft(C, 5) + HRound(D, E, A) + B + W(11) + K3
    D = RotateLeft(D, 30)
    W(12) = XOR4(W(9), W(4), W(14), W(12))
    A = RotateLeft(B, 5) + HRound(C, D, E) + A + W(12) + K3
    C = RotateLeft(C, 30)
    W(13) = XOR4(W(10), W(5), W(15), W(13))
    E = RotateLeft(A, 5) + HRound(B, C, D) + E + W(13) + K3
    B = RotateLeft(B, 30)
    W(14) = XOR4(W(11), W(6), W(0), W(14))
    D = RotateLeft(E, 5) + HRound(A, B, C) + D + W(14) + K3
    A = RotateLeft(A, 30)
    W(15) = XOR4(W(12), W(7), W(1), W(15))
    C = RotateLeft(D, 5) + HRound(E, A, B) + C + W(15) + K3
    E = RotateLeft(E, 30)
    W(0) = XOR4(W(13), W(8), W(2), W(0))
    B = RotateLeft(C, 5) + HRound(D, E, A) + B + W(0) + K3
    D = RotateLeft(D, 30)
    W(1) = XOR4(W(14), W(9), W(3), W(1))
    A = RotateLeft(B, 5) + HRound(C, D, E) + A + W(1) + K3
    C = RotateLeft(C, 30)
    W(2) = XOR4(W(15), W(10), W(4), W(2))
    E = RotateLeft(A, 5) + HRound(B, C, D) + E + W(2) + K3
    B = RotateLeft(B, 30)
    W(3) = XOR4(W(0), W(11), W(5), W(3))
    D = RotateLeft(E, 5) + HRound(A, B, C) + D + W(3) + K3
    A = RotateLeft(A, 30)
    W(4) = XOR4(W(1), W(12), W(6), W(4))
    C = RotateLeft(D, 5) + HRound(E, A, B) + C + W(4) + K3
    E = RotateLeft(E, 30)
    W(5) = XOR4(W(2), W(13), W(7), W(5))
    B = RotateLeft(C, 5) + HRound(D, E, A) + B + W(5) + K3
    D = RotateLeft(D, 30)
    W(6) = XOR4(W(3), W(14), W(8), W(6))
    A = RotateLeft(B, 5) + HRound(C, D, E) + A + W(6) + K3
    C = RotateLeft(C, 30)
    W(7) = XOR4(W(4), W(15), W(9), W(7))
    E = RotateLeft(A, 5) + HRound(B, C, D) + E + W(7) + K3
    B = RotateLeft(B, 30)
    W(8) = XOR4(W(5), W(0), W(10), W(8))
    D = RotateLeft(E, 5) + HRound(A, B, C) + D + W(8) + K3
    A = RotateLeft(A, 30)
    W(9) = XOR4(W(6), W(1), W(11), W(9))
    C = RotateLeft(D, 5) + HRound(E, A, B) + C + W(9) + K3
    E = RotateLeft(E, 30)
    W(10) = XOR4(W(7), W(2), W(12), W(10))
    B = RotateLeft(C, 5) + HRound(D, E, A) + B + W(10) + K3
    D = RotateLeft(D, 30)
    W(11) = XOR4(W(8), W(3), W(13), W(11))
    A = RotateLeft(B, 5) + HRound(C, D, E) + A + W(11) + K3
    C = RotateLeft(C, 30)
        
    ! I round where IRound = GRound
    W(12) = XOR4(W(9), W(4), W(14), W(12))
    E = RotateLeft(A, 5) + IRound(B, C, D) + E + W(12) + K4
    B = RotateLeft(B, 30)
    W(13) = XOR4(W(10), W(5), W(15), W(13))
    D = RotateLeft(E, 5) + IRound(A, B, C) + D + W(13) + K4
    A = RotateLeft(A, 30)
    W(14) = XOR4(W(11), W(6), W(0), W(14))
    C = RotateLeft(D, 5) + IRound(E, A, B) + C + W(14) + K4
    E = RotateLeft(E, 30)
    W(15) = XOR4(W(12), W(7), W(1), W(15))
    B = RotateLeft(C, 5) + IRound(D, E, A) + B + W(15) + K4
    D = RotateLeft(D, 30)
    W(0) = XOR4(W(13), W(8), W(2), W(0))
    A = RotateLeft(B, 5) + IRound(C, D, E) + A + W(0) + K4
    C = RotateLeft(C, 30)
    W(1) = XOR4(W(14), W(9), W(3), W(1))
    E = RotateLeft(A, 5) + IRound(B, C, D) + E + W(1) + K4
    B = RotateLeft(B, 30)
    W(2) = XOR4(W(15), W(10), W(4), W(2))
    D = RotateLeft(E, 5) + IRound(A, B, C) + D + W(2) + K4
    A = RotateLeft(A, 30)
    W(3) = XOR4(W(0), W(11), W(5), W(3))
    C = RotateLeft(D, 5) + IRound(E, A, B) + C + W(3) + K4
    E = RotateLeft(E, 30)
    W(4) = XOR4(W(1), W(12), W(6), W(4))
    B = RotateLeft(C, 5) + IRound(D, E, A) + B + W(4) + K4
    D = RotateLeft(D, 30)
    W(5) = XOR4(W(2), W(13), W(7), W(5))
    A = RotateLeft(B, 5) + IRound(C, D, E) + A + W(5) + K4
    C = RotateLeft(C, 30)
    W(6) = XOR4(W(3), W(14), W(8), W(6))
    E = RotateLeft(A, 5) + IRound(B, C, D) + E + W(6) + K4
    B = RotateLeft(B, 30)
    W(7) = XOR4(W(4), W(15), W(9), W(7))
    D = RotateLeft(E, 5) + IRound(A, B, C) + D + W(7) + K4
    A = RotateLeft(A, 30)
    W(8) = XOR4(W(5), W(0), W(10), W(8))
    C = RotateLeft(D, 5) + IRound(E, A, B) + C + W(8) + K4
    E = RotateLeft(E, 30)
    W(9) = XOR4(W(6), W(1), W(11), W(9))
    B = RotateLeft(C, 5) + IRound(D, E, A) + B + W(9) + K4
    D = RotateLeft(D, 30)
    W(10) = XOR4(W(7), W(2), W(12), W(10))
    A = RotateLeft(B, 5) + IRound(C, D, E) + A + W(10) + K4
    C = RotateLeft(C, 30)
    W(11) = XOR4(W(8), W(3), W(13), W(11))
    E = RotateLeft(A, 5) + IRound(B, C, D) + E + W(11) + K4
    B = RotateLeft(B, 30)
    W(12) = XOR4(W(9), W(4), W(14), W(12))
    D = RotateLeft(E, 5) + IRound(A, B, C) + D + W(12) + K4
    A = RotateLeft(A, 30)
    W(13) = XOR4(W(10), W(5), W(15), W(13))
    C = RotateLeft(D, 5) + IRound(E, A, B) + C + W(13) + K4
    E = RotateLeft(E, 30)
    W(14) = XOR4(W(11), W(6), W(0), W(14))
    B = RotateLeft(C, 5) + IRound(D, E, A) + B + W(14) + K4
    D = RotateLeft(D, 30)
    W(15) = XOR4(W(12), W(7), W(1), W(15))
    A = RotateLeft(B, 5) + IRound(C, D, E) + A + W(15) + K4
    C = RotateLeft(C, 30)

    MD%State(0) = MD%State(0) + A
    MD%State(1) = MD%State(1) + B
    MD%State(2) = MD%State(2) + C
    MD%State(3) = MD%State(3) + D
    MD%State(4) = MD%State(4) + E
        
    RETURN

#undef FRound
#undef GRound
#undef HRound
#undef IRound
#undef XOR4

END SUBROUTINE SHA0_ProcessBlock

!******************************************************************************

SUBROUTINE SHA0_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA0), INTENT(INOUT)  :: MD           !! 'SHA0' object
    tByte,       INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,      INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    CALL MD%HelperPadding()
    CALL ByteUnpackBE(MD%State, BytesOut, Offset)

    RETURN

END SUBROUTINE SHA0_DoPadding

!******************************************************************************

SUBROUTINE SHA0_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA0), INTENT(INOUT)  :: MD           !! 'SHA0' object
    tByte,       INTENT(IN)     :: LastByte     !! the last byte
    tByte,       INTENT(IN)     :: NBits        !! number of bits in the last byte
    tByte,       INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,      INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! SHA-0 algorithm does not handle partial bytes
    CALL MD%DoPadding(BytesOut, Offset)
    ASSOCIATE(Dummy => LastByte, Dummy2 => NBits); END ASSOCIATE

    RETURN

END SUBROUTINE SHA0_AddBitsNPad

!******************************************************************************

END MODULE MClass_SHA0
    
!******************************************************************************
