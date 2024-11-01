
MODULE MClass_MD2

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *MD2* type and its related routines.
!   The *MD2* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *MD2* type implements an incremental cryptographic hash
!   function by employing the *MD2 message-digest* algorithm [1]
!   where the algorithm is described in RFC 1319.  The implementation
!   here is mainly based on the *SPHLIB* implementation [2].  <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://datatracker.ietf.org/doc/html/rfc1319">RFC1319:
!       The MD2 Message-Digest Algorithm. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_BytePack,           ONLY: BytePackLE, ByteUnpackLE
    USE MClass_BaseDigest
    USE MClass_MDEngine

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: MD2

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tIndex,   PARAMETER :: BlockLen  = 16_kIndex
    tIndex,   PARAMETER :: DigestLen = 16_kIndex
    ! Internal "magic" table
    tInteger, PARAMETER    :: S(0:255) = [          &
             41,  46,  67, 201, 162, 216, 124,   1, &
             61,  54,  84, 161, 236, 240,   6,  19, &
             98, 167,   5, 243, 192, 199, 115, 140, &
            152, 147,  43, 217, 188,  76, 130, 202, &
             30, 155,  87,  60, 253, 212, 224,  22, &
            103,  66, 111,  24, 138,  23, 229,  18, &
            190,  78, 196, 214, 218, 158, 222,  73, &
            160, 251, 245, 142, 187,  47, 238, 122, &
            169, 104, 121, 145,  21, 178,   7,  63, &
            148, 194,  16, 137,  11,  34,  95,  33, &
            128, 127,  93, 154,  90, 144,  50,  39, &
             53,  62, 204, 231, 191, 247, 151,   3, &
            255,  25,  48, 179,  72, 165, 181, 209, &
            215,  94, 146,  42, 172,  86, 170, 198, &
             79, 184,  56, 210, 150, 164, 125, 182, &
            118, 252, 107, 226, 156, 116,   4, 241, &
             69, 157, 112,  89, 100, 113, 135,  32, &
            134,  91, 207, 101, 230,  45, 168,   2, &
             27,  96,  37, 173, 174, 176, 185, 246, &
             28,  70,  97, 105,  52,  64, 126,  15, &
             85,  71, 163,  35, 221,  81, 175,  58, &
            195,  92, 249, 206, 186, 197, 234,  38, &
             44,  83,  13, 110, 133,  40, 132,   9, &
            211, 223, 205, 244,  65, 129,  77,  82, &
            106, 220,  55, 200, 108, 193, 171, 250, &
             36, 225, 123,   8,  12, 189, 177,  74, &
            120, 136, 149, 139, 227,  99, 232, 109, &
            233, 203, 213, 254,  59,   0,  29,  57, &
            242, 239, 183,  14, 102,  88, 208, 228, &
            166, 119, 114, 248, 235, 117,  75,  10, &
             49,  68,  80, 180, 143, 237,  31,  26, &
            219, 153, 141,  51, 159,  17, 131,  20]

!** DERIVED TYPE DEFINITIONS
    !> *MD2* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing
    !  the *MD2 message-digest* algorithm.
    TYPE, EXTENDS(MDEngine) :: MD2
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state and counter variables
        tInteger    :: X(0:47)              = 0_kInt32
        tInteger    :: C(0:15)              = 0_kInt32
        tInteger    :: L                    = 0_kInt32
    CONTAINS
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object before starting using it.
        PROCEDURE       :: Initialize   => MD2_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => MD2_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone         => MD2_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName          => MD2_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen     => MD2_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen      => MD2_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr        => MD2_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock     => MD2_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding        => MD2_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad      => MD2_AddBitsNPad
    END TYPE MD2

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE MD2_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD2),       INTENT(INOUT) :: MD           !! 'MD2' object
        
!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE MD2_Initialize

!******************************************************************************

SUBROUTINE MD2_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD2), INTENT(INOUT)   :: MD   !! 'MD2' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    MD%BufArr = 0_kInt8
    MD%X = 0_kInt32
    MD%C = 0_kInt32
    MD%L = 0_kInt32
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE MD2_Reset

!******************************************************************************

SUBROUTINE MD2_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD2),                     INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(MD2 :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (MD2)
        CALL Dst%Create()
        Dst%X = Src%X
        Dst%C = Src%C
        Dst%L = Src%L
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE MD2_GetClone

!******************************************************************************

FUNCTION MD2_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD2), INTENT(IN)  :: MD       !! 'MD2' object
    tCharAlloc              :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'MD2'
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION MD2_GetName

!******************************************************************************

FUNCTION MD2_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD2), INTENT(IN)  :: MD       !! 'MD2' object
    tIndex                  :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = DigestLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION MD2_GetDigestLen

!******************************************************************************

FUNCTION MD2_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD2), INTENT(IN)  :: MD       !! 'MD2' object
    tIndex                  :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION MD2_GetBlockLen

!******************************************************************************

SUBROUTINE MD2_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD2), TARGET, INTENT(INOUT)   :: MD           !! 'MD2' object
    tByte,     POINTER, INTENT(INOUT)   :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE MD2_SetBufPtr

!******************************************************************************

SUBROUTINE MD2_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD2), INTENT(INOUT)   :: MD           !! 'MD2' object
    tByte,      INTENT(IN)      :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: TL, U
    tIndex      :: I, J, K, L, T

! FLOW
        
    TL = MD%L
    DO I = 0, 15
        U = IAND(ToInt32(BytesIn(I)), ToInt32(Z'000000FF'))
        MD%X(16 + I) = U
        MD%X(32 + I) = IEOR(MD%X(I), U)
        MD%C(I) = IEOR(MD%C(I), S(IEOR(U, TL)))
        TL = MD%C(I)
    END DO
    MD%L = TL
    T = 0
    DO J = 0, 17
        DO K = 0, 47, 8
            DO L = 0, 7
                MD%X(K+L) = IEOR(MD%X(K+L), S(T))
                T = ToIndex(MD%X(K+L))
            END DO
        END DO
        T = IAND((T + J), ToIndex(Z'000000FF'))
    END DO

    RETURN

END SUBROUTINE MD2_ProcessBlock

!******************************************************************************

SUBROUTINE MD2_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD2), INTENT(INOUT)   :: MD           !! 'MD2' object
    tByte,      INTENT(INOUT)   :: BytesOut(0:) !! the output buffer
    tIndex,     INTENT(IN)      :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: D(0:15)
    tIndex      :: Pending, I, J

! FLOW
        
    Pending = MD%GetBufLen()
    DO I = 0, (15 - Pending)
        CALL MD%Update(ToInt8(16_kIndex-Pending), 1_kIndex)
    END DO
    J = MD%GetBufLen()
    DO I = 0, 15
        D(I) = ToInt8(MD%C(I))
    END DO
    CALL MD%ProcessBlock(D)
    DO I = 0, 15
        BytesOut(Offset+I) = ToInt8(MD%X(I))
    END DO

    RETURN

END SUBROUTINE MD2_DoPadding

!******************************************************************************

SUBROUTINE MD2_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(MD2), INTENT(INOUT)   :: MD           !! 'MD2' object
    tByte,      INTENT(IN)      :: LastByte     !! the last byte
    tByte,      INTENT(IN)      :: NBits        !! number of bits in the last byte
    tByte,      INTENT(INOUT)   :: BytesOut(0:) !! the output buffer
    tIndex,     INTENT(IN)      :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    ! MD2 algorithm does not handle partial bytes
    CALL MD%DoPadding(BytesOut, Offset)
    ASSOCIATE(Dummy => LastByte, Dummy2 => NBits); END ASSOCIATE

    RETURN

END SUBROUTINE MD2_AddBitsNPad

!******************************************************************************

END MODULE MClass_MD2
    
!******************************************************************************
