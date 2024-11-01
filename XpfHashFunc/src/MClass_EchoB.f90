
MODULE MClass_EChoB

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *EChoB* type and its related routines.
!   The *EChoB* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *EChoB* type implements an incremental cryptographic hash function
!   by employing either the *ECHO-384* or the *ECHO-512* algorithm [1].  The
!   implementation here is based mainly on the *SPHLIB* implementation [2].  <br>
!   By default, the *EChoB* type employs the *ECHO-512 message-digest*
!   algorithm.  However, a user can specify the *IsECHO384* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *ECHO-384 message-digest* algorithm
!   instead of the default one. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://web.archive.org/web/20180315075044/http://crypto.rd.francetelecom.com/ECHO/">
!       ECHO hash function. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_BytePack,           ONLY: BytePackLE, ByteUnpackLE
    USE MClass_BaseDigest
    USE MClass_MDEngine

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: EChoB

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64
#include    "Includes/AES_LittleEndian.f90"

!** MODULE PARAMETERS:
#include    "Includes/AES_Constants.f90"
    tIndex,   PARAMETER :: BlockLen = 128_kIndex
    tIndex,   PARAMETER :: DLen384  = 48_kIndex
    tIndex,   PARAMETER :: DLen512  = 64_kIndex

!** DERIVED TYPE DEFINITIONS
    !> *EChoB* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing either
    !  the *ECHO-384* or the *ECHO-512 message-digest* algorithm.
    TYPE, EXTENDS(MDEngine) :: EChoB
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state and counter variables
        tInteger    :: V(0:31) = 0
        tInteger    :: C(0:3) = 0
        !% flag indicating whether the ECHO-384 algorithm is employed or not.
        tLogical    :: IsECHO384 = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                    Private Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => EChoB_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (ECHO-512).
        PROCEDURE       :: Initialize   => EChoB_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => EChoB_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => EChoB_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => EChoB_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => EChoB_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => EChoB_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => EChoB_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => EChoB_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => EChoB_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => EChoB_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (ECHO-512) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the ECHO-384 algorithm <br>
        !   --->    CALL MD%Create(IsECHO384=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
        ! ---------------------------------------------------------------------
    END TYPE EChoB

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE EChoB_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(EChoB), INTENT(INOUT) :: MD    !! 'EChoB' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the ECHO-512 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE EChoB_Initialize

!******************************************************************************

SUBROUTINE EChoB_Initialize_wFlag(MD, IsECHO384)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(EChoB), INTENT(INOUT) :: MD           !! 'EChoB' object
    tLogical,     INTENT(IN)    :: IsECHO384
    !^ flag indicating whether the ECHO-384 algorithm is employed or not. <br>
    !  - If true, use the ECHO-384 algorithm. <br>
    !  - Otherwise, use the ECHO-512 algorithm. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsECHO384 = IsECHO384
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE EChoB_Initialize_wFlag

!******************************************************************************

SUBROUTINE EChoB_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(EChoB), INTENT(INOUT) :: MD   !! 'EChoB' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex  :: I

! FLOW
        
    MD%BufArr = 0_kInt8
    MD%C = 0
    MD%V = 0
    MD%V(0)  = ToInt32(SHIFTL(MD%GetDigestLen(), 3))
    DO I = 1, 7
        MD%V(I*4) = MD%V(0)
    END DO
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE EChoB_Reset

!******************************************************************************

SUBROUTINE EChoB_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(EChoB),                   INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(EChoB :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (EChoB)
        CALL Dst%Create(Src%IsECHO384)
        Dst%V      = Src%V
        Dst%C      = Src%C
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE EChoB_GetClone

!******************************************************************************

FUNCTION EChoB_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(EChoB), INTENT(IN)    :: MD       !! 'EChoB' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsECHO384) THEN
        Name = 'ECHO-384'
    ELSE
        Name = 'ECHO-512'
    END IF

    RETURN

END FUNCTION EChoB_GetName

!******************************************************************************

FUNCTION EChoB_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(EChoB), INTENT(IN)    :: MD       !! 'EChoB' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsECHO384) THEN
        Length = DLen384
    ELSE
        Length = DLen512
    END IF

    RETURN

END FUNCTION EChoB_GetDigestLen

!******************************************************************************

FUNCTION EChoB_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(EChoB), INTENT(IN)    :: MD       !! 'EChoB' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION EChoB_GetBlockLen

!******************************************************************************

SUBROUTINE EChoB_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(EChoB), TARGET, INTENT(INOUT) :: MD           !! 'EChoB' object
    tByte,       POINTER, INTENT(INOUT) :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE EChoB_SetBufPtr

!******************************************************************************

SUBROUTINE EChoB_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(EChoB), INTENT(INOUT) :: MD           !! 'EChoB' object
    tByte,        INTENT(IN)    :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: IncVal

! FLOW
    
    ! increment value
    IncVal = 1024
    
    ! increment counters
    CALL IncrementCounters(MD%C(0), MD%C(1), MD%C(2), MD%C(3), IncVal)

    ! compress
    CALL Compress(MD%V, MD%C(0), MD%C(1), MD%C(2), MD%C(3), BytesIn)

    RETURN

END SUBROUTINE EChoB_ProcessBlock

!******************************************************************************

SUBROUTINE EChoB_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(EChoB), INTENT(INOUT) :: MD           !! 'EChoB' object
    tByte,        INTENT(INOUT) :: BytesOut(0:) !! the output buffer
    tIndex,       INTENT(IN)    :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE EChoB_DoPadding

!******************************************************************************

SUBROUTINE EChoB_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(EChoB), INTENT(INOUT) :: MD           !! 'EChoB' object
    tByte,        INTENT(IN)    :: LastByte     !! the last byte
    tByte,        INTENT(IN)    :: NBits        !! number of bits in the last byte
    tByte,        INTENT(INOUT) :: BytesOut(0:) !! the output buffer
    tIndex,       INTENT(IN)    :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Ptr, OutLen, I
    tInteger    :: C0, C1, C2, C3, ELen
    tByte       :: Z

! FLOW

    ! padding
    ASSOCIATE(TmpBuf => MD%BufArr)
        Ptr = MD%GetBufLen()
        ELen = SHIFTL(Ptr, 3) + NBits
        CALL IncrementCounters(MD%C(0), MD%C(1), MD%C(2), MD%C(3), ELen)
        C0 = MD%C(0)
        C1 = MD%C(1)
        C2 = MD%C(2)
        C3 = MD%C(3)
        ! If ELen is zero, then this block actually contains no message
        ! bit, only the first padding bit.
        IF (ELen == 0) THEN
            MD%C(0) = 0
            MD%C(1) = 0
            MD%C(2) = 0
            MD%C(3) = 0
        END IF
        Z = SHIFTR(FByte80, NBits)
        TmpBuf(Ptr) = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
        Ptr = Ptr + 1
        IF (Ptr > 110) THEN
            TmpBuf(Ptr:127) = FByte00
            CALL Compress(MD%V, MD%C(0), MD%C(1), MD%C(2), MD%C(3), TmpBuf)
            MD%C(0) = 0
            MD%C(1) = 0
            MD%C(2) = 0
            MD%C(3) = 0
            Ptr = 0
        END IF
        TmpBuf(Ptr:109) = FByte00
        OutLen = SHIFTL(MD%GetDigestLen(), 3)
        TmpBuf(110) = ToInt8(OutLen)
        TmpBuf(111) = ToInt8(SHIFTR(OutLen, 8))
        CALL ByteUnpackLE(C0, TmpBuf, 112_kIndex)
        CALL ByteUnpackLE(C1, TmpBuf, 116_kIndex)
        CALL ByteUnpackLE(C2, TmpBuf, 120_kIndex)
        CALL ByteUnpackLE(C3, TmpBuf, 124_kIndex)
        CALL Compress(MD%V, MD%C(0), MD%C(1), MD%C(2), MD%C(3), TmpBuf)
    END ASSOCIATE

    ! finalizing
    OutLen = SHIFTR(OutLen, 5)
    DO I = 0, OutLen-1
        CALL ByteUnpackLE(MD%V(I), BytesOut, Offset + SHIFTL(I, 2))
    END DO
        
    RETURN

END SUBROUTINE EChoB_AddBitsNPad

!******************************************************************************

SUBROUTINE IncrementCounters(C0, C1, C2, C3, IncVal)

!** PURPOSE OF THIS SUBROUTINE:
    ! To increment counters.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: C0, C1, C2, C3
    tInteger, INTENT(IN)    :: IncVal

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    C0 = C0 + IncVal
    IF ((C0 >= 0).AND.(C0 < IncVal)) THEN
        C1 = C1 + 1
        IF (C1 == 0) THEN
            C2 = C2 + 1
            IF (C2 == 0) THEN
                C3 = C3 + 1
            END IF
        END IF
    END IF

    RETURN
    
END SUBROUTINE IncrementCounters

!******************************************************************************

SUBROUTINE Compress(V, C0, C1, C2, C3, InpDat)

!** PURPOSE OF THIS SUBROUTINE:
    ! To compress input block.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: V(0:)            ! state values
    tInteger, INTENT(IN)    :: C0, C1, C2, C3   ! constants
    tByte,    INTENT(IN)    :: InpDat(0:)       ! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger            :: K0, K1, K2, K3
    tInteger            :: A, Tmp
    tInteger            :: Y0, Y1, Y2, Y3
    tInteger, TARGET    :: W2D(0:3,0:15)
    tInteger, POINTER   :: W1D(:)
    tIndex              :: U, I, J

!** SUBROUTINE MACRO DEFINITIONS:
#define AES_2ROUNDS(W, C)   \
        AES_ROUND_LE(W(0, C), W(1, C), W(2, C), W(3, C), K0, K1, K2, K3, Y0, Y1, Y2, Y3); \
        AES_ROUND_NOKEY_LE(Y0, Y1, Y2, Y3, W(0, C), W(1, C), W(2, C), W(3, C)); \
        K0 = K0 + 1; \
        IF (K0 == 0) THEN; \
            K1 = K1 + 1; \
            IF (K1 == 0) THEN; \
                K2 = K2 + 1; \
                IF (K2 == 0) THEN; \
                    K3 = K3 + 1; \
                END IF; \
            END IF; \
        END IF;
#define BIG_SUB_WORDS(W) \
    DO I = 0, 15; \
        AES_2ROUNDS(W, I); \
    END DO;
#define SHIFT_ROW1(W, A, B, C, D)  \
        Tmp = W(0, A); \
        W(0, A) = W(0, B); \
        W(0, B) = W(0, C); \
        W(0, C) = W(0, D); \
        W(0, D) = Tmp; \
        Tmp = W(1, A); \
        W(1, A) = W(1, B); \
        W(1, B) = W(1, C); \
        W(1, C) = W(1, D); \
        W(1, D) = Tmp; \
        Tmp = W(2, A); \
        W(2, A) = W(2, B); \
        W(2, B) = W(2, C); \
        W(2, C) = W(2, D); \
        W(2, D) = Tmp; \
        Tmp = W(3, A); \
        W(3, A) = W(3, B); \
        W(3, B) = W(3, C); \
        W(3, C) = W(3, D); \
        W(3, D) = Tmp;
#define SHIFT_ROW2(W, A, B, C, D)  \
        Tmp = W(0, A); \
        W(0, A) = W(0, C); \
        W(0, C) = Tmp; \
        Tmp = W(0, B); \
        W(0, B) = W(0, D); \
        W(0, D) = Tmp; \
        Tmp = W(1, A); \
        W(1, A) = W(1, C); \
        W(1, C) = Tmp; \
        Tmp = W(1, B); \
        W(1, B) = W(1, D); \
        W(1, D) = Tmp; \
        Tmp = W(2, A); \
        W(2, A) = W(2, C); \
        W(2, C) = Tmp; \
        Tmp = W(2, B); \
        W(2, B) = W(2, D); \
        W(2, D) = Tmp; \
        Tmp = W(3, A); \
        W(3, A) = W(3, C); \
        W(3, C) = Tmp; \
        Tmp = W(3, B); \
        W(3, B) = W(3, D); \
        W(3, D) = Tmp;
#define SHIFT_ROW3(W, A, B, C, D)   SHIFT_ROW1(W, D, C, B, A)
#define BIG_SHIFT_ROWS(W)  \
        SHIFT_ROW1(W, 1, 5, 9, 13); \
        SHIFT_ROW2(W, 2, 6, 10, 14); \
        SHIFT_ROW3(W, 3, 7, 11, 15);
#define BIG_MIX_COLUMNS(W) \
        CALL MixColumn(W, 0, 1, 2, 3); \
        CALL MixColumn(W, 4, 5, 6, 7); \
        CALL MixColumn(W, 8, 9, 10, 11); \
        CALL MixColumn(W, 12, 13, 14, 15);

! FLOW
    
    ! set Ks
    K0 = C0
    K1 = C1
    K2 = C2
    K3 = C3

    ! set pointer
    W1D(0:63) => W2D
    
    ! +++ INPUT_BLOCK_SMALL +++
    ! copy state values
    W1D(0:31) = V(0:31)
    ! get input block
    CALL BytePackLE(InpDat, 0_kIndex, W1D(32:63))

    DO U = 0, 9
        ! +++ BIG_ROUND +++
        BIG_SUB_WORDS(W2D)
        BIG_SHIFT_ROWS(W2D)
        BIG_MIX_COLUMNS(W2D)
    END DO

    ! +++ FINAL_SMALL +++
    ! get output and return state values
    J = 0_kIndex
    DO I = 0, 31
        CALL BytePackLE(InpDat, J, A)
        V(I) = IEOR(V(I), IEOR(IEOR(A, W1D(I)), W1D(I+32)))
        J = J + 4_kIndex
    END DO

    NULLIFY(W1D)

    RETURN
    
#undef AES_2ROUNDS
#undef BIG_SUB_WORDS
#undef SHIFT_ROW1
#undef SHIFT_ROW2
#undef SHIFT_ROW3
#undef BIG_SHIFT_ROWS
#undef MIX_COLUMN
#undef BIG_MIX_COLUMNS
#include "Includes/AES_Undef Macro.f90"

CONTAINS

    SUBROUTINE MixColumn(W, IA, IB, IC, ID)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform mixing of columns of W.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, INTENT(INOUT) :: W(0:,0:)
        tInteger, INTENT(IN)    :: IA, IB, IC, ID

    !** SUBROUTINE PARAMETER DECLARATIONS:
        tInteger, PARAMETER :: C1 = ToInt32(Z'80808080')
        tInteger, PARAMETER :: C2 = ToInt32(Z'7F7F7F7F')
        
    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger            :: A, B, C, D
        tInteger            :: AB, BC, CD
        tInteger            :: ABX, BCX, CDX
        tInteger            :: N

    ! FLOW

        DO N = 0, 3
            A = W(N, IA)
            B = W(N, IB)
            C = W(N, IC)
            D = W(N, ID)
            AB = IEOR(A, B)
            BC = IEOR(B, C)
            CD = IEOR(C, D)
            ABX = IEOR(SHIFTR(IAND(AB, C1), 7)*27, SHIFTL(IAND(AB, C2), 1))
            BCX = IEOR(SHIFTR(IAND(BC, C1), 7)*27, SHIFTL(IAND(BC, C2), 1))
            CDX = IEOR(SHIFTR(IAND(CD, C1), 7)*27, SHIFTL(IAND(CD, C2), 1))
            W(N, IA) = IEOR(IEOR(ABX, BC), D)
            W(N, IB) = IEOR(IEOR(BCX, A), CD)
            W(N, IC) = IEOR(IEOR(CDX, AB), D)
            W(N, ID) = IEOR(IEOR(IEOR(IEOR(ABX, BCX), CDX), AB), C)
        END DO

        RETURN
        
    END SUBROUTINE MixColumn

    !**************************************************************************

END SUBROUTINE Compress

!******************************************************************************

END MODULE MClass_EChoB
    
!******************************************************************************
