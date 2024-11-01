
MODULE MClass_GroestlB

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *GroestlB* type and its related routines.
!   The *GroestlB* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *GroestlB* type implements an incremental cryptographic hash function
!   by employing either the *Groestl-384* or the *Groestl-512 message-digest*
!   algorithm [1].  The implementation here is based mainly on the *SPHLIB*
!   implementation [2].  <br>
!   By default, the *GroestlB* type employs the *Groestl-512 message-digest*
!   algorithm.  However, a user can specify the *IsGroestl384* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *Groestl-384 message-digest* algorithm
!   instead of the default one. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="http://www.groestl.info/">Grostl - a SHA-3 candidate. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_BytePack,           ONLY: BytePackBE, ByteUnpackBE
    USE MClass_BaseDigest
    USE MClass_MDEngine
    USE MClass_GroestlS,             ONLY: T0, T1, T2, T3, T4, T5, T6, T7

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: GroestlB

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64
#define BX64(X, Pos)    ToInt32(SHIFTR(X, Pos))
#define BY64(X, Pos)    IAND(ToInt32(SHIFTR(X, Pos)), ToInt32(Z'000000FF'))
#define RBTT(A, I0, I1, I2, I3, I4, I5, I6, I7) \
    IEOR(IEOR(IEOR(IEOR(IEOR(IEOR(IEOR(T0(BX64(A(I0), 56)),  \
                                       T1(BY64(A(I1), 48))), \
                                       T2(BY64(A(I2), 40))), \
                                       T3(BY64(A(I3), 32))), \
                                       T4(BY64(A(I4), 24))), \
                                       T5(BY64(A(I5), 16))), \
                                       T6(BY64(A(I6),  8))), \
                                       T7(BY64(A(I7),  0)))
#define PC64(A, J, R)   IEOR(A, SHIFTL(ToInt64(J + R), 56))
#define QC64(A, J, R)   IEOR(A, IEOR(-ToInt64(J), ToInt64(R)))

!** MODULE PARAMETERS:
    tIndex, PARAMETER   :: BlockLen = 128_kIndex
    tIndex, PARAMETER   :: DLen384  = 48_kIndex
    tIndex, PARAMETER   :: DLen512  = 64_kIndex

!** DERIVED TYPE DEFINITIONS
    !> *GroestlB* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing either
    !  the *Groestl-384* or the *Groestl-512 message-digest* algorithm.
    TYPE, EXTENDS(MDEngine) :: GroestlB
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state variable
        tLong       :: H(0:15) = 0_kInt64
        !% flag indicating whether the Groestl-384 algorithm is employed or not.
        tLogical    :: IsGroestl384 = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                    Private Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => GroestlB_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (Groestl-512).
        PROCEDURE       :: Initialize   => GroestlB_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => GroestlB_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => GroestlB_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => GroestlB_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => GroestlB_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => GroestlB_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => GroestlB_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => GroestlB_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => GroestlB_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => GroestlB_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (Groestl-512) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the Groestl-384 algorithm <br>
        !   --->    CALL MD%Create(IsGroestl384=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
        ! ---------------------------------------------------------------------
    END TYPE GroestlB

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE GroestlB_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlB), INTENT(INOUT)  :: MD    !! 'GroestlB' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the Groestl-512 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE GroestlB_Initialize

!******************************************************************************

SUBROUTINE GroestlB_Initialize_wFlag(MD, IsGroestl384)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlB), INTENT(INOUT)  :: MD           !! 'GroestlB' object
    tLogical,        INTENT(IN)     :: IsGroestl384
    !^ flag indicating whether the Groestl-384 algorithm is employed or not. <br>
    !  - If true, use the Groestl-384 algorithm. <br>
    !  - Otherwise, use the Groestl-512 algorithm. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsGroestl384 = IsGroestl384
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE GroestlB_Initialize_wFlag

!******************************************************************************

SUBROUTINE GroestlB_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlB), INTENT(INOUT)  :: MD   !! 'GroestlB' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr  = 0_kInt8
    MD%H(0:14) = 0_kInt64
    MD%H(15)   = SHIFTL(ToInt64(MD%GetDigestLen()), 3)
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE GroestlB_Reset

!******************************************************************************

SUBROUTINE GroestlB_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlB),                INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(GroestlB :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (GroestlB)
        CALL Dst%Create(Src%IsGroestl384)
        Dst%H      = Src%H
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE GroestlB_GetClone

!******************************************************************************

FUNCTION GroestlB_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlB), INTENT(IN) :: MD       !! 'GroestlB' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsGroestl384) THEN
        Name = 'Groestl-384'
    ELSE
        Name = 'Groestl-512'
    END IF

    RETURN

END FUNCTION GroestlB_GetName

!******************************************************************************

FUNCTION GroestlB_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlB), INTENT(IN) :: MD       !! 'GroestlB' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsGroestl384) THEN
        Length = DLen384
    ELSE
        Length = DLen512
    END IF

    RETURN

END FUNCTION GroestlB_GetDigestLen

!******************************************************************************

FUNCTION GroestlB_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlB), INTENT(IN) :: MD       !! 'GroestlB' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION GroestlB_GetBlockLen

!******************************************************************************

SUBROUTINE GroestlB_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlB), TARGET, INTENT(INOUT)  :: MD           !! 'GroestlB' object
    tByte,          POINTER, INTENT(INOUT)  :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE GroestlB_SetBufPtr

!******************************************************************************

SUBROUTINE GroestlB_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlB), INTENT(INOUT)  :: MD           !! 'GroestlB' object
    tByte,           INTENT(IN)     :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: G(0:15)
    tLong       :: MS(0:15)
    tIndex      :: I

! FLOW

    ! input block
    CALL BytePackBE(BytesIn, 0_kIndex, MS)
    DO I = 0, 15
        G(I) = IEOR(MS(I), MD%H(I))
    END DO

    ! perform permutations
    CALL DoPermP(G)
    CALL DoPermQ(MS)

    ! get output states
    DO I = 0, 15
        MD%H(I) = IEOR(MD%H(I), IEOR(G(I), MS(I)))
    END DO

    RETURN

CONTAINS

    SUBROUTINE DoPermQ(X)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform permutation Q.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: X(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: T(0:15)
        tInteger    :: R, RP1

    ! FLOW

        DO R = 0, 13, 2
            X( 0) = QC64(X( 0), ToInt32(Z'00000001'), R)
            X( 1) = QC64(X( 1), ToInt32(Z'00000011'), R)
            X( 2) = QC64(X( 2), ToInt32(Z'00000021'), R)
            X( 3) = QC64(X( 3), ToInt32(Z'00000031'), R)
            X( 4) = QC64(X( 4), ToInt32(Z'00000041'), R)
            X( 5) = QC64(X( 5), ToInt32(Z'00000051'), R)
            X( 6) = QC64(X( 6), ToInt32(Z'00000061'), R)
            X( 7) = QC64(X( 7), ToInt32(Z'00000071'), R)
            X( 8) = QC64(X( 8), ToInt32(Z'00000081'), R)
            X( 9) = QC64(X( 9), ToInt32(Z'00000091'), R)
            X(10) = QC64(X(10), ToInt32(Z'000000A1'), R)
            X(11) = QC64(X(11), ToInt32(Z'000000B1'), R)
            X(12) = QC64(X(12), ToInt32(Z'000000C1'), R)
            X(13) = QC64(X(13), ToInt32(Z'000000D1'), R)
            X(14) = QC64(X(14), ToInt32(Z'000000E1'), R)
            X(15) = QC64(X(15), ToInt32(Z'000000F1'), R)
            T( 0) = RBTT(X,  1,  3,  5, 11,  0,  2,  4,  6)
            T( 1) = RBTT(X,  2,  4,  6, 12,  1,  3,  5,  7)
            T( 2) = RBTT(X,  3,  5,  7, 13,  2,  4,  6,  8)
            T( 3) = RBTT(X,  4,  6,  8, 14,  3,  5,  7,  9)
            T( 4) = RBTT(X,  5,  7,  9, 15,  4,  6,  8, 10)
            T( 5) = RBTT(X,  6,  8, 10,  0,  5,  7,  9, 11)
            T( 6) = RBTT(X,  7,  9, 11,  1,  6,  8, 10, 12)
            T( 7) = RBTT(X,  8, 10, 12,  2,  7,  9, 11, 13)
            T( 8) = RBTT(X,  9, 11, 13,  3,  8, 10, 12, 14)
            T( 9) = RBTT(X, 10, 12, 14,  4,  9, 11, 13, 15)
            T(10) = RBTT(X, 11, 13, 15,  5, 10, 12, 14,  0)
            T(11) = RBTT(X, 12, 14,  0,  6, 11, 13, 15,  1)
            T(12) = RBTT(X, 13, 15,  1,  7, 12, 14,  0,  2)
            T(13) = RBTT(X, 14,  0,  2,  8, 13, 15,  1,  3)
            T(14) = RBTT(X, 15,  1,  3,  9, 14,  0,  2,  4)
            T(15) = RBTT(X,  0,  2,  4, 10, 15,  1,  3,  5)
            RP1 = R + 1
            T( 0) = QC64(T( 0), ToInt32(Z'00000001'), RP1)
            T( 1) = QC64(T( 1), ToInt32(Z'00000011'), RP1)
            T( 2) = QC64(T( 2), ToInt32(Z'00000021'), RP1)
            T( 3) = QC64(T( 3), ToInt32(Z'00000031'), RP1)
            T( 4) = QC64(T( 4), ToInt32(Z'00000041'), RP1)
            T( 5) = QC64(T( 5), ToInt32(Z'00000051'), RP1)
            T( 6) = QC64(T( 6), ToInt32(Z'00000061'), RP1)
            T( 7) = QC64(T( 7), ToInt32(Z'00000071'), RP1)
            T( 8) = QC64(T( 8), ToInt32(Z'00000081'), RP1)
            T( 9) = QC64(T( 9), ToInt32(Z'00000091'), RP1)
            T(10) = QC64(T(10), ToInt32(Z'000000A1'), RP1)
            T(11) = QC64(T(11), ToInt32(Z'000000B1'), RP1)
            T(12) = QC64(T(12), ToInt32(Z'000000C1'), RP1)
            T(13) = QC64(T(13), ToInt32(Z'000000D1'), RP1)
            T(14) = QC64(T(14), ToInt32(Z'000000E1'), RP1)
            T(15) = QC64(T(15), ToInt32(Z'000000F1'), RP1)
            X( 0) = RBTT(T,  1,  3,  5, 11,  0,  2,  4,  6)
            X( 1) = RBTT(T,  2,  4,  6, 12,  1,  3,  5,  7)
            X( 2) = RBTT(T,  3,  5,  7, 13,  2,  4,  6,  8)
            X( 3) = RBTT(T,  4,  6,  8, 14,  3,  5,  7,  9)
            X( 4) = RBTT(T,  5,  7,  9, 15,  4,  6,  8, 10)
            X( 5) = RBTT(T,  6,  8, 10,  0,  5,  7,  9, 11)
            X( 6) = RBTT(T,  7,  9, 11,  1,  6,  8, 10, 12)
            X( 7) = RBTT(T,  8, 10, 12,  2,  7,  9, 11, 13)
            X( 8) = RBTT(T,  9, 11, 13,  3,  8, 10, 12, 14)
            X( 9) = RBTT(T, 10, 12, 14,  4,  9, 11, 13, 15)
            X(10) = RBTT(T, 11, 13, 15,  5, 10, 12, 14,  0)
            X(11) = RBTT(T, 12, 14,  0,  6, 11, 13, 15,  1)
            X(12) = RBTT(T, 13, 15,  1,  7, 12, 14,  0,  2)
            X(13) = RBTT(T, 14,  0,  2,  8, 13, 15,  1,  3)
            X(14) = RBTT(T, 15,  1,  3,  9, 14,  0,  2,  4)
            X(15) = RBTT(T,  0,  2,  4, 10, 15,  1,  3,  5)
        END DO

        RETURN

    END SUBROUTINE DoPermQ

    !**************************************************************************

END SUBROUTINE GroestlB_ProcessBlock

!******************************************************************************

SUBROUTINE DoPermP(X)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform permutation P.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT)    :: X(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: T(0:15)
    tInteger    :: R, RP1

! FLOW

    DO R = 0, 13, 2
        X( 0) = PC64(X( 0), ToInt32(Z'00000000'), R)
        X( 1) = PC64(X( 1), ToInt32(Z'00000010'), R)
        X( 2) = PC64(X( 2), ToInt32(Z'00000020'), R)
        X( 3) = PC64(X( 3), ToInt32(Z'00000030'), R)
        X( 4) = PC64(X( 4), ToInt32(Z'00000040'), R)
        X( 5) = PC64(X( 5), ToInt32(Z'00000050'), R)
        X( 6) = PC64(X( 6), ToInt32(Z'00000060'), R)
        X( 7) = PC64(X( 7), ToInt32(Z'00000070'), R)
        X( 8) = PC64(X( 8), ToInt32(Z'00000080'), R)
        X( 9) = PC64(X( 9), ToInt32(Z'00000090'), R)
        X(10) = PC64(X(10), ToInt32(Z'000000A0'), R)
        X(11) = PC64(X(11), ToInt32(Z'000000B0'), R)
        X(12) = PC64(X(12), ToInt32(Z'000000C0'), R)
        X(13) = PC64(X(13), ToInt32(Z'000000D0'), R)
        X(14) = PC64(X(14), ToInt32(Z'000000E0'), R)
        X(15) = PC64(X(15), ToInt32(Z'000000F0'), R)
        T( 0) = RBTT(X,  0,  1,  2,  3,  4,  5,  6, 11)
        T( 1) = RBTT(X,  1,  2,  3,  4,  5,  6,  7, 12)
        T( 2) = RBTT(X,  2,  3,  4,  5,  6,  7,  8, 13)
        T( 3) = RBTT(X,  3,  4,  5,  6,  7,  8,  9, 14)
        T( 4) = RBTT(X,  4,  5,  6,  7,  8,  9, 10, 15)
        T( 5) = RBTT(X,  5,  6,  7,  8,  9, 10, 11,  0)
        T( 6) = RBTT(X,  6,  7,  8,  9, 10, 11, 12,  1)
        T( 7) = RBTT(X,  7,  8,  9, 10, 11, 12, 13,  2)
        T( 8) = RBTT(X,  8,  9, 10, 11, 12, 13, 14,  3)
        T( 9) = RBTT(X,  9, 10, 11, 12, 13, 14, 15,  4)
        T(10) = RBTT(X, 10, 11, 12, 13, 14, 15,  0,  5)
        T(11) = RBTT(X, 11, 12, 13, 14, 15,  0,  1,  6)
        T(12) = RBTT(X, 12, 13, 14, 15,  0,  1,  2,  7)
        T(13) = RBTT(X, 13, 14, 15,  0,  1,  2,  3,  8)
        T(14) = RBTT(X, 14, 15,  0,  1,  2,  3,  4,  9)
        T(15) = RBTT(X, 15,  0,  1,  2,  3,  4,  5, 10)
        RP1 = R + 1
        T( 0) = PC64(T( 0), ToInt32(Z'00000000'), RP1)
        T( 1) = PC64(T( 1), ToInt32(Z'00000010'), RP1)
        T( 2) = PC64(T( 2), ToInt32(Z'00000020'), RP1)
        T( 3) = PC64(T( 3), ToInt32(Z'00000030'), RP1)
        T( 4) = PC64(T( 4), ToInt32(Z'00000040'), RP1)
        T( 5) = PC64(T( 5), ToInt32(Z'00000050'), RP1)
        T( 6) = PC64(T( 6), ToInt32(Z'00000060'), RP1)
        T( 7) = PC64(T( 7), ToInt32(Z'00000070'), RP1)
        T( 8) = PC64(T( 8), ToInt32(Z'00000080'), RP1)
        T( 9) = PC64(T( 9), ToInt32(Z'00000090'), RP1)
        T(10) = PC64(T(10), ToInt32(Z'000000A0'), RP1)
        T(11) = PC64(T(11), ToInt32(Z'000000B0'), RP1)
        T(12) = PC64(T(12), ToInt32(Z'000000C0'), RP1)
        T(13) = PC64(T(13), ToInt32(Z'000000D0'), RP1)
        T(14) = PC64(T(14), ToInt32(Z'000000E0'), RP1)
        T(15) = PC64(T(15), ToInt32(Z'000000F0'), RP1)
        X( 0) = RBTT(T,  0,  1,  2,  3,  4,  5,  6, 11)
        X( 1) = RBTT(T,  1,  2,  3,  4,  5,  6,  7, 12)
        X( 2) = RBTT(T,  2,  3,  4,  5,  6,  7,  8, 13)
        X( 3) = RBTT(T,  3,  4,  5,  6,  7,  8,  9, 14)
        X( 4) = RBTT(T,  4,  5,  6,  7,  8,  9, 10, 15)
        X( 5) = RBTT(T,  5,  6,  7,  8,  9, 10, 11,  0)
        X( 6) = RBTT(T,  6,  7,  8,  9, 10, 11, 12,  1)
        X( 7) = RBTT(T,  7,  8,  9, 10, 11, 12, 13,  2)
        X( 8) = RBTT(T,  8,  9, 10, 11, 12, 13, 14,  3)
        X( 9) = RBTT(T,  9, 10, 11, 12, 13, 14, 15,  4)
        X(10) = RBTT(T, 10, 11, 12, 13, 14, 15,  0,  5)
        X(11) = RBTT(T, 11, 12, 13, 14, 15,  0,  1,  6)
        X(12) = RBTT(T, 12, 13, 14, 15,  0,  1,  2,  7)
        X(13) = RBTT(T, 13, 14, 15,  0,  1,  2,  3,  8)
        X(14) = RBTT(T, 14, 15,  0,  1,  2,  3,  4,  9)
        X(15) = RBTT(T, 15,  0,  1,  2,  3,  4,  5, 10)
    END DO

    RETURN

END SUBROUTINE DoPermP

!******************************************************************************

SUBROUTINE GroestlB_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlB), INTENT(INOUT)  :: MD           !! 'GroestlB' object
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE GroestlB_DoPadding

!******************************************************************************

SUBROUTINE GroestlB_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(GroestlB), INTENT(INOUT)  :: MD           !! 'GroestlB' object
    tByte,           INTENT(IN)     :: LastByte     !! the last byte
    tByte,           INTENT(IN)     :: NBits        !! number of bits in the last byte
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: G(0:15)
    tIndex      :: I, Ptr, DLen
    tLong       :: Count
    tByte       :: Z

! FLOW

    ASSOCIATE(TmpBuf => MD%BufArr)
        Ptr = MD%GetBufLen()
        Z = SHIFTR(FByte80, NBits)
        TmpBuf(Ptr) = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
        Ptr = Ptr + 1_kIndex
        Count = MD%GetBlockCount()
        IF (Ptr <= 120_kIndex) THEN
            TmpBuf(Ptr:119) = FByte00
            Count = Count + 1_kInt64
        ELSE
            TmpBuf(Ptr:127) = FByte00
            CALL MD%ProcessBlock(TmpBuf)
            TmpBuf(0:119) = FByte00
            Count = Count + 2_kInt64
        END IF
        CALL ByteUnpackBE(Count, TmpBuf, 120_kIndex)
        CALL MD%ProcessBlock(TmpBuf)
        G = MD%H
        CALL DoPermP(G)
        DO I = 0_kIndex, 7_kIndex
            CALL ByteUnpackBE(IEOR(MD%H(I+8_kIndex), G(I+8_kIndex)), TmpBuf, I*8_kIndex)
        END DO
        DLen = MD%GetDigestLen()
        BytesOut(Offset:Offset+DLen-1) = TmpBuf(64-DLen:63)
    END ASSOCIATE
        
    RETURN

END SUBROUTINE GroestlB_AddBitsNPad

!******************************************************************************

END MODULE MClass_GroestlB
    
!******************************************************************************
