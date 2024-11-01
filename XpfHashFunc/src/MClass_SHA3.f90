
MODULE MClass_SHA3

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SHA3* type and its related routines.
!   The *SHA3* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *SHA3* type implements an incremental cryptographic hash
!   function by employing the *Keccak message-digest* algorithm [1, 2].
!   The implementation here is mainly based on the references [3, 4]. <br>
!   Unlike other digest types, which represent one or two hash functions,
!   the *SHA3* type represents eight cryptographic hash functions; four of
!   which are in the so-called SHA-3 family while the rest are in the
!   so-called Keccak family.  These two families of hash functions employ
!   the same Keccak message-digest algorithm; however, they use different
!   padding strategies. <br>
!   As the name suggested, the *SHA3* type represents the *SHA-3* family
!   by default.  However, a user can specify the *IsKeccak* flag to true
!   when initializing the digest object (by calling the *Create* method)
!   in order to use the padding strategy of the *Keccak* family.  Also,
!   the *SHA3* type employs the SHA3-256 hash function as a default
!   algorithm.  This implies that the hash output has the output size
!   and the strength of security (against pre-image attack) of 256 bits.
!   The user can also specify the *Security* argument (to one of the four
!   applicable values: 224, 256, 384 and 512) when initializing the digest
!   object in order to use a different algorithm and get a different hash
!   output size. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://csrc.nist.gov/csrc/media/Projects/hash-functions/documents/Keccak-reference-3.0.pdf">
!       The Keccak Reference, Version 3.0. </a> <br>
!   [2] <a href="https://doi.org/10.6028/NIST.FIPS.202">SHA-3 Standard:
!       Permutation-Based Hash and Extendable-Output Functions. </a> <br>
!   [3] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>
!   [4] <a href="https://csrc.nist.gov/CSRC/media/Projects/Hash-Functions/documents/Keccak_FinalRnd.zip">
!       The Keccak Reference Implementation in C. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil,           ONLY: ToDecStrSigned
    USE MBase_BytePack,           ONLY: BytePackLE, ByteUnpackLE
    USE MClass_BaseDigest
    USE MClass_MDEngine

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: SHA3

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tLong,  PARAMETER   :: RC(0:23) = [                               &
            ToInt64(Z'0000000000000001'), ToInt64(Z'0000000000008082'), &
            ToInt64(Z'800000000000808A'), ToInt64(Z'8000000080008000'), &
            ToInt64(Z'000000000000808B'), ToInt64(Z'0000000080000001'), &
            ToInt64(Z'8000000080008081'), ToInt64(Z'8000000000008009'), &
            ToInt64(Z'000000000000008A'), ToInt64(Z'0000000000000088'), &
            ToInt64(Z'0000000080008009'), ToInt64(Z'000000008000000A'), &
            ToInt64(Z'000000008000808B'), ToInt64(Z'800000000000008B'), &
            ToInt64(Z'8000000000008089'), ToInt64(Z'8000000000008003'), &
            ToInt64(Z'8000000000008002'), ToInt64(Z'8000000000000080'), &
            ToInt64(Z'000000000000800A'), ToInt64(Z'800000008000000A'), &
            ToInt64(Z'8000000080008081'), ToInt64(Z'8000000000008080'), &
            ToInt64(Z'0000000080000001'), ToInt64(Z'8000000080008008')]

!** DERIVED TYPE DEFINITIONS
    !> *SHA3* is a concrete *digest* type that implements an incremental
    !  cryptographic hash function based on the so-called Keccak hash
    !  functions, which is a family of message-digest algorithms.
    TYPE, EXTENDS(MDEngine) :: SHA3
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufInp(0:143) = 0_kInt8
        !% state
        tLong       :: State(0:24) = 0_kInt64
        !% flag indicating whether the Keccak family is employed or not.
        tLogical    :: IsKeccak = FalseVal
        !% security strength in bits
        tInteger    :: Security = 256
        !% length of hash output in bytes
        tIndex      :: DigestLen = 32_kIndex
        !% length of data block to be processed when exceeded
        tIndex      :: BlockLen = 136_kIndex
        !% length of temporary output buffer
        tIndex      :: OutLen = 32_kIndex
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWOption* method to
        !  initialize the *digest* object with specified options.
        PROCEDURE, PRIVATE  :: InitializeWOption    => SHA3_Initialize_wOption
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (SHA3-256).
        PROCEDURE       :: Initialize   => SHA3_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => SHA3_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => SHA3_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => SHA3_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => SHA3_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => SHA3_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => SHA3_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => SHA3_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => SHA3_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => SHA3_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (SHA3-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the Keccak-256 algorithm <br>
        !   --->    CALL MD%Create(IsKeccak=.TRUE.) <br>
        !   ! initialize the object to employ the SHA3-384 algorithm <br>
        !   --->    CALL MD%Create(IsKeccak=.FALSE., Security=384) <br>
        !   ! initialize the object to employ the Keccak-512 algorithm <br>
        !   --->    CALL MD%Create(IsKeccak=.TRUE., Security=512) <br>
        GENERIC         :: Create       => InitializeWOption
    END TYPE SHA3

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE SHA3_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA3), INTENT(INOUT)  :: MD    !! 'SHA3' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the SHA3-256 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE SHA3_Initialize

!******************************************************************************

SUBROUTINE SHA3_Initialize_wOption(MD, IsKeccak, Security)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA3),        INTENT(INOUT)   :: MD           !! 'SHA3' object
    tLogical,           INTENT(IN)      :: IsKeccak
    !^ flag indicating whether the Keccak family is employed or not. <br>
    !  - If true, use the Keccak family. <br>
    !  - Otherwise, use the SHA-3 algorithm. <br>
    tInteger, OPTIONAL, INTENT(IN)      :: Security
    !^ Strength of security in bits with four possible values: 224, 256, 384 and 512.
    !  If the specified value is NOT valid, it is set to the default (256) value.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (PRESENT(Security)) THEN
        SELECT CASE (Security)
        CASE (224, 256, 384, 512)
            MD%Security = Security
        CASE DEFAULT
            MD%Security = 256
        END SELECT
    ELSE
        MD%Security = 256
    END IF
    MD%IsKeccak  = IsKeccak
    MD%DigestLen = ToIndex(SHIFTR(MD%Security, 3))
    MD%BlockLen  = 200_kIndex - 2_kIndex*MD%DigestLen
    MD%OutLen    = IAND(MD%DigestLen + 7_kIndex, NOT(7_kIndex))
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE SHA3_Initialize_wOption

!******************************************************************************

SUBROUTINE SHA3_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA3), INTENT(INOUT)  :: MD   !! 'SHA3' object

!** SUBROUTINE PARAMETER DECLARATIONS:
    tLong, PARAMETER    :: MaxU64 = ToInt64(Z'FFFFFFFFFFFFFFFF')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufInp = 0_kInt8
    MD%State  = 0_kInt64
    MD%State( 1) = MaxU64
    MD%State( 2) = MaxU64
    MD%State( 8) = MaxU64
    MD%State(12) = MaxU64
    MD%State(17) = MaxU64
    MD%State(20) = MaxU64
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE SHA3_Reset

!******************************************************************************

SUBROUTINE SHA3_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA3),                    INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(SHA3 :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (SHA3)
        CALL Dst%Create(Src%IsKeccak, Src%Security)
        Dst%State  = Src%State
        Dst%BufInp = Src%BufInp
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE SHA3_GetClone

!******************************************************************************

FUNCTION SHA3_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA3), INTENT(IN) :: MD       !! 'SHA3' object
    tCharAlloc              :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsKeccak) THEN
        Name = 'Keccak-' // ToDecStrSigned(MD%Security)
    ELSE
        Name = 'SHA3-' // ToDecStrSigned(MD%Security)
    END IF

    RETURN

END FUNCTION SHA3_GetName

!******************************************************************************

FUNCTION SHA3_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA3), INTENT(IN) :: MD       !! 'SHA3' object
    tIndex                  :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Length = MD%DigestLen

    RETURN

END FUNCTION SHA3_GetDigestLen

!******************************************************************************

FUNCTION SHA3_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA3), INTENT(IN) :: MD       !! 'SHA3' object
    tIndex                  :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = MD%BlockLen

    RETURN

END FUNCTION SHA3_GetBlockLen

!******************************************************************************

SUBROUTINE SHA3_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA3), TARGET, INTENT(INOUT)  :: MD           !! 'SHA3' object
    tByte,      POINTER, INTENT(INOUT)  :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufInp(0:MD%BlockLen-1)

    RETURN

END SUBROUTINE SHA3_SetBufPtr

!******************************************************************************

SUBROUTINE SHA3_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA3), INTENT(INOUT)  :: MD           !! 'SHA3' object
    tByte,       INTENT(IN)     :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: T
    tLong       :: Z
    tIndex      :: I, J

! FLOW

    DO I = 0, SIZE(BytesIn)-1, 8
        J = SHIFTR(I, 3)
        CALL BytePackLE(BytesIn, I, Z)
        MD%State(J) = IEOR(MD%State(J), Z)
    END DO
    ! Unrolling two rounds.
    DO J = 0, 23, 2
        ! Theta
        CALL Theta_Step_I(MD%State)
        ! Rho/Pi
        CALL RhoPi_Step_I(MD%State)
        ! Chi
        CALL Chi_Step_I(MD%State)
        ! Iota
        MD%State( 0) = IEOR(MD%State( 0), RC(J))
        ! Theta
        CALL Theta_Step_II(MD%State)
        ! Rho/Pi
        CALL RhoPi_Step_II(MD%State)
        ! Chi
        CALL Chi_Step_II(MD%State)
        ! Iota
        MD%State( 0) = IEOR(MD%State( 0), RC(J + 1))
        ! re-arrange state
        T = MD%State( 5)
        MD%State( 5) = MD%State(18)
        MD%State(18) = MD%State(11)
        MD%State(11) = MD%State(10)
        MD%State(10) = MD%State( 6)
        MD%State( 6) = MD%State(22)
        MD%State(22) = MD%State(20)
        MD%State(20) = MD%State(12)
        MD%State(12) = MD%State(19)
        MD%State(19) = MD%State(15)
        MD%State(15) = MD%State(24)
        MD%State(24) = MD%State( 8)
        MD%State( 8) = T
        !...
        T = MD%State( 1)
        MD%State( 1) = MD%State( 9)
        MD%State( 9) = MD%State(14)
        MD%State(14) = MD%State( 2)
        MD%State( 2) = MD%State(13)
        MD%State(13) = MD%State(23)
        MD%State(23) = MD%State( 4)
        MD%State( 4) = MD%State(21)
        MD%State(21) = MD%State(16)
        MD%State(16) = MD%State( 3)
        MD%State( 3) = MD%State(17)
        MD%State(17) = MD%State( 7)
        MD%State( 7) = T
    END DO

    RETURN

CONTAINS

    SUBROUTINE Theta_Step_I(A)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform Theta step-mapping.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: A(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: T(0:4)
        tLong       :: TT0, TT1, TT2, TT3, TT4
        tIndex      :: I, J, K

    ! FLOW
        
        J = 1
        K = 4
        DO I = 0, 4
            TT0 = IEOR(A(J), A(J+5))
            TT1 = IEOR(A(J+10), A(J+15))
            TT0 = IEOR(TT0, IEOR(A(J+20), TT1))
            TT0 = RotateLeft(TT0, 1)
            TT2 = IEOR(A(K), A(K+5))
            TT3 = IEOR(A(K+10), A(K+15))
            TT0 = IEOR(TT0, A(K+20))
            TT2 = IEOR(TT2, TT3)
            T(I) = IEOR(TT0, TT2)
            J = J + 1
            K = K + 1
            IF (J == 5) J = 0
            IF (K == 5) K = 0
        END DO
        DO I = 0, 20, 5
            A(I) = IEOR(A(I), T(0))
        END DO
        DO I = 1, 21, 5
            A(I) = IEOR(A(I), T(1))
        END DO
        DO I = 2, 22, 5
            A(I) = IEOR(A(I), T(2))
        END DO
        DO I = 3, 23, 5
            A(I) = IEOR(A(I), T(3))
        END DO
        DO I = 4, 24, 5
            A(I) = IEOR(A(I), T(4))
        END DO

        RETURN

    END SUBROUTINE Theta_Step_I

    !**************************************************************************

    SUBROUTINE Theta_Step_II(A)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform Theta step-mapping.
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: A(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: T0, T1, T2, T3, T4
        tLong       :: TT0, TT1, TT2, TT3, TT4
        tIndex      :: I

    ! FLOW

        TT0 = IEOR(A( 6), A( 9))
        TT1 = IEOR(A( 7), A( 5))
        TT0 = IEOR(TT0, IEOR(A( 8), TT1))
        TT0 = RotateLeft(TT0, 1)
        TT2 = IEOR(A(24), A(22))
        TT3 = IEOR(A(20), A(23))
        TT0 = IEOR(TT0, A(21))
        TT2 = IEOR(TT2, TT3)
        T0  = IEOR(TT0, TT2)

        TT0 = IEOR(A(12), A(10))
        TT1 = IEOR(A(13), A(11))
        TT0 = IEOR(TT0, IEOR(A(14), TT1))
        TT0 = RotateLeft(TT0, 1)
        TT2 = IEOR(A( 0), A( 3))
        TT3 = IEOR(A( 1), A( 4))
        TT0 = IEOR(TT0, A( 2))
        TT2 = IEOR(TT2, TT3)
        T1  = IEOR(TT0, TT2)

        TT0 = IEOR(A(18), A(16))
        TT1 = IEOR(A(19), A(17))
        TT0 = IEOR(TT0, IEOR(A(15), TT1))
        TT0 = RotateLeft(TT0, 1)
        TT2 = IEOR(A( 6), A( 9))
        TT3 = IEOR(A( 7), A( 5))
        TT0 = IEOR(TT0, A( 8))
        TT2 = IEOR(TT2, TT3)
        T2  = IEOR(TT0, TT2)

        TT0 = IEOR(A(24), A(22))
        TT1 = IEOR(A(20), A(23))
        TT0 = IEOR(TT0, IEOR(A(21), TT1))
        TT0 = RotateLeft(TT0, 1)
        TT2 = IEOR(A(12), A(10))
        TT3 = IEOR(A(13), A(11))
        TT0 = IEOR(TT0, A(14))
        TT2 = IEOR(TT2, TT3)
        T3  = IEOR(TT0, TT2)

        TT0 = IEOR(A( 0), A( 3))
        TT1 = IEOR(A( 1), A( 4))
        TT0 = IEOR(TT0, IEOR(A( 2), TT1))
        TT0 = RotateLeft(TT0, 1)
        TT2 = IEOR(A(18), A(16))
        TT3 = IEOR(A(19), A(17))
        TT0 = IEOR(TT0, A(15))
        TT2 = IEOR(TT2, TT3)
        T4  = IEOR(TT0, TT2)
            
        DO I = 0, 4
            A(I) = IEOR(A(I), T0)
        END DO
        DO I = 5, 9
            A(I) = IEOR(A(I), T1)
        END DO
        DO I = 10, 14
            A(I) = IEOR(A(I), T2)
        END DO
        DO I = 15, 19
            A(I) = IEOR(A(I), T3)
        END DO
        DO I = 20, 24
            A(I) = IEOR(A(I), T4)
        END DO

        RETURN

    END SUBROUTINE Theta_Step_II

    !**************************************************************************

    SUBROUTINE RhoPi_Step_I(A)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform Rho and Pi step-mappings. (The two steps are merged for optimal
        ! performance).
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: A(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW
    
        A( 5) = RotateLeft(A( 5), 36)
        A(10) = RotateLeft(A(10), 3)
        A(15) = RotateLeft(A(15), 41)
        A(20) = RotateLeft(A(20), 18)
        A( 1) = RotateLeft(A( 1), 1)
        A( 6) = RotateLeft(A( 6), 44)
        A(11) = RotateLeft(A(11), 10)
        A(16) = RotateLeft(A(16), 45)
        A(21) = RotateLeft(A(21), 2)
        A( 2) = RotateLeft(A( 2), 62)
        A( 7) = RotateLeft(A( 7), 6)
        A(12) = RotateLeft(A(12), 43)
        A(17) = RotateLeft(A(17), 15)
        A(22) = RotateLeft(A(22), 61)
        A( 3) = RotateLeft(A( 3), 28)
        A( 8) = RotateLeft(A( 8), 55)
        A(13) = RotateLeft(A(13), 25)
        A(18) = RotateLeft(A(18), 21)
        A(23) = RotateLeft(A(23), 56)
        A( 4) = RotateLeft(A( 4), 27)
        A( 9) = RotateLeft(A( 9), 20)
        A(14) = RotateLeft(A(14), 39)
        A(19) = RotateLeft(A(19), 8)
        A(24) = RotateLeft(A(24), 14)
        
        RETURN

    END SUBROUTINE RhoPi_Step_I

    !**************************************************************************

    SUBROUTINE RhoPi_Step_II(A)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform Rho and Pi step-mappings. (The two steps are merged for optimal
        ! performance).
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: A(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW
    
        A( 3) = RotateLeft(A( 3), 36)
        A( 1) = RotateLeft(A( 1), 3)
        A( 4) = RotateLeft(A( 4), 41)
        A( 2) = RotateLeft(A( 2), 18)
        A( 6) = RotateLeft(A( 6), 1)
        A( 9) = RotateLeft(A( 9), 44)
        A( 7) = RotateLeft(A( 7), 10)
        A( 5) = RotateLeft(A( 5), 45)
        A( 8) = RotateLeft(A( 8), 2)
        A(12) = RotateLeft(A(12), 62)
        A(10) = RotateLeft(A(10), 6)
        A(13) = RotateLeft(A(13), 43)
        A(11) = RotateLeft(A(11), 15)
        A(14) = RotateLeft(A(14), 61)
        A(18) = RotateLeft(A(18), 28)
        A(16) = RotateLeft(A(16), 55)
        A(19) = RotateLeft(A(19), 25)
        A(17) = RotateLeft(A(17), 21)
        A(15) = RotateLeft(A(15), 56)
        A(24) = RotateLeft(A(24), 27)
        A(22) = RotateLeft(A(22), 20)
        A(20) = RotateLeft(A(20), 39)
        A(23) = RotateLeft(A(23), 8)
        A(21) = RotateLeft(A(21), 14)

        RETURN

    END SUBROUTINE RhoPi_Step_II

    !**************************************************************************

    SUBROUTINE Chi_Step_I(A)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform Chi step-mapping.
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: A(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: C0, C1, C2, C3, C4, BNN, KT

    ! FLOW
    
        BNN = NOT(A(12))
        KT = IOR(A( 6), A(12))
        C0 = IEOR(A( 0), KT)
        KT = IOR(BNN, A(18))
        C1 = IEOR(A( 6), KT)
        KT = IAND(A(18), A(24))
        C2 = IEOR(A(12), KT)
        KT = IOR(A(24), A( 0))
        C3 = IEOR(A(18), KT)
        KT = IAND(A( 0), A( 6))
        C4 = IEOR(A(24), KT)
        A( 0) = C0
        A( 6) = C1
        A(12) = C2
        A(18) = C3
        A(24) = C4
            
        BNN = NOT(A(22))
        KT = IOR(A( 9), A(10))
        C0 = IEOR(A( 3), KT)
        KT = IAND(A(10), A(16))
        C1 = IEOR(A( 9), KT)
        KT = IOR(A(16), BNN)
        C2 = IEOR(A(10), KT)
        KT = IOR(A(22), A( 3))
        C3 = IEOR(A(16), KT)
        KT = IAND(A( 3), A( 9))
        C4 = IEOR(A(22), KT)
        A( 3) = C0
        A( 9) = C1
        A(10) = C2
        A(16) = C3
        A(22) = C4
            
        BNN = NOT(A(19))
        KT = IOR(A( 7), A(13))
        C0 = IEOR(A( 1), KT)
        KT = IAND(A(13), A(19))
        C1 = IEOR(A( 7), KT)
        KT = IAND(BNN, A(20))
        C2 = IEOR(A(13), KT)
        KT = IOR(A(20), A( 1))
        C3 = IEOR(BNN, KT)
        KT = IAND(A( 1), A( 7))
        C4 = IEOR(A(20), KT)
        A( 1) = C0
        A( 7) = C1
        A(13) = C2
        A(19) = C3
        A(20) = C4
            
        BNN = NOT(A(17))
        KT = IAND(A( 5), A(11))
        C0 = IEOR(A( 4), KT)
        KT = IOR(A(11), A(17))
        C1 = IEOR(A( 5), KT)
        KT = IOR(BNN, A(23))
        C2 = IEOR(A(11), KT)
        KT = IAND(A(23), A( 4))
        C3 = IEOR(BNN, KT)
        KT = IOR(A( 4), A( 5))
        C4 = IEOR(A(23), KT)
        A( 4) = C0
        A( 5) = C1
        A(11) = C2
        A(17) = C3
        A(23) = C4
            
        BNN = NOT(A( 8))
        KT = IAND(BNN, A(14))
        C0 = IEOR(A( 2), KT)
        KT = IOR(A(14), A(15))
        C1 = IEOR(BNN, KT)
        KT = IAND(A(15), A(21))
        C2 = IEOR(A(14), KT)
        KT = IOR(A(21), A( 2))
        C3 = IEOR(A(15), KT)
        KT = IAND(A( 2), A( 8))
        C4 = IEOR(A(21), KT)
        A( 2) = C0
        A( 8) = C1
        A(14) = C2
        A(15) = C3
        A(21) = C4
        
        RETURN

    END SUBROUTINE Chi_Step_I

    !**************************************************************************

    SUBROUTINE Chi_Step_II(A)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform Chi step-mapping.
        
    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong, INTENT(INOUT)    :: A(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: C0, C1, C2, C3, C4, BNN, KT

    ! FLOW
    
        BNN = NOT(A(13))
        KT = IOR(A( 9), A(13))
        C0 = IEOR(A( 0), KT)
        KT = IOR(BNN, A(17))
        C1 = IEOR(A( 9), KT)
        KT = IAND(A(17), A(21))
        C2 = IEOR(A(13), KT)
        KT = IOR(A(21), A( 0))
        C3 = IEOR(A(17), KT)
        KT = IAND(A( 0), A( 9))
        C4 = IEOR(A(21), KT)
        A( 0) = C0
        A( 9) = C1
        A(13) = C2
        A(17) = C3
        A(21) = C4
            
        BNN = NOT(A(14))
        KT = IOR(A(22), A( 1))
        C0 = IEOR(A(18), KT)
        KT = IAND(A( 1), A( 5))
        C1 = IEOR(A(22), KT)
        KT = IOR(A( 5), BNN)
        C2 = IEOR(A( 1), KT)
        KT = IOR(A(14), A(18))
        C3 = IEOR(A( 5), KT)
        KT = IAND(A(18), A(22))
        C4 = IEOR(A(14), KT)
        A(18) = C0
        A(22) = C1
        A( 1) = C2
        A( 5) = C3
        A(14) = C4
            
        BNN = NOT(A(23))
        KT = IOR(A(10), A(19))
        C0 = IEOR(A( 6), KT)
        KT = IAND(A(19), A(23))
        C1 = IEOR(A(10), KT)
        KT = IAND(BNN, A( 2))
        C2 = IEOR(A(19), KT)
        KT = IOR(A( 2), A( 6))
        C3 = IEOR(BNN, KT)
        KT = IAND(A( 6), A(10))
        C4 = IEOR(A( 2), KT)
        A( 6) = C0
        A(10) = C1
        A(19) = C2
        A(23) = C3
        A( 2) = C4
            
        BNN = NOT(A(11))
        KT = IAND(A( 3), A( 7))
        C0 = IEOR(A(24), KT)
        KT = IOR(A( 7), A(11))
        C1 = IEOR(A( 3), KT)
        KT = IOR(BNN, A(15))
        C2 = IEOR(A( 7), KT)
        KT = IAND(A(15), A(24))
        C3 = IEOR(BNN, KT)
        KT = IOR(A(24), A( 3))
        C4 = IEOR(A(15), KT)
        A(24) = C0
        A( 3) = C1
        A( 7) = C2
        A(11) = C3
        A(15) = C4
            
        BNN = NOT(A(16))
        KT = IAND(BNN, A(20))
        C0 = IEOR(A(12), KT)
        KT = IOR(A(20), A( 4))
        C1 = IEOR(BNN, KT)
        KT = IAND(A( 4), A( 8))
        C2 = IEOR(A(20), KT)
        KT = IOR(A( 8), A(12))
        C3 = IEOR(A( 4), KT)
        KT = IAND(A(12), A(16))
        C4 = IEOR(A( 8), KT)
        A(12) = C0
        A(16) = C1
        A(20) = C2
        A( 4) = C3
        A( 8) = C4

        RETURN

    END SUBROUTINE Chi_Step_II

    !**************************************************************************

END SUBROUTINE SHA3_ProcessBlock

!******************************************************************************

SUBROUTINE SHA3_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA3), INTENT(INOUT)  :: MD           !! 'SHA3' object
    tByte,       INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,      INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE SHA3_DoPadding

!******************************************************************************

SUBROUTINE SHA3_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHA3), INTENT(INOUT)  :: MD           !! 'SHA3' object
    tByte,       INTENT(IN)     :: LastByte     !! the last byte
    tByte,       INTENT(IN)     :: NBits        !! number of bits in the last byte
    tByte,       INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,      INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte, PARAMETER    :: Keccak_Suffix = ToInt8(Z'01')
    tByte, PARAMETER    :: SHA3_Suffix   = ToInt8(Z'06')

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: Buf(0:MD%BlockLen+1)
    tIndex      :: I, Ptr, BlockLen, BufLen
    tInteger    :: BitsInQueue

! FLOW

    Ptr = MD%GetBufLen()
    BlockLen = MD%GetBlockLen()
    BitsInQueue = Ptr*8 + NBits
    IF (MD%IsKeccak) THEN
        ! perform Keccak padding
        BLOCK
            tByte       :: FinalInput
            ! execution
            FinalInput = IEOR(SHIFTR(LastByte, (8 - NBits)), &
                              SHIFTL(Keccak_Suffix, IAND(BitsInQueue, 7)))
            IF (Ptr == (BlockLen-1)) THEN
                IF (NBits == 7) THEN
                    Buf(0) = FinalInput
                    Buf(1:BlockLen-1) = FByte00
                    Buf(BlockLen) = FByte80
                    BufLen = BlockLen + 1
                ELSE
                    Buf(0) = IOR(FinalInput, FByte80)
                    BufLen = 1
                END IF
            ELSE
                Buf(0) = FinalInput
                BufLen = BlockLen - Ptr
                Buf(1:BufLen-2) = FByte00
                Buf(BufLen-1) = FByte80
            END IF
        END BLOCK
    ELSE
        ! perform SHA-3 padding
        BLOCK
            tInteger    :: FinalInput
            tByte       :: FinalNBits
            ! execution
            FinalInput = IEOR(SHIFTR(LastByte, (8 - NBits)), &
                              SHIFTL(SHA3_Suffix, IAND(BitsInQueue, 7)))
            FinalNBits = NBits + 2
            IF (Ptr == (BlockLen-1)) THEN
                IF (FinalNBits < 7) THEN
                    Buf(0) = IOR(ToInt8(FinalInput), FByte80)
                    BufLen = 1
                ELSEIF (FinalNBits == 7) THEN
                    Buf(0) = ToInt8(FinalInput)
                    Buf(1:BlockLen-1) = FByte00
                    Buf(BlockLen) = FByte80
                    BufLen = BlockLen + 1
                ELSE
                    Buf(0) = ToInt8(FinalInput)
                    Buf(1) = ToInt8(SHIFTR(FinalInput, 8))
                    Buf(2:BlockLen-1) = FByte00
                    Buf(BlockLen) = FByte80
                    BufLen = BlockLen + 1
                END IF
            ELSE
                IF (FinalNBits < 8) THEN
                    Buf(0) = ToInt8(FinalInput)
                    BufLen = BlockLen - Ptr
                    Buf(1:BufLen-2) = FByte00
                    Buf(BufLen-1) = FByte80
                ELSE
                    IF (Ptr == (BlockLen-2)) THEN
                        Buf(0) = ToInt8(FinalInput)
                        Buf(1) = IOR(ToInt8(SHIFTR(FinalInput, 8)), FByte80)
                        BufLen = BlockLen - Ptr
                    ELSE
                        Buf(0) = ToInt8(FinalInput)
                        Buf(1) = ToInt8(SHIFTR(FinalInput, 8))
                        BufLen = BlockLen - Ptr
                        Buf(2:BufLen-2) = FByte00
                        Buf(BufLen-1) = FByte80
                    END IF
                END IF
            END IF
        END BLOCK
    END IF
    CALL MD%Update(Buf, 0_kIndex, BufLen)
    MD%State( 1) = NOT(MD%State( 1))
    MD%State( 2) = NOT(MD%State( 2))
    MD%State( 8) = NOT(MD%State( 8))
    MD%State(12) = NOT(MD%State(12))
    MD%State(17) = NOT(MD%State(17))
    MD%State(20) = NOT(MD%State(20))
    BLOCK
        !% buffer array used to store output data
        tByte       :: BufOut(0:MD%OutLen-1)
        DO I = 0, MD%DigestLen-1, 8
            CALL ByteUnpackLE(MD%State(SHIFTR(I, 3)), BufOut, I)
        END DO
        BytesOut(Offset:Offset+MD%DigestLen-1) = BufOut(0:MD%DigestLen-1)
    END BLOCK
        
    RETURN

END SUBROUTINE SHA3_AddBitsNPad

!******************************************************************************

END MODULE MClass_SHA3
    
!******************************************************************************
