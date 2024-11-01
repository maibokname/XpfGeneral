
MODULE MClass_Blake2B

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Blake2B* type and its related routines.
!   The *Blake2B* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_blake2core.html#type-blake2core">Blake2Core</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *Blake2B* type implements an incremental cryptographic hash
!   function by employing the *BLAKE2b* message-digest algorithm, which
!   is optimized for 64-bit platforms and can produce the hash output of
!   any size between 1 and 64 bytes [1, 2].  The implementation here is
!   based mainly on the references [3, 4].  Unlike most of other *digest*
!   types, the *Blake2B* type can perform keyed hashing providing that a
!   user specifies the key during an initialization of the digest object
!   by calling the *CreateHMAC* method instead of the *Create* method. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.aumasson.jp/blake/book/">J.P. Aumasson, W. Meier,
!       R.C.W. Phan, and L. Henzen. 2015. The Hash Function BLAKE. Springer. </a> <br>
!   [2] <a href="https://www.blake2.net/">BLAKE2 - Fast Secure Hashing. </a> <br>
!   [3] <a href="https://github.com/kocakosm/jblake2">JBlake2: A pure Java
!       implementation of BLAKE2 (RFC 7693). </a> <br>
!   [4] <a href="https://github.com/BLAKE2/BLAKE2">BLAKE2 official implementations. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_BytePack,   ONLY: BytePackLE, ByteUnpackLE
    USE MClass_BaseDigest
    USE MClass_Blake2Core

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: Blake2B

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tIndex, PARAMETER   :: BlockLen  = 128_kIndex
    tIndex, PARAMETER   :: DigestLen = 64_kIndex
    tLong,  PARAMETER   :: IV(0:7) = [                                &
            ToInt64(Z'6A09E667F3BCC908'), ToInt64(Z'BB67AE8584CAA73B'), &
            ToInt64(Z'3C6EF372FE94F82B'), ToInt64(Z'A54FF53A5F1D36F1'), &
            ToInt64(Z'510E527FADE682D1'), ToInt64(Z'9B05688C2B3E6C1F'), &
            ToInt64(Z'1F83D9ABFB41BD6B'), ToInt64(Z'5BE0CD19137E2179')]
    tIndex, PARAMETER   :: SIGMA(0:15,0:11) = RESHAPE([                     &
             0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, &
            14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3, &
            11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4, &
             7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8, &
             9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13, &
             2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9, &
            12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11, &
            13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10, &
             6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5, &
            10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13,  0, &
             0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, &
            14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3], [16, 12])

!** DERIVED TYPE DEFINITIONS
    !> *Blake2B* is a concrete *digest* type that implements an incremental cryptographic
    !  hash function by employing the *BLAKE2b message-digest* algorithm.  It can also be
    !  utilized as a hash-based message authentication code (HMAC).
    TYPE, EXTENDS(Blake2Core) :: Blake2B
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% internal state
        tLong       :: H(0:7) = IV(0:7)
        !% counter's LSB
        tLong       :: T0     = 0_kInt64
        !% counter's MSB
        tLong       :: T1     = 0_kInt64
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                    Private Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitWOutLen* method to
        !  initialize the *digest* object with the specified hash output length.
        PROCEDURE, PRIVATE  :: InitWOutLen  => Blake2B_InitWOutLen
        !> Use the *Create* method in place of the *InitWKey* method to initialize
        !  the *digest* object with the specified key for keyed hashing (and the
        !  optionally specified hash output length).
        PROCEDURE, PRIVATE  :: InitWKey     => Blake2B_InitWKey
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default hash output length.
        PROCEDURE       :: Initialize       => Blake2B_InitDefault
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset            => Blake2B_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone         => Blake2B_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName          => Blake2B_GetName
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr        => Blake2B_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock     => Blake2B_ProcessBlock
        !> *EncodeOutput* is a procedure to encode the hash output. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: EncodeOutput     => Blake2B_EncodeOutput
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object
        !                (as a hasher). <br>
        !  **Usage**: <br>
        !   ! initialization with default output length <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialization with specified output length <br>
        GENERIC     :: Create       => InitWOutLen
        !> **Type-Bound Subroutine**: CreateHMAC <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object
        !                as a *HMAC* object (for keyed hashing). <br>
        !  **Usage**: <br>
        !   ! initialization with default output length <br>
        !   --->    CALL MD%CreateHMAC(Key) <br>
        !   ! initialization with specified output length <br>
        !   --->    CALL MD%CreateHMAC(Key, OutputLen) <br>
        GENERIC     :: CreateHMAC   => InitWKey
        ! ---------------------------------------------------------------------
    END TYPE Blake2B

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Blake2B_InitDefault(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform default initialization of the digest object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2B), INTENT(INOUT)   :: MD    !! 'Blake2B' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL MD%CoreInit(BlockLen, DigestLen)
   
    RETURN

END SUBROUTINE Blake2B_InitDefault

!******************************************************************************

SUBROUTINE Blake2B_InitWOutLen(MD, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified output length.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2B),  INTENT(INOUT)  :: MD           !! 'Blake2B' object
    tIndex,          INTENT(IN)     :: OutputLen
    !^ the desired length of hash output, which must be between 1 and 32 bytes. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL MD%CoreInit(BlockLen, OutputLen)
   
    RETURN

END SUBROUTINE Blake2B_InitWOutLen

!******************************************************************************

SUBROUTINE Blake2B_InitWKey(MD, Key, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified key and
    !  the optionally specified length of hash output.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2B),   INTENT(INOUT) :: MD           !! 'Blake2B' object
    tByte,            INTENT(IN)    :: Key(0:)
    !^ an (8-bit integer) byte array representing a key for a keyed hashing <br>
    tIndex, OPTIONAL, INTENT(IN)    :: OutputLen
    !^ desired length of hash output; must be between 1 and 32 bytes. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (PRESENT(OutputLen)) THEN
        CALL MD%CoreInit(BlockLen, OutputLen, Key)
    ELSE
        CALL MD%CoreInit(BlockLen, DigestLen, Key)
    END IF
   
    RETURN

END SUBROUTINE Blake2B_InitWKey

!******************************************************************************

SUBROUTINE Blake2B_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2B), INTENT(INOUT) :: MD   !! 'Blake2B' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    MD%T0 = 0_kInt64
    MD%T1 = 0_kInt64
    MD%H(0:7) = IV(0:7)
    MD%H(0) = IEOR(MD%H(0), ToInt64(IOR(IOR(MD%GetDigestLen(), SHIFTL(MD%GetKeyLen(), 8)), ToIndex(Z'01010000'))))
    CALL MD%CoreReset()

    RETURN

END SUBROUTINE Blake2B_Reset

!******************************************************************************

SUBROUTINE Blake2B_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2B),                 INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(Blake2B :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (Blake2B)
        Dst%H      = Src%H
        Dst%T0     = Src%T0
        Dst%T1     = Src%T1
        Dst%BufArr = Src%BufArr
        CALL Src%CoreClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE Blake2B_GetClone

!******************************************************************************

FUNCTION Blake2B_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2B), INTENT(IN)  :: MD       !! 'Blake2B' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%GetKeyLen() > 0_kIndex) THEN
        Name = 'BLAKE2b HMAC'
    ELSE
        Name = 'BLAKE2b Hasher'
    END IF
    RETURN

END FUNCTION Blake2B_GetName

!******************************************************************************

SUBROUTINE Blake2B_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2B), TARGET, INTENT(INOUT)   :: MD           !! 'Blake2B' object
    tByte,         POINTER, INTENT(INOUT)   :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE Blake2B_SetBufPtr

!******************************************************************************

SUBROUTINE Blake2B_ProcessBlock(MD, BytesIn, LastBlock)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2B), INTENT(INOUT)   :: MD           !! 'Blake2B' object
    tByte,          INTENT(IN)      :: BytesIn(0:)  !! the input data block
    tLogical,       INTENT(IN)      :: LastBlock    !! true if the input is the last block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! update counters
    MD%T0 = MD%T0 + MD%GetBufLen()
    IF ((MD%T0 == 0_kInt64).AND.(MD%GetBufLen() > 0_kIndex)) MD%T1 = MD%T1 + 1_kInt64

    CALL F(BytesIn, LastBlock)

    RETURN

CONTAINS

    SUBROUTINE F(Input, LastBlock)

        ! To perform data compression process.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: Input(0:)    ! the data block
        tLogical, INTENT(IN)    :: LastBlock    ! true if the buffer is the last block

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: M(0:15)
        tLong       :: V(0:15)
        tIndex      :: I

    ! FLOW

        ! initialize
        V(0:7)  = MD%H(0:7)
        V(8:15) = IV(0:7)
        V(12) = IEOR(V(12), MD%T0)
        V(13) = IEOR(V(13), MD%T1)
        IF (LastBlock) V(14) = NOT(V(14))

        ! input block
        CALL BytePackLE(BytesIn, 0_kIndex, M)

        ! perform block transformation
        DO I = 0, 11
            CALL G(V, 0, 4,  8, 12, M(SIGMA( 0, I)), M(SIGMA( 1, I)))
            CALL G(V, 1, 5,  9, 13, M(SIGMA( 2, I)), M(SIGMA( 3, I)))
            CALL G(V, 2, 6, 10, 14, M(SIGMA( 4, I)), M(SIGMA( 5, I)))
            CALL G(V, 3, 7, 11, 15, M(SIGMA( 6, I)), M(SIGMA( 7, I)))
            CALL G(V, 0, 5, 10, 15, M(SIGMA( 8, I)), M(SIGMA( 9, I)))
            CALL G(V, 1, 6, 11, 12, M(SIGMA(10, I)), M(SIGMA(11, I)))
            CALL G(V, 2, 7,  8, 13, M(SIGMA(12, I)), M(SIGMA(13, I)))
            CALL G(V, 3, 4,  9, 14, M(SIGMA(14, I)), M(SIGMA(15, I)))
        END DO

        ! update H
        DO I = 0, 7
            MD%H(I) = IEOR(MD%H(I), IEOR(V(I), V(I+8)))
        END DO

        RETURN

    END SUBROUTINE F

    !**************************************************************************

    SUBROUTINE G(V, A, B, C, D, X, Y)
    
        ! To perform block transformation

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tLong,    INTENT(INOUT) :: V(0:15)
        tInteger, INTENT(IN)    :: A, B, C, D
        tLong,    INTENT(IN)    :: X, Y

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        V(A) = V(A) + (V(B) + X)
        V(D) = RotateRight(IEOR(V(D), V(A)), 32)
        V(C) = V(C) + V(D)
        V(B) = RotateRight(IEOR(V(B), V(C)), 24)
        V(A) = V(A) + (V(B) + Y)
        V(D) = RotateRight(IEOR(V(D), V(A)), 16)
        V(C) = V(C) + V(D)
        V(B) = RotateRight(IEOR(V(B), V(C)), 63)

        RETURN

    END SUBROUTINE G

    !**************************************************************************

END SUBROUTINE Blake2B_ProcessBlock

!******************************************************************************

SUBROUTINE Blake2B_EncodeOutput(MD, BytesOut)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the encoding of the hash output (i.e.
    !  unpacking the internal state into the output byte array).
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake2B), INTENT(INOUT)   :: MD           !! 'Blake2B' object
    tByte,          INTENT(INOUT)   :: BytesOut(0:) !! the output array

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, J, OutLen
    tByte       :: Last(0:7)

! FLOW

    ! initialize indices
    I = 0_kIndex
    J = 0_kIndex
    OutLen = MD%GetDigestLen()
    
    ! unpack the internal states to the output array
    DO WHILE (I < 8_kIndex)
        CALL ByteUnpackLE(MD%H(I), BytesOut, J)
        I = I + 1_kIndex
        J = J + 8_kIndex
        IF (J >= OutLen-8_kIndex) EXIT
    END DO
    
    ! unpack the last internal state to the buffer
    CALL ByteUnpackLE(MD%H(I), Last, 0_kIndex)
    
    ! copy the buffer to the output array
    BytesOut(J:OutLen-1) = Last(0:OutLen-J-1)

    RETURN

END SUBROUTINE Blake2B_EncodeOutput

!******************************************************************************

END MODULE MClass_Blake2B
    
!******************************************************************************
