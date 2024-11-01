
MODULE MClass_Blake1B

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Blake1B* type and its related routines.
!   The *Blake1B* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *Blake1B* type implements an incremental cryptographic hash
!   function by employing either the *BLAKE-384* or the *BLAKE-512
!   message-digest* algorithm where both algorithms are described in
!   the *Hash Function BLAKE* book [1].  The implementation here is
!   based mainly on the *SPHLIB* implementation [2].  <br>
!   By default, the *Blake1B* type employs the *BLAKE-512 message-digest*
!   algorithm.  However, a user can specify the *IsBLAKE384* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *BLAKE-384 message-digest* algorithm
!   instead of the default one. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.aumasson.jp/blake/book/">J.P. Aumasson, W. Meier,
!       R.C.W. Phan, and L. Henzen. 2015. The Hash Function BLAKE. Springer. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_BytePack,           ONLY: BytePackBE, ByteUnpackBE
    USE MClass_BaseDigest
    USE MClass_MDEngine

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: Blake1B

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tIndex, PARAMETER :: BlockLen = 128_kIndex
    tIndex, PARAMETER :: DLen384  = 48_kIndex
    tIndex, PARAMETER :: DLen512  = 64_kIndex
    tLong,  PARAMETER :: IV384(0:7) = [                               &
            ToInt64(Z'CBBB9D5DC1059ED8'), ToInt64(Z'629A292A367CD507'), &
            ToInt64(Z'9159015A3070DD17'), ToInt64(Z'152FECD8F70E5939'), &
            ToInt64(Z'67332667FFC00B31'), ToInt64(Z'8EB44A8768581511'), &
            ToInt64(Z'DB0C2E0D64F98FA7'), ToInt64(Z'47B5481DBEFA4FA4')]
    tLong,  PARAMETER :: IV512(0:7) = [                               &
            ToInt64(Z'6A09E667F3BCC908'), ToInt64(Z'BB67AE8584CAA73B'), &
            ToInt64(Z'3C6EF372FE94F82B'), ToInt64(Z'A54FF53A5F1D36F1'), &
            ToInt64(Z'510E527FADE682D1'), ToInt64(Z'9B05688C2B3E6C1F'), &
            ToInt64(Z'1F83D9ABFB41BD6B'), ToInt64(Z'5BE0CD19137E2179')]
    tIndex, PARAMETER   :: SIGMA(0:255) = [                                 &
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
            14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3, &
            11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4, &
             7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8, &
             9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13, &
             2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9]
    tLong,  PARAMETER   :: CS(0:15) = [                               &
            ToInt64(Z'243F6A8885A308D3'), ToInt64(Z'13198A2E03707344'), &
            ToInt64(Z'A4093822299F31D0'), ToInt64(Z'082EFA98EC4E6C89'), &
            ToInt64(Z'452821E638D01377'), ToInt64(Z'BE5466CF34E90C6C'), &
            ToInt64(Z'C0AC29B7C97C50DD'), ToInt64(Z'3F84D5B5B5470917'), &
            ToInt64(Z'9216D5D98979FB1B'), ToInt64(Z'D1310BA698DFB5AC'), &
            ToInt64(Z'2FFD72DBD01ADFB7'), ToInt64(Z'B8E1AFED6A267E96'), &
            ToInt64(Z'BA7C9045F12C7F99'), ToInt64(Z'24A19947B3916CF7'), &
            ToInt64(Z'0801F2E2858EFC16'), ToInt64(Z'636920D871574E69')]

!** DERIVED TYPE DEFINITIONS
    !> *Blake1B* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing either
    !  the *BLAKE-384* or the *BLAKE-512 message-digest* algorithm.
    TYPE, EXTENDS(MDEngine) :: Blake1B
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state and counter variables
        tLong       :: H(0:7) = IV512(0:7)
        tLong       :: S(0:3) = 0_kInt64
        tLong       :: T(0:1) = 0_kInt64
        !% flag indicating whether the BLAKE-384 algorithm is employed or not.
        tLogical    :: IsBLAKE384 = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                    Private Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => Blake1B_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (BLAKE-512).
        PROCEDURE       :: Initialize   => Blake1B_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => Blake1B_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => Blake1B_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => Blake1B_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => Blake1B_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => Blake1B_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => Blake1B_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => Blake1B_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => Blake1B_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => Blake1B_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (BLAKE-512) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the BLAKE-384 algorithm <br>
        !   --->    CALL MD%Create(IsBLAKE384=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
        ! ---------------------------------------------------------------------
    END TYPE Blake1B

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Blake1B_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake1B), INTENT(INOUT)   :: MD    !! 'Blake1B' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the BLAKE-512 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE Blake1B_Initialize

!******************************************************************************

SUBROUTINE Blake1B_Initialize_wFlag(MD, IsBLAKE384)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake1B), INTENT(INOUT)   :: MD           !! 'Blake1B' object
    tLogical,       INTENT(IN)      :: IsBLAKE384
    !^ flag indicating whether the BLAKE-384 algorithm is employed or not. <br>
    !  - If true, use the BLAKE-384 algorithm. <br>
    !  - Otherwise, use the BLAKE-512 algorithm. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsBLAKE384 = IsBLAKE384
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE Blake1B_Initialize_wFlag

!******************************************************************************

SUBROUTINE Blake1B_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake1B), INTENT(INOUT) :: MD   !! 'Blake1B' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    MD%S = 0_kInt64
    MD%T = 0_kInt64
    IF (MD%IsBLAKE384) THEN
        MD%H = IV384
    ELSE
        MD%H = IV512
    END IF
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE Blake1B_Reset

!******************************************************************************

SUBROUTINE Blake1B_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake1B),                 INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(Blake1B :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (Blake1B)
        CALL Dst%Create(Src%IsBLAKE384)
        Dst%H      = Src%H
        Dst%S      = Src%S
        Dst%T      = Src%T
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE Blake1B_GetClone

!******************************************************************************

FUNCTION Blake1B_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake1B), INTENT(IN)  :: MD       !! 'Blake1B' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsBLAKE384) THEN
        Name = 'BLAKE-384'
    ELSE
        Name = 'BLAKE-512'
    END IF

    RETURN

END FUNCTION Blake1B_GetName

!******************************************************************************

FUNCTION Blake1B_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake1B), INTENT(IN)  :: MD       !! 'Blake1B' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsBLAKE384) THEN
        Length = DLen384
    ELSE
        Length = DLen512
    END IF

    RETURN

END FUNCTION Blake1B_GetDigestLen

!******************************************************************************

FUNCTION Blake1B_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake1B), INTENT(IN)  :: MD       !! 'Blake1B' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION Blake1B_GetBlockLen

!******************************************************************************

SUBROUTINE Blake1B_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake1B), TARGET, INTENT(INOUT)   :: MD           !! 'Blake1B' object
    tByte,         POINTER, INTENT(INOUT)   :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE Blake1B_SetBufPtr

!******************************************************************************

SUBROUTINE Blake1B_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake1B), INTENT(INOUT)   :: MD           !! 'Blake1B' object
    tByte,          INTENT(IN)      :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: M(0:15)

! FLOW

    ! initialize
    MD%T(0) = MD%T(0) + 1024_kInt64
    IF (IAND(MD%T(0), NOT(ToInt64(Z'00000000000003FF'))) == 0_kInt64) MD%T(1) = MD%T(1) + 1

    ! input block
    CALL BytePackBE(BytesIn, 0_kIndex, M)

    ! perform block transformation
    CALL Process_One_Block(MD, M)

    RETURN

CONTAINS

    SUBROUTINE Process_One_Block(MD, M)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To process one block of data.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(Blake1B), INTENT(INOUT)   :: MD       ! 'Blake1B' object
        tLong,          INTENT(IN)      :: M(0:15)  ! the message block

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tLong       :: V(0:15)
        tIndex      :: I, J, R
        tIndex      :: K(0:15)

    !** SUBROUTINE MACRO DEFINITIONS:
#define IEOR2(X, Y, Z)      IEOR(IEOR(X, Y), Z)
#define LShift4(X)          SHIFTL(X, 4)
#define GB(K0, K1, A, B, C, D) \
    A = A + B + (IEOR(M(K0), CS(K1))); \
    D = RotateRight(IEOR(D, A), 32); \
    C = C + D; \
    B = RotateRight(IEOR(B, C), 25); \
    A = A + B + (IEOR(M(K1), CS(K0))); \
    D = RotateRight(IEOR(D, A), 16); \
    C = C + D; \
    B = RotateRight(IEOR(B, C), 11);

    ! FLOW

        ! initialize
        V(0:3) = MD%S(0:3)
        V(4:5) = MD%T(0)
        V(6:7) = MD%T(1)
        DO I = 0, 7
            V(I+8) = IEOR(V(I), CS(I))
        END DO
        V(0:7)  = MD%H(0:7)

        ! perform block transformation
        DO R = 0, 15
            ! compute indices
            DO I = 0, 15
                K(I) = SIGMA(LShift4(R) + I)
            END DO
            ! perform core operations
            GB(K(0),  K(1),  V(0), V(4), V(8),  V(12))
            GB(K(2),  K(3),  V(1), V(5), V(9),  V(13))
            GB(K(4),  K(5),  V(2), V(6), V(10), V(14))
            GB(K(6),  K(7),  V(3), V(7), V(11), V(15))
            GB(K(8),  K(9),  V(0), V(5), V(10), V(15))
            GB(K(10), K(11), V(1), V(6), V(11), V(12))
            GB(K(12), K(13), V(2), V(7), V(8),  V(13))
            GB(K(14), K(15), V(3), V(4), V(9),  V(14))
        END DO

        ! update H
        DO I = 0, 7
            J = I
            IF (J > 3) J = J - 4
            MD%H(I) = IEOR(MD%H(I), IEOR2(MD%S(J), V(I), V(I+8)))
        END DO

#undef IEOR2
#undef LShift4
#undef GB

        RETURN

    END SUBROUTINE Process_One_Block

    !**************************************************************************

END SUBROUTINE Blake1B_ProcessBlock

!******************************************************************************

SUBROUTINE Blake1B_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake1B), INTENT(INOUT)   :: MD           !! 'Blake1B' object
    tByte,          INTENT(INOUT)   :: BytesOut(0:) !! the output buffer
    tIndex,         INTENT(IN)      :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE Blake1B_DoPadding

!******************************************************************************

SUBROUTINE Blake1B_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Blake1B), INTENT(INOUT)   :: MD           !! 'Blake1B' object
    tByte,          INTENT(IN)      :: LastByte     !! the last byte
    tByte,          INTENT(IN)      :: NBits        !! number of bits in the last byte
    tByte,          INTENT(INOUT)   :: BytesOut(0:) !! the output buffer
    tIndex,         INTENT(IN)      :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Ptr, BitLen, I
    tLong       :: TH, TL
    tByte       :: Z

! FLOW

    ASSOCIATE(TmpBuf => MD%BufArr)   
        TmpBuf = 0_kInt8
        Ptr = MD%GetBufLen()
        BitLen = SHIFTL(Ptr, 3) + NBits
        Z = SHIFTR(FByte80, NBits)
        TmpBuf(Ptr) = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
        TL = MD%T(0) + BitLen
        TH = MD%T(1)
        IF ((Ptr == 0_kIndex).AND.(NBits == 0_kIndex)) THEN
            MD%T(0) = ToInt64(Z'FFFFFFFFFFFFFC00')
            MD%T(1) = ToInt64(Z'FFFFFFFFFFFFFFFF')
        ELSEIF (MD%T(0) == 0) THEN
            MD%T(0) = ToInt64(Z'FFFFFFFFFFFFFC00') + ToInt64(BitLen)
            MD%T(1) = MD%T(1) - 1_kInt64
        ELSE
            MD%T(0) = MD%T(0) - ToInt64(1024 - BitLen)
        END IF
        IF (BitLen <= 894_kIndex) THEN
            TmpBuf(Ptr+1:111) = FByte00
            IF (MD%GetDigestLen() == 64_kIndex) TmpBuf(111) = IOR(TmpBuf(111), FByte01)
            CALL ByteUnpackBE(TH, TmpBuf, 112_kIndex)
            CALL ByteUnpackBE(TL, TmpBuf, 120_kIndex)
            CALL MD%Update(TmpBuf, Ptr, 128_kIndex - Ptr)
        ELSE
            TmpBuf(Ptr+1:127) = FByte00
            CALL MD%Update(TmpBuf, Ptr, 128_kIndex - Ptr)
            MD%T(0) = ToInt64(Z'FFFFFFFFFFFFFC00')
            MD%T(1) = ToInt64(Z'FFFFFFFFFFFFFFFF')
            TmpBuf(0:111) = FByte00
            IF (MD%GetDigestLen() == 64_kIndex) TmpBuf(111) = FByte01
            CALL ByteUnpackBE(TH, TmpBuf, 112_kIndex)
            CALL ByteUnpackBE(TL, TmpBuf, 120_kIndex)
            CALL MD%Update(TmpBuf, 0_kIndex, 128_kIndex)
        END IF
    END ASSOCIATE

    DO I = 0_kIndex, (MD%GetDigestLen()/8_kIndex - 1_kIndex)
        CALL ByteUnpackBE(MD%H(I), BytesOut, Offset + 8_kIndex*I)
    END DO

    RETURN

END SUBROUTINE Blake1B_AddBitsNPad

!******************************************************************************

END MODULE MClass_Blake1B
    
!******************************************************************************
