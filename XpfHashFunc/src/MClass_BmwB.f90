
MODULE MClass_BmwB

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *BmwB* type and its related routines.
!   The *BmwB* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *BmwB* type implements an incremental cryptographic hash function
!   by employing a *Blue Midnight Wish (BMW) message-digest* algorithm
!   (either the *BMW-384* or the *BMW-512*) [1].  The implementation here
!   is based mainly on the *SPHLIB* implementation [2].  <br>
!   By default, the *BmwB* type employs the *BMW-512 message-digest*
!   algorithm.  However, a user can specify the *IsBMW384* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *BMW-384 message-digest* algorithm
!   instead of the default one. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="http://people.item.ntnu.no/~danilog/Hash/BMW-SecondRound/">
!       The Blue Midnight Wish cryptographic hash function package submitted
!       to the second round of the NIST's SHA-3 hash competition. </a> <br>
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
    PUBLIC :: BmwB

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tIndex, PARAMETER   :: BlockLen = 128_kIndex
    tIndex, PARAMETER   :: DLen384  = 48_kIndex
    tIndex, PARAMETER   :: DLen512  = 64_kIndex
    tLong,  PARAMETER   :: IV384(0:15) = [                            &
            ToInt64(Z'0001020304050607'), ToInt64(Z'08090A0B0C0D0E0F'), &
            ToInt64(Z'1011121314151617'), ToInt64(Z'18191A1B1C1D1E1F'), &
            ToInt64(Z'2021222324252627'), ToInt64(Z'28292A2B2C2D2E2F'), &
            ToInt64(Z'3031323334353637'), ToInt64(Z'38393A3B3C3D3E3F'), &
            ToInt64(Z'4041424344454647'), ToInt64(Z'48494A4B4C4D4E4F'), &
            ToInt64(Z'5051525354555657'), ToInt64(Z'58595A5B5C5D5E5F'), &
            ToInt64(Z'6061626364656667'), ToInt64(Z'68696A6B6C6D6E6F'), &
            ToInt64(Z'7071727374757677'), ToInt64(Z'78797A7B7C7D7E7F')]
    tLong,  PARAMETER   :: IV512(0:15) = [                            &
            ToInt64(Z'8081828384858687'), ToInt64(Z'88898A8B8C8D8E8F'), &
            ToInt64(Z'9091929394959697'), ToInt64(Z'98999A9B9C9D9E9F'), &
            ToInt64(Z'A0A1A2A3A4A5A6A7'), ToInt64(Z'A8A9AAABACADAEAF'), &
            ToInt64(Z'B0B1B2B3B4B5B6B7'), ToInt64(Z'B8B9BABBBCBDBEBF'), &
            ToInt64(Z'C0C1C2C3C4C5C6C7'), ToInt64(Z'C8C9CACBCCCDCECF'), &
            ToInt64(Z'D0D1D2D3D4D5D6D7'), ToInt64(Z'D8D9DADBDCDDDEDF'), &
            ToInt64(Z'E0E1E2E3E4E5E6E7'), ToInt64(Z'E8E9EAEBECEDEEEF'), &
            ToInt64(Z'F0F1F2F3F4F5F6F7'), ToInt64(Z'F8F9FAFBFCFDFEFF')]
    tLong,  PARAMETER   :: FINAL_PARAM(0:15) = [                      &
            ToInt64(Z'AAAAAAAAAAAAAAA0'), ToInt64(Z'AAAAAAAAAAAAAAA1'), &
            ToInt64(Z'AAAAAAAAAAAAAAA2'), ToInt64(Z'AAAAAAAAAAAAAAA3'), &
            ToInt64(Z'AAAAAAAAAAAAAAA4'), ToInt64(Z'AAAAAAAAAAAAAAA5'), &
            ToInt64(Z'AAAAAAAAAAAAAAA6'), ToInt64(Z'AAAAAAAAAAAAAAA7'), &
            ToInt64(Z'AAAAAAAAAAAAAAA8'), ToInt64(Z'AAAAAAAAAAAAAAA9'), &
            ToInt64(Z'AAAAAAAAAAAAAAAA'), ToInt64(Z'AAAAAAAAAAAAAAAB'), &
            ToInt64(Z'AAAAAAAAAAAAAAAC'), ToInt64(Z'AAAAAAAAAAAAAAAD'), &
            ToInt64(Z'AAAAAAAAAAAAAAAE'), ToInt64(Z'AAAAAAAAAAAAAAAF')]

!** DERIVED TYPE DEFINITIONS
    !> *BmwB* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing either
    !  the *BMW-384* or the *BMW-512 message-digest* algorithm.
    TYPE, EXTENDS(MDEngine) :: BmwB
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state variable
        tLong       :: H(0:15) = IV512(0:15)
        !% flag indicating whether the BMW-384 algorithm is employed or not.
        tLogical    :: IsBMW384 = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                    Private Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => BmwB_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (BMW-512).
        PROCEDURE       :: Initialize   => BmwB_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => BmwB_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => BmwB_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => BmwB_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => BmwB_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => BmwB_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => BmwB_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => BmwB_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => BmwB_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => BmwB_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (BMW-512) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the BMW-384 algorithm <br>
        !   --->    CALL MD%Create(IsBMW384=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
        ! ---------------------------------------------------------------------
    END TYPE BmwB

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE BmwB_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwB), INTENT(INOUT)  :: MD    !! 'BmwB' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the BMW-512 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE BmwB_Initialize

!******************************************************************************

SUBROUTINE BmwB_Initialize_wFlag(MD, IsBMW384)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwB), INTENT(INOUT)  :: MD           !! 'BmwB' object
    tLogical,    INTENT(IN)     :: IsBMW384
    !^ flag indicating whether the BMW-384 algorithm is employed or not. <br>
    !  - If true, use the BMW-384 algorithm. <br>
    !  - Otherwise, use the BMW-512 algorithm. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsBMW384 = IsBMW384
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE BmwB_Initialize_wFlag

!******************************************************************************

SUBROUTINE BmwB_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwB), INTENT(INOUT)  :: MD   !! 'BmwB' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    IF (MD%IsBMW384) THEN
        MD%H = IV384
    ELSE
        MD%H = IV512
    END IF
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE BmwB_Reset

!******************************************************************************

SUBROUTINE BmwB_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwB),                    INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(BmwB :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (BmwB)
        CALL Dst%Create(Src%IsBMW384)
        Dst%H      = Src%H
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE BmwB_GetClone

!******************************************************************************

FUNCTION BmwB_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwB), INTENT(IN) :: MD       !! 'BmwB' object
    tCharAlloc              :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsBMW384) THEN
        Name = 'BMW-384'
    ELSE
        Name = 'BMW-512'
    END IF

    RETURN

END FUNCTION BmwB_GetName

!******************************************************************************

FUNCTION BmwB_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwB), INTENT(IN) :: MD       !! 'BmwB' object
    tIndex                  :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsBMW384) THEN
        Length = DLen384
    ELSE
        Length = DLen512
    END IF

    RETURN

END FUNCTION BmwB_GetDigestLen

!******************************************************************************

FUNCTION BmwB_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwB), INTENT(IN) :: MD       !! 'BmwB' object
    tIndex                  :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION BmwB_GetBlockLen

!******************************************************************************

SUBROUTINE BmwB_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwB), TARGET, INTENT(INOUT)  :: MD           !! 'BmwB' object
    tByte,      POINTER, INTENT(INOUT)  :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE BmwB_SetBufPtr

!******************************************************************************

SUBROUTINE BmwB_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwB), INTENT(INOUT)  :: MD           !! 'BmwB' object
    tByte,       INTENT(IN)     :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: M(0:15)

! FLOW
    
    ! input block
    CALL BytePackLE(BytesIn, 0_kIndex, M)
    
    ! perform 16 rounds of mixing
    CALL Compress(MD%H, M)

    RETURN

END SUBROUTINE BmwB_ProcessBlock

!******************************************************************************

SUBROUTINE BmwB_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwB), INTENT(INOUT)  :: MD           !! 'BmwB' object
    tByte,       INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,      INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE BmwB_DoPadding

!******************************************************************************

SUBROUTINE BmwB_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwB), INTENT(INOUT)  :: MD           !! 'BmwB' object
    tByte,       INTENT(IN)     :: LastByte     !! the last byte
    tByte,       INTENT(IN)     :: NBits        !! number of bits in the last byte
    tByte,       INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,      INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: H2(0:15)
    tIndex      :: Ptr, I, J, DLen
    tLong       :: BitLen
    tByte       :: Z

! FLOW

    ! padding
    ASSOCIATE(TmpBuf => MD%BufArr)   
        Ptr = MD%GetBufLen()
        BitLen = SHIFTL(ToInt64(MD%GetBlockCount()), 10) + SHIFTL(ToInt64(Ptr), 3)
        Z = SHIFTR(FByte80, NBits)
        TmpBuf(Ptr) = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
        Ptr = Ptr + 1_kIndex
        IF (Ptr > 120_kIndex) THEN
            TmpBuf(Ptr:127) = FByte00
            CALL MD%ProcessBlock(TmpBuf)
            Ptr = 0_kIndex
        END IF
        TmpBuf(Ptr:119) = FByte00
        CALL ByteUnpackLE(BitLen + NBits, TmpBuf, 120_kIndex)
        CALL MD%ProcessBlock(TmpBuf)
    END ASSOCIATE

    ! finalizing
    H2 = MD%H
    MD%H = FINAL_PARAM
    CALL Compress(MD%H, H2)
    DLen = SHIFTR(MD%GetDigestLen(), 3)
    J = 16_kIndex - DLen
    DO I = 0_kIndex, DLen-1_kIndex
        CALL ByteUnpackLE(MD%H(J), BytesOut, Offset+I*8_kIndex)
        J = J + 1_kIndex
    END DO
        
    RETURN

END SUBROUTINE BmwB_AddBitsNPad

!******************************************************************************

SUBROUTINE Compress(H, M)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the compression of the given message.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tLong, INTENT(INOUT)    :: H(0:15)
    tLong, INTENT(IN)       :: M(0:15)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong    :: Q(0:31)
    tLong    :: Xl, Xh

!** SUBROUTINE MACRO DEFINITIONS:
#define SB0(x)      IEOR(IEOR(IEOR(SHIFTR(x, 1), SHIFTL(x, 3)), RotateLeft(x,  4)), RotateLeft(x, 37))
#define SB1(x)      IEOR(IEOR(IEOR(SHIFTR(x, 1), SHIFTL(x, 2)), RotateLeft(x,  13)), RotateLeft(x, 43))
#define SB2(x)      IEOR(IEOR(IEOR(SHIFTR(x, 2), SHIFTL(x, 1)), RotateLeft(x,  19)), RotateLeft(x, 53))
#define SB3(x)      IEOR(IEOR(IEOR(SHIFTR(x, 2), SHIFTL(x, 2)), RotateLeft(x,  28)), RotateLeft(x, 59))
#define SB4(x)      IEOR(SHIFTR(x, 1), x)
#define SB5(x)      IEOR(SHIFTR(x, 2), x)
#define RB1(x)      RotateLeft(x,  5)
#define RB2(x)      RotateLeft(x, 11)
#define RB3(x)      RotateLeft(x, 27)
#define RB4(x)      RotateLeft(x, 32)
#define RB5(x)      RotateLeft(x, 37)
#define RB6(x)      RotateLeft(x, 43)
#define RB7(x)      RotateLeft(x, 53)
#define KB(j)       (j * ToInt64(Z'0555555555555555'))
#define Add_Elt_B(mf, hf, j0m, j1m, j3m, j4m, j7m, j10m, j11m, j16) \
    IEOR((RotateLeft(mf(j0m),  j1m)  + RotateLeft(mf(j3m), j4m) - \
          RotateLeft(mf(j10m), j11m) + KB(j16)), hf(j7m))
#define Expand1B_Inner(qf, mf, hf, i16, i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i0m, i1m, i3m, i4m, i7m, i10m, i11m) \
         (SB1(qf(i0))  + SB2(qf(i1))  + SB3(qf(i2))  + SB0(qf(i3)) \
        + SB1(qf(i4))  + SB2(qf(i5))  + SB3(qf(i6))  + SB0(qf(i7)) \
        + SB1(qf(i8))  + SB2(qf(i9))  + SB3(qf(i10)) + SB0(qf(i11)) \
        + SB1(qf(i12)) + SB2(qf(i13)) + SB3(qf(i14)) + SB0(qf(i15)) \
        + Add_Elt_B(mf, hf, i0m, i1m, i3m, i4m, i7m, i10m, i11m, i16))
#define Expand1B16(qf, mf, hf, i16)     Expand1B_Inner(qf, mf, hf, i16, 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 0,  1,  3,  4,  7, 10, 11)
#define Expand1B17(qf, mf, hf, i16)     Expand1B_Inner(qf, mf, hf, i16, 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 1,  2,  4,  5,  8, 11, 12)
#define Expand2B_Inner(qf, mf, hf, i16, i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i0m, i1m, i3m, i4m, i7m, i10m, i11m) \
         (qf(i0)  + RB1(qf(i1))  + qf(i2)  + RB2(qf(i3)) \
        + qf(i4)  + RB3(qf(i5))  + qf(i6)  + RB4(qf(i7)) \
        + qf(i8)  + RB5(qf(i9))  + qf(i10) + RB6(qf(i11)) \
        + qf(i12) + RB7(qf(i13)) + SB4(qf(i14)) + SB5(qf(i15)) \
        + Add_Elt_B(mf, hf, i0m, i1m, i3m, i4m, i7m, i10m, i11m, i16))
#define Expand2B18(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 2,  3,  5,  6,  9, 12, 13)
#define Expand2B19(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 3,  4,  6,  7, 10, 13, 14)
#define Expand2B20(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 4,  5,  7,  8, 11, 14, 15)
#define Expand2B21(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 5,  6,  8,  9, 12, 15, 16)
#define Expand2B22(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 6,  7,  9, 10, 13,  0,  1)
#define Expand2B23(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 7,  8, 10, 11, 14,  1,  2)
#define Expand2B24(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 8,  9, 11, 12, 15,  2,  3)
#define Expand2B25(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 9, 10, 12, 13,  0,  3,  4)
#define Expand2B26(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 10, 11, 13, 14,  1,  4,  5)
#define Expand2B27(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 11, 12, 14, 15,  2,  5,  6)
#define Expand2B28(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 12, 13, 15, 16,  3,  6,  7)
#define Expand2B29(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 13, 14,  0,  1,  4,  7,  8)
#define Expand2B30(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 14, 15,  1,  2,  5,  8,  9)
#define Expand2B31(qf, mf, hf, i16)     Expand2B_Inner(qf, mf, hf, i16, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 15, 16,  2,  3,  6,  9, 10)
#define MAKE_W(i0, op01, i1, op12, i2, op23, i3, op34, i4) \
    (IEOR(M(i0), H(i0)) op01 IEOR(M(i1), H(i1)) op12 IEOR(M(i2), H(i2)) op23 IEOR(M(i3), H(i3)) op34 IEOR(M(i4), H(i4)))
#define Wb0             MAKE_W( 5, -,  7, +, 10, +, 13, +, 14)
#define Wb1             MAKE_W( 6, -,  8, +, 11, +, 14, -, 15)
#define Wb2             MAKE_W( 0, +,  7, +,  9, -, 12, +, 15)
#define Wb3             MAKE_W( 0, -,  1, +,  8, -, 10, +, 13)
#define Wb4             MAKE_W( 1, +,  2, +,  9, -, 11, -, 14)
#define Wb5             MAKE_W( 3, -,  2, +, 10, -, 12, +, 15)
#define Wb6             MAKE_W( 4, -,  0, -,  3, -, 11, +, 13)
#define Wb7             MAKE_W( 1, -,  4, -,  5, -, 12, -, 14)
#define Wb8             MAKE_W( 2, -,  5, -,  6, +, 13, -, 15)
#define Wb9             MAKE_W( 0, -,  3, +,  6, -,  7, +, 14)
#define Wb10            MAKE_W( 8, -,  1, -,  4, -,  7, +, 15)
#define Wb11            MAKE_W( 8, -,  0, -,  2, -,  5, +,  9)
#define Wb12            MAKE_W( 1, +,  3, -,  6, -,  9, +, 10)
#define Wb13            MAKE_W( 2, +,  4, +,  7, +, 10, +, 11)
#define Wb14            MAKE_W( 3, -,  5, +,  8, -, 11, -, 12)
#define Wb15            MAKE_W(12, -,  4, -,  6, -,  9, +, 13)
#define IEOR3(A, B, C)  IEOR(IEOR(A, B), C)

! FLOW

    Q( 0) = SB0(Wb0 ) + H( 1)
    Q( 1) = SB1(Wb1 ) + H( 2)
    Q( 2) = SB2(Wb2 ) + H( 3)
    Q( 3) = SB3(Wb3 ) + H( 4)
    Q( 4) = SB4(Wb4 ) + H( 5)
    Q( 5) = SB0(Wb5 ) + H( 6)
    Q( 6) = SB1(Wb6 ) + H( 7)
    Q( 7) = SB2(Wb7 ) + H( 8)
    Q( 8) = SB3(Wb8 ) + H( 9)
    Q( 9) = SB4(Wb9 ) + H(10)
    Q(10) = SB0(Wb10) + H(11)
    Q(11) = SB1(Wb11) + H(12)
    Q(12) = SB2(Wb12) + H(13)
    Q(13) = SB3(Wb13) + H(14)
    Q(14) = SB4(Wb14) + H(15)
    Q(15) = SB0(Wb15) + H( 0)
    
    Q(16) = Expand1B16(Q, M, H, 16)
    Q(17) = Expand1B17(Q, M, H, 17)
    Q(18) = Expand2B18(Q, M, H, 18)
    Q(19) = Expand2B19(Q, M, H, 19)
    Q(20) = Expand2B20(Q, M, H, 20)
    Q(21) = Expand2B21(Q, M, H, 21)
    Q(22) = Expand2B22(Q, M, H, 22)
    Q(23) = Expand2B23(Q, M, H, 23)
    Q(24) = Expand2B24(Q, M, H, 24)
    Q(25) = Expand2B25(Q, M, H, 25)
    Q(26) = Expand2B26(Q, M, H, 26)
    Q(27) = Expand2B27(Q, M, H, 27)
    Q(28) = Expand2B28(Q, M, H, 28)
    Q(29) = Expand2B29(Q, M, H, 29)
    Q(30) = Expand2B30(Q, M, H, 30)
    Q(31) = Expand2B31(Q, M, H, 31)
    
    XL = IEOR(IEOR(IEOR(IEOR(IEOR(IEOR(IEOR(Q(16), Q(17)), Q(18)), Q(19)), &
                                            Q(20)), Q(21)), Q(22)), Q(23))
    XH = IEOR(IEOR(IEOR(IEOR(IEOR(IEOR(IEOR(IEOR(XL, Q(24)), Q(25)), Q(26)), Q(27)), &
                                                 Q(28)), Q(29)), Q(30)), Q(31))
    H(0)  = IEOR3(SHIFTL(XH, 5),  SHIFTR(Q(16), 5), M(0)) + IEOR3(XL, Q(24), Q(0))
    H(1)  = IEOR3(SHIFTR(XH, 7),  SHIFTL(Q(17), 8), M(1)) + IEOR3(XL, Q(25), Q(1))
    H(2)  = IEOR3(SHIFTR(XH, 5),  SHIFTL(Q(18), 5), M(2)) + IEOR3(XL, Q(26), Q(2))
    H(3)  = IEOR3(SHIFTR(XH, 1),  SHIFTL(Q(19), 5), M(3)) + IEOR3(XL, Q(27), Q(3))
    H(4)  = IEOR3(SHIFTR(XH, 3),  SHIFTL(Q(20), 0), M(4)) + IEOR3(XL, Q(28), Q(4))
    H(5)  = IEOR3(SHIFTL(XH, 6),  SHIFTR(Q(21), 6), M(5)) + IEOR3(XL, Q(29), Q(5))
    H(6)  = IEOR3(SHIFTR(XH, 4),  SHIFTL(Q(22), 6), M(6)) + IEOR3(XL, Q(30), Q(6))
    H(7)  = IEOR3(SHIFTR(XH, 11), SHIFTL(Q(23), 2), M(7)) + IEOR3(XL, Q(31), Q(7))
    H(8)  = RotateLeft(H(4), 9)  + IEOR3(XH, Q(24), M(8))  + IEOR3(SHIFTL(XL, 8), Q(23), Q(8))
    H(9)  = RotateLeft(H(5), 10) + IEOR3(XH, Q(25), M(9))  + IEOR3(SHIFTR(XL, 6), Q(16), Q(9))
    H(10) = RotateLeft(H(6), 11) + IEOR3(XH, Q(26), M(10)) + IEOR3(SHIFTL(XL, 6), Q(17), Q(10))
    H(11) = RotateLeft(H(7), 12) + IEOR3(XH, Q(27), M(11)) + IEOR3(SHIFTL(XL, 4), Q(18), Q(11))
    H(12) = RotateLeft(H(0), 13) + IEOR3(XH, Q(28), M(12)) + IEOR3(SHIFTR(XL, 3), Q(19), Q(12))
    H(13) = RotateLeft(H(1), 14) + IEOR3(XH, Q(29), M(13)) + IEOR3(SHIFTR(XL, 4), Q(20), Q(13))
    H(14) = RotateLeft(H(2), 15) + IEOR3(XH, Q(30), M(14)) + IEOR3(SHIFTR(XL, 7), Q(21), Q(14))
    H(15) = RotateLeft(H(3), 16) + IEOR3(XH, Q(31), M(15)) + IEOR3(SHIFTR(XL, 2), Q(22), Q(15))

    RETURN

END SUBROUTINE Compress

!******************************************************************************

END MODULE MClass_BmwB
    
!******************************************************************************
