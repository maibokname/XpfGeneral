
MODULE MClass_BmwS

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *BmwS* type and its related routines.
!   The *BmwS* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *BmwS* type implements an incremental cryptographic hash function
!   by employing a *Blue Midnight Wish (BMW) message-digest* algorithm
!   (either the *BMW-224* or the *BMW-256*) [1].  The implementation here
!   is based mainly on the *SPHLIB* implementation [2].  <br>
!   By default, the *BmwS* type employs the *BMW-256 message-digest*
!   algorithm.  However, a user can specify the *IsBMW224* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *BMW-224 message-digest* algorithm
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
    PUBLIC :: BmwS

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tIndex,   PARAMETER :: BlockLen = 64_kIndex
    tIndex,   PARAMETER :: DLen224  = 28_kIndex
    tIndex,   PARAMETER :: DLen256  = 32_kIndex
    tInteger, PARAMETER :: IV224(0:15) = [                  &
            ToInt32(Z'00010203'), ToInt32(Z'04050607'), &
            ToInt32(Z'08090A0B'), ToInt32(Z'0C0D0E0F'), &
            ToInt32(Z'10111213'), ToInt32(Z'14151617'), &
            ToInt32(Z'18191A1B'), ToInt32(Z'1C1D1E1F'), &
            ToInt32(Z'20212223'), ToInt32(Z'24252627'), &
            ToInt32(Z'28292A2B'), ToInt32(Z'2C2D2E2F'), &
            ToInt32(Z'30313233'), ToInt32(Z'34353637'), &
            ToInt32(Z'38393A3B'), ToInt32(Z'3C3D3E3F')]
    tInteger, PARAMETER :: IV256(0:15) = [                  &
            ToInt32(Z'40414243'), ToInt32(Z'44454647'), &
            ToInt32(Z'48494A4B'), ToInt32(Z'4C4D4E4F'), &
            ToInt32(Z'50515253'), ToInt32(Z'54555657'), &
            ToInt32(Z'58595A5B'), ToInt32(Z'5C5D5E5F'), &
            ToInt32(Z'60616263'), ToInt32(Z'64656667'), &
            ToInt32(Z'68696A6B'), ToInt32(Z'6C6D6E6F'), &
            ToInt32(Z'70717273'), ToInt32(Z'74757677'), &
            ToInt32(Z'78797A7B'), ToInt32(Z'7C7D7E7F')]
    tInteger, PARAMETER :: FINAL_PARAM(0:15) = [            &
            ToInt32(Z'AAAAAAA0'), ToInt32(Z'AAAAAAA1'), &
            ToInt32(Z'AAAAAAA2'), ToInt32(Z'AAAAAAA3'), &
            ToInt32(Z'AAAAAAA4'), ToInt32(Z'AAAAAAA5'), &
            ToInt32(Z'AAAAAAA6'), ToInt32(Z'AAAAAAA7'), &
            ToInt32(Z'AAAAAAA8'), ToInt32(Z'AAAAAAA9'), &
            ToInt32(Z'AAAAAAAA'), ToInt32(Z'AAAAAAAB'), &
            ToInt32(Z'AAAAAAAC'), ToInt32(Z'AAAAAAAD'), &
            ToInt32(Z'AAAAAAAE'), ToInt32(Z'AAAAAAAF')]

!** DERIVED TYPE DEFINITIONS
    !> *BmwS* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing either
    !  the *BMW-224* or the *BMW-256 message-digest* algorithm.
    TYPE, EXTENDS(MDEngine) :: BmwS
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state variable
        tInteger    :: H(0:15) = IV256(0:15)
        !% flag indicating whether the BMW-224 algorithm is employed or not.
        tLogical    :: IsBMW224 = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                    Private Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => BmwS_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (BMW-256).
        PROCEDURE       :: Initialize   => BmwS_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => BmwS_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => BmwS_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => BmwS_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => BmwS_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => BmwS_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => BmwS_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => BmwS_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => BmwS_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => BmwS_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (BMW-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the BMW-224 algorithm <br>
        !   --->    CALL MD%Create(IsBMW224=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
        ! ---------------------------------------------------------------------
    END TYPE BmwS

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE BmwS_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwS), INTENT(INOUT)  :: MD    !! 'BmwS' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the BMW-256 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE BmwS_Initialize

!******************************************************************************

SUBROUTINE BmwS_Initialize_wFlag(MD, IsBMW224)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwS), INTENT(INOUT)  :: MD           !! 'BmwS' object
    tLogical,    INTENT(IN)     :: IsBMW224
    !^ flag indicating whether the BMW-224 algorithm is employed or not. <br>
    !  - If true, use the BMW-224 algorithm. <br>
    !  - Otherwise, use the BMW-256 algorithm. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsBMW224 = IsBMW224
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE BmwS_Initialize_wFlag

!******************************************************************************

SUBROUTINE BmwS_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwS), INTENT(INOUT)  :: MD   !! 'BmwS' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    IF (MD%IsBMW224) THEN
        MD%H = IV224
    ELSE
        MD%H = IV256
    END IF
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE BmwS_Reset

!******************************************************************************

SUBROUTINE BmwS_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwS),                    INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(BmwS :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (BmwS)
        CALL Dst%Create(Src%IsBMW224)
        Dst%H      = Src%H
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE BmwS_GetClone

!******************************************************************************

FUNCTION BmwS_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwS), INTENT(IN) :: MD       !! 'BmwS' object
    tCharAlloc              :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsBMW224) THEN
        Name = 'BMW-224'
    ELSE
        Name = 'BMW-256'
    END IF

    RETURN

END FUNCTION BmwS_GetName

!******************************************************************************

FUNCTION BmwS_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwS), INTENT(IN) :: MD       !! 'BmwS' object
    tIndex                  :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsBMW224) THEN
        Length = DLen224
    ELSE
        Length = DLen256
    END IF

    RETURN

END FUNCTION BmwS_GetDigestLen

!******************************************************************************

FUNCTION BmwS_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwS), INTENT(IN) :: MD       !! 'BmwS' object
    tIndex                  :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION BmwS_GetBlockLen

!******************************************************************************

SUBROUTINE BmwS_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwS), TARGET, INTENT(INOUT)  :: MD           !! 'BmwS' object
    tByte,      POINTER, INTENT(INOUT)  :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE BmwS_SetBufPtr

!******************************************************************************

SUBROUTINE BmwS_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwS), INTENT(INOUT)  :: MD           !! 'BmwS' object
    tByte,       INTENT(IN)     :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: M(0:15)

! FLOW
    
    ! input block
    CALL BytePackLE(BytesIn, 0_kIndex, M)
    
    ! perform 16 rounds of mixing
    CALL Compress(MD%H, M)

    RETURN

END SUBROUTINE BmwS_ProcessBlock

!******************************************************************************

SUBROUTINE BmwS_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwS), INTENT(INOUT)  :: MD           !! 'BmwS' object
    tByte,       INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,      INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE BmwS_DoPadding

!******************************************************************************

SUBROUTINE BmwS_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(BmwS), INTENT(INOUT)  :: MD           !! 'BmwS' object
    tByte,       INTENT(IN)     :: LastByte     !! the last byte
    tByte,       INTENT(IN)     :: NBits        !! number of bits in the last byte
    tByte,       INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,      INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: H2(0:15)
    tIndex      :: Ptr, I, J, DLen
    tLong       :: BitLen
    tByte       :: Z

! FLOW

    ! padding
    ASSOCIATE(TmpBuf => MD%BufArr)   
        Ptr = MD%GetBufLen()
        BitLen = SHIFTL(ToInt64(MD%GetBlockCount()), 9) + SHIFTL(ToInt64(Ptr), 3)
        Z = SHIFTR(FByte80, NBits)
        TmpBuf(Ptr) = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
        Ptr = Ptr + 1_kIndex
        IF (Ptr > 56_kIndex) THEN
            TmpBuf(Ptr:63) = FByte00
            CALL MD%ProcessBlock(TmpBuf)
            Ptr = 0_kIndex
        END IF
        TmpBuf(Ptr:55) = FByte00
        CALL ByteUnpackLE(ToInt32(BitLen) + NBits, TmpBuf, 56_kIndex)
        CALL ByteUnpackLE(ToInt32(SHIFTR(BitLen, 32)), TmpBuf, 60_kIndex)
        CALL MD%ProcessBlock(TmpBuf)
    END ASSOCIATE

    ! finalizing
    H2 = MD%H
    MD%H = FINAL_PARAM
    CALL Compress(MD%H, H2)
    DLen = SHIFTR(MD%GetDigestLen(), 2)
    J = 16_kIndex - DLen
    DO I = 0_kIndex, DLen-1_kIndex
        CALL ByteUnpackLE(MD%H(J), BytesOut, Offset+I*4_kIndex)
        J = J + 1_kIndex
    END DO
        
    RETURN

END SUBROUTINE BmwS_AddBitsNPad

!******************************************************************************

SUBROUTINE Compress(H, M)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the compression of the given message.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: H(0:15)
    tInteger, INTENT(IN)    :: M(0:15)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Q(0:31)
    tInteger    :: Xl, Xh

!** SUBROUTINE MACRO DEFINITIONS:
#define SS0(x)      IEOR(IEOR(IEOR(SHIFTR(x, 1), SHIFTL(x, 3)), RotateLeft(x,  4)), RotateLeft(x, 19))
#define SS1(x)      IEOR(IEOR(IEOR(SHIFTR(x, 1), SHIFTL(x, 2)), RotateLeft(x,  8)), RotateLeft(x, 23))
#define SS2(x)      IEOR(IEOR(IEOR(SHIFTR(x, 2), SHIFTL(x, 1)), RotateLeft(x,  12)), RotateLeft(x, 25))
#define SS3(x)      IEOR(IEOR(IEOR(SHIFTR(x, 2), SHIFTL(x, 2)), RotateLeft(x,  15)), RotateLeft(x, 29))
#define SS4(x)      IEOR(SHIFTR(x, 1), x)
#define SS5(x)      IEOR(SHIFTR(x, 2), x)
#define RS1(x)      RotateLeft(x,  3)
#define RS2(x)      RotateLeft(x,  7)
#define RS3(x)      RotateLeft(x, 13)
#define RS4(x)      RotateLeft(x, 16)
#define RS5(x)      RotateLeft(x, 19)
#define RS6(x)      RotateLeft(x, 23)
#define RS7(x)      RotateLeft(x, 27)
#define KS(j)       (j * ToInt32(Z'05555555'))
#define Add_Elt_S(mf, hf, j0m, j1m, j3m, j4m, j7m, j10m, j11m, j16) \
    IEOR((RotateLeft(mf(j0m),  j1m)  + RotateLeft(mf(j3m), j4m) - \
          RotateLeft(mf(j10m), j11m) + KS(j16)), hf(j7m))
#define Expand1S_Inner(qf, mf, hf, i16, i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i0m, i1m, i3m, i4m, i7m, i10m, i11m) \
         (SS1(qf(i0))  + SS2(qf(i1))  + SS3(qf(i2))  + SS0(qf(i3)) \
        + SS1(qf(i4))  + SS2(qf(i5))  + SS3(qf(i6))  + SS0(qf(i7)) \
        + SS1(qf(i8))  + SS2(qf(i9))  + SS3(qf(i10)) + SS0(qf(i11)) \
        + SS1(qf(i12)) + SS2(qf(i13)) + SS3(qf(i14)) + SS0(qf(i15)) \
        + Add_Elt_S(mf, hf, i0m, i1m, i3m, i4m, i7m, i10m, i11m, i16))
#define Expand1S16(qf, mf, hf, i16)     Expand1S_Inner(qf, mf, hf, i16, 0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 0,  1,  3,  4,  7, 10, 11)
#define Expand1S17(qf, mf, hf, i16)     Expand1S_Inner(qf, mf, hf, i16, 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 1,  2,  4,  5,  8, 11, 12)
#define Expand2S_Inner(qf, mf, hf, i16, i0, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i0m, i1m, i3m, i4m, i7m, i10m, i11m) \
         (qf(i0)  + RS1(qf(i1))  + qf(i2)  + RS2(qf(i3)) \
        + qf(i4)  + RS3(qf(i5))  + qf(i6)  + RS4(qf(i7)) \
        + qf(i8)  + RS5(qf(i9))  + qf(i10) + RS6(qf(i11)) \
        + qf(i12) + RS7(qf(i13)) + SS4(qf(i14)) + SS5(qf(i15)) \
        + Add_Elt_S(mf, hf, i0m, i1m, i3m, i4m, i7m, i10m, i11m, i16))
#define Expand2S18(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 2,  3,  5,  6,  9, 12, 13)
#define Expand2S19(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 3,  4,  6,  7, 10, 13, 14)
#define Expand2S20(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 4,  5,  7,  8, 11, 14, 15)
#define Expand2S21(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 5,  6,  8,  9, 12, 15, 16)
#define Expand2S22(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 6,  7,  9, 10, 13,  0,  1)
#define Expand2S23(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 7,  8, 10, 11, 14,  1,  2)
#define Expand2S24(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 8,  9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 8,  9, 11, 12, 15,  2,  3)
#define Expand2S25(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 9, 10, 12, 13,  0,  3,  4)
#define Expand2S26(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 10, 11, 13, 14,  1,  4,  5)
#define Expand2S27(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 11, 12, 14, 15,  2,  5,  6)
#define Expand2S28(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 12, 13, 15, 16,  3,  6,  7)
#define Expand2S29(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 13, 14,  0,  1,  4,  7,  8)
#define Expand2S30(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 14, 15,  1,  2,  5,  8,  9)
#define Expand2S31(qf, mf, hf, i16)     Expand2S_Inner(qf, mf, hf, i16, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 15, 16,  2,  3,  6,  9, 10)
#define MAKE_W(i0, op01, i1, op12, i2, op23, i3, op34, i4) \
    (IEOR(M(i0), H(i0)) op01 IEOR(M(i1), H(i1)) op12 IEOR(M(i2), H(i2)) op23 IEOR(M(i3), H(i3)) op34 IEOR(M(i4), H(i4)))
#define Ws0             MAKE_W( 5, -,  7, +, 10, +, 13, +, 14)
#define Ws1             MAKE_W( 6, -,  8, +, 11, +, 14, -, 15)
#define Ws2             MAKE_W( 0, +,  7, +,  9, -, 12, +, 15)
#define Ws3             MAKE_W( 0, -,  1, +,  8, -, 10, +, 13)
#define Ws4             MAKE_W( 1, +,  2, +,  9, -, 11, -, 14)
#define Ws5             MAKE_W( 3, -,  2, +, 10, -, 12, +, 15)
#define Ws6             MAKE_W( 4, -,  0, -,  3, -, 11, +, 13)
#define Ws7             MAKE_W( 1, -,  4, -,  5, -, 12, -, 14)
#define Ws8             MAKE_W( 2, -,  5, -,  6, +, 13, -, 15)
#define Ws9             MAKE_W( 0, -,  3, +,  6, -,  7, +, 14)
#define Ws10            MAKE_W( 8, -,  1, -,  4, -,  7, +, 15)
#define Ws11            MAKE_W( 8, -,  0, -,  2, -,  5, +,  9)
#define Ws12            MAKE_W( 1, +,  3, -,  6, -,  9, +, 10)
#define Ws13            MAKE_W( 2, +,  4, +,  7, +, 10, +, 11)
#define Ws14            MAKE_W( 3, -,  5, +,  8, -, 11, -, 12)
#define Ws15            MAKE_W(12, -,  4, -,  6, -,  9, +, 13)
#define IEOR3(A, B, C)  IEOR(IEOR(A, B), C)

! FLOW

    Q( 0) = SS0(Ws0 ) + H( 1)
    Q( 1) = SS1(Ws1 ) + H( 2)
    Q( 2) = SS2(Ws2 ) + H( 3)
    Q( 3) = SS3(Ws3 ) + H( 4)
    Q( 4) = SS4(Ws4 ) + H( 5)
    Q( 5) = SS0(Ws5 ) + H( 6)
    Q( 6) = SS1(Ws6 ) + H( 7)
    Q( 7) = SS2(Ws7 ) + H( 8)
    Q( 8) = SS3(Ws8 ) + H( 9)
    Q( 9) = SS4(Ws9 ) + H(10)
    Q(10) = SS0(Ws10) + H(11)
    Q(11) = SS1(Ws11) + H(12)
    Q(12) = SS2(Ws12) + H(13)
    Q(13) = SS3(Ws13) + H(14)
    Q(14) = SS4(Ws14) + H(15)
    Q(15) = SS0(Ws15) + H( 0)
    
    Q(16) = Expand1S16(Q, M, H, 16)
    Q(17) = Expand1S17(Q, M, H, 17)
    Q(18) = Expand2S18(Q, M, H, 18)
    Q(19) = Expand2S19(Q, M, H, 19)
    Q(20) = Expand2S20(Q, M, H, 20)
    Q(21) = Expand2S21(Q, M, H, 21)
    Q(22) = Expand2S22(Q, M, H, 22)
    Q(23) = Expand2S23(Q, M, H, 23)
    Q(24) = Expand2S24(Q, M, H, 24)
    Q(25) = Expand2S25(Q, M, H, 25)
    Q(26) = Expand2S26(Q, M, H, 26)
    Q(27) = Expand2S27(Q, M, H, 27)
    Q(28) = Expand2S28(Q, M, H, 28)
    Q(29) = Expand2S29(Q, M, H, 29)
    Q(30) = Expand2S30(Q, M, H, 30)
    Q(31) = Expand2S31(Q, M, H, 31)

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

END MODULE MClass_BmwS
    
!******************************************************************************
