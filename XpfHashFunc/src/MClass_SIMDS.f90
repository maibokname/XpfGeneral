
MODULE MClass_SIMDS

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SIMDS* type and its related routines.
!   The *SIMDS* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *SIMDS* type implements an incremental cryptographic hash function
!   by employing either the *SIMD-224* or the *SIMD-256 message-digest*
!   algorithm [1].  The implementation here is based mainly on the *SPHLIB*
!   implementation [2].  <br>
!   By default, the *SIMDS* type employs the *SIMD-256 message-digest*
!   algorithm.  However, a user can specify the *IsSIMD224* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *SIMD-224 message-digest* algorithm
!   instead of the default one. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://who.rocq.inria.fr/Gaetan.Leurent/simd.html">
!       The SIMD Hash Function. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_BytePack,   ONLY: BytePackLE, ByteUnpackLE
    USE MClass_BaseDigest
    USE MClass_MDEngine

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: SIMDS

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
    tInteger, PARAMETER :: IV224(0:15) = [              &
        ToInt32(Z'33586E9F'), ToInt32(Z'12FFF033'), &
        ToInt32(Z'B2D9F64D'), ToInt32(Z'6F8FEA53'), &
        ToInt32(Z'DE943106'), ToInt32(Z'2742E439'), &
        ToInt32(Z'4FBAB5AC'), ToInt32(Z'62B9FF96'), &
        ToInt32(Z'22E7B0AF'), ToInt32(Z'C862B3A8'), &
        ToInt32(Z'33E00CDC'), ToInt32(Z'236B86A6'), &
        ToInt32(Z'F64AE77C'), ToInt32(Z'FA373B76'), &
        ToInt32(Z'7DC1EE5B'), ToInt32(Z'7FB29CE8')]
    tInteger, PARAMETER :: IV256(0:15) = [              &
        ToInt32(Z'4D567983'), ToInt32(Z'07190BA9'), &
        ToInt32(Z'8474577B'), ToInt32(Z'39D726E9'), &
        ToInt32(Z'AAF3D925'), ToInt32(Z'3EE20B03'), &
        ToInt32(Z'AFD5E751'), ToInt32(Z'C96006D3'), &
        ToInt32(Z'C2C2BA14'), ToInt32(Z'49B3BCB4'), &
        ToInt32(Z'F67CAF46'), ToInt32(Z'668626C9'), &
        ToInt32(Z'E2EAA8D2'), ToInt32(Z'1FF47833'), &
        ToInt32(Z'D0C661A5'), ToInt32(Z'55693DE1')]
    tInteger, PARAMETER, PUBLIC :: MaskI8  = ToInt32(Z'000000FF')
    tInteger, PARAMETER, PUBLIC :: MaskI16 = ToInt32(Z'0000FFFF')
    tInteger, PARAMETER, PUBLIC :: AlphaTab(0:255) = [                      &
          1,  41, 139,  45,  46,  87, 226,  14,  60, 147, 116, 130, &
        190,  80, 196,  69,   2,  82,  21,  90,  92, 174, 195,  28, &
        120,  37, 232,   3, 123, 160, 135, 138,   4, 164,  42, 180, &
        184,  91, 133,  56, 240,  74, 207,   6, 246,  63,  13,  19, &
          8,  71,  84, 103, 111, 182,   9, 112, 223, 148, 157,  12, &
        235, 126,  26,  38,  16, 142, 168, 206, 222, 107,  18, 224, &
        189,  39,  57,  24, 213, 252,  52,  76,  32,  27,  79, 155, &
        187, 214,  36, 191, 121,  78, 114,  48, 169, 247, 104, 152, &
         64,  54, 158,  53, 117, 171,  72, 125, 242, 156, 228,  96, &
         81, 237, 208,  47, 128, 108,  59, 106, 234,  85, 144, 250, &
        227,  55, 199, 192, 162, 217, 159,  94, 256, 216, 118, 212, &
        211, 170,  31, 243, 197, 110, 141, 127,  67, 177,  61, 188, &
        255, 175, 236, 167, 165,  83,  62, 229, 137, 220,  25, 254, &
        134,  97, 122, 119, 253,  93, 215,  77,  73, 166, 124, 201, &
         17, 183,  50, 251,  11, 194, 244, 238, 249, 186, 173, 154, &
        146,  75, 248, 145,  34, 109, 100, 245,  22, 131, 231, 219, &
        241, 115,  89,  51,  35, 150, 239,  33,  68, 218, 200, 233, &
         44,   5, 205, 181, 225, 230, 178, 102,  70,  43, 221,  66, &
        136, 179, 143, 209,  88,  10, 153, 105, 193, 203,  99, 204, &
        140,  86, 185, 132,  15, 101,  29, 161, 176,  20,  49, 210, &
        129, 149, 198, 151,  23, 172, 113,   7,  30, 202,  58,  65, &
         95,  40,  98, 163]

!** DERIVED TYPE DEFINITIONS
    !> *SIMDS* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing either
    !  the *SIMD-224* or the *SIMD-256 message-digest* algorithm.
    TYPE, EXTENDS(MDEngine) :: SIMDS
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state variable
        tInteger    :: State(0:15) = IV256(0:15)
        !% flag indicating whether the SIMD-224 algorithm is employed or not.
        tLogical    :: IsSIMD224 = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                    Private Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => SIMDS_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (SIMD-256).
        PROCEDURE       :: Initialize   => SIMDS_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => SIMDS_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => SIMDS_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => SIMDS_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => SIMDS_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => SIMDS_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => SIMDS_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => SIMDS_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => SIMDS_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => SIMDS_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (SIMD-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the SIMD-224 algorithm <br>
        !   --->    CALL MD%Create(IsSIMD224=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
        ! ---------------------------------------------------------------------
    END TYPE SIMDS

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE SIMDS_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDS), INTENT(INOUT) :: MD    !! 'SIMDS' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the SIMD-256 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE SIMDS_Initialize

!******************************************************************************

SUBROUTINE SIMDS_Initialize_wFlag(MD, IsSIMD224)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDS), INTENT(INOUT) :: MD           !! 'SIMDS' object
    tLogical,     INTENT(IN)    :: IsSIMD224
    !^ flag indicating whether the SIMD-224 algorithm is employed or not. <br>
    !  - If true, use the SIMD-224 algorithm. <br>
    !  - Otherwise, use the SIMD-256 algorithm. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsSIMD224 = IsSIMD224
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE SIMDS_Initialize_wFlag

!******************************************************************************

SUBROUTINE SIMDS_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDS), INTENT(INOUT) :: MD   !! 'SIMDS' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    IF (MD%IsSIMD224) THEN
        MD%State = IV224
    ELSE
        MD%State = IV256
    END IF
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE SIMDS_Reset

!******************************************************************************

SUBROUTINE SIMDS_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDS),                   INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(SIMDS :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (SIMDS)
        CALL Dst%Create(Src%IsSIMD224)
        Dst%State  = Src%State
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE SIMDS_GetClone

!******************************************************************************

FUNCTION SIMDS_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDS), INTENT(IN)    :: MD       !! 'SIMDS' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSIMD224) THEN
        Name = 'SIMD-224'
    ELSE
        Name = 'SIMD-256'
    END IF

    RETURN

END FUNCTION SIMDS_GetName

!******************************************************************************

FUNCTION SIMDS_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDS), INTENT(IN)    :: MD       !! 'SIMDS' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSIMD224) THEN
        Length = DLen224
    ELSE
        Length = DLen256
    END IF

    RETURN

END FUNCTION SIMDS_GetDigestLen

!******************************************************************************

FUNCTION SIMDS_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDS), INTENT(IN)    :: MD       !! 'SIMDS' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION SIMDS_GetBlockLen

!******************************************************************************

SUBROUTINE SIMDS_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDS), TARGET, INTENT(INOUT) :: MD           !! 'SIMDS' object
    tByte,       POINTER, INTENT(INOUT) :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE SIMDS_SetBufPtr

!******************************************************************************

SUBROUTINE SIMDS_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDS), INTENT(INOUT) :: MD           !! 'SIMDS' object
    tByte,        INTENT(IN)    :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL SIMDCore_Compress_Small(MD, BytesIn, FalseVal)

    RETURN

END SUBROUTINE SIMDS_ProcessBlock

!******************************************************************************

SUBROUTINE SIMDS_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDS), INTENT(INOUT) :: MD           !! 'SIMDS' object
    tByte,        INTENT(INOUT) :: BytesOut(0:) !! the output buffer
    tIndex,       INTENT(IN)    :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE SIMDS_DoPadding

!******************************************************************************

SUBROUTINE SIMDS_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDS), INTENT(INOUT) :: MD           !! 'SIMDS' object
    tByte,        INTENT(IN)    :: LastByte     !! the last byte
    tByte,        INTENT(IN)    :: NBits        !! number of bits in the last byte
    tByte,        INTENT(INOUT) :: BytesOut(0:) !! the output buffer
    tIndex,       INTENT(IN)    :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Ptr, I, DLen
    tLong       :: BlockCount

! FLOW

    ASSOCIATE(TmpBuf => MD%BufArr)
        Ptr  = MD%GetBufLen()
        IF ((Ptr > 0_kIndex).OR.(NBits > 0_kInt8)) THEN
            TmpBuf(Ptr) = IAND(LastByte, SHIFTL(FByteFF, 8-NBits))
            TmpBuf(Ptr+1:) = FByte00
            CALL MD%ProcessBlock(TmpBuf)
        END IF
        ! encode counter and compress buffer
        BlockCount = SHIFTL(MD%GetBlockCount(), 9) + ToInt64(SHIFTL(Ptr, 3)) + ToInt64(NBits)
        CALL ByteUnpackLE(ToInt32(BlockCount), TmpBuf, 0_kIndex)
        CALL ByteUnpackLE(ToInt32(SHIFTA(BlockCount, 32)), TmpBuf, 4_kIndex)
        TmpBuf(8:) = FByte00
        CALL SIMDCore_Compress_Small(MD, TmpBuf, TrueVal)
    END ASSOCIATE

    DLen = MD%GetDigestLen()
    DO I = 0, SHIFTR(DLen, 2)-1
        CALL ByteUnpackLE(MD%State(I), BytesOut, Offset + SHIFTL(I, 2))
    END DO

    RETURN

END SUBROUTINE SIMDS_AddBitsNPad

!******************************************************************************

SUBROUTINE SIMDCore_Compress_Small(MD, X, Last)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform compression based on a Feistel-like cipher in Davies-Meyer mode.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDS), INTENT(INOUT) :: MD
    tByte,        INTENT(IN)    :: X(0:)
    tLogical,     INTENT(IN)    :: Last

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   PARAMETER :: Wsp(0:31) = [                                                        &
        SHIFTL( 4_kIndex, 3), SHIFTL( 6_kIndex, 3), SHIFTL( 0_kIndex, 3), SHIFTL( 2_kIndex, 3), &
        SHIFTL( 7_kIndex, 3), SHIFTL( 5_kIndex, 3), SHIFTL( 3_kIndex, 3), SHIFTL( 1_kIndex, 3), &
        SHIFTL(15_kIndex, 3), SHIFTL(11_kIndex, 3), SHIFTL(12_kIndex, 3), SHIFTL( 8_kIndex, 3), &
        SHIFTL( 9_kIndex, 3), SHIFTL(13_kIndex, 3), SHIFTL(10_kIndex, 3), SHIFTL(14_kIndex, 3), &
        SHIFTL(17_kIndex, 3), SHIFTL(18_kIndex, 3), SHIFTL(23_kIndex, 3), SHIFTL(20_kIndex, 3), &
        SHIFTL(22_kIndex, 3), SHIFTL(21_kIndex, 3), SHIFTL(16_kIndex, 3), SHIFTL(19_kIndex, 3), &
        SHIFTL(30_kIndex, 3), SHIFTL(24_kIndex, 3), SHIFTL(25_kIndex, 3), SHIFTL(31_kIndex, 3), &
        SHIFTL(27_kIndex, 3), SHIFTL(29_kIndex, 3), SHIFTL(28_kIndex, 3), SHIFTL(26_kIndex, 3)]
    tInteger, PARAMETER :: YOff_S_N(0:127) = [                      &
          1,  98,  95,  58,  30, 113,  23, 198, 129,  49, 176,  29, &
         15, 185, 140,  99, 193, 153,  88, 143, 136, 221,  70, 178, &
        225, 205,  44, 200,  68, 239,  35,  89, 241, 231,  22, 100, &
         34, 248, 146, 173, 249, 244,  11,  50,  17, 124,  73, 215, &
        253, 122, 134,  25, 137,  62, 165, 236, 255,  61,  67, 141, &
        197,  31, 211, 118, 256, 159, 162, 199, 227, 144, 234,  59, &
        128, 208,  81, 228, 242,  72, 117, 158,  64, 104, 169, 114, &
        121,  36, 187,  79,  32,  52, 213,  57, 189,  18, 222, 168, &
         16,  26, 235, 157, 223,   9, 111,  84,   8,  13, 246, 207, &
        240, 133, 184,  42,   4, 135, 123, 232, 120, 195,  92,  21, &
          2, 196, 190, 116,  60, 226,  46, 139]
    tInteger, PARAMETER :: YOff_S_F(0:127) = [                      &
          2, 156, 118, 107,  45, 212, 111, 162,  97, 249, 211,   3, &
         49, 101, 151, 223, 189, 178, 253, 204,  76,  82, 232,  65, &
         96, 176, 161,  47, 189,  61, 248, 107,   0, 131, 133, 113, &
         17,  33,  12, 111, 251, 103,  57, 148,  47,  65, 249, 143, &
        189,   8, 204, 230, 205, 151, 187, 227, 247, 111, 140,   6, &
         77,  10,  21, 149, 255, 101, 139, 150, 212,  45, 146,  95, &
        160,   8,  46, 254, 208, 156, 106,  34,  68,  79,   4,  53, &
        181, 175,  25, 192, 161,  81,  96, 210,  68, 196,   9, 150, &
          0, 126, 124, 144, 240, 224, 245, 146,   6, 154, 200, 109, &
        210, 192,   8, 114,  68, 249,  53,  27,  52, 106,  70,  30, &
         10, 146, 117, 251, 180, 247, 236, 108]
    tInteger, PARAMETER :: PP4_0(0:3) = [1, 0, 3, 2]
    tInteger, PARAMETER :: PP4_1(0:3) = [2, 3, 0, 1]
    tInteger, PARAMETER :: PP4_2(0:3) = [3, 2, 1, 0]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger, TARGET    :: State(0:15)
    tInteger, POINTER   :: A(:), B(:), C(:), D(:)
    tInteger            :: Q(0:127)
    tInteger            :: W(0:31)
    tInteger            :: tA(0:3)
    tInteger            :: TQ, MWord, Tmp
    tIndex              :: II, JJ, KK

!** SUBROUTINE MACRO DEFINITIONS:
#include    "Includes/SIMD_Def Macro.f90"

! FLOW

    ! set pointers
    A(0:3) => State(0:)
    B(0:3) => State(4:)
    C(0:3) => State(8:)
    D(0:3) => State(12:)

    ! ++++++++ FFT_128(0, 1, 0) +++++++++
    CALL FFT_32(X(0:), SHIFTL(1_kIndex, 2), Q(0:))
    CALL FFT_32(X(2:), SHIFTL(1_kIndex, 2), Q(32:))
    FFT_LOOP(0, 32, 4)
    CALL FFT_32(X(1:), SHIFTL(1_kIndex, 2), Q(64:))
    CALL FFT_32(X(3:), SHIFTL(1_kIndex, 2), Q(96:))
    FFT_LOOP(64, 32, 4)
    FFT_LOOP(0, 64, 2)
    ! +++++++++++++++++++++++++++++++++++

    IF (Last) THEN
        DO II = 0, 127
            TQ = Q(II) + YOff_S_F(II)
            TQ = REDS2(TQ)
            TQ = REDS1(TQ)
            TQ = REDS1(TQ)
            IF (TQ <= 128) THEN
                Q(II) = TQ
            ELSE
                Q(II) = TQ - 257
            END IF
        END DO
    ELSE
        DO II = 0, 127
            TQ = Q(II) + YOff_S_N(II)
            TQ = REDS2(TQ)
            TQ = REDS1(TQ)
            TQ = REDS1(TQ)
            IF (TQ <= 128) THEN
                Q(II) = TQ
            ELSE
                Q(II) = TQ - 257
            END IF
        END DO
    END IF

    State = MD%State
    DO II = 0, 15, 4
        DO JJ = II, II+3
            KK = 4*JJ
            CALL BytePackLE(X, KK, MWord)
            State(JJ) = IEOR(State(JJ), MWord)
        END DO
    END DO

    WSREAD( 0,    0,    1, 185)
    CALL OneRound_Small(State, W, 0,  3, 23, 17, 27)
    WSREAD( 8,    0,    1, 185)
    CALL OneRound_Small(State, W, 2, 28, 19, 22,  7)
    WSREAD(16, (-128),  (-64), 233)
    CALL OneRound_Small(State, W, 1, 29,  9, 15,  5)
    WSREAD(24, (-191), (-127), 233)
    CALL OneRound_Small(State, W, 0,  4, 13, 10, 25)

    STEP_SMALL_II(MD%State( 0), MD%State( 1), MD%State( 2), MD%State( 3), IFF,  4, 13, PP4_2)
    STEP_SMALL_II(MD%State( 4), MD%State( 5), MD%State( 6), MD%State( 7), IFF, 13, 10, PP4_0)
    STEP_SMALL_II(MD%State( 8), MD%State( 9), MD%State(10), MD%State(11), IFF, 10, 25, PP4_1)
    STEP_SMALL_II(MD%State(12), MD%State(13), MD%State(14), MD%State(15), IFF, 25,  4, PP4_2)

    ! return state values
    MD%State = State

    NULLIFY(A, B, C, D)

    RETURN

CONTAINS

    SUBROUTINE FFT_32(X, XS, Q)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: X(0:)
        tIndex,   INTENT(IN)    :: XS
        tInteger, INTENT(INOUT) :: Q(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex                  :: XD

    ! FLOW

        XD = SHIFTL(XS, 1)
        FFT16(0, XD, 0)
        FFT16(XS, XD, 16)
        FFT_LOOP(0, 16, 8)

        RETURN

    END SUBROUTINE FFT_32

    !******************************************************************************

    SUBROUTINE OneRound_Small(State, W, ISP, P0, P1, P2, P3)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform one round of mixing.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, TARGET, INTENT(INOUT) :: State(0:15)
        tInteger,         INTENT(INOUT) :: W(0:31)
        tInteger,         INTENT(IN)    :: ISP, P0, P1, P2, P3

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, PARAMETER :: PP4K(0:10) = [1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2]

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger, POINTER   :: A(:), B(:), C(:), D(:)
        tInteger            :: tA(0:3), Tmp

    ! FLOW

        A(0:3) => State(0:)
        B(0:3) => State(4:)
        C(0:3) => State(8:)
        D(0:3) => State(12:)

        STEP_SMALL(W( 0), W( 1), W( 2), W( 3), IFF, P0, P1, PP4K(ISP + 0))
        STEP_SMALL(W( 4), W( 5), W( 6), W( 7), IFF, P1, P2, PP4K(ISP + 1))
        STEP_SMALL(W( 8), W( 9), W(10), W(11), IFF, P2, P3, PP4K(ISP + 2))
        STEP_SMALL(W(12), W(13), W(14), W(15), IFF, P3, P0, PP4K(ISP + 3))
        STEP_SMALL(W(16), W(17), W(18), W(19), MAJ, P0, P1, PP4K(ISP + 4))
        STEP_SMALL(W(20), W(21), W(22), W(23), MAJ, P1, P2, PP4K(ISP + 5))
        STEP_SMALL(W(24), W(25), W(26), W(27), MAJ, P2, P3, PP4K(ISP + 6))
        STEP_SMALL(W(28), W(29), W(30), W(31), MAJ, P3, P0, PP4K(ISP + 7))

        NULLIFY(A, B, C, D)

        RETURN

    END SUBROUTINE OneRound_Small

    !******************************************************************************

#include    "Includes/SIMD_Undef Macro.f90"

END SUBROUTINE SIMDCore_Compress_Small

!******************************************************************************

END MODULE MClass_SIMDS
    
!******************************************************************************
