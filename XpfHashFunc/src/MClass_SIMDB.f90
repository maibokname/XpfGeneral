
MODULE MClass_SIMDB

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SIMDB* type and its related routines.
!   The *SIMDB* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *SIMDB* type implements an incremental cryptographic hash function
!   by employing either the *SIMD-384* or the *SIMD-512 message-digest*
!   algorithm [1].  The implementation here is based mainly on the *SPHLIB*
!   implementation [2].  <br>
!   By default, the *SIMDB* type employs the *SIMD-512 message-digest*
!   algorithm.  However, a user can specify the *IsSIMD384* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *SIMD-384 message-digest* algorithm
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
    USE MClass_SIMDS,        ONLY: MaskI8, MaskI16, AlphaTab

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: SIMDB

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tIndex,   PARAMETER :: BlockLen = 128_kIndex
    tIndex,   PARAMETER :: DLen384  = 48_kIndex
    tIndex,   PARAMETER :: DLen512  = 64_kIndex
    tInteger, PARAMETER :: IV384(0:31) = [              &
        ToInt32(Z'8A36EEBC'), ToInt32(Z'94A3BD90'), &
        ToInt32(Z'D1537B83'), ToInt32(Z'B25B070B'), &
        ToInt32(Z'F463F1B5'), ToInt32(Z'B6F81E20'), &
        ToInt32(Z'0055C339'), ToInt32(Z'B4D144D1'), &
        ToInt32(Z'7360CA61'), ToInt32(Z'18361A03'), &
        ToInt32(Z'17DCB4B9'), ToInt32(Z'3414C45A'), &
        ToInt32(Z'A699A9D2'), ToInt32(Z'E39E9664'), &
        ToInt32(Z'468BFE77'), ToInt32(Z'51D062F8'), &
        ToInt32(Z'B9E3BFE8'), ToInt32(Z'63BECE2A'), &
        ToInt32(Z'8FE506B9'), ToInt32(Z'F8CC4AC2'), &
        ToInt32(Z'7AE11542'), ToInt32(Z'B1AADDA1'), &
        ToInt32(Z'64B06794'), ToInt32(Z'28D2F462'), &
        ToInt32(Z'E64071EC'), ToInt32(Z'1DEB91A8'), &
        ToInt32(Z'8AC8DB23'), ToInt32(Z'3F782AB5'), &
        ToInt32(Z'039B5CB8'), ToInt32(Z'71DDD962'), &
        ToInt32(Z'FADE2CEA'), ToInt32(Z'1416DF71')]
    tInteger, PARAMETER :: IV512(0:31) = [              &
        ToInt32(Z'0BA16B95'), ToInt32(Z'72F999AD'), &
        ToInt32(Z'9FECC2AE'), ToInt32(Z'BA3264FC'), &
        ToInt32(Z'5E894929'), ToInt32(Z'8E9F30E5'), &
        ToInt32(Z'2F1DAA37'), ToInt32(Z'F0F2C558'), &
        ToInt32(Z'AC506643'), ToInt32(Z'A90635A5'), &
        ToInt32(Z'E25B878B'), ToInt32(Z'AAB7878F'), &
        ToInt32(Z'88817F7A'), ToInt32(Z'0A02892B'), &
        ToInt32(Z'559A7550'), ToInt32(Z'598F657E'), &
        ToInt32(Z'7EEF60A1'), ToInt32(Z'6B70E3E8'), &
        ToInt32(Z'9C1714D1'), ToInt32(Z'B958E2A8'), &
        ToInt32(Z'AB02675E'), ToInt32(Z'ED1C014F'), &
        ToInt32(Z'CD8D65BB'), ToInt32(Z'FDB7A257'), &
        ToInt32(Z'09254899'), ToInt32(Z'D699C7BC'), &
        ToInt32(Z'9019B6DC'), ToInt32(Z'2B9022E4'), &
        ToInt32(Z'8FA14956'), ToInt32(Z'21BF9BD3'), &
        ToInt32(Z'B94D0943'), ToInt32(Z'6FFDDC22')]

!** DERIVED TYPE DEFINITIONS
    !> *SIMDB* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing either
    !  the *SIMD-384* or the *SIMD-512 message-digest* algorithm.
    TYPE, EXTENDS(MDEngine) :: SIMDB
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state variable
        tInteger    :: State(0:31) = IV512(0:31)
        !% flag indicating whether the SIMD-384 algorithm is employed or not.
        tLogical    :: IsSIMD384 = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                    Private Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => SIMDB_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (SIMD-512).
        PROCEDURE       :: Initialize   => SIMDB_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => SIMDB_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => SIMDB_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => SIMDB_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => SIMDB_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => SIMDB_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => SIMDB_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => SIMDB_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => SIMDB_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => SIMDB_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (SIMD-512) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the SIMD-384 algorithm <br>
        !   --->    CALL MD%Create(IsSIMD384=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
        ! ---------------------------------------------------------------------
    END TYPE SIMDB

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE SIMDB_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDB), INTENT(INOUT) :: MD    !! 'SIMDB' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the SIMD-512 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE SIMDB_Initialize

!******************************************************************************

SUBROUTINE SIMDB_Initialize_wFlag(MD, IsSIMD384)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDB), INTENT(INOUT) :: MD           !! 'SIMDB' object
    tLogical,     INTENT(IN)    :: IsSIMD384
    !^ flag indicating whether the SIMD-384 algorithm is employed or not. <br>
    !  - If true, use the SIMD-384 algorithm. <br>
    !  - Otherwise, use the SIMD-512 algorithm. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsSIMD384 = IsSIMD384
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE SIMDB_Initialize_wFlag

!******************************************************************************

SUBROUTINE SIMDB_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDB), INTENT(INOUT) :: MD   !! 'SIMDB' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    IF (MD%IsSIMD384) THEN
        MD%State = IV384
    ELSE
        MD%State = IV512
    END IF
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE SIMDB_Reset

!******************************************************************************

SUBROUTINE SIMDB_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDB),                   INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(SIMDB :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (SIMDB)
        CALL Dst%Create(Src%IsSIMD384)
        Dst%State  = Src%State
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE SIMDB_GetClone

!******************************************************************************

FUNCTION SIMDB_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDB), INTENT(IN)    :: MD       !! 'SIMDB' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSIMD384) THEN
        Name = 'SIMD-384'
    ELSE
        Name = 'SIMD-512'
    END IF

    RETURN

END FUNCTION SIMDB_GetName

!******************************************************************************

FUNCTION SIMDB_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDB), INTENT(IN)    :: MD       !! 'SIMDB' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSIMD384) THEN
        Length = DLen384
    ELSE
        Length = DLen512
    END IF

    RETURN

END FUNCTION SIMDB_GetDigestLen

!******************************************************************************

FUNCTION SIMDB_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDB), INTENT(IN)    :: MD       !! 'SIMDB' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION SIMDB_GetBlockLen

!******************************************************************************

SUBROUTINE SIMDB_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDB), TARGET, INTENT(INOUT) :: MD           !! 'SIMDB' object
    tByte,       POINTER, INTENT(INOUT) :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE SIMDB_SetBufPtr

!******************************************************************************

SUBROUTINE SIMDB_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDB), INTENT(INOUT) :: MD           !! 'SIMDB' object
    tByte,        INTENT(IN)    :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL SIMDCore_Compress_Big(MD, BytesIn, FalseVal)

    RETURN

END SUBROUTINE SIMDB_ProcessBlock

!******************************************************************************

SUBROUTINE SIMDB_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDB), INTENT(INOUT) :: MD           !! 'SIMDB' object
    tByte,        INTENT(INOUT) :: BytesOut(0:) !! the output buffer
    tIndex,       INTENT(IN)    :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE SIMDB_DoPadding

!******************************************************************************

SUBROUTINE SIMDB_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDB), INTENT(INOUT) :: MD           !! 'SIMDB' object
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
        BlockCount = SHIFTL(MD%GetBlockCount(), 10) + ToInt64(SHIFTL(Ptr, 3)) + ToInt64(NBits)
        CALL ByteUnpackLE(ToInt32(BlockCount), TmpBuf, 0_kIndex)
        CALL ByteUnpackLE(ToInt32(SHIFTA(BlockCount, 32)), TmpBuf, 4_kIndex)
        TmpBuf(8:) = FByte00
        CALL SIMDCore_Compress_Big(MD, TmpBuf, TrueVal)
    END ASSOCIATE

    DLen = MD%GetDigestLen()
    DO I = 0, SHIFTR(DLen, 2)-1
        CALL ByteUnpackLE(MD%State(I), BytesOut, Offset + SHIFTL(I, 2))
    END DO

    RETURN

END SUBROUTINE SIMDB_AddBitsNPad

!******************************************************************************

SUBROUTINE SIMDCore_Compress_Big(MD, X, Last)

!** PURPOSE OF THIS SUBROUTINE:
    ! To perform compression based on a Feistel-like cipher in Davies-Meyer mode.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SIMDB), INTENT(INOUT) :: MD
    tByte,        INTENT(IN)    :: X(0:)
    tLogical,     INTENT(IN)    :: Last

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,   PARAMETER :: Wbp(0:31) = [                                                        &
        SHIFTL( 4_kIndex, 4), SHIFTL( 6_kIndex, 4), SHIFTL( 0_kIndex, 4), SHIFTL( 2_kIndex, 4), &
        SHIFTL( 7_kIndex, 4), SHIFTL( 5_kIndex, 4), SHIFTL( 3_kIndex, 4), SHIFTL( 1_kIndex, 4), &
        SHIFTL(15_kIndex, 4), SHIFTL(11_kIndex, 4), SHIFTL(12_kIndex, 4), SHIFTL( 8_kIndex, 4), &
        SHIFTL( 9_kIndex, 4), SHIFTL(13_kIndex, 4), SHIFTL(10_kIndex, 4), SHIFTL(14_kIndex, 4), &
        SHIFTL(17_kIndex, 4), SHIFTL(18_kIndex, 4), SHIFTL(23_kIndex, 4), SHIFTL(20_kIndex, 4), &
        SHIFTL(22_kIndex, 4), SHIFTL(21_kIndex, 4), SHIFTL(16_kIndex, 4), SHIFTL(19_kIndex, 4), &
        SHIFTL(30_kIndex, 4), SHIFTL(24_kIndex, 4), SHIFTL(25_kIndex, 4), SHIFTL(31_kIndex, 4), &
        SHIFTL(27_kIndex, 4), SHIFTL(29_kIndex, 4), SHIFTL(28_kIndex, 4), SHIFTL(26_kIndex, 4)]
    tInteger, PARAMETER :: YOff_B_N(0:255) = [                      &
          1, 163,  98,  40,  95,  65,  58, 202,  30,   7, 113, 172, &
         23, 151, 198, 149, 129, 210,  49,  20, 176, 161,  29, 101, &
         15, 132, 185,  86, 140, 204,  99, 203, 193, 105, 153,  10, &
         88, 209, 143, 179, 136,  66, 221,  43,  70, 102, 178, 230, &
        225, 181, 205,   5,  44, 233, 200, 218,  68,  33, 239, 150, &
         35,  51,  89, 115, 241, 219, 231, 131,  22, 245, 100, 109, &
         34, 145, 248,  75, 146, 154, 173, 186, 249, 238, 244, 194, &
         11, 251,  50, 183,  17, 201, 124, 166,  73,  77, 215,  93, &
        253, 119, 122,  97, 134, 254,  25, 220, 137, 229,  62,  83, &
        165, 167, 236, 175, 255, 188,  61, 177,  67, 127, 141, 110, &
        197, 243,  31, 170, 211, 212, 118, 216, 256,  94, 159, 217, &
        162, 192, 199,  55, 227, 250, 144,  85, 234, 106,  59, 108, &
        128,  47, 208, 237,  81,  96, 228, 156, 242, 125,  72, 171, &
        117,  53, 158,  54,  64, 152, 104, 247, 169,  48, 114,  78, &
        121, 191,  36, 214, 187, 155,  79,  27,  32,  76,  52, 252, &
        213,  24,  57,  39, 189, 224,  18, 107, 222, 206, 168, 142, &
         16,  38,  26, 126, 235,  12, 157, 148, 223, 112,   9, 182, &
        111, 103,  84,  71,   8,  19,  13,  63, 246,   6, 207,  74, &
        240,  56, 133,  91, 184, 180,  42, 164,   4, 138, 135, 160, &
        123,   3, 232,  37, 120,  28, 195, 174,  92,  90,  21,  82, &
          2,  69, 196,  80, 190, 130, 116, 147,  60,  14, 226,  87, &
         46,  45, 139,  41]
    tInteger, PARAMETER :: YOff_B_F(0:255) = [                      &
          2, 203, 156,  47, 118, 214, 107, 106,  45,  93, 212,  20, &
        111,  73, 162, 251,  97, 215, 249,  53, 211,  19,   3,  89, &
         49, 207, 101,  67, 151, 130, 223,  23, 189, 202, 178, 239, &
        253, 127, 204,  49,  76, 236,  82, 137, 232, 157,  65,  79, &
         96, 161, 176, 130, 161,  30,  47,   9, 189, 247,  61, 226, &
        248,  90, 107,  64,   0,  88, 131, 243, 133,  59, 113, 115, &
         17, 236,  33, 213,  12, 191, 111,  19, 251,  61, 103, 208, &
         57,  35, 148, 248,  47, 116,  65, 119, 249, 178, 143,  40, &
        189, 129,   8, 163, 204, 227, 230, 196, 205, 122, 151,  45, &
        187,  19, 227,  72, 247, 125, 111, 121, 140, 220,   6, 107, &
         77,  69,  10, 101,  21,  65, 149, 171, 255,  54, 101, 210, &
        139,  43, 150, 151, 212, 164,  45, 237, 146, 184,  95,   6, &
        160,  42,   8, 204,  46, 238, 254, 168, 208,  50, 156, 190, &
        106, 127,  34, 234,  68,  55,  79,  18,   4, 130,  53, 208, &
        181,  21, 175, 120,  25, 100, 192, 178, 161,  96,  81, 127, &
         96, 227, 210, 248,  68,  10, 196,  31,   9, 167, 150, 193, &
          0, 169, 126,  14, 124, 198, 144, 142, 240,  21, 224,  44, &
        245,  66, 146, 238,   6, 196, 154,  49, 200, 222, 109,   9, &
        210, 141, 192, 138,   8,  79, 114, 217,  68, 128, 249,  94, &
         53,  30,  27,  61,  52, 135, 106, 212,  70, 238,  30, 185, &
         10, 132, 146, 136, 117,  37, 251, 150, 180, 188, 247, 156, &
        236, 192, 108,  86]
    tInteger, PARAMETER :: PP8_0(0:7) = [1, 0, 3, 2, 5, 4, 7, 6]
    tInteger, PARAMETER :: PP8_4(0:7) = [5, 4, 7, 6, 1, 0, 3, 2]
    tInteger, PARAMETER :: PP8_5(0:7) = [7, 6, 5, 4, 3, 2, 1, 0]
    tInteger, PARAMETER :: PP8_6(0:7) = [4, 5, 6, 7, 0, 1, 2, 3]

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger, TARGET    :: State(0:31)
    tInteger, POINTER   :: A(:), B(:), C(:), D(:)
    tInteger            :: Q(0:255)
    tInteger            :: W(0:63)
    tInteger            :: tA(0:7)
    tInteger            :: TQ, MWord, Tmp
    tIndex              :: II, JJ, KK

!** SUBROUTINE MACRO DEFINITIONS:
#include    "Includes/SIMD_Def Macro.f90"

! FLOW

    ! set pointers
    A(0:7) => State(0:)
    B(0:7) => State(8:)
    C(0:7) => State(16:)
    D(0:7) => State(24:)

    ! ++++++++ FFT_256(0, 1, 0) +++++++++
    CALL FFT_64(X(0:), SHIFTL(1_kIndex, 2), Q(0:))
    CALL FFT_64(X(2:), SHIFTL(1_kIndex, 2), Q(64:))
    FFT_LOOP(0,   64, 2)
    CALL FFT_64(X(1:), SHIFTL(1_kIndex, 2), Q(128:))
    CALL FFT_64(X(3:), SHIFTL(1_kIndex, 2), Q(192:))
    FFT_LOOP(128, 64, 2)
    FFT_LOOP(0,  128, 1)
    ! +++++++++++++++++++++++++++++++++++

    IF (Last) THEN
        DO II = 0, 255
            TQ = Q(II) + YOff_B_F(II)
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
        DO II = 0, 255
            TQ = Q(II) + YOff_B_N(II)
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
    DO II = 0, 31, 8
        DO JJ = II, II+7
            KK = 4*JJ
            CALL BytePackLE(X, KK, MWord)
            State(JJ) = IEOR(State(JJ), MWord)
        END DO
    END DO

    WBREAD( 0,    0,    1, 185)
    CALL OneRound_Big(State, W, 0,  3, 23, 17, 27)
    WBREAD( 8,    0,    1, 185)
    CALL OneRound_Big(State, W, 1, 28, 19, 22,  7)
    WBREAD(16, (-256), (-128), 233)
    CALL OneRound_Big(State, W, 2, 29,  9, 15,  5)
    WBREAD(24, (-383), (-255), 233)
    CALL OneRound_Big(State, W, 3,  4, 13, 10, 25)

    STEP_BIG_II(MD%State( 0), MD%State( 1), MD%State( 2), MD%State( 3), \
                MD%State( 4), MD%State( 5), MD%State( 6), MD%State( 7), \
                IFF,  4, 13, PP8_4)
    STEP_BIG_II(MD%State( 8), MD%State( 9), MD%State(10), MD%State(11), \
                MD%State(12), MD%State(13), MD%State(14), MD%State(15), \
                IFF, 13, 10, PP8_5)
    STEP_BIG_II(MD%State(16), MD%State(17), MD%State(18), MD%State(19), \
                MD%State(20), MD%State(21), MD%State(22), MD%State(23), \
                IFF, 10, 25, PP8_6)
    STEP_BIG_II(MD%State(24), MD%State(25), MD%State(26), MD%State(27), \
                MD%State(28), MD%State(29), MD%State(30), MD%State(31), \
                IFF, 25,  4, PP8_0)

    ! return state values
    MD%State = State

    NULLIFY(A, B, C, D)

    RETURN

CONTAINS

    SUBROUTINE FFT_32(X, XB, XS, RB, Q)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: X(0:)
        tIndex,   INTENT(IN)    :: XB
        tIndex,   INTENT(IN)    :: XS
        tIndex,   INTENT(IN)    :: RB
        tInteger, INTENT(INOUT) :: Q(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        ! na

    ! FLOW

        FFT16(XB, SHIFTL(XS, 1), RB)
        FFT16((XB + XS), SHIFTL(XS, 1), (RB + 16))
        FFT_LOOP(RB, 16, 8)

        RETURN

    END SUBROUTINE FFT_32

    !******************************************************************************

    SUBROUTINE FFT_64(X, XS, Q)

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tByte,    INTENT(IN)    :: X(0:)
        tIndex,   INTENT(IN)    :: XS
        tInteger, INTENT(INOUT) :: Q(0:)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tIndex  :: XD

    ! FLOW

        XD = SHIFTL(XS, 1)
        CALL FFT_32(X, 0_kIndex, XD, 0_kIndex, Q)
        CALL FFT_32(X, XS, XD, 32_kIndex, Q)
        FFT_LOOP(0, 32, 4)

        RETURN

    END SUBROUTINE FFT_64

    !******************************************************************************

    SUBROUTINE OneRound_Big(State, W, ISP, P0, P1, P2, P3)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To perform one round of mixing.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, TARGET, INTENT(INOUT) :: State(0:31)
        tInteger,         INTENT(INOUT) :: W(0:63)
        tInteger,         INTENT(IN)    :: ISP, P0, P1, P2, P3

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        tInteger, PARAMETER :: PP8K(0:10) = [1, 6, 2, 3, 5, 7, 4, 1, 6, 2, 3]

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tInteger, POINTER   :: A(:), B(:), C(:), D(:)
        tInteger            :: tA(0:7), Tmp

    ! FLOW

        A(0:7) => State(0:)
        B(0:7) => State(8:)
        C(0:7) => State(16:)
        D(0:7) => State(24:)

        STEP_BIG(W( 0), W( 1), W( 2), W( 3), W( 4), W( 5), W( 6), W( 7), IFF, P0, P1, PP8K(ISP + 0))
        STEP_BIG(W( 8), W( 9), W(10), W(11), W(12), W(13), W(14), W(15), IFF, P1, P2, PP8K(ISP + 1))
        STEP_BIG(W(16), W(17), W(18), W(19), W(20), W(21), W(22), W(23), IFF, P2, P3, PP8K(ISP + 2))
        STEP_BIG(W(24), W(25), W(26), W(27), W(28), W(29), W(30), W(31), IFF, P3, P0, PP8K(ISP + 3))
        STEP_BIG(W(32), W(33), W(34), W(35), W(36), W(37), W(38), W(39), MAJ, P0, P1, PP8K(ISP + 4))
        STEP_BIG(W(40), W(41), W(42), W(43), W(44), W(45), W(46), W(47), MAJ, P1, P2, PP8K(ISP + 5))
        STEP_BIG(W(48), W(49), W(50), W(51), W(52), W(53), W(54), W(55), MAJ, P2, P3, PP8K(ISP + 6))
        STEP_BIG(W(56), W(57), W(58), W(59), W(60), W(61), W(62), W(63), MAJ, P3, P0, PP8K(ISP + 7))

        NULLIFY(A, B, C, D)

        RETURN

    END SUBROUTINE OneRound_Big

    !******************************************************************************

#include    "Includes/SIMD_Undef Macro.f90"

END SUBROUTINE SIMDCore_Compress_Big

!******************************************************************************

END MODULE MClass_SIMDB
    
!******************************************************************************
