
MODULE MClass_ShaviteB

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *ShaviteB* type and its related routines.
!   The *ShaviteB* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *ShaviteB* type implements an incremental cryptographic hash function
!   by employing a *SHAvite-3 message-digest* algorithm (either the *SHAvite-384*
!   or the *SHAvite-512*) [1].  The implementation here is based mainly on the
!   *SPHLIB* implementation [2].  <br>
!   By default, the *ShaviteB* type employs the *SHAvite-512 message-digest*
!   algorithm.  However, a user can specify the *IsSHAvite384* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *SHAvite-384 message-digest* algorithm
!   instead of the default one. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://web.archive.org/web/20161220074845mp_/http://www.cs.technion.ac.il/~orrd/SHAvite-3/">
!       The SHAvite-3 hash function. </a> <br>
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
    PUBLIC :: ShaviteB

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
    tInteger, PARAMETER :: IV384_BE(0:15) = [           &
        ToInt32(Z'71F48510'), ToInt32(Z'A903A8AC'), & 
        ToInt32(Z'FE3216DD'), ToInt32(Z'0B2D2AD4'), &
        ToInt32(Z'6672900A'), ToInt32(Z'41032819'), & 
        ToInt32(Z'15A7D780'), ToInt32(Z'B3CAB8D9'), &
        ToInt32(Z'34EF4711'), ToInt32(Z'DE019FE8'), & 
        ToInt32(Z'4D674DC4'), ToInt32(Z'E056D96B'), &
        ToInt32(Z'A35C016B'), ToInt32(Z'DD903BA7'), & 
        ToInt32(Z'8C1B09B4'), ToInt32(Z'2C3E9F25')]
    tInteger, PARAMETER :: IV384_LE(0:15) = [           &
        ToInt32(Z'83DF1545'), ToInt32(Z'F9AAEC13'), & 
        ToInt32(Z'F4803CB0'), ToInt32(Z'11FE1F47'), &
        ToInt32(Z'DA6CD269'), ToInt32(Z'4F53FCD7'), & 
        ToInt32(Z'950529A2'), ToInt32(Z'97908147'), &
        ToInt32(Z'B0A4D7AF'), ToInt32(Z'2B9132BF'), & 
        ToInt32(Z'226E607D'), ToInt32(Z'3C0F8D7C'), &
        ToInt32(Z'487B3F0F'), ToInt32(Z'04363E22'), & 
        ToInt32(Z'0155C99C'), ToInt32(Z'EC2E20D3')]
    tInteger, PARAMETER :: IV512_BE(0:15) = [           &
        ToInt32(Z'D5652B63'), ToInt32(Z'25F1E6EA'), & 
        ToInt32(Z'B18F48FA'), ToInt32(Z'A1EE3A47'), &
        ToInt32(Z'C8B67B07'), ToInt32(Z'BDCE48D3'), & 
        ToInt32(Z'E3937B78'), ToInt32(Z'05DB5186'), &
        ToInt32(Z'613BE326'), ToInt32(Z'A11FA303'), & 
        ToInt32(Z'90C833D4'), ToInt32(Z'79CEE316'), &
        ToInt32(Z'1E1AF00F'), ToInt32(Z'2829B165'), & 
        ToInt32(Z'23B25F80'), ToInt32(Z'21E11499')]
    tInteger, PARAMETER :: IV512_LE(0:15) = [           &
        ToInt32(Z'72FCCDD8'), ToInt32(Z'79CA4727'), & 
        ToInt32(Z'128A077B'), ToInt32(Z'40D55AEC'), &
        ToInt32(Z'D1901A06'), ToInt32(Z'430AE307'), & 
        ToInt32(Z'B29F5CD1'), ToInt32(Z'DF07FBFC'), &
        ToInt32(Z'8E45D73D'), ToInt32(Z'681AB538'), & 
        ToInt32(Z'BDE86578'), ToInt32(Z'DD577E47'), &
        ToInt32(Z'E275EADE'), ToInt32(Z'502D9FCD'), & 
        ToInt32(Z'B9357178'), ToInt32(Z'022A4B9A')]

!** DERIVED TYPE DEFINITIONS
    !> *ShaviteB* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing either
    !  the *SHAvite-384* or the *SHAvite-512 message-digest* algorithm.
    TYPE, EXTENDS(MDEngine) :: ShaviteB
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state and counter variables
        tInteger    :: H(0:15) = IV512_LE
        !% flag indicating whether the SHAvite-384 algorithm is employed or not.
        tLogical    :: IsSHAvite384 = FalseVal
        !> flag indicating whether to use little-endian order for initial values
        !  and AES tables.
        tLogical    :: LittleEndian = TrueVal
        !% pointer to a procedure that processes a block of data
        PROCEDURE(ProcessData), POINTER, NOPASS :: Process => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                    Private Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => ShaviteB_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (SHAvite-512).
        PROCEDURE       :: Initialize   => ShaviteB_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => ShaviteB_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => ShaviteB_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => ShaviteB_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => ShaviteB_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => ShaviteB_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => ShaviteB_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => ShaviteB_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => ShaviteB_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => ShaviteB_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (SHAvite-512) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the SHAvite-384 algorithm <br>
        !   --->    CALL MD%Create(IsSHAvite384=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
        ! ---------------------------------------------------------------------
        FINAL           :: ShaviteB_Finalize
        ! ---------------------------------------------------------------------
    END TYPE ShaviteB

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE        
        ! To process input block
        SUBROUTINE ProcessData(H, InpDat, Cnt0, Cnt1, Cnt2)
            IMPORT
            tInteger, INTENT(INOUT) :: H(0:)            ! state values
            tByte,    INTENT(IN)    :: InpDat(0:)       ! the data block
            tInteger, INTENT(IN)    :: Cnt0, Cnt1, Cnt2 ! counter numbers
        END SUBROUTINE ProcessData
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE ShaviteB_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteB), INTENT(INOUT)  :: MD    !! 'ShaviteB' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the SHAvite-512 algorithm with little-endian order parameters
    CALL MD%Create(FalseVal, FalseVal)
   
    RETURN

END SUBROUTINE ShaviteB_Initialize

!******************************************************************************

SUBROUTINE ShaviteB_Initialize_wFlag(MD, IsSHAvite384, BigEndian)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteB),    INTENT(INOUT)   :: MD           !! 'ShaviteB' object
    tLogical,           INTENT(IN)      :: IsSHAvite384
    !^ flag indicating whether the SHAvite-384 algorithm is employed or not. <br>
    !  - If true, use the SHAvite-384 algorithm. <br>
    !  - Otherwise, use the SHAvite-512 algorithm. <br>
    tLogical, OPTIONAL, INTENT(IN)      :: BigEndian
    !^ flag indicating whether to use the big-endian order for initial values and AES tables. <br>
    !  - If true, use the big-endian order. <br>
    !  - Otherwise, use little-endian order. <br>
    !  default value: false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsSHAvite384 = IsSHAvite384
    MD%LittleEndian = TrueVal
    IF (PRESENT(BigEndian)) MD%LittleEndian = .NOT.BigEndian
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE ShaviteB_Initialize_wFlag

!******************************************************************************

SUBROUTINE ShaviteB_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteB), INTENT(INOUT) :: MD   !! 'ShaviteB' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    IF (MD%LittleEndian) THEN
        MD%Process => Process_LE
        IF (MD%IsSHAvite384) THEN
            MD%H = IV384_LE
        ELSE
            MD%H = IV512_LE
        END IF
    ELSE
        MD%Process => Process_BE
        IF (MD%IsSHAvite384) THEN
            MD%H = IV384_BE
        ELSE
            MD%H = IV512_BE
        END IF
    END IF
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE ShaviteB_Reset

!******************************************************************************

SUBROUTINE ShaviteB_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteB),                INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(ShaviteB :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (ShaviteB)
        CALL Dst%Create(Src%IsSHAvite384, .NOT.Src%LittleEndian)
        Dst%H      = Src%H
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE ShaviteB_GetClone

!******************************************************************************

FUNCTION ShaviteB_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteB), INTENT(IN) :: MD       !! 'ShaviteB' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSHAvite384) THEN
        Name = 'SHAvite-384'
    ELSE
        Name = 'SHAvite-512'
    END IF

    RETURN

END FUNCTION ShaviteB_GetName

!******************************************************************************

FUNCTION ShaviteB_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteB), INTENT(IN) :: MD       !! 'ShaviteB' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSHAvite384) THEN
        Length = DLen384
    ELSE
        Length = DLen512
    END IF

    RETURN

END FUNCTION ShaviteB_GetDigestLen

!******************************************************************************

FUNCTION ShaviteB_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteB), INTENT(IN) :: MD       !! 'ShaviteB' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION ShaviteB_GetBlockLen

!******************************************************************************

SUBROUTINE ShaviteB_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteB), TARGET, INTENT(INOUT)  :: MD           !! 'ShaviteB' object
    tByte,          POINTER, INTENT(INOUT)  :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE ShaviteB_SetBufPtr

!******************************************************************************

SUBROUTINE ShaviteB_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteB), INTENT(INOUT)  :: MD           !! 'ShaviteB' object
    tByte,           INTENT(IN)     :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: BitLen, BC
    tInteger    :: Cnt0, Cnt1, Cnt2

! FLOW

    BC = MD%GetBlockCount() + 1_kInt64
    BitLen = SHIFTL(BC, 10)
    Cnt0 = ToInt32(BitLen)
    Cnt1 = ToInt32(SHIFTR(BitLen, 32))
    Cnt2 = ToInt32(SHIFTR(BC, 54))
    CALL MD%Process(MD%H, BytesIn, Cnt0, Cnt1, Cnt2)

    RETURN

END SUBROUTINE ShaviteB_ProcessBlock

!******************************************************************************

SUBROUTINE ShaviteB_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteB), INTENT(INOUT)  :: MD           !! 'ShaviteB' object
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE ShaviteB_DoPadding

!******************************************************************************

SUBROUTINE ShaviteB_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteB), INTENT(INOUT)  :: MD           !! 'ShaviteB' object
    tByte,           INTENT(IN)     :: LastByte     !! the last byte
    tByte,           INTENT(IN)     :: NBits        !! number of bits in the last byte
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Ptr, DLen, I
    tLong       :: BC, BitLen
    tInteger    :: Cnt0, Cnt1, Cnt2
    tByte       :: Z

! FLOW

    ! padding
    ASSOCIATE(TmpBuf => MD%BufArr)
        Ptr = MD%GetBufLen()
        BC = MD%GetBlockCount()
        BitLen = SHIFTL(BC, 10) + SHIFTL(Ptr, 3)
        Cnt0 = ToInt32(BitLen) + NBits
        Cnt1 = ToInt32(SHIFTR(BitLen, 32))
        Cnt2 = ToInt32(SHIFTR(BC, 54))
        Z = SHIFTR(FByte80, NBits)
        Z = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
        IF ((Ptr == 0_kIndex).AND.(NBits == 0_kInt8)) THEN
            TmpBuf(0) = FByte80
            TmpBuf(1:109) = FByte00
            Cnt0 = 0
            Cnt1 = 0
            Cnt2 = 0
        ELSEIF (Ptr < 110_kIndex) THEN
            TmpBuf(Ptr) = Z
            Ptr = Ptr + 1_kIndex
            TmpBuf(Ptr:109) = FByte00
        ELSE
            TmpBuf(Ptr) = Z
            Ptr = Ptr + 1_kIndex
            TmpBuf(Ptr:127) = FByte00
            CALL MD%Process(MD%H, TmpBuf, Cnt0, Cnt1, Cnt2)
            TmpBuf(0:109) = FByte00
            Cnt0 = 0
            Cnt1 = 0
            Cnt2 = 0
        END IF
        CALL ByteUnpackLE(ToInt32(BitLen) + NBits, TmpBuf, 110_kIndex)
        CALL ByteUnpackLE(ToInt32(SHIFTR(BitLen, 32)), TmpBuf, 114_kIndex)
        CALL ByteUnpackLE(ToInt32(SHIFTR(BC, 54)), TmpBuf, 118_kIndex)
        TmpBuf(122:125) = FByte00
        DLen = MD%GetDigestLen()
        TmpBuf(126) = ToInt8(SHIFTL(DLen, 3))
        TmpBuf(127) = ToInt8(SHIFTR(DLen, 5))
        CALL MD%Process(MD%H, TmpBuf, Cnt0, Cnt1, Cnt2)
    END ASSOCIATE

    ! finalizing
    I = 0_kIndex
    DO WHILE (I < DLen)
        CALL ByteUnpackLE(MD%H(SHIFTR(I, 2)), BytesOut, Offset + I)
        I = I + 4_kIndex
    END DO
        
    RETURN

END SUBROUTINE ShaviteB_AddBitsNPad

!******************************************************************************

SUBROUTINE ShaviteB_Finalize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free a pointer component of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ShaviteB), INTENT(INOUT)   :: MD   !! 'ShaviteB' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    NULLIFY(MD%Process)
   
    RETURN

END SUBROUTINE ShaviteB_Finalize

!******************************************************************************

SUBROUTINE Process_BE(H, InpDat, Cnt0, Cnt1, Cnt2)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process one block.  This implementation supports up to about
    ! 2^64 input blocks, i.e. 2^74 bits.  Thus, the counter highest
    ! word (cnt3) is always zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: H(0:)        ! state values
    tByte,    INTENT(IN)    :: InpDat(0:)   ! the data block (128 bytes)
    tInteger, INTENT(IN)    :: Cnt0         ! the first (least significant) bit counter word
    tInteger, INTENT(IN)    :: Cnt1         ! the second bit count word
    tInteger, INTENT(IN)    :: Cnt2         ! the third bit count word

!** SUBROUTINE MACRO DEFINITIONS AND PARAMETER DECLARATIONS:
#include    "Includes/AES_BigEndian.f90"
#define     AES_ROUND_NOKEY(X0, X1, X2, X3)     \
        T0 = X0; T1 = X1; T2 = X2; T3 = X3; \
        AES_ROUND_NOKEY_BE(T0, T1, T2, T3, X0, X1, X2, X3);
#define     C512_ELT(L0, L1, L2, L3, R0, R1, R2, R3)    \
    X0 = IEOR(R0, RK(U)); \
    X1 = IEOR(R1, RK(U + 1)); \
    X2 = IEOR(R2, RK(U + 2)); \
    X3 = IEOR(R3, RK(U + 3)); \
    U = U + 4; \
    AES_ROUND_NOKEY(X0, X1, X2, X3); \
    X0 = IEOR(X0, RK(U)); \
    X1 = IEOR(X1, RK(U + 1)); \
    X2 = IEOR(X2, RK(U + 2)); \
    X3 = IEOR(X3, RK(U + 3)); \
    U = U + 4; \
    AES_ROUND_NOKEY(X0, X1, X2, X3); \
    X0 = IEOR(X0, RK(U)); \
    X1 = IEOR(X1, RK(U + 1)); \
    X2 = IEOR(X2, RK(U + 2)); \
    X3 = IEOR(X3, RK(U + 3)); \
    U = U + 4; \
    AES_ROUND_NOKEY(X0, X1, X2, X3); \
    X0 = IEOR(X0, RK(U)); \
    X1 = IEOR(X1, RK(U + 1)); \
    X2 = IEOR(X2, RK(U + 2)); \
    X3 = IEOR(X3, RK(U + 3)); \
    U = U + 4; \
    AES_ROUND_NOKEY(X0, X1, X2, X3); \
    L0 = IEOR(L0, X0); \
    L1 = IEOR(L1, X1); \
    L2 = IEOR(L2, X2); \
    L3 = IEOR(L3, X3);
#define     WROT(A, B, C, D)    \
    T = D; \
    D = C; \
    C = B; \
    B = A; \
    A = T;

!** SUBROUTINE INTERNAL PARAMETER DECLARATIONS:
#include    "Includes/AES_Constants.f90"

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: T0, T1, T2, T3, T
    tInteger    :: P0, P1, P2, P3, P4, P5, P6, P7
    tInteger    :: P8, P9, PA, PB, PC, PD, PE, PF
    tInteger    :: X0, X1, X2, X3
    tIndex      :: U, R, S
    tInteger    :: RK(0:447)

! FLOW

    DO U = 0, 31, 4
        CALL BytePackLE(InpDat, SHIFTL(U, 2),  RK(U))
        CALL BytePackLE(InpDat, SHIFTL(U, 2) + 4,  RK(U + 1))
        CALL BytePackLE(InpDat, SHIFTL(U, 2) + 8,  RK(U + 2))
        CALL BytePackLE(InpDat, SHIFTL(U, 2) + 12, RK(U + 3))
    END DO
    U = 32
    DO
        DO S = 0, 3
            X0 = RK(U - 31)
            X1 = RK(U - 30)
            X2 = RK(U - 29)
            X3 = RK(U - 32)
            AES_ROUND_NOKEY(X0, X1, X2, X3)
            RK(U)     = IEOR(X0, RK(U - 4))
            RK(U + 1) = IEOR(X1, RK(U - 3))
            RK(U + 2) = IEOR(X2, RK(U - 2))
            RK(U + 3) = IEOR(X3, RK(U - 1))
            IF (U == 32) THEN
                RK(32) = IEOR(RK(32), Cnt0)
                RK(33) = IEOR(RK(33), Cnt1)
                RK(34) = IEOR(RK(34), Cnt2)
                RK(35) = IEOR(RK(35), NOT(0))
            ELSEIF (U == 440) THEN
                RK(440) = IEOR(RK(440), Cnt1)
                RK(441) = IEOR(RK(441), Cnt0)
                ! RK(442) = IEOR(RK(442), 0)
                RK(443) = IEOR(RK(443), NOT(Cnt2))
            END IF
            U = U + 4
            X0 = RK(U - 31)
            X1 = RK(U - 30)
            X2 = RK(U - 29)
            X3 = RK(U - 32)
            AES_ROUND_NOKEY(X0, X1, X2, X3)
            RK(U)     = IEOR(X0, RK(U - 4))
            RK(U + 1) = IEOR(X1, RK(U - 3))
            RK(U + 2) = IEOR(X2, RK(U - 2))
            RK(U + 3) = IEOR(X3, RK(U - 1))
            IF (U == 164) THEN
                ! RK(164) = IEOR(RK(164), 0)
                RK(165) = IEOR(RK(165), Cnt2)
                RK(166) = IEOR(RK(166), Cnt1)
                RK(167) = IEOR(RK(167), NOT(Cnt0))
            ELSEIF (U == 316) THEN
                RK(316) = IEOR(RK(316), Cnt2)
                ! RK(317) = IEOR(RK(317), 0)
                RK(318) = IEOR(RK(318), Cnt0)
                RK(319) = IEOR(RK(319), NOT(Cnt1))
            END IF
            U = U + 4
        END DO
        IF (U == 448) EXIT
        DO S = 0, 7
            RK(U)     = IEOR(RK(U - 32), RK(U - 7))
            RK(U + 1) = IEOR(RK(U - 31), RK(U - 6))
            RK(U + 2) = IEOR(RK(U - 30), RK(U - 5))
            RK(U + 3) = IEOR(RK(U - 29), RK(U - 4))
            U = U + 4
        END DO
    END DO

    P0 = H(0)
    P1 = H(1)
    P2 = H(2)
    P3 = H(3)
    P4 = H(4)
    P5 = H(5)
    P6 = H(6)
    P7 = H(7)
    P8 = H(8)
    P9 = H(9)
    PA = H(10)
    PB = H(11)
    PC = H(12)
    PD = H(13)
    PE = H(14)
    PF = H(15)
    U  = 0
    DO R = 0, 13
        C512_ELT(P0, P1, P2, P3, P4, P5, P6, P7)
        C512_ELT(P8, P9, PA, PB, PC, PD, PE, PF)
        WROT(P0, P4, P8, PC)
        WROT(P1, P5, P9, PD)
        WROT(P2, P6, PA, PE)
        WROT(P3, P7, PB, PF)
    END DO
    H(0)  = IEOR(H(0),  P0)
    H(1)  = IEOR(H(1),  P1)
    H(2)  = IEOR(H(2),  P2)
    H(3)  = IEOR(H(3),  P3)
    H(4)  = IEOR(H(4),  P4)
    H(5)  = IEOR(H(5),  P5)
    H(6)  = IEOR(H(6),  P6)
    H(7)  = IEOR(H(7),  P7)
    H(8)  = IEOR(H(8),  P8)
    H(9)  = IEOR(H(9),  P9)
    H(10) = IEOR(H(10), PA)
    H(11) = IEOR(H(11), PB)
    H(12) = IEOR(H(12), PC)
    H(13) = IEOR(H(13), PD)
    H(14) = IEOR(H(14), PE)
    H(15) = IEOR(H(15), PF)
    
    RETURN

#include    "Includes/AES_Undef Macro.f90"
#undef      AES_ROUND_NOKEY
#undef      C512_ELT
#undef      WROT

END SUBROUTINE Process_BE

!******************************************************************************

SUBROUTINE Process_LE(H, InpDat, Cnt0, Cnt1, Cnt2)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process one block.  This implementation supports up to about
    ! 2^64 input blocks, i.e. 2^74 bits.  Thus, the counter highest
    ! word (cnt3) is always zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: H(0:)        ! state values
    tByte,    INTENT(IN)    :: InpDat(0:)   ! the data block (128 bytes)
    tInteger, INTENT(IN)    :: Cnt0         ! the first (least significant) bit counter word
    tInteger, INTENT(IN)    :: Cnt1         ! the second bit count word
    tInteger, INTENT(IN)    :: Cnt2         ! the third bit count word

!** SUBROUTINE MACRO DEFINITIONS:
#include    "Includes/AES_LittleEndian.f90"
#define     AES_ROUND_NOKEY(X0, X1, X2, X3)     \
        T0 = X0; T1 = X1; T2 = X2; T3 = X3; \
        AES_ROUND_NOKEY_LE(T0, T1, T2, T3, X0, X1, X2, X3);
#define     C512_ELT(L0, L1, L2, L3, R0, R1, R2, R3)    \
    X0 = IEOR(R0, RK(U)); \
    X1 = IEOR(R1, RK(U + 1)); \
    X2 = IEOR(R2, RK(U + 2)); \
    X3 = IEOR(R3, RK(U + 3)); \
    U = U + 4; \
    AES_ROUND_NOKEY(X0, X1, X2, X3); \
    X0 = IEOR(X0, RK(U)); \
    X1 = IEOR(X1, RK(U + 1)); \
    X2 = IEOR(X2, RK(U + 2)); \
    X3 = IEOR(X3, RK(U + 3)); \
    U = U + 4; \
    AES_ROUND_NOKEY(X0, X1, X2, X3); \
    X0 = IEOR(X0, RK(U)); \
    X1 = IEOR(X1, RK(U + 1)); \
    X2 = IEOR(X2, RK(U + 2)); \
    X3 = IEOR(X3, RK(U + 3)); \
    U = U + 4; \
    AES_ROUND_NOKEY(X0, X1, X2, X3); \
    X0 = IEOR(X0, RK(U)); \
    X1 = IEOR(X1, RK(U + 1)); \
    X2 = IEOR(X2, RK(U + 2)); \
    X3 = IEOR(X3, RK(U + 3)); \
    U = U + 4; \
    AES_ROUND_NOKEY(X0, X1, X2, X3); \
    L0 = IEOR(L0, X0); \
    L1 = IEOR(L1, X1); \
    L2 = IEOR(L2, X2); \
    L3 = IEOR(L3, X3);
#define     WROT(A, B, C, D)    \
    T = D; \
    D = C; \
    C = B; \
    B = A; \
    A = T;

!** SUBROUTINE INTERNAL PARAMETER DECLARATIONS:
#include    "Includes/AES_Constants.f90"

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: T0, T1, T2, T3, T
    tInteger    :: P0, P1, P2, P3, P4, P5, P6, P7
    tInteger    :: P8, P9, PA, PB, PC, PD, PE, PF
    tInteger    :: X0, X1, X2, X3
    tIndex      :: U, R, S
    tInteger    :: RK(0:447)

! FLOW

    DO U = 0, 31, 4
        CALL BytePackLE(InpDat, SHIFTL(U, 2),  RK(U))
        CALL BytePackLE(InpDat, SHIFTL(U, 2) + 4,  RK(U + 1))
        CALL BytePackLE(InpDat, SHIFTL(U, 2) + 8,  RK(U + 2))
        CALL BytePackLE(InpDat, SHIFTL(U, 2) + 12, RK(U + 3))
    END DO
    U = 32
    DO
        DO S = 0, 3
            X0 = RK(U - 31)
            X1 = RK(U - 30)
            X2 = RK(U - 29)
            X3 = RK(U - 32)
            AES_ROUND_NOKEY(X0, X1, X2, X3)
            RK(U)     = IEOR(X0, RK(U - 4))
            RK(U + 1) = IEOR(X1, RK(U - 3))
            RK(U + 2) = IEOR(X2, RK(U - 2))
            RK(U + 3) = IEOR(X3, RK(U - 1))
            IF (U == 32) THEN
                RK(32) = IEOR(RK(32), Cnt0)
                RK(33) = IEOR(RK(33), Cnt1)
                RK(34) = IEOR(RK(34), Cnt2)
                RK(35) = IEOR(RK(35), NOT(0))
            ELSEIF (U == 440) THEN
                RK(440) = IEOR(RK(440), Cnt1)
                RK(441) = IEOR(RK(441), Cnt0)
                ! RK(442) = IEOR(RK(442), 0)
                RK(443) = IEOR(RK(443), NOT(Cnt2))
            END IF
            U = U + 4
            X0 = RK(U - 31)
            X1 = RK(U - 30)
            X2 = RK(U - 29)
            X3 = RK(U - 32)
            AES_ROUND_NOKEY(X0, X1, X2, X3)
            RK(U)     = IEOR(X0, RK(U - 4))
            RK(U + 1) = IEOR(X1, RK(U - 3))
            RK(U + 2) = IEOR(X2, RK(U - 2))
            RK(U + 3) = IEOR(X3, RK(U - 1))
            IF (U == 164) THEN
                ! RK(164) = IEOR(RK(164), 0)
                RK(165) = IEOR(RK(165), Cnt2)
                RK(166) = IEOR(RK(166), Cnt1)
                RK(167) = IEOR(RK(167), NOT(Cnt0))
            ELSEIF (U == 316) THEN
                RK(316) = IEOR(RK(316), Cnt2)
                ! RK(317) = IEOR(RK(317), 0)
                RK(318) = IEOR(RK(318), Cnt0)
                RK(319) = IEOR(RK(319), NOT(Cnt1))
            END IF
            U = U + 4
        END DO
        IF (U == 448) EXIT
        DO S = 0, 7
            RK(U)     = IEOR(RK(U - 32), RK(U - 7))
            RK(U + 1) = IEOR(RK(U - 31), RK(U - 6))
            RK(U + 2) = IEOR(RK(U - 30), RK(U - 5))
            RK(U + 3) = IEOR(RK(U - 29), RK(U - 4))
            U = U + 4
        END DO
    END DO

    P0 = H(0)
    P1 = H(1)
    P2 = H(2)
    P3 = H(3)
    P4 = H(4)
    P5 = H(5)
    P6 = H(6)
    P7 = H(7)
    P8 = H(8)
    P9 = H(9)
    PA = H(10)
    PB = H(11)
    PC = H(12)
    PD = H(13)
    PE = H(14)
    PF = H(15)
    U  = 0
    DO R = 0, 13
        C512_ELT(P0, P1, P2, P3, P4, P5, P6, P7)
        C512_ELT(P8, P9, PA, PB, PC, PD, PE, PF)
        WROT(P0, P4, P8, PC)
        WROT(P1, P5, P9, PD)
        WROT(P2, P6, PA, PE)
        WROT(P3, P7, PB, PF)
    END DO
    H(0)  = IEOR(H(0),  P0)
    H(1)  = IEOR(H(1),  P1)
    H(2)  = IEOR(H(2),  P2)
    H(3)  = IEOR(H(3),  P3)
    H(4)  = IEOR(H(4),  P4)
    H(5)  = IEOR(H(5),  P5)
    H(6)  = IEOR(H(6),  P6)
    H(7)  = IEOR(H(7),  P7)
    H(8)  = IEOR(H(8),  P8)
    H(9)  = IEOR(H(9),  P9)
    H(10) = IEOR(H(10), PA)
    H(11) = IEOR(H(11), PB)
    H(12) = IEOR(H(12), PC)
    H(13) = IEOR(H(13), PD)
    H(14) = IEOR(H(14), PE)
    H(15) = IEOR(H(15), PF)

    RETURN

#include    "Includes/AES_Undef Macro.f90"
#undef      AES_ROUND_NOKEY
#undef      C512_ELT
#undef      WROT

END SUBROUTINE Process_LE

!******************************************************************************

END MODULE MClass_ShaviteB
    
!******************************************************************************
