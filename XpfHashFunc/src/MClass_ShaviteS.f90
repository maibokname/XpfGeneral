
MODULE MClass_ShaviteS

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *ShaviteS* type and its related routines.
!   The *ShaviteS* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *ShaviteS* type implements an incremental cryptographic hash function
!   by employing a *SHAvite-3 message-digest* algorithm (either the *SHAvite-224*
!   or the *SHAvite-256*) [1].  The implementation here is based mainly on the
!   *SPHLIB* implementation [2].  <br>
!   By default, the *ShaviteS* type employs the *SHAvite-256 message-digest*
!   algorithm.  However, a user can specify the *IsSHAvite224* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *SHAvite-224 message-digest* algorithm
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
    PUBLIC :: ShaviteS

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
    tInteger, PARAMETER :: IV224_BE(0:7) = [            &
        ToInt32(Z'C4C67795'), ToInt32(Z'C0B1817F'), & 
        ToInt32(Z'EAD88924'), ToInt32(Z'1ABB1BB0'), &
        ToInt32(Z'E0C29152'), ToInt32(Z'BDE046BA'), & 
        ToInt32(Z'AEEECF99'), ToInt32(Z'58D509D8')]
    tInteger, PARAMETER :: IV224_LE(0:7) = [            &
        ToInt32(Z'6774F31C'), ToInt32(Z'990AE210'), & 
        ToInt32(Z'C87D4274'), ToInt32(Z'C9546371'), &
        ToInt32(Z'62B2AEA8'), ToInt32(Z'4B5801D8'), & 
        ToInt32(Z'1B702860'), ToInt32(Z'842F3017')]
    tInteger, PARAMETER :: IV256_BE(0:7) = [            &
        ToInt32(Z'3EECF551'), ToInt32(Z'BF10819B'), & 
        ToInt32(Z'E6DC8559'), ToInt32(Z'F3E23FD5'), &
        ToInt32(Z'431AEC73'), ToInt32(Z'79E3F731'), & 
        ToInt32(Z'98325F05'), ToInt32(Z'A92A31F1')]
    tInteger, PARAMETER :: IV256_LE(0:7) = [            &
        ToInt32(Z'49BB3E47'), ToInt32(Z'2674860D'), & 
        ToInt32(Z'A8B392AC'), ToInt32(Z'021AC4E6'), &
        ToInt32(Z'409283CF'), ToInt32(Z'620E5D86'), & 
        ToInt32(Z'6D929DCB'), ToInt32(Z'96CC2A8B')]

!** DERIVED TYPE DEFINITIONS
    !> *ShaviteS* is a concrete *digest* type that implements an
    !  incremental cryptographic hash function by employing either
    !  the *SHAvite-224* or the *SHAvite-256 message-digest* algorithm.
    TYPE, EXTENDS(MDEngine) :: ShaviteS
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state and counter variables
        tInteger    :: H(0:7) = IV256_LE
        !% flag indicating whether the SHAvite-224 algorithm is employed or not.
        tLogical    :: IsSHAvite224 = FalseVal
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
        PROCEDURE, PRIVATE  :: InitializeWFlag  => ShaviteS_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (SHAvite-256).
        PROCEDURE       :: Initialize   => ShaviteS_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => ShaviteS_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => ShaviteS_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => ShaviteS_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => ShaviteS_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => ShaviteS_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => ShaviteS_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => ShaviteS_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => ShaviteS_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => ShaviteS_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (SHAvite-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the SHAvite-224 algorithm <br>
        !   --->    CALL MD%Create(IsSHAvite224=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
        ! ---------------------------------------------------------------------
        FINAL           :: ShaviteS_Finalize
        ! ---------------------------------------------------------------------
    END TYPE ShaviteS

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE        
        ! To process input block
        SUBROUTINE ProcessData(H, InpDat, Cnt0, Cnt1)
            IMPORT
            tInteger, INTENT(INOUT) :: H(0:)        ! state values
            tByte,    INTENT(IN)    :: InpDat(0:)   ! the data block
            tInteger, INTENT(IN)    :: Cnt0, Cnt1   ! counter numbers
        END SUBROUTINE ProcessData
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE ShaviteS_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteS), INTENT(INOUT)  :: MD    !! 'ShaviteS' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the SHAvite-256 algorithm with little-endian order parameters
    CALL MD%Create(FalseVal, FalseVal)
   
    RETURN

END SUBROUTINE ShaviteS_Initialize

!******************************************************************************

SUBROUTINE ShaviteS_Initialize_wFlag(MD, IsSHAvite224, BigEndian)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteS),    INTENT(INOUT)   :: MD           !! 'ShaviteS' object
    tLogical,           INTENT(IN)      :: IsSHAvite224
    !^ flag indicating whether the SHAvite-224 algorithm is employed or not. <br>
    !  - If true, use the SHAvite-224 algorithm. <br>
    !  - Otherwise, use the SHAvite-256 algorithm. <br>
    tLogical, OPTIONAL, INTENT(IN)      :: BigEndian
    !^ flag indicating whether to use the big-endian order for initial values and AES tables. <br>
    !  - If true, use the big-endian order. <br>
    !  - Otherwise, use little-endian order. <br>
    !  default value: false.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsSHAvite224 = IsSHAvite224
    MD%LittleEndian = TrueVal
    IF (PRESENT(BigEndian)) MD%LittleEndian = .NOT.BigEndian
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE ShaviteS_Initialize_wFlag

!******************************************************************************

SUBROUTINE ShaviteS_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteS), INTENT(INOUT) :: MD   !! 'ShaviteS' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    IF (MD%LittleEndian) THEN
        MD%Process => Process_LE
        IF (MD%IsSHAvite224) THEN
            MD%H = IV224_LE
        ELSE
            MD%H = IV256_LE
        END IF
    ELSE
        MD%Process => Process_BE
        IF (MD%IsSHAvite224) THEN
            MD%H = IV224_BE
        ELSE
            MD%H = IV256_BE
        END IF
    END IF
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE ShaviteS_Reset

!******************************************************************************

SUBROUTINE ShaviteS_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteS),                INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(ShaviteS :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (ShaviteS)
        CALL Dst%Create(Src%IsSHAvite224, .NOT.Src%LittleEndian)
        Dst%H      = Src%H
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE ShaviteS_GetClone

!******************************************************************************

FUNCTION ShaviteS_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteS), INTENT(IN) :: MD       !! 'ShaviteS' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSHAvite224) THEN
        Name = 'SHAvite-224'
    ELSE
        Name = 'SHAvite-256'
    END IF

    RETURN

END FUNCTION ShaviteS_GetName

!******************************************************************************

FUNCTION ShaviteS_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteS), INTENT(IN) :: MD       !! 'ShaviteS' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSHAvite224) THEN
        Length = DLen224
    ELSE
        Length = DLen256
    END IF

    RETURN

END FUNCTION ShaviteS_GetDigestLen

!******************************************************************************

FUNCTION ShaviteS_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteS), INTENT(IN) :: MD       !! 'ShaviteS' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION ShaviteS_GetBlockLen

!******************************************************************************

SUBROUTINE ShaviteS_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteS), TARGET, INTENT(INOUT)  :: MD           !! 'ShaviteS' object
    tByte,          POINTER, INTENT(INOUT)  :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE ShaviteS_SetBufPtr

!******************************************************************************

SUBROUTINE ShaviteS_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteS), INTENT(INOUT)  :: MD           !! 'ShaviteS' object
    tByte,           INTENT(IN)     :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLong       :: BitLen
    tInteger    :: Cnt0, Cnt1

! FLOW

    BitLen = SHIFTL(MD%GetBlockCount() + 1_kInt64, 9)
    Cnt0 = ToInt32(BitLen)
    Cnt1 = ToInt32(SHIFTR(BitLen, 32))
    CALL MD%Process(MD%H, BytesIn, Cnt0, Cnt1)

    RETURN

END SUBROUTINE ShaviteS_ProcessBlock

!******************************************************************************

SUBROUTINE ShaviteS_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteS), INTENT(INOUT)  :: MD           !! 'ShaviteS' object
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE ShaviteS_DoPadding

!******************************************************************************

SUBROUTINE ShaviteS_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(ShaviteS), INTENT(INOUT)  :: MD           !! 'ShaviteS' object
    tByte,           INTENT(IN)     :: LastByte     !! the last byte
    tByte,           INTENT(IN)     :: NBits        !! number of bits in the last byte
    tByte,           INTENT(INOUT)  :: BytesOut(0:) !! the output buffer
    tIndex,          INTENT(IN)     :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Ptr, DLen, I
    tLong       :: BC, BitLen
    tInteger    :: Cnt0, Cnt1
    tByte       :: Z

! FLOW

    ! padding
    ASSOCIATE(TmpBuf => MD%BufArr)
        Ptr = MD%GetBufLen()
        BC = MD%GetBlockCount()
        BitLen = SHIFTL(BC, 9) + SHIFTL(Ptr, 3)
        Cnt0 = ToInt32(BitLen) + NBits
        Cnt1 = ToInt32(SHIFTR(BitLen, 32))
        Z = SHIFTR(FByte80, NBits)
        Z = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
        IF ((Ptr == 0_kIndex).AND.(NBits == 0_kInt8)) THEN
            TmpBuf(0) = FByte80
            TmpBuf(1:53) = FByte00
            Cnt0 = 0
            Cnt1 = 0
        ELSEIF (Ptr < 54_kIndex) THEN
            TmpBuf(Ptr) = Z
            Ptr = Ptr + 1_kIndex
            TmpBuf(Ptr:53) = FByte00
        ELSE
            TmpBuf(Ptr) = Z
            Ptr = Ptr + 1_kIndex
            TmpBuf(Ptr:63) = FByte00
            CALL MD%Process(MD%H, TmpBuf, Cnt0, Cnt1)
            TmpBuf(0:53) = FByte00
            Cnt0 = 0
            Cnt1 = 0
        END IF
        CALL ByteUnpackLE(ToInt32(BitLen) + NBits, TmpBuf, 54_kIndex)
        CALL ByteUnpackLE(ToInt32(SHIFTR(BitLen, 32)), TmpBuf, 58_kIndex)
        DLen = MD%GetDigestLen()
        TmpBuf(62) = ToInt8(SHIFTL(DLen, 3))
        TmpBuf(63) = ToInt8(SHIFTR(DLen, 5))
        CALL MD%Process(MD%H, TmpBuf, Cnt0, Cnt1)
    END ASSOCIATE

    ! finalizing
    I = 0_kIndex
    DO WHILE (I < DLen)
        CALL ByteUnpackLE(MD%H(SHIFTR(I, 2)), BytesOut, Offset + I)
        I = I + 4_kIndex
    END DO
        
    RETURN

END SUBROUTINE ShaviteS_AddBitsNPad

!******************************************************************************

SUBROUTINE ShaviteS_Finalize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free a pointer component of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(ShaviteS), INTENT(INOUT)   :: MD   !! 'ShaviteS' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    NULLIFY(MD%Process)
   
    RETURN

END SUBROUTINE ShaviteS_Finalize

!******************************************************************************

SUBROUTINE Process_BE(H, InpDat, Cnt0, Cnt1)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process input block.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: H(0:)        ! state values
    tByte,    INTENT(IN)    :: InpDat(0:)   ! the data block
    tInteger, INTENT(IN)    :: Cnt0, Cnt1   ! count numbers

!** SUBROUTINE MACRO DEFINITIONS AND PARAMETER DECLARATIONS:
#include    "Includes/AES_BigEndian.f90"
#define     AES_ROUND_NOKEY(X0, X1, X2, X3)     \
        T0 = X0; T1 = X1; T2 = X2; T3 = X3; \
        AES_ROUND_NOKEY_BE(T0, T1, T2, T3, X0, X1, X2, X3);

!** SUBROUTINE INTERNAL PARAMETER DECLARATIONS:
#include    "Includes/AES_Constants.f90"

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: T0, T1, T2, T3
    tInteger    :: P0, P1, P2, P3, P4, P5, P6, P7
    tInteger    :: X0, X1, X2, X3
    tIndex      :: U, R, S
    tInteger    :: RK(0:143)

! FLOW

    DO U = 0, 15, 4
        CALL BytePackLE(InpDat, SHIFTL(U, 2),  RK(U))
        CALL BytePackLE(InpDat, SHIFTL(U, 2) + 4,  RK(U + 1))
        CALL BytePackLE(InpDat, SHIFTL(U, 2) + 8,  RK(U + 2))
        CALL BytePackLE(InpDat, SHIFTL(U, 2) + 12, RK(U + 3))
    END DO
    U = 16
    DO R = 0, 3
        DO S = 0, 1
            X0 = RK(U - 15)
            X1 = RK(U - 14)
            X2 = RK(U - 13)
            X3 = RK(U - 16)
            AES_ROUND_NOKEY(X0, X1, X2, X3)
            RK(U)     = IEOR(X0, RK(U - 4))
            RK(U + 1) = IEOR(X1, RK(U - 3))
            RK(U + 2) = IEOR(X2, RK(U - 2))
            RK(U + 3) = IEOR(X3, RK(U - 1))
            IF (U == 16) THEN
                RK(16) = IEOR(RK(16), Cnt0)
                RK(17) = IEOR(RK(17), NOT(Cnt1))
            ELSEIF (U == 56) THEN
                RK(57) = IEOR(RK(57), Cnt1)
                RK(58) = IEOR(RK(58), NOT(Cnt0))
            END IF
            U = U + 4
            X0 = RK(U - 15)
            X1 = RK(U - 14)
            X2 = RK(U - 13)
            X3 = RK(U - 16)
            AES_ROUND_NOKEY(X0, X1, X2, X3)
            RK(U)     = IEOR(X0, RK(U - 4))
            RK(U + 1) = IEOR(X1, RK(U - 3))
            RK(U + 2) = IEOR(X2, RK(U - 2))
            RK(U + 3) = IEOR(X3, RK(U - 1))
            IF (U == 84) THEN
                RK(86) = IEOR(RK(86), Cnt1)
                RK(87) = IEOR(RK(87), NOT(Cnt0))
            ELSEIF (U == 124) THEN
                RK(124) = IEOR(RK(124), Cnt0)
                RK(127) = IEOR(RK(127), NOT(Cnt1))
            END IF
            U = U + 4
        END DO
        DO S = 0, 3
            RK(U)     = IEOR(RK(U - 16), RK(U - 3))
            RK(U + 1) = IEOR(RK(U - 15), RK(U - 2))
            RK(U + 2) = IEOR(RK(U - 14), RK(U - 1))
            RK(U + 3) = IEOR(RK(U - 13), RK(U))
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
    U  = 0
    DO R = 0, 5
        X0 = IEOR(P4, RK(U))
        X1 = IEOR(P5, RK(U + 1))
        X2 = IEOR(P6, RK(U + 2))
        X3 = IEOR(P7, RK(U + 3))
        U = U + 4
        AES_ROUND_NOKEY(X0, X1, X2, X3)
        X0 = IEOR(X0, RK(U))
        X1 = IEOR(X1, RK(U + 1))
        X2 = IEOR(X2, RK(U + 2))
        X3 = IEOR(X3, RK(U + 3))
        U = U + 4
        AES_ROUND_NOKEY(X0, X1, X2, X3)
        X0 = IEOR(X0, RK(U))
        X1 = IEOR(X1, RK(U + 1))
        X2 = IEOR(X2, RK(U + 2))
        X3 = IEOR(X3, RK(U + 3))
        U = U + 4
        AES_ROUND_NOKEY(X0, X1, X2, X3)
        P0 = IEOR(P0, X0)
        P1 = IEOR(P1, X1)
        P2 = IEOR(P2, X2)
        P3 = IEOR(P3, X3)

        X0 = IEOR(P0, RK(U))
        X1 = IEOR(P1, RK(U + 1))
        X2 = IEOR(P2, RK(U + 2))
        X3 = IEOR(P3, RK(U + 3))
        U = U + 4
        AES_ROUND_NOKEY(X0, X1, X2, X3)
        X0 = IEOR(X0, RK(U))
        X1 = IEOR(X1, RK(U + 1))
        X2 = IEOR(X2, RK(U + 2))
        X3 = IEOR(X3, RK(U + 3))
        U = U + 4
        AES_ROUND_NOKEY(X0, X1, X2, X3)
        X0 = IEOR(X0, RK(U))
        X1 = IEOR(X1, RK(U + 1))
        X2 = IEOR(X2, RK(U + 2))
        X3 = IEOR(X3, RK(U + 3))
        U = U + 4
        AES_ROUND_NOKEY(X0, X1, X2, X3)
        P4 = IEOR(P4, X0)
        P5 = IEOR(P5, X1)
        P6 = IEOR(P6, X2)
        P7 = IEOR(P7, X3)
    END DO
    H(0) = IEOR(H(0), P0)
    H(1) = IEOR(H(1), P1)
    H(2) = IEOR(H(2), P2)
    H(3) = IEOR(H(3), P3)
    H(4) = IEOR(H(4), P4)
    H(5) = IEOR(H(5), P5)
    H(6) = IEOR(H(6), P6)
    H(7) = IEOR(H(7), P7)

    RETURN

#include    "Includes/AES_Undef Macro.f90"
#undef      AES_ROUND_NOKEY

END SUBROUTINE Process_BE

!******************************************************************************

SUBROUTINE Process_LE(H, InpDat, Cnt0, Cnt1)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process input block.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: H(0:)        ! state values
    tByte,    INTENT(IN)    :: InpDat(0:)   ! the data block
    tInteger, INTENT(IN)    :: Cnt0, Cnt1   ! count numbers

!** SUBROUTINE MACRO DEFINITIONS:
#include    "Includes/AES_LittleEndian.f90"
#define     AES_ROUND_NOKEY(X0, X1, X2, X3)     \
        T0 = X0; T1 = X1; T2 = X2; T3 = X3; \
        AES_ROUND_NOKEY_LE(T0, T1, T2, T3, X0, X1, X2, X3);

!** SUBROUTINE INTERNAL PARAMETER DECLARATIONS:
#include    "Includes/AES_Constants.f90"

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: T0, T1, T2, T3
    tInteger    :: P0, P1, P2, P3, P4, P5, P6, P7
    tInteger    :: X0, X1, X2, X3
    tIndex      :: U, R, S
    tInteger    :: RK(0:143)

! FLOW

    DO U = 0, 15, 4
        CALL BytePackLE(InpDat, SHIFTL(U, 2),  RK(U))
        CALL BytePackLE(InpDat, SHIFTL(U, 2) + 4,  RK(U + 1))
        CALL BytePackLE(InpDat, SHIFTL(U, 2) + 8,  RK(U + 2))
        CALL BytePackLE(InpDat, SHIFTL(U, 2) + 12, RK(U + 3))
    END DO
    U = 16
    DO R = 0, 3
        DO S = 0, 1
            X0 = RK(U - 15)
            X1 = RK(U - 14)
            X2 = RK(U - 13)
            X3 = RK(U - 16)
            AES_ROUND_NOKEY(X0, X1, X2, X3)
            RK(U)     = IEOR(X0, RK(U - 4))
            RK(U + 1) = IEOR(X1, RK(U - 3))
            RK(U + 2) = IEOR(X2, RK(U - 2))
            RK(U + 3) = IEOR(X3, RK(U - 1))
            IF (U == 16) THEN
                RK(16) = IEOR(RK(16), Cnt0)
                RK(17) = IEOR(RK(17), NOT(Cnt1))
            ELSEIF (U == 56) THEN
                RK(57) = IEOR(RK(57), Cnt1)
                RK(58) = IEOR(RK(58), NOT(Cnt0))
            END IF
            U = U + 4
            X0 = RK(U - 15)
            X1 = RK(U - 14)
            X2 = RK(U - 13)
            X3 = RK(U - 16)
            AES_ROUND_NOKEY(X0, X1, X2, X3)
            RK(U)     = IEOR(X0, RK(U - 4))
            RK(U + 1) = IEOR(X1, RK(U - 3))
            RK(U + 2) = IEOR(X2, RK(U - 2))
            RK(U + 3) = IEOR(X3, RK(U - 1))
            IF (U == 84) THEN
                RK(86) = IEOR(RK(86), Cnt1)
                RK(87) = IEOR(RK(87), NOT(Cnt0))
            ELSEIF (U == 124) THEN
                RK(124) = IEOR(RK(124), Cnt0)
                RK(127) = IEOR(RK(127), NOT(Cnt1))
            END IF
            U = U + 4
        END DO
        DO S = 0, 3
            RK(U)     = IEOR(RK(U - 16), RK(U - 3))
            RK(U + 1) = IEOR(RK(U - 15), RK(U - 2))
            RK(U + 2) = IEOR(RK(U - 14), RK(U - 1))
            RK(U + 3) = IEOR(RK(U - 13), RK(U))
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
    U  = 0
    DO R = 0, 5
        X0 = IEOR(P4, RK(U))
        X1 = IEOR(P5, RK(U + 1))
        X2 = IEOR(P6, RK(U + 2))
        X3 = IEOR(P7, RK(U + 3))
        U = U + 4
        AES_ROUND_NOKEY(X0, X1, X2, X3)
        X0 = IEOR(X0, RK(U))
        X1 = IEOR(X1, RK(U + 1))
        X2 = IEOR(X2, RK(U + 2))
        X3 = IEOR(X3, RK(U + 3))
        U = U + 4
        AES_ROUND_NOKEY(X0, X1, X2, X3)
        X0 = IEOR(X0, RK(U))
        X1 = IEOR(X1, RK(U + 1))
        X2 = IEOR(X2, RK(U + 2))
        X3 = IEOR(X3, RK(U + 3))
        U = U + 4
        AES_ROUND_NOKEY(X0, X1, X2, X3)
        P0 = IEOR(P0, X0)
        P1 = IEOR(P1, X1)
        P2 = IEOR(P2, X2)
        P3 = IEOR(P3, X3)

        X0 = IEOR(P0, RK(U))
        X1 = IEOR(P1, RK(U + 1))
        X2 = IEOR(P2, RK(U + 2))
        X3 = IEOR(P3, RK(U + 3))
        U = U + 4
        AES_ROUND_NOKEY(X0, X1, X2, X3)
        X0 = IEOR(X0, RK(U))
        X1 = IEOR(X1, RK(U + 1))
        X2 = IEOR(X2, RK(U + 2))
        X3 = IEOR(X3, RK(U + 3))
        U = U + 4
        AES_ROUND_NOKEY(X0, X1, X2, X3)
        X0 = IEOR(X0, RK(U))
        X1 = IEOR(X1, RK(U + 1))
        X2 = IEOR(X2, RK(U + 2))
        X3 = IEOR(X3, RK(U + 3))
        U = U + 4
        AES_ROUND_NOKEY(X0, X1, X2, X3)
        P4 = IEOR(P4, X0)
        P5 = IEOR(P5, X1)
        P6 = IEOR(P6, X2)
        P7 = IEOR(P7, X3)
    END DO
    H(0) = IEOR(H(0), P0)
    H(1) = IEOR(H(1), P1)
    H(2) = IEOR(H(2), P2)
    H(3) = IEOR(H(3), P3)
    H(4) = IEOR(H(4), P4)
    H(5) = IEOR(H(5), P5)
    H(6) = IEOR(H(6), P6)
    H(7) = IEOR(H(7), P7)

    RETURN

#include    "Includes/AES_Undef Macro.f90"
#undef      AES_ROUND_NOKEY

END SUBROUTINE Process_LE

!******************************************************************************

END MODULE MClass_ShaviteS
    
!******************************************************************************
