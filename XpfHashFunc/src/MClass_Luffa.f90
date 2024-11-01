
MODULE MClass_Luffa

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Luffa* type and its related routines.
!   The *Luffa* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *Luffa* type implements an incremental cryptographic hash
!   function by employing the *Luffa message-digest* algorithm [1].  The
!   implementation here is mainly based on the references [2]. <br>
!   The *Luffa* type represents four cryptographic hash functions:
!   the *Luffa-224*, *Luffa-256*, *Luffa-384*, and *Luffa-512* hash
!   functions.  By default, the *Luffa* type represents the *Luffa-256*
!   hash function.  However, a user can specify the *Security* argument
!   (to one of the four applicable values: 224, 256, 384 and 512) when
!   initializing the digest object in order to use a different hash
!   function and get a different hash output size. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.hitachi.com/rd/yrl/crypto/luffa/">The Hash Function
!       Family Luffa (Round 2 Archive). </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil,           ONLY: ToDecStrSigned
    USE MBase_BytePack,           ONLY: BytePackBE, ByteUnpackBE
    USE MClass_BaseDigest
    USE MClass_MDEngine

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: Luffa

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tIndex,   PARAMETER :: BlockLen = 32_kIndex
    tIndex,   PARAMETER :: DLen224  = 28_kIndex
    tIndex,   PARAMETER :: DLen256  = 32_kIndex
    tIndex,   PARAMETER :: DLen384  = 48_kIndex
    tIndex,   PARAMETER :: DLen512  = 64_kIndex
    tInteger, PARAMETER :: V_INIT(0:7,0:4) = RESHAPE([  &
        ToInt32(Z'6D251E69'), ToInt32(Z'44B051E0'), &
        ToInt32(Z'4EAA6FB4'), ToInt32(Z'DBF78465'), &
        ToInt32(Z'6E292011'), ToInt32(Z'90152DF4'), &
        ToInt32(Z'EE058139'), ToInt32(Z'DEF610BB'), &
        ToInt32(Z'C3B44B95'), ToInt32(Z'D9D2F256'), &
        ToInt32(Z'70EEE9A0'), ToInt32(Z'DE099FA3'), &
        ToInt32(Z'5D9B0557'), ToInt32(Z'8FC944B3'), &
        ToInt32(Z'CF1CCF0E'), ToInt32(Z'746CD581'), &
        ToInt32(Z'F7EFC89D'), ToInt32(Z'5DBA5781'), &
        ToInt32(Z'04016CE5'), ToInt32(Z'AD659C05'), &
        ToInt32(Z'0306194F'), ToInt32(Z'666D1836'), &
        ToInt32(Z'24AA230A'), ToInt32(Z'8B264AE7'), &
        ToInt32(Z'858075D5'), ToInt32(Z'36D79CCE'), &
        ToInt32(Z'E571F7D7'), ToInt32(Z'204B1F67'), &
        ToInt32(Z'35870C6A'), ToInt32(Z'57E9E923'), &
        ToInt32(Z'14BCB808'), ToInt32(Z'7CDE72CE'), &
        ToInt32(Z'6C68E9BE'), ToInt32(Z'5EC41E22'), &
        ToInt32(Z'C825B7C7'), ToInt32(Z'AFFB4363'), &
        ToInt32(Z'F5DF3999'), ToInt32(Z'0FC688F1'), &
        ToInt32(Z'B07224CC'), ToInt32(Z'03E86CEA')], [8, 5])
    tInteger, PARAMETER :: RC00(0:7) = [                &
        ToInt32(Z'303994A6'), ToInt32(Z'C0E65299'), &
        ToInt32(Z'6CC33A12'), ToInt32(Z'DC56983E'), &
        ToInt32(Z'1E00108F'), ToInt32(Z'7800423D'), &
        ToInt32(Z'8F5B7882'), ToInt32(Z'96E1DB12')]
    tInteger, PARAMETER :: RC04(0:7) = [                &
        ToInt32(Z'E0337818'), ToInt32(Z'441BA90D'), &
        ToInt32(Z'7F34D442'), ToInt32(Z'9389217F'), &
        ToInt32(Z'E5A8BCE6'), ToInt32(Z'5274BAF4'), &
        ToInt32(Z'26889BA7'), ToInt32(Z'9A226E9D')]
    tInteger, PARAMETER :: RC10(0:7) = [                &
        ToInt32(Z'B6DE10ED'), ToInt32(Z'70F47AAE'), &
        ToInt32(Z'0707A3D4'), ToInt32(Z'1C1E8F51'), &
        ToInt32(Z'707A3D45'), ToInt32(Z'AEB28562'), &
        ToInt32(Z'BACA1589'), ToInt32(Z'40A46F3E')]
    tInteger, PARAMETER :: RC14(0:7) = [                &
        ToInt32(Z'01685F3D'), ToInt32(Z'05A17CF4'), &
        ToInt32(Z'BD09CACA'), ToInt32(Z'F4272B28'), &
        ToInt32(Z'144AE5CC'), ToInt32(Z'FAA7AE2B'), &
        ToInt32(Z'2E48F1C1'), ToInt32(Z'B923C704')]
    tInteger, PARAMETER :: RC20(0:7) = [                &
        ToInt32(Z'FC20D9D2'), ToInt32(Z'34552E25'), &
        ToInt32(Z'7AD8818F'), ToInt32(Z'8438764A'), &
        ToInt32(Z'BB6DE032'), ToInt32(Z'EDB780C8'), &
        ToInt32(Z'D9847356'), ToInt32(Z'A2C78434')]
    tInteger, PARAMETER :: RC24(0:7) = [                &
        ToInt32(Z'E25E72C1'), ToInt32(Z'E623BB72'), &
        ToInt32(Z'5C58A4A4'), ToInt32(Z'1E38E2E7'), &
        ToInt32(Z'78E38B9D'), ToInt32(Z'27586719'), &
        ToInt32(Z'36EDA57F'), ToInt32(Z'703AACE7')]
    tInteger, PARAMETER :: RC30(0:7) = [                &
        ToInt32(Z'B213AFA5'), ToInt32(Z'C84EBE95'), &
        ToInt32(Z'4E608A22'), ToInt32(Z'56D858FE'), &
        ToInt32(Z'343B138F'), ToInt32(Z'D0EC4E3D'), &
        ToInt32(Z'2CEB4882'), ToInt32(Z'B3AD2208')]
    tInteger, PARAMETER :: RC34(0:7) = [                &
        ToInt32(Z'E028C9BF'), ToInt32(Z'44756F91'), &
        ToInt32(Z'7E8FCE32'), ToInt32(Z'956548BE'), &
        ToInt32(Z'FE191BE2'), ToInt32(Z'3CB226E5'), &
        ToInt32(Z'5944A28E'), ToInt32(Z'A1C4C355')]
    tInteger, PARAMETER :: RC40(0:7) = [                &
        ToInt32(Z'F0D2E9E3'), ToInt32(Z'AC11D7FA'), &
        ToInt32(Z'1BCB66F2'), ToInt32(Z'6F2D9BC9'), &
        ToInt32(Z'78602649'), ToInt32(Z'8EDAE952'), &
        ToInt32(Z'3B6BA548'), ToInt32(Z'EDAE9520')]
    tInteger, PARAMETER :: RC44(0:7) = [                &
        ToInt32(Z'5090D577'), ToInt32(Z'2D1925AB'), &
        ToInt32(Z'B46496AC'), ToInt32(Z'D1925AB0'), &
        ToInt32(Z'29131AB6'), ToInt32(Z'0FC053C3'), &
        ToInt32(Z'3F014F0C'), ToInt32(Z'FC053C31')]

!** DERIVED TYPE DEFINITIONS
    !> *Luffa* is a concrete *digest* type that implements an incremental
    !  cryptographic hash function based on the Luffa hash functions.
    TYPE, EXTENDS(MDEngine) :: Luffa
        PRIVATE
        !% buffer array used to store input data
        tByte               :: BufArr(0:BlockLen-1) = 0_kInt8
        !% state and its storage
        tInteger            :: Store(0:7,0:4) = 0
        tInteger, POINTER   :: State(:,:) => NULL()
        !% security strength in bits
        tInteger            :: Security = 256
        !% length of hash output in bytes
        tIndex              :: DigestLen = DLen256
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWSecurity* method to
        !  initialize the *digest* object with specified security.
        PROCEDURE, PRIVATE  :: InitializeWSecurity  => Luffa_Initialize_wSecurity
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (Luffa-256).
        PROCEDURE       :: Initialize   => Luffa_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => Luffa_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => Luffa_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => Luffa_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => Luffa_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => Luffa_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => Luffa_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => Luffa_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => Luffa_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => Luffa_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (Luffa-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the Luffa-512 algorithm <br>
        !   --->    CALL MD%Create(512) <br>
        GENERIC         :: Create       => InitializeWSecurity
        ! ---------------------------------------------------------------------
        FINAL           :: Luffa_Finalize
        ! ---------------------------------------------------------------------
    END TYPE Luffa

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Luffa_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Luffa), INTENT(INOUT) :: MD    !! 'Luffa' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%Create(256)

    RETURN

END SUBROUTINE Luffa_Initialize

!******************************************************************************

SUBROUTINE Luffa_Initialize_wSecurity(MD, Security)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified Security.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Luffa), TARGET, INTENT(INOUT) :: MD           !! 'Luffa' object
    tInteger,             INTENT(IN)    :: Security
    !^ Strength of security in bits with four possible values: 224, 256, 384 and 512.
    !  If the specified value is NOT valid, it is set to the default (256) value.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT CASE (Security)
    CASE (224, 256, 384, 512)
        MD%Security = Security
    CASE DEFAULT
        MD%Security = 256
    END SELECT

    SELECT CASE (MD%Security)
    CASE (224)
        MD%DigestLen = DLen224
    CASE (256)
        MD%DigestLen = DLen256
    CASE (384)
        MD%DigestLen = DLen384
    CASE (512)
        MD%DigestLen = DLen512
    END SELECT

    SELECT CASE (MD%DigestLen)
    CASE (28, 32)
!        MD%State(0:7,0:2) => MD%Store(0:7,0:2)
        MD%State(0:7,0:2) => MD%Store
    CASE (48)
!        MD%State(0:7,0:3) => MD%Store(0:7,0:3)
        MD%State(0:7,0:3) => MD%Store
    CASE (64)
!        MD%State(0:7,0:4) => MD%Store(0:7,0:4)
        MD%State(0:7,0:4) => MD%Store
    END SELECT

    CALL MD%Reset()

    RETURN

END SUBROUTINE Luffa_Initialize_wSecurity

!******************************************************************************

SUBROUTINE Luffa_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Luffa), INTENT(INOUT)  :: MD   !! 'Luffa' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    MD%BufArr = 0_kInt8
    IF (ASSOCIATED(MD%State)) THEN
        DO I = 0, SIZE(MD%State, DIM=2)-1
            MD%State(:,I) = V_INIT(:,I)
        END DO
    ELSE
        MD%Store = V_INIT
    END IF
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE Luffa_Reset

!******************************************************************************

SUBROUTINE Luffa_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Luffa),                   INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ALLOCATE(Luffa :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (Luffa)
        CALL Dst%Create(Src%Security)
        Dst%Store  = Src%Store
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT

    RETURN

END SUBROUTINE Luffa_GetClone

!******************************************************************************

FUNCTION Luffa_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Luffa), INTENT(IN)    :: MD       !! 'Luffa' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Name = 'Luffa-' // ToDecStrSigned(MD%Security)

    RETURN

END FUNCTION Luffa_GetName

!******************************************************************************

FUNCTION Luffa_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Luffa), INTENT(IN)    :: MD       !! 'Luffa' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Length = MD%DigestLen

    RETURN

END FUNCTION Luffa_GetDigestLen

!******************************************************************************

FUNCTION Luffa_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Luffa), INTENT(IN)    :: MD       !! 'Luffa' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION Luffa_GetBlockLen

!******************************************************************************

SUBROUTINE Luffa_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Luffa), TARGET, INTENT(INOUT) :: MD           !! 'Luffa' object
    tByte,       POINTER, INTENT(INOUT) :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE Luffa_SetBufPtr

!******************************************************************************

SUBROUTINE Luffa_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Luffa), INTENT(INOUT) :: MD           !! 'Luffa' object
    tByte,        INTENT(IN)    :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    SELECT CASE (MD%DigestLen)
    CASE (28, 32)
        CALL LuffaCore_Process3(MD%State(:,0), MD%State(:,1), MD%State(:,2), BytesIn)
    CASE (48)
        CALL LuffaCore_Process4(MD%State(:,0), MD%State(:,1), MD%State(:,2), &
                                MD%State(:,3), BytesIn)
    CASE (64)
        CALL LuffaCore_Process5(MD%State(:,0), MD%State(:,1), MD%State(:,2), &
                                MD%State(:,3), MD%State(:,4), BytesIn)
    END SELECT

    RETURN

END SUBROUTINE Luffa_ProcessBlock

!******************************************************************************

SUBROUTINE Luffa_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Luffa), INTENT(INOUT) :: MD           !! 'Luffa' object
    tByte,        INTENT(INOUT) :: BytesOut(0:) !! the output buffer
    tIndex,       INTENT(IN)    :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE Luffa_DoPadding

!******************************************************************************

SUBROUTINE Luffa_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Luffa), INTENT(INOUT) :: MD           !! 'Luffa' object
    tByte,        INTENT(IN)    :: LastByte     !! the last byte
    tByte,        INTENT(IN)    :: NBits        !! number of bits in the last byte
    tByte,        INTENT(INOUT) :: BytesOut(0:) !! the output buffer
    tIndex,       INTENT(IN)    :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte       :: TmpBuf(0:31)
    tIndex      :: Ptr, DLen
    tByte       :: Z

! FLOW

    Ptr = MD%GetBufLen()
    Z = SHIFTR(FByte80, NBits)
    TmpBuf(Ptr) = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
    TmpBuf(Ptr+1:31) = FByte00
    CALL MD%Update(TmpBuf, Ptr, 32_kIndex-Ptr)
    TmpBuf(0:Ptr) = FByte00
    CALL MD%Update(TmpBuf, 0_kIndex, 32_kIndex)
    DLen = MD%DigestLen
    SELECT CASE (DLen)
    CASE (28, 32)
        CALL  LuffaCore_GetOutput(MD, DLen, BytesOut, Offset, MD%State(:,0), &
                                  MD%State(:,1), MD%State(:,2))
    CASE (48)
        CALL  LuffaCore_GetOutput(MD, DLen, BytesOut, Offset, MD%State(:,0), &
                                  MD%State(:,1), MD%State(:,2), MD%State(:,3))
    CASE (64)
        CALL  LuffaCore_GetOutput(MD, DLen, BytesOut, Offset, MD%State(:,0),   &
                                  MD%State(:,1), MD%State(:,2), MD%State(:,3), &
                                  MD%State(:,4))
    END SELECT

    RETURN

CONTAINS

    SUBROUTINE LuffaCore_GetOutput(MD, DigestLen, BytesOut, Offset, VC0, VC1, VC2, VC3, VC4)

    !** PURPOSE OF THIS SUBROUTINE:
        ! To get output.

    !** SUBROUTINE ARGUMENT DECLARATIONS:
        CLASS(Luffa),       INTENT(INOUT)   :: MD
        tIndex,             INTENT(IN)      :: DigestLen
        tByte,              INTENT(INOUT)   :: BytesOut(0:)
        tIndex,             INTENT(IN)      :: Offset
        tInteger,           INTENT(INOUT)   :: VC0(0:7)
        tInteger,           INTENT(INOUT)   :: VC1(0:7)
        tInteger,           INTENT(INOUT)   :: VC2(0:7)
        tInteger, OPTIONAL, INTENT(INOUT)   :: VC3(0:7)
        tInteger, OPTIONAL, INTENT(INOUT)   :: VC4(0:7)

    !** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
        tByte       :: TmpBuf(0:31)
        tInteger    :: Output
        tIndex      :: I, CurOff

    !** SUBROUTINE MACRO DEFINITIONS:
#define IEOR_3(A, B, C)         IEOR(IEOR(A, B), C)
#define IEOR_4(A, B, C, D)      IEOR(IEOR_3(A, B, C), D)
#define IEOR_5(A, B, C, D, E)   IEOR(IEOR_4(A, B, C, D), E)

    ! FLOW

        SELECT CASE (DigestLen)
        CASE (28)
            CurOff = Offset
            DO I = 0, 6
                Output = IEOR_3(VC0(I), VC1(I), VC2(I))
                CALL ByteUnpackBE(Output, BytesOut, CurOff)
                CurOff = CurOff + 4_kIndex
            END DO
        CASE (32)
            CurOff = Offset
            DO I = 0, 7
                Output = IEOR_3(VC0(I), VC1(I), VC2(I))
                CALL ByteUnpackBE(Output, BytesOut, CurOff)
                CurOff = CurOff + 4_kIndex
            END DO
        CASE (48)
            CurOff = Offset
            DO I = 0, 7
                Output = IEOR_4(VC0(I), VC1(I), VC2(I), VC3(I))
                CALL ByteUnpackBE(Output, BytesOut, CurOff)
                CurOff = CurOff + 4_kIndex
            END DO
            TmpBuf = 0_kInt8
            CALL MD%Update(TmpBuf, 0_kIndex, 32_kIndex)
            DO I = 0, 3
                Output = IEOR_4(VC0(I), VC1(I), VC2(I), VC3(I))
                CALL ByteUnpackBE(Output, BytesOut, CurOff)
                CurOff = CurOff + 4_kIndex
            END DO
        CASE (64)
            CurOff = Offset
            DO I = 0, 7
                Output = IEOR_5(VC0(I), VC1(I), VC2(I), VC3(I), VC4(I))
                CALL ByteUnpackBE(Output, BytesOut, CurOff)
                CurOff = CurOff + 4_kIndex
            END DO
            TmpBuf = 0_kInt8
            CALL MD%Update(TmpBuf, 0_kIndex, 32_kIndex)
            DO I = 0, 7
                Output = IEOR_5(VC0(I), VC1(I), VC2(I), VC3(I), VC4(I))
                CALL ByteUnpackBE(Output, BytesOut, CurOff)
                CurOff = CurOff + 4_kIndex
            END DO
        END SELECT

        RETURN

#undef IEOR_3
#undef IEOR_4
#undef IEOR_5

    END SUBROUTINE LuffaCore_GetOutput

    !**************************************************************************

END SUBROUTINE Luffa_AddBitsNPad

!******************************************************************************

SUBROUTINE Luffa_Finalize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free a pointer component of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Luffa), INTENT(INOUT)  :: MD   !! 'Luffa' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    NULLIFY(MD%State)

    RETURN

END SUBROUTINE Luffa_Finalize

!******************************************************************************

SUBROUTINE LuffaCore_Process3(VC0, VC1, VC2, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: VC0(0:7)
    tInteger, INTENT(INOUT) :: VC1(0:7)
    tInteger, INTENT(INOUT) :: VC2(0:7)
    tByte,    INTENT(IN)    :: BytesIn(0:)  ! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: M(0:7)
    tInteger    :: A(0:7)
    tInteger    :: Tmp
    tIndex      :: I

!** SUBROUTINE MACRO DEFINITIONS:
#include    "Includes/Luffa_Def Macro.f90"

! FLOW

    ! MI3(BytesIn, VC0, VC1, VC2)
    CALL BytePackBE(BytesIn, 0_kIndex, M)
    XOR_ALL(A, VC0, VC1)
    XOR_ALL(A, A, VC2)
    M2(A, A)
    XOR_ALL(VC0, A, VC0)
    XOR_ALL(VC0, M, VC0)
    M2(M, M)
    XOR_ALL(VC1, A, VC1)
    XOR_ALL(VC1, M, VC1)
    M2(M, M)
    XOR_ALL(VC2, A, VC2)
    XOR_ALL(VC2, M, VC2)
    ! P3(VC0, VC1, VC2)
    TWEAK3(VC1, VC2)
    DO I = 0, 7
        SUB_CRUMB(VC0(0), VC0(1), VC0(2), VC0(3))
        SUB_CRUMB(VC0(5), VC0(6), VC0(7), VC0(4))
        MIX_WORD(VC0(0), VC0(4))
        MIX_WORD(VC0(1), VC0(5))
        MIX_WORD(VC0(2), VC0(6))
        MIX_WORD(VC0(3), VC0(7))
        VC0(0) = IEOR(VC0(0), RC00(I))
        VC0(4) = IEOR(VC0(4), RC04(I))
    END DO
    DO I = 0, 7
        SUB_CRUMB(VC1(0), VC1(1), VC1(2), VC1(3))
        SUB_CRUMB(VC1(5), VC1(6), VC1(7), VC1(4))
        MIX_WORD(VC1(0), VC1(4))
        MIX_WORD(VC1(1), VC1(5))
        MIX_WORD(VC1(2), VC1(6))
        MIX_WORD(VC1(3), VC1(7))
        VC1(0) = IEOR(VC1(0), RC10(I))
        VC1(4) = IEOR(VC1(4), RC14(I))
    END DO
    DO I = 0, 7
        SUB_CRUMB(VC2(0), VC2(1), VC2(2), VC2(3))
        SUB_CRUMB(VC2(5), VC2(6), VC2(7), VC2(4))
        MIX_WORD(VC2(0), VC2(4))
        MIX_WORD(VC2(1), VC2(5))
        MIX_WORD(VC2(2), VC2(6))
        MIX_WORD(VC2(3), VC2(7))
        VC2(0) = IEOR(VC2(0), RC20(I))
        VC2(4) = IEOR(VC2(4), RC24(I))
    END DO

    RETURN

#include    "Includes/Luffa_Undef Macro.f90"

END SUBROUTINE LuffaCore_Process3

!******************************************************************************

SUBROUTINE LuffaCore_Process4(VC0, VC1, VC2, VC3, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: VC0(0:7)
    tInteger, INTENT(INOUT) :: VC1(0:7)
    tInteger, INTENT(INOUT) :: VC2(0:7)
    tInteger, INTENT(INOUT) :: VC3(0:7)
    tByte,    INTENT(IN)    :: BytesIn(0:)  ! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: M(0:7)
    tInteger    :: A(0:7)
    tInteger    :: B(0:7)
    tInteger    :: Tmp
    tIndex      :: I

!** SUBROUTINE MACRO DEFINITIONS:
#include    "Includes/Luffa_Def Macro.f90"

! FLOW

    ! MI4(BytesIn, VC0, VC1, VC2, VC3)
    CALL BytePackBE(BytesIn, 0_kIndex, M)
    XOR_ALL(A, VC0, VC1)
    XOR_ALL(B, VC2, VC3)
    XOR_ALL(A, A, B)
    M2(A, A)
    XOR_ALL(VC0, A, VC0)
    XOR_ALL(VC1, A, VC1)
    XOR_ALL(VC2, A, VC2)
    XOR_ALL(VC3, A, VC3)
    M2(B, VC0)
    XOR_ALL(B, B, VC3)
    M2(VC3, VC3)
    XOR_ALL(VC3, VC3, VC2)
    M2(VC2, VC2)
    XOR_ALL(VC2, VC2, VC1)
    M2(VC1, VC1)
    XOR_ALL(VC1, VC1, VC0)
    XOR_ALL(VC0, B, M)
    M2(M, M)
    XOR_ALL(VC1, VC1, M)
    M2(M, M)
    XOR_ALL(VC2, VC2, M)
    M2(M, M)
    XOR_ALL(VC3, VC3, M)
    ! P4(VC0, VC1, VC2, VC3)
    TWEAK4(VC1, VC2, VC3)
    DO I = 0, 7
        SUB_CRUMB(VC0(0), VC0(1), VC0(2), VC0(3))
        SUB_CRUMB(VC0(5), VC0(6), VC0(7), VC0(4))
        MIX_WORD(VC0(0), VC0(4))
        MIX_WORD(VC0(1), VC0(5))
        MIX_WORD(VC0(2), VC0(6))
        MIX_WORD(VC0(3), VC0(7))
        VC0(0) = IEOR(VC0(0), RC00(I))
        VC0(4) = IEOR(VC0(4), RC04(I))
    END DO
    DO I = 0, 7
        SUB_CRUMB(VC1(0), VC1(1), VC1(2), VC1(3))
        SUB_CRUMB(VC1(5), VC1(6), VC1(7), VC1(4))
        MIX_WORD(VC1(0), VC1(4))
        MIX_WORD(VC1(1), VC1(5))
        MIX_WORD(VC1(2), VC1(6))
        MIX_WORD(VC1(3), VC1(7))
        VC1(0) = IEOR(VC1(0), RC10(I))
        VC1(4) = IEOR(VC1(4), RC14(I))
    END DO
    DO I = 0, 7
        SUB_CRUMB(VC2(0), VC2(1), VC2(2), VC2(3))
        SUB_CRUMB(VC2(5), VC2(6), VC2(7), VC2(4))
        MIX_WORD(VC2(0), VC2(4))
        MIX_WORD(VC2(1), VC2(5))
        MIX_WORD(VC2(2), VC2(6))
        MIX_WORD(VC2(3), VC2(7))
        VC2(0) = IEOR(VC2(0), RC20(I))
        VC2(4) = IEOR(VC2(4), RC24(I))
    END DO
    DO I = 0, 7
        SUB_CRUMB(VC3(0), VC3(1), VC3(2), VC3(3))
        SUB_CRUMB(VC3(5), VC3(6), VC3(7), VC3(4))
        MIX_WORD(VC3(0), VC3(4))
        MIX_WORD(VC3(1), VC3(5))
        MIX_WORD(VC3(2), VC3(6))
        MIX_WORD(VC3(3), VC3(7))
        VC3(0) = IEOR(VC3(0), RC30(I))
        VC3(4) = IEOR(VC3(4), RC34(I))
    END DO

    RETURN

#include    "Includes/Luffa_Undef Macro.f90"

END SUBROUTINE LuffaCore_Process4

!******************************************************************************

SUBROUTINE LuffaCore_Process5(VC0, VC1, VC2, VC3, VC4, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process one block of data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tInteger, INTENT(INOUT) :: VC0(0:7)
    tInteger, INTENT(INOUT) :: VC1(0:7)
    tInteger, INTENT(INOUT) :: VC2(0:7)
    tInteger, INTENT(INOUT) :: VC3(0:7)
    tInteger, INTENT(INOUT) :: VC4(0:7)
    tByte,    INTENT(IN)    :: BytesIn(0:)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: M(0:7)
    tInteger    :: A(0:7)
    tInteger    :: B(0:7)
    tInteger    :: Tmp
    tIndex      :: I

!** SUBROUTINE MACRO DEFINITIONS:
#include    "Includes/Luffa_Def Macro.f90"

! FLOW

    ! MI5(BytesIn, VC0, VC1, VC2, VC3, VC4)
    CALL BytePackBE(BytesIn, 0_kIndex, M)
    XOR_ALL(A, VC0, VC1)
    XOR_ALL(B, VC2, VC3)
    XOR_ALL(A, A, B)
    XOR_ALL(A, A, VC4)
    M2(A, A)
    XOR_ALL(VC0, A, VC0)
    XOR_ALL(VC1, A, VC1)
    XOR_ALL(VC2, A, VC2)
    XOR_ALL(VC3, A, VC3)
    XOR_ALL(VC4, A, VC4)
    M2(B, VC0)
    XOR_ALL(B, B, VC1)
    M2(VC1, VC1)
    XOR_ALL(VC1, VC1, VC2)
    M2(VC2, VC2)
    XOR_ALL(VC2, VC2, VC3)
    M2(VC3, VC3)
    XOR_ALL(VC3, VC3, VC4)
    M2(VC4, VC4)
    XOR_ALL(VC4, VC4, VC0)
    M2(VC0, B)
    XOR_ALL(VC0, VC0, VC4)
    M2(VC4, VC4)
    XOR_ALL(VC4, VC4, VC3)
    M2(VC3, VC3)
    XOR_ALL(VC3, VC3, VC2)
    M2(VC2, VC2)
    XOR_ALL(VC2, VC2, VC1)
    M2(VC1, VC1)
    XOR_ALL(VC1, VC1, B)
    XOR_ALL(VC0, VC0, M)
    M2(M, M)
    XOR_ALL(VC1, VC1, M)
    M2(M, M)
    XOR_ALL(VC2, VC2, M)
    M2(M, M)
    XOR_ALL(VC3, VC3, M)
    M2(M, M)
    XOR_ALL(VC4, VC4, M)
    ! P5(VC0, VC1, VC2, VC3, VC4)
    TWEAK5(VC1, VC2, VC3, VC4)
    DO I = 0, 7
        SUB_CRUMB(VC0(0), VC0(1), VC0(2), VC0(3))
        SUB_CRUMB(VC0(5), VC0(6), VC0(7), VC0(4))
        MIX_WORD(VC0(0), VC0(4))
        MIX_WORD(VC0(1), VC0(5))
        MIX_WORD(VC0(2), VC0(6))
        MIX_WORD(VC0(3), VC0(7))
        VC0(0) = IEOR(VC0(0), RC00(I))
        VC0(4) = IEOR(VC0(4), RC04(I))
    END DO
    DO I = 0, 7
        SUB_CRUMB(VC1(0), VC1(1), VC1(2), VC1(3))
        SUB_CRUMB(VC1(5), VC1(6), VC1(7), VC1(4))
        MIX_WORD(VC1(0), VC1(4))
        MIX_WORD(VC1(1), VC1(5))
        MIX_WORD(VC1(2), VC1(6))
        MIX_WORD(VC1(3), VC1(7))
        VC1(0) = IEOR(VC1(0), RC10(I))
        VC1(4) = IEOR(VC1(4), RC14(I))
    END DO
    DO I = 0, 7
        SUB_CRUMB(VC2(0), VC2(1), VC2(2), VC2(3))
        SUB_CRUMB(VC2(5), VC2(6), VC2(7), VC2(4))
        MIX_WORD(VC2(0), VC2(4))
        MIX_WORD(VC2(1), VC2(5))
        MIX_WORD(VC2(2), VC2(6))
        MIX_WORD(VC2(3), VC2(7))
        VC2(0) = IEOR(VC2(0), RC20(I))
        VC2(4) = IEOR(VC2(4), RC24(I))
    END DO
    DO I = 0, 7
        SUB_CRUMB(VC3(0), VC3(1), VC3(2), VC3(3))
        SUB_CRUMB(VC3(5), VC3(6), VC3(7), VC3(4))
        MIX_WORD(VC3(0), VC3(4))
        MIX_WORD(VC3(1), VC3(5))
        MIX_WORD(VC3(2), VC3(6))
        MIX_WORD(VC3(3), VC3(7))
        VC3(0) = IEOR(VC3(0), RC30(I))
        VC3(4) = IEOR(VC3(4), RC34(I))
    END DO
    DO I = 0, 7
        SUB_CRUMB(VC4(0), VC4(1), VC4(2), VC4(3))
        SUB_CRUMB(VC4(5), VC4(6), VC4(7), VC4(4))
        MIX_WORD(VC4(0), VC4(4))
        MIX_WORD(VC4(1), VC4(5))
        MIX_WORD(VC4(2), VC4(6))
        MIX_WORD(VC4(3), VC4(7))
        VC4(0) = IEOR(VC4(0), RC40(I))
        VC4(4) = IEOR(VC4(4), RC44(I))
    END DO

    RETURN

#include    "Includes/Luffa_Undef Macro.f90"

END SUBROUTINE LuffaCore_Process5

!******************************************************************************

END MODULE MClass_Luffa

!******************************************************************************
