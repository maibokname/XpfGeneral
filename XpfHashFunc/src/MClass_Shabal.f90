
MODULE MClass_Shabal

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Shabal* type and its related routines.
!   The *Shabal* type is a *digest* type that directly extends from the
!   <a href="../module/mclass_mdengine.html#type-mdengine">MDEngine</a>
!   type.  As a *concrete* derived type, it provides all remaining
!   deferred procedures required by all its abstract parent types. <br>
!   The *Shabal* type implements an incremental cryptographic hash
!   function by employing the *Shabal message-digest* algorithm [1].
!   The implementation here is mainly based on the references [2]. <br>
!   The *Shabal* type represents five cryptographic hash functions:
!   the *Shabal-192*,  *Shabal-224*, *Shabal-256*, *Shabal-384*, and
!   *Shabal-512* hash functions.  By default, the *Shabal* type
!   represents the *Shabal-256* hash function.  However, a user can
!   specify the *Security* argument (to one of the five applicable
!   values: 192, 224, 256, 384 and 512) when initializing the digest
!   object in order to use a different hash function and get a
!   different hash output size. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://web.archive.org/web/20170611041218mp_/http://www.shabal.com/">
!       SHABAL: A submission to NIST's Cryptographic Hash Algorithm Competition. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil,           ONLY: ToDecStrSigned
    USE MBase_BytePack,           ONLY: BytePackLE, ByteUnpackLE
    USE MClass_BaseDigest
    USE MClass_MDEngine

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: Shabal

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64
#include    "Includes/Shabal_Def Macro.f90"

!** MODULE PARAMETERS:
    tIndex,   PARAMETER :: BlockLen = 64_kIndex
    tIndex,   PARAMETER :: DLen192  = 24_kIndex
    tIndex,   PARAMETER :: DLen224  = 28_kIndex
    tIndex,   PARAMETER :: DLen256  = 32_kIndex
    tIndex,   PARAMETER :: DLen384  = 48_kIndex
    tIndex,   PARAMETER :: DLen512  = 64_kIndex
#include    "Includes/Shabal_Constants.f90"

!** DERIVED TYPE DEFINITIONS
    !> *Shabal* is a concrete *digest* type that implements an incremental
    !  cryptographic hash function based on the Shabal hash functions.
    TYPE, EXTENDS(MDEngine) :: Shabal
        PRIVATE
        !% buffer array used to store input data
        tByte       :: BufArr(0:BlockLen-1) = 0_kInt8
        !% states and counters
        tInteger    :: A(0:11) = AIV_256(0:11)
        tInteger    :: B(0:15) = BIV_256(0:15)
        tInteger    :: C(0:15) = CIV_256(0:15)
        tInteger    :: WLo = 1
        tInteger    :: WHi = 0
        !% security strength in bits
        tInteger    :: Security = 256
        !% length of hash output in bytes
        tIndex      :: DigestLen = DLen256
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWSecurity* method to
        !  initialize the *digest* object with specified security.
        PROCEDURE, PRIVATE  :: InitializeWSecurity  => Shabal_Initialize_wSecurity
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (Shabal-256).
        PROCEDURE       :: Initialize   => Shabal_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset        => Shabal_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => Shabal_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => Shabal_GetName
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen => Shabal_GetDigestLen
        !> *GetBlockLen* is a procedure to return the block length of the digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: GetBlockLen  => Shabal_GetBlockLen
        !> *SetBufPtr* is a procedure to set a pointer to the buffer array of this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: SetBufPtr    => Shabal_SetBufPtr
        !> *ProcessBlock* is a procedure to process one block of data for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: ProcessBlock => Shabal_ProcessBlock
        !> *DoPadding* is a procedure to perform padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: DoPadding    => Shabal_DoPadding
        !> *AddBitsNPad* is a procedure to add the final partial byte and then perform
        !  padding of the message for this digest. <br>
        !  This method is NOT intended to be used by a user.
        PROCEDURE       :: AddBitsNPad  => Shabal_AddBitsNPad
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (Shabal-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the Shabal-512 algorithm <br>
        !   --->    CALL MD%Create(512) <br>
        GENERIC         :: Create       => InitializeWSecurity
        ! ---------------------------------------------------------------------
    END TYPE Shabal

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Shabal_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Shabal), INTENT(INOUT)    :: MD    !! 'Shabal' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL MD%Create(256)
   
    RETURN

END SUBROUTINE Shabal_Initialize

!******************************************************************************

SUBROUTINE Shabal_Initialize_wSecurity(MD, Security)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified Security.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Shabal), TARGET, INTENT(INOUT)    :: MD           !! 'Shabal' object
    tInteger,              INTENT(IN)       :: Security
    !^ Strength of security in bits with five possible values: 192, 224, 256, 384 and 512.
    !  If the specified value is NOT valid, it is set to the default (256) value.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    SELECT CASE (Security)
    CASE (192, 224, 256, 384, 512)
        MD%Security = Security
    CASE DEFAULT
        MD%Security = 256
    END SELECT
    
    SELECT CASE (MD%Security)
    CASE (192)
        MD%DigestLen = DLen192
    CASE (224)
        MD%DigestLen = DLen224
    CASE (256)
        MD%DigestLen = DLen256
    CASE (384)
        MD%DigestLen = DLen384
    CASE (512)
        MD%DigestLen = DLen512
    END SELECT
    
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE Shabal_Initialize_wSecurity

!******************************************************************************

SUBROUTINE Shabal_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Shabal), INTENT(INOUT)    :: MD   !! 'Shabal' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    MD%BufArr = 0_kInt8
    SELECT CASE (MD%Security)
    CASE (192)
        MD%A = AIV_192
        MD%B = BIV_192
        MD%C = CIV_192
    CASE (224)
        MD%A = AIV_224
        MD%B = BIV_224
        MD%C = CIV_224
    CASE (256)
        MD%A = AIV_256
        MD%B = BIV_256
        MD%C = CIV_256
    CASE (384)
        MD%A = AIV_384
        MD%B = BIV_384
        MD%C = CIV_384
    CASE (512)
        MD%A = AIV_512
        MD%B = BIV_512
        MD%C = CIV_512
    END SELECT
    MD%WLo = 1
    MD%WHi = 0
    CALL MD%EngineReset()

    RETURN

END SUBROUTINE Shabal_Reset

!******************************************************************************

SUBROUTINE Shabal_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Shabal),                  INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(Shabal :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (Shabal)
        CALL Dst%Create(Src%Security)
        Dst%A      = Src%A
        Dst%B      = Src%B
        Dst%C      = Src%C
        Dst%WLo    = Src%WLo
        Dst%WHi    = Src%WHi
        Dst%BufArr = Src%BufArr
        CALL Src%EngineClone(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE Shabal_GetClone

!******************************************************************************

FUNCTION Shabal_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Shabal), INTENT(IN)   :: MD       !! 'Shabal' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Shabal-' // ToDecStrSigned(MD%Security)

    RETURN

END FUNCTION Shabal_GetName

!******************************************************************************

FUNCTION Shabal_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Shabal), INTENT(IN)   :: MD       !! 'Shabal' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Length = MD%DigestLen

    RETURN

END FUNCTION Shabal_GetDigestLen

!******************************************************************************

FUNCTION Shabal_GetBlockLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the block length for the hash function.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Shabal), INTENT(IN)   :: MD       !! 'Shabal' object
    tIndex                      :: Length   !! the block length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Length = BlockLen
    ASSOCIATE(Dummy => MD); END ASSOCIATE

    RETURN

END FUNCTION Shabal_GetBlockLen

!******************************************************************************

SUBROUTINE Shabal_SetBufPtr(MD, BufPtr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To set the pointer *BufPtr* to the actual buffer array with starting index of zero.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Shabal), TARGET, INTENT(INOUT)    :: MD           !! 'Shabal' object
    tByte,        POINTER, INTENT(INOUT)    :: BufPtr(:)    !! a buffer pointer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW

    BufPtr => MD%BufArr

    RETURN

END SUBROUTINE Shabal_SetBufPtr

!******************************************************************************

SUBROUTINE Shabal_ProcessBlock(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To process one block of data.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Shabal), INTENT(INOUT)    :: MD           !! 'Shabal' object
    tByte,         INTENT(IN)       :: BytesIn(0:)  !! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: MWords(0:15)
    tInteger    :: Temp
    tIndex      :: I

! FLOW

    CALL BytePackLE(BytesIn, 0_kIndex, MWords)
    
    INPUT_BLOCK_ADD(MD%B, MWords)
    XOR_W(MD%A, MD%WLo, MD%WHi)
    CALL ApplyPermutation(MD, MWords)
    INPUT_BLOCK_SUB(MD%C, MWords)
    SWAP_BC(MD%B, MD%C)
    INCR_W(MD%WLo, MD%WHi)

    RETURN

END SUBROUTINE Shabal_ProcessBlock

!******************************************************************************

SUBROUTINE ApplyPermutation(MD, MWords)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform permutation.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Shabal), INTENT(INOUT)    :: MD           !! 'Shabal' object
    tInteger,      INTENT(IN)       :: MWords(0:)   !! message block in words

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I

! FLOW

    STATE_ROTL(MD%B)
    PERM_STEP_0(MD%A, MD%B, MD%C, MWords)
    PERM_STEP_1(MD%A, MD%B, MD%C, MWords)
    PERM_STEP_2(MD%A, MD%B, MD%C, MWords)
    STATE_ADD(MD%A, MD%C)

    RETURN

END SUBROUTINE ApplyPermutation

!******************************************************************************

SUBROUTINE Shabal_DoPadding(MD, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform the final padding and store the result in the
    !  provided buffer.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Shabal), INTENT(INOUT)    :: MD           !! 'Shabal' object
    tByte,         INTENT(INOUT)    :: BytesOut(0:) !! the output buffer
    tIndex,        INTENT(IN)       :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! use the 'AddBitsNPad' method with no partial last byte
    CALL MD%AddBitsNPad(0_kInt8, 0_kInt8, BytesOut, Offset)

    RETURN

END SUBROUTINE Shabal_DoPadding

!******************************************************************************

SUBROUTINE Shabal_AddBitsNPad(MD, LastByte, NBits, BytesOut, Offset)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then perform the final padding and
    !  store the result in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Shabal), INTENT(INOUT)    :: MD           !! 'Shabal' object
    tByte,         INTENT(IN)       :: LastByte     !! the last byte
    tByte,         INTENT(IN)       :: NBits        !! number of bits in the last byte
    tByte,         INTENT(INOUT)    :: BytesOut(0:) !! the output buffer
    tIndex,        INTENT(IN)       :: Offset       !! the output offset

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: MWords(0:15)
    tInteger    :: Temp, BStart, OutWordSize
    tIndex      :: Ptr, I, J, CurrOff
    tByte       :: Z

! FLOW

    Ptr = MD%GetBufLen()
    Z = SHIFTR(FByte80, NBits)
    MD%BufArr(Ptr) = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
    MD%BufArr(Ptr+1:63) = FByte00
    CALL BytePackLE(MD%BufArr, 0_kIndex, MWords)
    INPUT_BLOCK_ADD(MD%B, MWords)
    XOR_W(MD%A, MD%WLo, MD%WHi)
    CALL ApplyPermutation(MD, MWords)
    DO J = 0, 2
        SWAP_BC(MD%B, MD%C)
        XOR_W(MD%A, MD%WLo, MD%WHi)
        CALL ApplyPermutation(MD, MWords)
    END DO

    OutWordSize = SHIFTR(MD%Security, 5)
    BStart = 16_kIndex - OutWordSize
    CurrOff = Offset
    DO I = BStart, 15_kIndex
        CALL ByteUnpackLE(MD%B(I), BytesOut, CurrOff)
        CurrOff = CurrOff + 4_kIndex
    END DO

    RETURN

END SUBROUTINE Shabal_AddBitsNPad

!******************************************************************************

#include    "Includes/Shabal_Undef Macro.f90"

END MODULE MClass_Shabal
    
!******************************************************************************
