
MODULE MClass_HamsiS

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *HamsiS* type and its related routines.
!   The *HamsiS* type is a *digest* type that extends directly from the
!   <a href="../module/mclass_basedigest.html#type-basedigest">BaseDigest</a>
!   type.  It implements all deferred procedures required by a digest type. <br>
!   The *HamsiS* type implements an incremental cryptographic hash function
!   by employing either the *Hamsi-224* or the *Hamsi-256 message-digest*
!   algorithm [1].  The implementation here is based mainly on the *SPHLIB*
!   implementation [2].  <br>
!   By default, the *HamsiS* type employs the *Hamsi-256 message-digest*
!   algorithm.  However, a user can specify the *IsHamsi224* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *Hamsi-224 message-digest* algorithm
!   instead of the default one. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://www.esat.kuleuven.be/cosic/publications/article-1203.pdf">
!       The Hash Function Hamsi. </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MemHandlers,  ONLY: MemAlloc
    USE MBase_SIntUtil,     ONLY: ToDecStrSigned
    USE MBase_BytePack,     ONLY: ByteUnpackBE
    USE MBase_ByteUtil,     ONLY: AnyType_2_ByteArrPtr, ByteArr_2_HexStr => ToHexStr_BE
    USE MClass_BaseDigest

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: HamsiS

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64
#include    "Includes/HamsiS_Def Macro.f90"

!** MODULE PARAMETERS:
#include    "Includes/HamsiS_Constants.f90"
    tByte,    PARAMETER :: FByte00 = ToInt8(Z'00')
    tByte,    PARAMETER :: FByte80 = ToInt8(Z'80')
    tByte,    PARAMETER :: FByteFF = ToInt8(Z'FF')
    tIndex,   PARAMETER :: DLen224  = 28_kIndex
    tIndex,   PARAMETER :: DLen256  = 32_kIndex
    tInteger, PARAMETER :: Alpha_N(0:31) = [                &
            ToInt32(Z'FF00F0F0'), ToInt32(Z'CCCCAAAA'), &
            ToInt32(Z'F0F0CCCC'), ToInt32(Z'FF00AAAA'), &
            ToInt32(Z'CCCCAAAA'), ToInt32(Z'F0F0FF00'), &
            ToInt32(Z'AAAACCCC'), ToInt32(Z'F0F0FF00'), &
            ToInt32(Z'F0F0CCCC'), ToInt32(Z'AAAAFF00'), &
            ToInt32(Z'CCCCFF00'), ToInt32(Z'AAAAF0F0'), &
            ToInt32(Z'AAAAF0F0'), ToInt32(Z'FF00CCCC'), &
            ToInt32(Z'CCCCF0F0'), ToInt32(Z'FF00AAAA'), &
            ToInt32(Z'CCCCAAAA'), ToInt32(Z'FF00F0F0'), &
            ToInt32(Z'FF00AAAA'), ToInt32(Z'F0F0CCCC'), &
            ToInt32(Z'F0F0FF00'), ToInt32(Z'CCCCAAAA'), &
            ToInt32(Z'F0F0FF00'), ToInt32(Z'AAAACCCC'), &
            ToInt32(Z'AAAAFF00'), ToInt32(Z'F0F0CCCC'), &
            ToInt32(Z'AAAAF0F0'), ToInt32(Z'CCCCFF00'), &
            ToInt32(Z'FF00CCCC'), ToInt32(Z'AAAAF0F0'), &
            ToInt32(Z'FF00AAAA'), ToInt32(Z'CCCCF0F0')]
    tInteger, PARAMETER :: Alpha_F(0:31) = [                &
            ToInt32(Z'CAF9639C'), ToInt32(Z'0FF0F9C0'), &
            ToInt32(Z'639C0FF0'), ToInt32(Z'CAF9F9C0'), &
            ToInt32(Z'0FF0F9C0'), ToInt32(Z'639CCAF9'), &
            ToInt32(Z'F9C00FF0'), ToInt32(Z'639CCAF9'), &
            ToInt32(Z'639C0FF0'), ToInt32(Z'F9C0CAF9'), &
            ToInt32(Z'0FF0CAF9'), ToInt32(Z'F9C0639C'), &
            ToInt32(Z'F9C0639C'), ToInt32(Z'CAF90FF0'), &
            ToInt32(Z'0FF0639C'), ToInt32(Z'CAF9F9C0'), &
            ToInt32(Z'0FF0F9C0'), ToInt32(Z'CAF9639C'), &
            ToInt32(Z'CAF9F9C0'), ToInt32(Z'639C0FF0'), &
            ToInt32(Z'639CCAF9'), ToInt32(Z'0FF0F9C0'), &
            ToInt32(Z'639CCAF9'), ToInt32(Z'F9C00FF0'), &
            ToInt32(Z'F9C0CAF9'), ToInt32(Z'639C0FF0'), &
            ToInt32(Z'F9C0639C'), ToInt32(Z'0FF0CAF9'), &
            ToInt32(Z'CAF90FF0'), ToInt32(Z'F9C0639C'), &
            ToInt32(Z'CAF9F9C0'), ToInt32(Z'0FF0639C')]
    tInteger, PARAMETER :: IV224(0:7) = [                   &
            ToInt32(Z'c3967a67'), ToInt32(Z'c3bc6c20'), &
            ToInt32(Z'4bc3bcc3'), ToInt32(Z'a7c3bc6b'), &
            ToInt32(Z'2c204b61'), ToInt32(Z'74686f6c'), &
            ToInt32(Z'69656b65'), ToInt32(Z'20556e69')]
    tInteger, PARAMETER :: IV256(0:7) = [                   &
            ToInt32(Z'76657273'), ToInt32(Z'69746569'), &
            ToInt32(Z'74204c65'), ToInt32(Z'7576656e'), &
            ToInt32(Z'2c204465'), ToInt32(Z'70617274'), &
            ToInt32(Z'656d656e'), ToInt32(Z'7420456c')]

!** DERIVED TYPE DEFINITIONS
    !> *HamsiS* is a concrete *digest* type that implements an incremental
    !  cryptographic hash function based on the Hamsi hash algorithms.
    TYPE, EXTENDS(BaseDigest) :: HamsiS
        PRIVATE
        !% state
        tInteger    :: State(0:7)   = IV256(0:7)
        !% buffer
        tByte       :: Partial(0:3) = 0_kInt8
        !% number of bytes used in the buffer
        tIndex      :: PartialLen   = 0_kIndex
        !% bit count
        tLong       :: BitCount     = 0_kInt64
        !% flag indicating whether the Hamsi-224 algorithm is employed or not.
        tLogical    :: IsHamsi224   = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: HamsiS_ByteDigest_AddBits
        PROCEDURE, PRIVATE  :: HamsiS_HexDigest_AddBits
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => HamsiS_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (HamsiS-256).
        PROCEDURE       :: Initialize           => HamsiS_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset                => HamsiS_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone             => HamsiS_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName              => HamsiS_GetName
        !> Use the *Update* method in place of the *InsertBytes* method to insert input
        !  data as an array of bytes (8-bit integers) where the offset (zero-based) into
        !  the array and the length of the input in bytes are specified.
        PROCEDURE       :: InsertBytes          => HamsiS_InsertBytes
        !> Use the *Update* method in place of the *InsertGen* method to insert
        !  input data in a generic way where the *Input* argument can be any type and
        !  any rank and the *InpSize* argument specifies the size of input data in bytes.
        PROCEDURE       :: InsertGen            => HamsiS_InsertGen
        !> Use the *Digest* method in place of the *ByteDigest* method to finalize the
        !  current hash computation and return the hash value as an array of bytes
        !  in a newly-allocated array.
        PROCEDURE       :: ByteDigest           => HamsiS_ByteDigest
        !> Use the *Digest* method in place of the *ByteDigest_wInput* method to insert final
        !  input in a generic way and then finalize the current hash computation and return
        !  the hash value as an array of bytes in a newly-allocated array.
        PROCEDURE       :: ByteDigest_wInput    => HamsiS_ByteDigest_wInput
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen         => HamsiS_GetDigestLen
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (Hamsi-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the Hamsi-224 algorithm <br>
        !   --->    CALL MD%Create(IsHamsi224=.TRUE.) <br>
        GENERIC         :: Create       => InitializeWFlag
        !> **Type-Bound Subroutine**: AddBitsNDigest <br>
        !  **Purpose**:  To add the last byte and then finalize the current hash computation
        !                and return the hash output. The object is reset. <br>
        !  **Usage**: <br>
        !   ! insert final input and return hash output as a byte array <br>
        !   --->    CALL MD%AddBitsNDigest(Input, InpSize, ByteArr) <br>
        !   ! insert final input and return hash output as a hexadecimal string <br>
        !   --->    CALL MD%AddBitsNDigest(Input, InpSize, HexStr) <br>
        !  **Note**: <br>
        !  This method is only used for an input message whose bit length is not
        !  a multiple of eight (i.e. a message with partial bytes). <br>
        !  The method is intended to be used by a digest type that implements a hash function
        !  that is an entrant of the SHA-3 competition.  It is mainly used for a test purpose.
        GENERIC     :: AddBitsNDigest           => HamsiS_ByteDigest_AddBits, &
                                                   HamsiS_HexDigest_AddBits
        ! ---------------------------------------------------------------------
    END TYPE HamsiS

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE HamsiS_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS), INTENT(INOUT)    :: MD    !! 'HamsiS' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the Hamsi-256 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE HamsiS_Initialize

!******************************************************************************

SUBROUTINE HamsiS_Initialize_wFlag(MD, IsHamsi224)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS), INTENT(INOUT)    :: MD           !! 'HamsiS' object
    tLogical,      INTENT(IN)       :: IsHamsi224
    !^ flag indicating whether the Hamsi-224 algorithm is employed or not. <br>
    !  - If true, use the Hamsi-224 algorithm. <br>
    !  - Otherwise, use the Hamsi-256 algorithm. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsHamsi224 = IsHamsi224
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE HamsiS_Initialize_wFlag

!******************************************************************************

SUBROUTINE HamsiS_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS), INTENT(INOUT)    :: MD   !! 'HamsiS' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (MD%IsHamsi224) THEN
        MD%State = IV224
    ELSE
        MD%State = IV256
    END IF
    MD%BitCount   = 0_kInt64
    MD%Partial    = 0_kInt8
    MD%PartialLen = 0_kIndex

    RETURN

END SUBROUTINE HamsiS_Reset

!******************************************************************************

SUBROUTINE HamsiS_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS),                  INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(HamsiS :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (HamsiS)
        CALL Dst%Create(Src%IsHamsi224)
        Dst%State      = Src%State
        Dst%BitCount   = Src%BitCount
        Dst%Partial    = Src%Partial
        Dst%PartialLen = Src%PartialLen
    END SELECT
        
    RETURN

END SUBROUTINE HamsiS_GetClone

!******************************************************************************

FUNCTION HamsiS_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS), INTENT(IN)   :: MD       !! 'HamsiS' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsHamsi224) THEN
        Name = 'Hamsi-224'
    ELSE
        Name = 'Hamsi-256'
    END IF

    RETURN

END FUNCTION HamsiS_GetName

!******************************************************************************

FUNCTION HamsiS_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS), INTENT(IN)   :: MD       !! 'HamsiS' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (MD%IsHamsi224) THEN
        Length = DLen224
    ELSE
        Length = DLen256
    END IF

    RETURN

END FUNCTION HamsiS_GetDigestLen

!******************************************************************************

SUBROUTINE HamsiS_InsertBytes(MD, ByteArr, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data as a byte (8-bit integer) array where offset (zero-based)
    !  and length are specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS), INTENT(INOUT)    :: MD             !! 'HamsiS' object
    tByte,         INTENT(IN)       :: ByteArr(0:)    !! a byte array of input data
    tIndex,        INTENT(IN)       :: Offset         !! the offset in input data
    tIndex,        INTENT(IN)       :: Length         !! the length of input data in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MLen, CurPos, CurLen

! FLOW

    CurLen = Length
    CurPos = Offset
    IF (MD%PartialLen /= 0_kIndex) THEN

        MLen = 4_kIndex - MD%PartialLen
        IF (CurLen < MLen) THEN
            MD%Partial(MD%PartialLen:MD%PartialLen+CurLen-1) = ByteArr(CurPos:CurPos+CurLen-1)
            MD%PartialLen = MD%PartialLen + CurLen
            RETURN
        ELSE
            MD%Partial(MD%PartialLen:MD%PartialLen+MLen-1) = ByteArr(CurPos:CurPos+MLen-1)
            CurLen = CurLen - MLen
            CurPos = CurPos + MLen
            CALL HamsiS_Process(MD, MD%Partial, 0_kIndex, 1_kIndex)
            MD%PartialLen = 0_kIndex
        END IF
    END IF

    CALL HamsiS_Process(MD, ByteArr, CurPos, SHIFTR(CurLen, 2))
    CurPos = CurPos + IAND(CurLen, NOT(3_kIndex))
    CurLen = IAND(CurLen, 3_kIndex)
    MD%Partial(0:CurLen-1) = ByteArr(CurPos:CurPos+CurLen-1)
    MD%PartialLen = CurLen

    RETURN

END SUBROUTINE HamsiS_InsertBytes

!******************************************************************************

SUBROUTINE HamsiS_InsertGen(MD, Input, InpSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data in a generic way where the *Input* argument can be
    !  any type and any rank and the *InpSize* argument specifies the size of
    !  the input data in a number of bytes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS),          INTENT(INOUT)   :: MD       !! 'HamsiS' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)!! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize  !! size of the input (in bytes)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: InpPtr(:)

! FLOW
    
    ! set pointer to the input
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)
    
    ! insert the input data as a byte array
    CALL MD%Update(InpPtr, 0_kIndex, InpSize)

    NULLIFY(InpPtr)

    RETURN

END SUBROUTINE HamsiS_InsertGen

!******************************************************************************

SUBROUTINE HamsiS_ByteDigest(MD, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes in a newly-allocated array.
    !  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS),      INTENT(INOUT)   :: MD           !! 'HamsiS' object
    tByte, ALLOCATABLE, INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%AddBitsNDigest(0_kInt8, 0_kInt8, ByteArr)

    RETURN

END SUBROUTINE HamsiS_ByteDigest

!******************************************************************************

SUBROUTINE HamsiS_ByteDigest_wInput(MD, Input, InpSize, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash
    !  computation and return the hash value as an array of (8-bit integer) bytes
    !  in a newly-allocated array.  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS),          INTENT(INOUT)   :: MD           !! 'HamsiS' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tByte,   ALLOCATABLE,   INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%Update(Input, InpSize)
    CALL MD%Digest(ByteArr)
        
    RETURN

END SUBROUTINE HamsiS_ByteDigest_wInput

!******************************************************************************

SUBROUTINE HamsiS_ByteDigest_AddBits(MD, LastByte, NBits, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then finalize the current hash computation
    !  and return the hash output as a byte array. <br>
    !  This routine is only used for an input message whose bit length is not
    !  a multiple of eight (i.e. a message with partial bytes). <br>
    !  This procedure is intended to be used by a digest type that implements
    !  a hash function that is an entrant of the SHA-3 competition.  It is mainly
    !  used for a test purpose.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS),      INTENT(INOUT)   :: MD           !! 'HamsiS' object
    tByte,              INTENT(IN)      :: LastByte     !! the last byte
    tByte,              INTENT(IN)      :: NBits        !! number of bits in the last byte
    tByte, ALLOCATABLE, INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, Ptr
    tByte       :: Z
    tByte       :: Pad(0:11)

! FLOW
    
    Ptr = MD%PartialLen
    Pad(0:Ptr-1) = MD%Partial(0:Ptr-1)
    CALL ByteUnpackBE(MD%BitCount + SHIFTL(Ptr, 3) + ToInt64(NBits), Pad, 4_kIndex)
    Z = SHIFTR(FByte80, NBits)
    Pad(Ptr) = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
    Ptr = Ptr + 1_kIndex
    DO WHILE (Ptr < 4_kIndex)
        Pad(Ptr) = FByte00
        Ptr = Ptr + 1_kIndex
    END DO
    CALL HamsiS_Process(MD, Pad, 0_kIndex, 2_kIndex)
    CALL HamsiS_Final(MD, Pad(8:))
    
    ! get output
    CALL MemAlloc(ByteArr, MD%GetDigestLen(), StartID=0_kIndex)
    DO I = 0, (SHIFTR(MD%GetDigestLen(), 2) - 1)
        CALL ByteUnpackBE(MD%State(I), ByteArr, SHIFTL(I, 2))
    END DO
    
    ! reset the states
    CALL MD%Reset()

    RETURN

END SUBROUTINE HamsiS_ByteDigest_AddBits

!******************************************************************************

SUBROUTINE HamsiS_HexDigest_AddBits(MD, LastByte, NBits, HexStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then finalize the current hash computation
    !  and return the hash output as a hexadecimal string. <br>
    !  This routine is only used for an input message whose bit length is not
    !  a multiple of eight (i.e. a message with partial bytes). <br>
    !  This procedure is intended to be used by a digest type that implements
    !  a hash function that is an entrant of the SHA-3 competition.  It is mainly
    !  used for a test purpose.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS), INTENT(INOUT)    :: MD           !! 'HamsiS' object
    tByte,         INTENT(IN)       :: LastByte     !! the last byte
    tByte,         INTENT(IN)       :: NBits        !! number of bits in the last byte
    tCharAlloc,    INTENT(OUT)      :: HexStr       !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, ALLOCATABLE  :: ByteArr(:)

! FLOW

    CALL MD%AddBitsNDigest(LastByte, NBits, ByteArr)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)
        
    RETURN

END SUBROUTINE HamsiS_HexDigest_AddBits

!******************************************************************************

SUBROUTINE HamsiS_Process(MD, Buffer, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process input data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS), INTENT(INOUT)    :: MD          ! 'HamsiS' object
    tByte,         INTENT(IN)       :: Buffer(0:)  ! the input data buffer
    tIndex,        INTENT(IN)       :: Offset      ! input offset
    tIndex,        INTENT(IN)       :: Length      ! input length to be processed (in words?)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Indx(0:3)
    tInteger    :: VC(0:7), VM(0:7), T
    tIndex      :: Num, CurID, I

! FLOW

    ! initialize
    MD%BitCount = MD%BitCount + SHIFTL(ToInt64(Length), 5)
    Num   = Length
    CurID = Offset
    READ_STATE_SMALL(VC)
    
    DO WHILE (Num > 0)
        Num = Num - 1
        Indx(0) = GetIndex(Buffer(CurID))
        Indx(1) = GetIndex(Buffer(CurID+1))
        Indx(2) = GetIndex(Buffer(CurID+2))
        Indx(3) = GetIndex(Buffer(CurID+3))
        INPUT_SMALL(VM, Indx)
        ROUND_SMALL(0, Alpha_N)
        ROUND_SMALL(1, Alpha_N)
        ROUND_SMALL(2, Alpha_N)
        T_SMALL(VC)
        CurID = CurID + 4
    END DO
    WRITE_STATE_SMALL(VC)

    RETURN

END SUBROUTINE HamsiS_Process

!******************************************************************************

SUBROUTINE HamsiS_Final(MD, Buffer)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process final input data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiS), INTENT(INOUT)    :: MD          ! 'HamsiS' object
    tByte,         INTENT(IN)       :: Buffer(0:)  ! the input data buffer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Indx(0:3)
    tInteger    :: VC(0:7), VM(0:7), T
    tIndex      :: I

! FLOW

    READ_STATE_SMALL(VC)
    Indx(0) = GetIndex(Buffer(0))
    Indx(1) = GetIndex(Buffer(1))
    Indx(2) = GetIndex(Buffer(2))
    Indx(3) = GetIndex(Buffer(3))
    INPUT_SMALL(VM, Indx)
    ROUND_SMALL(0, Alpha_F)
    ROUND_SMALL(1, Alpha_F)
    ROUND_SMALL(2, Alpha_F)
    ROUND_SMALL(3, Alpha_F)
    ROUND_SMALL(4, Alpha_F)
    ROUND_SMALL(5, Alpha_F)
    T_SMALL(VC)
    WRITE_STATE_SMALL(VC)
    
    RETURN

END SUBROUTINE HamsiS_Final

!******************************************************************************

#include    "Includes/HamsiS_Undef Macro.f90"

END MODULE MClass_HamsiS
    
!******************************************************************************
