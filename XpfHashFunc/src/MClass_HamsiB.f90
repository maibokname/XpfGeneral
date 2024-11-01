
MODULE MClass_HamsiB

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *HamsiB* type and its related routines.
!   The *HamsiB* type is a *digest* type that extends directly from the
!   <a href="../module/mclass_basedigest.html#type-basedigest">BaseDigest</a>
!   type.  It implements all deferred procedures required by a digest type. <br>
!   The *HamsiB* type implements an incremental cryptographic hash function
!   by employing either the *Hamsi-384* or the *Hamsi-512 message-digest*
!   algorithm [1].  The implementation here is based mainly on the *SPHLIB*
!   implementation [2].  <br>
!   By default, the *HamsiB* type employs the *Hamsi-512 message-digest*
!   algorithm.  However, a user can specify the *IsHamsi384* flag to
!   true when initializing the digest object (by calling the *Create*
!   method) in order to use the *Hamsi-384 message-digest* algorithm
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
    PUBLIC :: HamsiB

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64
#include    "Includes/HamsiB_Def Macro.f90"

!** MODULE PARAMETERS:
#include    "Includes/HamsiB_Constants.f90"
    tByte,    PARAMETER :: FByte00 = ToInt8(Z'00')
    tByte,    PARAMETER :: FByte80 = ToInt8(Z'80')
    tByte,    PARAMETER :: FByteFF = ToInt8(Z'FF')
    tIndex,   PARAMETER :: DLen384  = 48_kIndex
    tIndex,   PARAMETER :: DLen512  = 64_kIndex
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
    tInteger, PARAMETER :: IV384(0:15) = [                  &
            ToInt32(Z'656B7472'), ToInt32(Z'6F746563'), &
            ToInt32(Z'686E6965'), ToInt32(Z'6B2C2043'), &
            ToInt32(Z'6F6D7075'), ToInt32(Z'74657220'), &
            ToInt32(Z'53656375'), ToInt32(Z'72697479'), &
            ToInt32(Z'20616E64'), ToInt32(Z'20496E64'), &
            ToInt32(Z'75737472'), ToInt32(Z'69616C20'), &
            ToInt32(Z'43727970'), ToInt32(Z'746F6772'), &
            ToInt32(Z'61706879'), ToInt32(Z'2C204B61')]
    tInteger, PARAMETER :: IV512(0:15) = [                  &
            ToInt32(Z'73746565'), ToInt32(Z'6C706172'), &
            ToInt32(Z'6B204172'), ToInt32(Z'656E6265'), &
            ToInt32(Z'72672031'), ToInt32(Z'302C2062'), &
            ToInt32(Z'75732032'), ToInt32(Z'3434362C'), &
            ToInt32(Z'20422D33'), ToInt32(Z'30303120'), &
            ToInt32(Z'4C657576'), ToInt32(Z'656E2D48'), &
            ToInt32(Z'65766572'), ToInt32(Z'6C65652C'), &
            ToInt32(Z'2042656C'), ToInt32(Z'6769756D')]

!** DERIVED TYPE DEFINITIONS
    !> *HamsiB* is a concrete *digest* type that implements an incremental
    !  cryptographic hash function based on the Hamsi hash algorithms.
    TYPE, EXTENDS(BaseDigest) :: HamsiB
        PRIVATE
        !% state
        tInteger    :: State(0:15)  = IV512(0:15)
        !% buffer
        tByte       :: Partial(0:7) = 0_kInt8
        !% number of bytes used in the buffer
        tIndex      :: PartialLen   = 0_kIndex
        !% bit count
        tLong       :: BitCount     = 0_kInt64
        !% flag indicating whether the Hamsi-384 algorithm is employed or not.
        tLogical    :: IsHamsi384   = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: HamsiB_ByteDigest_AddBits
        PROCEDURE, PRIVATE  :: HamsiB_HexDigest_AddBits
        !> Use the *Create* method in place of the *InitializeWFlag* method to
        !  initialize the *digest* object with specified flag.
        PROCEDURE, PRIVATE  :: InitializeWFlag  => HamsiB_Initialize_wFlag
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (HamsiB-512).
        PROCEDURE       :: Initialize           => HamsiB_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset                => HamsiB_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone             => HamsiB_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName              => HamsiB_GetName
        !> Use the *Update* method in place of the *InsertBytes* method to insert input
        !  data as an array of bytes (8-bit integers) where the offset (zero-based) into
        !  the array and the length of the input in bytes are specified.
        PROCEDURE       :: InsertBytes          => HamsiB_InsertBytes
        !> Use the *Update* method in place of the *InsertGen* method to insert
        !  input data in a generic way where the *Input* argument can be any type and
        !  any rank and the *InpSize* argument specifies the size of input data in bytes.
        PROCEDURE       :: InsertGen            => HamsiB_InsertGen
        !> Use the *Digest* method in place of the *ByteDigest* method to finalize the
        !  current hash computation and return the hash value as an array of bytes
        !  in a newly-allocated array.
        PROCEDURE       :: ByteDigest           => HamsiB_ByteDigest
        !> Use the *Digest* method in place of the *ByteDigest_wInput* method to insert final
        !  input in a generic way and then finalize the current hash computation and return
        !  the hash value as an array of bytes in a newly-allocated array.
        PROCEDURE       :: ByteDigest_wInput    => HamsiB_ByteDigest_wInput
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen         => HamsiB_GetDigestLen
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (Hamsi-512) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the Hamsi-384 algorithm <br>
        !   --->    CALL MD%Create(IsHamsi384=.TRUE.) <br>
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
        GENERIC     :: AddBitsNDigest           => HamsiB_ByteDigest_AddBits, &
                                                   HamsiB_HexDigest_AddBits
        ! ---------------------------------------------------------------------
    END TYPE HamsiB

!** INTERFACE DEFINITIONS:
    ! na

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE HamsiB_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB), INTENT(INOUT)    :: MD    !! 'HamsiB' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the Hamsi-512 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE HamsiB_Initialize

!******************************************************************************

SUBROUTINE HamsiB_Initialize_wFlag(MD, IsHamsi384)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB), INTENT(INOUT)    :: MD           !! 'HamsiB' object
    tLogical,      INTENT(IN)       :: IsHamsi384
    !^ flag indicating whether the Hamsi-384 algorithm is employed or not. <br>
    !  - If true, use the Hamsi-384 algorithm. <br>
    !  - Otherwise, use the Hamsi-512 algorithm. <br>

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    MD%IsHamsi384 = IsHamsi384
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE HamsiB_Initialize_wFlag

!******************************************************************************

SUBROUTINE HamsiB_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB), INTENT(INOUT)    :: MD   !! 'HamsiB' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    IF (MD%IsHamsi384) THEN
        MD%State = IV384
    ELSE
        MD%State = IV512
    END IF
    MD%BitCount   = 0_kInt64
    MD%Partial    = 0_kInt8
    MD%PartialLen = 0_kIndex

    RETURN

END SUBROUTINE HamsiB_Reset

!******************************************************************************

SUBROUTINE HamsiB_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB),                  INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(HamsiB :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (HamsiB)
        CALL Dst%Create(Src%IsHamsi384)
        Dst%State      = Src%State
        Dst%BitCount   = Src%BitCount
        Dst%Partial    = Src%Partial
        Dst%PartialLen = Src%PartialLen
    END SELECT
        
    RETURN

END SUBROUTINE HamsiB_GetClone

!******************************************************************************

FUNCTION HamsiB_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB), INTENT(IN)   :: MD       !! 'HamsiB' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsHamsi384) THEN
        Name = 'Hamsi-384'
    ELSE
        Name = 'Hamsi-512'
    END IF

    RETURN

END FUNCTION HamsiB_GetName

!******************************************************************************

FUNCTION HamsiB_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB), INTENT(IN)   :: MD       !! 'HamsiB' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    IF (MD%IsHamsi384) THEN
        Length = DLen384
    ELSE
        Length = DLen512
    END IF

    RETURN

END FUNCTION HamsiB_GetDigestLen

!******************************************************************************

SUBROUTINE HamsiB_InsertBytes(MD, ByteArr, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data as a byte (8-bit integer) array where offset (zero-based)
    !  and length are specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB), INTENT(INOUT)    :: MD             !! 'HamsiB' object
    tByte,         INTENT(IN)       :: ByteArr(0:)    !! a byte array of input data
    tIndex,        INTENT(IN)       :: Offset         !! the offset in input data
    tIndex,        INTENT(IN)       :: Length         !! the length of input data in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: MLen, CurPos, CurLen

! FLOW

    CurLen = Length
    CurPos = Offset
    IF (MD%PartialLen /= 0_kIndex) THEN

        MLen = 8_kIndex - MD%PartialLen
        IF (CurLen < MLen) THEN
            MD%Partial(MD%PartialLen:MD%PartialLen+CurLen-1) = ByteArr(CurPos:CurPos+CurLen-1)
            MD%PartialLen = MD%PartialLen + CurLen
            RETURN
        ELSE
            MD%Partial(MD%PartialLen:MD%PartialLen+MLen-1) = ByteArr(CurPos:CurPos+MLen-1)
            CurLen = CurLen - MLen
            CurPos = CurPos + MLen
            CALL HamsiB_Process(MD, MD%Partial, 0_kIndex, 1_kIndex)
            MD%PartialLen = 0_kIndex
        END IF
    END IF

    CALL HamsiB_Process(MD, ByteArr, CurPos, SHIFTR(CurLen, 3))
    CurPos = CurPos + IAND(CurLen, NOT(7_kIndex))
    CurLen = IAND(CurLen, 7_kIndex)
    MD%Partial(0:CurLen-1) = ByteArr(CurPos:CurPos+CurLen-1)
    MD%PartialLen = CurLen

    RETURN

END SUBROUTINE HamsiB_InsertBytes

!******************************************************************************

SUBROUTINE HamsiB_InsertGen(MD, Input, InpSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data in a generic way where the *Input* argument can be
    !  any type and any rank and the *InpSize* argument specifies the size of
    !  the input data in a number of bytes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB),          INTENT(INOUT)   :: MD       !! 'HamsiB' object
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

END SUBROUTINE HamsiB_InsertGen

!******************************************************************************

SUBROUTINE HamsiB_ByteDigest(MD, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes in a newly-allocated array.
    !  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB),      INTENT(INOUT)   :: MD           !! 'HamsiB' object
    tByte, ALLOCATABLE, INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%AddBitsNDigest(0_kInt8, 0_kInt8, ByteArr)

    RETURN

END SUBROUTINE HamsiB_ByteDigest

!******************************************************************************

SUBROUTINE HamsiB_ByteDigest_wInput(MD, Input, InpSize, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash
    !  computation and return the hash value as an array of (8-bit integer) bytes
    !  in a newly-allocated array.  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB),          INTENT(INOUT)   :: MD           !! 'HamsiB' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tByte,   ALLOCATABLE,   INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%Update(Input, InpSize)
    CALL MD%Digest(ByteArr)
        
    RETURN

END SUBROUTINE HamsiB_ByteDigest_wInput

!******************************************************************************

SUBROUTINE HamsiB_ByteDigest_AddBits(MD, LastByte, NBits, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then finalize the current hash computation
    !  and return the hash output as a byte array. <br>
    !  This routine is only used for an input message whose bit length is not
    !  a multiple of eight (i.e. a message with partial bytes). <br>
    !  This procedure is intended to be used by a digest type that implements
    !  a hash function that is an entrant of the SHA-3 competition.  It is mainly
    !  used for a test purpose.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB),      INTENT(INOUT)   :: MD           !! 'HamsiB' object
    tByte,              INTENT(IN)      :: LastByte     !! the last byte
    tByte,              INTENT(IN)      :: NBits        !! number of bits in the last byte
    tByte, ALLOCATABLE, INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I, Ptr
    tByte       :: Z
    tByte       :: Pad(0:7)

! FLOW

    Ptr = MD%PartialLen
    CALL ByteUnpackBE(MD%BitCount + SHIFTL(Ptr, 3) + ToInt64(NBits), Pad, 0_kIndex)
    Z = SHIFTR(FByte80, NBits)
    MD%Partial(Ptr) = IAND(IOR(IAND(LastByte, -Z), Z), FByteFF)
    Ptr = Ptr + 1_kIndex
    DO WHILE (Ptr < 8_kIndex)
        MD%Partial(Ptr) = FByte00
        Ptr = Ptr + 1_kIndex
    END DO
    CALL HamsiB_Process(MD, MD%Partial, 0_kIndex, 1_kIndex)
    CALL HamsiB_Final(MD, Pad)
    
    ! get output
    CALL MemAlloc(ByteArr, MD%GetDigestLen(), StartID=0_kIndex)
    IF (MD%IsHamsi384) THEN
        CALL ByteUnpackBE(MD%State(0),  ByteArr, 0_kIndex)
        CALL ByteUnpackBE(MD%State(1),  ByteArr, 4_kIndex)
        CALL ByteUnpackBE(MD%State(3),  ByteArr, 8_kIndex)
        CALL ByteUnpackBE(MD%State(4),  ByteArr, 12_kIndex)
        CALL ByteUnpackBE(MD%State(5),  ByteArr, 16_kIndex)
        CALL ByteUnpackBE(MD%State(6),  ByteArr, 20_kIndex)
        CALL ByteUnpackBE(MD%State(8),  ByteArr, 24_kIndex)
        CALL ByteUnpackBE(MD%State(9),  ByteArr, 28_kIndex)
        CALL ByteUnpackBE(MD%State(10), ByteArr, 32_kIndex)
        CALL ByteUnpackBE(MD%State(12), ByteArr, 36_kIndex)
        CALL ByteUnpackBE(MD%State(13), ByteArr, 40_kIndex)
        CALL ByteUnpackBE(MD%State(15), ByteArr, 44_kIndex)
    ELSE
        DO I = 0, (SHIFTR(MD%GetDigestLen(), 2) - 1)
            CALL ByteUnpackBE(MD%State(I), ByteArr, SHIFTL(I, 2))
        END DO
    END IF

    ! reset the states
    CALL MD%Reset()

    RETURN

END SUBROUTINE HamsiB_ByteDigest_AddBits

!******************************************************************************

SUBROUTINE HamsiB_HexDigest_AddBits(MD, LastByte, NBits, HexStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then finalize the current hash computation
    !  and return the hash output as a hexadecimal string. <br>
    !  This routine is only used for an input message whose bit length is not
    !  a multiple of eight (i.e. a message with partial bytes). <br>
    !  This procedure is intended to be used by a digest type that implements
    !  a hash function that is an entrant of the SHA-3 competition.  It is mainly
    !  used for a test purpose.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB), INTENT(INOUT)    :: MD           !! 'HamsiB' object
    tByte,         INTENT(IN)       :: LastByte     !! the last byte
    tByte,         INTENT(IN)       :: NBits        !! number of bits in the last byte
    tCharAlloc,    INTENT(OUT)      :: HexStr       !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, ALLOCATABLE  :: ByteArr(:)

! FLOW

    CALL MD%AddBitsNDigest(LastByte, NBits, ByteArr)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)
        
    RETURN

END SUBROUTINE HamsiB_HexDigest_AddBits

!******************************************************************************

SUBROUTINE HamsiB_Process(MD, Buffer, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process input data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB), INTENT(INOUT)    :: MD          ! 'HamsiB' object
    tByte,         INTENT(IN)       :: Buffer(0:)  ! the input data buffer
    tIndex,        INTENT(IN)       :: Offset      ! input offset
    tIndex,        INTENT(IN)       :: Length      ! input length to be processed (in words?)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Indx(0:7)
    tInteger    :: VC(0:15), VM(0:15), T
    tIndex      :: Num, CurID, I

! FLOW

    ! initialize
    MD%BitCount = MD%BitCount + SHIFTL(ToInt64(Length), 6)
    Num   = Length
    CurID = Offset
    READ_STATE_BIG(VC)
    
    DO WHILE (Num > 0)
        Num = Num - 1
        DO I = 0, 7
            Indx(I) = GetIndex(Buffer(CurID+I))
        END DO
        INPUT_BIG(VM, Indx)
        DO I = 0, 5
            ROUND_BIG_INIT(I, Alpha_N)
            ROUND_BIG_SBOX
            ROUND_BIG_LBOX
        END DO
        T_BIG(VC)
        CurID = CurID + 8
    END DO
    WRITE_STATE_BIG(VC)

    RETURN

END SUBROUTINE HamsiB_Process

!******************************************************************************

SUBROUTINE HamsiB_Final(MD, Buffer)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process final input data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(HamsiB), INTENT(INOUT)    :: MD          ! 'HamsiB' object
    tByte,         INTENT(IN)       :: Buffer(0:)  ! the input data buffer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Indx(0:7)
    tInteger    :: VC(0:15), VM(0:15), T
    tIndex      :: I

! FLOW

    READ_STATE_BIG(VC)
    DO I = 0, 7
        Indx(I) = GetIndex(Buffer(I))
    END DO
    INPUT_BIG(VM, Indx)
    DO I = 0, 11
        ROUND_BIG_INIT(I, Alpha_F)
        ROUND_BIG_SBOX
        ROUND_BIG_LBOX
    END DO
    T_BIG(VC)
    WRITE_STATE_BIG(VC)
    
    RETURN

END SUBROUTINE HamsiB_Final

!******************************************************************************

#include    "Includes/HamsiB_Undef Macro.f90"

END MODULE MClass_HamsiB
    
!******************************************************************************
