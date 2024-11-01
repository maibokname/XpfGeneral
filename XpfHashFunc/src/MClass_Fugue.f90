
MODULE MClass_Fugue

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Fugue* type and its related routines.
!   The *Fugue* type is a *digest* type that extends directly from the
!   <a href="../module/mclass_basedigest.html#type-basedigest">BaseDigest</a>
!   type.  It implements all deferred procedures required by a digest type. <br>
!   The *Fugue* type implements an incremental cryptographic hash
!   function by employing the *Fugue message-digest* algorithm [1].
!   The implementation here is mainly based on the references [2]. <br>
!   The *Fugue* type represents four cryptographic hash functions: the
!   *Fugue-224*, *Fugue-256*, *Fugue-384*, and *Fugue-512* hash functions.
!   By default, the *Fugue* type represents the *Fugue-256* hash function.
!   However, a user can specify the *Security* argument (to one of the
!   four applicable values: 224, 256, 384 and 512) when initializing the
!   digest object in order to use a different hash function and get a
!   different hash output size. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://web.archive.org/web/20170604091329/http://csrc.nist.gov/groups/ST/hash/sha-3/Round2/documents/Fugue_Round2_Update.zip">
!       The Hash Function "Fugue". </a> <br>
!   [2] <a href="https://github.com/pornin/sphlib">SPHLIB 3.0: A Set of
!       Implementations of Various Hash Functions, Both in C and in Java. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_MemHandlers,  ONLY: MemAlloc
    USE MBase_SIntUtil,     ONLY: ToDecStrSigned
    USE MBase_BytePack,     ONLY: BytePackBE, ByteUnpackBE
    USE MBase_ByteUtil,     ONLY: AnyType_2_ByteArrPtr, ByteArr_2_HexStr => ToHexStr_BE
    USE MClass_BaseDigest

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: Fugue

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64
#include    "Includes/Fugue_Def Macro.f90"

!** MODULE PARAMETERS:
#include    "Includes/Fugue_Constants.f90"
    tIndex,   PARAMETER :: DLen224  = 28_kIndex
    tIndex,   PARAMETER :: DLen256  = 32_kIndex
    tIndex,   PARAMETER :: DLen384  = 48_kIndex
    tIndex,   PARAMETER :: DLen512  = 64_kIndex
    tInteger, PARAMETER :: IV224(0:6) = [                   &
            ToInt32(Z'F4C9120D'), ToInt32(Z'6286F757'), &
            ToInt32(Z'EE39E01C'), ToInt32(Z'E074E3CB'), &
            ToInt32(Z'A1127C62'), ToInt32(Z'9A43D215'), &
            ToInt32(Z'BD8D679A')]
    tInteger, PARAMETER :: IV256(0:7) = [                   &
            ToInt32(Z'E952BDDE'), ToInt32(Z'6671135F'), &
            ToInt32(Z'E0D4F668'), ToInt32(Z'D2B0B594'), &
            ToInt32(Z'F96C621D'), ToInt32(Z'FBF929DE'), &
            ToInt32(Z'9149E899'), ToInt32(Z'34F8C248')]
    tInteger, PARAMETER :: IV384(0:11) = [                  &
            ToInt32(Z'AA61EC0D'), ToInt32(Z'31252E1F'), &
            ToInt32(Z'A01DB4C7'), ToInt32(Z'00600985'), &
            ToInt32(Z'215EF44A'), ToInt32(Z'741B5E9C'), &
            ToInt32(Z'FA693E9A'), ToInt32(Z'473EB040'), &
            ToInt32(Z'E502AE8A'), ToInt32(Z'A99C25E0'), &
            ToInt32(Z'BC95517C'), ToInt32(Z'5C1095A1')]
    tInteger, PARAMETER :: IV512(0:15) = [                  &
            ToInt32(Z'8807A57E'), ToInt32(Z'E616AF75'), &
            ToInt32(Z'C5D3E4DB'), ToInt32(Z'AC9AB027'), &
            ToInt32(Z'D915F117'), ToInt32(Z'B6EECC54'), &
            ToInt32(Z'06E8020B'), ToInt32(Z'4A92EFD1'), &
            ToInt32(Z'AAC6E2C9'), ToInt32(Z'DDB21398'), &
            ToInt32(Z'CAE65838'), ToInt32(Z'437F203F'), &
            ToInt32(Z'25EA78E7'), ToInt32(Z'951FDDD6'), &
            ToInt32(Z'DA6ED11D'), ToInt32(Z'E13E3567')]

!** DERIVED TYPE DEFINITIONS
    !> *Fugue* is a concrete *digest* type that implements an incremental
    !  cryptographic hash function based on the Fugue hash algorithms.
    TYPE, EXTENDS(BaseDigest) :: Fugue
        PRIVATE
        !% state (pointer) and its storage
        tInteger            :: Store(0:35) = 0
        tInteger, POINTER   :: State(:) => NULL()
        !% security strength in bits
        tInteger            :: Security = 256
        !% length of hash output in bytes
        tIndex              :: DigestLen = DLen256
        !% other working variables
        tLong               :: BitCount
        tInteger            :: Partial
        tInteger            :: PartialLen
        tInteger            :: RoundShift
        !% pointer to a procedure that processes input data
        PROCEDURE(FugueCore),  POINTER  :: DoUpdate => NULL()
        !% pointer to a procedure that finalize the hash process
        PROCEDURE(FugueClose), POINTER  :: DoFinal  => NULL()
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: Fugue_ByteDigest_AddBits
        PROCEDURE, PRIVATE  :: Fugue_HexDigest_AddBits
        PROCEDURE, PRIVATE  :: InitializeWSecurity  => Fugue_Initialize_wSecurity
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (Fugue-256).
        PROCEDURE       :: Initialize           => Fugue_Initialize
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE       :: Reset                => Fugue_Reset
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone             => Fugue_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName              => Fugue_GetName
        !> Use the *Update* method in place of the *InsertBytes* method to insert input
        !  data as an array of bytes (8-bit integers) where the offset (zero-based) into
        !  the array and the length of the input in bytes are specified.
        PROCEDURE       :: InsertBytes          => Fugue_InsertBytes
        !> Use the *Update* method in place of the *InsertGen* method to insert
        !  input data in a generic way where the *Input* argument can be any type and
        !  any rank and the *InpSize* argument specifies the size of input data in bytes.
        PROCEDURE       :: InsertGen            => Fugue_InsertGen
        !> Use the *Digest* method in place of the *ByteDigest* method to finalize the
        !  current hash computation and return the hash value as an array of bytes
        !  in a newly-allocated array.
        PROCEDURE       :: ByteDigest           => Fugue_ByteDigest
        !> Use the *Digest* method in place of the *ByteDigest_wInput* method to insert final
        !  input in a generic way and then finalize the current hash computation and return
        !  the hash value as an array of bytes in a newly-allocated array.
        PROCEDURE       :: ByteDigest_wInput    => Fugue_ByteDigest_wInput
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE       :: GetDigestLen         => Fugue_GetDigestLen
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (Fugue-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the Fugue-512 algorithm <br>
        !   --->    CALL MD%Create(512) <br>
        GENERIC         :: Create               => InitializeWSecurity
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
        GENERIC     :: AddBitsNDigest           => Fugue_ByteDigest_AddBits, &
                                                   Fugue_HexDigest_AddBits
        ! ---------------------------------------------------------------------
        FINAL       :: Fugue_Finalize
        ! ---------------------------------------------------------------------
    END TYPE Fugue

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> FugueCore is a procedure to process input data. <br>
        SUBROUTINE FugueCore(MD, BytesIn)
            IMPORT
            CLASS(Fugue), INTENT(INOUT) :: MD           !! *Fugue* object
            tByte,        INTENT(IN)    :: BytesIn(0:)  !! the data block
        END SUBROUTINE
        !> FugueClose is a procedure to add the last byte and then perform
        !  the final padding and store the result in the provided buffer.
        SUBROUTINE FugueClose(MD, LastByte, NBits, BytesOut)
            IMPORT
            CLASS(Fugue), INTENT(INOUT) :: MD           !! 'Fugue' object
            tByte,        INTENT(IN)    :: LastByte     !! the last byte
            tByte,        INTENT(IN)    :: NBits        !! number of bits in the last byte
            tByte,        INTENT(INOUT) :: BytesOut(0:) !! the output buffer
        END SUBROUTINE
    END INTERFACE
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Fugue_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue), INTENT(INOUT) :: MD    !! 'Fugue' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    CALL MD%Create(256)
   
    RETURN

END SUBROUTINE Fugue_Initialize

!******************************************************************************

SUBROUTINE Fugue_Initialize_wSecurity(MD, Security)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified Security.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue), TARGET, INTENT(INOUT) :: MD           !! 'Fugue' object
    tInteger,             INTENT(IN)    :: Security
    !^ Strength of security in bits with four possible values: 224, 256, 384 and 512.
    !  If the specified value is NOT valid, it is set to the default (256) value.

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! set security
    SELECT CASE (Security)
    CASE (224, 256, 384, 512)
        MD%Security = Security
    CASE DEFAULT
        MD%Security = 256
    END SELECT
    
    ! set digest length
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

    ! set pointers
    SELECT CASE (MD%DigestLen)
    CASE (28, 32)
        MD%State(0:29) => MD%Store(0:29)
        MD%DoUpdate    => Fugue2_Core
        MD%DoFinal     => Fugue2_Close
    CASE (48)
        MD%State(0:35) => MD%Store(0:35)
        MD%DoUpdate    => Fugue3_Core
        MD%DoFinal     => Fugue3_Close
    CASE (64)
        MD%State(0:35) => MD%Store(0:35)
        MD%DoUpdate    => Fugue4_Core
        MD%DoFinal     => Fugue4_Close
    END SELECT
    
    ! reset
    CALL MD%Reset()
   
    RETURN

END SUBROUTINE Fugue_Initialize_wSecurity

!******************************************************************************

SUBROUTINE Fugue_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset the hash algorithm state.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue), INTENT(INOUT) :: MD   !! 'Fugue' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
        
    SELECT CASE (MD%DigestLen)
    CASE (28)
        MD%Store(0:22)  = 0
        MD%Store(23:29) = IV224(0:6)
    CASE (32)
        MD%Store(0:21)  = 0
        MD%Store(22:29) = IV256(0:7)
    CASE (48)
        MD%Store(0:23)  = 0
        MD%Store(24:35) = IV384(0:11)
    CASE (64)
        MD%Store(0:19)  = 0
        MD%Store(20:35) = IV512(0:15)
    END SELECT
    MD%BitCount   = 0_kInt64
    MD%Partial    = 0
    MD%PartialLen = 0
    MD%RoundShift = 0

    RETURN

END SUBROUTINE Fugue_Reset

!******************************************************************************

SUBROUTINE Fugue_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue),                   INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(Fugue :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (Fugue)
        CALL Dst%Create(Src%Security)
        Dst%State      = Src%State
        Dst%BitCount   = Src%BitCount
        Dst%Partial    = Src%Partial
        Dst%PartialLen = Src%PartialLen
        Dst%RoundShift = Src%RoundShift
    END SELECT
        
    RETURN

END SUBROUTINE Fugue_GetClone

!******************************************************************************

FUNCTION Fugue_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue), INTENT(IN)    :: MD       !! 'Fugue' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    Name = 'Fugue-' // ToDecStrSigned(MD%Security)

    RETURN

END FUNCTION Fugue_GetName

!******************************************************************************

FUNCTION Fugue_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To get the natural hash function output length (in bytes).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue), INTENT(IN)    :: MD       !! 'Fugue' object
    tIndex                      :: Length   !! the digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Length = MD%DigestLen

    RETURN

END FUNCTION Fugue_GetDigestLen

!******************************************************************************

SUBROUTINE Fugue_InsertBytes(MD, ByteArr, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data as a byte (8-bit integer) array where offset (zero-based)
    !  and length are specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue), INTENT(INOUT) :: MD             !! 'Fugue' object
    tByte,        INTENT(IN)    :: ByteArr(0:)    !! a byte array of input data
    tIndex,       INTENT(IN)    :: Offset         !! the offset in input data
    tIndex,       INTENT(IN)    :: Length         !! the length of input data in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%DoUpdate(ByteArr(Offset:Offset+Length-1))

    RETURN

END SUBROUTINE Fugue_InsertBytes

!******************************************************************************

SUBROUTINE Fugue_InsertGen(MD, Input, InpSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data in a generic way where the *Input* argument can be
    !  any type and any rank and the *InpSize* argument specifies the size of
    !  the input data in a number of bytes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue),           INTENT(INOUT)   :: MD       !! 'Fugue' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)!! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize  !! size of the input (in bytes)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: InpPtr(:)

! FLOW
    
    ! set pointer to the input
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)
    
    ! insert the input data as a byte array
    CALL MD%DoUpdate(InpPtr)

    NULLIFY(InpPtr)

    RETURN

END SUBROUTINE Fugue_InsertGen

!******************************************************************************

SUBROUTINE Fugue_ByteDigest(MD, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes in a newly-allocated array.
    !  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue),       INTENT(INOUT)   :: MD           !! 'Fugue' object
    tByte, ALLOCATABLE, INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%AddBitsNDigest(0_kInt8, 0_kInt8, ByteArr)

    RETURN

END SUBROUTINE Fugue_ByteDigest

!******************************************************************************

SUBROUTINE Fugue_ByteDigest_wInput(MD, Input, InpSize, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash
    !  computation and return the hash value as an array of (8-bit integer) bytes
    !  in a newly-allocated array.  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue),           INTENT(INOUT)   :: MD           !! 'Fugue' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tByte,   ALLOCATABLE,   INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%Update(Input, InpSize)
    CALL MD%Digest(ByteArr)
        
    RETURN

END SUBROUTINE Fugue_ByteDigest_wInput

!******************************************************************************

SUBROUTINE Fugue_ByteDigest_AddBits(MD, LastByte, NBits, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then finalize the current hash computation
    !  and return the hash output as a byte array. <br>
    !  This routine is only used for an input message whose bit length is not
    !  a multiple of eight (i.e. a message with partial bytes). <br>
    !  This procedure is intended to be used by a digest type that implements
    !  a hash function that is an entrant of the SHA-3 competition.  It is mainly
    !  used for a test purpose.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue),       INTENT(INOUT)   :: MD           !! 'Fugue' object
    tByte,              INTENT(IN)      :: LastByte     !! the last byte
    tByte,              INTENT(IN)      :: NBits        !! number of bits in the last byte
    tByte, ALLOCATABLE, INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MemAlloc(ByteArr, MD%DigestLen, StartID=0_kIndex)
    CALL MD%DoFinal(LastByte, NBits, ByteArr)
    CALL MD%Reset()

    RETURN

END SUBROUTINE Fugue_ByteDigest_AddBits

!******************************************************************************

SUBROUTINE Fugue_HexDigest_AddBits(MD, LastByte, NBits, HexStr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To add the last byte and then finalize the current hash computation
    !  and return the hash output as a hexadecimal string. <br>
    !  This routine is only used for an input message whose bit length is not
    !  a multiple of eight (i.e. a message with partial bytes). <br>
    !  This procedure is intended to be used by a digest type that implements
    !  a hash function that is an entrant of the SHA-3 competition.  It is mainly
    !  used for a test purpose.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue), INTENT(INOUT) :: MD           !! 'Fugue' object
    tByte,        INTENT(IN)    :: LastByte     !! the last byte
    tByte,        INTENT(IN)    :: NBits        !! number of bits in the last byte
    tCharAlloc,   INTENT(OUT)   :: HexStr       !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, ALLOCATABLE  :: ByteArr(:)

! FLOW

    CALL MD%AddBitsNDigest(LastByte, NBits, ByteArr)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)
        
    RETURN

END SUBROUTINE Fugue_HexDigest_AddBits

!******************************************************************************

SUBROUTINE Fugue_Finalize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free a pointer component of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Fugue), INTENT(INOUT)  :: MD   !! 'Fugue' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    NULLIFY(MD%State)
    NULLIFY(MD%DoUpdate)
    NULLIFY(MD%DoFinal)
   
    RETURN

END SUBROUTINE Fugue_Finalize

!******************************************************************************

SUBROUTINE Fugue2_Core(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process input data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue), INTENT(INOUT) :: MD           ! 'FugueCore' object
    tByte,        INTENT(IN)    :: BytesIn(0:)  ! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length, CurID
    tInteger    :: C0, C1, C2, C3
    tInteger    :: R0, R1, R2, R3
    tInteger    :: P, PLen, RShift, Count, Tmp, Q

! FLOW

    Length = SIZE(BytesIn, KIND=kIndex)
    CORE_ENTRY(BytesIn, Length)
    RShift = MD%RoundShift

    DO
        IF (RShift == 0) THEN
            Q = P
            TIX2(Q, S00, S01, S08, S10, S24)
            CMIX30(S27, S28, S29, S01, S02, S03, S12, S13, S14)
            SMIX(S27, S28, S29, S00)
            CMIX30(S24, S25, S26, S28, S29, S00, S09, S10, S11)
            SMIX(S24, S25, S26, S27)
            NEXT(1, BytesIn, Length)
            ! fall through
        END IF
        IF (RShift <= 1) THEN
            Q = P
            TIX2(Q, S24, S25, S02, S04, S18)
            CMIX30(S21, S22, S23, S25, S26, S27, S06, S07, S08)
            SMIX(S21, S22, S23, S24)
            CMIX30(S18, S19, S20, S22, S23, S24, S03, S04, S05)
            SMIX(S18, S19, S20, S21)
            NEXT(2, BytesIn, Length)
            ! fall through
        END IF
        IF (RShift <= 2) THEN
            Q = P
            TIX2(Q, S18, S19, S26, S28, S12)
            CMIX30(S15, S16, S17, S19, S20, S21, S00, S01, S02)
            SMIX(S15, S16, S17, S18)
            CMIX30(S12, S13, S14, S16, S17, S18, S27, S28, S29)
            SMIX(S12, S13, S14, S15)
            NEXT(3, BytesIn, Length)
            ! fall through
        END IF
        IF (RShift <= 3) THEN
            Q = P
            TIX2(Q, S12, S13, S20, S22, S06)
            CMIX30(S09, S10, S11, S13, S14, S15, S24, S25, S26)
            SMIX(S09, S10, S11, S12)
            CMIX30(S06, S07, S08, S10, S11, S12, S21, S22, S23)
            SMIX(S06, S07, S08, S09)
            NEXT(4, BytesIn, Length)
            ! fall through
        END IF
        IF (RShift <= 4) THEN
            Q = P
            TIX2(Q, S06, S07, S14, S16, S00)
            CMIX30(S03, S04, S05, S07, S08, S09, S18, S19, S20)
            SMIX(S03, S04, S05, S06)
            CMIX30(S00, S01, S02, S04, S05, S06, S15, S16, S17)
            SMIX(S00, S01, S02, S03)
            NEXT(0, BytesIn, Length)
            ! fall through
        END IF
        ! reset RShift in order to loop over through all IFs
        ! until all input data are processed
        RShift = 0
    END DO
    CORE_EXIT(BytesIn, Length)

    RETURN

END SUBROUTINE Fugue2_Core

!******************************************************************************

SUBROUTINE Fugue3_Core(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process input data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue), INTENT(INOUT) :: MD           ! 'FugueCore' object
    tByte,        INTENT(IN)    :: BytesIn(0:)  ! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length, CurID
    tInteger    :: C0, C1, C2, C3
    tInteger    :: R0, R1, R2, R3
    tInteger    :: P, PLen, RShift, Count, Tmp, Q

! FLOW

    Length = SIZE(BytesIn, KIND=kIndex)
    CORE_ENTRY(BytesIn, Length)
    RShift = MD%RoundShift
    DO
        IF (RShift == 0) THEN
            Q = P
            TIX3(Q, S00, S01, S04, S08, S16, S27, S30)
            CMIX36(S33, S34, S35, S01, S02, S03, S15, S16, S17)
            SMIX(S33, S34, S35, S00)
            CMIX36(S30, S31, S32, S34, S35, S00, S12, S13, S14)
            SMIX(S30, S31, S32, S33)
            CMIX36(S27, S28, S29, S31, S32, S33, S09, S10, S11)
            SMIX(S27, S28, S29, S30)
            NEXT(1, BytesIn, Length)
            ! fall through
        END IF
        IF (RShift <= 1) THEN
            Q = P
            TIX3(Q, S27, S28, S31, S35, S07, S18, S21)
            CMIX36(S24, S25, S26, S28, S29, S30, S06, S07, S08)
            SMIX(S24, S25, S26, S27)
            CMIX36(S21, S22, S23, S25, S26, S27, S03, S04, S05)
            SMIX(S21, S22, S23, S24)
            CMIX36(S18, S19, S20, S22, S23, S24, S00, S01, S02)
            SMIX(S18, S19, S20, S21)
            NEXT(2, BytesIn, Length)
            ! fall through
        END IF
        IF (RShift <= 2) THEN
            Q = P
            TIX3(Q, S18, S19, S22, S26, S34, S09, S12)
            CMIX36(S15, S16, S17, S19, S20, S21, S33, S34, S35)
            SMIX(S15, S16, S17, S18)
            CMIX36(S12, S13, S14, S16, S17, S18, S30, S31, S32)
            SMIX(S12, S13, S14, S15)
            CMIX36(S09, S10, S11, S13, S14, S15, S27, S28, S29)
            SMIX(S09, S10, S11, S12)
            NEXT(3, BytesIn, Length)
            ! fall through
        END IF
        IF (RShift <= 3) THEN
            Q = P
            TIX3(Q, S09, S10, S13, S17, S25, S00, S03)
            CMIX36(S06, S07, S08, S10, S11, S12, S24, S25, S26)
            SMIX(S06, S07, S08, S09)
            CMIX36(S03, S04, S05, S07, S08, S09, S21, S22, S23)
            SMIX(S03, S04, S05, S06)
            CMIX36(S00, S01, S02, S04, S05, S06, S18, S19, S20)
            SMIX(S00, S01, S02, S03)
            NEXT(0, BytesIn, Length)
        END IF
        ! reset RShift in order to loop over through all IFs
        ! until all input data are processed
        RShift = 0
    END DO
    CORE_EXIT(BytesIn, Length)

    RETURN

END SUBROUTINE Fugue3_Core

!******************************************************************************

SUBROUTINE Fugue4_Core(MD, BytesIn)

!** PURPOSE OF THIS SUBROUTINE:
    ! To process input data.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue), INTENT(INOUT) :: MD           ! 'FugueCore' object
    tByte,        INTENT(IN)    :: BytesIn(0:)  ! the data block

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: Length, CurID
    tInteger    :: C0, C1, C2, C3
    tInteger    :: R0, R1, R2, R3
    tInteger    :: P, PLen, RShift, Count, Tmp, Q

! FLOW

    Length = SIZE(BytesIn, KIND=kIndex)
    CORE_ENTRY(BytesIn, Length)
    RShift = MD%RoundShift
    DO
        IF (RShift == 0) THEN
            Q = P
            TIX4(Q, S00, S01, S04, S07, S08, S22, S24, S27, S30)
            CMIX36(S33, S34, S35, S01, S02, S03, S15, S16, S17)
            SMIX(S33, S34, S35, S00)
            CMIX36(S30, S31, S32, S34, S35, S00, S12, S13, S14)
            SMIX(S30, S31, S32, S33)
            CMIX36(S27, S28, S29, S31, S32, S33, S09, S10, S11)
            SMIX(S27, S28, S29, S30)
            CMIX36(S24, S25, S26, S28, S29, S30, S06, S07, S08)
            SMIX(S24, S25, S26, S27)
            NEXT(1, BytesIn, Length)
            ! fall through
        END IF
        IF (RShift <= 1) THEN
            Q = P
            TIX4(Q, S24, S25, S28, S31, S32, S10, S12, S15, S18)
            CMIX36(S21, S22, S23, S25, S26, S27, S03, S04, S05)
            SMIX(S21, S22, S23, S24)
            CMIX36(S18, S19, S20, S22, S23, S24, S00, S01, S02)
            SMIX(S18, S19, S20, S21)
            CMIX36(S15, S16, S17, S19, S20, S21, S33, S34, S35)
            SMIX(S15, S16, S17, S18)
            CMIX36(S12, S13, S14, S16, S17, S18, S30, S31, S32)
            SMIX(S12, S13, S14, S15)
            NEXT(2, BytesIn, Length)
            ! fall through
        END IF
        IF (RShift <= 2) THEN
            Q = P
            TIX4(Q, S12, S13, S16, S19, S20, S34, S00, S03, S06)
            CMIX36(S09, S10, S11, S13, S14, S15, S27, S28, S29)
            SMIX(S09, S10, S11, S12)
            CMIX36(S06, S07, S08, S10, S11, S12, S24, S25, S26)
            SMIX(S06, S07, S08, S09)
            CMIX36(S03, S04, S05, S07, S08, S09, S21, S22, S23)
            SMIX(S03, S04, S05, S06)
            CMIX36(S00, S01, S02, S04, S05, S06, S18, S19, S20)
            SMIX(S00, S01, S02, S03)
            NEXT(0, BytesIn, Length)
        END IF
        ! reset RShift in order to loop over through all IFs
        ! until all input data are processed
        RShift = 0
    END DO
    CORE_EXIT(BytesIn, Length)

    RETURN

END SUBROUTINE Fugue4_Core

!******************************************************************************

SUBROUTINE Fugue2_Close(MD, LastByte, NBits, BytesOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To add the last byte, perform final padding and
    ! return the output in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue), INTENT(INOUT) :: MD           ! 'FugueCore' object
    tByte,        INTENT(IN)    :: LastByte     ! the last byte
    tByte,        INTENT(IN)    :: NBits        ! number of bits in the last byte
    tByte,        INTENT(INOUT) :: BytesOut(0:) ! the output buffer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: C0, C1, C2, C3
    tInteger    :: R0, R1, R2, R3
    tInteger    :: TmpS(0:29)
    tInteger    :: S(0:29)
    tByte       :: TmpBuf(0:15)
    tInteger    :: PLen, Rms, Tmp, I

! FLOW

    CLOSE_ENTRY(30, 6, Fugue2_Core)
    
    DO I = 0, 9
        ROR(3, 30)
        CMIX30(S(0), S(1), S(2), S(4), S(5), S(6), S(15), S(16), S(17))
        SMIX(S(0), S(1), S(2), S(3))
    END DO
    DO I = 0, 12
        S(4)  = IEOR(S(4), S(0))
        S(15) = IEOR(S(15), S(0))
        ROR(15, 30)
        SMIX(S(0), S(1), S(2), S(3))
        S(4)  = IEOR(S(4), S(0))
        S(16) = IEOR(S(16), S(0))
        ROR(14, 30)
        SMIX(S(0), S(1), S(2), S(3))
    END DO
    S(4)  = IEOR(S(4), S(0))
    S(15) = IEOR(S(15), S(0))
    
    CALL ByteUnpackBE(S( 1), BytesOut, 0_kIndex)
    CALL ByteUnpackBE(S( 2), BytesOut, 4_kIndex)
    CALL ByteUnpackBE(S( 3), BytesOut, 8_kIndex)
    CALL ByteUnpackBE(S( 4), BytesOut, 12_kIndex)
    CALL ByteUnpackBE(S(15), BytesOut, 16_kIndex)
    CALL ByteUnpackBE(S(16), BytesOut, 20_kIndex)
    CALL ByteUnpackBE(S(17), BytesOut, 24_kIndex)
    IF (MD%DigestLen >= 32) THEN
        CALL ByteUnpackBE(S(18), BytesOut, 28_kIndex)
    END IF

    RETURN

END SUBROUTINE Fugue2_Close

!******************************************************************************

SUBROUTINE Fugue3_Close(MD, LastByte, NBits, BytesOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To add the last byte, perform final padding and
    ! return the output in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue), INTENT(INOUT) :: MD           ! 'FugueCore' object
    tByte,        INTENT(IN)    :: LastByte     ! the last byte
    tByte,        INTENT(IN)    :: NBits        ! number of bits in the last byte
    tByte,        INTENT(INOUT) :: BytesOut(0:) ! the output buffer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: C0, C1, C2, C3
    tInteger    :: R0, R1, R2, R3
    tInteger    :: TmpS(0:35)
    tInteger    :: S(0:35)
    tByte       :: TmpBuf(0:15)
    tInteger    :: PLen, Rms, Tmp, I

! FLOW

    CLOSE_ENTRY(36, 9, Fugue3_Core)
    
    DO I = 0, 17
        ROR(3, 36)
        CMIX30(S(0), S(1), S(2), S(4), S(5), S(6), S(18), S(19), S(20))
        SMIX(S(0), S(1), S(2), S(3))
    END DO
    DO I = 0, 12
        S(4)  = IEOR(S(4), S(0))
        S(12) = IEOR(S(12), S(0))
        S(24) = IEOR(S(24), S(0))
        ROR(12, 36)
        SMIX(S(0), S(1), S(2), S(3))
        S(4)  = IEOR(S(4), S(0))
        S(13) = IEOR(S(13), S(0))
        S(24) = IEOR(S(24), S(0))
        ROR(12, 36)
        SMIX(S(0), S(1), S(2), S(3))
        S(4)  = IEOR(S(4), S(0))
        S(13) = IEOR(S(13), S(0))
        S(25) = IEOR(S(25), S(0))
        ROR(11, 36)
        SMIX(S(0), S(1), S(2), S(3))
    END DO
    S(4)  = IEOR(S(4), S(0))
    S(12) = IEOR(S(12), S(0))
    S(24) = IEOR(S(24), S(0))
    
    CALL ByteUnpackBE(S( 1), BytesOut, 0_kIndex)
    CALL ByteUnpackBE(S( 2), BytesOut, 4_kIndex)
    CALL ByteUnpackBE(S( 3), BytesOut, 8_kIndex)
    CALL ByteUnpackBE(S( 4), BytesOut, 12_kIndex)
    CALL ByteUnpackBE(S(12), BytesOut, 16_kIndex)
    CALL ByteUnpackBE(S(13), BytesOut, 20_kIndex)
    CALL ByteUnpackBE(S(14), BytesOut, 24_kIndex)
    CALL ByteUnpackBE(S(15), BytesOut, 28_kIndex)
    CALL ByteUnpackBE(S(24), BytesOut, 32_kIndex)
    CALL ByteUnpackBE(S(25), BytesOut, 36_kIndex)
    CALL ByteUnpackBE(S(26), BytesOut, 40_kIndex)
    CALL ByteUnpackBE(S(27), BytesOut, 44_kIndex)

    RETURN

END SUBROUTINE Fugue3_Close

!******************************************************************************

SUBROUTINE Fugue4_Close(MD, LastByte, NBits, BytesOut)

!** PURPOSE OF THIS SUBROUTINE:
    ! To add the last byte, perform final padding and
    ! return the output in the provided buffer.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Fugue), INTENT(INOUT) :: MD           ! 'FugueCore' object
    tByte,        INTENT(IN)    :: LastByte     ! the last byte
    tByte,        INTENT(IN)    :: NBits        ! number of bits in the last byte
    tByte,        INTENT(INOUT) :: BytesOut(0:) ! the output buffer

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: C0, C1, C2, C3
    tInteger    :: R0, R1, R2, R3
    tInteger    :: TmpS(0:35)
    tInteger    :: S(0:35)
    tByte       :: TmpBuf(0:15)
    tInteger    :: PLen, Rms, Tmp, I

! FLOW

    CLOSE_ENTRY(36, 12, Fugue4_Core)
    
    DO I = 0, 31
        ROR(3, 36)
        CMIX30(S(0), S(1), S(2), S(4), S(5), S(6), S(18), S(19), S(20))
        SMIX(S(0), S(1), S(2), S(3))
    END DO
    DO I = 0, 12
        S(4)  = IEOR(S(4), S(0))
        S(9)  = IEOR(S(9), S(0))
        S(18) = IEOR(S(18), S(0))
        S(27) = IEOR(S(27), S(0))
        ROR(9, 36)
        SMIX(S(0), S(1), S(2), S(3))
        S(4)  = IEOR(S(4), S(0))
        S(10) = IEOR(S(10), S(0))
        S(18) = IEOR(S(18), S(0))
        S(27) = IEOR(S(27), S(0))
        ROR(9, 36)
        SMIX(S(0), S(1), S(2), S(3))
        S(4)  = IEOR(S(4), S(0))
        S(10) = IEOR(S(10), S(0))
        S(19) = IEOR(S(19), S(0))
        S(27) = IEOR(S(27), S(0))
        ROR(9, 36)
        SMIX(S(0), S(1), S(2), S(3))
        S(4)  = IEOR(S(4), S(0))
        S(10) = IEOR(S(10), S(0))
        S(19) = IEOR(S(19), S(0))
        S(28) = IEOR(S(28), S(0))
        ROR(8, 36)
        SMIX(S(0), S(1), S(2), S(3))
    END DO
    S(4)  = IEOR(S(4), S(0))
    S(9)  = IEOR(S(9), S(0))
    S(18) = IEOR(S(18), S(0))
    S(27) = IEOR(S(27), S(0))
    
    CALL ByteUnpackBE(S( 1), BytesOut, 0_kIndex)
    CALL ByteUnpackBE(S( 2), BytesOut, 4_kIndex)
    CALL ByteUnpackBE(S( 3), BytesOut, 8_kIndex)
    CALL ByteUnpackBE(S( 4), BytesOut, 12_kIndex)
    CALL ByteUnpackBE(S( 9), BytesOut, 16_kIndex)
    CALL ByteUnpackBE(S(10), BytesOut, 20_kIndex)
    CALL ByteUnpackBE(S(11), BytesOut, 24_kIndex)
    CALL ByteUnpackBE(S(12), BytesOut, 28_kIndex)
    CALL ByteUnpackBE(S(18), BytesOut, 32_kIndex)
    CALL ByteUnpackBE(S(19), BytesOut, 36_kIndex)
    CALL ByteUnpackBE(S(20), BytesOut, 40_kIndex)
    CALL ByteUnpackBE(S(21), BytesOut, 44_kIndex)
    CALL ByteUnpackBE(S(27), BytesOut, 48_kIndex)
    CALL ByteUnpackBE(S(28), BytesOut, 52_kIndex)
    CALL ByteUnpackBE(S(29), BytesOut, 56_kIndex)
    CALL ByteUnpackBE(S(30), BytesOut, 60_kIndex)
    
    RETURN

END SUBROUTINE Fugue4_Close

!******************************************************************************

#include    "Includes/Fugue_Undef Macro.f90"

END MODULE MClass_Fugue
    
!******************************************************************************
