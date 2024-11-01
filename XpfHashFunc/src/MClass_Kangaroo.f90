
MODULE MClass_Kangaroo

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Kangaroo* type and its related routines.
!   The *Kangaroo* type is a *digest* type extending directly from the
!   <a href="../module/mclass_basedigest.html#type-basedigest">BaseDigest</a>
!   type.  It implements all deferred procedures required by a digest type. <br>
!   As a *Keccak-based digest* type, the *Kangaroo* type utilizes the
!   <a href="../module/mclass_kp1600sponge.html#type-kp1600sponge">
!   KP1600Sponge</a> type, similar to other *Keccak-based digest* types.
!   However, unlike other *Keccak-based digest* types, the *Kangaroo* type does
!   not extends from the <a href="../module/mclass_kp1600core.html#type-kp1600core">
!   KP1600Core</a> type, due to different designs of internal structures. <br>
!   Similar to the <a href="../module/mclass_shake.html#type-shake">SHAKE</a>
!   type, the *Kangaroo* type represents two incremental cryptographic hash
!   functions (*KangarooTwelve* and *MarsupilamiFourteen*) and is capable of
!   producing a variable-length hash output.  By default, the *Kangaroo* type
!   employs the *KangarooTwelve* hash function as a default algorithm.  However,
!   a user can specify the *IsMarsupilami14* flag to true when initializing the
!   digest object in order to use the *MarsupilamiFourteen* hash function instead
!   of the default one.  As previously mentioned, the *Kangaroo* type is capable
!   of producing variable-length hash output.  Therefore, a user can specify an
!   output length through the optional *OutputLen* argument when initializing the
!   digest object.  If the optional argument is NOT present, the *Kangaroo* type
!   produces the output length based on a default length for a specific algorithm.
!   In addition, a user may use the *Kangaroo* type as an extendable-output function
!   (XOF) by specifying the hash output length during a finalization of the digest
!   object where the *DigestWOutLen* method is called.  This method will ignore the
!   output length specified during initialization if the specified length is valid
!   (greater than or equal to 1). <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://keccak.team/kangarootwelve.html">KangarooTwelve:
!       fast hashing based on Keccak-p. </a> <br>
!   [2] <a href="https://github.com/XKCP/XKCP">The eXtended Keccak Code Package. </a> <br>

!** USE STATEMENTS:
    USE, INTRINSIC  :: ISO_C_BINDING,   ONLY: C_SIZEOF
    USE MBase_Common
    USE MBase_MemHandlers,              ONLY: MemAlloc, MemFree
    USE MBase_ByteUtil,                 ONLY: AnyType_2_ByteArrPtr, ByteArr_2_HexStr => ToHexStr_BE
    USE MClass_BaseDigest
    USE MClass_KP1600Sponge

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Kangaroo

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    tInteger, PARAMETER :: ChunkSize  = 8192
    tByte,    PARAMETER :: SuffixLeaf = ToInt8(Z'0B')  ! '110': message hop, simple padding, inner node
    tLogical, PARAMETER :: NO_ERROR   = .FALSE.
    tLogical, PARAMETER :: HAVE_ERROR = .TRUE.

!** DERIVED TYPE DEFINITIONS
    !> *Kangaroo* is a concrete *Keccak-based digest* type that implements an incremental
    !  cryptographic hash function by employing either the *KangarooTwelve* or the
    !  *MarsupilamiFourteen message-digest* algorithm.
    TYPE, EXTENDS(BaseDigest) :: Kangaroo
        PRIVATE
        !% sponge instance for queue node
        TYPE(KP1600Sponge)  :: QueueNode
        ! sponge instance for final node
        TYPE(KP1600Sponge)  :: FinalNode
        !% customization string
        tByte, POINTER      :: Custom(:) => NULL()
        !% block number
        tIndex              :: BlockNumber = 0_kIndex
        !% length of input queue absorbed
        tIndex              :: QueueAbsorbedLen = 0_kIndex
        !% the value of the capacity in bits
        tInteger            :: Capacity = 256
        !% the length of output message in bytes
        tIndex              :: DigestLen = 16_kIndex
        !% the number of permutation rounds
        tInteger            :: NRounds = 12
        !% flag indicating whether the MarsupilamiFourteen algorithm is employed or not.
        tLogical            :: IsMarsupilami14 = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: DoInit               => Kangaroo_DoInit
        PROCEDURE, PRIVATE  :: DoUpdate             => Kangaroo_DoUpdate
        PROCEDURE, PRIVATE  :: DoFinal              => Kangaroo_DoFinal
        !> Use the *Create* method in place of the *InitializeWOption* method to
        !  initialize the *digest* object with specified options.
        PROCEDURE, PRIVATE  :: InitializeWOption    => Kangaroo_Initialize_wOption
        PROCEDURE, PRIVATE  :: Kangaroo_ByteDigest_wOutLen
        PROCEDURE, PRIVATE  :: Kangaroo_ByteDigest_wInputNOutLen
        PROCEDURE, PRIVATE  :: Kangaroo_HexDigest_wOutLen
        PROCEDURE, PRIVATE  :: Kangaroo_HexDigest_wInputNOutLen
        ! ---------------------------------------------------------------------
        ! -----                 Deferred Procedures Implemented           -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (KangarooTwelve)
        !  and default hash output length.
        PROCEDURE   :: Initialize           => Kangaroo_Initialize
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE   :: GetClone             => Kangaroo_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE   :: GetName              => Kangaroo_GetName
        !> **Type-Bound Subroutine**: Reset <br>
        !  **Purpose**:  To reset the *digest* object to its initial state. <br>
        !  **Usage**: <br>
        !   --->    CALL MD%Reset()
        PROCEDURE   :: Reset                => Kangaroo_Reset
        !> Use the *Update* method in place of the *InsertBytes* method to insert input
        !  data as an array of bytes (8-bit integers) where the offset (zero-based) into
        !  the array and the length of the input in bytes are specified.
        PROCEDURE   :: InsertBytes          => Kangaroo_InsertBytes
        !> Use the *Update* method in place of the *InsertGen* method to insert
        !  input data in a generic way where the *Input* argument can be any type and
        !  any rank and the *InpSize* argument specifies the size of input data in bytes.
        PROCEDURE   :: InsertGen            => Kangaroo_InsertGen
        !> Use the *Digest* method in place of the *ByteDigest* method to finalize the
        !  current hash computation and return the hash value as an array of bytes
        !  in a newly-allocated array.
        PROCEDURE   :: ByteDigest           => Kangaroo_ByteDigest
        !> Use the *Digest* method in place of the *ByteDigest_wInput* method to insert final
        !  input in a generic way and then finalize the current hash computation and return
        !  the hash value as an array of bytes in a newly-allocated array.
        PROCEDURE   :: ByteDigest_wInput    => Kangaroo_ByteDigest_wInput
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Function**: GetDigestLen <br>
        !  **Purpose**:  To return the natural hash function output length (in bytes). <br>
        !  **Usage**: <br>
        !   --->    Length = MD%GetDigestLen()
        PROCEDURE   :: GetDigestLen         => Kangaroo_GetDigestLen
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! employ the default (KangarooTwelve) algorithm and default output length <br>
        !   --->    CALL MD%Create() <br>
        !   ! employ the MarsupilamiFourteen algorithm and default output length <br>
        !   --->    CALL MD%Create(IsMarsupilami14=.TRUE.) <br>
        !   ! employ the KangarooTwelve algorithm with specified output length <br>
        !   --->    CALL MD%Create(IsMarsupilami14=.FALSE., OutputLen=32) <br>
        !   ! employ the KMarsupilamiFourteen algorithm with specified output length <br>
        !   --->    CALL MD%Create(IsMarsupilami14=.TRUE., OutputLen=64) <br>
        GENERIC     :: Create               => InitializeWOption
        !> **Type-Bound Subroutine**: DigestWOutLen <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash output
        !                with specified length.  The object is reset.  Some final input data
        !                can be inserted. <br>
        !  **Usage**: <br>
        !   ! finalize hash computation and return hash output as a byte array <br>
        !   --->    CALL MD%DigestWOutLen(ByteArr, OutLen) <br>
        !   ! insert final input and return hash output as a byte array <br>
        !   --->    CALL MD%DigestWOutLen(Input, InpSize, ByteArr, OutLen) <br>
        !   ! finalize hash computation and return hash output as a hexadecimal string <br>
        !   --->    CALL MD%DigestWOutLen(HexStr, OutLen) <br>
        !   ! insert final input and return hash output as a hexadecimal string <br>
        !   --->    CALL MD%DigestWOutLen(Input, InpSize, HexStr, OutLen) <br>
        !  **Important Note**: If the specified output length is applicable, the output
        !   length specified during initialization will be ignored. <br>
        GENERIC     :: DigestWOutLen        => Kangaroo_ByteDigest_wOutLen, &
                                               Kangaroo_ByteDigest_wInputNOutLen, &
                                               Kangaroo_HexDigest_wOutLen, &
                                               Kangaroo_HexDigest_wInputNOutLen
        ! ---------------------------------------------------------------------
        FINAL       :: Kangaroo_Finalize
        ! ---------------------------------------------------------------------
    END TYPE Kangaroo

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Kangaroo_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with
    !  default algorithm and default hash output length.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo), INTENT(INOUT)  :: MD   !! 'Kangaroo' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the KangarooTwelve algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE Kangaroo_Initialize

!******************************************************************************

SUBROUTINE Kangaroo_Initialize_wOption(MD, IsMarsupilami14, OutputLen, Custom)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified options.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo),         INTENT(INOUT)  :: MD           !! 'Kangaroo' object
    tLogical,                INTENT(IN)     :: IsMarsupilami14
    !^ flag indicating whether the MarsupilamiFourteen algorithm is employed or not. <br>
    !  - If true, use the MarsupilamiFourteen algorithm. <br>
    !  - Otherwise, use the KangarooTwelve algorithm. <br>
    tIndex,        OPTIONAL, INTENT(IN)     :: OutputLen
    !^ the hash output length in bytes (must be positive; otherwise,
    !  the default length produced).
    tByte, TARGET, OPTIONAL, INTENT(IN)     :: Custom(:)
    !^ an array of (8-bit integers) bytes representing the customization bit string (S)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Security     ! security strength in bits
    tInteger    :: Capacity     ! the value of the capacity C in bits
    tIndex      :: DigestLen    ! the desired length of output in bytes
    tInteger    :: NRounds      ! number of permutation rounds

! FLOW
    
    ! set security strength
    MD%IsMarsupilami14 = IsMarsupilami14
    IF (IsMarsupilami14) THEN
        Security = 256
        NRounds  = 14
    ELSE
        Security = 128
        NRounds  = 12
    END IF
    
    ! set input parameters for the *CoreInit* method
    Capacity  = SHIFTL(Security, 1)  ! Security*2
    DigestLen = SHIFTR(Security, 3)  ! Security/8
    
    ! check optional input
    IF (PRESENT(OutputLen)) THEN
        IF (OutputLen > 0_kIndex) THEN
            ! valid input
            DigestLen = OutputLen
        END IF
    END IF
    
    ! initialize the digest object
    CALL MD%DoInit(Capacity, DigestLen, NRounds)
    
    IF (PRESENT(Custom)) MD%Custom => Custom
   
    RETURN

END SUBROUTINE Kangaroo_Initialize_wOption

!******************************************************************************

SUBROUTINE Kangaroo_Reset(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To reset components of the digest.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo), INTENT(INOUT)  :: MD   !! 'Kangaroo' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

!** FLOW
    
    CALL MD%DoInit(MD%Capacity, MD%DigestLen, MD%NRounds)
    
    RETURN

END SUBROUTINE Kangaroo_Reset

!******************************************************************************

SUBROUTINE Kangaroo_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo),                INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(Kangaroo :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (Kangaroo)
        CALL Src%QueueNode%CopyState(Dst%QueueNode)
        CALL Src%FinalNode%CopyState(Dst%FinalNode)
        Dst%BlockNumber      = Src%BlockNumber
        Dst%QueueAbsorbedLen = Src%QueueAbsorbedLen
        Dst%DigestLen        = Src%DigestLen
        Dst%NRounds          = Src%NRounds
        Dst%Capacity         = Src%Capacity
        Dst%IsMarsupilami14  = Src%IsMarsupilami14
        IF (ASSOCIATED(Src%Custom)) Dst%Custom => Src%Custom
    END SELECT
        
    RETURN

END SUBROUTINE Kangaroo_GetClone

!******************************************************************************

FUNCTION Kangaroo_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo), INTENT(IN) :: MD       !! 'Kangaroo' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsMarsupilami14) THEN
        Name = 'MarsupilamiFourteen'
    ELSE
        Name = 'KangarooTwelve'
    END IF

    RETURN

END FUNCTION Kangaroo_GetName

!******************************************************************************

FUNCTION Kangaroo_GetDigestLen(MD) RESULT(Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the output length.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo), INTENT(IN) :: MD       !! 'KP1600_Core' object
    tIndex                      :: Length   !! digest length

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    Length = MD%DigestLen

    RETURN

END FUNCTION Kangaroo_GetDigestLen

!******************************************************************************

SUBROUTINE Kangaroo_InsertBytes(MD, ByteArr, Offset, Length)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data as a byte (8-bit integer) array where offset (zero-based)
    !  and length are specified.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo), INTENT(INOUT)  :: MD             !! 'Kangaroo' object
    tByte,           INTENT(IN)     :: ByteArr(0:)    !! a byte array of input data
    tIndex,          INTENT(IN)     :: Offset         !! the offset in input data
    tIndex,          INTENT(IN)     :: Length         !! the length of input data in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Flag

! FLOW

    Flag = MD%DoUpdate(ByteArr(Offset:), Length)

    RETURN

END SUBROUTINE Kangaroo_InsertBytes

!******************************************************************************

SUBROUTINE Kangaroo_InsertGen(MD, Input, InpSize)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data in a generic way where the *Input* argument can be
    !  any type and any rank and the *InpSize* argument specifies the size of
    !  the input data in a number of bytes.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo),        INTENT(INOUT)   :: MD       !! 'Kangaroo' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)!! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize  !! size of the input (in bytes)

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, POINTER  :: InpPtr(:)
    tLogical        :: Flag

! FLOW
    
    ! set pointer to the input
    CALL AnyType_2_ByteArrPtr(Input, InpSize, InpPtr)
    
    ! insert the input data as a byte array
    Flag = MD%DoUpdate(InpPtr, InpSize)

    NULLIFY(InpPtr)

    RETURN

END SUBROUTINE Kangaroo_InsertGen

!******************************************************************************

SUBROUTINE Kangaroo_ByteDigest(MD, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes in a newly-allocated array.
    !  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo),    INTENT(INOUT)   :: MD           !! 'Kangaroo' object
    tByte, ALLOCATABLE, INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Flag

! FLOW

    IF (MD%DigestLen > 0_kIndex) THEN
        CALL MemAlloc(ByteArr, MD%DigestLen, StartID=0_kIndex)
        Flag = MD%DoFinal(ByteArr)
        CALL MD%Reset()
    END IF

    RETURN

END SUBROUTINE Kangaroo_ByteDigest

!******************************************************************************

SUBROUTINE Kangaroo_ByteDigest_wInput(MD, Input, InpSize, ByteArr)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash
    !  computation and return the hash value as an array of (8-bit integer) bytes
    !  in a newly-allocated array.  The digest object is reset. <br>
    !  **Important Note**: The newly-allocated array has a zero-based index.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo),        INTENT(INOUT)   :: MD           !! 'Kangaroo' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tByte,   ALLOCATABLE,   INTENT(OUT)     :: ByteArr(:)   !! the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%Update(Input, InpSize)
    CALL MD%Digest(ByteArr)
        
    RETURN

END SUBROUTINE Kangaroo_ByteDigest_wInput

!******************************************************************************

SUBROUTINE Kangaroo_ByteDigest_wOutLen(MD, ByteArr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes. The digest object is reset. <br>
    !  Note: The routine will ignore the output length specified during
    !  initialization if the given *OutputLen* is valid (>= 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo), INTENT(INOUT)  :: MD                   !! 'Kangaroo' object
    tIndex,          INTENT(IN)     :: OutputLen            !! the desired output length in bytes
    tByte,           INTENT(OUT)    :: ByteArr(OutputLen)   !! an array containing the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tLogical    :: Flag

! FLOW

    IF (OutputLen > 0_kIndex) THEN
        Flag = MD%DoFinal(ByteArr, OutputLen)
        CALL MD%Reset()
    END IF

    RETURN

END SUBROUTINE Kangaroo_ByteDigest_wOutLen

!******************************************************************************

SUBROUTINE Kangaroo_ByteDigest_wInputNOutLen(MD, Input, InpSize, ByteArr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash
    !  computation and return the hash value as an array of (8-bit integer) bytes.
    !  Note: The routine will ignore the output length specified during
    !  initialization if the given *OutputLen* is valid (>= 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo),        INTENT(INOUT)   :: MD                   !! 'Kangaroo' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)            !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize              !! size of the input (in bytes)
    tIndex,                 INTENT(IN)      :: OutputLen            !! the desired output length in bytes
    tByte,                  INTENT(OUT)     :: ByteArr(OutputLen)   !! an array containing the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%Update(Input, InpSize)
    CALL MD%DigestWOutLen(ByteArr, OutputLen)
        
    RETURN

END SUBROUTINE Kangaroo_ByteDigest_wInputNOutLen

!******************************************************************************

SUBROUTINE Kangaroo_HexDigest_wOutLen(MD, HexStr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as a hexadecimal string in a newly-allocated character string.
    !  Note: The routine will ignore the output length specified during
    !  initialization if the given *OutputLen* is valid (>= 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo), INTENT(INOUT)  :: MD           !! 'BaseDigest' object
    tCharAlloc,      INTENT(OUT)    :: HexStr       !! the hash output as a hexadecimal string
    tIndex,          INTENT(IN)     :: OutputLen    !! the desired output length in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte   :: ByteArr(OutputLen)

! FLOW

    CALL MD%DigestWOutLen(ByteArr, OutputLen)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)

    RETURN

END SUBROUTINE Kangaroo_HexDigest_wOutLen

!******************************************************************************

SUBROUTINE Kangaroo_HexDigest_wInputNOutLen(MD, Input, InpSize, HexStr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash computation
    !  and return the hash value as a hexadecimal string in a newly-allocated character string.
    !  Note: The routine will ignore the output length specified during
    !  initialization if the given *OutputLen* is valid (>= 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo),        INTENT(INOUT)   :: MD           !! 'BaseDigest' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)    !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize      !! size of the input (in bytes)
    tCharAlloc,             INTENT(OUT)     :: HexStr       !! the hash output as a hexadecimal string
    tIndex,                 INTENT(IN)      :: OutputLen    !! the desired output length in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte   :: ByteArr(OutputLen)

! FLOW

    CALL MD%DigestWOutLen(Input, InpSize, ByteArr, OutputLen)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)

    RETURN

END SUBROUTINE Kangaroo_HexDigest_wInputNOutLen

!******************************************************************************

SUBROUTINE Kangaroo_Finalize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To free a pointer component of the object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    TYPE(Kangaroo), INTENT(INOUT)   :: MD   !! 'Kangaroo' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    NULLIFY(MD%Custom)
   
    RETURN

END SUBROUTINE Kangaroo_Finalize

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!                                                                             +
!                     ENGINE ROUTINES FOR KangarooCore                        +
!                                                                             +
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SUBROUTINE Kangaroo_DoInit(MD, Capacity, OutputByteLen, NRounds)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To initialize components of the digest object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo), INTENT(INOUT)  :: MD
    tInteger,        INTENT(IN)     :: Capacity         !! the capacity in bits
    tIndex,          INTENT(IN)     :: OutputByteLen    !! the length of output message in bytes
    tInteger,        INTENT(IN)     :: NRounds          !! number of permutation rounds

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    ! initialize
    MD%BlockNumber      = 0_kIndex
    MD%QueueAbsorbedLen = 0_kIndex
    MD%DigestLen        = OutputByteLen
    MD%NRounds          = NRounds
    MD%Capacity         = Capacity

    ! create the instance
    CALL MD%FinalNode%Initialize(MD%Capacity, MD%NRounds)

    RETURN

END SUBROUTINE Kangaroo_DoInit

!******************************************************************************

FUNCTION Kangaroo_DoUpdate(MD, Input, InputByteLen) RESULT(RetFlag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert input data to be absorbed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo), INTENT(INOUT)  :: MD
    tByte,           INTENT(IN)     :: Input(0:)    !! a byte array containing input data
    tIndex,          INTENT(IN)     :: InputByteLen !! length of the input data in bytes
    tLogical                        :: RetFlag      !! true if success; otherwise, false

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: InLen, CurIndx
    tIndex      :: Length
    tByte       :: Padding(0:0)
    tIndex      :: CapacityInBytes
    tByte       :: Intermediate(0:SHIFTR(MD%Capacity, 3)-1)

! FLOW

    ! initialize
    CapacityInBytes = SHIFTR(MD%Capacity, 3)
    InLen           = InputByteLen
    CurIndx         = 0_kIndex
    IF (MD%BlockNumber == 0_kIndex) THEN
        ! First block, absorb in final node
        IF (InLen < (ChunkSize - MD%QueueAbsorbedLen)) THEN
            Length = InLen
        ELSE
            Length = ChunkSize - MD%QueueAbsorbedLen
        END IF
        IF (MD%FinalNode%Absorb(Input(CurIndx:), Length) /= SUCCESS) THEN
            RetFlag = HAVE_ERROR
            RETURN
        END IF
        CurIndx = CurIndx + Length
        InLen = InLen - Length
        MD%QueueAbsorbedLen = MD%QueueAbsorbedLen + Length
        IF ((MD%QueueAbsorbedLen == ChunkSize).AND.(InLen /= 0_kIndex)) THEN
            ! First block complete and more input data available, finalize it
            ! '110^6': message hop, simple padding
            Padding = ToInt8(Z'03')
            MD%QueueAbsorbedLen = 0_kIndex
            MD%BlockNumber = 1_kIndex
            IF (MD%FinalNode%Absorb(Padding, 1_kIndex) /= SUCCESS) THEN
                RetFlag = HAVE_ERROR
                RETURN
            END IF
            ! Zero padding up to 64 bits
            CALL MD%FinalNode%SetByteIOIndex(IAND((MD%FinalNode%GetByteIOIndex() + 7_kIndex), NOT(7_kIndex)))
        END IF
    ELSEIF (MD%QueueAbsorbedLen /= 0_kIndex) THEN
        ! There is data in the queue, absorb further in queue until block complete
        IF (InLen < (ChunkSize - MD%QueueAbsorbedLen)) THEN
            Length = InLen
        ELSE
            Length = ChunkSize - MD%QueueAbsorbedLen
        END IF
        IF (MD%QueueNode%Absorb(Input(CurIndx:), Length) /= SUCCESS) THEN
            RetFlag = HAVE_ERROR
            RETURN
        END IF
        CurIndx = CurIndx + Length
        InLen = InLen - Length
        MD%QueueAbsorbedLen = MD%QueueAbsorbedLen + Length
        IF (MD%QueueAbsorbedLen == ChunkSize) THEN
            MD%QueueAbsorbedLen = 0_kIndex
            MD%BlockNumber = MD%BlockNumber + 1_kIndex
            IF (MD%QueueNode%AbsorbLastFewBits(SuffixLeaf) /= SUCCESS) THEN
                RetFlag = HAVE_ERROR
                RETURN
            END IF
            IF (MD%QueueNode%Squeeze(Intermediate, CapacityInBytes) /= SUCCESS) THEN
                RetFlag = HAVE_ERROR
                RETURN
            END IF
            IF (MD%FinalNode%Absorb(Intermediate, CapacityInBytes) /= SUCCESS) THEN
                RetFlag = HAVE_ERROR
                RETURN
            END IF
        END IF
    END IF

    DO WHILE (InLen > 0_kIndex)
        IF (InLen < ChunkSize) THEN
            Length = InLen
        ELSE
            Length = ChunkSize
        END IF
        CALL MD%QueueNode%Initialize(MD%Capacity, MD%NRounds)
        IF (MD%QueueNode%Absorb(Input(CurIndx:), Length) /= SUCCESS) THEN
            RetFlag = HAVE_ERROR
            RETURN
        END IF
        CurIndx = CurIndx + Length
        InLen = InLen - Length
        IF (Length == ChunkSize) THEN
            MD%BlockNumber = MD%BlockNumber + 1
            IF (MD%QueueNode%AbsorbLastFewBits(SuffixLeaf) /= SUCCESS) THEN
                RetFlag = HAVE_ERROR
                RETURN
            END IF
            IF (MD%QueueNode%Squeeze(Intermediate, CapacityInBytes) /= SUCCESS) THEN
                RetFlag = HAVE_ERROR
                RETURN
            END IF
            IF (MD%FinalNode%Absorb(Intermediate, CapacityInBytes) /= SUCCESS) THEN
                RetFlag = HAVE_ERROR
                RETURN
            END IF
        ELSE
            MD%QueueAbsorbedLen = Length
        END IF
    END DO

    RetFlag = NO_ERROR

    RETURN

END FUNCTION Kangaroo_DoUpdate

!******************************************************************************

FUNCTION Kangaroo_DoFinal(MD, Output, OutLen) RESULT(RetFlag)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the output data after all the input data have been absorbed.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Kangaroo),  INTENT(INOUT) :: MD
    tByte,            INTENT(OUT)   :: Output(0:)   !! a byte array containing output data
    tIndex, OPTIONAL, INTENT(IN)    :: OutLen       !! desired output length
    tLogical                        :: RetFlag      !! true if success; otherwise, false

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte, ALLOCATABLE  :: EncBuf(:)    ! the encoding buffer
    tIndex              :: N            ! size of the buffer
    tByte               :: Padding
    tByte               :: IsSuccessful
    tIndex              :: CustomLen

! FLOW

    ! Absorb Customization | right_encode(CustomLen)
    IF (ASSOCIATED(MD%Custom)) THEN
        CustomLen = SIZE(MD%Custom, KIND=kIndex)
        IF (MD%DoUpdate(MD%Custom, CustomLen) .NEQV. NO_ERROR) THEN
            RetFlag = HAVE_ERROR
            RETURN
        END IF
    ELSE
        BLOCK
            tByte   :: Custom(0:0)
            CustomLen = 0_kIndex
            IF (MD%DoUpdate(Custom, CustomLen) .NEQV. NO_ERROR) THEN
                RetFlag = HAVE_ERROR
                RETURN
            END IF
        END BLOCK
    END IF
    CALL Right_Encode(CustomLen, EncBuf, N)
    IF (MD%DoUpdate(EncBuf, N) .NEQV. NO_ERROR) THEN
        RetFlag = HAVE_ERROR
        RETURN
    END IF

    IF (MD%BlockNumber == 0_kIndex) THEN
        ! Non complete first block in final node, pad it
        !  '11': message hop, final node
        Padding = ToInt8(Z'07')
    ELSE

        IF (MD%QueueAbsorbedLen /= 0) THEN
            ! There is data in the queue node
            BLOCK
                tIndex              :: CapacityInBytes
                tByte, ALLOCATABLE  :: Intermediate(:)
                CapacityInBytes = SHIFTR(MD%Capacity, 3)
                CALL MemAlloc(Intermediate, CapacityInBytes, StartID=0_kIndex)
                MD%BlockNumber = MD%BlockNumber + 1
                IF (MD%QueueNode%AbsorbLastFewBits(SuffixLeaf) /= SUCCESS) THEN
                    RetFlag = HAVE_ERROR
                    RETURN
                END IF
                IF (MD%QueueNode%Squeeze(Intermediate, CapacityInBytes) /= SUCCESS) THEN
                    RetFlag = HAVE_ERROR
                    RETURN
                END IF
                IF (MD%FinalNode%Absorb(Intermediate, CapacityInBytes) /= SUCCESS) THEN
                    RetFlag = HAVE_ERROR
                    RETURN
                END IF
                CALL MemFree(Intermediate)
            END BLOCK
        END IF
        ! Absorb right_encode(number of Chaining Values) || 0xFF || 0xFF
        MD%BlockNumber = MD%BlockNumber - 1
        CALL Right_Encode(MD%BlockNumber, EncBuf, N)
        EncBuf(N:N+1) = ToInt8(Z'FF')
        N = N + 2
        IF (MD%FinalNode%Absorb(EncBuf, N) /= SUCCESS) THEN
            RetFlag = HAVE_ERROR
            RETURN
        END IF
        ! '01': chaining hop, final node
        Padding = ToInt8(Z'06')
    END IF
    IF (MD%FinalNode%AbsorbLastFewBits(Padding) /= SUCCESS) THEN
        RetFlag = HAVE_ERROR
        RETURN
    END IF

    IF (PRESENT(OutLen)) THEN
        IsSuccessful =MD%FinalNode%Squeeze(Output, OutLen)
    ELSE
        IsSuccessful = MD%FinalNode%Squeeze(Output, MD%DigestLen)
    END IF
    IF (IsSuccessful == SUCCESS) THEN
        RetFlag = NO_ERROR
    ELSE
        RetFlag = HAVE_ERROR
    END IF

    RETURN

END FUNCTION Kangaroo_DoFinal

!******************************************************************************

SUBROUTINE Right_Encode(Value, EncBuf, N)

!** PURPOSE OF THIS SUBROUTINE:
    ! To encode the length

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tIndex,             INTENT(IN)  :: Value
    tByte, ALLOCATABLE, INTENT(OUT) :: EncBuf(:)
    tIndex,             INTENT(OUT) :: N

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tIndex      :: I
    tIndex      :: V

! FLOW

    ! determine size of the buffer
    V = Value
    N = 0
    DO WHILE ((V /= 0).AND.(N < C_SIZEOF(Value)))
        N = N + 1
        V = SHIFTR(V, 8)
    END DO

    ! allocate and set the buffer
    CALL MemAlloc(EncBuf, N+1, StartID=0_kIndex)
    DO I = 1, N
        EncBuf(I-1) = ToInt8(SHIFTR(Value, 8*(N-I)))
    END DO
    EncBuf(N) = ToInt8(N)
    N = N + 1

    RETURN

END SUBROUTINE Right_Encode

!******************************************************************************

END MODULE MClass_Kangaroo
    
!******************************************************************************
