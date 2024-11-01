
MODULE MClass_cSHAKE

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *cSHAKE* type and its related routines.
!   The *cSHAKE* type is a *Keccak-based digest* type that extends from
!   the <a href="../module/mclass_kp1600core.html#type-kp1600core">
!   KP1600Core</a> type.  As a *concrete* derived type, it provides
!   all remaining deferred procedures required by all its parent types. <br>
!   Like the <a href="../module/mclass_shake.html#type-shake">SHAKE</a>
!   type, the *cSHAKE* type represents two incremental cryptographic hash
!   functions and is capable of producing a variable-length hash output.
!   The *cSHAKE* type is a customizable version of the *SHAKE* type that
!   supports explicit domain separation via customization parameters.
!   For default initializations (initializing without other input arguments),
!   both types should produces the same hash output for the same input message. <br>
!   See the <a href="../module/mclass_shake.html">MClass_SHAKE</a> module for
!   the default algorithm and how to specify the desired algorithm and/or
!   the desired output length.  See the *Create* (*InitializeWOption* to be
!   exact) method for detailed explanation of customization parameters.
!   Similar to the *SHAKE* type, a user may use the *cSHAKE* type as an
!   extendable-output function (XOF) by specifying the hash output length
!   during a finalization of the digest object where the *DigestWOutLen*
!   method is called.  This method will ignore the output length specified
!   during initialization if the specified length is valid (greater than or
!   equal to 1). <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://doi.org/10.6028/NIST.FIPS.202">SHA-3 Standard:
!       Permutation-Based Hash and Extendable-Output Functions. </a> <br>
!   [2] <a href="https://github.com/XKCP/XKCP">The eXtended Keccak Code Package. </a> <br>

!** USE STATEMENTS:
    USE, INTRINSIC  :: ISO_C_BINDING,   ONLY: C_SIZEOF
    USE MBase_Common
    USE MBase_MemHandlers,              ONLY: MemAlloc
    USE MBase_ByteUtil,                 ONLY: AnyType_2_ByteArrPtr, ByteArr_2_HexStr => ToHexStr_BE
    USE MClass_BaseDigest
    USE MClass_KP1600Core
    USE MClass_KP1600Sponge

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: cSHAKE

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define     tByte       tUInt8
#define     tInteger    tUInt32
#define     tLong       tUInt64

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> *cSHAKE* is a concrete *Keccak-based digest* type that implements an
    !  incremental cryptographic hash function by employing either the
    !  *cSHAKE-128* or the *cSHAKE-256 message-digest* algorithm.
    TYPE, EXTENDS(KP1600Core) :: cSHAKE
        PRIVATE
        !% flag indicating whether the cSHAKE-256 algorithm is employed or not.
        tLogical    :: IscSHAKE256 = FalseVal
        !% flag indicating whether both function and customization strings are empty or not.
        tLogical    :: EmptyNS
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        PROCEDURE, PRIVATE  :: ReInit               => cSHAKE_ReInit
        !> Use the *Create* method in place of the *InitializeWOption* method to
        !  initialize the *digest* object with specified options and customization
        !  parameters.
        PROCEDURE, PRIVATE  :: InitializeWOption    => cSHAKE_Initialize_wOption
        PROCEDURE, PRIVATE  :: cSHAKE_ByteDigest_wOutLen
        PROCEDURE, PRIVATE  :: cSHAKE_ByteDigest_wInputNOutLen
        PROCEDURE, PRIVATE  :: cSHAKE_HexDigest_wOutLen
        PROCEDURE, PRIVATE  :: cSHAKE_HexDigest_wInputNOutLen
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (cSHAKE-128)
        !  and default hash output length, and without customization parameters.
        PROCEDURE       :: Initialize   => cSHAKE_Initialize
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => cSHAKE_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => cSHAKE_GetName
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! employ the default (cSHAKE-256) algorithm and default output length <br>
        !   --->    CALL MD%Create() <br>
        !   ! employ the cSHAKE-256 algorithm and default output length <br>
        !   --->    CALL MD%Create(IscSHAKE256=.TRUE.) <br>
        !   ! employ the cSHAKE-128 algorithm with specified output length <br>
        !   --->    CALL MD%Create(IscSHAKE256=.FALSE., OutputLen=64) <br>
        !   ! employ the cSHAKE-256 algorithm with specified output length <br>
        !   --->    CALL MD%Create(IscSHAKE256=.TRUE., OutputLen=128) <br>
        !   ! employ the cSHAKE-128 algorithm with customization parameters <br>
        !   --->    CALL MD%Create(IscSHAKE256=.FALSE., Name=FuncStr, Custom=CustomStr) <br>
        GENERIC         :: Create       => InitializeWOption
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
        GENERIC         :: DigestWOutLen    => cSHAKE_ByteDigest_wOutLen, &
                                               cSHAKE_ByteDigest_wInputNOutLen, &
                                               cSHAKE_HexDigest_wOutLen, &
                                               cSHAKE_HexDigest_wInputNOutLen
        ! ---------------------------------------------------------------------
    END TYPE cSHAKE

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE cSHAKE_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with
    !  default algorithm and default hash output length, and
    !  without customization parameters.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(cSHAKE), INTENT(INOUT) :: MD    !! 'cSHAKE' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the cSHAKE-128 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE cSHAKE_Initialize

!******************************************************************************

SUBROUTINE cSHAKE_Initialize_wOption(MD, IscSHAKE256, OutputLen, Name, Custom, CustomBitLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified 
    !  options and customization parameters.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(cSHAKE),    INTENT(INOUT) :: MD           !! 'cSHAKE' object
    tLogical,         INTENT(IN)    :: IscSHAKE256
    !^ flag indicating whether the cSHAKE-256 algorithm is employed or not. <br>
    !  - If true, use the cSHAKE-256 algorithm. <br>
    !  - Otherwise, use the cSHAKE-128 algorithm. <br>
    tIndex, OPTIONAL, INTENT(IN)    :: OutputLen
    !^ the hash output length in bytes (must be positive; otherwise,
    !  the default length produced).
    tByte,  OPTIONAL, INTENT(IN)    :: Name(:)
    !^ an array of (8-bit integer) bytes representing the function-name bit string (N);
    !  if not present, N is an empty string.
    tByte,  OPTIONAL, INTENT(IN)    :: Custom(:)
    !^ an array of (8-bit integer) bytes representing the customization bit string (S);
    !  if not present, S is an empty string.
    tIndex, OPTIONAL, INTENT(IN)    :: CustomBitLen
    !^ the length of the customization string in bits

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    PARAMETER :: NonEmptySuffix = ToInt8(Z'04')
    tByte,    PARAMETER :: EmptySuffix    = ToInt8(Z'1F')
    tInteger, PARAMETER :: NRounds        = 24

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Security     ! security strength in bits
    tInteger    :: Capacity     ! the value of the capacity C in bits
    tIndex      :: DigestLen    ! the desired length of output in bytes
    TYPE(KP1600Sponge), POINTER :: Sponge
    tByte,          ALLOCATABLE :: EncBuf(:)    ! the encoding buffer
    tIndex                          :: N            ! size of the buffer
!    tInteger                        :: Capacity     ! the value of the capacity C in bits
    tIndex                          :: RateInBytes
    tByte                           :: Dummy(0:0)

! FLOW
    
    ! set security strength
    MD%IscSHAKE256 = IscSHAKE256
    IF (IscSHAKE256) THEN
        Security = 256
    ELSE
        Security = 128
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
    
    ! initialize the core components
    IF ((.NOT.PRESENT(Name)).AND.(.NOT.PRESENT(Custom))) THEN
        ! both name and customization strings are empty
        MD%EmptyNS = TrueVal
        CALL MD%CoreInit(Capacity, EmptySuffix, DigestLen, NRounds)
    ELSE
        ! one or both name and customization strings is/are not empty
        MD%EmptyNS = FalseVal
        CALL MD%CoreInit(Capacity, NonEmptySuffix, DigestLen, NRounds)
        ! get sponge instance
        Sponge => MD%GetSponge()
        Encoding: BLOCK
            ! Absorb padding byte (.., rate)
            RateInBytes = (1600-Capacity)/8
            CALL Left_Encode(RateInBytes, EncBuf, N)
            IF (Sponge%Absorb(EncBuf, N) /= SUCCESS) EXIT Encoding
            ! Absorb encode_string(Name)
            IF (PRESENT(Name)) THEN
                CALL Left_Encode(SIZE(Name, KIND=kIndex)*8, EncBuf, N)
                IF (Sponge%Absorb(EncBuf, N) /= SUCCESS) EXIT Encoding
                IF (Sponge%Absorb(Name, SIZE(Name, KIND=kIndex)) /= SUCCESS) EXIT Encoding
            ELSE
                CALL Left_Encode(0_kIndex, EncBuf, N)
                IF (Sponge%Absorb(EncBuf, N) /= SUCCESS) EXIT Encoding
                IF (Sponge%Absorb(Dummy, 0_kIndex) /= SUCCESS) EXIT Encoding
            END IF
            ! Absorb encode_string(Customization)
            IF (PRESENT(Custom)) THEN
                IF (PRESENT(CustomBitLen)) THEN
                    ! custom length may be not a multiple of eight bits
                    CALL Left_Encode(CustomBitLen, EncBuf, N)
                    IF (Sponge%Absorb(EncBuf, N) /= SUCCESS) EXIT Encoding
                    ! allowed to be a bit string, as zero padding is following
                    IF (Sponge%Absorb(Custom, (CustomBitLen + 7)/8) /= SUCCESS) EXIT Encoding
                    ! Zero padding up to rate
                    IF (Sponge%GetByteIOIndex() /= 0_kIndex) THEN
                        CALL Sponge%SetByteIOIndex(RateInBytes - 1)
                        EncBuf(0) = 0
                        IF (Sponge%Absorb(EncBuf, 1_kIndex) /= SUCCESS) EXIT Encoding
                    END IF
                ELSE
                    ! custom length is a multiple of eight bits
                    CALL Left_Encode(SIZE(Custom, KIND=kIndex)*8, EncBuf, N)
                    IF (Sponge%Absorb(EncBuf, N) /= SUCCESS) EXIT Encoding
                    IF (Sponge%Absorb(Custom, SIZE(Custom, KIND=kIndex)) /= SUCCESS) EXIT Encoding
                END IF
            ELSE
                CALL Left_Encode(0_kIndex, EncBuf, N)
                IF (Sponge%Absorb(EncBuf, N) /= SUCCESS) EXIT Encoding
                IF (Sponge%Absorb(Dummy, 0_kIndex) /= SUCCESS) EXIT Encoding
            END IF
        END BLOCK Encoding
        NULLIFY(Sponge)
    END IF

    RETURN

END SUBROUTINE cSHAKE_Initialize_wOption

!******************************************************************************

SUBROUTINE cSHAKE_ReInit(MD, IscSHAKE256, OutputLen, EmptyNS)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform re-initialization of the digest object with the specified 
    !  options.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(cSHAKE),    INTENT(INOUT) :: MD           !! 'cSHAKE' object
    tLogical,         INTENT(IN)    :: IscSHAKE256
    !^ flag indicating whether the cSHAKE-256 algorithm is employed or not. <br>
    !  - If true, use the cSHAKE-256 algorithm. <br>
    !  - Otherwise, use the cSHAKE-128 algorithm. <br>
    tIndex, OPTIONAL, INTENT(IN)    :: OutputLen
    !^ the hash output length in bytes (must be positive; otherwise,
    !  the default length produced).
    tLogical,         INTENT(IN)    :: EmptyNS
    !^ flag indicating whether both function and customization strings are empty or not.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    PARAMETER :: NonEmptySuffix = ToInt8(Z'04')
    tByte,    PARAMETER :: EmptySuffix    = ToInt8(Z'1F')
    tInteger, PARAMETER :: NRounds        = 24

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Security     ! security strength in bits
    tInteger    :: Capacity     ! the value of the capacity C in bits
    tIndex      :: DigestLen    ! the desired length of output in bytes

! FLOW
    
    ! set security strength
    MD%IscSHAKE256 = IscSHAKE256
    IF (IscSHAKE256) THEN
        Security = 256
    ELSE
        Security = 128
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
    
    ! initialize the core components
    MD%EmptyNS = EmptyNS
    IF (MD%EmptyNS) THEN
        CALL MD%CoreInit(Capacity, EmptySuffix, DigestLen, NRounds)
    ELSE
        CALL MD%CoreInit(Capacity, NonEmptySuffix, DigestLen, NRounds)
    END IF

    RETURN

END SUBROUTINE cSHAKE_ReInit

!******************************************************************************

SUBROUTINE cSHAKE_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(cSHAKE),                  INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(cSHAKE :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (cSHAKE)
        CALL Dst%ReInit(Src%IscSHAKE256, Src%GetDigestLen(), Src%EmptyNS)
        CALL Src%CopyState(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE cSHAKE_GetClone

!******************************************************************************

FUNCTION cSHAKE_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(cSHAKE), INTENT(IN)   :: MD       !! 'cSHAKE' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IscSHAKE256) THEN
        Name = 'cSHAKE-256'
    ELSE
        Name = 'cSHAKE-128'
    END IF

    RETURN

END FUNCTION cSHAKE_GetName

!******************************************************************************

SUBROUTINE cSHAKE_ByteDigest_wOutLen(MD, ByteArr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes. The digest object is reset. <br>
    !  Note: The routine will ignore the output length specified during
    !  initialization if the given *OutputLen* is valid (>= 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(cSHAKE), INTENT(INOUT)    :: MD                   !! 'cSHAKE' object
    tIndex,        INTENT(IN)       :: OutputLen            !! the desired output length in bytes
    tByte,         INTENT(OUT)      :: ByteArr(OutputLen)   !! an array containing the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%DoFinalWOutLen(ByteArr, OutputLen)

    RETURN

END SUBROUTINE cSHAKE_ByteDigest_wOutLen

!******************************************************************************

SUBROUTINE cSHAKE_ByteDigest_wInputNOutLen(MD, Input, InpSize, ByteArr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash
    !  computation and return the hash value as an array of (8-bit integer) bytes.
    !  Note: The routine will ignore the output length specified during
    !  initialization if the given *OutputLen* is valid (>= 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(cSHAKE),          INTENT(INOUT)   :: MD                   !! 'cSHAKE' object
    TYPE(*), CONTIGUOUS,    INTENT(IN)      :: Input(..)            !! input data (any type and rank)
    tIndex,                 INTENT(IN)      :: InpSize              !! size of the input (in bytes)
    tIndex,                 INTENT(IN)      :: OutputLen            !! the desired output length in bytes
    tByte,                  INTENT(OUT)     :: ByteArr(OutputLen)   !! an array containing the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%Update(Input, InpSize)
    CALL MD%DoFinalWOutLen(ByteArr, OutputLen)
        
    RETURN

END SUBROUTINE cSHAKE_ByteDigest_wInputNOutLen

!******************************************************************************

SUBROUTINE cSHAKE_HexDigest_wOutLen(MD, HexStr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as a hexadecimal string in a newly-allocated character string.
    !  Note: The routine will ignore the output length specified during
    !  initialization if the given *OutputLen* is valid (>= 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(cSHAKE), INTENT(INOUT)    :: MD           !! 'BaseDigest' object
    tCharAlloc,    INTENT(OUT)      :: HexStr       !! the hash output as a hexadecimal string
    tIndex,        INTENT(IN)       :: OutputLen    !! the desired output length in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte   :: ByteArr(OutputLen)

! FLOW

    CALL MD%DoFinalWOutLen(ByteArr, OutputLen)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)

    RETURN

END SUBROUTINE cSHAKE_HexDigest_wOutLen

!******************************************************************************

SUBROUTINE cSHAKE_HexDigest_wInputNOutLen(MD, Input, InpSize, HexStr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash computation
    !  and return the hash value as a hexadecimal string in a newly-allocated character string.
    !  Note: The routine will ignore the output length specified during
    !  initialization if the given *OutputLen* is valid (>= 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(cSHAKE),          INTENT(INOUT)   :: MD           !! 'BaseDigest' object
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

END SUBROUTINE cSHAKE_HexDigest_wInputNOutLen

!******************************************************************************

SUBROUTINE Left_Encode(Value, EncBuf, N)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To encode the length

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
    IF (N == 0) N = 1
    
    ! allocate and set the buffer
    CALL MemAlloc(EncBuf, N+1, StartID=0_kIndex)
    DO I = 1, N
        EncBuf(I) = ToInt8(SHIFTR(Value, 8*(N-I)))
    END DO
    EncBuf(0) = ToInt8(N)
    N = N + 1

    RETURN

END SUBROUTINE Left_Encode

!******************************************************************************

END MODULE MClass_cSHAKE
    
!******************************************************************************
