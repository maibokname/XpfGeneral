
MODULE MClass_SHAKE

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *SHAKE* type and its related routines.
!   The *SHAKE* type is a *Keccak-based digest* type that extends from
!   the <a href="../module/mclass_kp1600core.html#type-kp1600core">
!   KP1600Core</a> type.  As a *concrete* derived type, it provides
!   all remaining deferred procedures required by all its parent types. <br>
!   Unlike the <a href="../module/mclass_keccak.html#type-keccak">
!   Keccak</a> type, which produces a fixed-length hash output for the
!   specified strength of security, the *SHAKE* type allows an arbitrary 
!   output length, which is useful in applications such as optimal
!   asymmetric encryption padding.  Similar to the *Keccak* type, the
!   *SHAKE* type represents a family of incremental cryptographic hash
!   functions (two functions, exactly), rather than just one hash function. <br>
!   By default, the *SHAKE* type employs the *SHAKE-128* hash function as
!   a default algorithm.  However, a user can specify the *IsSHAKE256*
!   flag to true when initializing the digest object (by calling the
!   *Create* method) in order to use the *SHAKE-256* hash function, in
!   place of the *SHAKE-128* hash function.  As previously mentioned,
!   the *SHAKE* type is capable of producing variable-length hash output.
!   Therefore, a user can specify an output length through the optional
!   *OutputLen* argument when initializing the digest object.  If the
!   optional argument is NOT present, the *SHAKE* type produces the hash
!   output length based on a default length for a specific algorithm.
!   In addition, a user may use the *SHAKE* type as an extendable-output
!   function (XOF) by specifying the hash output length during a finalization
!   of the digest object where the *DigestWOutLen* method is called.  This
!   method will ignore the output length specified during initialization if
!   the specified length is valid (greater than or equal to 1). <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://doi.org/10.6028/NIST.FIPS.202">SHA-3 Standard:
!       Permutation-Based Hash and Extendable-Output Functions. </a> <br>
!   [2] <a href="https://github.com/XKCP/XKCP">The eXtended Keccak Code Package. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_ByteUtil,   ONLY: AnyType_2_ByteArrPtr, &
                                ByteArr_2_HexStr => ToHexStr_BE
    USE MClass_BaseDigest
    USE MClass_KP1600Core

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: SHAKE

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
    !> *SHAKE* is a concrete *Keccak-based digest* type that implements an
    !  incremental cryptographic hash function by employing either the
    !  *SHAKE-128* or the *SHAKE-256 message-digest* algorithm.
    TYPE, EXTENDS(KP1600Core) :: SHAKE
        PRIVATE
        !% flag indicating whether the SHAKE-256 algorithm is employed or not.
        tLogical    :: IsSHAKE256 = FalseVal
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWOption* method to
        !  initialize the *digest* object with specified options.
        PROCEDURE, PRIVATE  :: InitializeWOption    => SHAKE_Initialize_wOption
        PROCEDURE, PRIVATE  :: SHAKE_ByteDigest_wOutLen
        PROCEDURE, PRIVATE  :: SHAKE_ByteDigest_wInputNOutLen
        PROCEDURE, PRIVATE  :: SHAKE_HexDigest_wOutLen
        PROCEDURE, PRIVATE  :: SHAKE_HexDigest_wInputNOutLen
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (SHAKE-128)
        !  and default hash output length.
        PROCEDURE       :: Initialize       => SHAKE_Initialize
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone         => SHAKE_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName          => SHAKE_GetName
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! employ the default (SHAKE-256) algorithm and default output length <br>
        !   --->    CALL MD%Create() <br>
        !   ! employ the SHAKE-256 algorithm and default output length <br>
        !   --->    CALL MD%Create(IsSHAKE256=.TRUE.) <br>
        !   ! employ the SHAKE-128 algorithm with specified output length <br>
        !   --->    CALL MD%Create(IsSHAKE256=.FALSE., OutputLen=64) <br>
        !   ! employ the SHAKE-256 algorithm with specified output length <br>
        !   --->    CALL MD%Create(IsSHAKE256=.TRUE., OutputLen=128) <br>
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
        GENERIC         :: DigestWOutLen    => SHAKE_ByteDigest_wOutLen, &
                                               SHAKE_ByteDigest_wInputNOutLen, &
                                               SHAKE_HexDigest_wOutLen, &
                                               SHAKE_HexDigest_wInputNOutLen
        ! ---------------------------------------------------------------------
    END TYPE SHAKE

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE SHAKE_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with
    !  default algorithm and default hash output length.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHAKE), INTENT(INOUT) :: MD    !! 'SHAKE' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the SHAKE-128 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE SHAKE_Initialize

!******************************************************************************

SUBROUTINE SHAKE_Initialize_wOption(MD, IsSHAKE256, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified options.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHAKE),     INTENT(INOUT) :: MD           !! 'SHAKE' object
    tLogical,         INTENT(IN)    :: IsSHAKE256
    !^ flag indicating whether the SHAKE-256 algorithm is employed or not. <br>
    !  - If true, use the SHAKE-256 algorithm. <br>
    !  - Otherwise, use the SHAKE-128 algorithm. <br>
    tIndex, OPTIONAL, INTENT(IN)    :: OutputLen
    !^ the hash output length in bytes (must be positive; otherwise,
    !  the default length produced).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    PARAMETER :: Suffix  = ToInt8(Z'1F')
    tInteger, PARAMETER :: NRounds = 24

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Security     ! security strength in bits
    tInteger    :: Capacity     ! the value of the capacity C in bits
    tIndex      :: DigestLen    ! the desired length of output in bytes

! FLOW
    
    ! set security strength
    MD%IsSHAKE256 = IsSHAKE256
    IF (IsSHAKE256) THEN
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
    CALL MD%CoreInit(Capacity, Suffix, DigestLen, NRounds)
   
    RETURN

END SUBROUTINE SHAKE_Initialize_wOption

!******************************************************************************

SUBROUTINE SHAKE_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHAKE),                   INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(SHAKE :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (SHAKE)
        CALL Dst%Create(Src%IsSHAKE256, Src%GetDigestLen())
        CALL Src%CopyState(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE SHAKE_GetClone

!******************************************************************************

FUNCTION SHAKE_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHAKE), INTENT(IN)    :: MD       !! 'SHAKE' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSHAKE256) THEN
        Name = 'SHAKE-256'
    ELSE
        Name = 'SHAKE-128'
    END IF

    RETURN

END FUNCTION SHAKE_GetName

!******************************************************************************

SUBROUTINE SHAKE_ByteDigest_wOutLen(MD, ByteArr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as an array of (8-bit integer) bytes. The digest object is reset. <br>
    !  Note: The routine will ignore the output length specified during
    !  initialization if the given *OutputLen* is valid (>= 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHAKE), INTENT(INOUT) :: MD                   !! 'SHAKE' object
    tIndex,       INTENT(IN)    :: OutputLen            !! the desired output length in bytes
    tByte,        INTENT(OUT)   :: ByteArr(OutputLen)   !! an array containing the hash output

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW

    CALL MD%DoFinalWOutLen(ByteArr, OutputLen)

    RETURN

END SUBROUTINE SHAKE_ByteDigest_wOutLen

!******************************************************************************

SUBROUTINE SHAKE_ByteDigest_wInputNOutLen(MD, Input, InpSize, ByteArr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash
    !  computation and return the hash value as an array of (8-bit integer) bytes.
    !  Note: The routine will ignore the output length specified during
    !  initialization if the given *OutputLen* is valid (>= 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHAKE),           INTENT(INOUT)   :: MD                   !! 'SHAKE' object
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

END SUBROUTINE SHAKE_ByteDigest_wInputNOutLen

!******************************************************************************

SUBROUTINE SHAKE_HexDigest_wOutLen(MD, HexStr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To finalize the current hash computation and return the hash value
    !  as a hexadecimal string in a newly-allocated character string.
    !  Note: The routine will ignore the output length specified during
    !  initialization if the given *OutputLen* is valid (>= 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHAKE), INTENT(INOUT) :: MD           !! 'BaseDigest' object
    tCharAlloc,   INTENT(OUT)   :: HexStr       !! the hash output as a hexadecimal string
    tIndex,       INTENT(IN)    :: OutputLen    !! the desired output length in bytes

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tByte   :: ByteArr(OutputLen)

! FLOW

    CALL MD%DoFinalWOutLen(ByteArr, OutputLen)
    CALL ByteArr_2_HexStr(ByteArr, HexStr)

    RETURN

END SUBROUTINE SHAKE_HexDigest_wOutLen

!******************************************************************************

SUBROUTINE SHAKE_HexDigest_wInputNOutLen(MD, Input, InpSize, HexStr, OutputLen)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To insert final input in a generic way and then finalize the current hash computation
    !  and return the hash value as a hexadecimal string in a newly-allocated character string.
    !  Note: The routine will ignore the output length specified during
    !  initialization if the given *OutputLen* is valid (>= 1).

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(SHAKE),           INTENT(INOUT)   :: MD           !! 'BaseDigest' object
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

END SUBROUTINE SHAKE_HexDigest_wInputNOutLen

!******************************************************************************

END MODULE MClass_SHAKE
    
!******************************************************************************
