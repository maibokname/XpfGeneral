
MODULE MClass_Keccak

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Keccak* type and its related routines.
!   The *Keccak* type is a *Keccak-based digest* type that extends from
!   the <a href="../module/mclass_kp1600core.html#type-kp1600core">
!   KP1600Core</a> type.  As a *concrete* derived type, it provides
!   all remaining deferred procedures required by all its parent types. <br>
!   Similar to the <a href="../module/mclass_sha3.html#type-sha3">SHA3</a>
!   type, the *Keccak* type represents two families of incremental
!   cryptographic hash functions: the *Keccak* and the *SHA-3* families.
!   Both types are functionally the same and should provide the same
!   hash output if they are initialized to employ the same message-digest
!   algorithm (e.g. SHA3-256).  However, they uses completely different
!   implementations. <br>
!   As the name suggested, the *Keccak* type represents the *Keccak* family
!   by default.  However, a user can specify the *IsSHA3* flag to true
!   when initializing the digest object (by calling the *Create* method)
!   in order to use the padding strategy of the *SHA-3* family.  Also,
!   the *Keccak* type employs the Keccak-256 hash function as a default
!   algorithm.  This implies that the hash output has the output size
!   and the strength of security (against pre-image attack) of 256 bits.
!   The user can also specify the *Security* argument (to one of the four
!   applicable values: 224, 256, 384 and 512) when initializing the digest
!   object in order to use a different algorithm and get a different hash
!   output size. <br>
!    <br>
!^ **REFERENCES**: <br>
!   [1] <a href="https://doi.org/10.6028/NIST.FIPS.202">SHA-3 Standard:
!       Permutation-Based Hash and Extendable-Output Functions. </a> <br>
!   [2] <a href="https://github.com/XKCP/XKCP">The eXtended Keccak Code Package. </a> <br>

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil,           ONLY: ToDecStrSigned
    USE MClass_BaseDigest
    USE MClass_KP1600Core

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    ! derived type
    PUBLIC :: Keccak

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
    !> *Keccak* is a concrete *Keccak-based digest* type that implements an
    !  incremental cryptographic hash function based on the so-called Keccak
    !  hash functions, which is a family of message-digest algorithms.
    TYPE, EXTENDS(KP1600Core) :: Keccak
        PRIVATE
        !% flag indicating whether the SHA-3 family is employed or not.
        tLogical    :: IsSHA3 = FalseVal
        !% security strength in bits
        tInteger    :: Security = 256
    CONTAINS
        ! ---------------------------------------------------------------------
        ! -----                     Private Procedures                    -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *InitializeWOption* method to
        !  initialize the *digest* object with specified options.
        PROCEDURE, PRIVATE  :: InitializeWOption    => Keccak_Initialize_wOption
        ! ---------------------------------------------------------------------
        ! -----                     Public Procedures                     -----
        ! ---------------------------------------------------------------------
        !> Use the *Create* method in place of the *Initialize* method to
        !  initialize the *digest* object with default algorithm (Keccak-256).
        PROCEDURE       :: Initialize   => Keccak_Initialize
        !> **Type-Bound Subroutine**: GetClone <br>
        !  **Purpose**:  To clone the current state of the source object. The destination
        !                object evolves independently of the source object. <br>
        !  **Usage**: <br>
        !   --->    CALL SrcMD%GetClone(DstMD)
        PROCEDURE       :: GetClone     => Keccak_GetClone
        !> **Type-Bound Function**: GetName <br>
        !  **Purpose**:  To get the name of the *digest* object. <br>
        !  **Usage**: <br>
        !   --->    Name = MD%GetName()
        PROCEDURE       :: GetName      => Keccak_GetName
        ! ---------------------------------------------------------------------
        ! -----                     Generic Interfaces                    -----
        ! ---------------------------------------------------------------------
        !> **Type-Bound Subroutine**: Create <br>
        !  **Purpose**:  To perform any essential initialization of the *digest* object. <br>
        !  **Usage**: <br>
        !   ! initialize the object to employ the default (Keccak-256) algorithm <br>
        !   --->    CALL MD%Create() <br>
        !   ! initialize the object to employ the SHA3-256 algorithm <br>
        !   --->    CALL MD%Create(IsSHA3=.TRUE.) <br>
        !   ! initialize the object to employ the Keccak-384 algorithm <br>
        !   --->    CALL MD%Create(IsSHA3=.FALSE., Security=384) <br>
        !   ! initialize the object to employ the SHA3-512 algorithm <br>
        !   --->    CALL MD%Create(IsSHA3=.TRUE., Security=512) <br>
        GENERIC         :: Create       => InitializeWOption
    END TYPE Keccak

!** INTERFACE DEFINITIONS:
    ! na
    
!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

SUBROUTINE Keccak_Initialize(MD)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with default algorithm.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Keccak), INTENT(INOUT)  :: MD    !! 'Keccak' object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ! to employ the Keccak-256 algorithm
    CALL MD%Create(FalseVal)
   
    RETURN

END SUBROUTINE Keccak_Initialize

!******************************************************************************

SUBROUTINE Keccak_Initialize_wOption(MD, IsSHA3, Security)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To perform initialization of the digest object with the specified flag.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Keccak),      INTENT(INOUT)   :: MD           !! 'Keccak' object
    tLogical,           INTENT(IN)      :: IsSHA3
    !^ flag indicating whether the SHA-3 family is employed or not. <br>
    !  - If true, use the SHA-3 family. <br>
    !  - Otherwise, use the Keccak algorithm. <br>
    tInteger, OPTIONAL, INTENT(IN)      :: Security
    !^ Strength of security in bits with four possible values: 224, 256, 384 and 512.
    !  If the specified value is NOT valid, it is set to the default (256) value.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    tByte,    PARAMETER :: Keccak_Suffix = ToInt8(Z'01')
    tByte,    PARAMETER :: SHA3_Suffix   = ToInt8(Z'06')
    tInteger, PARAMETER :: NRounds = 24

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    tInteger    :: Capacity     ! the value of the capacity C in bits
    tIndex      :: DigestLen    ! the desired length of output in bytes
    tByte       :: Suffix

! FLOW
    
    IF (PRESENT(Security)) THEN
        SELECT CASE (Security)
        CASE (224, 256, 384, 512)
            MD%Security = Security
        CASE DEFAULT
            MD%Security = 256
        END SELECT
    ELSE
        MD%Security = 256
    END IF
    MD%IsSHA3  = IsSHA3
    
    ! set input parameters for the *CoreInit* method
    Capacity  = SHIFTL(MD%Security, 1)  ! Security*2
    DigestLen = SHIFTR(MD%Security, 3)  ! Security/8
    IF (IsSHA3) THEN
        Suffix = SHA3_Suffix
    ELSE
        Suffix = Keccak_Suffix
    END IF
    
    ! initialize the core components
    CALL MD%CoreInit(Capacity, Suffix, DigestLen, NRounds)
   
    RETURN

END SUBROUTINE Keccak_Initialize_wOption

!******************************************************************************

SUBROUTINE Keccak_GetClone(Src, Dst)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To clone the current state of the source object.

!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Keccak),                  INTENT(INOUT)   :: Src !! a source object
    CLASS(BaseDigest), ALLOCATABLE, INTENT(OUT)     :: Dst !! a destination object

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    ALLOCATE(Keccak :: Dst)
    SELECT TYPE (Dst)
    TYPE IS (Keccak)
        CALL Dst%Create(Src%IsSHA3, Src%Security)
        CALL Src%CopyState(Dst)
    END SELECT
        
    RETURN

END SUBROUTINE Keccak_GetClone

!******************************************************************************

FUNCTION Keccak_GetName(MD) RESULT(Name)

!** PURPOSE OF THIS SUBROUTINE:
    !^ To return the algorithm name of the hash function.
        
!** SUBROUTINE ARGUMENT DECLARATIONS:
    CLASS(Keccak), INTENT(IN)   :: MD       !! 'Keccak' object
    tCharAlloc                  :: Name     !! name of the hash function

!** SUBROUTINE INTERNAL VARIABLE DECLARATIONS:
    ! na

! FLOW
    
    IF (MD%IsSHA3) THEN
        Name = 'SHA3-' // ToDecStrSigned(MD%Security)
    ELSE
        Name = 'Keccak-' // ToDecStrSigned(MD%Security)
    END IF

    RETURN

END FUNCTION Keccak_GetName

!******************************************************************************

END MODULE MClass_Keccak
    
!******************************************************************************
