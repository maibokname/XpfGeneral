
MODULE MClass_Hasher32

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *Hasher32* type and its related routines.
!   The *Hasher32* type is an abstract type that extends from the
!   <a href="../module/mclass_basehasher.html#type-basehasher">BaseHasher</a> 
!   type.  It represents a 32-bit integer hasher where additional methods,
!   specific for 32-bit integer hash functions, are defined to complete
!   the API for an incremental non-cryptographic hash function.  All
!   hashers that output a hash value as a 32-bit integer should extends
!   from this abstract type. <br>
!   <br>
!  **USAGE**: <br>
!   Although there are a number of methods provided, several of them are
!   intended for internal use (by developers).  Only five methods are
!   intentionally provided for users including: <br>
!   - the *Initialize* method that initializes the hasher, <br>
!   - the *Update* method that inputs data into the hasher, <br>
!   - the *Finalize* method that returns the hash value, <br>
!   - the *GetName* method that returns the hasher name, and <br>
!   - the *HashDirect* method that compute the hash value directly. <br>
!   The first three methods mentioned are provided for the incremental
!   hash computations where the *Initialize* method is first called
!   (once), the *Update* method is then called (many times), and the
!   *Finalize* method is finally called (once). <br>
!   If the *Update* method is to be called only one time, then the
!   *HashDirect* method (for non-incremental hash computations) should
!   be employed in place of those three methods.

!** USE STATEMENTS:
    USE MBase_Common
    USE MClass_BaseHasher

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: Hasher32

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MACRO DEFINITIONS:
#include    "../../../../MacroDef/Macro - Basic Definitions.f90"

!** MODULE PARAMETERS:
    ! na

!** DERIVED TYPE DEFINITIONS
    !> *Hasher32* is an abstract type representing a hasher that outputs the hash value
    !  as a 32-bit integer for an incremental non-cryptographic hash function.
   TYPE, ABSTRACT, EXTENDS(BaseHasher) :: Hasher32
    CONTAINS
        !> **Type-Bound Subroutine**: Initialize <br>
        !  **Purpose**:  To initialize the hasher. <br>
        !  **Usage**: <br>
        !   --->    CALL Hasher%Initialize(Seed)            ! hash value with sign <br>
        !   --->    CALL Hasher%Initialize(Seed, .TRUE.)    ! remove sign from hash value <br>
        PROCEDURE(HS32Init),  DEFERRED  :: Initialize
        !> **Type-Bound Function**: Finalize <br>
        !  **Purpose**:  To finalize the current hash computation and return the hash value
        !                in a 32-bit integer.  The object is reset. <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%Finalize()
        PROCEDURE(HS32Final), DEFERRED  :: Finalize
        !> **Type-Bound Function**: HashDirect <br>
        !  **Purpose**:  To compute the hash value directly (non-incrementally). <br>
        !  **Usage**: <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, Seed) <br>
        !   --->    HashCode = Hasher%HashDirect(Input, InpSize, RemoveSign=.TRUE.) <br>
        !  **Note**: The specified input can be any type and any rank where its size is
        !            the number of bytes of storage used by the input.
        PROCEDURE(HS32Hash),  DEFERRED  :: HashDirect
    END TYPE Hasher32

!** INTERFACE DEFINITIONS:
    ABSTRACT INTERFACE
        !> HS32Init is a deferred procedure to initialize the hasher. <br>
        SUBROUTINE HS32Init(HS, Seed, RemoveSign)
            IMPORT
            CLASS(Hasher32), TARGET, INTENT(INOUT)  :: HS   !! a hasher (HS) object
            tUInt32,                 INTENT(IN)     :: Seed !! seed
            tLogical,      OPTIONAL, INTENT(IN)     :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
        END SUBROUTINE
        !> HS32Final is a deferred procedure to finalize the current hash computation
        !  and return the hash value in a 32-bit integer.  The object is reset. <br>
        FUNCTION HS32Final(HS) RESULT(HashCode)
            IMPORT
            CLASS(Hasher32), INTENT(INOUT)  :: HS       !! a hasher (HS) object
            tUInt32                         :: HashCode !! hash code
        END FUNCTION HS32Final
        !> HS32Hash is a deferred procedure to compute the hash value directly
        !  (non-incrementally). <br>
        FUNCTION HS32Hash(HS, Input, InpSize, Seed, RemoveSign) RESULT(HashCode)
            IMPORT
            CLASS(Hasher32),     INTENT(INOUT)  :: HS           !! a hasher (HS) object
            TYPE(*), CONTIGUOUS, INTENT(IN)     :: Input(..)    !! input (any type and rank)
            tIndex,              INTENT(IN)     :: InpSize      !! size of the input (in bytes)
            tUInt32,  OPTIONAL,  INTENT(IN)     :: Seed         !! seed
            tLogical, OPTIONAL,  INTENT(IN)     :: RemoveSign
            !^ flag indicating whether to remove sign bit or not. <br>
            !  - If true, always returns a positive value of the hash code. <br>
            !  - If false, returns either a positive or negative value of the hash code. <br>
            !  - default is false.
            tUInt32                             :: HashCode     !! hash code
        END FUNCTION
        !----------------------------------------------------------------------
    END INTERFACE

!** MODULE VARIABLE DECLARATIONS:
    ! na

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!******************************************************************************

END MODULE MClass_Hasher32

!******************************************************************************
