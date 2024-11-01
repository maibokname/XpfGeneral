
MODULE MClass_HTabInteger2B

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *HTabInteger2B* type, the *TabItem* type and their
!   related routines.  The *TabItem* type is a helper and private type used to
!   store a key-value pair.  The *HTabInteger2B* type is a container type that
!   employs an open-addressing hash table implementation to provide common
!   operations for an unordered symbol table. <br>
!   Unlike the *list-based* and *tree-based* types, which can be used instantly
!   by inserting objects into a container, the *HTabInteger2B* type requires an
!   explicit construction before using other provided operations.  There are two
!   methods provided to create the container.  The *CreateEmpty* method constructs
!   an empty table with optional multiple arguments (including an initial capacity,
!   a load factor, a probing algorithm, and a hash function used to compute
!   a hash code of a key) whereas the *Construct* method constructs a table from
!   arrays of keys and values. <br>
!   As an unordered symbol table, the *HTabInteger2B* type uses the Fortran intrinsic
!   *INTEGER(KIND=kInt16)* type as the type of its stored keys and an unlimited polymorphic type
!   as the type of its stored values.  As a symbol table, the *HTabInteger2B* type
!   does not allow duplicated keys.  Therefore, if an inserted key is equal to a key
!   stored in the table, an associated value of the stored key is replaced by an
!   associated value of the inserted key. <br>
!   Technically, the *HTabInteger2B* type employs the open-addressing as a collision
!   resolution technique where the hash resolution is performed through probing.  It
!   provides three probing algorithms: linear probing, quadratic probing and double
!   hashing.  By default, the linear probing algorithm is used.  However, a user can
!   specify other probing algorithm during the construction of the table. <br>

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#define     tHash           tIndex
!----------------------------------------------------------
#define     HashTable       HTabInteger2B
#define     KeyTypeA        INTEGER(KIND=kInt16)
#define     KeyTypeB        INTEGER(KIND=kInt16)
#define     QueueKey        ListInteger2B
#define     QueueVal        ListAnyType
!----------------------------------------------------------

!** USE STATEMENTS:
    USE MBase_Common
    USE ISO_C_BINDING,      ONLY: C_SIZEOF
    USE MBase_ErrHandlers
    USE MBase_SIntUtil
    USE MBase_MathUtil
    USE MBase_DoublyLinkedLists
#ifdef Indx32Bits
    USE MBase_SimpleHash32, ONLY: HashFuncDefault => Hash32_FNV1a
#else
    USE MBase_SimpleHash64, ONLY: HashFuncDefault => Hash64_FNV1a
#endif

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: HTabInteger2B

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_HTabInteger2B'
    ! Special marker token used to indicate the deletion of a key-value pair
    tSInt16,   PARAMETER    :: DELKEY = ToInt16(Z'7FFF')
    ! Special marker token used to indicate the empty of a key-value pair
    tSInt16,   PARAMETER    :: NULKEY = ToInt16(Z'8000')

!** INCLUDE FILE FOR DECLARATION PART**
#include "Includes/Intrinsic HashTable - Declaraction.f90"

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!** INCLUDE FILE FOR IMPLEMENTATION PART **
#include "Includes/Intrinsic HashTable - Implementation.f90"

!** UNDEFINE MACROS **
#undef tHash
#undef HashTable
#undef KeyTypeA
#undef KeyTypeB

END MODULE MClass_HTabInteger2B

!******************************************************************************
