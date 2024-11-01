
MODULE MClass_ListRealSP

!^ **PURPOSE OF THIS MODULE**: <br>
!   This module contains the *ListRealSP* type and related routines.
!   The *ListRealSP* type is a container with *REAL(KIND=kSingle)* as the type
!   of its stored items.  It employs a conventional doubly-linked list
!   implementation. <br>
!   See the <a href="../module/mbase_doublylinkedlists.html">MBase_DoublyLinkedLists</a>
!   module for an overview and usage notes of a *doubly-linked-list-based* type.
!   A user may use the *MBase_DoublyLinkedLists* module instead of using this
!   module directly. <br>
!   See the <a href="../module/mclass_linkedlists.html">MClass_LinkedLists</a>
!   module for doubly-linked-list-based types of containers that are functionally
!   similar to the *ListRealSP* type but utilizes a different implementation.
!   Also, unlike the *ListRealSP* type, these container types are designed as
!   generic containers that can be used to store various data types providing that
!   the size (in bytes) of the data to be stored is known at compile time.

!** USE STATEMENTS:
    USE MBase_Common
    USE MBase_SIntUtil,           ONLY: ToChar => ToDecStrSigned
    USE MBase_ErrHandlers
    USE MBase_MemHandlers

    IMPLICIT NONE       ! Enforce explicit typing of all variables

!** ACCESSIBLE SPECIFICATIONS OF MODULE DATA, SUBROUTINES OR FUNCTONS
    PUBLIC :: ListRealSP

    PRIVATE          ! by default, hide all data and routines except those declared explicitly

!*** MACRO DEFINITIONS:
#include    "../../../MacroDef/Macro - Basic Definitions.f90"
#include    "../../../MacroDef/Macro - Util Definitions.f90"
#define IS_EQUAL(A, B)      (A == B)
#define DblLnkList          ListRealSP
#define DLLNode             DLLNodeRealSP
#define ItemTypeA           REAL(KIND=kSingle)
#define ItemTypeB           REAL(KIND=kSingle)
#define ItemTypeC           REAL(KIND=kSingle)
#define TypeOfItem          REAL(KIND=kSingle)

!** MODULE PARAMETERS:
    ! name of module
    tCharStar, PARAMETER    :: ModName = 'MClass_ListRealSP'
    ! Size of character variable for storing error messages returned from
    ! ALLOCATE and DEALLOCATE statement.
    tSInt32,   PARAMETER    :: MsgLen = 128

!** INCLUDE FILE FOR DECLARATION PART**
#include "Includes/Intrinsic List - Declaraction.f90"

!** MODULE VARIABLE DECLARATIONS:
    tCharAlloc      :: SubName      ! routine name
    tCharAlloc      :: ErrMsg       ! error message

    CONTAINS

!** MODULE PROCEDURES (SUBROUTINES OR FUNCTIONS):

!** INCLUDE FILE FOR IMPLEMENTATION PART **
#include "Includes/Intrinsic List - Implementation.f90"

!** UNDEFINE MACROS **
#undef DblLnkList
#undef DLLNode
#undef ItemTypeA
#undef ItemTypeB
#undef ItemTypeC
#undef TypeOfItem

END MODULE MClass_ListRealSP

!******************************************************************************
